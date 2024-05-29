## GAMM Session 2
## NZILBB
## 2024-05-29

## Load libraries
## Install these if you don't have them.

# Tidyverse and friends
library(tidyverse)
library(here)

# GAMM fit
library(mgcv)

# GAMM visualisation + 
library(itsadug)
library(gratia)

# This package is used to work out how many cpu cores you have
library(parallel)

# Set global theme.
theme_set(theme_bw())

# Today we are using data from Sóskuthy et al. 2019. 
# "HORIZONTAL DIPHTHONG SHIFT IN NEW ZEALAND ENGLISH"

# Load data
price <- read_rds(here('data', 'price_anon.rds'))

# Look at the data in the Rstudio viewer
View(price)

# Let's do something with the techniques from last session. We need to avoid
# complex random effects here. We want one value for each speaker. We'll take
# the ninth measurement for each token, and get the mean F1 and F2 for each
# speaker. We'll use the upper and lower bounds from the data to filter as well.
price_offset_means <- price |>
  filter(
    measurement_no == 9,
    between(f1, f1_lower, f1_upper),
    between(f2, f2_lower, f2_upper),
    following_voiceless == FALSE # filtering step used in paper.
  ) |> 
  group_by(speaker_anon) |> 
  summarise(
    f1 = mean(f1, na.rm=TRUE),
    f2 = mean(f2, na.rm=TRUE),
    yob = first(yob),
    sex = first(sex),
    syllables_per_sec = mean(syllables_per_sec, na.rm=TRUE)
  )

# Let's have a look at these.
price_offset_means |> 
  ggplot(
    aes(
      x = yob,
      y = f1,
      colour = sex
    )
  ) +
  geom_point() +
  geom_smooth()

# Let's fit a more formal GAMM
f1_fit <- gam(
  formula = f1 ~ sex + s(yob, by = sex, k = 10, bs = "tp") + 
    s(syllables_per_sec, k = 10, bs = "tp"),
  data = price_offset_means
)
# Error: Error in smoothCon(split$smooth.spec[[i]], data, knots, absorb.cons, scale.penalty = scale.penalty,  : 
# Can't find by variable

# Solution: We need _factor_ variables
price_offset_means <- price_offset_means |> 
  mutate(
    sex = factor(sex)
  )

# try again
f1_fit <- gam(
  formula = f1 ~ sex + s(yob, by = sex, k = 10, bs = "tp") + 
    s(syllables_per_sec, k = 10, bs = "tp"),
  data = price_offset_means
)

# Check summary
summary(f1_fit)

# Check if k is high enough.
gam.check(f1_fit)
# Looking fine.

# Let's look at these plots in the nice `gratia::appraise()` way
appraise(f1_fit)
# Close enough for Jazz.

# Let's look at each smooth.
draw(f1_fit)
# 1: early increase in women, then a bit of wiggling.
# 2: slower increase in men, then a bit of decline.
# 3: As you speak faster, the frequency of your 'f1' increases, i.e., your 
# tongue ends up lower in your mouth.

# Plotting with `itsadug::plot_smooth()`
plot_smooth(
  f1_fit,
  view = "yob", # Which variable do you want to see?
  plot_all = "sex"
)
# Unlike `draw` we see the parametric terms as well. Smaller absolute 
# change in the men. This is not surprising though, given how frequency works. 

# note the console output. What is the value of `syllables_per_sec`?

# If you have a different plot in mind, you can use the very helpful 
# `itsadug::get_predictions()` function.
price_mean_f1_preds <- get_predictions(
  f1_fit,
  cond = list(
    yob = seq(
      min(price_offset_means$yob), 
      max(price_offset_means$yob)
    ),
    sex = c("F", "M")
  )
)
# Again, a mean value is picked for syllables_per_sec. You can now plot this
# information however you prefer.

## Exercise: fit the same model but with F2 as a response. 
## Bonus exercise: plot the results of the F1 and F2 models _together_.

### Interactions.
f1_ti_fit <- gam(
  formula = f1 ~ sex + s(yob, by = sex, k = 10, bs = "tp") + 
    s(syllables_per_sec, k = 4, bs = "tp") + 
    ti(yob, syllables_per_sec, by = sex, k = c(10, 4), bs = "tp"),
  data = price_offset_means
)

# Look at the summary again.
summary(f1_ti_fit)
# All terms are significant in the summary except the interaction for male
# speakers.

# But what is the effect of each smooth term in the model.
draw(f1_ti_fit)

# We can make a similar plot with `itsadug::pvisgam()`
# Partial effects plot for female speakers.
pvisgam(
  f1_ti_fit, 
  view=c("yob", "syllables_per_sec"),
  select = 4
)
# Try to figure out what happens if you change the number after 'select'.
# For more on these visualisation functions see https://cran.r-project.org/web/packages/itsadug/vignettes/inspect.html


# Another kind of random effect: te().
f1_te_fit <- gam(
  formula = f1 ~ sex + 
    te(yob, syllables_per_sec, by = sex, k = c(10, 4), bs = "tp"),
  data = price_offset_means
)

summary(f1_te_fit)
# What is different about this fit?

draw(f1_te_fit)

# This model does not let you distinguish between main effect and interaction
# terms. The version which is most sensible will depend on your hypothesis.

# Random effects. The first kind: random intercepts. Here we include _each measurement_
# from each speaker, rather than just their means. 

# First, we need to apply the filtering to the price data set.
price_filtered <- price |> 
  filter(
    measurement_no == 9,
    between(f1, f1_lower, f1_upper),
    between(f2, f2_lower, f2_upper),
    following_voiceless == FALSE # filtering step used in paper.
  ) |> 
  mutate(
    sex = factor(sex)
  )

# Quick look at the summary to make sure there's nothing too crazy going on.
summary(price_filtered)

# We'll switch to `bam` now. We also change to cubic regression splines. Thin
# plate splines can be slightly inefficient with bam (see warnings section of
# `?bam`).
f1_int_fit <- bam(
  formula = f1 ~ sex + s(yob, by = sex, k = 10, bs = "cr") + 
    s(syllables_per_sec, k = 4, bs = "cr") + 
    ti(yob, syllables_per_sec, by = sex, k = c(10, 4), bs = "cr") +
    s(speaker_anon, bs = "re"),
  data = price_filtered
)
# Error in names(dat) <- object$term : 
# 'names' attribute [1] must be the same length as the vector [0]

# This is what the factor issue looks like for bs="re".

# Let's fix it this time the base R way.
price_filtered$speaker_anon <- factor(price_filtered$speaker_anon)

f1_int_fit <- bam(
  formula = f1 ~ sex + s(yob, by = sex, k = 10, bs = "cr") + 
    s(syllables_per_sec, k = 4, bs = "cr") + 
    ti(yob, syllables_per_sec, by = sex, k = c(10, 4), bs = "cr") +
    s(speaker_anon, bs = "re"),
  data = price_filtered,
  discrete = TRUE,
  nthreads = detectCores() - 1
)

# Things are starting to slow down (this often happens with random effects)!
summary(f1_int_fit)
# We lose the significance on the summary. The other smooths have similar
# looking levels of 'wiggliness'.

appraise(f1_int_fit)
# plots look good.

gam.check(f1_int_fit)
# not too low k.

draw(f1_int_fit)
# Uh oh! We should hav elooked at our data more.

hist(price_filtered$syllables_per_sec, breaks = 100)
# Let's just filter this to make it max out at 10.

price_filtered <- price_filtered |> 
  filter(
    syllables_per_sec < 10
  )

# Fit again
f1_int_fit <-  bam(
  formula = f1 ~ sex + s(yob, by = sex, k = 10, bs = "cr") + 
    s(syllables_per_sec, k = 4, bs = "cr") + 
    ti(yob, syllables_per_sec, by = sex, k = c(10, 4), bs = "cr") +
    s(speaker_anon, bs = "re"),
  data = price_filtered,
  discrete = TRUE,
  nthreads = detectCores() - 1
)

summary(f1_int_fit)

draw(f1_int_fit)
# Interestingly different results for syllables per second.
# Note also that our speaker intercept increasingly depart from normality as
# they increase.

plot_smooth(
  f1_int_fit,
  plot_all = "sex",
  view = "yob"
)
# Similar story to the above, but with many more data points.

# Let's create a new variable to provide an instance of a random slope.
price_filtered <- price_filtered |> 
  mutate(
    following_liquid = following %in% c("r", "l"),
    following_liquid = factor(following_liquid)
  )

summary(price_filtered$following_liquid)
# Not great balance, but that is OK!

# Fit again
f1_int_slope_fit <-  bam(
  formula = f1 ~ sex + following_liquid + 
    s(yob, by = sex, k = 10, bs = "tp") + 
    s(syllables_per_sec, k = 4, bs = "tp") + 
    ti(yob, syllables_per_sec, by = sex, k = c(10, 4), bs = "tp") +
    s(speaker_anon, following_liquid, bs = "re"),
  data = price_filtered,
  discrete = TRUE,
  nthreads = detectCores() - 1
)

summary(f1_int_slope_fit)

# To get an example of random _smooths_ we'll simplify some aspects of the
# model and will turn back on `discrete = TRUE` (used in the last session).
price_filtered <- price |> 
  filter(
    between(f1, f1_lower, f1_upper),
    between(f2, f2_lower, f2_upper),
    syllables_per_sec < 10,
    following_voiceless == FALSE # filtering step used in paper.
  ) |> 
  mutate(
    sex = factor(sex),
    speaker_anon = factor(speaker_anon),
  ) |> 
  # create identifier for each trajectory.
  group_by(speaker_anon) |>
  mutate(
    traj_no = cumsum(measurement_no <= lag(measurement_no, default = 11)),
    traj_id = paste0(speaker_anon, '_', traj_no),
    traj_id = factor(traj_id)
  ) |> 
  ungroup()

# make a subset of the data so that it can be fit on a laptop. Just keep 
# the first three trajectories and 30% of the speakers. We want some early
# and some later speakers.
early_speakers <- price |> 
  filter(yob < 1900) |> 
  pull(speaker_anon) |>
  unique()

mid_speakers <- price |> 
  filter(between(yob, 1900, 1950)) |> 
  pull(speaker_anon) |>
  unique()

late_speakers <- price |> 
  filter(yob > 1950) |> 
  pull(speaker_anon) |>
  unique()

speakers_to_keep <- c(
  sample(early_speakers, size = 50),
  sample(mid_speakers, size = 50),
  sample(late_speakers, size = 50)
)

price_filtered <- price_filtered |> 
  filter(
    speaker_anon %in% speakers_to_keep,
    traj_no <= 3
  )

# Check balance
price_filtered |> 
  select(sex, yob, f1) |> 
  summary()

# We now fit the model with random smooths
f1_fs_fit <- bam(
  formula = f1 ~ sex + 
    s(measurement_no, by = sex, k = 4, bs = "tp") +
    s(yob, by = sex, k = 4, bs = "tp") + 
    ti(measurement_no, yob, by = sex, k = c(4, 4), bs = "tp") +
    s(syllables_per_sec, k = 4, bs = "tp") + 
    s(speaker_anon, bs = "re") +
    s(measurement_no, traj_id, bs = "fs", k = 4, m = 1),
  data = price_filtered,
  discrete = TRUE,
  nthreads = detectCores() - 1
)

# If you run this, it will take a long time.
summary(f1_fs_fit)

# This, with re.test = FALSE, is much faster.
summary(f1_fs_fit, re.test = FALSE)

appraise(f1_fs_fit)

draw(f1_fs_fit)

# Here, the plots are getting a bit complicated. We can do something a bit
# nicer with the `get_predictions` function.
big_mod_preds <- get_predictions(
  f1_fs_fit,
  cond = list(
    yob = c(1860, 1880, 1900, 1920, 1950, 1970),
    measurement_no = 1:11,
    sex = c("M", "F")
  )
)

big_mod_preds |> 
  ggplot(
    aes(
      x = measurement_no, 
      ymin = fit - CI,
      ymax = fit + CI,
      y = fit,
      fill = sex
    )
  ) +
  geom_line(aes(colour = sex)) +
  geom_ribbon(alpha = 0.2) +
  facet_wrap(vars(yob))

# Exercise: fit this for f2 then find some way to plot them together.

# Exercise: in these data the speech rate seems to increase over time. Can you
# figure out a way to modify the plot above so that each facet is at the 
# average speech rate _for that year_.

# Exercise: how might we modify the model to capture the fact that speech rate
# seems to increase with time? Perhaps another `ti` term would work? You may
# need more data points for this to work. To get more data points, go up to the
# part of the code with `traj_no <= 3` above and increase the value.
