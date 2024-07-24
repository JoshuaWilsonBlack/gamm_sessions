## GAMM Session 3
## NZILBB
## 2024-07-24

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

# We continue to use data from Sóskuthy et al. 2019. 
# "HORIZONTAL DIPHTHONG SHIFT IN NEW ZEALAND ENGLISH"

# Load data
price <- read_rds(here('data', 'price_anon.rds'))

# Apply filtering steps from previous session.
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

# Fit the model with random smooths from the previous session
f1_fs_fit <- bam(
  formula = f1 ~ sex + 
    s(measurement_no, by = sex, k = 4, bs = "cr") +
    s(yob, by = sex, k = 4, bs = "cr") + 
    ti(measurement_no, yob, by = sex, k = c(4, 4), bs = "cr") +
    s(syllables_per_sec, k = 4, bs = "cr") + 
    s(speaker_anon, bs = "re") +
    s(measurement_no, traj_id, bs = "fs", k = 4, m = 1),
  data = price_filtered,
  discrete = TRUE,
  nthreads = detectCores() - 1
)

# A bad fit, ignoring autocorrelation
bad_fit <- bam(
  formula = f1 ~ sex + 
    s(measurement_no, by = sex, k = 4, bs = "cr") +
    s(yob, by = sex, k = 4, bs = "cr") + 
    ti(measurement_no, yob, by = sex, k = c(4, 4), bs = "cr") +
    s(syllables_per_sec, k = 4, bs = "cr") + 
    s(speaker_anon, bs = "re"),
  data = price_filtered,
  discrete = TRUE,
  nthreads = detectCores() - 1
)

# check the autocorrelation
acf_resid(bad_fit)

# What about our model with random smooths?
acf_resid(f1_fs_fit, split_pred="traj_id")

# Let's fit an AR1 model. We want a column to indicate when a new trajectory
# starts
price_filtered <- price_filtered |> 
  mutate(
    traj_start = traj_id != lag(
      traj_id, 
      # 'default' must be something other than the first traj_id.
      default=price_filtered$traj_id[12]
    )
  )

# How can you achieve the same effect using `start_event()`? Note you may need
# to use the `as.data.frame()` function around `price_filtered` if you try this.

# Work out rho
r1 <- start_value_rho(bad_fit, plot=TRUE) 

# Apply an AR1 model
ar1_fit <- bam(
  formula = f1 ~ sex + 
    s(measurement_no, by = sex, k = 4, bs = "cr") +
    s(yob, by = sex, k = 4, bs = "cr") + 
    ti(measurement_no, yob, by = sex, k = c(4, 4), bs = "cr") +
    s(syllables_per_sec, k = 4, bs = "cr") + 
    s(speaker_anon, bs = "re"),
  data = price_filtered,
  rho = r1,
  AR.start = price_filtered$traj_start,
  discrete = TRUE,
  nthreads = detectCores() - 1
)

# check if it worked
acf_resid(ar1_fit, split_pred="AR.start")

# Model comparisons section

# fit our big model with method="ML". 
ar1_ml_fit <- bam(
  formula = f1 ~ sex + 
    s(measurement_no, by = sex, k = 4, bs = "cr") +
    s(yob, by = sex, k = 4, bs = "cr") + 
    ti(measurement_no, yob, by = sex, k = c(4, 4), bs = "cr") +
    s(syllables_per_sec, k = 4, bs = "cr") + 
    s(speaker_anon, bs = "re"),
  data = price_filtered,
  rho = r1,
  AR.start = price_filtered$traj_start,
  method = "ML"
)

# Fit a nested model
ar1_nosex_fit <- bam(
  formula = f1 ~ 
    s(measurement_no, k = 4, bs = "cr") +
    s(yob, k = 4, bs = "cr") + 
    ti(measurement_no, yob, k = c(4, 4), bs = "cr") +
    s(syllables_per_sec, k = 4, bs = "cr") + 
    s(speaker_anon, bs = "re"),
  data = price_filtered,
  rho = r1,
  AR.start = price_filtered$traj_start,
  method = "ML"
)

# compare using compareML()
compareML(ar1_nosex_fit, ar1_ml_fit)

# Exercise: work out rho for the model without sex. Currently, it uses the
# value for the model with sex. Repeat the model comparison and see if anything
# changes. 

# Exercise: test the significance of a different variable using compareML().

# The following code fits a model with a difference smooth using a subset of the
# data from the previous session. It concludes with a difference smooth plot
# showing a difference smooth with significantly distinct parts of the smooth
# for men and women indicated in red. Run through the code and check whether
# you understand each step. 
#
# NB: we will discuss difference smooths in the next session.
price_offset_means <- price |>
  filter(
    measurement_no == 9,
    between(f1, f1_lower, f1_upper),
    between(f2, f2_lower, f2_upper),
    following_voiceless == FALSE 
  ) |> 
  group_by(speaker_anon) |> 
  summarise(
    f1 = mean(f1, na.rm=TRUE),
    f2 = mean(f2, na.rm=TRUE),
    yob = first(yob),
    sex = first(sex),
    syllables_per_sec = mean(
      syllables_per_sec, 
      na.rm=TRUE
    )
  ) |> 
  mutate(
    # Setting sex to an ordered factor is necessary to fit a difference smooth
    sex = ordered(sex)
  )

# This changes the contrasts back to treatment contrasts (the default for 
# unordered factors, but not for ordered factors)
contrasts(price_offset_means$sex) <- "contr.treatment"

f1_fit <- gam(
  formula = f1 ~ sex + 
    # With a difference smooth you have a smooth for the time variable 
    # individually, this fits a smooth for the base level of any factors...
    s(yob, k=10, bs = "cr") + 
    # and a smooth with `by = sex`. This fits a smooth for the difference of
    # latter levels from the base level.
    s(yob, by = sex, k = 10, bs = "cr") + 
    s(syllables_per_sec, k = 10, bs = "cr"),
  data = price_offset_means
)

plot_diff(
  f1_fit, 
  view = "yob", 
  comp=list(sex=c("M","F")), 
  rm.ranef=T,
  print.summary = FALSE
)
