# This script contains code and exercises for the fourth GAMM workshop 
# session.

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

# The nzilbb.vowels package provides some useful functions.
# It has to be installed using github.
# remotes::install_github("nzilbb/nzilbb_vowels")
library(nzilbb.vowels)

# Set global theme.
theme_set(theme_bw())

# nzilbb.vowels provides a subset of F1 and F2 readings for NZE monopthongs
# from the Brand et al. 2021 paper.
# have a look at what's in the data set using the viewer:
View(onze_vowels)
# and the documentation
?onze_vowels

# Exercise 1: fit a GAMM to the onze_vowels data set, predicting F1 for DRESS, 
# with a parametric term for `vowel`, a smooth controlling for speech rate, 
# and separate year of birth smooths for each gender. Fit random intercepts for
# speaker and word. Plot the model using the `draw` function.

# Exercise 2: modify the data and model from Exercise 1 to fit a difference
# smooth (using an ordered factor with treatment contrasts)

# Exercise 3: modify the data and model fro Exercise 1 to fit a dummy variable
# difference smooth.


