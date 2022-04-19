
# libraries ---------------------------------------------------------------

#.libPaths("S:/CoronaWatch/r_libraries_4")

library(tidyverse)
library(crayon) # for printing results in colour
library(RColorBrewer)
library(ggpubr)
library(ggridges)
library(gt)
library(gtsummary)
library(epitrix)
library(distcrete) # to discretize distributions
library(outbreaker2)
library(truncnorm)
library(VirusWatch) # if error message go to
# Coronawatch/R_libraries_testing/VirusWatch_package/instructions.txt


cat(
  blue(
    "##################################################\n" %+%
      green$underline$bold("libraries.R ran successfully\n") %+%
      "##################################################\n"
  )
)
