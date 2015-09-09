# Two useful packages for printing numbers, accessing content of objects.
library(broom)
library(reporttools)

# Tables in markdown
library(ascii)

# mine
library(plantecophys)

# use git
library(git2r)

# figures
source("figures.R")

# output
if(!dir.exists("output"))dir.create("output")

# cache, for downloaded files and analyses
if(!dir.exists("cache"))dir.create("cache")

