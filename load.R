
source("R/utils.R")


if(!require(pacman))install.packages("pacman")
pacman::p_load(broom, reporttools,  # Two useful packages for 
                                    #printing numbers, accessing content of objects.
               ascii,                         #  Tables in markdown
               plantecophys,
               git2r,                         # use git from R
               downloader)                    # for downloading files

# figures
# (Script with figure definitions, does not make PDFs)
source("R/figures.R")

# output
if(!dir.exists("output"))dir.create("output")

# cache, for downloaded files and analyses
if(!dir.exists("cache"))dir.create("cache")

# Download Medlyn data for example application
acifn <- "data/TumbarumbaGasex_ACis_Medlyn.csv"
spotfn <- "data/TumbarumbaGasex_Spot_Medlyn.csv"



         