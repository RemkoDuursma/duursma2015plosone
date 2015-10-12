
source("R/utils.R")

# Two useful packages for printing numbers, accessing content of objects.
Library(broom)
Library(reporttools)

# Tables in markdown
Library(ascii)

# mine
Library(plantecophys)
if(packageVersion("plantecophys") < "0.6.6")
  stop("Please install plantecophys 0.6.6 or newer, if needed from www.bitbucket.org/remkoduursma/plantecophys")

# use git
Library(git2r)

# for downloading files
Library(downloader)


# figures
source("figures.R")

# output
if(!dir.exists("output"))dir.create("output")

# cache, for downloaded files and analyses
if(!dir.exists("cache"))dir.create("cache")

# Download Medlyn data for example application
acifn <- "cache/MedlynAci.csv"
spotfn <- "cache/MedlynSpot.csv"

if(!file.exists(acifn)){
  download("http://files.figshare.com/2256196/TumbarumbaGasex_ACis_Medlyn.csv", 
         acifn)
}
if(!file.exists(spotfn)){
  download("http://files.figshare.com/2256198/TumbarumbaGasex_Spot_Medlyn.csv",
         spotfn)
}



         