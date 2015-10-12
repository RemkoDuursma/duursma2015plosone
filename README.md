
# plantecophys - an R package for analysing and modelling leaf gas exchange data
[![DOI](https://zenodo.org/badge/11128/RemkoDuursma/duursma2015plosone.svg)](https://zenodo.org/badge/latestdoi/11128/RemkoDuursma/duursma2015plosone)

This repository contains the code to generate the figures and manuscript for this paper:

**Duursma, R.A. plantecophys - an R package for analysing and modelling leaf gas exchange data. PlosONE.**


## Instructions

- To compile the entire manuscript, including references, and figures (not embedded but written to the subdirectory `output`), run:

```r
rmarkdown::render("manuscript.Rmd", "word_document", "manuscript.docx")
```

This assumes you have the `rmarkdown` package installed, as well as Pandoc (but not needed if you run this from Rstudio). It also requires an internet connection, needed by Pandoc for formatting the references.

- To compile the figures only (as PDF), run the following commands.

```r
source("load.R")
source("R/analysis.R")
source("R/make_figures.R")
```

The `load.R` script will attempt to install missing packages, except `plantecophys`, because this repository depends on version 0.6.6 (or higher), which is not yet available on CRAN (but it soon will be). Install the dev version via,

```r
devtools::install_bitbucket("remkoduursma/plantecophys")
```

- An example script that shows how to extract additional statistics from fitted A-Ci curves is given in the file `R/more_stats_acicurvefit.R`.


