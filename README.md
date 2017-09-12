
# plantecophys - an R package for analysing and modelling leaf gas exchange data
[![DOI](https://zenodo.org/badge/11128/RemkoDuursma/duursma2015plosone.svg)](https://zenodo.org/badge/latestdoi/11128/RemkoDuursma/duursma2015plosone)

This repository contains the code to generate the figures and manuscript for this paper:

Duursma, R.A., 2015. Plantecophys - An R Package for Analysing and Modelling Leaf Gas Exchange Data. PLoS ONE 10, e0143346. [doi:10.1371/journal.pone.0143346](). (Open Access)

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

The `load.R` script will attempt to install missing packages from CRAN automatically.

- An example script that shows how to extract additional statistics from fitted A-Ci curves is given in the file `R/more_stats_acicurvefit.R`.


