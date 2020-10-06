
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Computational text analysis of archaeologists’ writing about race and inequality

<!-- badges: start -->

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/parkgayoung/bulletinracisminarchy/master?urlpath=rstudio)
[![Docker build
status](https://github.com/parkgayoung/bulletinracisminarchy/workflows/docker-build/badge.svg)](https://github.com/parkgayoung/bulletinracisminarchy/actions)
<!-- badges: end -->

This repository contains the data and code for our paper. Our pre-print
is online here:

> Park, G., L-Y Wang, B. Marwick, (2020) *How do archaeologists write
> about race and inequality? Computational text analysis of 40 years of
> Society of American Archaeology Annual Meeting Abstracts*. , Accessed
> 06 Oct 2020. Online at <https://doi.org/10.31219/osf.io/zm73f>

### How to cite

Please cite this compendium as:

> Park, G., L-Y Wang, B. Marwick, (2020), (2020). *Compendium of R code
> and data for How do archaeologists write about race and inequality?
> Computational text analysis of 40 years of Society of American
> Archaeology Annual Meeting Abstracts*. Accessed 06 Oct 2020. Online at
> <https://doi.org/10.17605/OSF.IO/2N3RF>

## Contents

The **analysis** directory contains:

  - [:file\_folder: code](/analysis/code): R script files that include
    code to reproduce the figures and tables generated by the analysis.
  - [:file\_folder: data](/analysis/data): Data used in the analysis.
  - [:file\_folder: figures](/analysis/figures): Plots and other
    illustrations

## How to run in your broswer or download and run locally

This research compendium has been developed using the statistical
programming language R. To work with the compendium, you will need
installed on your computer the [R
software](https://cloud.r-project.org/) itself and optionally [RStudio
Desktop](https://rstudio.com/products/rstudio/download/).

The simplest way to explore the text, code and data is to click on
[binder](https://mybinder.org/v2/gh/parkgayoung/bulletinracisminarchy/master?urlpath=rstudio)
to open an instance of RStudio in your browser, which will have the
compendium files ready to work with. Binder uses rocker-project.org
Docker images to ensure a consistent and reproducible computational
environment. These Docker images can also be used locally.

You can download the compendium as a zip from from this URL:
[master.zip](/archive/master.zip). After unzipping: - open the `.Rproj`
file in RStudio - run `devtools::install()` to ensure you have the
packages this analysis depends on (also listed in the
[DESCRIPTION](/DESCRIPTION) file). - finally, start with the
first-numbered R script file in the ‘code/’ directory and run the R
code.

### Licenses

**Text and figures :**
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code :** See the [DESCRIPTION](DESCRIPTION) file

**Data :** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/)
attribution requested in reuse

### Contributions

We welcome contributions from everyone. Before you get started, please
see our [contributor guidelines](CONTRIBUTING.md). Please note that this
project is released with a [Contributor Code of Conduct](CONDUCT.md). By
participating in this project you agree to abide by its terms.
