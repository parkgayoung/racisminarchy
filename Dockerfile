# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:4.0.2

# required
MAINTAINER Ben Marwick <benmarwick@gmail.com>

WORKDIR /racisminarchy
COPY . /racisminarchy

# go into the repo directory
RUN . /etc/environment \
  # Install linux depedendencies here
  # e.g. need this for ggforce::geom_sina
  && sudo apt-get update \
  # install pkgs we need
  && sudo apt-get install -y libgsl-dev libudunits2-dev libnlopt-dev libpoppler-cpp-dev libtesseract-dev  tesseract-ocr-eng \
  && R -e "install.packages(c('BiocManager', 'remotes'), repos = c(CRAN = 'https://cloud.r-project.org'))" \
  && R -e "remotes::install_github(c('rstudio/renv'))" \
  && R -e "renv::restore()" \
  # render the manuscript into a docx, you'll need to edit this if you've
  # customised the location and name of your main Rmd file
  && R -e "rmarkdown::render('/racisminarchy/analysis/paper/paper.Rmd')"
