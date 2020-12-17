# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:4.0.2

# required
MAINTAINER Ben Marwick <benmarwick@gmail.com>

COPY . /bulletinracisminarchy

# go into the repo directory
RUN . /etc/environment \
  # Install linux depedendencies here
  # e.g. need this for ggforce::geom_sina
  && sudo apt-get update \
  && sudo apt-get install libudunits2-dev -y \
  # build this compendium package
  && R -e "devtools::install('/bulletinracisminarchy', dep=TRUE)" \
  && R -e "devtools::check('/bulletinracisminarchy')" \
  # render the manuscript into a docx, you'll need to edit this if you've
  # customised the location and name of your main Rmd file
  && R -e "rmarkdown::render('/bulletinracisminarchy/analysis/paper/paper.Rmd')"
