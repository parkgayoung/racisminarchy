# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:4.0.2

# required
MAINTAINER Ben Marwick <benmarwick@gmail.com>

COPY . /bulletinracisminarchy

WORKDIR /bulletinracisminarchy

# go into the repo directory
RUN . /etc/environment \
  # Install linux depedendencies here
  # e.g. need this for ggforce::geom_sina
  && sudo apt-get update \
  && sudo apt-get install libudunits2-dev -y \
  # build this compendium package
  && R -e "devtools::install('/bulletinracisminarchy', dep=TRUE)" \
  && R -e "devtools::check('/bulletinracisminarchy')" \
  && R -e "source('/bulletinracisminarchy/analysis/code/001-word-stats-plots.R')" \
  && R -e "source('/bulletinracisminarchy/analysis/code/002-topic-model.R')" \
  && R -e "source('/bulletinracisminarchy/analysis/code/003-word-distances-and-similarities.R')" \
  && R -e "source('/bulletinracisminarchy/analysis/code/004-keyword-in-context.R')" \
  && R -e "source('/bulletinracisminarchy/analysis/code/005-social-event-correlation.R')"
