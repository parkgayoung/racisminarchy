FROM rocker/binder:4.0.0
LABEL maintainer='Ben Marwick'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}



RUN wget https://github.com/parkgayoung/bulletinracisminarchy/raw/master/DESCRIPTION && R  -e  "install.packages('renv');  renv::restore(repos = c(CRAN='https://packagemanager.rstudio.com'))"

RUN rm DESCRIPTION.1; exit 0
