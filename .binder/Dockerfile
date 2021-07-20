FROM rocker/binder:4.0.0
LABEL maintainer='Ben Marwick'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}



RUN wget https://github.com/parkgayoung/bulletinracisminarchy/raw/master/DESCRIPTION && R -e --vanilla "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2020-12-19/')); install.packages('renv'); renv::restore()"

RUN rm DESCRIPTION.1; exit 0
