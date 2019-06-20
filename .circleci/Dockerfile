FROM r-base:3.5.0

SHELL ["/bin/bash", "-c"]

RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'))" >> $(R RHOME)/etc/Rprofile.site

COPY packages.R /tmp/packages.R

RUN R -f /tmp/packages.R

