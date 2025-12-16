FROM --platform=linux/amd64 rocker/r-ver:4.2.1

# Fuerza una única librería global
ENV R_LIBS_SITE=/usr/local/lib/R/site-library
ENV R_LIBS_USER=/usr/local/lib/R/site-library

RUN apt-get update && apt-get install -y \
  ca-certificates \
  lsb-release \
  libcurl4-openssl-dev \
  libv8-dev \
  libxml2-dev \
  zlib1g-dev \
  imagemagick \
  libmagick++-dev \
  libudunits2-dev \
  gdal-bin \
  libproj-dev \
  libgdal-dev \
  libgeos-dev \
  libgeos++-dev \
  protobuf-compiler \
  libprotobuf-dev \
  libjq-dev \
  libharfbuzz-dev \
  libfribidi-dev \
  libpoppler-cpp-dev \
  wget \
  && rm -rf /var/lib/apt/lists/*

RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" \
  >> /usr/local/lib/R/etc/Rprofile.site

# remotes
RUN Rscript -e 'install.packages("remotes")'

# CRAN packages
RUN Rscript -e 'install.packages(c( "DBI","RSQLite","duckdb","dplyr","tidyr","data.tree","jsonlite","shiny","DT","purrr","leaflet","leaflet.extras","highcharter","htmlwidgets","openxlsx","shinyjs"))'

# GitHub packages
RUN Rscript -e 'remotes::install_github("datasketch/dstools", lib = .libPaths()[1])'
RUN Rscript -e 'remotes::install_github("datasketch/shinyinvoer", lib = .libPaths()[1])'
RUN Rscript -e 'remotes::install_github("datasketch/sibdata", lib = .libPaths()[1], dependencies = TRUE, upgrade = "never")'

# Verificación (ahora sí)
RUN Rscript -e 'stopifnot(requireNamespace("sibdata", quietly = TRUE)); \
  app_dir <- system.file("org_sibhumboldt_sibdata_app4", package="sibdata"); \
  stopifnot(app_dir != ""); \
  cat("sibdata OK:", app_dir, "\n")'

# PhantomJS
RUN wget https://ds-services-assets.s3.amazonaws.com/common/phantomjs-2.1.1-linux-x86_64.tar.bz2 \
 && tar -xjf phantomjs-2.1.1-linux-x86_64.tar.bz2 \
 && mv phantomjs-2.1.1-linux-x86_64 /usr/local/share \
 && ln -sf /usr/local/share/phantomjs-2.1.1-linux-x86_64/bin/phantomjs /usr/local/bin

EXPOSE 3838

CMD R -e "options(shiny.port=3838, shiny.host='0.0.0.0'); \
  app_dir <- system.file('org_sibhumboldt_sibdata_app4', package='sibdata'); \
  shiny::runApp(app_dir, port=3838, host='0.0.0.0')"
