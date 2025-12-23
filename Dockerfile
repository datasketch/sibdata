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

# CRAN packages - incluyendo todas las dependencias de Imports
RUN Rscript -e 'install.packages(c( \
    "DBI", \
    "RSQLite", \
    "duckdb", \
    "tidyverse", \
    "data.tree", \
    "jsonlite", \
    "shiny", \
    "DT", \
    "leaflet", \
    "leaflet.extras", \
    "highcharter", \
    "htmlwidgets", \
    "openxlsx", \
    "shinyjs", \
    "dbplyr", \
    "sf" \
  ), repos = "https://cran.rstudio.com/")'

# GitHub packages
RUN Rscript -e 'remotes::install_github("datasketch/dstools", lib = .libPaths()[1], upgrade = "never")'

RUN Rscript -e 'remotes::install_github("datasketch/shinyinvoer", lib = .libPaths()[1], upgrade = "never")'

# Instalar sibdata con todas sus dependencias
RUN Rscript -e 'remotes::install_github("datasketch/sibdata", lib = .libPaths()[1], dependencies = TRUE, upgrade = "never")'

# Verificación con mejor manejo de errores
RUN Rscript -e ' \
  if (!requireNamespace("sibdata", quietly = TRUE)) { \
    cat("ERROR: sibdata package not found\n"); \
    cat("Installed packages:\n"); \
    print(rownames(installed.packages())); \
    stop("sibdata installation failed") \
  }; \
  cat("sibdata package loaded successfully\n"); \
  app_dir <- system.file("org_sibhumboldt_sibdata_app4", package="sibdata"); \
  if (app_dir == "") { \
    stop("app directory org_sibhumboldt_sibdata_app4 not found") \
  }; \
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
