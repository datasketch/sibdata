FROM --platform=linux/amd64 rocker/r-ver:4.2.1

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
  && rm -rf /var/lib/apt/lists/*

RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site

# Install remotes for package installation
RUN R -e 'install.packages("remotes")'

# Install CRAN packages (Imports from DESCRIPTION)
RUN Rscript -e 'install.packages(c("DBI", "RSQLite", "duckdb", "dplyr", "tidyr", "data.tree", "jsonlite", "shiny", "DT", "purrr", "leaflet", "leaflet.extras", "highcharter", "htmlwidgets"))'

# Install CRAN packages (Suggests needed by app4.R)
RUN Rscript -e 'install.packages(c("openxlsx", "shinyjs"))'

# Install GitHub packages that are in Remotes field or explicitly needed by app4.R
# These are public repositories, no GITHUB_PAT needed

RUN Rscript -e 'remotes::install_github("datasketch/dstools")'
RUN Rscript -e 'remotes::install_github("datasketch/shinyinvoer")'
RUN Rscript -e 'remotes::install_github("datasketch/shinypanels")'


RUN apt-get update && apt-get install -y wget && rm -rf /var/lib/apt/lists/*

RUN wget https://ds-services-assets.s3.amazonaws.com/common/phantomjs-2.1.1-linux-x86_64.tar.bz2

RUN tar -xjf phantomjs-2.1.1-linux-x86_64.tar.bz2

RUN mv phantomjs-2.1.1-linux-x86_64 /usr/local/share

RUN ln -sf /usr/local/share/phantomjs-2.1.1-linux-x86_64/bin/phantomjs /usr/local/bin

RUN mkdir /build_zone

ADD . /build_zone

WORKDIR /build_zone

# Install local package with all dependencies (Imports and Suggests)
# This will automatically install all packages listed in DESCRIPTION
RUN R -e 'remotes::install_local(upgrade="never", dependencies=TRUE)'

RUN rm -rf /build_zone

EXPOSE 3838

CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0');app_file <- system.file('org_sibhumboldt_sibdata_app4/app4.R', package = 'sibdata');shiny::runApp(app_file)"
