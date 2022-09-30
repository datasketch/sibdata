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

ARG GITHUB_PAT

RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site

RUN R -e 'install.packages("remotes")'

RUN Rscript -e 'remotes::install_version("tidyverse", upgrade="never", version = "1.3.2")'

RUN Rscript -e 'remotes::install_version("RSQLite", upgrade="never", version = "2.2.17")'

RUN Rscript -e 'remotes::install_version("googlesheets4", upgrade="never", version = "1.0.1")'

RUN Rscript -e 'remotes::install_version("shiny", upgrade="never", version = "1.7.2")'

RUN Rscript -e 'remotes::install_version("DT", upgrade="never", version = "0.23")'

RUN Rscript -e 'remotes::install_version("dotenv", upgrade="never", version = "1.0.3")'

RUN Rscript -e 'remotes::install_version("shinydisconnect", upgrade="never", version = "0.1.0")'

RUN Rscript -e 'remotes::install_version("tictoc", upgrade="never", version = "1.1")'

RUN Rscript -e 'remotes::install_version("waffle", upgrade="never", version = "0.7.0")'

RUN Rscript -e 'remotes::install_version("htmlwidgets", upgrade="never", version = "1.5.4")'

RUN Rscript -e 'remotes::install_version("openxlsx", upgrade="never", version = "4.2.5")'

RUN Rscript -e 'remotes::install_version("DBI", upgrade="never", version = "1.1.3")'

RUN Rscript -e 'remotes::install_version("jsonlite", upgrade="never", version = "1.8.0")'

RUN Rscript -e 'remotes::install_version("data.tree", upgrade="never", version = "1.0.0")'

RUN Rscript -e 'remotes::install_version("gt", upgrade="never", version = "0.7.0")'

RUN Rscript -e 'remotes::install_version("devtools", upgrade="never", version = "2.4.4")'

RUN Rscript -e 'remotes::install_github("dgrtwo/dbcooper@6757ff84ec40b0c9d2c60560cc6ee4032ef2ac4a")'

RUN Rscript -e 'remotes::install_github("jpmarindiaz/mop@44a9f30b7d3fe8c5ff18f828ceac4473397df132")'

RUN Rscript -e 'remotes::install_github("datasketch/makeup@e4cde16244da49883e728de54b34586ebd84e40c")'

RUN Rscript -e 'remotes::install_github("datasketch/shinyinvoer@dd8178db99cac78f0abbd236e83e07bf1f22ba18")'

RUN Rscript -e 'remotes::install_github("datasketch/shinypanels@ce26c64f9749d1fbe90c992b1c15e83af576b305")'

RUN Rscript -e 'remotes::install_github("datasketch/ggmagic@cfeb47aafd792bf342d657177206ff28fa833b0d")'

RUN Rscript -e 'remotes::install_github("datasketch/dsmodules@5e9a9860ae27aad2cbecf3492be5eab1545e5ff5")'

RUN Rscript -e 'remotes::install_github("datasketch/hgchmagic@1c2126cb2722071855fa7133bffcd32a805399e1")'

RUN Rscript -e 'remotes::install_github("datasketch/lfltmagic@7677b096a1440ba105c67c883d1144830204923e")'

RUN apt-get update && apt-get install -y wget && rm -rf /var/lib/apt/lists/*

RUN wget phantomjs-2.1.1-linux-x86_64.tar.bz2

RUN tar phantomjs-2.1.1-linux-x86_64.tar.bz2

RUN mv phantomjs-2.1.1-linux-x86_64 /usr/local/share

RUN ln -sf /usr/local/share/phantomjs-2.1.1-linux-x86_64/bin/phantomjs /usr/local/bin

RUN mkdir /build_zone

ADD . /build_zone

WORKDIR /build_zone

RUN R -e 'remotes::install_local(upgrade="never")'

RUN rm -rf /build_zone

EXPOSE 3838

CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0');sibdata::run_app()"
