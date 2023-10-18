FROM rocker/verse:4.3.1
RUN apt-get update -y && apt-get install -y  zlib1g-dev  libcurl4-openssl-dev libssl-dev libnode-dev  libicu-dev  libicu-dev make zlib1g-dev  make  libicu-dev zlib1g-dev  libcurl4-openssl-dev libssl-dev  make libssl-dev libcurl4-openssl-dev  libpng-dev libjpeg-dev libicu-dev libxml2-dev libcurl4-openssl-dev libssl-dev  libicu-dev libxml2-dev make pandoc libcurl4-openssl-dev libssl-dev libnode-dev  libicu-dev make zlib1g-dev libxml2-dev pandoc libcurl4-openssl-dev libssl-dev libnode-dev  make zlib1g-dev  libicu-dev make pandoc  libssl-dev libcurl4-openssl-dev  libssl-dev  libjpeg-dev  pandoc  libpng-dev  libfreetype6-dev libfribidi-dev libharfbuzz-dev libfontconfig1-dev libjpeg-dev libpng-dev libtiff-dev  pandoc libicu-dev make  libicu-dev libxml2-dev libssl-dev libcurl4-openssl-dev  libfontconfig1-dev libfreetype6-dev  libfreetype6-dev libfribidi-dev libharfbuzz-dev libfontconfig1-dev  pandoc libicu-dev zlib1g-dev make libxml2-dev libssl-dev libcurl4-openssl-dev libfreetype6-dev libfribidi-dev libharfbuzz-dev libfontconfig1-dev libjpeg-dev libpng-dev libtiff-dev  libxml2-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(renv.config.pak.enabled = TRUE, repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages(c("renv","remotes"))'
COPY renv.lock renv.lock
RUN R -e 'renv::restore()'