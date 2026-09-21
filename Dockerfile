FROM rocker/shiny:4.4.1

# Install system dependencies
RUN apt-get update && apt-get install -y \
    jags \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy application files
COPY . /srv/shiny-server/wpp711/

# Set working directory
WORKDIR /srv/shiny-server/wpp711

# Install R packages into the bundled library
RUN Rscript -e " \
  pkgs <- c('shiny','shinyjs','bslib','DT','processx','zip','readr','shinyvalidate', \
            'R2jags','coda','foreach','doParallel','gplots','mvtnorm','snpar', \
            'neuralnet','conicfit','pracma','geigen','LNPar'); \
  install.packages(pkgs, lib='R_library', repos='https://cran.r-project.org', \
                   dependencies=TRUE, Ncpus=4); \
  "

# Expose port
EXPOSE 3838

# Run the app
CMD ["Rscript", "-e", "shiny::runApp('/srv/shiny-server/wpp711/app_batch.R', host='0.0.0.0', port=3838)"]
