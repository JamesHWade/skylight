# Skylight Calendar - Docker Image
# Based on rocker/shiny for R Shiny applications

FROM rocker/shiny:4.4.0

LABEL maintainer="James Wade <github@jameshwade.com>"
LABEL description="Skylight - Family Calendar Display with AI Chat"

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages from CRAN
RUN R -e "install.packages(c( \
    'base64enc', \
    'bsicons', \
    'bslib', \
    'cachem', \
    'DBI', \
    'duckdb', \
    'gargle', \
    'glue', \
    'htmltools', \
    'httr2', \
    'jsonlite', \
    'lubridate', \
    'purrr', \
    'rappdirs', \
    'rlang', \
    'sass', \
    'shiny', \
    'shinyjs', \
    'yaml' \
), repos = 'https://cloud.r-project.org/')"

# Install packages from GitHub (ellmer and shinychat)
RUN R -e "install.packages('pak', repos = 'https://cloud.r-project.org/')"
RUN R -e "pak::pak('hadley/ellmer')"
RUN R -e "pak::pak('posit-dev/shinychat')"

# Create app directory
RUN mkdir -p /srv/shiny-server/skylight

# Copy the package
COPY . /srv/shiny-server/skylight/

# Install the skylight package
RUN R -e "install.packages('/srv/shiny-server/skylight', repos = NULL, type = 'source')"

# Create data directory for DuckDB
RUN mkdir -p /data/skylight && chown -R shiny:shiny /data/skylight

# Environment variables
ENV SKYLIGHT_DB_PATH=/data/skylight/skylight.duckdb
ENV SHINY_PORT=3838
ENV SHINY_HOST=0.0.0.0

# Create app.R entry point
RUN echo 'library(skylight); skylight::run_app(host = "0.0.0.0", port = as.integer(Sys.getenv("SHINY_PORT", 3838)))' > /srv/shiny-server/skylight/app.R

# Expose port
EXPOSE 3838

# Set working directory
WORKDIR /srv/shiny-server/skylight

# Run as shiny user
USER shiny

# Start the app
CMD ["R", "-e", "skylight::run_app(host = '0.0.0.0', port = as.integer(Sys.getenv('SHINY_PORT', 3838)))"]
