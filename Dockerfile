FROM rocker/shiny:4.4.1

# Install system dependencies for Shiny
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy app files
COPY f1_net_zero_simulator.R /srv/shiny-server/app.R

# Railway provides PORT env var; Shiny listens on 3838 by default.
# We override to use Railway's PORT.
ENV PORT=3838

# Run Shiny on the port Railway assigns
CMD ["sh", "-c", "R -e \"shiny::runApp('/srv/shiny-server/app.R', host='0.0.0.0', port=as.integer(Sys.getenv('PORT', 3838)))\""]
