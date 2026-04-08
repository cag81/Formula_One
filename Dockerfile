FROM rocker/shiny:4.4.1

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

COPY f1_net_zero_simulator.R /srv/shiny-server/app.R

ENV PORT=3838

CMD ["sh", "-c", "R -e \"shiny::runApp('/srv/shiny-server/app.R', host='0.0.0.0', port=as.integer(Sys.getenv('PORT', 3838)))\""]
