# Set base image
FROM --platform=linux/amd64 rocker/shiny

# My authorship
LABEL maintainer="ehill@iolani.org"
LABEL version="1.0.0"
LABEL description="Paepae O Waikolu Viz for Iolani School"

# Convenience packages
RUN apt update
RUN apt install -y curl git wget nano

# Install R packages
RUN R -e "install.packages(c('ggplot2', 'dplyr', 'stringr', 'plotly', 'shinydashboard', 'DT', 'htmlwidgets', 'leaflet', 'shinyBS', 'tidyr'))"

# Copy app to image
RUN rm -r /srv/shiny-server/*
COPY pwviz.R /srv/shiny-server/app.R
COPY modules/ /srv/shiny-server/modules/
COPY functions/ /srv/shiny-server/functions/
COPY data_processing/ /srv/shiny-server/data_processing/
