FROM fredhutch/r-shiny-server-base:4.4.1

RUN apt-get update -y && apt-get install -y libudunits2-dev libgdal-dev libabsl-dev libgeos-dev  libproj-dev

# Install R packages
RUN R -e 'install.packages(c("shiny", "tidyverse", "sf", "leaflet", "leaflet.extras",  "leaflet.extras2", "mapview", "crosstalk", "RColorBrewer", "bslib", "bsicons", "dplyr", "rsconnect", "rlang", "plotly", "reactable", "data.table"))'

# Copy app to /srv/shiny-server/
COPY app.R /srv/shiny-server/

# Copy www to /srv/shiny-server/
COPY www/* /srv/shiny-server/www/

COPY Data_Processed/ /srv/shiny-server/Data_Processed/

RUN mkdir -p /srv/shiny-server/app_cache/sass

RUN chown -R shiny:shiny /srv/shiny-server/


WORKDIR /srv/shiny-server/

EXPOSE 3838
