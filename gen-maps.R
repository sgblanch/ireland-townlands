#!/usr/bin/env Rscript --vanilla

library(sf)
library(tmap)

# https://www.townlands.ie/static/downloads/townlands.zip

townlands <- st_read(file.path("data", "townlands.shp"), options = "ENCODING=UTF-8")

if(!dir.exists("maps"))
  dir.create("maps")

#for (county in levels(townlands$CO_NAME)) {
for (county in c("Kilkenny", "Offaly", "Tipperary", "Waterford")) {
  if(!dir.exists(file.path("maps", county)))
    dir.create(file.path("maps", county))
  
  county.shp <- townlands[townlands$CO_NAME==county, ]
  county.shp$CP_NAME <- droplevels(county.shp$CP_NAME)
  for (parish in levels(county.shp$CP_NAME)) {
    parish.shp <- county.shp[county.shp$CP_NAME==parish, ]
    parish.shp$NAME_TAG <- droplevels(parish.shp$NAME_TAG)
    parish.map <- tm_shape(parish.shp)
    parish.map <- parish.map + tm_polygons("MAP_COLORS", ncols = 5)
    parish.map <- parish.map + tm_text("NAME_TAG") # auto.placement = TRUE
    parish.map <- parish.map + tm_legend(legend.show = FALSE)
    parish.map <- parish.map + tm_scale_bar()
    #parish.map <- parish.map + tm_compass()
    tmap_save(parish.map, file.path("maps", county, paste0(parish, ".pdf")), width=10, height=10, units = "in")
  }
}