#!/usr/bin/env Rscript --vanilla

library(sf)
library(tmap)

# https://www.townlands.ie/static/downloads/townlands.zip

townlands <- st_read(file.path("data", "townlands.shp"), options = "ENCODING=UTF-8")

if(!dir.exists("maps"))
  dir.create("maps", county)

#for (county in levels(townlands$CO_NAME)) {
for (county in c("Kilkenny", "Offaly", "Tipperary", "Waterford")) {
  if(!dir.exists(file.path("maps", county)))
    dir.create(file.path("maps", county))
  
  county.shp <- townlands[townlands$CO_NAME==county, ]
  county.shp$CP_NAME <- droplevels(county.shp$CP_NAME)
  for (parish in levels(county.shp$CP_NAME)) {
    parish.shp <- county.shp[county.shp$CP_NAME==parish, ]
    parish.shp$NAME_TAG <- droplevels(parish.shp$NAME_TAG)
    parish.map <- tm_shape(parish.shp) + tm_fill("NAME_TAG") + tm_text("NAME_TAG")
    tmap_save(parish.map, file.path("maps", county, paste0(parish, ".pdf")), width=10, height=10, units = "in")
  }
}