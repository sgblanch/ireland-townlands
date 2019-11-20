#!/usr/bin/env Rscript --vanilla

library(dplyr)
library(sf)
library(tmap)

# https://www.townlands.ie/static/downloads/civil_parishes.zip
parishes <- st_read(file.path("data", "civil_parishes.shp"), options = "ENCODING=UTF-8")

# https://www.townlands.ie/static/downloads/townlands.zip
townlands <- st_read(file.path("data", "townlands.shp"), options = "ENCODING=UTF-8")

# dump parish data to CSV
parishes.out <- st_drop_geometry(parishes)
parishes.out <- parishes.out[, c("NAME_TAG", "NAME_GA", "NAME_EN", "ALT_NAME", "ALT_NAME_G", "CO_NAMES", "ATTRIBUTIO")]
write.csv( parishes.out, file.path("data", "civil_parishes.csv"), fileEncoding = "UTF-8")

# dump townland data to CSV
townlands.out <- st_drop_geometry(townlands)
townlands.out <- townlands.out[, c("NAME_TAG", "NAME_GA", "NAME_EN", "ALT_NAME", "ALT_NAME_G", "ATTRIBUTIO", "CO_NAME", "CP_NAME", "ED_NAME", "BAR_NAME", "T_IE_URL")]
write.csv(townlands.out, file.path("data", "townlands.csv"), fileEncoding = "UTF-8")

dir.create("maps", showWarnings = FALSE)

counties <- c("Kilkenny", "Offaly", "Tipperary", "Waterford")

map_parish <- function(parish) {
  townlands_in_parish <- rbind(townlands[townlands$CP_OSM_ID == parish$OSM_ID, ], townlands.lost[parish, ])
  townlands_in_parish$BAR_NAME <- droplevels(townlands_in_parish$BAR_NAME)
  
  outdir <- file.path("maps", parish$CO_NAMES, paste(levels(townlands_in_parish$BAR_NAME), collapse=", "))
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

  # layer 0: parish
  parish.map <- tm_shape(parish)
  parish.map <- parish.map + tm_fill()
  # layer 1: townlands
  parish.map <- parish.map + tm_shape(townlands_in_parish)
  parish.map <- parish.map + tm_polygons("MAP_COLORS", ncol = 5)
  # parish.map <- parish.map + tm_legend(legend.show = FALSE)
  # layer 2: parish outline and scale
  parish.map <- parish.map + tm_shape(parish)
  parish.map <- parish.map + tm_borders("red")
  parish.map <- parish.map + tm_scale_bar()
  # layer 3: townland names
  parish.map <- parish.map + tm_shape(townlands_in_parish)
  parish.map <- parish.map + tm_text("NAME_TAG") # auto.placement = TRUE
  tmap_save(parish.map, file.path(outdir, paste0(parish$NAME_TAG, ".pdf")), width = 10, height = 10, units = "in")
}

# poor little lost townlands without a parish...
townlands.lost <- townlands[is.na(townlands$CP_OSM_ID), ]

parishes.filtered <- filter(parishes, grepl(paste(counties, collapse = "|"), CO_NAMES))
parishes.filtered$NAME_TAG <- droplevels(parishes.filtered$NAME_TAG)
by(parishes.filtered, parishes.filtered$OSM_ID, map_parish)
