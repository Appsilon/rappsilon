saveShapefile <- function(shape, directory, layerName) {
  rgdal::writeOGR(shape, directory, layerName, driver="ESRI Shapefile")
}

saveGeojson <- function(shape, filepath) {
  rgdal::writeOGR(shape, filepath, "OGRGeoJSON", driver="GeoJSON")
}

