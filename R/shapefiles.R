readDataShapefile <- function(dirPath, layer) {
  readOGR(dsn = dirPath, layer = layer, encoding = "UTF-8", verbose = F)
}

readDataGeojson <- function(geojsonPath) {
  readOGR(geojsonPath, "OGRGeoJSON", verbose = F)
}
