convertFromEPSG2180 <- function(shape) {
  spTransform(shape, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
}

voronoiPolygons = function(lat, lon) {
  w <- c(min(c(lon)) - 1, max(c(lon)) + 1, min(c(lat)) - 1, max(c(lat)) + 1)
  if (length(lat) == 1) {
    polys = list(Polygons(list(Polygon(cbind(c(w[1],w[2],w[2],w[1]),c(w[4],w[4],w[3],w[3])))), ID="1"))
  } else {
    z = suppressMessages(deldir(lon, lat, rw = w))
    w = tile.list(z)
    polys = vector(mode='list', length=length(w))
    for (i in seq(along=polys)) {
      pcrds = cbind(w[[i]]$x, w[[i]]$y)
      pcrds = rbind(pcrds, pcrds[1,])
      polys[[i]] = Polygons(list(Polygon(pcrds)), ID=as.character(i))
    }
  }
  id = seq(along=polys) %>% as.character

  SpatialPolygons(polys, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) %>%
    SpatialPolygonsDataFrame(data=data.frame(id = id))
}

getObservationsSegments <- function(segmentation, observations, column) {
  data <- observations
  sp::coordinates(data) <- ~ lon + lat
  sp::proj4string(data) <- sp::proj4string(segmentation)
  sp::over(data, segmentation) %>% as.data.frame -> output
  if (is.character(column) & !(column %in% names(output))) {
    warning(paste(names(output)))
    stop("Selected not available column name")
  }
  output[[column]] %>% unlist
}
