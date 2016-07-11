#' @import magrittr

#' @export
ads_map <- function() {
  leaflet::leaflet() %>%
    leaflet::addProviderTiles("CartoDB.Positron", group = "CartoDB")
}

#' @export
vizualizeSegmentation <- function(segmentation, column = 0) {
  ads_map() %>%
    leaflet::addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
    leaflet::addLayersControl(
      baseGroups = c("CartoDB", "Toner Lite"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) -> leafletMap
  if (column > 0) {
    leafletMap %>% leaflet::addPolygons(data = segmentation, fillOpacity = 0, weight = 1, popup = segmentation[[column]])
  } else {
    leafletMap %>% leaflet::addPolygons(data = segmentation, fillOpacity = 0, weight = 1)
  }
}

#' @export
vizualizeMarkers <- function(dataPoints) {
  ads_map() %>%
    leaflet::addMarkers(data = dataPoints, clusterOptions = leaflet::markerClusterOptions())
}

# This code should be refactored:
vizualizeDataClusteringBySize <- function(observations, polygons, addPopups = T) {
  generatePopup <- function(clusterId, clusterSize, minprice, maxprice) {
    paste(sep = "<br/>",
          paste("Nazwa: ", clusterId),
          paste("Rozmiar: ", clusterSize),
          paste("Zakres cen: ", minprice, "-", maxprice)
    )
  }
  priceRanges <- observations %>% group_by(currentClusterId) %>%
    dplyr::summarize(minprice = min(price), maxprice = max(price), avgprice = mean(price), size = n()) %>%
    ungroup %>%
    mutate(popupData = generatePopup(currentClusterId, size, minprice, maxprice)) %>%
    mutate(color = ifelse(size > 9, "#31a354", ifelse(size > 4, "#fec44f", "#f03b20")))

  polygons@data$color <- priceRanges$color
  polygons@data$popupData <- priceRanges$popupData

  map <- leaflet() %>%
    addProviderTiles("CartoDB.Positron", "Positron") %>%
    addPolygons(data = polygons, group="Wyliczone", color = ~color, weight = 1, popup = ~popupData)

  if (addPopups) {
    centroids <- gCentroid(polygons, byid=TRUE)
    info <-priceRanges[polygons$id == priceRanges$currentClusterId, ] %>%
      mutate(priceRangeText = paste(minprice, "-", maxprice)) %>%
      {.$priceRangeText}
    map <- map %>% addPopups(data = centroids, popup = info, group="Popupy")
  }
  map
}

# This code should be refactored:
vizualizeDataClustering <- function(dataClustering, initialClusters) {
  clustersMap <- dataClustering %>% group_by(initialClusterId) %>% dplyr::summarize(currentCluster = currentClusterId[1]) %>% ungroup
  clustersAssignment <- initialClusters$id %>% as.character %>%
    map(~ which(. == clustersMap$initialClusterId)) %>%
    map(~ clustersMap[., 2]) %>%
    unlist %>% c

  clustersUnion <- maptools::unionSpatialPolygons(initialClusters, clustersAssignment)

  leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = clustersUnion, fillOpacity = 0, weight = 2) %>%
    addMarkers(data = dataClustering, popup = ~as.character(price))
}
