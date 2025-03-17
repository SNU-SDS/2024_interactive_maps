# Loading Packages --------------------------------------------------------

library(sf)
library(readxl)
library(tmap)
library(tidyverse)
library(leaflet)
library(leaflet.minicharts)
library(htmlwidgets)


# Data Preparing ----------------------------------------------------------

sido <- st_read("sido.shp", options = "ENCODING=EUC-KR")
sido <- sido |> st_transform(crs = 4326)
coords <- as.data.frame(st_coordinates(st_centroid(sido)))

sgg <- st_read("bnd_sigungu_00_2020_2020_4Q.shp", options = "ENCODING = EUC-KR")
sgg <- sgg |> st_transform(crs = 4326)

religion <- read_xlsx("religion.xlsx")
religion <- religion |> 
  cbind(coords)
religion

religion$Y[9] <- (religion$Y[8] + religion$Y[1]) / 2

# Mapping -----------------------------------------------------------------


tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"

basemap <- leaflet(sido, width = "100%", height = "800px") |> 
  setView(127.5, 35, 7)

quantile(sgg$pop_den)

bins <- c(0, 20, 100, 1000, 10000, 20000, Inf)
pal <- colorBin("YlOrRd", domain = sgg$pop_den, bins = bins)
colors <- c("#7161A3", "#F5A8B3", "#BE475A", "#B8B8B8", "#57C2DC")

m <- basemap |> 
  addPolygons(
  weight = 1,
  opacity = 1,
  color = "#C0BD96",
  fillColor = "#E7E5D4",
  fillOpacity = 0.7
) |> 
  addMinicharts(
    religion$X, religion$Y,
    type = "pie",
    chartdata = religion[, c("불교", "개신교", "천주교", "기타", "종교 없음")],
    colorPalette = colors,
    width = 70 * sqrt(religion$total) / sqrt(max(religion$total)),
    transitionTime = 0
  ) |> 
  addLabelOnlyMarkers(
    lng = religion$X,
    lat = religion$Y,
    label = lapply(
      paste0("<strong>", religion$sido, "</strong><br>", format(religion$total, big.mark = ",")),
      htmltools::HTML
    ),
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = "bottom",
      textOnly = TRUE,
      style = list(
        "color" = "black",
        "font-size" = "10px",
        "font-family" = "Arial",
        "font-weight" = "bold"
      )
    )
  )

m

saveWidget(m, file = "piemap.html")





