## @knitr leaflet_map

library(dplyr)
library(sf)
library(leaflet)
library(htmlwidgets)
library(rdrop2)


update_index = FALSE
ancillaries_dir <- file.path(PROJ_ROOT, 'ancillaries')

parks <-
  read.table(file.path(ancillaries_dir, 'parks.txt'), header=TRUE)
networks <-
  read.table(file.path(ancillaries_dir, 'networks.txt'), header=TRUE) %>%
  mutate(network_name=factor(network_name, levels=network_name))
unit_boundaries <-
  st_read(file.path(ancillaries_dir, 'unit_boundaries.geojson'), quiet=TRUE)

leaflet_overlay <- unit_boundaries %>%
  left_join(parks, by=c('UNIT_CODE'='park_code')) %>%
  left_join(networks, by='network_code')

mapbox_light_template <-
  'https://api.mapbox.com/styles/v1/mapbox/light-v9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoibHphY2htYW5uIiwiYSI6ImNpcW1oODczZTAwcjBnc2pmaGRhYjVudHIifQ.LeGAHvHXv36-vorTmuNtSg'
overlay_pal <- colorFactor(
  palette=RColorBrewer::brewer.pal(5, 'Set1'),
  domain=leaflet_overlay$network_name
)
m <- leaflet() %>%
  addTiles(urlTemplate=mapbox_light_template) %>%
  addPolygons(data=as(leaflet_overlay, 'Spatial'),
              stroke=TRUE, weight=3, opacity=1, fillOpacity=.2,
              color=~overlay_pal(network_name),
              fillColor= ~overlay_pal(network_name),
              popup=~UNIT_NAME) %>%
  addScaleBar(position='bottomleft') %>%
  addLegend(position='topright', title='Network', opacity=0.8, pal=overlay_pal,
            values=leaflet_overlay$network_name
  )
m
if(update_index) {
  saveWidget(m, file=file.path(PROJ_ROOT, 'maps', 'index.html'))
  drop_upload(file.path(PROJ_ROOT, 'maps', 'index.html'), dest = 'public/sev_leaflet_map')
}
