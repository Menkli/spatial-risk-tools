
mapping_x <- function(dat, layer, title, transp, outline){

    palette <- colorBin(palette = 'RdBu', 
                        layer, 
                        reverse = TRUE,
                        bins = c(0,10,20,30,40,50,60,70,80,90,100),
                        pretty = TRUE)
    labels <- sprintf(paste0("<strong>",title,"</strong><br/>%.1f%%"), layer) %>% lapply(htmltools::HTML)
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% # Stamen.TerrainBackground
      setView(lng = 13.1176, lat = 47.5, zoom = 9) %>% 
      addPolylines(data = outline,
                  color = "#a6a6a6", 
                  weight = 2, 
                  smoothFactor = 0.5,
                  opacity = 1.0, 
                  #fillColor = "transpartent",
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)) %>% 
      addPolygons(data = dat, 
                  color = "#444444", 
                  weight = 1, 
                  smoothFactor = 0.5,
                  opacity = 1.0, 
                  fillOpacity = transp,
                  fillColor = ~palette(layer),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  label = labels,
                  layerId = ~id) %>% 
      addLegend(data = dat, pal = palette, title = "Index", 
                values = ~layer, position = "topright") 
  
} 

# Creates the maps that show the Indices
mapping <- function(data, values,title){
  renderLeaflet({
    bins <- c(0,10,20,30,40,50,60,70,80,90,100)
    palette <- colorBin(palette = 'RdBu', 
                        domain = values, 
                        reverse = TRUE,
                        pretty = FALSE,
                        bins = bins)
    labels <- sprintf(paste0("<strong>",title,"</strong><br/>%.1f%%"), values) %>% lapply(htmltools::HTML)
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = 13.1176, lat = 47.5, zoom = 9) %>% 
      
      addPolygons(data = data, 
                  color = "#444444", 
                  weight = 1, 
                  smoothFactor = 0.5,
                  opacity = 1.0, 
                  fillOpacity = 0.8,
                  fillColor = ~palette(values),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  label = labels, 
                  layerId = ~id) %>% 
      addLegend(data = data, 
                pal = palette, 
                title = title_legend, 
                values = ~values, 
                position = "topleft"
                ) 
  })
} 

# For the hazard maps with the rasters in it
rastermapper <- function(values, outline, title){
  renderLeaflet ({
    # Make maps with raster images
    pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FF0000"), values(values),
                        na.color = "transparent")
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addRasterImage(values, colors = pal, opacity = 0.8) %>%
      addPolylines(data = outline,
                   color = "#ffffff", 
                   weight = 2, 
                   smoothFactor = 0.5,
                   opacity = 1.0) %>%  
                   #fillColor = "transpartent",
                   # highlightOptions = highlightOptions(color = "white", weight = 2,
                   #                                     bringToFront = TRUE)) %>% 
      addLegend(pal = pal, values = values(values),
                title = title) %>% 
      setView(lng = 13.1176, lat = 47.5, zoom = 8.4)
    
  })
}

# Creates the maps that show the clusters. It uses a discrete colour palette. 

cluster_map <- function(data, palette, values, title_legend){
  renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = 13.1176, lat = 47.4662, zoom = 9) %>% 
      addPolygons(data = data, color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.8,
                  fillColor = ~palette(values),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)) %>% 
      addLegend(data = data, pal = palette, title = title_legend, 
                values = ~values, position = "topleft")
  })
}

# This one is not in use currently
indicator_map <- function (data, palette, values, title_legend){
  renderLeaflet({
    palette <- colorBin('RdBu', values, reverse = TRUE)
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = 13.1176, lat = 47.5, zoom = 9) %>% 
      addPolygons(data = data, color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.8,
                  fillColor = ~palette(values),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  label = labels) %>% 
      addLegend(data = data, pal = palette, title = title_legend, 
                values = ~values, position = "topleft") 
  })
}


# This function observes whether a polygon has been clicked and updates the barchart accordingly
click_monitor <- function(id_current, id_match, val, nm, cat){
  if(is.null(id_current)){
    idx <- 1
    val <- val
    nm <- nm
    cat <- cat
    
    data.frame(
      value = val[idx],
      name = nm, 
      cat = cat
    )
  } else if(!is.null(id_current)){
  #id_current <- id_current
  idx <- id_match==id_current
  
  val <- val
  nm <- nm
  cat <- cat
  
  data.frame(
    value = val[idx],
    name = nm, 
    cat = cat
  )}
}

# Creates the ggplot2 barcharts 
var_chart <- function(whereclick, title){
  renderPlot({
    
    whereclick() %>% 
      #ggplot(aes(x=reorder(name,value), y = value, fill = cat)) +
      ggplot(aes(x=name, y = value, fill = cat)) +
      geom_bar(stat = "identity") +
      coord_cartesian(ylim = c(0,100)) +
      scale_fill_solarized() +
      theme_minimal() +
      coord_flip() +
      xlab("") +
      ylab("") +
      facet_grid(cat~., scales ="free",space="free")+
      ggtitle(title) + 
      theme(legend.position="bottom") +
      theme(text = element_text(size = 17)) +
      theme(plot.title = element_text(size = 20))
    
  })
}

# Creates pie charts based on the selected geon, but is currently not in use
# pie_chart <- function(whereclick, title){
#   renderPlot({
#     whereclick() %>% 
#       ggplot(aes(x= "", y = value, fill = cat)) +
#       geom_bar(stat = "identity") +
#       coord_polar("y", start = 0)
#       # ggplot(aes(x="", y=value, fill=cat))+
#       # geom_bar(stat = "identity", width = 1) +
#       # coord_polar("y", start = 0) +
#       # theme_minimal() +
#       # theme(legend.position="bottom") +
#       # theme(text = element_text(size = 17)) +
#       # theme(plot.title = element_text(size = 20))
# 
#   })
# }
