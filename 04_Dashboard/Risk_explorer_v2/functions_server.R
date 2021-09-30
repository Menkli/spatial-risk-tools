# Creates the colorbins for the maps
mapcolor <- function(values){
  colorBin('RdBu', values, reverse = TRUE)
}

# Defines what is shows when hovering over a polygon in a map
hover_labels <- function(title, value){
  sprintf(paste0("<strong>",title,"</strong><br/>%.1f%%"), value) %>% lapply(htmltools::HTML)
}

# Creates the maps that show the Indices
mapping <- function(data, palette, values, labels, title_legend){
  renderLeaflet({
    leaflet() %>% addProviderTiles(providers$Stamen.TerrainBackground) %>% 
      setView(lng = 13.1176, lat = 47.5, zoom = 9) %>% 
      addPolygons(data = data, color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.8,
                  fillColor = ~palette(values),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  label = labels, layerId = ~id) %>% 
      addLegend(data = data, pal = palette, title = title_legend, 
                values = ~values, position = "topleft") 
  })
} 

# Creates the maps that show the clusters. It uses a discrete colour palette. 

cluster_map <- function(data, palette, values, title_legend){
  renderLeaflet({
    leaflet() %>% addProviderTiles(providers$Stamen.TerrainBackground) %>% 
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