library(shiny)
library(leaflet)
library(RColorBrewer)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(plotly)
library(png)

#risk_data = sf::st_read("data/Drought.v3.shp")
#risk = sf::st_transform(risk_data, 4326)

risk = sf::st_read("data/Risk_H1_45.shp") %>% 
  sf::st_transform(4326)

exposure = sf::st_read("data/Exposure_smooth.shp") %>% 
  sf::st_transform(4326)

vulnerability = sf::st_read("data/Vulnerability_smooth.shp") %>% 
  sf::st_transform(4326)

vul_rast_stack = terra::rast("data/Vulnerability_stack.tif")

mapcolor <- function(values){
  colorBin('RdBu', values, reverse = TRUE)
}

bin_pal_risk = mapcolor(risk$Risk_H1_45)
bin_pal_exp = mapcolor(exposure$Exp_Index)
bin_pal_haz = mapcolor(risk$H1_45_norm)
bin_pal_vul = mapcolor(vulnerability$Vul_index)
bin_pal_acc = mapcolor(risk$V1_acc_nor)

hover_labels <- function(title, value){
  sprintf(paste0("<strong>",title,"</strong><br/>%.1f%%"), value) %>% lapply(htmltools::HTML)
}

labels1 = hover_labels("Risk Index", risk$Risk_H1_45) 
labels2 = hover_labels("Exposure Index", exposure$Exp_Index)
labels3 = hover_labels("Hazard Index", risk$H1_45_norm)
labels4 = hover_labels("Vulnerability Index", vulnerability$Vul_index)
labels5 = hover_labels("Accessibility Index", risk$V1_acc_nor) 

maps <- function (whichone){
  column(width = 6,
         box(width = NULL, solidHeader = TRUE,
             leafletOutput(whichone, height = 700)
         ))
}

plots <- function (whichone){
  column(width = 6, 
         box(width = NULL, solidHeader = TRUE,
             plotOutput(outputId = whichone, height = 700)))
}

texts <- function (whichone){
  column(width = 6, 
         box(width = NULL, solidHeader = TRUE,
             textOutput(outputId = whichone)))
}



dbHeader <- dashboardHeader(title = "Risk of agricultural drought in Austria",
                            tags$li(a(href = 'https://www.vestforsk.no/en/project/unpacking-climate-impact-chains-new-generation-action-and-user-oriented-climate-change-risk',
                                      img(src = 'logo.png',
                                          title = "Company Home", height = "50px"),
                                      style = "padding-top:1px; padding-bottom:10px;"),
                                    class = "dropdown"))
ui <- dashboardPage(skin = "black",
                    dbHeader,
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Risk", tabName = "Risk", icon = icon("dashboard")),
                        menuItem("Hazard", tabName = "Hazard", icon = icon("dashboard")),
                        menuItem("Vulnerability", tabName = "Vulnerability", icon = icon("dashboard")),
                        menuItem("Exposure", tabName = "Exposure", icon = icon("dashboard")),
                        menuItem("Indicators", tabName = "Indicators", icon = icon("th"))
                      )
                    ),
                    dashboardBody(
                      # tags$head(
                      #     tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                      # ),
                      tabItems(
                        
                        tabItem(tabName = "Risk",
                                fluidRow(
                                  maps("riskmap"),
                                  plots("risk_bar"), 
                                  texts("risk_info")
                                )
                        ), 
                        
                        tabItem(tabName = "Hazard",
                                fluidRow(
                                  maps("hazardmap")
                                )
                        ), 
                        
                        tabItem(tabName = "Vulnerability",
                                fluidRow(
                                  maps("vulnerabilitymap"),
                                  plots("vul_bar")
                                )
                        ), 
                        
                        tabItem(tabName = "Exposure",
                                fluidRow(
                                  maps("exposuremap"), 
                                  plots("exp_bar")
                                )
                        ),
                        
                        tabItem(tabName = "Indicators",
                                fluidRow(
                                  column(width = 12,
                                         box(width = NULL, solidHeader = TRUE,
                                             plotlyOutput("facets", height = 1000)
                                         )
                                  )
                                )
                        )
                      ) 
                    )
)


server <- function(input, output, session) {
  
  risk$id <- 1:nrow(risk)
  exposure$id <- 1:nrow(exposure)
  vulnerability$id <- 1:nrow(vulnerability)
  
  click_monitor <- function(id_current, id_match, val, nm, cat){
    id_current <- id_current
    idx <- id_match==id_current
    
    val <- val
    nm <- nm
    cat <- cat
    
    data.frame(
      value = val[idx],
      name = nm, 
      cat = cat
    )
  }
  
  risk_click <- reactive({
    click_monitor(input$riskmap_shape_click$id, 
                  risk$id,
                  c(risk$V1_acc_nor, risk$V2_soil_no, risk$V3_fert_no, risk$V4_wasver_, risk$V5_gruen_n, risk$V6_div_nor, risk$V7_acco_no, risk$V8_seal_no,
                    risk$V9_bengeb_, risk$V10_swf_no, risk$V11_nature, risk$V12_water_, risk$E1_prim_no, risk$E2_agri_no, risk$H1_45_norm),
                  c("Accessibility","Habitat for soil organisms","Natural soil fertility","Soil water conditions", "Grassland quality", "Diversity in agricultural crop", "Gastronomy & tourist accomodation", 
                    "Change in imperviousness","Agriculturally disadvantaged areas","Small Woody Features", "Nature reserves & conservation areas", "Water source availability",
                    "Population working in primary sector","Agricultural fields", "Variability in precipitation"),
                  c("Vulnerability","Vulnerability","Vulnerability","Vulnerability","Vulnerability","Vulnerability","Vulnerability","Vulnerability","Vulnerability",
                    "Vulnerability","Vulnerability","Vulnerability","Exposure","Exposure","Hazard")
    )})
  
  vulnerability_click <- reactive({
    click_monitor(input$vulnerabilitymap_shape_click$id,
                  vulnerability$id,
                  c(vulnerability$V1_acc_nor, vulnerability$V2_soil_no, vulnerability$V3_fert_no, vulnerability$V4_wasver_, vulnerability$V5_gruen_n, vulnerability$V6_div_nor, 
                    vulnerability$V7_acco_no, vulnerability$V8_seal_no,
                    vulnerability$V9_bengeb_, vulnerability$V10_swf_no, vulnerability$V11_nature, vulnerability$V12_water_),
                  c("Accessibility","Habitat for soil organisms","Natural soil fertility","Soil water conditions", "Grassland quality", "Diversity in agricultural crop", "Gastronomy & tourist accomodation", 
                    "Change in imperviousness","Agriculturally disadvantaged areas","Small Woody Features", "Nature reserves & conservation areas", "Water source availability"),
                  c("Vulnerability","Vulnerability","Vulnerability","Vulnerability","Vulnerability","Vulnerability","Vulnerability","Vulnerability","Vulnerability",
                    "Vulnerability","Vulnerability","Vulnerability")
    )})
  
  exposure_click <- reactive({
    click_monitor(input$exposuremap_shape_click$id,
                  exposure$id,
                  c(exposure$E1_prim_no, exposure$E2_agri_no),
                  c("Population working in primary sector","Agricultural fields"),
                  c("Exposure","Exposure")
    )
  })
  
  mapping <- function(data, palette, values, labels, title_legend){
    renderLeaflet({
      leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% 
        setView(lng = 13.1176, lat = 47.4662, zoom = 9) %>% 
        addPolygons(data = data, color = "#444444", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.7,
                    fillColor = ~palette(values),
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE),
                    label = labels, layerId = ~id) %>% 
        addLegend(data = data, pal = palette, title = title_legend, 
                  values = ~values, position = "bottomright")
    })
  } 
  
  output$riskmap <- mapping(risk, bin_pal_risk, risk$Risk_H1_45, labels1, "Risk Index")
  output$exposuremap <- mapping(exposure, bin_pal_exp, exposure$Exp_Index, labels2, "Exposure Index")
  output$hazardmap <- mapping(risk, bin_pal_haz, risk$H1_45_norm, labels3, "Hazard Index (RCP 4.5, 2021-2050")
  output$vulnerabilitymap <- mapping(vulnerability, bin_pal_vul, vulnerability$Vul_index, labels4, "Vulnerability Index")
  output$accessmap <- mapping(risk, bin_pal_acc, risk$V1_acc_nor, labels5, "Accessibility Index")
  
  
  var_chart <- function(whereclick, title){
    renderPlot({
      
      whereclick() %>% 
        ggplot(aes(x=reorder(name,value), y = value, fill = cat)) +
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
  
  
  output$risk_bar <- var_chart(risk_click,"Indicators")    
  output$vul_bar <- var_chart(vulnerability_click, "Vulnerability Indicators")
  output$exp_bar <- var_chart(exposure_click, "Exposure Indicators")
  
  # Plotly
  
  output$facets<- renderPlotly({
    gplot(vul_rast_stack) + facet_wrap(~variable) +
      geom_tile(aes(fill=value)) + coord_equal()
  })
  
  output$risk_info <- renderText("test")
}

shinyApp(ui, server)
