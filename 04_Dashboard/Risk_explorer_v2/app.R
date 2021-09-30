library(shiny)
library(leaflet)
library(RColorBrewer)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(plotly)
library(png)
source("functions_server.R")
source("functions_ui.R")
#risk_data = sf::st_read("data/Drought.v3.shp")
#risk = sf::st_transform(risk_data, 4326)

# setwd("R:\\02_PROJECTS\\01_P_330001\\127_UNCHAIN\\03__Work\\WP4\\03_Casestudy\\R_Scripts\\04_Dashboard\\Risk_explorer_v2")

risk = sf::st_read("data/Risk_H1_45.shp") %>% 
    sf::st_transform(4326)

exposure = sf::st_read("data/Exposure_smooth.shp") %>% 
    sf::st_transform(4326)

vulnerability = sf::st_read("data/Vulnerability_smooth.shp") %>% 
    sf::st_transform(4326)

hazard1 = sf::st_read("data/H1_45.shp") %>% 
    sf::st_transform(4326)

hazard2 = sf::st_read("data/H2_45.shp") %>% 
    sf::st_transform(4326)

hazard3 = sf::st_read("data/H1_85.shp") %>% 
    sf::st_transform(4326)

hazard4 = sf::st_read("data/H2_85.shp") %>% 
    sf::st_transform(4326)

vul_rast_stack = terra::rast("data/Vulnerability_stack.tif")

vul_cluster = sf::st_read("data/DS3.shp") %>% 
    sf::st_transform(4326)

vul_cluster$DS3 <- as.numeric(vul_cluster$DS3)


# Creates the colorbins for the maps
bin_pal_risk = mapcolor(risk$Risk_H1_45)
bin_pal_exp = mapcolor(exposure$Exp_Index)
bin_pal_haz1 = mapcolor(hazard1$H1_45_norm)
bin_pal_haz2 = mapcolor(hazard2$Risk_H2_45)
bin_pal_haz3 = mapcolor(hazard3$H1_85_norm)
bin_pal_haz4 = mapcolor(hazard4$H2_85_norm)
bin_pal_vul = mapcolor(vulnerability$Vul_index)
bin_pal_acc = mapcolor(risk$V1_acc_nor)
clus_pal_vul <- colorFactor(c("red", "green", "blue"), vul_cluster$DS3)


# Defines what is shows when hovering over a polygon in a map
labels1 = hover_labels("Risk Index", risk$Risk_H1_45) 
labels2 = hover_labels("Exposure Index", exposure$Exp_Index)
labels3a = hover_labels("Hazard Index", hazard1$H1_45_norm)
labels3b = hover_labels("Hazard Index", hazard2$Risk_H2_45)
labels3c = hover_labels("Hazard Index", hazard3$H1_85_norm)
labels3d = hover_labels("Hazard Index", hazard4$H2_85_norm)
labels4 = hover_labels("Vulnerability Index", vulnerability$Vul_index)
labels5 = hover_labels("Accessibility Index", risk$V1_acc_nor) 




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
            menuItem("Impact Chain", tabName = "Impact_Chain", icon = icon("dashboard")),
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
            
            tabItem(tabName = "Impact_Chain",
                    fluidRow(
                        images("IC")
                    )
            ), 

            tabItem(tabName = "Risk",
                    fluidRow(
                        maps("riskmap"),
                        plots("risk_bar"), 
                        texts("risk_info")
                      )
            ), 
            
            tabItem(tabName = "Hazard",
                    fluidRow(
                        maps("hazardmap1"),
                        maps("hazardmap2"),
                        maps("hazardmap3"),
                        maps("hazardmap4"),
                    )
            ), 
            
            tabItem(tabName = "Vulnerability",
                    fluidRow(
                        maps("vulnerabilitymap"),
                        plots("vul_bar"),
                        maps("vul_clus")
                    )
            ), 
            
            tabItem(tabName = "Exposure",
                    fluidRow(
                        maps("exposuremap"), 
                        plots("exp_bar"),
                        tables("indicator_table")
                        
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
    hazard1$id <- 1:nrow(hazard1)
    hazard2$id <- 1:nrow(hazard2)
    hazard3$id <- 1:nrow(hazard3)
    hazard4$id <- 1:nrow(hazard4)
    

# This function observes whether a polygon has been clicked and updates the barchart accordingly  
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
   
    
    # Creates the Index maps
    output$riskmap <- mapping(risk, bin_pal_risk, risk$Risk_H1_45, labels1, "Risk Index")
    output$exposuremap <- mapping(exposure, bin_pal_exp, exposure$Exp_Index, labels2, "Exposure Index")
    
    output$hazardmap1 <- mapping(hazard1, bin_pal_haz1, hazard1$H1_45_norm, labels3a, "RCP 4.5, 2021-2050")
    output$hazardmap2 <- mapping(hazard2, bin_pal_haz2, hazard2$Risk_H2_45, labels3b, "RCP 4.5, 2071-2100")
    output$hazardmap3 <- mapping(hazard3, bin_pal_haz3, hazard3$H1_85_norm, labels3c, "RCP 8.5, 2021-2050")
    output$hazardmap4 <- mapping(hazard4, bin_pal_haz4, hazard4$H2_85_norm, labels3d, "RCP 8.5, 2071-2100")
    
    output$vulnerabilitymap <- mapping(vulnerability, bin_pal_vul, vulnerability$Vul_index, labels4, "Vulnerability Index")
    output$accessmap <- mapping(risk, bin_pal_acc, risk$V1_acc_nor, labels5, "Accessibility Index")
    
    
    # Creates the map that shows the clusters
    output$vul_clus <- cluster_map(vul_cluster, clus_pal_vul, vul_cluster$DS3, "Clusters")
    
   
    # Creates the ggplot2 barcharts
    output$risk_bar <- var_chart(risk_click,"Indicators")    
    output$vul_bar <- var_chart(vulnerability_click, "Vulnerability Indicators")
    output$exp_bar <- var_chart(exposure_click, "Exposure Indicators")
    
    # Plotly
    
    output$facets<- renderPlotly({
        gplot(vul_rast_stack) + facet_wrap(~variable) +
        geom_tile(aes(fill=value)) + coord_equal()
    })
    
    # Text
    
    output$risk_info <- renderText(HTML("<I>Can</I> <em>this</em> <strong>happen</strong>?"))
    
    output$IC <- renderImage(
        list(src = "images/UNCHAIN_Duerre_Wirkungskette.PNG",
             width = 1600,
             height = 1000)
    )
    
    
    data_sources <- data.frame(Indicator = c("Accessibility",
                                             "Soil function evaluation: Habitat for soil organsism",
                                             "Soil function evaluation: Natural soil fertility",
                                             "Soil map: Water conditions",
                                             "Soil map: Gassland quality",
                                             "Diversity of Plants in Agriculture",
                                             "Homesteads" ,  
                                             "Imperviousness change",
                                             "Agriculturally disadvantaged areas",
                                             "Small Woody Features", 
                                             "Inhabitants per sector: Primary sector",
                                             "Accomodation and gastronomy",
                                             "Landscape- and Lake-conservation",
                                             "Nature reserves",
                                             "Forested Areas",
                                             "Industrial Water supply"),
                               
                               Year = c("2015","2015", "2015", "2019", "2019", "2016(?)", "2019", "2006-2012", "2019", "2015","2013","2013","2015","2015","2016","unnkown"),
                               Data_source = c("http://www.statistik.at/web_en/classifications/regional_breakdown/urban_rural/index.html",
                                               "https://www.data.gv.at/katalog/en/dataset/bodenfunktionsbewertung-land-salzburg",
                                               "https://www.data.gv.at/katalog/en/dataset/bodenfunktionsbewertung-land-salzburg",
                                               "https://bfw.ac.at/rz/bfwcms.web?dok=9644",
                                               "https://bfw.ac.at/rz/bfwcms.web?dok=9644",
                                               "RESPECT",
                                               "https://www.data.gv.at/katalog/dataset/6936ee45-ab14-4f61-8fef-1ba34789d743",
                                               "https://land.copernicus.eu/pan-european/high-resolution-layers/imperviousness",
                                               "https://www.bmlrt.gv.at/land/laendl_entwicklung/berggebiete-benachteiligte_gebiete/benachteiligte_geb.html",
                                               "https://land.copernicus.eu/pan-european/high-resolution-layers/small-woody-features/small-woody-features-2015?tab=metadata",
                                               "http://www.statistik.at/web_de/klassifikationen/regionale_gliederungen/regionalstatistische_rastereinheiten/index.html",
                                               "http://www.statistik.at/web_de/klassifikationen/regionale_gliederungen/regionalstatistische_rastereinheiten/index.html",
                                               "https://service.salzburg.gv.at/ogd/client/showDetail/8fca789d-988f-404a-97d8-a8c2d417d766",
                                               "https://service.salzburg.gv.at/ogd/client/showDetail/f035e1ef-9b98-4d77-b2ad-1daf6013e6b3",
                                               "https://service.salzburg.gv.at/ogd/client/showDetail/d9ca9622-5be5-444a-b6f3-1beb6d11b755",
                                               "Sent via email"
                               ))
    xx <- xtable::xtable(data_sources)
    
    output$indicator_table <- renderTable(
        xx
    )
}

shinyApp(ui, server)
