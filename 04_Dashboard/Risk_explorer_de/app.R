library(shiny)
library(leaflet)
library(RColorBrewer)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(plotly)
library(png)

library(rasterVis)
source("functions_server.R")
source("functions_ui.R")

# To run the app locally, the working directory must be set to the location of the app.R file
#setwd("R:\\02_PROJECTS\\01_P_330001\\127_UNCHAIN\\03__Work\\WP4\\03_Casestudy\\R_Scripts\\04_Dashboard\\Risk_explorer_de")

#LOAD ALL THE FILES________________

# Load vector files
vector_load <- function(file){
    sf::st_read(file) %>% 
        sf::st_transform(4326)
}

risk <- vector_load("data/Risk_H1_45_smooth.shp")
risk2 <- vector_load("data/Risk_H1_85_smooth.shp")
vul_cluster <- vector_load("data/AS2_smooth.shp")
SBG <- vector_load("data/BL_Salzburg.shp")


# Load raster files
raster_load <- function(file){
    terra::rast(file) %>% 
        brick()
}
haz_for_pres <- raster_load("data/Hazard_stack_for_pres.tif")
SPI_count_stack <- raster_load("data/SPI_count_stack.tif")
SPI_delta_stack <- raster_load("data/SPI_delta_stack.tif")
#_________________________________

vul_cluster$AS2 <- as.numeric(vul_cluster$AS2)
clus_pal_vul <- colorFactor(c( "#70B1AE", "#FCE953"), vul_cluster$AS2)


dbHeader <- dashboardHeader(title = "Landwirtschaftliche Dürre in Österreich", titleWidth = 400,
                           tags$li(a(href = 'https://www.vestforsk.no/en/project/unpacking-climate-impact-chains-new-generation-action-and-user-oriented-climate-change-risk',
                                     img(src = 'logo.png',
                                         title = "Company Home", height = "39px"),
                                     style = "padding-top:1px; padding-bottom:10px;"),
                                   class = "dropdown"))
ui <- dashboardPage(skin = "black",
    dbHeader,
    dashboardSidebar(
        sidebarMenu(
            menuItem("Workflow", tabName = "Workflow", icon = icon("bullseye")),
            menuItem("Wirkungskette", tabName = "Impact_Chain", icon = icon("bullseye")), 
            menuItem("Indikatoren", tabName = "Indicators", icon = icon("bullseye")),
            menuItem("Risiko", tabName = "Risk", icon = icon("bullseye")),
            menuItem("Klimagefahr", tabName = "Klimagefahr", icon = icon("bullseye"),
                     menuSubItem("Variabilität", tabName = "Hazard", icon = icon("bullseye")),
                     menuSubItem("SPI-6: Absolut", tabName = "SPI_abs", icon = icon("bullseye")),
                     menuSubItem("SPI-6: Delta", tabName = "SPI_delta", icon = icon("bullseye"))),
            menuItem("Risikocluster", tabName = "Risikocluster", icon = icon("bullseye"))

        )
    ),
    dashboardBody(

        tabItems(
            tabItem(tabName = "Workflow",
                    fluidRow(
                        images("WF")
                    )
            ), 
            tabItem(tabName = "Impact_Chain",
                    fluidRow(
                        images("IC")
                    )
            ), 

            tabItem(tabName = "Risk",
                    fluidRow(
                        sliders("trans"),
                        layerSelect45(),
                        layerSelect85(),
                        maps("riskmap"),
                        maps("riskmap2"),
                        plots("risk_bar"), 
                        plots("risk_bar2")

                      )
            ), 
            
            tabItem(tabName = "Hazard",
                    fluidRow(
                        little_maps("t1_45"),
                        little_maps("t2_45"),
                        little_maps("t1_85"),
                        little_maps("t2_85")
                    )
            ), 
            
            tabItem(tabName = "Risikocluster",
                    fluidRow(
                        maps("vul_clus"),
                        images("cluster_table")
                    )
            ), 

            tabItem(tabName = "SPI_abs",
                    fluidRow(
                        fullsize_plots("SPI_abs")
                )
            ),
            tabItem(tabName = "SPI_delta",
                    fluidRow(
                        fullsize_plots("SPI_del")
                    )
            ),
            tabItem(tabName = "Indicators",
                    fluidRow(
                        tables("indicator_table")
                    )
            )
        ) 
    )
)


server <- function(input, output, session) {

    risk$id <- 1:nrow(risk)
    risk2$id <- 1:nrow(risk2)
    
    
    # To chnage the transparency of the geons layer
    transparency <- reactive({
        input$trans
    })  
    
# This function observes whether a polygon has been clicked and updates the barchart accordingly  
    risk_click <- reactive({
        click_monitor(input$riskmap_shape_click$id, 
                      risk$id,
                      c(risk$V1_acc_nor, risk$V2_soil_no, risk$V3_fert_no, risk$V4_wasver_, risk$V5_gruen_n, risk$V6_div_nor, risk$V7_acco_no, risk$V8_seal_no,
                        risk$V9_bengeb_, risk$V10_swf_no, risk$V11_nature, risk$V12_water_, risk$E1_prim_no, risk$E2_agri_no, risk$H1_45_norm),
                      c("Erreichbarkeit",
                        "Bodenfunktionsbewertung: Lebensraum für Bodenorganismen",
                        "Bodenfunktionsbewertung: Natürliche Bodenfruchtbarkeit",
                        "Bodenkarte: Wasserverhältnisse",
                        "Bodenkarte: Grünlandwert",
                        "Vielfalt landwirtschaftlicher Nutzpflanzen",
                        "Beherbergungs- und Gastronomiebetriebe",
                        "Vernderung der Bodenversiegelung",
                        "Landwirtschaftlich benachteiligte Gebiete",
                        "Kleinräumige Gehölzstrukturen", 
                        "Landschaftsschutzgebiete und Naturschutzgebiete",
                        "Nutzwasserversorgung",
                        "Einwohner nach Wirtschaftszweig: Primärer Sektor",
                        "Landwirtschaftlich genutzte Flächen (Feldstücke)",
                        "Niederschlagsvariabilität Jun-Aug"),
                      c("Verwundbarkeit","Verwundbarkeit","Verwundbarkeit","Verwundbarkeit","Verwundbarkeit","Verwundbarkeit","Verwundbarkeit","Verwundbarkeit","Verwundbarkeit",
                        "Verwundbarkeit","Verwundbarkeit","Verwundbarkeit","Exposition","Exposition","Klimagefahr")
                      )})
    
    risk2_click <- reactive({
        click_monitor(input$riskmap2_shape_click$id, 
                      risk2$id,
                      c(risk2$V1_acc_nor, risk2$V2_soil_no, risk2$V3_fert_no, risk2$V4_wasver_, risk2$V5_gruen_n, risk2$V6_div_nor, risk2$V7_acco_no, risk2$V8_seal_no,
                        risk2$V9_bengeb_, risk2$V10_swf_no, risk2$V11_nature, risk2$V12_water_, risk2$E1_prim_no, risk2$E2_agri_no, risk2$H1_85_norm),
                      c("Erreichbarkeit",
                        "Bodenfunktionsbewertung: Lebensraum für Bodenorganismen",
                        "Bodenfunktionsbewertung: Natürliche Bodenfruchtbarkeit",
                        "Bodenkarte: Wasserverhältnisse",
                        "Bodenkarte: Grünlandwert",
                        "Vielfalt landwirtschaftlicher Nutzpflanzen",
                        "Beherbergungs- und Gastronomiebetriebe",
                        "Vernderung der Bodenversiegelung",
                        "Landwirtschaftlich benachteiligte Gebiete",
                        "Kleinräumige Gehölzstrukturen", 
                        "Landschaftsschutzgebiete und Naturschutzgebiete",
                        "Nutzwasserversorgung",
                        "Einwohner nach Wirtschaftszweig: Primärer Sektor",
                        "Landwirtschaftlich genutzte Flächen (Feldstücke)",
                        "Niederschlagsvariabilität Jun-Aug"),
                      c("Verwundbarkeit","Verwundbarkeit","Verwundbarkeit","Verwundbarkeit","Verwundbarkeit","Verwundbarkeit","Verwundbarkeit","Verwundbarkeit","Verwundbarkeit",
                        "Verwundbarkeit","Verwundbarkeit","Verwundbarkeit","Exposition","Exposition","Klimagefahr")
        )})
   
   
    
    # Creates the Index maps
    
    output$riskmap <- renderLeaflet({
         mapping_x(risk,  risk[[input$attribute]], "Indexwert",transparency(), SBG)
         })
     
    output$riskmap2 <- renderLeaflet({
        mapping_x(risk2,  risk2[[input$attribute2]], "Indexwert",transparency(), SBG)
    })
    
    
    
    
    # Creates the map that shows the clusters
    output$vul_clus <- cluster_map(vul_cluster, clus_pal_vul, vul_cluster$AS2, "Cluster")
    
    #output$v1_acc <- indicator_map(v_acc, bin_pal_acc, v_acc$results__4, "Erreichbarkeit")
   
     # Creates the ggplot2 barcharts
    output$risk_bar <- var_chart(risk_click,"RCP 4.5, 2021-2050: Indikatoren") 
    output$risk_bar2 <- var_chart(risk2_click,"RCP 8.5, 2021-2050:Indikatoren")    
    
    # Creates the pie charts
    # output$risk_pie <- pie_chart(risk_click, "title")
    # Plotly
    
    # output$facets<- renderPlotly({
    #     gplot(vul_rast_stack) + 
    #         facet_wrap(~variable) +
    #         geom_tile(aes(fill=value)) + 
    #         coord_equal() + 
    #         scale_fill_gradient2(low = "#ab374e",
    #                             mid = "white",
    #                              high = "#56B1F7",
    #                              space = "Lab",
    #                              na.value = "grey50",
    #                              guide = "colourbar",
    #                              aesthetics = "fill") 
    # })
    
    
    output$SPI_abs <- renderPlot({
        rasterNames <- c("RCP 4.5 1981-2010", "RCP 4.5 2021-2050", "RCP 4.5 2071-2100", "RCP 8.5 1981-2010", "RCP 8.5 2021-2050", "RCP 8.5 2071-2100")
        rasterVis::levelplot(SPI_count_stack, main="Durschnittliche Anzahl der Jahre mit SPI-6 <= -1 für RCP 4.5 & RCP 8.5 [September]",
                         names.attr=rasterNames,
                         scales=list(draw= FALSE),
                         layout=c(3,2),
                         par.settings = BuRdTheme())

    })
    
    output$SPI_del <- renderPlot({
        rasterNames <- c("RCP 4.5 2021-2050", "RCP 4.5 2071-2100", "RCP 8.5 2021-2050", "RCP 8.5 2071-2100")
        rasterVis::levelplot(SPI_delta_stack, main="Delta der Anzahl der Jahre mit SPI-6 <= -1 für RCP 4.5 & RCP 8.5 [September]",
                             names.attr=rasterNames,
                             scales=list(draw= FALSE),
                             layout=c(2,2),
                             par.settings = BuRdTheme())
        
    })
    
    #output$risk_info <- renderText(HTML("<I>Can</I> <em>this</em> <strong>happen</strong>?"))
    
    # Impact Chain graphic
    output$IC <- renderImage(
        list(src = "images/UNCHAIN_Duerre_Wirkungskette.PNG",
             width = 1600,
             height = 1000), 
             deleteFile = FALSE
    )
    
    # Workflow chart
    output$WF <- renderImage(
        list(src = "images/Workflow_for_Stakeholders_crop.png",
             width = 800,
             heigth = 1000),
             deleteFile = FALSE
    )
    
    output$cluster_table <-renderImage(
        list(src = "images/cluster_tablelle.PNG",
             width = 800,
             heigth = 1000),
             deleteFile = FALSE
    )
    
    # Load and present the data of the indicator table
    data_sources <- read.csv(file = "data\\Final_list_de.csv", header = TRUE, sep = ";")
    xx <- xtable::xtable(data_sources)
    output$indicator_table <- renderTable(
        xx, hover = TRUE
    )
    
# Creates the hazard maps which show raster data
output$t1_45 <- rastermapper(haz_for_pres$H_SPI3_Sep_2021_2050_RCP45,SBG, "RCP 4.5, 2021-2050, Jun-Aug")
output$t2_45 <- rastermapper(haz_for_pres$H_SPI3_Sep_2071_2100_RCP45,SBG, "RCP 4.5, 2071-2100, Jun-Aug")    
output$t1_85 <- rastermapper(haz_for_pres$H_SPI3_Sep_2021_2050_RCP85,SBG, "RCP 8.5, 2021-2050, Jun-Aug")  
output$t2_85 <- rastermapper(haz_for_pres$H_SPI3_Sep_2071_2100_RCP85,SBG, "RCP 8.5, 2071-2100, Jun-Aug")  

}
# Necessary to run the app
shinyApp(ui, server)
