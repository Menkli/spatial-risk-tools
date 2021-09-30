# Defines the panels in the ui which show maps
maps <- function (whichone){
  column(width = 6,
         box(width = NULL, solidHeader = TRUE,
             leafletOutput(whichone, height = 600)
         ))
}

# Same as maps but smaller- Currently in use to show the hazard maps.
little_maps <- function (whichone){
  column(width = 6,
         box(width = NULL, solidHeader = TRUE,
             leafletOutput(whichone, height = 500)
         ))
}

# Defines the panels in the ui which show plots
plots <- function (whichone){
  column(width = 6, 
         box(width = NULL, solidHeader = TRUE,
             plotOutput(outputId = whichone, height = 400)))
}

# For the pie chart
pie <- function (whichone){
  column(width = 3, 
         box(width = NULL, solidHeader = TRUE,
             plotOutput(outputId = whichone, height = 300)))
}

# For the dropdown menu
layerSelect45 <- function (whichone){
  column(width = 6, 
         box(width = NULL, solidHeader = TRUE,
             selectInput(inputId = "attribute",
                         label= "RCP 4.5, 2021-2050: Anzeige wechseln", 
                         choices = c("Risikoindex" = "Risk_H1_45", 
                                     "Expositionsindex" ="Exp_Index",
                                     "Verwundbarkeitsindex" = "Vul_index",
                                     "Klimagefahr: Niederschlagsvariabilität Jun-Aug" = "H1_45_norm",
                                     "Erreichbarkeit" = "V1_acc_nor",
                                     "Bodenfunktionsbewertung: Lebensraum für Bodenorganismen" = "V2_soil_no",
                                     "Bodenfunktionsbewertung: Natürliche Bodenfruchtbarkeit" = "V3_fert_no",
                                     "Bodenkarte: Wasserverhältnisse" = "V4_wasver_",
                                     "Bodenkarte: Grünlandwert" = "V5_gruen_n",
                                     "Vielfalt landwirtschaftlicher Nutzpflanzen" = "V6_div_nor",
                                     "Beherbergungs- und Gastronomiebetriebe" = "V7_acco_no",
                                     "Vernderung der Bodenversiegelung" = "V8_seal_no",
                                     "Landwirtschaftlich benachteiligte Gebiete" = "V9_bengeb_",
                                     "Kleinräumige Gehölzstrukturen" = "V10_swf_no", 
                                     "Landschaftsschutzgebiete und Naturschutzgebiete" = "V11_nature",
                                     "Nutzwasserversorgung" = "V12_water_",
                                     "Einwohner nach Wirtschaftszweig: Primärer Sektor" = "E1_prim_no",
                                     "Landwirtschaftlich genutzte Flächen (Feldstücke)" = "E2_agri_no"),
                         selected = "Risk_H1_45")))
}

# For the dropdown menu to select a layer to be displayed in the Risiko section
layerSelect85 <- function (whichone){
  column(width = 6, 
         box(width = NULL, solidHeader = TRUE,
             selectInput(inputId = "attribute2",
                         label= "RCP 8.5, 2021-2050: Anzeige wechseln", 
                         choices = c("Risikoindex" = "Risk_H1_85", 
                                     
                                     "Klimagefahr: Niederschlagsvariabilität" = "H1_85_norm"),
                         selected = "Risk_H1_85")))
}

# Plots that span over the whole page
fullsize_plots <- function (whichone){
  column(width = 12, 
         box(width = NULL, solidHeader = TRUE,
             plotOutput(outputId = whichone, height = 800)))
}

# To display tables that span over the whole page
tables <- function (whichone){
  column(width = 12, 
         box(width = NULL, solidHeader = TRUE,
             tableOutput(outputId = whichone)))
}

# Defines the panels in the ui which show texts
texts <- function (whichone){
  column(width = 6, 
         box(width = NULL, solidHeader = TRUE,
             textOutput(outputId = whichone)))
}

# To plot images
images <- function (whichone){
 
              imageOutput(outputId = whichone)
}

# The sliders to control the layer opacity
sliders <- function(id){
  column(width = 12,
         box(sliderInput(id, "Transparenz", min = 0, max = 1,
                         value = 1, step = 0.1)
         ))
}


