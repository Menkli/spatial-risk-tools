# Defines the panels in the ui which show maps
maps <- function (whichone){
  column(width = 6,
         box(width = NULL, solidHeader = TRUE,
             leafletOutput(whichone, height = 600)
         ))
}

# Defines the panels in the ui which show plots
plots <- function (whichone){
  column(width = 6, 
         box(width = NULL, solidHeader = TRUE,
             plotOutput(outputId = whichone, height = 600)))
}

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

images <- function (whichone){
 
              imageOutput(outputId = whichone)
}
