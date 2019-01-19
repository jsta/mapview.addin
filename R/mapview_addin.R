#' @title mapview Addin
#' @description View spatial objects within the Rstudio IDE
#'
#' @return sf object
#' @importFrom shiny callModule paneViewer observeEvent stopApp runGadget textInput updateTextInput div fluidPage mainPanel shinyServer
#' @importFrom mapview mapview
#' @importFrom shinydashboard dashboardHeader dashboardSidebar dashboardBody dashboardPage
#' @importFrom leaflet setView leafletOutput renderLeaflet
#' @importFrom rstudioapi getActiveDocumentContext
#' @export
#' @examples \dontrun{
#' library(sf)
#' fname <- system.file("shape/nc.shp", package="sf")
#' dt <- st_read(fname)
#' dt
#' }
mapviewAddin <- function() {

  body <- dashboardBody(
    fluidPage(
      mainPanel(
        leafletOutput("mapplot"),
        mapview::mapviewOutput("test")
      ))
  )

  ui <- dashboardPage(dashboardHeader(), dashboardSidebar(), body,
                      skin = "black")

  server <- shinyServer(function(input, output, session) {
    ct <- getActiveDocumentContext()

    TEXT       <- ct$selection[[1]]$text
    OBJECTNAME <- ifelse(TEXT == '', 'geom', TEXT)
    SF_OBJECT  <- NULL

    # test selected text an sf object
    try({
      SF_OBJECT <- base::get(TEXT)
      if(!('sf' %in% class(SF_OBJECT))) {
        message(paste0("selected object is not Spatial but is of class: ",
                       class(SF_OBJECT)))
        stopApp()
        }
    })

    if('sf' %in% class(SF_OBJECT)){
      m <- mapview(SF_OBJECT)
      output$mapplot <- renderLeaflet({m@map})
    }

    session$onSessionEnded(function() {
      stopApp()
    })
  })

  viewer <- paneViewer(600)
  runGadget(ui, server, viewer = viewer)
}
