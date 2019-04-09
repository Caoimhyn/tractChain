library(shiny)
library(leaflet.extras)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(DT)
library(dplyr)
library(osrm)

ui <- navbarPage("TractChain", id="nav",

           tabPanel("Interactive map",
                    div(class="outer",

                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),

                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),

                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",

                                      h2("BlockChain explorer"),
                                      actionButton("startTraject", "Démarrer le trajet"),
                                      plotOutput("histCentile", height = 200),
                                      plotOutput("scatterCollegeIncome", height = 250)
                        ),

                        tags$div(id="cite",
                                 'Data compiled for ', tags$em('personnal project'), ' by Kevin POTARD (2019).'
                        )
                    )
           ),tabPanel("Trajet",
                      DTOutput("trajet", width = "100%")
                      )
)

server <- function(input, output, session) {
  # global variable
  this_table = data.frame()

  # Leaflet bindings are a bit slow; for now we'll just sample to compensate
  set.seed(100)

    ## Interactive Map ###########################################

    # Create the map
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 6, dragging = T)) %>%
      addProviderTiles(provider = "OpenStreetMap.France") %>%
      addSearchOSM() %>%
      setView(lng = 2.43, lat = 46.53, zoom = 7) %>%
      setMaxBounds(
        lng1 = 2.43 + 9,
        lat1 = 46.53 + 12,
        lng2 = 2.43 - 7,
        lat2 = 46.53 - 10
      )
  })

  this_table <- reactiveVal(this_table)

  ## Observe mouse clicks and add markers
  observeEvent(input$map_click, {
    ## Get the click info like had been doing
    click <- input$map_click
    clat <- click$lat
    clng <- click$lng

    ## Add the maker to the map proxy
    ## not need to re-render the whole thing
    ## the markers a group, "markers", so you can
    ## then do something like hide all the markers with hideGroup('markers')
    leafletProxy('map') %>% # use the proxy to save computation
      addMarkers(lng = clng,
                 lat = clat,
                 group = 'markers')

    t = rbind(
      data.frame(
        Latitude = round(click$lat,2),
        Longitude = round(click$lng,2)
      ),
      this_table()
    )
    this_table(t)
  })

  observeEvent(input$startTraject,{
    Sys.sleep(1)
    data <- this_table()
    data <- cbind(1:nrow(data), data)
    colnames(data)[1] <- "id"
    for(i in 1:nrow(data)){
      points <- osrmRoute(src = data[i,], dst = data[i+1,])
      Sys.sleep(1000)
    }

    leafletProxy('map') %>%
        addPolylines(points$lon, points$lat, group="trajet")
  })

# Data panel --------------------------------------------------------------

  output$trajet <- renderDT({
    datatable(this_table(), selection = 'single', options = list(dom = 't'))

  })
}

shinyApp(ui, server)
