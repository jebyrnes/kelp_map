library(shiny)
library(sf)
library(mregions) #devtools::install_github("ropensci/mregions")
library(dplyr)
library(leaflet)
library(tidyr)
library(readxl)
library(bslib)

###
## Load up the data
###
m <- st_read("ocean_data/mar_ecoreg_data_sum.geojson") |>
  filter(!is.na(genus))
names(m) <- tolower(names(m))

# load fields
# fields from https://gmed.auckland.ac.nz/layersd.html
# min, 25%, mean, med, 75%, and max 
# fields <- read_excel("ocean_data/data_fields.xlsx") |>
#   mutate(`File Name Code` = gsub("_x", "", `File Name Code`))

er <-  mr_shp(key = "Ecoregions:ecoregions", maxFeatures = 250) |>
  filter(ecoregion %in% m$ecoregion)

###
## User Interface
###
ui <- fluidPage(
  titlePanel("Kelp Environmental Tolerances", 
             windowTitle = "Kelp Environmental Tolerances"),
  
  HTML("Click on an ecoregion to see information about the local genera of kelp's environmental tolerances.<br><br>"),
  HTML("See <a href=https://gmed.auckland.ac.nz/layersd.html>here</a> for definition of environmental fields."),
  
  layout_columns(
    leafletOutput("map"),
  
    layout_columns(uiOutput("field_select"),
                   tableOutput("kelp_info"),
                   col_widths = c(12, 12))
  )  
  
)

###
## Server
###
server <- function(input, output){
  
  ## A reactive for data
  dat <- reactive({
    # print(m |>   filter(ecoregion %in% input$map_shape_click$id))
    
    if(length(input$map_shape_click$id)==0) return(NA)

   d <-  m |>
      filter(ecoregion %in% input$map_shape_click$id)
   
   d |> pivot_longer(aq_primprod_min:tideaverage_p75) |>
     filter(!is.na(value))
  })
  
  ## An output for active fields
  output$field_select <- renderUI({
    if(length(input$map_shape_click$id)==0){
      return(    selectizeInput("show_fields",
                                "Choose what fields you wish to explore:",
                                choices = "",
                                multiple = TRUE)) #blank return if nothing selected
    }

    # otherwise...
    fieldnames <- dat() |>
      pull(name) |>
      unique()

   list( 

    selectizeInput("show_fields",
                   "Choose what fields you wish to explore:",
                   choices = fieldnames,
                   multiple = TRUE))
  })

  
  ## An initial map with **only** elements that one' change
  output$map <- renderLeaflet({
    
    leaflet() |>
      addProviderTiles("Esri.WorldTopoMap") |>
      addPolygons( 
        data = m,
        color = "darkgreen",
        weight = 1.5,
        layerId = ~ecoregion)
    
  })
  
  
  ## An observe statement to update the map
  observeEvent(input$map_shape_click, {
    

    # here we use leafletProxy()
    leafletProxy(mapId = "map") |>
      addPolygons( #reset
        data = m,
        color = "darkgreen",
        weight = 1.5,
        layerId = ~ecoregion) |>
      addPolygons( 
        data = dat(),
        color = "pink",
        weight = 1.5,
        layerId = ~ecoregion) 
      
  })
  
  output$kelp_info <- renderTable({
    if(is.null(dat())) return(NULL)
    
    if(length(input$show_fields)==0) return(
      dat() |> 
        as_tibble() |>
        dplyr::select(genus) |>
        group_by(genus) |>
        slice(1L) |>
        ungroup()
      )
    
    #print( dat() )
    #print( input$show_fields)
   # data.frame(hello = 1)
    
    dat() |> 
      as_tibble() |>
      dplyr::filter(name %in% input$show_fields) |> 
      dplyr::select(c(genus, name, value)) |>
      group_by(genus, name, value) |>
      slice(1L) |>
      ungroup()
    })
  
}

shinyApp(ui = ui, server = server)