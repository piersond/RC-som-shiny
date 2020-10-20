library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# # Leaflet bindings are a bit slow; for now we'll just sample to compensate
# set.seed(100)
# RC_data <- allzips[sample.int(nrow(allzips), 10000),]
# # By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# # will be drawn last and thus be easier to see
# RC_data <- RC_data[order(RC_data$centile),]

RC_data <- RC_database
colnames(RC_data)
drop_cols <- c('google_id', 'addit_contact_email', 'addit_contact_person', 'author_email', 'author_orcid_id',
               'author_PersonName', 'coarse_tot', 'curator_email', 'curator_organization', 'curator_PersonName', 'data_file',
               'experiments', 'gradient', 'header_row', 'key_version', 'location_name', 'merge_align', 'modification_date',
               'NA_1', 'NA_2', 'network', 'site_code', 'time_series', 'sample_collector') 


function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      # addTiles(
      #   urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      #   attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      # ) 
      
        #Good base maps
          #OpenStreetMap.Mapnik
          #Esri.WorldImagery
          #
      
      #addProviderTiles("OpenStreetMap.Mapnik", options = providerTileOptions(noWrap = TRUE)) %>%
    addProviderTiles(
      "OpenStreetMap",
      # give the layer a name
      group = "OpenStreetMap"
    ) %>%
      addProviderTiles(
        "Stamen.Toner",
        group = "Stamen.Toner"
      ) %>%
      addProviderTiles(
        "Stamen.Terrain",
        group = "Stamen.Terrain"
      ) %>%
      addProviderTiles(
        "Esri.WorldStreetMap",
        group = "Esri.WorldStreetMap"
      ) %>%
      addProviderTiles(
        "Wikimedia",
        group = "Wikimedia"
      ) %>%
      addProviderTiles(
        "CartoDB.Positron",
        group = "CartoDB.Positron"
      ) %>%
      addProviderTiles(
        "Esri.WorldImagery",
        group = "Esri.WorldImagery"
      ) %>%
      # add a layers control
      addLayersControl(
        baseGroups = c(
          "OpenStreetMap", "Stamen.Toner",
          "Stamen.Terrain", "Esri.WorldStreetMap",
          "Wikimedia", "CartoDB.Positron", "Esri.WorldImagery"
        ),
        # position it on the topleft
        position = "topleft"
      ) %>%
    
      setView(lng = -116.75, lat = 43.16, zoom = 11)
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  ptsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(RC_data[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(RC_data,
      lat >= latRng[1] & lat <= latRng[2] &
        long >= lngRng[1] & long <= lngRng[2])
  })

  # Precalculate the breaks we'll need for the two histograms
  #centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks

  output$histCentile <- renderPlot({
    #DP SKIP FEATURE
    return(NULL)
    
    # # If no zipcodes are in view, don't plot
    # if (nrow(ptsInBounds()) == 0)
    #   
    # 
    # hist(ptsInBounds()$centile,
    #   breaks = centileBreaks,
    #   main = "SuperZIP score (visible zips)",
    #   xlab = "Percentile",
    #   xlim = range(allzips$centile),
    #   col = '#00DD00',
    #   border = 'white')
  })

  output$scatterCollegeIncome <- renderPlot({
    #DP SKIP FEATURE
    return(NULL)
    
    # # If no zipcodes are in view, don't plot
    # if (nrow(ptsInBounds()) == 0)
    #   return(NULL)
    # 
    # print(xyplot(income ~ college, data = ptsInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
  })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- input$size

    if (colorBy == "superzip") {
      # Color and palette are treated specially in the "superzip" case, because
      # the values are categorical instead of continuous.
      colorData <- ifelse(RC_data$centile >= (100 - input$threshold), "yes", "no")
      pal <- colorFactor("viridis", colorData)
    } else {
      colorData <- RC_data[[colorBy]]
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    }

    if (sizeBy == "superzip") {
      # Radius is treated specially in the "superzip" case.
      radius <- ifelse(RC_data$centile >= (100 - input$threshold), 30000, 3000)
    } else {
      radius <- RC_data[[sizeBy]] / max(RC_data[[sizeBy]], na.rm = T) * 30000
    }

    leafletProxy("map", data = RC_data) %>%
      clearShapes() %>%
      addCircles(~long, ~lat, radius=5, layerId=~uniqueID, 
                 stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
        layerId="colorLegend") %>%
      addPolygons(data=rc_watersheds,
                  col = 'black',
                  stroke = TRUE, 
                  weight=2,
                  opacity=1,
                  fillColor="grey90",
                  fillOpacity = 0.05, 
                  smoothFactor = 2)
  })

  # Show a popup at the given location
  showZipcodePopup <- function(uniqueID, lat, lng) {
    selectedZip <- RC_data[RC_data$uniqueID == uniqueID,]
    content <- as.character(tagList(
      tags$h4("ID:", as.character(selectedZip$uniqueID)),
      tags$strong(HTML(sprintf("%s, %s",
        selectedZip$lat, selectedZip$long
      ))), tags$br(),
      sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
      sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
      sprintf("Adult population: %s", selectedZip$adultpop)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = uniqueID)
  }

  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })


  ## Data Explorer ###########################################

  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  output$ziptable <- DT::renderDataTable({
    df <- RC_data %>% 
      .[,setdiff(names(.),drop_cols)] %>%
      #{if (!is.null(input$dataset != "")) filter(., layer_top <= input$minDepth) else filter(., layer_top <= 10000)} %>%
      # filter(
      #   layer_top <= input$minDepth,
      #   layer_bot <= input$maxDepth) %>%
      #   {if (!is.null(input$dataset != "")) filter(., Dataset == input$dataset)} %>%
        #is.null(input$dataset) | Dataset %in% input$datasets#,
        #is.null(input$cities) | City %in% input$cities,
        #is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      #) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', lat, '" data-long="', long, '" data-zip="', uniqueID, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df, outputId = "ziptable")
    
    DT::datatable(df, options = list(ajax = list(url = action), lengthMenu = c(10, 50, 100), pageLength = 50), escape = FALSE, class = "display nowrap")
    })
}

  
  
  

#   
#   observe({
#     cities <- if (is.null(input$states)) character(0) else {
#       filter(cleantable, State %in% input$states) %>%
#         `$`('City') %>%
#         unique() %>%
#         sort()
#     }
#     stillSelected <- isolate(input$cities[input$cities %in% cities])
#     updateSelectizeInput(session, "cities", choices = cities,
#       selected = stillSelected, server = TRUE)
#   })
# 
#   observe({
#     zipcodes <- if (is.null(input$states)) character(0) else {
#       cleantable %>%
#         filter(State %in% input$states,
#           is.null(input$cities) | City %in% input$cities) %>%
#         `$`('Zipcode') %>%
#         unique() %>%
#         sort()
#     }
#     stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
#     updateSelectizeInput(session, "zipcodes", choices = zipcodes,
#       selected = stillSelected, server = TRUE)
#   })
# 
#   observe({
#     if (is.null(input$goto))
#       return()
#     isolate({
#       map <- leafletProxy("map")
#       map %>% clearPopups()
#       dist <- 0.5
#       zip <- input$goto$zip
#       lat <- input$goto$lat
#       lng <- input$goto$lng
#       showZipcodePopup(zip, lat, lng)
#       map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
#     })
#   })
# 
#   output$ziptable <- DT::renderDataTable({
#     df <- cleantable %>%
#       filter(
#         Score >= input$minScore,
#         Score <= input$maxScore,
#         is.null(input$states) | State %in% input$states,
#         is.null(input$cities) | City %in% input$cities,
#         is.null(input$zipcodes) | Zipcode %in% input$zipcodes
#       ) %>%
#       mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
#     action <- DT::dataTableAjax(session, df, outputId = "ziptable")
# 
#     DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
#   })
# }
