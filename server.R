library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

library(viridisLite)
# Leaflet bindings are a bit slow; for now we'll just sample to compensate
# set.seed(100)
# zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# # By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# # will be drawn last and thus be easier to see
# zipdata <- zipdata[order(zipdata$centile),]

function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  basemap <- leaflet()  %>% addTiles() %>% 
      addCircleMarkers(data = qlPts, 
                       layerId = ~id2,
                       label = ~id2, 
                       group = 'Points', radius = 2) %>%
    addProviderTiles( "OpenStreetMap", group = "OpenStreetMap" ) %>%
    addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" ) %>%
    addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery")) %>%
      addLegend(colors = c('red', 'blue'), 
                labels = c('Snip', 'Original'), opacity = 0.7, title = NULL,
                position = "bottomleft") # , lng = ~xshp, lat = ~yshp
    # addCircleMarkers(data = rv$pts_sp, label = ~ID, group = 'Points',  radius = 5)
    #label = ~ID, group = 'Points'
    #addTiles() # %>% setView(lng = -93.85, lat = 37.45, zoom = 4)
  
  observeEvent( input$inXreset,{
    output$map <- renderLeaflet({
      basemap
    })
    output$inxxcord <-  renderText({isolate("X: NA")})
    output$inxycord <- renderText({isolate("Y: NA")})
  })
  
  output$map <- renderLeaflet({
    basemap
  })
  
  output$inxxcord <-  renderText({isolate("X: NA")})
  output$inxycord <- renderText({isolate("Y: NA")})
  
  
  isolate({observe({
    
    selCol <- input$inxcolor
    print(selCol)
    # selCol <- "km2Nosn"
    selVar <- vars[vars %in% selCol]
    #selVar <- 'sou'
    qlPts$tempVar <- as.data.frame(qlPts[, selVar])[, selVar]
    # vars <- c(
    #   "Tamaño cuenca original" = "km2Nosn",
    #   "Tamaño cuenca ajustado" = "km2Hort",
    #   "Validado" = "validated",
    #   "Diferencia en Km2 entre cuencas" = "differencekm2",
    #   "Diferencia en % entre cuencas" = "differenceperc",
    #   "Fuente" = "source"
    # )
    
    # get domain of numeric data
    
    if (selVar %in% c('validated', 'source') ) {
    
      qlPts$tempVarFact <- as.numeric(as.factor(qlPts$tempVar))
      uVals <- unique(cbind.data.frame(tempVarFact = qlPts$tempVarFact,
                                       tempVar = qlPts$tempVar))
      #uVals <- unique(qlPts$tempVarFact)
      #colorPal <- colorNumeric(c("Yellow", "Purple"), domain = range(uVals$tempVarFact))
      colorPal <- colorNumeric(palette = viridis(100), domain = range(uVals$tempVarFact))
      
      qlPts$tempVar <- qlPts$tempVarFact
        
      # # make map
      # leaflet(qlPts) %>% 
      #   addTiles() %>% 
      #   addCircleMarkers(color = ~colorPal(tempVarFact))  %>%  
      # 
      # addLegend("bottomright",
      #           #colors = c("#FFC125",  "#FFC125", "#8A4117", "#7D0552", "#571B7E"),
      #           colors = colorPal(uVals$tempVarFact),
      #           labels = uVals$tempVar,
      #           title = selVar,
      #           opacity = 1)
      
    } else if( selVar %in% c('km2Nosn','km2Hort','km2Nosn','km2Nosn')) {
      
      (domain <- range(qlPts$tempVar))
      # make palette
      colorPal <- colorNumeric(palette = viridis(100), domain = domain)
      
      
    }
    
    output$map <- renderLeaflet({
      
      leaflet()  %>% addTiles() %>% 
        addCircleMarkers(data = qlPts, 
                         layerId = ~id2,
                         color = ~colorPal(tempVar),
                         label = ~id2, 
                         group = 'Points', radius = 2) %>%
        addProviderTiles( "OpenStreetMap", group = "OpenStreetMap" ) %>%
        addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" ) %>%
        addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery")) %>%
        addLegend(title = names(selVar),
                  #title = NULL,
                  opacity = 0.7,
                  colors = colorPal(uVals$tempVarFact), #c('red', 'blue'), 
                  labels = uVals$tempVar, #c('Snip', 'Original'), 
                  position = "bottomleft") # , lng = ~xshp, lat = ~yshp
      # addCircleMarkers(data = rv$pts_sp, label = ~ID, group = 'Points',  radius = 5)
      #label = ~ID, group = 'Points'
      #addTiles() # %>% setView(lng = -93.85, lat = 37.45, zoom = 4)
    })
  })
  })
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  # zipsInBounds <- reactive({
  #   if (is.null(input$map_bounds))
  #     return(zipdata[FALSE,])
  #   bounds <- input$map_bounds
  #   latRng <- range(bounds$north, bounds$south)
  #   lngRng <- range(bounds$east, bounds$west)
  # 
  #   subset(zipdata,
  #          latitude >= latRng[1] & latitude <= latRng[2] &
  #            longitude >= lngRng[1] & longitude <= lngRng[2])
  # })
  
  
  observe({
    if (is.null(input$map_marker_click) & !is.null (input$map_click)){
    print(input$map_click)
    output$inxxcord <- renderText({isolate(paste0("X: ", input$map_click$lng))})
    output$inxycord <- renderText({isolate(paste0("Y: ", input$map_click$lat))})
    } else if( !is.null(input$map_marker_click) & is.null (input$map_click) ) {
      clk <- input$map_marker_click;
      idLayer <- clk$id
      #print(clk)
      # idLayer <- "per-359"
      
      newPt <- newPoints[ which(newPoints$qlid == idLayer),]
      selBas <- basins[ which(basins$id2 == idLayer),  ]
      xBas <- selBas[selBas$bastype == 'nosn', ]
      #xSni <- selBas[selBas$bastype == 'snip', ]
      xHor <- selBas[selBas$bastype == 'hort', ]
      #selBas$color <- c('red', 'blue', 'black')
      #print(selBas)
      #print(xHor)
      updateTextInput(session, inputId = 'inxqlid', value = idLayer)
      
      if(nrow(xHor) >= 1){
        #print(' OK A')
        #leaflet() %>% addTiles() 
        
        bbox <- st_bbox(selBas) %>% as.vector()
        
        leafletProxy("map") %>% fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>% 
          removeMarker(layerId = 'NewPoint')  %>% 
          removeMarker(layerId = 'OriginalPoint') %>%
          addPolygons(data = xHor, 
                      group = 'Snipped',
                      color = 'red',
                      weight = 2,
                      opacity = 1,
                      dashArray = "3",
                      fillOpacity = 0.2,
                      label = sprintf(
                        "<strong>Station ID: </strong>%s<br>Snip:%g Km<sup>2</sup>",
                        xHor$id2, round(xHor$km2, 3)
                      ) %>% lapply(htmltools::HTML),
                      
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")) %>%
          
          addPolygons(data = xBas, 
                      group = 'Original',
                      color = 'blue',
                      stroke = 10,
                      weight = 2,
                      opacity = 1,
                      dashArray = "3",
                      fillOpacity = 0.2,
                      label = sprintf(
                        "<strong>Station ID: </strong>%s<br>Snip:%g Km<sup>2</sup>",
                        xBas$id2, round(xBas$km2, 3)
                      ) %>% lapply(htmltools::HTML),
                      
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")) %>%
          
          addCircleMarkers(lng = newPt$xnew,
                           lat = newPt$ynew, 
                           color = 'red',
                           group = 'NewPoint', 
                           radius = 5) %>%
          
          addCircleMarkers(
            lng = newPt$xshp,
            lat = newPt$yshp, 
            color = 'blue', radius = 20,
            group = 'OriginalPoint'
          )  %>%
          
          addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
                           position = 'topleft', 
                           overlayGroups = c('Points',
                                             'NewPoint',
                                             'OriginalPoint'
                           ),
                           options = layersControlOptions(collapsed = FALSE)) 
        
        
      }
      
    }
    
  })
    
  
  observeEvent(input$map_marker_click,{
    clk <- input$map_marker_click;
    idLayer <- clk$id
    #print(clk)
    # idLayer <- "per-359"
    
    newPt <- newPoints[ which(newPoints$qlid == idLayer),]
    selBas <- basins[ which(basins$id2 == idLayer),  ]
    xBas <- selBas[selBas$bastype == 'nosn', ]
    #xSni <- selBas[selBas$bastype == 'snip', ]
    xHor <- selBas[selBas$bastype == 'hort', ]
    #selBas$color <- c('red', 'blue', 'black')
    #print(selBas)
    #print(xHor)
    updateTextInput(session, inputId = 'inxqlid', value = idLayer)
    
    if(nrow(xHor) >= 1){
      #print(' OK A')
      #leaflet() %>% addTiles() 
      
      bbox <- st_bbox(selBas) %>% as.vector()
      
      leafletProxy("map") %>% fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>% 
        removeMarker(layerId = 'NewPoint')  %>% 
        removeMarker(layerId = 'OriginalPoint') %>%
        addPolygons(data = xHor, 
                    group = 'Snipped',
                    color = 'red',
                    weight = 2,
                    opacity = 1,
                    dashArray = "3",
                    fillOpacity = 0.2,
                    label = sprintf(
                      "<strong>Station ID: </strong>%s<br>Snip:%g Km<sup>2</sup>",
                      xHor$id2, round(xHor$km2, 3)
                    ) %>% lapply(htmltools::HTML),
                    
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        
        addPolygons(data = xBas, 
                    group = 'Original',
                    color = 'blue',
                    stroke = 10,
                    weight = 2,
                    opacity = 1,
                    dashArray = "3",
                    fillOpacity = 0.2,
                    label = sprintf(
                      "<strong>Station ID: </strong>%s<br>Snip:%g Km<sup>2</sup>",
                      xBas$id2, round(xBas$km2, 3)
                    ) %>% lapply(htmltools::HTML),
                    
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        
        addCircleMarkers(lng = newPt$xnew,
                         lat = newPt$ynew, 
                         color = 'red',
                         group = 'NewPoint', 
                         radius = 5) %>%
        
        addCircleMarkers(
          lng = newPt$xshp,
          lat = newPt$yshp, 
           color = 'blue', radius = 20,
          group = 'OriginalPoint'
        )  %>%
      
      addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
                       position = 'topleft', 
                       overlayGroups = c('Points',
                                         'NewPoint',
                                         'OriginalPoint'
                       ),
                       options = layersControlOptions(collapsed = FALSE)) 
      
      
    }
  })
  
  
  
  
  
  # # Precalculate the breaks we'll need for the two histograms
  # centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks
  # 
  # output$histCentile <- renderPlot({
  #   # If no zipcodes are in view, don't plot
  #   if (nrow(zipsInBounds()) == 0)
  #     return(NULL)
  #   
  #   hist(zipsInBounds()$centile,
  #        breaks = centileBreaks,
  #        main = "SuperZIP score (visible zips)",
  #        xlab = "Percentile",
  #        xlim = range(allzips$centile),
  #        col = '#00DD00',
  #        border = 'white')
  # })
  # 
  # output$scatterCollegeIncome <- renderPlot({
  #   # If no zipcodes are in view, don't plot
  #   if (nrow(zipsInBounds()) == 0)
  #     return(NULL)
  #   
  #   print(xyplot(income ~ college, data = zipsInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
  # })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  # observe({
  #   colorBy <- input$color
  #   sizeBy <- input$size
  #   
  #   if (colorBy == "superzip") {
  #     # Color and palette are treated specially in the "superzip" case, because
  #     # the values are categorical instead of continuous.
  #     colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
  #     pal <- colorFactor("viridis", colorData)
  #   } else {
  #     colorData <- zipdata[[colorBy]]
  #     pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
  #   }
  #   
  #   if (sizeBy == "superzip") {
  #     # Radius is treated specially in the "superzip" case.
  #     radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
  #   } else {
  #     radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
  #   }
  #   
  #   leafletProxy("map", data = zipdata) %>%
  #     clearShapes() %>%
  #     addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
  #                stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
  #     addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
  #               layerId="colorLegend")
  # })
  
  # Show a popup at the given location
  # showZipcodePopup <- function(zipcode, lat, lng) {
  #   selectedZip <- allzips[allzips$zipcode == zipcode,]
  #   content <- as.character(tagList(
  #     tags$h4("Score:", as.integer(selectedZip$centile)),
  #     tags$strong(HTML(sprintf("%s, %s %s",
  #                              selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
  #     ))), tags$br(),
  #     sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
  #     sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
  #     sprintf("Adult population: %s", selectedZip$adultpop)
  #   ))
  #   leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  # }
  
  # When map is clicked, show a popup with city info
  # observe({
  #   leafletProxy("map") %>% clearPopups()
  #   event <- input$map_shape_click
  #   if (is.null(event))
  #     return()
  #   
  #   isolate({
  #     showZipcodePopup(event$id, event$lat, event$lng)
  #   })
  # })
  
  
  ## Data Explorer ###########################################
  
  # observe({
  #   cities <- if (is.null(input$states)) character(0) else {
  #     filter(cleantable, State %in% input$states) %>%
  #       `$`('City') %>%
  #       unique() %>%
  #       sort()
  #   }
  #   stillSelected <- isolate(input$cities[input$cities %in% cities])
  #   updateSelectizeInput(session, "cities", choices = cities,
  #                        selected = stillSelected, server = TRUE)
  # })
  # 
  # observe({
  #   zipcodes <- if (is.null(input$states)) character(0) else {
  #     cleantable %>%
  #       filter(State %in% input$states,
  #              is.null(input$cities) | City %in% input$cities) %>%
  #       `$`('Zipcode') %>%
  #       unique() %>%
  #       sort()
  #   }
  #   stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
  #   updateSelectizeInput(session, "zipcodes", choices = zipcodes,
  #                        selected = stillSelected, server = TRUE)
  # })
  # 
  # observe({
  #   if (is.null(input$goto))
  #     return()
  #   isolate({
  #     map <- leafletProxy("map")
  #     map %>% clearPopups()
  #     dist <- 0.5
  #     zip <- input$goto$zip
  #     lat <- input$goto$lat
  #     lng <- input$goto$lng
  #     showZipcodePopup(zip, lat, lng)
  #     map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
  #   })
  # })
  # 
  # output$ziptable <- DT::renderDataTable({
  #   df <- cleantable %>%
  #     filter(
  #       Score >= input$minScore,
  #       Score <= input$maxScore,
  #       is.null(input$states) | State %in% input$states,
  #       is.null(input$cities) | City %in% input$cities,
  #       is.null(input$zipcodes) | Zipcode %in% input$zipcodes
  #     ) %>%
  #     mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
  #   action <- DT::dataTableAjax(session, df, outputId = "ziptable")
  #   
  #   DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  # })
}