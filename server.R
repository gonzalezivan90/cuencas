library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(viridisLite)

users <- c('Ecuadorz', 'zPeru', 'IColombia')
vvars <- c(
  "Tamaño cuenca original" = "km2Nosn",
  "Tamaño cuenca ajustado" = "km2Hort",
  "Validado" = "validated",
  "Revisado" = "revised",
  "Diferencia en Km2 entre cuencas" = "differencekm2",
  "Diferencia en % entre cuencas" = "differenceperc",
  "Fuente" = "sou"
  #"Caudal promedio anual" = 'annualav',
  #"Caudal minimo anual" = 'annualmn',
  #"Caudal maximo anual" = 'annualmx'
)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
# set.seed(100)
# zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# # By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# # will be drawn last and thus be easier to see
# zipdata <- zipdata[order(zipdata$centile),]

function(input, output, session) {
  
  XX <<- "NA"
  YY <<- "NA"
  
  ## Interactive Map ###########################################
  
  # Create the map
  basemap <- leaflet()  %>% addTiles() %>% 
    addCircleMarkers(data = qlPts, 
                     layerId = ~id2,
                     label = ~id2, 
                     group = 'Points', radius = 2) %>%
    addProviderTiles( "OpenStreetMap", group = "OpenStreetMap" ) %>%
    addProviderTiles( "Esri.WorldImagery", group = "WorldImagery" ) %>%
    addProviderTiles( "Esri.OceanBasemap", group = "OceanBasemap" ) %>%
    addProviderTiles( "Esri.WorldPhysical", group = "WorldPhysical" ) %>%
    addLayersControl( 
      baseGroups = c("OpenStreetMap", "WorldImagery",
                     "OceanBasemap", 'WorldPhysical')) %>%
    
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
    XX <<- "NA"
    YY <<- "NA"
    output$inxxcord <-  renderText({paste0("X: ", XX)})
    output$inxycord <- renderText({paste0("Y: ", YY)})
  })
  
  output$map <- renderLeaflet({
    basemap
  })
  
  output$inxxcord <-  renderText({isolate("X: NA")})
  output$inxycord <- renderText({isolate("Y: NA")})
  
  ## Chance color points ----
  isolate({observe({
    
    selCol <- input$inxcolor
    # selCol <- "km2Nosn"
    # selCol <- 'sou'
    #print(selCol)
    #print(vvars)
    selVar <<- vvars[vvars %in% selCol]
    # selVar <- "km2Nosn"
    print(selVar)

    
    qlPts$tempVar <- as.data.frame(qlPts[, selVar])[, selVar]

    if (any(selVar %in% c('validated', 'sou', 'revised')) ) {
      
      qlPts$tempVarFact <- as.numeric(as.factor(qlPts$tempVar))
      uVals <- unique(cbind.data.frame(tempVarFact = qlPts$tempVarFact,
                                       tempVar = qlPts$tempVar))
      #uVals <- unique(qlPts$tempVarFact)
      #colorPal <- colorNumeric(c("Yellow", "Purple"), domain = range(uVals$tempVarFact))
      colorPal <- colorNumeric(palette = viridis(100), domain = range(uVals$tempVarFact))
      
      qlPts$tempVar <- qlPts$tempVarFact
      colorsLegend <- colorPal(uVals$tempVarFact) #c('red', 'blue'), 
      labelsLegend <- uVals$tempVar
      
    } else if( selVar %in% c('km2Nosn','km2Hort','km2Nosn','km2Nosn', 'differencekm2', 'differenceperc')) {
      
      (domain <- range(qlPts$tempVar, na.rm = TRUE))
      qlPts$tempVarFact <- cut(qlPts$tempVar, 
                               quantile(qlPts$tempVar, na.rm = TRUE,
                                        probs = seq(0, 1, by = 0.2)))
      
      # make palette
      # colorQuantile("RdYlBu", countries$gdp_md_est, n = 5)
      #colorPal <- colorNumeric(palette = viridis(100), domain = domain)
      #colorPal <- colorQuantile(palette = viridis(10), domain = domain, n = 10)
      
      uVals <- unique(
        cbind.data.frame(tempVarFact = qlPts$tempVarFact))
      #                         , colVar = colorPal(qlPts$tempVarFact)))
      uVals <- uVals[order(uVals$tempVarFact), , drop = FALSE]
      
      colorPal <- colorFactor(palette = viridis(6), 
                              domain = uVals$tempVarFact) 
      uVals$colVar = colorPal(uVals$tempVarFact)
      #, bins = 5, pretty = FALSE)
    }
    
    
    output$map <- renderLeaflet({
      
      llmap <- leaflet()  %>% addTiles() %>% 
        addCircleMarkers(data = qlPts,
                         layerId = ~id2,
                         color = ~colorPal(tempVarFact),
                         label = ~id2,
                         group = 'Points', radius = 2) %>%
        addProviderTiles( "OpenStreetMap", group = "OpenStreetMap" ) %>%
        addProviderTiles( "Esri.WorldImagery", group = "WorldImagery" ) %>%
        addProviderTiles( "Esri.OceanBasemap", group = "Esri.OceanBasemap" ) %>%
        addProviderTiles( "Esri.WorldPhysical", group = "Esri.WorldPhysical" ) %>%
        addLayersControl(
          position = "topleft",
          baseGroups = c("OpenStreetMap", "WorldImagery",
                         "Esri.OceanBasemap", 'Esri.WorldPhysical'))
        
        
        if (any(selVar %in% c('validated', 'sou', 'revised') )) {
          llmap <- llmap %>% 
            addLegend(title = names(selVar),
                      #title = NULL,
                      opacity = 0.7,
                      colors = colorPal(uVals$tempVarFact), #c('red', 'blue'), 
                      labels = uVals$tempVar, #c('Snip', 'Original'), 
                      position = "bottomleft")
          
        } else if( selVar %in% c('km2Nosn','km2Hort','km2Nosn','km2Nosn', 'differencekm2', 'differenceperc')) {
          # llmap <- llmap %>% 
          #   addLegend(title = names(selVar), opacity = 0.7,
          #             pal = colorPal, values = (qlPts$tempVar), position = "bottomleft")
          
          llmap <-llmap %>% 
            addLegend(title = names(selVar),
                      #title = NULL,
                      opacity = 0.7,
                      colors = uVals$colVar, #c('red', 'blue'), 
                      labels = uVals$tempVarFact, #c('Snip', 'Original'), 
                      position = "bottomleft")
          
        }
      
      llmap
      # , lng = ~xshp, lat = ~yshp
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
  
  
  
  ## click on map or coordinate ------
  observe({  
    if (is.null(input$map_marker_click) & !is.null (input$map_click)){
      print(input$map_click)
      XX <<- input$map_click$lng
      YY <<- input$map_click$lat
      output$inxxcord <- renderText({(paste0("X: ", XX))})
      output$inxycord <- renderText({(paste0("Y: ", YY))})
    } else if( !is.null(input$map_marker_click) & is.null (input$map_click) ) {
      clk <- input$map_marker_click;
      idLayer <<- clk$id
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
        
        leafletProxy("map") %>% 
          fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>% 
          # removeMarker(layerId = 'NewPoint')  %>% 
          # removeMarker(layerId = 'OriginalPoint') %>%
          
          addPolygons(
            data = xHor, 
            group = 'NewBasin',
            layerId = 'NewBasin',
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
          
          addPolygons(
            data = xBas, 
            group = 'OrigBasin',
            layerId = 'OrigBasin',
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
          
          addCircleMarkers(
            lng = newPt$xnew,
            lat = newPt$ynew, 
            color = 'red',
            group = 'NewPoint', 
            layerId = 'NewPoint',
            radius = 5) %>%
          
          addCircleMarkers(
            lng = newPt$xshp,
            lat = newPt$yshp, 
            color = 'blue', radius = 20,
            layerId = 'OriginalPoint',
            group = 'OriginalPoint'
            
          )  %>%
          
          addLayersControl(
            baseGroups = c("OpenStreetMap", "WorldImagery",
                           "OceanBasemap", 'WorldPhysical'),
            position = 'topleft', 
            overlayGroups = c('Points',
                              'NewPoint',
                              'OriginalPoint',
                              'OrigBasin',
                              'NewBasin'
            ),
            options = layersControlOptions(collapsed = FALSE)) 
      } else {
        updateTextInput(session, inputId = 'outxlog', 
                        value = paste0('No modified coordinate for ', input$inxqlid , ' station'))
      }
    }
  })
  
  
  ## click on map or coordinate ------
  observeEvent(input$map_click, {  
    
    print(input$map_click)
    XX <<- input$map_click$lng
    YY <<- input$map_click$lat
    
    output$inxxcord <- renderText({(paste0("X: ", XX))})
    output$inxycord <- renderText({(paste0("Y: ", YY))})
    
  })
  
  itPos <<- 1
  
  
  ## click on next button ------
  isolate({
    observeEvent(input$inXnext, { 
      
      print('next: itPos')
      print(itPos)
      ## click before
      if(exists('idLayer0') & exists('idLayer')){
        if(idLayer0 == idLayer){ # no new click
          itPos0 <- itPos
        } else { # new click
          itPos <- which(qlPts$id2 == idLayer0)
        }
        itPos <- itPos0
      } else {  ## no click before
        # itPos <- 1
        idLayer0 <- idLayer <- (newPoints$qlid[itPos])
      }
      #print(itPos)
      
      newPt <- newPoints[ which(newPoints$qlid == qlPts$id2[itPos]),]
      #newPt <- newPoints[ which(newPoints$qlid == idLayer),]
      
      
      selBas <- basins[ which(basins$id2 == newPt$qlid),  ]
      xBas <- selBas[selBas$bastype == 'nosn', ]
      #xSni <- selBas[selBas$bastype == 'snip', ]
      xHor <- selBas[selBas$bastype == 'hort', ]
      #selBas$color <- c('red', 'blue', 'black')
      #print(selBas)
      #print(xHor)
      updateTextInput(session, inputId = 'inxqlid', value = idLayer)
      itPos <<- itPos + 1
      print(itPos)
      print('next: itPos ---')
      
      
      
      if(nrow(xHor) >= 1){
        #print(' OK A')
        #leaflet() %>% addTiles() 
        
        bbox <- st_bbox(selBas) %>% as.vector()
        
        leafletProxy("map") %>% 
          fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>% 
          removeMarker(layerId = 'NewPoint')  %>% 
          removeMarker(layerId = 'OriginalPoint') %>%
          removeShape(layerId = 'OrigBasin')  %>% 
          removeShape(layerId = 'NewBasin') %>%
          
          addPolygons(
            data = xHor, 
            group = 'NewBasin',
            layerId = 'NewBasin',
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
          
          addPolygons(
            data = xBas, 
            group = 'OrigBasin',
            layerId = 'OrigBasin',
            
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
          
          addCircleMarkers(
            lng = newPt$xnew,
            lat = newPt$ynew, 
            color = 'red',
            group = 'NewPoint', 
            layerId = 'NewPoint',
            
            radius = 5) %>%
          
          addCircleMarkers(
            lng = newPt$xshp,
            lat = newPt$yshp, 
            color = 'blue', radius = 20,
            layerId = 'OriginalPoint',
            group = 'OriginalPoint'
            
          )  %>%
          
          addLayersControl(
            baseGroups = c("OpenStreetMap", "WorldImagery",
                           "OceanBasemap", 'WorldPhysical'),
            position = 'topleft', 
            overlayGroups = c('Points',
                              'NewPoint',
                              'OriginalPoint',
                              'OrigBasin',
                              'NewBasin'
            ),
            options = layersControlOptions(collapsed = FALSE)) 
        
      } else {
        updateTextInput(session, inputId = 'outxlog', 
                        value = paste0('No modified coordinate for ', input$inxqlid , ' station'))
      }
      
    })
  })
  
  
  ## click on prev button ------
  isolate({
    observeEvent(input$inXprev, { 
      
      print('--- itPos')
      print(itPos)
      itPos <<- itPos - 1
      if(itPos == 0){
        itPos <<- nrow(qlPts)
      }
      print(itPos)
      
      ## click before
      if(exists('idLayer0') & exists('idLayer')){
        if(idLayer0 == idLayer){ # no new click
          itPos0 <- itPos
        } else { # new click
          itPos <- which(qlPts$id2 == idLayer0)
        }
        itPos <- itPos0
      } else {  ## no click before
        # itPos <<- 1
        itPos0 <- idLayer <- (newPoints$qlid[itPos])
      }
      #print(itPos)
      
      newPt <- newPoints[ which(newPoints$qlid == qlPts$id2[itPos]),]
      #newPt <- newPoints[ itPos,]
      
      selBas <- basins[ which(basins$id2 == newPt$qlid),  ]
      xBas <- selBas[selBas$bastype == 'nosn', ]
      #xSni <- selBas[selBas$bastype == 'snip', ]
      xHor <- selBas[selBas$bastype == 'hort', ]
      #selBas$color <- c('red', 'blue', 'black')
      #print(selBas)
      
      print(nrow(xHor))
      print('--- end')
      
      updateTextInput(session, inputId = 'inxqlid', value = idLayer)
      
      
      if(nrow(xHor) >= 1){
        #print(' OK A')
        #leaflet() %>% addTiles() 
        
        bbox <- st_bbox(selBas) %>% as.vector()
        
        leafletProxy("map") %>% 
          fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>% 
          removeMarker(layerId = 'NewPoint')  %>% 
          removeMarker(layerId = 'OriginalPoint') %>%
          removeShape(layerId = 'OrigBasin')  %>% 
          removeShape(layerId = 'NewBasin') %>%
          
          addPolygons(
            data = xHor, 
            group = 'NewBasin',
            layerId = 'NewBasin',
            
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
          
          addPolygons(
            data = xBas, 
            group = 'OrigBasin',
            layerId = 'OrigBasin',
            
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
          
          addCircleMarkers(
            lng = newPt$xnew,
            lat = newPt$ynew, 
            color = 'red',
            group = 'NewPoint', 
            layerId = 'NewPoint',
            
            radius = 5) %>%
          
          addCircleMarkers(
            lng = newPt$xshp,
            lat = newPt$yshp, 
            color = 'blue', radius = 20,
            layerId = 'OriginalPoint',
            group = 'OriginalPoint'
            
          )  %>%
          
          addLayersControl(
            baseGroups = c("OpenStreetMap", "WorldImagery",
                           "OceanBasemap", 'WorldPhysical'),
            position = 'topleft', 
            overlayGroups = c('Points',
                              'NewPoint',
                              'OriginalPoint',
                              'OrigBasin',
                              'NewBasin'
            ),
            options = layersControlOptions(collapsed = FALSE)) 
        
      } else {
        updateTextInput(session, inputId = 'outxlog', 
                        value = paste0('No modified coordinate for ', input$inxqlid , ' station')
        )
      }
    })
  })
  
  ## click point and add baasin ------
  
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
        # removeMarker(layerId = 'NewPoint')  %>%
        # removeMarker(layerId = 'OriginalPoint') %>%
        addPolygons(
          data = xHor,
          group = 'NewBasin',
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
        
        addPolygons(
          data = xBas,
          group = 'OrigBasin',
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
        
        addCircleMarkers(
          lng = newPt$xnew,
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
        
        addLayersControl(
          baseGroups = c("OpenStreetMap", "WorldImagery",
                         "OceanBasemap", 'WorldPhysical'),
          position = 'topleft',
          overlayGroups = c('Points',
                            'OriginalPoint',
                            'NewPoint',
                            'OrigBasin',
                            'NewBasin'
          ),
          options = layersControlOptions(collapsed = FALSE))
      
      
    } else {
      updateTextInput(session, inputId = 'outxlog', 
                      value = paste0('No modified coordinate for ', input$inxqlid , ' station'))
    }
  })
  
  
  ## sent comment ------
  
  isolate({
    observeEvent(input$gocoment,{
      
      if(!is.na(input$inxqlid) & (input$inxqlid %in% qlPts$id2) & (input$inxuser %in% users)){
        
        outComm <- cbind.data.frame(
          datestamp = format(Sys.time(), "%Y-%M-%d_%H-%M-%S"),
          user = input$inxuser,
          qlid = input$inxqlid,
          comment = input$inxcomment,
          x = XX,
          y = YY,
          isvalid = input$isvalid
          
        )
        outName <- paste0(outComm$datestamp, '___',input$inxqlid, '.csv') 
        
        write.csv(outName, file = paste0('comm/', outName))
        
        
        output$outxlog <- renderText({
          isolate(
            paste0('Comment on station ', input$inxqlid , ' was sent!')
          )
        })
        
        # updateTextInput(session, inputId = 'outxlog', 
        #                 value = )
      }
    })
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


# git clone https://github.com/gonzalezivan90/cuencas.git
# chmod ugo+rwx . 
# sudo chown -R shiny:shiny

# sudo shiny; cd /srv...; git add . ; git commit -m "Change coordiantes()"; git push
# git pull main --rebase --autostash
# sudo chown -R shiny:shiny .
# git stash
# remove before commit, split or lost it
# git pull connectscape |||  git pull --rebase --autostash || git pull origin HEAD
