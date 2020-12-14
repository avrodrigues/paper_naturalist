map_module2 <- function(occ.cl,
         institution.source = "institutionCode",
         collection.code = "collectionCode",
         catalog.number = "catalogNumber",
         year.event = "year",
         date.identified = "dateIdentified",
         scientific.name = "species",
         determined.by = "identifiedBy",
         longitude = "decimalLongitude",
         latitude = "decimalLatitude",
         basis.of.rec = "basisOfRecord",
         media.type = "mediaType",
         occ.id = "occurrenceID"){
  require(shiny)
  require(shinyWidgets)
  require(shinydashboard)
  require(leaflet)
  require(raster)
  require(rgdal)
  require(leaflet.extras)
  require(shinyLP)
  
  final.df <- shiny::runApp(list(
    ui = shiny::fluidPage(
      shiny::titlePanel("{naturaList} map_module" ),
      shiny::fluidRow(
        shiny::column(3,
                      uiOutput("species"),
                      shinydashboard::box(width = NULL,
                                          shinyWidgets::checkboxGroupButtons(
                                            inputId = "grbox",
                                            label = "Selected levels:",
                                            choices = c("Level 1" = "1",
                                                        "Level 2" = "2",
                                                        "Level 3" = "3",
                                                        "Level 4" = "4",
                                                        "Level 5" = "5",
                                                        "Level 6" = "6"),
                                            justified = T,
                                            status = 'info',
                                            size = "xs",
                                            direction = "vertical",
                                            checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                             no = icon("remove", lib = "glyphicon")),
                                            selected = c("1",
                                                         "2",
                                                         "3",
                                                         "4",
                                                         "5",
                                                         "6"),
                                            width = "100%"),
                                          shinyWidgets::materialSwitch("del_mkr_button",
                                                                       label = "Delete points with click",
                                                                       status = "danger")
                                          
                                          
                      ),
                      shinydashboard::box(width = NULL, 
                                          solidHeader = T, status = "success",
                                          textOutput("sel_display"),
                                          textOutput("down.class.text"),
                                          actionBttn("done", "Done!",
                                                     style = "fill",
                                                     color = "success",
                                                     size = "sm")
                                          
                      )
        ),
        shiny::column(9,
                      leaflet::leafletOutput("map", height = 500)
                      
        )
      )
      
    ),
    server = function(input, output, session){
      values = shiny::reactiveValues()
      
      
      output$species <- renderUI({
        species <- as.character(unique(occ.cl[,scientific.name]))
        if(length(species) == 1){
          h4(strong(em(textOutput("txt.sp"))))
        }
        
      })
      
      output$txt.sp <- renderText({
        as.character(unique(occ.cl[,scientific.name]))
        #"hoje"
      })
      ## occurrences categories
      
      values$occur <- occ.cl
      
      
      values$Level_1 <- grep("1",  occ.cl$naturaList_levels)
      values$Level_2 <- grep("2",  occ.cl$naturaList_levels)
      values$Level_3 <- grep("3",  occ.cl$naturaList_levels)
      values$Level_4 <- grep("4",  occ.cl$naturaList_levels)
      values$Level_5 <- grep("5",  occ.cl$naturaList_levels)
      values$Level_6 <- grep("6",  occ.cl$naturaList_levels)
      
      
      
      
      
      ####
      ## POints to be deleted
      values$MK <- data.frame(id = character(),
                              x = numeric(),
                              y = numeric())
      
      ## Lines to be inserted in values$MK
      values$add_row <- data.frame(id = character(),
                                   x = numeric(),
                                   y = numeric())
      
      values$pol <- list()
      
      
      ## List of polygons
      values$list.pol.df <- list()
      
      output$map <- leaflet::renderLeaflet({
        
        values$pt.ctr1 <- values$occur[values$Level_1,]
        values$pt.ctr2 <- values$occur[values$Level_2,]
        values$pt.ctr3 <- values$occur[values$Level_3,]
        values$pt.ctr4 <- values$occur[values$Level_4,]
        values$pt.ctr5 <- values$occur[values$Level_5,]
        values$pt.ctr6 <- values$occur[values$Level_6,]
        
        leaflet::leaflet("map") %>%
          # Add two tiles
          leaflet::addTiles(options = providerTileOptions(noWrap = TRUE),group="StreetMap")%>%
          leaflet::addProviderTiles("Esri.WorldImagery", group="Satellite")  %>%
          
          
          # Add marker groups
          leaflet::addCircleMarkers(data= values$pt.ctr6,
                                    lng= values$pt.ctr6[,longitude] ,
                                    lat= values$pt.ctr6[,latitude],
                                    radius=6 ,
                                    layerId = row.names(values$pt.ctr6),
                                    popup = paste(strong("ID:"), values$pt.ctr6[, occ.id],"<br>",
                                                  strong("Institution Source:"), values$pt.ctr6[, institution.source], "<br>",
                                                  strong("Determined by:"), values$pt.ctr6[, determined.by], "<br>",
                                                  strong("Year of data colection:"), values$pt.ctr6[, year.event], "<br>",
                                                  strong("Date Identified:"), (values$pt.ctr6[, date.identified]), "<br>"),
                                    fillColor="purple", stroke = F, fillOpacity = 0.8, group="Level 6") %>%
          
          leaflet::addCircleMarkers(data= values$pt.ctr5,
                                    lng= values$pt.ctr5[,longitude] ,
                                    lat= values$pt.ctr5[,latitude],
                                    radius=6 ,
                                    layerId = row.names(values$pt.ctr5),
                                    popup = paste(strong("ID:"), values$pt.ctr5[, occ.id],"<br>",
                                                  strong("Institution Source:"), values$pt.ctr5[, institution.source], "<br>",
                                                  strong("Determined by:"), values$pt.ctr5[, determined.by], "<br>",
                                                  strong("Year of data colection:"), values$pt.ctr5[, year.event], "<br>",
                                                  strong("Date Identified:"), (values$pt.ctr5[, date.identified]), "<br>"),
                                    fillColor="blue", stroke = F, fillOpacity = 0.8, group="Level 5") %>%
          
          leaflet::addCircleMarkers(data= values$pt.ctr4,
                                    lng= values$pt.ctr4[,longitude] ,
                                    lat= values$pt.ctr4[,latitude],
                                    radius=6 ,
                                    layerId = row.names(values$pt.ctr4),
                                    popup = paste(strong("ID:"), values$pt.ctr4[, occ.id],"<br>",
                                                  strong("Institution Source:"), values$pt.ctr4[, institution.source], "<br>",
                                                  strong("Determined by:"), values$pt.ctr4[, determined.by], "<br>",
                                                  strong("Year of data colection:"), values$pt.ctr4[, year.event], "<br>",
                                                  strong("Date Identified:"), (values$pt.ctr4[, date.identified]), "<br>"),
                                    fillColor="darkgreen", stroke = F, fillOpacity = 0.8, group="Level 4") %>%
          
          leaflet::addCircleMarkers(data= values$pt.ctr3,
                                    lng= values$pt.ctr3[,longitude] ,
                                    lat= values$pt.ctr3[,latitude],
                                    radius=6 ,
                                    layerId = row.names(values$pt.ctr3),
                                    popup = paste(strong("ID:"), values$pt.ctr3[, occ.id],"<br>",
                                                  strong("Institution Source:"), values$pt.ctr3[, institution.source], "<br>",
                                                  strong("Determined by:"), values$pt.ctr3[, determined.by], "<br>",
                                                  strong("Year of data colection:"), values$pt.ctr3[, year.event], "<br>",
                                                  strong("Date Identified:"), (values$pt.ctr3[, date.identified]), "<br>"),
                                    fillColor="yellow", stroke = F, fillOpacity = 0.8, group="Level 3") %>%
          
          
          leaflet::addCircleMarkers(data= values$pt.ctr2,
                                    lng= values$pt.ctr2[,longitude] ,
                                    lat= values$pt.ctr2[,latitude],
                                    radius=6 ,
                                    layerId = row.names(values$pt.ctr2),
                                    popup = paste(strong("ID:"), values$pt.ctr2[, occ.id],"<br>",
                                                  strong("Institution Source:"), values$pt.ctr2[, institution.source], "<br>",
                                                  strong("Determined by:"), values$pt.ctr2[, determined.by], "<br>",
                                                  strong("Year of data colection:"), values$pt.ctr2[, year.event], "<br>",
                                                  strong("Date Identified:"), values$pt.ctr2[, date.identified], "<br>"),
                                    fillColor="orange", stroke = F, fillOpacity = 0.8, group="Level 2") %>%
          
          leaflet::addCircleMarkers(data= values$pt.ctr1,
                                    lng= values$pt.ctr1[,longitude] ,
                                    lat= values$pt.ctr1[,latitude],
                                    radius=6 ,
                                    layerId = row.names(values$pt.ctr1),
                                    popup = paste(strong("ID:"), values$pt.ctr1[, occ.id],"<br>",
                                                  strong("Institution Source:"), values$pt.ctr1[, institution.source], "<br>",
                                                  strong("Determined by:"), values$pt.ctr1[, determined.by], "<br>",
                                                  strong("Year of data colection:"), values$pt.ctr1[, year.event], "<br>",
                                                  strong("Date Identified:"), (values$pt.ctr1[, date.identified]), "<br>"),
                                    fillColor="red", stroke = F, fillOpacity = 0.8, group="Level 1") %>%
          
          
          
          
          # Add the control widget
          leaflet::addLayersControl(overlayGroups = c("Level 1",
                                                      "Level 2",
                                                      "Level 3",
                                                      "Level 4",
                                                      "Level 5",
                                                      "Level 6") ,
                                    baseGroups = c("StreetMap","Satellite"),
                                    options = layersControlOptions(collapsed = TRUE)) %>%
          
          ## Add tool to design poligons shapes to selection
          leaflet.extras::addDrawToolbar(
            targetGroup='Markers',
            polylineOptions = F,
            polygonOptions = T,
            circleOptions = F,
            rectangleOptions = F,
            markerOptions = F,
            circleMarkerOptions = F,
            editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))%>%
          leaflet::addLegend("bottomright",
                             colors = c("red", "orange", "yellow", "darkgreen", "blue", "purple"),
                             labels = c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5", "Level 6"),
                             opacity = 0.8)
      })
      
      ## Clear selected poitns
      shiny::observeEvent(input$del_mkr_button,{
        if(input$del_mkr_button == FALSE){
          values$add_row <- data.frame(id = character(),
                                       x = numeric(),
                                       y = numeric())
        }
      })
        
        ## Delete points with click
        shiny::observeEvent(input$map_marker_click, {
          proxy <- leaflet::leafletProxy("map")
          
          if(input$del_mkr_button == TRUE){
            values$add_row <- data.frame(id = input$map_marker_click$id,
                                         x = input$map_marker_click$lng,
                                         y = input$map_marker_click$lat)
            values$MK <-  rbind(values$MK, values$add_row)
            
            
            
            dp <- duplicated(values$MK$id)
            
            if(sum(dp) >= 1){
              
              del <- values$MK[,1] %in% values$MK[dp,1]
              
            }
            
            id <- values$add_row$id
            proxy %>%
              leaflet::removeMarker(id)
          }
          
        })
        
        ## Data.frame to create polygon
        observeEvent(input$map_draw_all_features, {
          
          if(length(input$map_draw_all_features$features) == 0){
            values$list.pol.df <- list()
          }
          if(length(input$map_draw_all_features$features) >0){
            for(i in 1:length(input$map_draw_all_features$features)){
              values$list.pol.df[[i]] <- pol.coords(input$map_draw_all_features$features[[i]])
            }
          }
          
        })
        
        ## Select points acordingly to classification levels
        shiny::observeEvent(input$grbox, {
          # occurrence data
          occ.df <- values$occur
          
          pttn <- paste(input$grbox, collapse="|")
          values$g.cri <- grep(pttn, occ.df$naturaList_levels)
          
          
        })
        
        shiny::observeEvent(is.null(input$grbox), {
          if(is.null(input$grbox)){
            g.cri <- NULL
            values$g.cri <- g.cri
            
          }
        })
        
        ## Final dataframe
        # Selectd by levels chosed and poligons created
        shiny::observeEvent(input$grbox, {
          output$sel_display <-   shiny::renderText({
            
            if (is.null(values$g.cri)|length(values$g.cri) == 0){
              values$sel.points <- data.frame()
            }else{
              
              occ.df <- values$occur
              n.id <- as.character(values$MK$id)
              del <- row.names(occ.df)%in% n.id
              c.d <- values$g.cri %in% which(!del)
              values$pol <- list()
              
              if(length(values$list.pol.df) == 1){
                values$pol <- make.polygon(values$list.pol.df[[1]])
              }
              
              
              if(length(values$list.pol.df) > 1){
                
                values$pol <- bind(lapply(values$list.pol.df, make.polygon))
              }
              
              df.crit <- values$occur[values$g.cri[c.d],]
              
              spt.df <- SpatialPointsDataFrame(df.crit[,c(longitude, latitude)], df.crit)
              
              
              if(length(values$pol) == 0){
                n.col <- ncol(spt.df)
                values$sel.points <- as.data.frame(spt.df)[,1:n.col]
              }
              
              if(length(values$pol) >= 1){
                n.col <- ncol(spt.df)
                values$sel.points <- as.data.frame(spt.df[values$pol,])[,1:n.col]
              }
            }
            
            paste("Selected", nrow(values$sel.points), "of", nrow(values$occur), 
                  "occurrences." )
            
          })
        })
        
        observeEvent(input$done, {
          result <- values$sel.points
          
          stopApp(returnValue = result)
        })
        
        
        
      
      
      
    }
    
    
    
  ))
  return(final.df)
}
