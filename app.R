


## libraries ----
library(shiny)
library(dplyr)
library(rlist)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(sf)
library(urltools)
library(ngpcFisheries)
library(FinCatchAnalysis)



## UI ----
# Define UI for application that draws a histogram
ui <- fluidPage(
  title = "Waterbody Title and Name Here",
  style = "background-color: #d6eaf8; padding: 20px;",
  theme = bs_theme(version = 5),
  ### head tags ----
  tags$head(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css")
    ),
    tags$link(
      rel = "stylesheet",
      href = "https://use.fontawesome.com/releases/v5.15.4/css/all.css",
      crossorigin = "anonymous"
    ),
    
    tags$style(
      HTML(
        "
                  .card {
        box-shadow: 0 4px 8px rgba(0,0,0,0.2);
        border-radius: 5px;
        background-color: #ffffff;
        margin-bottom: 20px;
        padding: 15px;
        min-height: 400px;
      }
      # /* Card header styling */
      # .myHeader {
      #   #border-bottom: 1px solid #e5e5e5;
      #   margin-bottom: 15px;
      #   padding-bottom: 5px;
      #   font-weight: 600;
      #   font-size: 52px;
      #   color: blue;
      #   display: flex;
      #   justify-content: flex-start;
      #   padding-right:20px;
      # }
      
      .bslib-card .card-header {
  margin-bottom: 15px !important;
  padding-bottom: 5px !important;
  font-weight: 600 !important;
  font-size: 24px !important;
  color: blue !important;
  display: flex !important;
  justify-content: flex-start !important;
  padding-right: 20px !important;
      }
      
      /* ensure the row itself is full‐height if you want viewport‐based sizing */
      .full‐height‐row { height: 100%; }
    "
      )
    )
  ),
  ### UI Start ----
  fluidRow(
    class = "d-flex full‐height‐row",
    column(width = 12)), # titlePanel("Waterbody Info"))),
  fluidRow(column(
    width = 6, class = "d-flex flex-column",
    card(full_height = TRUE, class = "flex-fill mb-2", card_header(class="myHeader", "Waterbody Info"), card_body(uiOutput("wbInfo_Code"))),
    card(full_height = TRUE, class = "flex-fill", card_header("Public Access Info"), card_body(uiOutput("wbInfo_Public")))
  ), column(
    width = 6, class = "d-flex flex-column",
    card(full_height = TRUE, class = "flex-fill",
         card_body(tabsetPanel(
           tabPanel("Map", leafletOutput("leaflet_polygon", height="800px")),
           tabPanel("Imagery", leafletOutput("leaflet_imagery", height="800px")),
           tabPanel("Contours", leafletOutput("leaflet_contours", height="800px"), tags$p(tags$h5("*Hover for depth"))),
           tabPanel("GPS/Weather", leafletOutput("leaflet_directions", height="800px"),
                    # this button will launch Google Maps navigation on mobile/desktop
                    tags$br(),
                    uiOutput("nav_button"))
         )))
  )),
  fluidRow(class = "d-flex full‐height‐row",
           column(
             width = 12, class = "d-flex flex-column",
             card("Surveys Go Here"))),
  fluidRow(class = "d-flex full‐height‐row",
           column(
             width = 6, class = "d-flex flex-column",
             card("Stockings Go Here")),
           column(
             width = 6, class = "d-flex flex-column",
             card("Planned Stockings"))),
  fluidRow(class = "d-flex full‐height‐row",
           column(
             width = 12, class = "d-flex flex-column",
             card("DJ Plans Go Here")))
  
)

## Server ----
server <- function(input, output) {
  
    ### reactives ----
  rv<-reactiveValues()
  rv$wb<-NA
  rv$name<-NA
  rv$publicRecordId<-NA
  rv$gisLink<-NA
  
  #### set code values
  #get WbCode here...from querystring, if missing then show selection UI
  observe({
    browser()
    
    if(!is.null(getQueryString()$wb) && !is.na(getQueryString()$wb)) {
    rv$wb<-getQueryString()$wb
  } else {
    rv$wb<-5555
  }
  
    rv$name=NGPC_getCodes_waterbody_byCode(rv$wb)$waterbodyName
    rv$publicRecordId=NGPC_getCodes_waterbody_byCode(rv$wb)$publicWaterRecordId
    rv$gisLink=(NGPC_getCodes_waterbodyGisLinks_all() %>% filter(waterbodyCode==rv$wb) %>% pull(geometryId))[[1]]
  })
  
  #### getWbInfo_Code ----
  getWbInfo_Code <- reactive({
    req(rv$wb)
    wbInfo_Code <- NGPC_getCodes_waterbody_byCode(rv$wb)
    
    if (!is.null(wbInfo_Code$countyCode) &&
        !is.na(wbInfo_Code$countyCode)) {
      wbInfo_Code <- c(wbInfo_Code,
                       NGPC_getCodes_county_byCode(wbInfo_Code$countyCode))
    }
    if (!is.null(wbInfo_Code$districtCode) &&
        !is.na(wbInfo_Code$districtCode)) {
      wbInfo_Code <- c(wbInfo_Code,
                       NGPC_getCodes_district_byCode(wbInfo_Code$districtCode))
    }
    if (!is.null(wbInfo_Code$waterbodyClass) &&
        !is.na(wbInfo_Code$waterbodyClass)) {
      wbInfo_Code <- c(
        wbInfo_Code,
        NGPC_getCodes_waterbodyClasses_byCode(wbInfo_Code$waterbodyClass)
      )
    }
    if (!is.null(wbInfo_Code$ownershipCode) &&
        !is.na(wbInfo_Code$ownershipCode)) {
      wbInfo_Code <- c(wbInfo_Code,
                       NGPC_getCodes_ownership_byCode(wbInfo_Code$ownershipCode))
    }
    if (!is.null(wbInfo_Code$nrdcode) &&
        !is.na(wbInfo_Code$nrdcode)) {
      nrd_row <- NGPC_getCodes_nrd_all() %>%
        filter(nrdcode == wbInfo_Code$nrdcode)
      if (nrow(nrd_row) > 0) {
        wbInfo_Code <- c(wbInfo_Code, as.list(nrd_row[1, , drop = TRUE]))
      }
    }
    
    setNames(
      wbInfo_Code[c(
        "waterbodyName",
        "waterbodyCode",
        "acres",
        "classification",
        "yearConstructed",
        "publicWaterbodyName",
        "countyName",
        "ownerName",
        "nrdname",
        "directions"
      )],
      c(
        "Waterbody",
        "Code",
        "Acres",
        "Classification",
        "Constructed",
        "Region",
        "County",
        "Owner",
        "NRD",
        "Directions"
      )
    )
  })
  
  #### getWbInfo_Public ----
  getWbInfo_Public <- reactive({
    req(rv$publicRecordId)
    wbInfo_Public <- NGPC_getCodes_waterbodyCodesPublic_byCode(rv$publicRecordId)
    wbInfo_PublicAttributes <- NGPC_getCodes_waterbodyCodesPublicAttributes_byPublicRecordId(rv$publicRecordId) %>%
      left_join(NGPC_getCodes_waterbodyCodesPublicAttributeTypes_all() %>% select(attributeUid, attributeGuideText), by=c("pwaAttributeUid"="attributeUid")) %>%
      group_by(pwaRecordId) %>%
      summarise(Attributes=paste(attributeGuideText, collapse=", "))
    
    if (!is.null(wbInfo_Public$accessType) &&
        !is.na(wbInfo_Public$accessType)) {
      wbInfo_Public <- c(wbInfo_Public,
                         NGPC_getCodes_waterbodyCodesPublicAccessTypes_byCode(wbInfo_Public$accessType))
    }
    if (!is.null(wbInfo_PublicAttributes) &&
        !any(is.na(wbInfo_PublicAttributes)) &&
        length(wbInfo_PublicAttributes)>0) {
      wbInfo_Public <- c(wbInfo_Public,
                         wbInfo_PublicAttributes$Attributes)
    }
    wbInfo_Public$Acres <- paste0(wbInfo_Public$acres, " acres ", wbInfo_Public$numberWbText)
    
    setNames(
      wbInfo_Public[c(
        "name",
        "location",
        "atlasMapSheet",
        "Acres",
        "species",
        "regs",
        "regsBoating",
        "comments"
      )],
      c(
        "Public Access Area",
        "Area Location",
        "Public Access Atlas Page",
        "Acres",
        "Species",
        "Fishing Regulations",
        "Boating Regulationss",
        "Comments"
      )
    )
  })
  
  #### get public polygon data ----
  # Reactive expression that builds URL and reads GeoJSON
  geo_data <- reactive({
    req(rv$gisLink)
    
    # build the URL dynamically
    base_url <- "https://services5.arcgis.com/IOshH1zLrIieqrNk/arcgis/rest/services/JoinedData_Lakes/FeatureServer/0/query"
    # assume the service takes a 'where' parameter like "PFA_CODE='PFA-0168'"
    where_clause <- URLencode( rv$gisLink)
    url <- paste0(
      base_url,
      "?where=FishingGeometryID%20%3D%20%27", where_clause, "%27",
      "&outFields=FishingGeometryID,Creator,Centroid_X,Centroid_Y",
      "&outSR=4326&f=json"
    )
    # read as an sf object
    st_read(url, quiet = TRUE)
  })
  
  # 2) compute bounding box & centroid for reuse
  bbox_of_polygon  <- reactive({ st_bbox(geo_data()) })
  center <- reactive({
    ct <- st_centroid(st_union(geo_data()))
    st_coordinates(ct)[1, ]
  })

  #### get contours ----
  getContours<-reactive({
  #baseURL for  Forest Service invasive species API
  baseURL <- "https://services5.arcgis.com/IOshH1zLrIieqrNk/arcgis/rest/services/LakeContours/FeatureServer/0/query?"
  #convert bounding box to character
  bbox1 <- toString(bbox_of_polygon())
  #encode for use within URL
  bbox1 <- urltools::url_encode(bbox1)
  #set parameters for query
  query <- urltools::param_set(baseURL,key="geometry", value=bbox1) %>%
    param_set(key="inSR", value=4326) %>%
    param_set(key="resultRecordCount", value=500) %>%
    param_set(key="spatialRel", value="esriSpatialRelIntersects") %>%
    param_set(key="f", value="geojson") %>%
    param_set(key="outFields", value="*") %>%
    param_set(key="geometryType", value="esriGeometryEnvelope") %>%
    param_set(key="returnGeometry", value="true") %>%
    param_set(key="returnTrueCurves", value="false") %>%
    param_set(key="returnIdsOnly", value="false") %>%
    param_set(key="returnCountOnly", value="false") %>%
    param_set(key="returnZ", value="false") %>%
    param_set(key="returnM", value="false") %>%
    param_set(key="returnDistinctValues", value="false") %>%
    param_set(key="returnExtentOnly", value="false") %>%
    param_set(key="featureEncoding", value="esriDefault")
  
  read_sf(query) %>% arrange(Depth)
  })
  
  
  ### outputs ----
  #### wbInfo_Code ----
  output$wbInfo_Code <- renderUI({
    info <- getWbInfo_Code()        # call the reactive
    tagList(lapply(names(info), function(key) {
      val <- info[[key]]
      
      # skip if NULL / length 0 / all NA / all blank string
      if (is.null(val) ||
          length(val) == 0 ||
          (is.atomic(val) && all(is.na(val))) ||
          (is.character(val) && all(trimws(val) == ""))) {
        return(NULL)
      }
      
      # otherwise render
      tags$div(style = "margin-bottom: 2px;", HTML(sprintf("<b>%s:</b> %s", key, val)))
    }))
  })
  #### wbInfo_Public ----
  output$wbInfo_Public <- renderUI({
    info <- getWbInfo_Public()        # call the reactive
    tagList(lapply(names(info), function(key) {
      val <- info[[key]]
      
      # skip if NULL / length 0 / all NA / all blank string
      if (is.null(val) ||
          length(val) == 0 ||
          (is.atomic(val) && all(is.na(val))) ||
          (is.character(val) && all(trimws(val) == ""))) {
        return(NULL)
      }
      
      # otherwise render
      tags$div(style = "margin-bottom: 2px;", HTML(sprintf("<b>%s:</b> %s", key, val)))
    }))
  })
  
  #### leaflet_polygon ----
  output$leaflet_polygon <- renderLeaflet({
    
    req(nrow(geo_data()) > 0)    
    leaflet(data = geo_data()) %>% 
      addTiles() %>%
        clearShapes() %>%
        addPolygons(
          color   = "blue",
          weight  = 2,
          fillOpacity = 0.3
        ) %>%
        fitBounds(
          lng1 = min(st_bbox(geo_data())$xmin),
          lat1 = min(st_bbox(geo_data())$ymin),
          lng2 = max(st_bbox(geo_data())$xmax),
          lat2 = max(st_bbox(geo_data())$ymax)
        ) %>%
        addFullscreenControl()
  })
  
  #### leaflet_imagery ----
  output$leaflet_imagery <- renderLeaflet({
    req(geo_data())
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery")%>%
      addFullscreenControl()  %>%
      fitBounds(
        lng1 = min(st_bbox(geo_data())$xmin),
        lat1 = min(st_bbox(geo_data())$ymin),
        lng2 = max(st_bbox(geo_data())$xmax),
        lat2 = max(st_bbox(geo_data())$ymax)
      )
  })
  
  
  #### leaflet_contours ----
  output$leaflet_contours <- renderLeaflet({
    req(geo_data())
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery")%>%
      clearShapes() %>%
      addPolygons(data=getContours(),
                  layerId      = ~OBJECTID,
                  color   = "blue",
                  weight  = 2,
                  fillOpacity = 0.05,
                  label     = ~paste0( Depth, " feet"),
                  labelOptions = labelOptions(
                    direction = "auto",
                    style     = list(
                      "font-size" = "14px",
                      "background-color" = "rgba(255,255,255,0.8)",
                      "padding"   = "3px 5px"
                    ),
                    textsize   = "13px"
                  )
                  ) %>%
      addFullscreenControl() %>%
      fitBounds(
        lng1 = min(st_bbox(geo_data())$xmin),
        lat1 = min(st_bbox(geo_data())$ymin),
        lng2 = max(st_bbox(geo_data())$xmax),
        lat2 = max(st_bbox(geo_data())$ymax)
      )
  })
  
  #### leaflet_directions ----
  output$leaflet_directions <- renderLeaflet({
    req(geo_data())
    dest <- center()
    leaflet() %>%
      addTiles() %>%
      addFullscreenControl() %>%        # adds the full‐screen plugin
      # addWMSTiles(
      #   baseUrl = "https://opengeo.ncep.noaa.gov/geoserver/conus/conus_bref_qcd/ows",
      #   layers  = "conus_bref_qcd",
      #   options = WMSTileOptions(
      #     format      = "image/png",
      #     transparent = TRUE,
      #   )
      # ) %>%
      addWMSTiles(
        baseUrl = "https://www.nowcoast.noaa.gov/geoserver/observations/lightning_detection/ows",
        layers  = "ldn_lightning_strike_density",
        group="Lightning",
        options = WMSTileOptions(
          format      = "image/png",
          transparent = TRUE
        )
       ) %>%

      # addWMSTiles(
      #   baseUrl = "https://nowcoast.noaa.gov/geoserver/observations/satellite/ows",
      #   layers  = "global_longwave_imagery_mosaic",
      #   group="Clouds",
      #   options = WMSTileOptions(
      #     format      = "image/png",
      #     transparent = TRUE
      #   )
      # ) %>%
      addWMSTiles(
        baseUrl = "https://services5.arcgis.com/IOshH1zLrIieqrNk/arcgis/rest/services/LakeContours/FeatureServer/0/query?outFields=*&where=1%3D1",
        layers  = "conus_base_reflectivity_mosaic",
        group="Radar",
        options = WMSTileOptions(
          format      = "image/png",
          transparent = TRUE
        )
      ) %>%addWMSTiles(
        baseUrl = "https://nowcoast.noaa.gov/geoserver/observations/weather_radar/ows",
        layers  = "conus_base_reflectivity_mosaic",
        group="Radar",
        options = WMSTileOptions(
          format      = "image/png",
          transparent = TRUE
        )
      ) %>%
      addWMSTiles(
        baseUrl = "https://nowcoast.noaa.gov/geoserver/hazards/alerts/ows",
        layers  = "watches_warnings_advisories",
        group="Watches and Warnings",
        options = WMSTileOptions(
          format      = "image/png",
          transparent = TRUE
        )
      ) %>%
      addLayersControl(overlayGroups=c("Radar", "Lightning", "Watches and Warnings")) %>%
      leaflet.extras::activateGPS()  %>% 
      leaflet.extras::addControlGPS(options=gpsOptions(autoCenter=TRUE, setView=TRUE)) %>%# show both your current location control and the lake marker
      addMarkers(lng = dest[1], lat = dest[2], label = rv$name) %>%
      fitBounds(
        lng1 = min(st_bbox(geo_data())$xmin),
        lat1 = min(st_bbox(geo_data())$ymin),
        lng2 = max(st_bbox(geo_data())$xmax),
        lat2 = max(st_bbox(geo_data())$ymax)
      )
  })
  
  
  ## ---- Navigation button ----
  output$nav_button <- renderUI({
    req(center())
    dest <- center()
    # URL scheme to open in Google Maps / device navigation
    nav_url <- sprintf(
      "https://www.google.com/maps/dir/?api=1&destination=%f,%f&travelmode=driving",
      dest[2], dest[1]
    )
    tags$a(
      class = "btn btn-primary",
      href  = nav_url,
      target = "_blank",
      "Begin Navigation"
    )
  })
 
  
  # observeEvent(input$leaflet_contours_shape_click, {
  #   browser()
  #   click <- input$leaflet_contours_shape_click
  #   
  #   # 3) Extract the clicked contour’s attributes
  #   clicked_id  <- click$id
  #   clicked_pt  <- c(click$lng, click$lat)
  #   df          <- getContours()
  #   sel_polygon <- df[df$OBJECTID == clicked_id, ]
  #   
  #   # Show some info in the UI (or do whatever you like with sel_polygon)
  #   output$click_info <- renderPrint({
  #     cat("You clicked on contour_id:", clicked_id, "\n")
  #     print(st_geometry(sel_polygon))       # geometry
  #     print(st_drop_geometry(sel_polygon))  # all other attributes
  #   })
  # })
  
  
  # # observe the geo_data and update the map
  # observeEvent(geo_data(), {
  #   df <- geo_data()
  #   req(nrow(df) > 0)
  # 
  #   leafletProxy("leaflet_polygon", data = df) %>%
  #     clearShapes() %>%
  #     addPolygons(
  #       color   = "blue",
  #       weight  = 2,
  #       fillOpacity = 0.3
  #     ) %>%
  #     fitBounds(
  #       lng1 = min(st_bbox(df)$xmin),
  #       lat1 = min(st_bbox(df)$ymin),
  #       lng2 = max(st_bbox(df)$xmax),
  #       lat2 = max(st_bbox(df)$ymax)
  #     )
  # })

}

# Run the application
shinyApp(ui = ui, server = server)
