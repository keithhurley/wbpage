


## libraries ----
library(shiny)
library(dplyr)
library(lubridate)
library(stringr)
library(rlist)
library(bslib)
library(shinyWidgets)
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
  useSweetAlert(),
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
        #min-height: 400px;
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
  fluidRow(
    column(
      width = 6, class = "d-flex flex-column",
      card(full_height = TRUE, class = "flex-fill", card_header("Past Surveys"), card_body(tableOutput("surveyHistory")))
    ),
    column(
      width = 6, class = "d-flex flex-column",
      card(full_height = TRUE, class = "flex-fill", card_header("Species Observed"), card_body(uiOutput("surveySpecies")))
    ),
  ),
  # fluidRow(
  #   column(
  #     width = 12,
  #     card(
  #       full_height = TRUE,
  #       class = "flex-fill d-flex flex-column",
  #       card_header("Recent Sampling Results"),
  #       card_body(
  #         class = "flex-fill d-flex flex-column",
  #         
  #         # a) the plot
  #         plotOutput("selectedCPUE", height = "60vh", width = "100%"),
  #         
  #         # b) the dots
  #         uiOutput("cpueDots")
  #       )
  #     )
  #   )
  # ),
  uiOutput("cpue_card"),
  fluidRow(class = "d-flex full‐height‐row",
           column(
             width = 6, class = "d-flex flex-column",
             card(          full_height = TRUE,
                            class = "flex-fill d-flex flex-column",
                            card_header("Stockings (Last 10 Years)"),
                            card_body(
                              class = "flex-fill d-flex flex-column",
                              tableOutput("stockings"),
                            ))
             ),
           column(
             width = 6, class = "d-flex flex-column",
             card(        
               full_height = TRUE,
                          class = "flex-fill d-flex flex-column",
                          card_header("Planned Stockings"),
                          card_body(
                            class = "flex-fill d-flex flex-column",
                            tableOutput("plannedStockings"),
                          )
                          ))),
  fluidRow(
    column(
      width = 12,
      card(
        full_height = TRUE,
        class = "flex-fill d-flex flex-column",
        card_header("Stocking Timeline"),
        card_body(
          # 1) Load the Timeline CSS & JS
          tags$head(
            tags$link(
              rel  = "stylesheet",
              href = "https://cdn.knightlab.com/libs/timeline/latest/css/storyjs-embed.css"
            ),
            tags$script(
              src = "https://cdn.knightlab.com/libs/timeline/latest/js/storyjs-embed.js"
            )
          ),
          
          # 2) The DIV where the timeline will go
          tags$div(
            id    = "stockingDiv",
            style = "width:100%; height:600px;"
          ),
          
          # 3) A little bit of JS to fire off the AJAX when Shiny is ready
          uiOutput("stockingTimelineScript")
        )
      )
    )
  ),
  fluidRow(class = "d-flex full‐height‐row",
           column(
             width = 12, class = "d-flex flex-column",
             card(
               full_height = TRUE,
               class = "flex-fill d-flex flex-column",
               card_header("DJ Plans"),
               card_body(
                 class = "flex-fill d-flex flex-column",
                 tableOutput("plannedDJ")
               )
             ))),
  # fluidRow(class = "d-flex full‐height‐row",
  #          column(
  #            width = 12, class = "d-flex flex-column",
  #            card(
  #              full_height = TRUE,
  #              class = "flex-fill d-flex flex-column",
  #              card_header("Creel Results"),
  #              card_body(
  #                class = "flex-fill d-flex flex-column",
  #                tableOutput("creelPressure")
  #              )
  #            )))
  
)

## Server ----
server <- function(input, output, session) {
  
    ### reactives ----
  rv<-reactiveValues()
  rv$wb<-NA
  rv$name<-NA
  rv$publicRecordId<-NA
  rv$gisLink<-NA
  rv$samplingPlots<-NA
  rv$currentPlot<-1
  rv$surveySpecies<-NA
  rv$surveyHistory<-NA
  rv$dj_data<-NA
  rv$stockings<-NA
  rv$contours<-NA
  
  #### set code values ----
  #get WbCode here...from querystring, if missing then show selection UI
  observe({

    if(!is.null(getQueryString()$wb) && !is.na(getQueryString()$wb)) {
    rv$wb<-getQueryString()$wb
  } else {
    rv$wb<-5555
  }
  

    wb_info <- tryCatch(
      NGPC_getCodes_waterbody_byCode(rv$wb),
      error = function(e) NULL
    )
    
    # if nothing came back, alert and stop here
    if ( 
      is.null(wb_info) ||
        length((wb_info)) == 0 ||
        is.null(wb_info$waterbodyName) ||
        is.na(wb_info$waterbodyName) ) {
      
      sendSweetAlert(
        session = session,
        title   = "Waterbody Not Found",
        text    = glue::glue("No waterbody found for code {rv$wb}."),
        type    = "error"
      )
      return()
    }
    
    # otherwise, populate your reactives
    rv$name           <- wb_info$waterbodyName
    rv$publicRecordId <- wb_info$publicWaterRecordId
    rv$gisLink        <- (NGPC_getCodes_waterbodyGisLinks_all() %>%
                            filter(waterbodyCode == rv$wb) %>%
                            pull(geometryId))[1]
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
    if(!is.na(rv$publicRecordId) && !is.null(rv$publicRecordId)) {
    wbInfo_Public <- NGPC_getCodes_waterbodyCodesPublic_byCode(rv$publicRecordId)
    wbInfo_PublicAttributes <- NGPC_getCodes_waterbodyCodesPublicAttributes_byPublicRecordId(rv$publicRecordId)
    if(length(wbInfo_PublicAttributes)>0){
      wbInfo_PublicAttributes <- wbInfo_PublicAttributes %>%       
        left_join(NGPC_getCodes_waterbodyCodesPublicAttributeTypes_all() %>% select(attributeUid, attributeGuideText), by=c("pwaAttributeUid"="attributeUid")) %>%
        group_by(pwaRecordId) %>%
        summarise(Attributes=paste(attributeGuideText, collapse=", "))
      
    } else {
      wbInfo_PublicAttributes <- NULL
    }
    

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
    )}
    else
    {
      NULL
    }
    
    
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

  #### dj data -----
  observe({
    tmp <- NGPC_getFDC_dj_bywaterbody(rv$wb) %>%
      filter(activityCode < 1100 | activityCode > 1290) %>%
      filter(activityCode != 2500) %>%
      filter(activityYear>=year(now())) %>%
      select(activityYear, activityMonth, activityName, comments) %>%
      mutate(activityMonth=month.abb[activityMonth]) %>%
      arrange(activityYear, activityMonth, activityName)
    
    if (is.null(tmp) || (is.data.frame(tmp) && nrow(tmp)==0)) {
      rv$dj_data <- NULL
    } else {
      rv$dj_data <- tmp
    }
  })
  #### dj stockings -----
  observe({
    tmp<- NGPC_getFDC_dj_bywaterbody(rv$wb) %>%
      filter(activityCode >= 1100 & activityCode <= 1290) %>%
      filter(activityCode != 2500) %>%
      filter(activityYear>=year(now())) %>%
      select(activityYear, activityMonth, speciesCode, speciesName, sizeInInches,dayOfMonth, totalRequested, comments) %>%
      mutate(activityMonth=month.abb[activityMonth],
             species=paste0(speciesName, " (", speciesCode, ")"),
             sizeInInches=paste0(sizeInInches, " in"),
             totalRequested=format(totalRequested, big.mark=",")) %>%
      arrange(activityYear, activityMonth, dayOfMonth, species) %>%
      select(activityYear, activityMonth,species, sizeInInches, totalRequested, comments)
    
    if (is.null(tmp) || (is.data.frame(tmp) && nrow(tmp)==0)) {
      rv$dj_stockings <- NULL
    } else {
      rv$dj_stockings <- tmp
    }
  })
  #### stockings -----
  observe({
    tmp<- NGPC_getFDC_stockings(myWaterbodyCode=rv$wb, myStartYear=year(now())-10) 
    if (is.null(tmp) || (is.data.frame(tmp) && nrow(tmp)==0) || is.list(tmp) && !is.data.frame(tmp)) {
      rv$stockings <- NULL
    } else {
      rv$stockings <- tmp %>%
        select(stkDate, stkSpeciesCode, speciesName, sizeCategoryName, stkSize, stkNumber) %>%
        mutate(species=paste0(speciesName, " (", stkSpeciesCode, ")"),
               stkSize=paste0(stkSize, " in"),
               stkNumber=format(stkNumber, big.mark=","),
               stkYear=year(stkDate),
               stkDate=format(ymd_hms(stkDate), "%m-%d-%Y")) %>%
        arrange(species, stkYear, sizeCategoryName) %>%
        group_by(species, sizeCategoryName) %>%
        summarise(details=paste(stkYear, collapse=", ")) %>%
        arrange(species, sizeCategoryName) %>%
        select(species, sizeCategoryName, details)
    }
  })
  #### get contours ----
  getContours<-reactive({
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
  
  raw<-GET(query)
  txt<-content(raw, as="text", encoding="UTF-8")
  op<-read_sf(txt,
          quiet  = TRUE)
  if(nrow(op)>0) {
    rv$contours <- op  %>% arrange(Depth)
  } else {
    rv$contours <- NULL
  }
  })
  
  
  #### get sampling plots ----
  observe({
    req(rv$wb)
    
    #get all surveys for a waterbody
    v<-getData_surveys_filteredList(myCriteria=list("waterbodies"=list(rv$wb)))$content

    if(length(v)>0 ) {
    #save survey history
    tmp<-v %>% 
      group_by(svyMethodCode, svyMethodName, svySeasonCode, svySeasonName) %>%
      arrange(svyYear) %>%
      summarise(surveyYears=paste(svyYear, collapse=", ")) %>%
      mutate(surveyMethod=paste0(svyMethodName, " (", svyMethodCode, ")")) %>%
      ungroup() %>%
      left_join(data.frame(svySeasonName=c("Spring", "Summer", "Fall", "Winter", "Early-Spring", "Year", "Unknown"),
                           seasonSortOrder=c(2,3,4,5,1,6,7)), by=c("svySeasonName"="svySeasonName")) %>%
      arrange(surveyMethod, seasonSortOrder) %>%
      select(surveyMethod, surveySeason=svySeasonName, surveyYears) %>%
      arrange(surveyMethod, surveySeason)
    
    if (is.null(tmp) || (is.data.frame(tmp) && nrow(tmp)==0)) {
      rv$surveyHistory <- NULL
    } else {
      rv$surveyHistory <- tmp
    }
    
    #select most recent survey of each method and season and produce plots
    do<-fcacc_filters$new()
    do$set_surveys_filter( v %>%
                              # filter(svyApproved==TRUE) %>%
                              group_by(svySeasonCode, svySeasonName,
                                       svyMethodCode, svyMethodName) %>%
                              slice_max(svyYear, n = 1, with_ties = FALSE) %>%
                              ungroup() %>%
                              pull(svyUid))
    myData<-fc_data$new(do)
    op<-fca_CPUE_incremental(myData)
    original_plots<-op$plots
    
    # Post‐process each ggplot in the list
    processed_plots <- lapply(original_plots, function(p) {
      old_sub <- p$labels$subtitle %||% ""

      # extract each field0
      method  <- as.integer(str_match(old_sub, "Method=([^;\\s]+)")[,2]) %>% fc_matchCodes(getCodes_method() %>% select(code=methodCode, text=methodName), asFactor=FALSE)
      season  <- str_match(old_sub, "Season=([^;\\s\\)]+)")[,2]
      year    <- str_match(old_sub, "Year=([^;\\s]+)")[,2]
      
      # if we got all three, build a new title
      if (!any(is.na(c(method, season, year)))) {
        new_title <- sprintf("%s %s", season, year)
        new_subtitle <- method
        p <- p +
          labs(
            title    = new_title,
            subtitle = new_subtitle
          ) +
          theme(
            plot.title    = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 12, face = "italic")
          )
      }
      
      p
    })
    # 3) now write once to the reactiveValue
    rv$samplingPlots <- processed_plots
    #while we have surveys being processed...get species present
    rv$surveySpecies<-sort(myData$calc_speciesInAnalysisData %>% rename(code=speciesCode) %>% pull(code) %>% unique() %>% fc_matchCodes(getCodes_species() %>% select(code=speciesCode, text=speciesName) %>% mutate(text=paste0(text, " (", code, ")")), asFactor=FALSE))
    
    }
        else {
          rv$surveyHistory=NULL
          rv$surveySpecies=NULL
        }
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
    if(any(!is.na(info)) && !is.null(info)) {
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
    } else {
      "No data exists"
    }
  })
  #### surveySpecies ----
  output$surveySpecies <- renderUI({
    if(!is.null(rv$surveySpecies) && length(rv$surveySpecies)>0) {
      paste(rv$surveySpecies, collapse=", ")
    } else {
      "No data exists"
    }
  })
  
  #### dj plans ----
  output$plannedDJ <- renderTable({
    if (is.null(rv$dj_data) || nrow(rv$dj_data) == 0) {
      # return a one‐row data.frame so that the card shows your message
      return(data.frame(` ` = "No data exists."))
    }
    rv$dj_data
  },
    striped=TRUE,
    colnames=FALSE
  )
  # #### creel pressure ----
  # output$creelPressure <-renderPlot({
  #   library(ngpcHistoricCreel)
  #   
  #   creels<-creel_getCreels()
  #   m<-foreach(intX=seq(1,nrow(creels)-1,3), .combine="rbind") %do% {
  #     creel_getData_designGeneral(ids[seq(intX,intX+2,1),] %>% pull(Creel_UID)%>% paste(.,collapse=", ")) %>% filter(dg_WaterbodyCode==5110)
  #     #if(nrow(op)>0) {return (op$dg_CreelUID)}
  #   } 
  #   n<-foreach(intX=seq(1,nrow(m)-1,3), .combine="rbind") %do% {
  #     op<-creel_getResults(m[seq(intX,intX+2,1),] %>% pull(dg_CreelUID) %>% paste(.,collapse=", ")) %>% 
  #       filter(r_Parameter %in% c(1,2,3)) %>% filter(r_AnglerType==4 & r_AnglerMethod==5) 
  #     #if(nrow(op)>0) {return (op$dg_CreelUID)}
  #   } 
  #   pressure <- n %>% group_by(r_CreelUID) %>% filter(r_Parameter==1) %>% filter(r_Species==0) %>% 
  #     summarise(Pressure=sum(r_Value, na.rm=TRUE)) %>% 
  #     left_join(m %>% select(dg_CreelUID, dg_CreelStartDate) %>% 
  #                 mutate(Year=year(ymd_hms(dg_CreelStartDate))), by=c("r_CreelUID"="dg_CreelUID")) %>%
  #     left_join(creels %>% select(Creel_UID, Creel_Title), by=c("r_CreelUID"="Creel_UID")) %>%
  #     arrange(Year) %>%
  #     select(Year, Creel_Title, Pressure) 
  #   
  #   library(scales)
  #   pressure %>%
  #     ggplot() +
  #     geom_bar(aes(x=Creel_Title, y=Pressure), stat="identity", fill="blue", color="blue") +
  #     coord_flip() +
  #     scale_y_continuous(labels = comma)+
  #     labs(x="", y="Angler-Hours") +
  #     theme_minimal() +
  #     theme(panel.grid.major.y = element_blank())
  # })
  #### dj stockings ----
  output$plannedStockings <- renderTable({

    if (is.null(rv$dj_stockings) || nrow(rv$dj_stockings) == 0) {
      # return a one‐row data.frame so that the card shows your message
      return(data.frame(` ` = "No data exists."))
    }
    rv$dj_stockings
  }, striped = TRUE, colnames = FALSE)

  #### stockings ----
  output$stockings <- renderTable({
    if (any(!is.na(rv$stockings)) || !is.null(rv$stockings)) {
          rv$stockings
} else {
      # return a one‐row data.frame so that the card shows your message
      return(data.frame(` ` = "No data exists."))
    }
  },
  striped=TRUE,
  colnames=FALSE
  )
  #### Stockings TImeline ----
  output$stockingTimelineScript <- renderUI({
    # grab the waterbody code from your reactiveValues

    wb <- rv$wb

    
    # emit a <script> that does exactly what your standalone HTML did,
    # but targeting rv$wb and our #stockingDiv
    tags$script(HTML(sprintf("
      function updateTimeline() {
        $('#stockingDiv').empty();
        $.ajax({
          url: 'https://fishstaff.outdoornebraska.gov/staffapi/fdc/GetStockingsPublicWaterbody?wb=%s',
          type: 'GET',
          success: function(data) {
            createStoryJS({
              width       : '100%%',
              height      : '600',
              source      : data,
              embed_id    : 'stockingDiv',
              start_at_end: true
            });
          }
        });
      }

      // fire it on initial load
      $(document).ready(function() {
        updateTimeline();
      });
    ", wb)))
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
    getContours()
    req(geo_data(), rv$contours)
    
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery")%>%
      clearShapes() %>%
      addPolygons(data=rv$contours,
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
      # addWMSTiles(
      #   baseUrl = "https://services5.arcgis.com/IOshH1zLrIieqrNk/arcgis/rest/services/LakeContours/FeatureServer/0/query?outFields=*&where=1%3D1",
      #   layers  = "conus_base_reflectivity_mosaic",
      #   group="Radar",
      #   options = WMSTileOptions(
      #     format      = "image/png",
      #     transparent = TRUE
      #   )) %>% 
      addWMSTiles(
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
      hideGroup(c("Radar", "Radar2", "Lightning", "Watches and Warnings")) %>%
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
  
  #### output CPUE plots ----
  output$selectedCPUE <- renderPlot({
    idx <- rv$currentPlot
    req(rv$samplingPlots, rv$samplingPlots[[idx]])
    rv$samplingPlots[[idx]]
  }, res = 96)  # adjust DPI if you like
  
    # 4. Update index when a dot is clicked
  observe({
    req(rv$samplingPlots)
    n <- length(rv$samplingPlots)
    lapply(seq_len(n), function(i) {
      # create an observer for each input$dot_i
      observeEvent(input[[paste0("dot", i)]], {
        rv$currentPlot=i
      }, ignoreInit = TRUE)
    })
  })
  
  output$cpueDots <- renderUI({
    req(rv$samplingPlots)
    n <- length(rv$samplingPlots)
    idx <- rv$currentPlot
    
    # generate one actionLink() per plot
    tags$div(
      style = "text-align: center; margin-top: 0px;",
      lapply(seq_len(n), function(i) {
        actionLink(
          inputId = paste0("dot", i),
          label   = HTML(if (i == idx) "&#9679;" else "&#9675;"),
          style   = "font-size: 24px; margin: 0 5px; color: #007bff; text-decoration:none;"
        )
      })
    )
  })

  output$cpue_card <- renderUI({
    # If there are no plots, return NULL → hides the card entirely
    if (any(is.na(rv$samplingPlots)) || is.null(rv$samplingPlots) || length(rv$samplingPlots) == 0) {
      return(NULL)
    }
    
    # Otherwise, render exactly the same card you had before
    fluidRow(
      column(
        width = 12,
        card(
          full_height = TRUE,
          class = "flex-fill d-flex flex-column",
          card_header("Recent Sampling Results"),
          card_body(
            class = "flex-fill d-flex flex-column",
            plotOutput("selectedCPUE", height = "60vh", width = "100%"),
            uiOutput("cpueDots")
          )
        )
      )
    )
  })
  
  #### survey History ----
  output$surveyHistory <- renderTable({
    if (is.null(rv$surveyHistory)) {
      # return a one‐row data.frame so that the card shows your message
      return(data.frame(` ` = "No data exists."))
    }
    rv$surveyHistory
    },
    striped=TRUE,
    colnames=FALSE
 )
  
  
  
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
 
   }
  
  
# Run the application
shinyApp(ui = ui, server = server)
