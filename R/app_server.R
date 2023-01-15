#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise
#' @importFrom leaflet addLayersControl
#' @importFrom leaflet addProviderTiles
#' @importFrom leaflet addTiles
#' @importFrom leaflet clearControls
#' @importFrom leaflet clearMarkers
#' @importFrom leaflet fitBounds
#' @importFrom leaflet leaflet
#' @importFrom leaflet leafletProxy
#' @importFrom leaflet renderLeaflet
#' @importFrom leaflet.extras addDrawToolbar
#' @importFrom leaflet.extras editToolbarOptions
#' @importFrom leaflet.extras selectedPathOptions
#' @importFrom lwgeom lwgeom_make_valid
#' @importFrom readxl read_xls
#' @importFrom readxl read_xlsx
#' @importFrom sf st_as_sf
#' @importFrom sf st_cast
#' @importFrom sf st_combine
#' @importFrom shiny div
#' @importFrom shiny downloadHandler
#' @importFrom shiny isTruthy
#' @importFrom shiny observeEvent
#' @importFrom shiny outputOptions
#' @importFrom shiny reactive
#' @importFrom shiny reactiveValues
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny uiOutput
#' @importFrom shiny.semantic actionButton
#' @importFrom shiny.semantic card
#' @importFrom shiny.semantic cards
#' @importFrom shiny.semantic dropdown_input
#' @importFrom shiny.semantic hide_modal
#' @importFrom shiny.semantic modal
#' @importFrom shiny.semantic segment
#' @importFrom shiny.semantic selectInput
#' @importFrom shiny.semantic semantic_DT
#' @importFrom shiny.semantic show_modal
#' @importFrom stats aggregate
#' @importFrom utils read.csv
#' @importFrom utils write.csv
#' @importFrom writexl write_xlsx
#'
#' @noRd
app_server <- function(input, output, session) {

  indata <- shiny::reactiveValues(data = NULL, header2 = NULL)

  output$showcard <- shiny::renderUI({

    card1 <-  shiny.semantic::card(class = "blue",
                                   shiny::div(class = "content",
                                              shiny::div(class = "header", "Cumulation"),
                                              shiny::div(class = "meta", shiny::uiOutput("somkolom1")),
                                              shiny::div(class = "description", shiny::uiOutput("tekst1"))))
    if ( !isTRUE(input$tog1) ){
      x <- shiny.semantic::segment(
        shiny.semantic::cards(
          class = "one",
          card1
        )
      )} else {
        x <- shiny.semantic::segment(
          shiny.semantic::cards(
            class = "two",
            card1,
            shiny.semantic::card(class = "blue",
                                 shiny::div(class = "content",
                                            shiny::div(class = "header", "Cumulation by group"),
                                            shiny::div(class = "meta", shiny::uiOutput("somkolom2")),
                                            shiny::div(class = "description", shiny::uiOutput("tekst2"))))
          )
        )
      }
    x
  })

  shiny::observeEvent(input$upload, {
    inFile <- input$upload
    extension <- tools::file_ext(inFile$name)
    filepath <- inFile$datapath
    df <- switch(extension,
                 csv = utils::read.csv(inFile$datapath),
                 xls = readxl::read_xls(filepath),
                 xlsx = readxl::read_xlsx(filepath))
    indata$data <- df
    indata$header2 <- "2. Select columns in uploaded data set"
  })

  shiny::observeEvent(input$golink, {
    indata$data <- spatialfilter::Groningen100
    indata$header2 <- "2. Select columns in example data set"
  })

  output$nrowsdata <- shiny::reactive({
    nrow(indata$data) > 0
  })

  shiny::outputOptions(output, "nrowsdata", suspendWhenHidden = FALSE)

  output$lonKolom <- shiny::renderUI({
    shiny::req(indata$data)
    nums <- unlist(lapply(indata$data, is.numeric), use.names = FALSE)
    nams <- names(indata$data[ , nums])
    x <- grep('lon|Lon|Leng|leng', nams, value = TRUE)
    if ( length(x) > 0 ) sel <- x[1] else sel <- NULL
    shiny.semantic::selectInput("loninput",
                                label = "Longitude",
                                nams,
                                selected = sel)
  })

  output$latKolom <- shiny::renderUI({
    shiny::req(indata$data)
    nums <- unlist(lapply(indata$data, is.numeric), use.names = FALSE)
    nams <- names(indata$data[ , nums])
    x <- grep('lat|Lat|Breed|breed', names(indata$data), value=TRUE)
    if ( length(x) > 0 ) sel <- x[1] else sel <- NULL
    shiny.semantic::selectInput("latinput",
                                label = "Latitude",
                                setdiff(nams, input$loninput),
                                selected = sel)
  })

  output$somKolom <- shiny::renderUI({
    shiny::req(indata$data)
    nums <- unlist(lapply(indata$data, is.numeric), use.names = FALSE)
    nams <- names(indata$data[ , nums])
    shiny.semantic::selectInput("valinput", "Value",
                                setdiff(nams, c(input$loninput, input$latinput)))
  })

  output$catKolom <- shiny::renderUI({
    shiny::req(indata$data)
    chars <- unlist(lapply(indata$data, is.character), use.names = FALSE)
    nams <- names(indata$data[ , chars])
    shiny.semantic::selectInput("catinput", "Category",
                                setdiff(nams, c(input$loninput, input$latinput, input$valinput)))
  })


  output$mymap <- leaflet::renderLeaflet({
    m <- leaflet::leaflet() %>%
      leaflet::addTiles(group = "OpenStreetMap") %>%
      leaflet::addProviderTiles("CartoDB.Positron",
                                group = "CartoDB.Positron") %>%
      leaflet::fitBounds(lng1 = -50,
                         lat1 = 30,
                         lng2 = 50,
                         lat2 = 70) %>%
      leaflet::addLayersControl(
        baseGroups = c("OpenStreetMap", "CartoDB.Positron"),
        position = "topright")
    m
  })

  shiny::observeEvent(input$showmap, {
    m <- leaflet::leafletProxy("mymap", data = indata$data) %>%
      leaflet::clearMarkers() %>%
      leaflet::clearControls() %>%
      leaflet::fitBounds(lng1 = min(indata$data[[input$loninput]]),
                         lat1 = min(indata$data[[input$latinput]]),
                         lng2 = max(indata$data[[input$loninput]]),
                         lat2 = max(indata$data[[input$latinput]])) %>%
      leaflet.extras::addDrawToolbar(
        targetGroup = "draw",
        polylineOptions = FALSE,
        circleMarkerOptions = FALSE,
        markerOptions = FALSE,
        editOptions = leaflet.extras::editToolbarOptions(
          selectedPathOptions = leaflet.extras::selectedPathOptions())
      )

    if ( input$tog1 ){
      pal <- leaflet::colorFactor("Set1", indata$data[[input$catinput]])
      m %>%
        leaflet::addCircleMarkers(lng = indata$data[[input$loninput]],
                                  lat = indata$data[[input$latinput]],
                                  color = ~pal(indata$data[[input$catinput]])) %>%
        leaflet::addLegend(pal = pal,
                           values = indata$data[[input$catinput]],
                           position = "bottomright")
    }

    if ( !isTRUE(input$tog1) ){
      m %>%
        leaflet::addCircleMarkers(lng = indata$data[[input$loninput]],
                                  lat = indata$data[[input$latinput]])
    }
  })

  vals <- shiny::reactiveValues(rows = NULL)

  shiny::observeEvent(input$mymap_draw_all_features, {

    x <- input$mymap_draw_all_features$features
    polygons <- x[lapply(x, function(ft) ft$geometry$type) == "Polygon"]
    points <- x[lapply(x, function(ft) ft$geometry$type) == "Point"]

    if ( length(points) > 0){
      drawncircles <- lapply(points,
                             function(ftr) {
                               lon <- ftr$geometry$coordinates[[1]]
                               lat <- ftr$geometry$coordinates[[2]]
                               radius <- ftr$properties$radius
                               df <- indata$data %>% mutate(id = 1:nrow(.))
                               names(df)[names(df) == input$loninput] <- "lon"
                               names(df)[names(df) == input$latinput] <- "lat"
                               spatialrisk::points_in_circle(df,
                                                             lon_center = lon,
                                                             lat_center = lat,
                                                             lon = lon,
                                                             lat = lat,
                                                             radius = radius)
                             })
      bcircles <- do.call(rbind, drawncircles)
      idc <- unique(bcircles$id)
    }

    if ( length(polygons) > 0 ){
      drawnshapes <- lapply(polygons,
                            function(ftr) {
                              x <- unlist(ftr$geometry$coordinates)
                              lon <- x[seq(1, length(x), by = 2)]
                              lat <- x[seq(2, length(x), by = 2)]
                              data.frame(lon = lon,
                                         lat = lat,
                                         id = ftr$properties$`_leaflet_id`)
                            })

      b <- do.call(rbind, drawnshapes)
      bsf <- sf::st_as_sf(b, coords = c("lon", "lat"), crs = 4326)

      polygon <- bsf %>%
        dplyr::group_by(id) %>%
        dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
        sf::st_cast('POLYGON') %>%
        dplyr::mutate(geometry = lwgeom::lwgeom_make_valid(geometry))

      pts <- indata$data[, c(input$loninput, input$latinput)]
      names(pts)[names(pts) == input$loninput] <- 'lon'
      names(pts)[names(pts) == input$latinput] <- 'lat'

      pts <- pts %>%
        sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
        dplyr::mutate(idpts = 1:nrow(.))

      join_tabel <- sf::st_join(pts, polygon, join = sf::st_within, left = FALSE)
      idp <- unique(join_tabel$idpts)
    }

    if ( length(x) == 0 ){
      vals$rows <- NULL
    }

    if ( length(polygons) > 0 & length(points) == 0 ){
      vals$rows <- idp
    }

    if ( length(polygons) == 0 & length(points) > 0 ){
      vals$rows <- idc
    }

    if ( length(polygons) > 0 & length(points) > 0 ){
      vals$rows <- unique(c(idp, idc))
    }
  }
  )

  output$header2 <- shiny::renderUI({
    indata$header2
  })

  output$somkolom1 <- shiny::renderUI({
    if ( shiny::isTruthy(input$valinput) ){
      shiny::p(paste0("Sum of choosen value \'", input$valinput, "\'"))
    } else{
      shiny::p("Sum of choosen value")
    }
  })

  output$somkolom2 <- shiny::renderUI({
    if ( shiny::isTruthy(input$catinput) ){
      shiny::p(paste0("Sum of choosen value \'", input$valinput, "\'", " by \'", input$catinput, "\'"))
    } else{
      shiny::p("Sum of choosen value")
    }
  })

  output$tekst1 <- shiny::renderUI({
    if ( shiny::isTruthy(input$valinput) ){
      sumval <- sum(indata$data[vals$rows,][[input$valinput]], na.rm = TRUE)
      lval <- length(vals$rows)
    } else {
      sumval <- 0
      lval <- 0
    }
    paste0(print_format(lval, p = TRUE), " selected with sum ", print_format(sumval))
  })

  output$tekst2 <- shiny::renderUI({
    if ( shiny::isTruthy(input$valinput) & shiny::isTruthy(vals$rows) ){
      shiny::req(indata$data)
      sumcat <- stats::aggregate(indata$data[vals$rows,][[input$valinput]],
                          list(cc = indata$data[vals$rows,][[input$catinput]]),
                          FUN = function(v){ c(sum = sum(v, na.rm = TRUE),
                                               ncol = length(v))})
      x <- cbind(cc = sumcat$cc, data.frame(sumcat$x))
      y <- character(length(x$cc))
      for (i in seq_along(x$cc)){
        y[i] <- paste0(x$cc[i], ": ", print_format(x$ncol[i], p = TRUE), ", sum ", print_format(x$sum[i]))
      }
      paste0(y, collapse = " || ") #\U2022
    } else {
      paste0(0, " points selected with sum ", 0)
    }
  })

  dtInput <- shiny::reactive({
    if( is.null(input$mymap_draw_all_features) ) {
      indata$data }
    else {
      indata$data[vals$rows,]
    }
  })

  output$contents <- DT::renderDataTable({
    shiny.semantic::semantic_DT(dtInput())
  })

  shiny::observeEvent(input$downloaddata, {
    shiny.semantic::show_modal('dmodal')
  })

  shiny::observeEvent(input$hide, {
    shiny.semantic::hide_modal('dmodal')
  })

  output$modalAction <- shiny::renderUI({
    shiny.semantic::modal(
      "Structure",
      shiny.semantic::dropdown_input("dd_ext1",
                                     choices = c("XLSX", "CSV"),
                                     value = "XLSX"),
      shiny::br(),
      shiny::downloadButton("download1", "Download",
                            class = paste("ui", "green", "button")),
      shiny::br(),
      id = "dmodal",
      header = "Download spatial filtered data",
      footer = shiny.semantic::actionButton("hide", "Close"),
      class = "tiny"
    )
  })

  output$download1 <- shiny::downloadHandler(
    filename = function() {
      switch(input$dd_ext1,
             CSV = paste0("data_csv_", Sys.Date(), ".csv"),
             XLSX = paste0("data_xlsx_", Sys.Date(), ".xlsx")
      )
    },
    content = function(file) {
      switch(input$dd_ext1,
             CSV = {
               utils::write.csv(dtInput(), file, row.names = FALSE)
             },
             XLSX = {
               writexl::write_xlsx(list(filtered_spatial_data = dtInput()), file)
             }
      )
    }
  )


}
