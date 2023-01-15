#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @importFrom shiny actionLink
#' @importFrom shiny br
#' @importFrom shiny conditionalPanel
#' @importFrom shiny h1
#' @importFrom shiny h3
#' @importFrom shiny h4
#' @importFrom shiny uiOutput
#' @importFrom shiny p
#' @importFrom shiny.semantic file_input
#' @importFrom shiny.semantic segment
#' @importFrom shiny.semantic semanticPage
#' @importFrom shiny.semantic sidebar_layout
#' @importFrom shiny.semantic sidebar_panel
#' @importFrom shiny.semantic toggle
#' @noRd
app_ui <- function(request) {
  shiny.semantic::semanticPage(
    title = "Spatial Filter",
    shiny::h1("Spatial Filter"),
    shiny.semantic::sidebar_layout(
      shiny.semantic::sidebar_panel(
        shiny::h3("Categories"),
        shiny.semantic::segment(
          shiny::h4("1. Upload data set  "),
          shiny.semantic::file_input("upload",
                                     label = "Upload data to visualize on map:",
                                     type = "file",
                                     accept = c("text/csv",
                                                ".xlsx",
                                                "text/comma-separated-values,text/plain",
                                                ".csv",
                                                ".xlsx")),

          shiny::p("Data set must contain columns for longitude and latitude (WGS84 lat-long)."),
          shiny::actionLink("golink", "Or click here to use sample data")
        ),
        shiny::conditionalPanel(
          condition = "output.nrowsdata == true",
          shiny.semantic::segment(
            shiny::h4(uiOutput("header2")),
            shiny::p("Select columns in data set to visualize on map."),
            shiny::uiOutput("lonKolom"),
            shiny::uiOutput("latKolom"),
            shiny::uiOutput("somKolom"),
            shiny::br(),
            shiny.semantic::toggle("tog1", "Show values by group", FALSE),
            shiny::br(),
            shiny::conditionalPanel(
              condition = "input.tog1 == true",
              shiny::br(),
              shiny::uiOutput("catKolom")
            ),
            shiny::br(),
            shiny.semantic::action_button(class = "blue",
                                          "showmap",
                                          label = "Show data on map")
          )),
        shiny::br(),
        shiny::conditionalPanel(
          condition = "output.nrowsdata == true",
          shiny.semantic::action_button(class = "green",
                                        "downloaddata",
                                        label = "Download filtered data")),
        shiny::uiOutput("modalAction")
      ),
      shiny.semantic::main_panel(
        shiny::uiOutput("showcard"),
        segment(leaflet::leafletOutput("mymap")),
        shiny::br(),
        shiny.semantic::semantic_DTOutput("contents")
      )
    ),
    shiny::p("\u24B8 Built by Martin Haringa.")
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "spatialfilter"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
