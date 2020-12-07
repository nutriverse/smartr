################################################################################
#
#
# This is a Shiny web application port of the SMART ENA software.
#
# This code is for the server logic function of the Shiny web aplication.
#
#
################################################################################


################################################################################
#
# Server logic for web application
#
################################################################################

server <- function(input, output, session) {
  ## Sampling panel ############################################################

  ## Check if SRS or cluster sampling
  output$sample_list <- renderUI({
    if (input$survey_sampling_type == "srs") {
      radioButtons(
        inputId = "srs_available_list",
        label = "What sampling unit list/s are available?",
        choices = c("Under 5 children" = "u5",
                    "Households" = "hh",
                    "Both" = "both"),
        selected = "u5",
        inline = TRUE
      )
    }
  })

  ## Check type of input for lists
  output$sample_list_input <- renderUI({
    if (input$sample_input_type == "upload") {
      fileInput(
        inputId = "upload_sample_file",
        label = "Upload file for sample unit list",
        accept = c(".csv", ".xlsx", ".xls")
      )
    }
  })

  ## Read file input
  sample_list_df <- reactive({
    inFile <- input$upload_sample_file
    if (is.null(inFile)) return(NULL)

    if (stringr::str_detect(string = inFile$datapath, pattern = ".csv")) {
      read.csv(file = inFile$datapath)
    } else {
      readxl::read_excel(path = inFile$datapath)
    }
  })

  output$sample_list_vars <- renderUI({
    if(!is.null(input$upload_sample_file)) {
      selectInput(
        inputId = "sample_list_pop",
        label = "Select variable for population size",
        choices = names(sample_list_df()),
        selected = stringr::str_extract_all(string = names(sample_list_df()),
                                            pattern = "pop|Pop|POP|population|Population|POPULATION")
      )
    }
  })

  ##
  output$sample_total_pop <- renderUI({
    column(
      width = 4,
      box(
        title = "Total population size",
        width = NULL,
        solidHeader = FALSE,
        status = "success",
        valueBox(
          value = total_pop(),
          subtitle = "Total population",
          icon = icon(name = "users",
                      lib = "font-awesome",
                      class = "fa-med"),
          color = "orange",
          width = 12
        )
      )
    )
  })

  ## Calculate total population
  total_pop <- reactive({
    req(input$sample_list_pop)
    sum(sample_list_df()[[input$sample_list_pop]], na.rm = TRUE)
  })

  ## Sample size - anthro - children
  ss_anthro_child <- reactive({
    req(input$est_prevalence, input$precision, input$deff)

    if (input$survey_sampling_type == "cluster") {
      ss <- sampsizer::get_ss_prevalence(p = input$est_prevalence / 100,
                                         c = input$precision / 100,
                                         deff = input$deff,
                                         fpc = input$fpc,
                                         pop = total_pop())
    } else {
      ss <- sampsizer::get_ss_prevalence(p = input$est_prevalence / 100,
                                         c = input$precision / 100,
                                         deff = 1,
                                         fpc = input$fpc,
                                         pop = total_pop())
    }

    ss <- ceiling(ss)
    ss
  })

  ## Create a ss_anthro_child output
  output$ss_anthro_child <- renderText({ ss_anthro_child() })

  ## Sample size - anthro - households
  ss_anthro_hh <- reactive({
    req(ss_anthro_child(), )

    sh <- calculate_ss_hh(ss = ss_anthro_child(),
                          hh_size = input$hh_size,
                          u5 = input$u5_perc / 100)

    ## Adjust for non-response
    sh <- sh + (sh * (input$hh_nr / 100))

    sh <- ceiling(sh)
    sh
  })

  ## Create a ss_anthro_hh output
  output$ss_anthro_hh <- renderText({ ss_anthro_hh() })

  sample_cluster_list <- eventReactive(input$assign_clusters, {
    get_pps(df = sample_list_df(),
            pop = input$sample_list_pop,
            m = input$n_clusters)
  })

  output$sample_list_table <- DT::renderDT(
    expr = sample_list_df(),
    options = list(scrollX = TRUE, pageLength = 20)
  )

  observeEvent(input$assign_clusters, {
    output$sample_list_table <- DT::renderDT(
      expr = sample_cluster_list()[[1]],
      options = list(scrollX = TRUE, pageLength = 20)
    )

    output$sample_list_reserved <- renderText({
      paste("Reserved clusters: ",
            paste(sample_cluster_list()[[3]], collapse = ", "),
            sep = "")
    })

    output$download_sample_list <- renderUI({
      downloadButton(
        outputId = "download_sample_excel",
        label = "",
        icon = icon(name = "file-excel",
                    lib = "font-awesome",
                    class = "fa-small"),
        class = "btn-primary"
      )
    })
  })

  ##
  output$download_sample_excel <- downloadHandler(
    filename = function() {
      paste("sample_list_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(sample_cluster_list()[[2]], file)
    }
  )

  ##
  output$sample_table <- renderUI({
    req(input$sample_list_pop)
    if (input$survey_sampling_type == "srs") {
      fluidRow(
        box(
          title = "Random number table",
          width = 4,
          solidHeader = TRUE,
          status = "success"
        ),
        box(
          title = "Random number table",
          width = 8,
          solidHeader = FALSE,
          status = "success"
        )
      )
    }

    if (input$survey_sampling_type == "cluster") {
      fluidRow(
        box(
          title = "Cluster sampling",
          width = 4,
          solidHeader = TRUE,
          status = "success",
          sliderInput(
            inputId = "n_clusters",
            label = "Number of clusters",
            min = 25,
            max = 200,
            value = 30,
            step = 1
          ),
          actionButton(
            inputId = "assign_clusters",
            label = "Assign clusters",
            icon = icon(name = "table",
                        lib = "font-awesome",
                        class = "fa-small"),
            class = "btn-primary"
          )
        ),
        box(
          title = "Cluster sampling",
          width = 8,
          solidHeader = FALSE,
          status = "success",
          DT::DTOutput("sample_list_table"),
          hr(),
          div(style="display:inline-block; vertical-align:middle;",
            h4(textOutput("sample_list_reserved")),
          ),
          div(style="display:inline-block; vertical-align:middle; text-align:right; width:100%;",
            uiOutput("download_sample_list")
          )
        )
      )
    }
  })
}
