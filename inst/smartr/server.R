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
            max = 60,
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

  ## Training panel ############################################################

  ## Read file input
  std_test_df <- reactive({
    inFile <- input$std_test_data
    if (is.null(inFile)) return(NULL)

    if (stringr::str_detect(string = inFile$datapath, pattern = ".csv")) {
      x <- read.csv(file = inFile$datapath)
    } else {
      x <- readxl::read_excel(path = inFile$datapath, skip = 1)

      x <- x[ , apply(X = x, MARGIN = 2, FUN = function(x) all(!is.na(x)))]
    }
  })

  ## Read file input
  std_test_df_long <- reactive({
    x <- std_test_df()

    x <- x %>%
      dplyr::mutate(subject = 1:10) %>%
      dplyr::relocate(subject, .before = names(x)[1])

    x <- tidyr::pivot_longer(data = x,
                             cols = names(x)[2]:names(x)[length(names(x))],
                             names_to = "measure_type",
                             values_to = "measure_value")

    measure_round <- stringr::str_extract(x$measure_type,
                                          pattern = "[0-9]{1}")

    observer <- vector(mode = "integer", length = nrow(x))

    for (i in unique(x$subject)) {
      y <- subset(x, subject == i)

      nObservers <- nrow(y) / 6

      z <- NULL
      for (j in seq_len(nObservers)) {
        z <- c(z, rep(j - 1, 6))
      }

      observer[x$subject == i] <- z
    }

    observer <- paste("Enumerator ", observer, sep = "")
    observer <- ifelse(observer == "Enumerator 0", "Supervisor", observer)

    x$measure_type = stringr::str_extract(x$measure_type,
                                          pattern = "[A-Za-z]{6}|[A-Za-z]{4}") %>%
      tolower()

    x <- tibble::tibble(observer, x, measure_round)

    return(x)
  })

  ##
  output$std_test_table <- DT::renderDT(
    expr = std_test_df_long(),
    options = list(scrollX = TRUE, pageLength = 20)
  )

  std_tem_individual <- reactive({
    x <- std_test_df_long() %>%
      tidyr::pivot_wider(names_from = measure_round,
                         values_from = measure_value,
                         names_prefix = "measure_value")

    calculate_tem_cohort(
      df = x,
      m1 = "measure_value1",
      m2 = "measure_value2",
      index = c("observer", "measure_type"),
      n = length(unique(x$subject)))
  })

  ##
  output$std_tem_individual_table <- DT::renderDT(
    expr = std_tem_individual(),
    options = list(scrollX = TRUE, pageLength = 20)
  )

  ##
  output$std_tem_individual_plot <- plotly::renderPlotly({
    x <- std_tem_individual()
    z <- x %>%
      mutate(observer = factor(observer,
                               levels = c("Supervisor",
                                          paste("Enumerator",
                                                1:length(unique(observer)),
                                                sep = " ")))) %>%
      ggplot(mapping = aes(x = observer, y = tem)) +
      geom_col(alpha = 0.7) +
      facet_wrap(. ~ measure_type, nrow = 3, scales = "free_y") +
      labs(x = "", y = "Technical Error of Measurement (TEM)") +
      katilingban_theme +
      theme(axis.text.x = element_text(size = 8, angle = 30))
    ggplotly(p = z, height = 600)
  })

  std_tem_team <- reactive({
    height_team_tem <- calculate_team_tem(
      n = nrow(std_test_df()),
      k = ncol(std_test_df()) / 3,
      m = data.frame(std_test_df()) %>% select(starts_with("Height"))
    )

    weight_team_tem <- calculate_team_tem(
      n = nrow(std_test_df()),
      k = ncol(std_test_df()) / 3,
      m = data.frame(std_test_df()) %>% select(starts_with("Weight"))
    )

    muac_team_tem <- calculate_team_tem(
      n = nrow(std_test_df()),
      k = ncol(std_test_df()) / 3,
      m = data.frame(std_test_df()) %>% select(starts_with("MUAC"))
    )

    z <- list(height_team_tem, weight_team_tem, muac_team_tem)
    names(z) <- c("height", "weight", "muac")

    return(z)
  })

  ##
  std_tem_total <- reactive({
    x <- std_tem_individual()
    y <- std_tem_team()

    total_tem_height <- calculate_total_tem(
      intra = x$tem[x$measure_type == "height"],
      inter = y$height
    )

    total_tem_weight <- calculate_total_tem(
      intra = x$tem[x$measure_type == "weight"],
      inter = y$weight
    )

    total_tem_muac <- calculate_total_tem(
      intra = x$tem[x$measure_type == "muac"],
      inter = y$muac
    )

    z <- list(total_tem_height, total_tem_weight, total_tem_muac)
    names(z) <- c("height", "weight", "muac")

    return(z)
  })

  ##
  std_reliability <- reactive({
    x <- std_tem_total()

    sm_height <- summary_measure(df = std_test_df_long() %>%
                                   filter(measure_type == "height"),
                                 measures = "measure_value",
                                 index = "observer")

    sm_weight <- summary_measure(df = std_test_df_long() %>%
                                   filter(measure_type == "weight"),
                                 measures = "measure_value",
                                 index = "observer")

    sm_muac <- summary_measure(df = std_test_df_long() %>%
                                   filter(measure_type == "muac"),
                                 measures = "measure_value",
                                 index = "observer")

    reliability_height <- calculate_coeff_r(
      total_tem = x$height,
      sd = sm_height$sd
    )

    reliability_height <- data.frame(observer = sm_height$observer,
                                     reliability_height)

    reliability_weight <- calculate_coeff_r(
      total_tem = x$weight,
      sd = summary_measure(df = std_test_df_long() %>%
                             filter(measure_type == "weight"),
                           measures = "measure_value",
                           index = "observer")$sd
    )

    reliability_weight <- data.frame(observer = sm_weight$observer,
                                     reliability_weight)

    reliability_muac <- calculate_coeff_r(
      total_tem = x$muac,
      sd = summary_measure(df = std_test_df_long() %>%
                             filter(measure_type == "muac"),
                           measures = "measure_value",
                           index = "observer")$sd
    )

    reliability_muac <- data.frame(observer = sm_muac$observer,
                                     reliability_muac)

    z <- merge(reliability_height, reliability_weight)
    z <- merge(z, reliability_muac)

    return(z)
  })

  output$std_reliability_table <- DT::renderDT(
    expr = std_reliability(),
    options = list(scrollX = TRUE, pageLength = 10)
  )

  ##
  std_bias <- reactive({
    x <- summary_measure(df = std_test_df_long(),
                         measures = "measure_value",
                         index = c("observer", "measure_type"))

    y <- summary_measure(df = std_test_df_long(),
                         measures = "measure_value",
                         index = "measure_type")

    bias_height <- estimate_bias(
      msur = x$mean[x$observer != "Supervisor" & x$measure_type == "height"],
      msup = x$mean[x$observer == "Supervisor" & x$measure_type == "height"],
      mall = y$mean[y$measure_type == "height"]
    )

    bias_height <- data.frame(observer = unique(x$observer[x$observer != "Supervisor"]),
                              measure_type = "height",
                              bias_height)

    bias_weight <- estimate_bias(
      msur = x$mean[x$observer != "Supervisor" & x$measure_type == "weight"],
      msup = x$mean[x$observer == "Supervisor" & x$measure_type == "weight"],
      mall = y$mean[y$measure_type == "weight"]
    )

    bias_weight <- data.frame(observer = unique(x$observer[x$observer != "Supervisor"]),
                              measure_type = "weight",
                              bias_weight)

    bias_muac <- estimate_bias(
      msur = x$mean[x$observer != "Supervisor" & x$measure_type == "muac"],
      msup = x$mean[x$observer == "Supervisor" & x$measure_type == "muac"],
      mall = y$mean[y$measure_type == "muac"]
    )

    bias_muac <- data.frame(observer = unique(x$observer[x$observer != "Supervisor"]),
                            measure_type = "muac",
                            bias_muac)



    z <- data.frame(rbind(bias_height, bias_weight, bias_muac))

    return(z)
  })

  output$std_bias_table <- DT::renderDT(
    expr = std_bias(),
    options = list(scrollX = TRUE, pageLength = 10)
  )

  ##
  output$std_test_outputs <- renderUI({
    req(input$std_test_data)
    tabBox(
      id = "std_outputs",
      title = "Anthropometric Standardisation Results",
      width = 8,
      height = "650px",
      tabPanel(
        title = "Intra-TEM",
        value = "intra-tem",
        plotlyOutput("std_tem_individual_plot")
      ),
      tabPanel(
        title = "Inter-TEM",
        value = "inter-tem",
        valueBox(
          value = round(std_tem_team()$height, digits = 2),
          subtitle = "Team TEM - Height",
          icon = icon(name = "ruler-vertical",
                      lib = "font-awesome",
                      class = "fa-med"),
          color = "purple",
          width = 4
        ),
        valueBox(
          value = round(std_tem_team()$weight, digits = 2),
          subtitle = "Team TEM - Weight",
          icon = icon(name = "weight",
                      lib = "font-awesome",
                      class = "fa-med"),
          color = "light-blue",
          width = 4
        ),
        valueBox(
          value = round(std_tem_team()$muac, digits = 2),
          subtitle = "Team TEM - MUAC",
          icon = icon(name = "ruler",
                      lib = "font-awesome",
                      class = "fa-med"),
          color = "orange",
          width = 4
        )
      ),
      tabPanel(
        title = "Reliability",
        value = "reliability",
        valueBox(
          value = round(std_tem_total()$height, digits = 2),
          subtitle = "Total TEM - Height",
          icon = icon(name = "ruler-vertical",
                      lib = "font-awesome",
                      class = "fa-med"),
          color = "purple",
          width = 4
        ),
        valueBox(
          value = round(std_tem_total()$weight, digits = 2),
          subtitle = "Total TEM - Weight",
          icon = icon(name = "weight",
                      lib = "font-awesome",
                      class = "fa-med"),
          color = "light-blue",
          width = 4
        ),
        valueBox(
          value = round(std_tem_total()$muac, digits = 2),
          subtitle = "Total TEM - MUAC",
          icon = icon(name = "ruler",
                      lib = "font-awesome",
                      class = "fa-med"),
          color = "orange",
          width = 4
        ),
        DT::DTOutput("std_reliability_table")
      ),
      tabPanel(
        title = "Bias",
        value = "bias",
        DT::DTOutput("std_bias_table")
      ),
      tabPanel(
        title = "Data",
        value = "std_data",
        DT::DTOutput("std_test_table")
      )
    )
  })
}


