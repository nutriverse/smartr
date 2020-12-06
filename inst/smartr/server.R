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
#
# Define server logic for application
#
server <- function(input, output, session) {
  ##

  ## Sample size
  ss_anthro_calc <- reactive({
    req(input$est_prevalence, input$precision, input$deff)

    sampsizer::get_ss_prevalence(p = input$est_prevalence / 100,
                                 c = input$precision / 100,
                                 deff = input$deff,
                                 fpc = input$fpc,
                                 pop = if (input$fpc) 10000 else NULL)
  })

  ## Create a ss_anthro output
  output$ss_anthro <- renderText({ ss_anthro_calc() })
}
