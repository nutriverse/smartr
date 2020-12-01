################################################################################
#
#
# This is a Shiny web application port of the SMART ENA software.
#
# This is the code for the user interface (UI) of the Shiny web application.
#
#
################################################################################


################################################################################
#
# UI for web application
#
################################################################################
#
# Define UI for application
#
ui <- dashboardPage(
  skin = "green",
  ## Header
  dashboardHeader(
    title = "smartr",
    titleWidth = 300
  ),
  ## Sidebar
  dashboardSidebar(
    width = 300,
    sidebarSearchForm(
      textId = "searchText",
      buttonId = "searchButton"
    ),
    ## Sidebar menu
    sidebarMenu(
      id = "tabs",
      menuItem(
        tabName = "planning",
        text = "Planning",
        icon = icon(name = "clipboard",
                    lib = "font-awesome",
                    class = "fa-lg")
      ),
      menuItem(
        tabName = "training",
        text = "Training",
        icon = icon(name = "chalkboard",
                    lib = "font-awesome",
                    class = "fa-med")
      ),
      menuItem(
        tabName = "anthropometry",
        text = "Anthropometry",
        icon = icon(name = "child",
                    lib = "font-awesome",
                    class = "fa-lg")
      ),
      menuItem(
        tabName = "death",
        text = "Death rates",
        icon = icon(name = "skull-crossbones",
                    lib = "font-awesome",
                    class = "fa-lg")
      ),
      menuItem(
        tabName = "food",
        text = "Food Security",
        icon = icon(name = "seedling",
                    lib = "font-awesome",
                    class = "fa-lg")
      ),
      menuItem(
        tabName = "options",
        text = "Options",
        icon = icon(name = "cog",
                    lib = "font-awesome",
                    class = "fa-lg")
      )
    )
  ),
  ## Body
  dashboardBody(
    ## Specify custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    ## Body outputs for every menu item on sidebar
    tabItems(
      ## Planning
      tabItem(
        tabName = "planning",
        fluidRow(
          column(
            width = 4,
            box(
              title = "Sampling and Sample Size",
              width = NULL,
              solidHeader = FALSE,
              status = "success",
              div(style="display:inline-block; vertical-align:middle;",
                textInput(
                  inputId = "survey_name",
                  label = "Name of survey",
                  placeholder = "Name of survey"
                )
              ),
              div(style="display:inline-block; vertical-align:middle;",
                radioButtons(
                  inputId = "survey_sampling_type",
                  label = "Sampling",
                  choices = c("Random", "Cluster"),
                  inline = TRUE,
                  selected = "Cluster"
                )
              )
            ),
            box(
              title = "Sample size - anthropometry",
              width = NULL,
              solidHeader = FALSE,
              status = "success",
              div(style="display:inline-block; vertical-align:middle;",
                numericInput(
                  inputId = "est_prevalence",
                  label = "Estimated prevalence (%)",
                  value = 20,
                  min = 0,
                  max = 100,
                  step = 1,
                  width = "150px"
                )
              ),
              div(style="display:inline-block; vertical-align:middle;",
                numericInput(
                  inputId = "hh_size",
                  label = "Average household size",
                  value = 5,
                  min = 1,
                  max = 20,
                  step = 1,
                  width = "150px"
                )
              ),
              div(style="display:inline-block; vertical-align:middle;",
                numericInput(
                  inputId = "precision",
                  label = "Desired precision (%)",
                  value = 5,
                  min = 1,
                  max = 10,
                  step = 0.5,
                  width = "150px"
                )
              ),
              div(style="display:inline-block; vertical-align:middle;",
                numericInput(
                  inputId = "u5_perc",
                  label = "% under 5 years",
                  value = 15,
                  min = 5,
                  max = 30,
                  step = 0.1,
                  width = "150px"
                )
              ),
              div(style="display:inline-block; vertical-align:middle;",
                numericInput(
                  inputId = "deff",
                  label = "Design effect",
                  value = 1.5,
                  min = 1,
                  max = 5,
                  step = 0.1,
                  width = "150px"
                )
              ),
              div(style="display:inline-block; vertical-align:middle;",
                numericInput(
                  inputId = "hh_nr",
                  label = "% non-response",
                  value = 3,
                  min = 1,
                  max = 10,
                  step = 1,
                  width = "150px"
                )
              ),
              valueBox(
                value = "X",
                subtitle = "Children to be included",
                icon = icon(name = "child",
                            lib = "font-awesome",
                            class = "fa-med"),
                color = "light-blue",
                width = 6
              ),
              valueBox(
                value = "Y",
                subtitle = "Households to be included",
                icon = icon(name = "house-user",
                            lib = "font-awesome",
                            class = "fa-med"),
                color = "light-blue",
                width = 6
              )
            ),
            box(
              title = "Sample size - mortality rate",
              width = NULL,
              solidHeader = FALSE,
              status = "success",
              div(style="display:inline-block; vertical-align:middle;",
                numericInput(
                  inputId = "death_rate",
                  label = "Estimated death rate per 10,000/day",
                  value = 0.5,
                  min = 0.1,
                  max = 2,
                  step = 0.1,
                  width = "150px"
                )
              ),
              div(style="display:inline-block; vertical-align:middle;",
                numericInput(
                  inputId = "death_precision",
                  label = "Desired precision per 10,000/day",
                  value = 0.3,
                  min = 0.1,
                  max = 1,
                  step = 0.1,
                  width = "150px"
                )
              ),
              div(style="display:inline-block; vertical-align:middle;",
                numericInput(
                  inputId = "death_deff",
                  label = "Design effect",
                  value = 1.5,
                  min = 1,
                  max = 10,
                  step = 0.5,
                  width = "150px"
                )
              ),
              div(style="display:inline-block; vertical-align:middle;",
                numericInput(
                  inputId = "recall",
                  label = "Recall period (days)",
                  value = 93,
                  min = 10,
                  max = 180,
                  width = "150px"
                )
              ),
              valueBox(
                value = "X",
                subtitle = "Population to be included",
                icon = icon(name = "child",
                            lib = "font-awesome",
                            class = "fa-med"),
                color = "purple",
                width = 6
              ),
              valueBox(
                value = "Y",
                subtitle = "Households to be included",
                icon = icon(name = "house-user",
                            lib = "font-awesome",
                            class = "fa-med"),
                color = "purple",
                width = 6
              )
            )
          ),
          column(
            width = 8,
            box(
              title = "Cluster sampling",
              width = NULL,
              solidHeader = FALSE,
              status = "success"
            )
          )
        )
      ),
      tabItem(
        tabName = "training"
      )
    )
  )
)
