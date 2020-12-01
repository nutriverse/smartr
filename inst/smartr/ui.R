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
            width = 6,
            box(
              title = "Sampling and Sample Size",
              width = NULL,
              solidHeader = TRUE,
              status = "primary",
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
              solidHeader = TRUE,
              status = "primary",
              div(style="display:inline-block; vertical-align:middle;",
                numericInput(
                  inputId = "est_p",
                  label = "Estimated prevalence (%)",
                  value = 20,
                  min = 0,
                  max = 100,
                  step = 1,
                  width = "225px"
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
                  width = "225px"
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
                  width = "225px"
                )
              ),
              div(style="display:inline-block; vertical-align:middle;",
                numericInput(
                  inputId = "u5_perc",
                  label = "% of children under 5 years",
                  value = 15,
                  min = 5,
                  max = 30,
                  step = 0.1,
                  width = "225px"
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
                  width = "225px"
                )
              ),
              div(style="display:inline-block; vertical-align:middle;",
                numericInput(
                  inputId = "hh_nr",
                  label = "% of non-responding households",
                  value = 3,
                  min = 1,
                  max = 10,
                  step = 1,
                  width = "225px"
                )
              ),
              valueBox(
                value = "X",
                subtitle = "Children to be included",
                icon = icon(name = "child",
                            lib = "font-awesome",
                            class = "fa-med"),
                width = 6
              ),
              valueBox(
                value = "Y",
                subtitle = "Households to be included",
                icon = icon(name = "house-user",
                            lib = "font-awesome",
                            class = "fa-med"),
                width = 6
              )
            ),
            box(
              title = "Sample size - mortality rate",
              width = NULL,
              solidHeader = TRUE,
              status = "primary"
            )
          ),
          column(
            width = 6,
            box(
              title = "Cluster sampling",
              width = NULL,
              solidHeader = TRUE,
              status = "primary"
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
