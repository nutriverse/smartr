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
        column(
          width = 12,
          box(
            title = "Sampling and Sample Size",
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
            solidHeader = TRUE,
            status = "primary",
            numericInput(
              inputId = "est_p",
              label = "Estimated prevalence (%)",
              value = 20,
              min = 0,
              max = 100,
              step = 1
            )
          ),
          box(
            title = "Sample size - mortality rate",
            solidHeader = TRUE,
            status = "primary"
          )
        ),
        column(
          width = 6,
          box(
            title = "Cluster sampling",
            solidHeader = TRUE,
            status = "primary"
          )
        )
      ),
      tabItem(
        tabName = "training"
      )
    )
  )
)
