################################################################################
#
#
# This is a Shiny web application port of the SMART ENA software.
#
# This code is for the global R requirements of the Shiny web application.
#
#
################################################################################


################################################################################
#
# Set-up
#
################################################################################

## Load libraries
if(!require(shiny)) install.packages("shiny")
if(!require(shinydashboard)) install.packages("shinydashboard")
if(!require(magrittr)) install.packages("magrittr")
if(!require(DT)) install.packages("DT")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(plotly)) install.packages("plotly")
if(!require(remotes)) install.packages("remotes")

## Load dev packages
if(!require(sampsizer)) remotes::install_github("ernestguevarra/sampsizer")
if(!require(anthrocheckr)) remotes::install_github("nutriverse/anthrocheckr")
if(!require(nutricheckr)) remotes::install_github("nutriverse/nutricheckr")

## Katilingban theme
katilingban_theme <- theme_bw() +
  theme(panel.border = element_rect(colour = "gray50",
                                    size = 0.5),
        panel.grid.major = element_line(linetype = 1,
                                        size = 0.1,
                                        colour = "gray90"),
        panel.grid.minor = element_line(linetype = 0),
        strip.background = element_rect(colour = "gray50",
                                        fill = "gray70"),
        strip.text = element_text(colour = "white", size = 12),
        legend.text = element_text(size = 12),
        legend.key = element_rect(linetype = 0),
        legend.key.size = unit(0.5, "cm"),
        legend.position = "top",
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(colour = "gray50", size = 0.5))
