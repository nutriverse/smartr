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
if(!require(remotes)) install.packages("remotes")

## Load dev packages
if(!require(sampsizer)) remotes::install_github("ernestguevarra/sampsizer")
if(!require(anthrocheckr)) remotes::install_github("nutriverse/anthrocheckr")
if(!require(nutricheckr)) remotes::install_github("nutriverse/nutricheckr")

