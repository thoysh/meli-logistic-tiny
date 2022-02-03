## ui.R ##
# Define a interface de usuário (UI) do Shiny web app

# Shiny libraries ----
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(shinyjs)
library(shinyalert)
library(shinycssloaders)
# DB libraries ----
library(mongolite)
# Common libraries ----
library(shinymanager)
library(tidyverse)
library(stringr)
library(lubridate)
# Log libraries ----
library(futile.logger)
# Other libraries ----
library(jsonlite)
library(readxl)
library(writexl)

# Faz source de todos os arquivos .R
list.files("R", pattern = "\\.R$", full.names = T, recursive = T) %>%
  walk(~source(.x, encoding = "UTF-8"))


# Define o UI header ----
mainHeader <- dashboardHeader(
  title = "TRANSITARIOLOG AP"
)


# Define UI sidebar ----
mainSidebar <- dashboardSidebar(
  useShinyjs(),    # Setup shinyjs
  useShinyalert(), # Setup shinyalert
  sidebarMenu(menuItem("Rotas", icon = icon("route"), tabName = "tabItem_route"),
              menuItem("Eventos", icon = icon("calendar-alt"), tabName = "tabItem_event"),
              menuItem("Relatórios", icon = icon("file-invoice-dollar"), tabName = "tabItem_report"),
              menuItem("Extratos", icon = icon("receipt"), tabName = "tabItem_extract"),
              menuItem("Motoristas", icon = icon("car-side"), tabName = "tabItem_driver")
  )
)


# Define UI body ----
mainBody <- dashboardBody(
  tags$head(
    tags$style(HTML(".shiny-output-error-validation { color: #ff0000; font-weight: bold; }"))
  ),

  tabItems(
    tabItem("tabItem_route", routeUI("route")),
    tabItem("tabItem_event", eventUI("event")),
    tabItem("tabItem_driver", driverUI("driver")),
    tabItem("tabItem_report", reportUI("report"))
  )
)


# Define a UI ----
ui <- #secure_app(
  dashboardPage(
    mainHeader,
    mainSidebar,
    mainBody,
    skin = "black"
  )
#)
