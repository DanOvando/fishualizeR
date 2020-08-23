library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(here)
library(shinyalert)
library(janitor)
library(readr)
library(ggplot2)
library(purrr)
library(dplyr)
library(tidyr)
library(vroom)
library(hrbrthemes)

shiny_theme <- hrbrthemes::theme_ipsum(base_size = 14,
                                       axis_title_size = 16)

ggplot2::theme_set(shiny_theme)
functions <- list.files(here::here("R"))

purrr::walk(functions, ~ source(here::here("R", .x)))


function(input,output){


  header  <-  dashboardHeader(title = "fishualizeR")

  # sidebar -----------------------------------------------------------------
  sidebar <- dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Description", tabName = "description", icon = icon("book-reader")),
      menuItem("Length Composition",tabName = "lcomps", icon = icon("chart-line")),
      menuItem("CPUE",tabName = "cpue", icon = icon("chart-line"))
    )) # close dashboardSidebar


  # body -----------------------------------------------------------------
  body <-  dashboardBody(
    useShinyalert(),
    tabItems(
    tabItem(tabName = "description",
    h1("fishualizeR"),
    "fishualizeR helps users visualize common fishery related data. To start, click on the 'Length Composition' tab."),
    tabItem(tabName = "lcomps",
      fluidRow(box(title = "Load Data", "This will be a button for loading your data and specifying details about your data", width = 12)),
    fluidRow(box(title = "Length Composition Data", dataTableOutput("lcomps"), width = 12, solidHeader = TRUE, collapsible = TRUE)),
    fluidRow(
    tabBox(
    title = "Examine Raw Data",
    id = "inspect",
    tabPanel(
    "Plot Raw Data",
    box(uiOutput("inspectplot_x")),
    box(uiOutput("inspectplot_y")),
    plotOutput("inspectplot",
               height = "300px"),
    br(),
    br()
    ),
    br(),
    br(),
    br(),
    br(),
    tabPanel(title = "Modify Raw Data",{
      "Placeholder for some minimal data filtering"
    }
    ),
    width = 12)
    ), # close inspect fluidrow
    fluidRow(
      box(title = "Aggregate Lenth Data", width = 12,solidHeader = TRUE, collapsible = TRUE,
          uiOutput("select_ldata")
          ,
      "If rows represent tallies per length bin, select column with number of length bin",
      uiOutput("select_tally"),
      uiOutput("select_groupers"),
      actionButton("group","Aggregate Data"),
      dataTableOutput("grouped_lcomps"),
      downloadButton("download_grouper", "Download Aggregated Data"),
      )
    ),
    fluidRow(
      box(title = "Plot Aggregated Lenth Data", width = 12,solidHeader = TRUE, collapsible = TRUE,
      box(uiOutput("grouped_plot_x")),
      box(uiOutput("grouped_plot_y")),
      box(uiOutput("grouped_plot_fill")),
      box(uiOutput("grouped_plot_facet")),
      actionButton("plot_groupers","Plot Aggregate Data"),
      plotOutput("lcomp_plot"),
      plotOutput("group_plot")
      )
    )
    ) # close lcomps tab
  )
)
  dashboardPage(
    header,
    sidebar,
    body
  ) # close dashboard Padte

} # close server function
