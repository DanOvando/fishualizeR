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
    fluidRow(box(title = "Length Composition Data",
                 uiOutput("colnames"),
                 dataTableOutput("lcomps"), width = 12, solidHeader = TRUE, collapsible = TRUE)),
    fluidRow(
      box(
        uiOutput("select_ldata"),
        "If rows represent tallies per length bin, select column with number of length bin. Otherwise leave as NA",
        uiOutput("select_tally")
      )
    ),
    fluidRow(
    tabBox(
    title = "Examine Raw Data",
    id = "inspect",
    tabPanel(
    "Plot Raw Data",
    "Use this area to explore your data data",
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
    tabPanel(title = "Modify Raw Data",
      "Use this tab to correct units in the length data. For example, divide all fish with length measurements over 100 by 10.",
      uiOutput("max_length"),
      uiOutput("max_length_factor"),
      uiOutput("min_length"),
      uiOutput("min_length_factor"),
      actionButton("correct_units","Correct Length Units"),
      actionButton("reset","Reset to Original Data")

    ),
    width = 12)
    ), # close inspect fluidrow
    fluidRow(
      box(title = "Assess Data Coverage", width = 12, solidHeader = TRUE, collapsible = TRUE,
          "Use this area to assess your data coverage",
          uiOutput("cov_var_1"),
          uiOutput("cov_var_2"),
          uiOutput("cov_var_3"))),
    fluidRow(plotOutput("data_tally_plot")),
    fluidRow(dataTableOutput("data_tally")),
    fluidRow(
      box(title = "Aggregate Length Data", width = 12,solidHeader = TRUE, collapsible = TRUE,
      "Use this area to bin your length data by different groups. For example, you can count the total number of individuals in each length bin by year and month and region",
      uiOutput("select_groupers"),
      "Use this area to specify a bin width for your length data (unless your data are already binned)",
      numericInput("bin_width","Length Bin Width", min = 0, value = NA),
      actionButton("group","Aggregate Data"),
      dataTableOutput("grouped_lcomps"),
      downloadButton("download_grouper", "Download Aggregated Data"),
      )
    ),
    fluidRow(
      box(title = "Plot Aggregated Length Data", width = 12,solidHeader = TRUE, collapsible = TRUE,
      "Use this area to examine your aggregated data",
      box(uiOutput("grouped_plot_x")),
      box(uiOutput("grouped_plot_y")),
      box(uiOutput("grouped_plot_fill")),
      box(uiOutput("grouped_plot_facet")),
      checkboxInput("factorfill","Convert Color Variable to Categorical?", value = FALSE),
      actionButton("plot_groupers","Plot Aggregated Data"),
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
