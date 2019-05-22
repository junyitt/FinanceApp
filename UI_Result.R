library(shiny)
library(shinydashboard)
library(plotly)
library(shinyjs)
library(shinyBS)
library(ECharts2Shiny)
library(DT)

# Result
resultbutton <- fluidRow(
    column(width = 3,
           actionButton("runresult","Run Result")
    )
)