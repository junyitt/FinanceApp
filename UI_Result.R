library(shiny)
library(shinydashboard)
library(plotly)
library(shinyjs)
library(shinyBS)
library(ECharts2Shiny)
library(DT)

#Inflation rate
inflationRate <- fluidRow(
    column(width = 7,
           h5("Inflation Rate (%):")
    ),
    column(width = 2,
           numericInput("inflationR",
                        label=NULL,
                        value = 10)
    ))

#Expenses growth
expenseGrowth <- fluidRow(
    column(width = 7,
           h5("Expenses growth(due to higher standard of living) (%):")
    ),
    column(width = 2,
           numericInput("expenseG",
                        label=NULL,
                        value = 10)
    ))
# Result
resultbutton <- fluidRow(
    column(width = 3,
           actionButton("runresult","Run Result")
    )
)