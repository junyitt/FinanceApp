library(shiny)
library(shinydashboard)
library(plotly)
library(shinyjs)
library(shinyBS)
library(ECharts2Shiny)
library(DT)

#Saving variable
ic_option <- fluidRow(
    column(width = 3,
           h5("Saving Options")
    ),
    column(width = 2,
           selectInput("var", 
                       label = NULL,
                       choices = c("Monthly", 
                                   "Yearly"
                       ),
                       selected = "Monthly")
    ))

ic_amount <- fluidRow(
    column(width = 3,
           h5("Saving Amount (RM):")
    ),
    column(width = 2,
           numericInput("numInput",
                        label=NULL,
                        value = 200)
    ))

ic_fdrate <- fluidRow(
    column(width = 3,
           h5("FD interest rate(%):")
    ),
    column(width = 2,
           numericInput("AIR",
                        label=NULL,
                        value = 4)
    ))

ic_years_s <- fluidRow(
    column(width = 3,
           h5("Duration of saving (Years):")
    ),
    column(width = 2,
           numericInput("range_2",
                        label=NULL,
                        value = 3)
    ))

saving_but <- fluidRow(
    column(width=3,
           actionButton("action_2","Saving Projection"))
)

ic_save_tab <- fluidRow(
    column(width = 8,
           dataTableOutput("mytable_2"))
)