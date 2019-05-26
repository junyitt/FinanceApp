library(shiny)
library(shinydashboard)
library(plotly)
library(shinyjs)
library(shinyBS)
library(ECharts2Shiny)
library(DT)


#Income variable 
ic_age <- fluidRow(
    column(width = 3,
           h5("Age (Years):")
    ),
    column(width = 2,
           numericInput(inputId = "age",
                        label=NULL,
                        value = 24,
                        width = '20em')
    ))

ic_month <- fluidRow(
    column(width = 3,
           h5("Age (Months):")
    ),
    column(width = 2,
           selectInput("age_m", 
                       label = NULL,
                       choices = c(0:11),
                       selected = 0)
    ))

ic_income <- fluidRow(
    column(width = 3,
           h5("Current Income (RM):")
    ),
    column(width = 2,
           numericInput("init_income",
                        label=NULL,
                        value = 3000)
    ))

ic_g_rate <- fluidRow(
    column(width = 3,
           h5("Income growth rate (%):")
    ),
    column(width = 2,
           numericInput("growth_rate",
                        label=NULL,
                        value = 5)
    ))

ic_g_dur <- fluidRow(
    column(width = 3,
           h5("Duration for Increment (Years):")
    ),
    column(width = 2,
           numericInput("growth_duration",
                        label=NULL,
                        value = 2)
    ))

ic_p_rate <- fluidRow(
    column(width = 3,
           h5("Income Promotion rate (%):")
    ),
    column(width = 2,
           numericInput("promo_rate",
                        label=NULL,
                        value = 10)
    ))

ic_p_dur <- fluidRow(
    column(width = 3,
           h5("Duration for Promotion (Years):")
    ),
    column(width = 2,
           numericInput("promo_time",
                        label=NULL,
                        value = 5)
    ))

ic_years_i <- fluidRow(
    column(width = 3,
           h5("Duration of projection (Years):")
    ),
    column(width = 2,
           numericInput("range",
                        label=NULL,
                        value = 40)
    ))

income_but <- fluidRow(
    column(width=3,
           actionButton("action","Income Projection"))
)