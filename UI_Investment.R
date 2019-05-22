library(shiny)
library(shinydashboard)
library(plotly)
library(shinyjs)
library(shinyBS)
library(ECharts2Shiny)
library(DT)

#Investment variable
ind_amount <- fluidRow(
    column(width = 3,
           h5("Investment Amount (RM):")
    ),
    column(width = 2,
           numericInput("indInput",
                        label=NULL,
                        value = 200)
    ))

ind_hangseng <- fluidRow(
    column(width = 3,
           h5("HSI (%):")
    ),
    column(width = 2,
           numericInput("hangseng",
                        label=NULL,
                        value = 20)
    )) 

ind_nasdaq <- fluidRow(
    column(width = 3,
           h5("NASDAQ (%):")
    ),
    column(width = 2,
           numericInput("nasdaq",
                        label=NULL,
                        value = 20)
    ))

ind_nikkei <- fluidRow(
    column(width = 3,
           h5("NIKKEI (%):")
    ),
    column(width = 2,
           numericInput("nikkei",
                        label=NULL,
                        value = 20)
    ))   

ind_bursa <- fluidRow(
    column(width = 3,
           h5("KLSE (%):")
    ),
    column(width = 2,
           numericInput("bursa",
                        label=NULL,
                        value = 20)
    ))

ind_sp500 <- fluidRow(
    column(width = 3,
           h5("SP500 (%):")
    ),
    column(width = 2,
           numericInput("sp500",
                        label=NULL,
                        value = 20)
    ))

ind_status<-fluidRow(
    column(width = 10,
           span(textOutput("status"), style="color:red")
           
    ))

index_but <- fluidRow(
    column(width=3,
           actionButton("action_3","Investment Projection"))
)

ic_index_tab <- fluidRow(
    column(width = 8,
           dataTableOutput("index_tab"))
)

index_proj_res <- fluidRow(
    column(width = 8,
           dataTableOutput("index_proj"))
)