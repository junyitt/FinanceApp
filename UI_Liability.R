library(shiny)
library(shinydashboard)
library(plotly)
library(shinyjs)
library(shinyBS)
library(ECharts2Shiny)
library(DT)

#House loan variable
lia_houseP <- fluidRow(
    column(width = 3,
           h5("Property price (RM):")
    ),
    column(width = 2,
           numericInput(inputId = "Property price (RM)",
                        label = NULL,
                        value = 500000,
                        width = "20em")
    ))

lia_houseDP<-fluidRow(
    column(width = 3,
           h5("Down payment (RM):")
    ),
    column(width = 2,
           numericInput(inputId = "Down payment (RM)",
                        label = NULL,
                        value = 50000,
                        width = "20em")
    ))

lia_houseLP <- fluidRow(
    column(width = 3,
           h5("Loan duration (years):")
    ),
    column(width = 2,
           numericInput(inputId = "Loan duration (years)",
                        label = NULL,
                        value = 30,
                        width = "20em")
    ))

lia_houseIR <- fluidRow(
    column(width = 3,
           h5("Interest rate (%):")
    ),
    column(width = 2,
           numericInput(inputId = "Interest rate (%)",
                        label = NULL,
                        value = 4.5,
                        width = "20em")
    ))

lia_houseAge <- fluidRow(
    column(width = 3,
           h5("Age to buy the house:")
    ),
    column(width = 2,
           numericInput(inputId = "Age",
                        label = NULL,
                        value = 29,
                        width ="20em")
    ))

houseDetails <- c("Property price (RM)","Down payment (RM)","Loan duration (years)","Interest rate (%)","Age")

lia_houseAdd <- fluidRow(
    column(width = 3,
           actionButton("addLH","Add")
    )
)

lia_houseTab <- fluidRow(
    column(width = 12,
           dataTableOutput("houseInput"))
)

houseIR_table <- fluidRow(
    column(width = 12,
           dataTableOutput("houseLoanRate"))
)

#Car loan variable
lia_carP <- fluidRow(
    column(width = 3,
           h5("Car price (RM):")
    ),
    column(width = 2,
           numericInput(inputId = "Car price (RM) ",
                        label = NULL,
                        value = 50000,
                        width = "20em")
    ))

lia_carDP <- fluidRow(
    column(width = 3,
           h5("Down payment (RM):")
    ),
    column(width =2,
           numericInput(inputId = "Down payment (RM) ",
                        label = NULL,
                        value = 5000,
                        width = "20em")
    ))

lia_carLP <- fluidRow(
    column(width = 3,
           h5("Loan duration (years):")
    ),
    column(width =2,
           numericInput(inputId = "Loan duration (years) ",
                        label = NULL,
                        value = 7,
                        width = "20em")
    ))

lia_carIR <- fluidRow(
    column(width = 3,
           h5("Interest rate (%):")
    ),
    column(width =2,
           numericInput(inputId = "Interest rate (%) ",
                        label = NULL,
                        value = 2.5,
                        width = "20em")
    ))

lia_carAge <- fluidRow(
    column(width = 3,
           h5("Age to buy the car:")
    ),
    column(width = 2,
           numericInput(inputId = "Age ",
                        label = NULL,
                        value = 26,
                        width ="20em")
    ))

carDetails <- c("Car price (RM) ","Down payment (RM) ","Loan duration (years) ","Interest rate (%) ","Age ")

lia_carAdd <- fluidRow(
    column(width = 3,
           actionButton("addLC","Add")
    )
)

lia_carTab <- fluidRow(
    column(width = 12,
           dataTableOutput("carInput"))
)

carIR_table <- fluidRow(
    column(width = 12,
           dataTableOutput("carLoanRate"))
)