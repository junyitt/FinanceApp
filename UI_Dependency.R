library(shiny)
library(shinydashboard)
library(plotly)
library(shinyjs)
library(shinyBS)
library(ECharts2Shiny)
library(DT)

#Dependency variable
raiseKidAge <- fluidRow(
    column(width = 6,
           numericInput(inputId = "Age (to have kid)",
                        label = h5("Age to have the kid:"),
                        value = 30,
                        width ="20em")
    ))

raiseKidOpt <- fluidRow(
    column(width = 6,
           selectInput("Cost of raising a kid (by income group)",h5("Cost of raising a kid (for different income group):"),
                       choices = c("Low Income Group",
                                   "Middle Income Group",
                                   "High Income Group"),
                       selected = "Low Income Group")
    ))

kidDetails <- c("Age (to have kid)","Cost of raising a kid (by income group)")

raiseKidAdd <- fluidRow(
    column(width = 3,
           actionButton("addDK","Add")
    )
)

raiseKidTab <- fluidRow(
    column(width = 12,
           dataTableOutput("kidInput"))
)

raiseKidC_table <- fluidRow(
    column(width = 12,
           tableOutput("raiseKidCost")
    )
)