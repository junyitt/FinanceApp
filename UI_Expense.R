library(shiny)
library(shinydashboard)
library(plotly)
library(shinyjs)
library(shinyBS)
library(ECharts2Shiny)
library(DT)

#Expense
#Monthly expense variable
mx_food <- fluidRow(
    column(width = 3,
           h5("Food (RM):")
    ),
    column(width = 2,
           numericInput(inputId = "mx1",
                        label = NULL,
                        value = 600,
                        width = "20em")
    ))

mx_transport <- fluidRow(
    column(width = 3,
           h5("Petrol/transport (RM):")
    ),
    column(width = 2,
           numericInput(inputId = "mx2",
                        label = NULL,
                        value = 200,
                        width = "20em")
    ))

mx_rent <- fluidRow(
    column(width = 3,
           h5("Rent (RM):")
    ),
    column(width = 2,
           numericInput(inputId = "mx3",
                        label = NULL,
                        value = 650,
                        width = "20em")
    ))

mx_water <- fluidRow(
    column(width = 3,
           h5("Water bill (RM):")
    ),
    column(width = 2,
           numericInput(inputId = "mx4",
                        label = NULL,
                        value = 30,
                        width = "20em")
    ))

mx_electric <- fluidRow(
    column(width = 3,
           h5("Electric bill (RM):")
    ),
    column(width = 2,
           numericInput(inputId = "mx5",
                        label = NULL,
                        value = 100,
                        width = "20em")
    ))

mx_wifi <- fluidRow(
    column(width = 3,
           h5("Wi-Fi bill (RM):")
    ),
    column(width = 2,
           numericInput(inputId = "mx6",
                        label = NULL,
                        value = 145,
                        width = "20em")
    ))

mx_tel <- fluidRow(
    column(width = 3,
           h5("Telephone bill (RM):")
    ),
    column(width = 2,
           numericInput(inputId = "mx7",
                        label = NULL,
                        value = 50,
                        width = "20em")
    ))

mx_astro <- fluidRow(
    column(width = 3,
           h5("Astro bill (RM):")
    ),
    column(width = 2,
           numericInput(inputId = "mx8",
                        label = NULL,
                        value = 150,
                        width = "20em")
    ))

mx_other <- fluidRow(
    column(width = 3,
           textInput("otherMX",
                     h5("Other Expenses (Type):")
           )),
    column(width = 3,
           numericInput(inputId = "otherMX_amount",
                        label = h5("Amount (RM):"),
                        value = NULL
           )),
    column(width = 1,
           actionButton("addMX","Add Expense")
    )
)

mx_output <- fluidRow(
    column(width = 10,
           uiOutput("mxOut")
    )
)

mx_done <- fluidRow(
    column(width = 3,
           actionButton("doneMX","Done")
    )
)

#Non-monthly expense variable
nmx_travel <- fluidRow(
    column(width = 3,
           h5("Travel (RM):")
    ),
    column(width = 2,
           numericInput(inputId = "nmx1",
                        label = NULL,
                        value = 900,
                        width = "180px")
    ),
    column(width = 1,
           h5("every")
    ),
    column(width = 2,
           offset = 0,
           numericInput(inputId = "nmxt1",
                        label = NULL,
                        value = 6,
                        width = "180px")
    ),
    column(width = 1,
           h5("month(s)")
    ))

nmx_carIns <- fluidRow(
    column(width = 3,
           h5("Car insurance (RM):")
    ),
    column(width = 2,
           numericInput(inputId = "nmx2",
                        label = NULL,
                        value = 900,
                        width = "180px")
    ),
    column(width = 1,
           h5("every")
    ),
    column(width = 2,
           offset = 0,
           numericInput(inputId = "nmxt2",
                        label = NULL,
                        value = 6,
                        width = "180px")
    ),
    column(width = 1,
           h5("month(s)")
    ))

nmx_healthIns <- fluidRow(
    column(width = 3,
           h5("Health insurance (RM):")
    ),
    column(width = 2,
           numericInput(inputId = "nmx3",
                        label = NULL,
                        value = 900,
                        width = "180px")
    ),
    column(width = 1,
           h5("every")
    ),
    column(width = 2,
           offset = 0,
           numericInput(inputId = "nmxt3",
                        label = NULL,
                        value = 6,
                        width = "180px")
    ),
    column(width = 1,
           h5("month(s)")
    ))

nmx_other <- fluidRow(
    column(width = 3,
           textInput("otherNMX",
                     h5("Other Expenses (Type):")
           )),
    column(width = 3,
           numericInput(inputId = "otherNMX_amount",
                        label = h5("Amount (RM):"),
                        value = NULL
           )),
    column(width = 3,
           numericInput(inputId = "otherNMX_period",
                        label = h5("Period (months)"),
                        value = NULL
           )),
    column(width = 1,
           actionButton("addNMX","Add Expense")
    )
)

nmx_output <- fluidRow(
    column(width = 10,
           uiOutput("nmxOut")
    )
)

nmx_done <- fluidRow(
    column(width = 3,
           actionButton("doneNMX","Done")
    )
)