library(shiny)
library(shinydashboard)
library(plotly)
library(shinyjs)
library(shinyBS)
library(ECharts2Shiny)
library(DT)
library(readxl)
source("loanrate.R")

# setwd("C:/Users/yinyen/Documents/finApp")

mx_food <- fluidRow(
                    column(width = 3,
                           h5("Food (RM):")
                    ),
                    column(width = 2,
                           numericInput(inputId = "mx1",
                                        label = NULL,
                                        value = 900,
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
                                 numericInput(inputId = "nmxt2",
                                              label = NULL,
                                              value = 6,
                                              width = "180px")
                          ),
                          column(width = 1,
                                 h5("month(s)")
                          ))

lia_houseP <- fluidRow(
                      column(width = 3,
                             h5("Property price (RM):")
                      ),
                      column(width = 2,
                             numericInput(inputId = "liaHP",
                                          label = NULL,
                                          value = 600000,
                                          width = "20em")
                      ))

lia_houseDP<-fluidRow(
                       column(width = 3,
                              h5("Down payment (RM):")
              ),
              column(width =2,
                     numericInput(inputId = "liaHDP",
                                  label = NULL,
                                  value = 60000,
                                  width = "20em")
              ))

lia_houseLP<-fluidRow(
                      column(width = 3,
                             h5("Loan period (years):")
              ),
              column(width =2,
                     numericInput(inputId = "liaHLP",
                                  label = NULL,
                                  value = 30,
                                  width = "20em")
              ))

lia_houseIR<-fluidRow(
                      column(width = 3,
                             h5("Interest rate (%):")
                      ),
                      column(width =2,
                             numericInput(inputId = "liaHIR",
                                          label = NULL,
                                          value = 4.35,
                                          width = "20em")
             ))

houseIR_table <- fluidRow(
                          column(width = 12,
                                 dataTableOutput("houseLoanRate"))
                  )

lia_carP <- fluidRow(
                    column(width = 3,
                           h5("Car price (RM):")
                    ),
                    column(width = 2,
                           numericInput(inputId = "liaCP",
                                        label = NULL,
                                        value = 80000,
                                        width = "20em")
            ))

lia_carDP<-fluidRow(
                    column(width = 3,
                           h5("Down payment (RM):")
                    ),
                    column(width =2,
                           numericInput(inputId = "liaCDP",
                                        label = NULL,
                                        value = 8000,
                                        width = "20em")
            ))

lia_carLP<-fluidRow(
                    column(width = 3,
                           h5("Loan period (years):")
                    ),
                    column(width =2,
                           numericInput(inputId = "liaCLP",
                                        label = NULL,
                                        value = 6,
                                        width = "20em")
            ))

lia_carIR <- fluidRow(
                      column(width = 3,
                           h5("Interest rate (%):")
                      ),
                      column(width =2,
                             numericInput(inputId = "liaCIR",
                                          label = NULL,
                                          value = 3.25,
                                          width = "20em")
             ))

carIR_table <- fluidRow(
                        column(width = 12,
                               dataTableOutput("carLoanRate"))
                )

raiseKidCost <- as.data.frame(read_excel("raisekidcost.xlsx"))

raiseKidC_table <- fluidRow(
                            column(width = 12,
                                   tableOutput("raiseKidCost")
                                   )
                   ) 

sidebar <- dashboardSidebar(
           sidebarMenu(
           menuItem("Income",tabName = "income",icon = icon("wallet")),
           menuItem("Expense",tabName = "expense",icon = icon("receipt") ),
           menuItem("Liability",tabName = "liability",icon = icon("file-invoice-dollar")),
           menuItem("Dependency",tabName = "dependency",icon = icon("child"))
    )
)

body <- dashboardBody(
        tabItems(
        tabItem(tabName = "income",
                tabBox(title = strong("Income"),
                       id = "tabI",height = "20em",width = "1em")),
        tabItem(tabName = "expense",
                tabBox(title = strong("Expense"),
                       id = "tabX", height = "20em", width = "1em",
                       tabPanel(strong("Monthly"),mx_food,mx_transport,mx_rent,mx_water,mx_electric,mx_wifi,mx_tel,mx_astro),
                       tabPanel(strong("Non-monthly"),nmx_travel,nmx_carIns,nmx_healthIns),
                       tabPanel(strong("Pie Chart"),h4(strong("Monthly Expenses"),align = "center"),loadEChartsLibrary(),tags$div(id="test",style = "width:100%;height:400px"), deliverChart(div_id="test"))
                )
        ),
        tabItem(tabName = "liability",
                tabBox(title = strong("Liability"),
                       id = "tabL",height = "50em",width = "1em",
                       tabPanel(strong("House Loan"),
                                lia_houseP,lia_houseDP,lia_houseLP,lia_houseIR,
                                hr(color = "grey"),
                                h5(strong("Bank House Loan Interest Rates:")),
                                houseIR_table,
                                h5(em("Source: https://ringgitplus.com/en/home-loan/"))
                                ),
                       tabPanel(strong("Car Loan"),
                                lia_carP,lia_carDP,lia_carLP,lia_carIR,
                                hr(color = "grey"),
                                h5(strong("Bank Car Loan Interest Rates:")),
                                carIR_table,
                                h5(em("Source: https://ringgitplus.com/en/car-loan/"))
                                )
                )
        ),
        tabItem(tabName = "dependency",
                tabBox(title = strong("Dependency"),
                       id = "tabD",height = "50em",width = "1em",
                       h5(strong("Estimated Cost of Raising A Child in Malaysia")),
                       raiseKidC_table,
                       hr(color="grey"),
                       h5(em("Source: https://www.theedgemarkets.com/article/cover-story-cost-raising-child-today/"))
                )
        )
    )
)


ui <- dashboardPage(
    dashboardHeader(title = "Financial Calculator"),
    sidebar,
    body
)

if (interactive()){
server <- function(input, output) {
        output$test <- renderPieChart(div_id = "test",
                       data = c(rep("Food", input$mx1),
                                rep("Transport", input$mx2),
                                rep("Rent", input$mx3),
                                rep("Water bill", input$mx4),
                                rep("Electric bill",input$mx5),
                                rep("Wi-Fi bill",input$mx6),
                                rep("Telephone bill",input$mx7),
                                rep("Astro bill",input$mx8)
                                ))
                       output$houseLoanRate <- renderDataTable(houseLoanRate,
                                                               options = list(orderClasses = TRUE,
                                                                              lengthMenu = c(10,30,50),
                                                                              pageLength = 5))
                       output$carLoanRate <- renderDataTable(carLoanRate,
                                                             options = list(orderClasses = TRUE,
                                                                            lengthMenu = c(10,30,50),
                                                                            pageLength = 5))
                       output$raiseKidCost <- renderTable(raiseKidCost)
                                                            
                                                
}}

shinyApp(ui = ui, server = server)





