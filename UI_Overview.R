library(shiny)
library(shinydashboard)
library(plotly)
library(shinyjs)
library(shinyBS)
library(ECharts2Shiny)
library(DT)

#Overview
oTitle <- fluidRow(
    column(width = 12,
           tags$div(
               HTML(paste(tags$span(style="font-weight:bold; color:#12454f", "FiNap"))), style = "font-size:1.8em",align = "center"),
           tags$div(
               HTML(paste(tags$span(style="color:#1c5a66", "- Nap More, Worry Less -"))), style = "font-size:1.4em",align = "center")
    ))

oInfo <- fluidRow(
    column(width =12,
           tags$div(
               HTML(paste("FiNap is a ", 
                          tags$span(style="font-weight:bold; color:#337a87", "financial planning app"),
                          " designed mainly for young working adults to understand their financial status and plan their retirement ahead.",
                          sep = "")), style = "font-size:1.2em"
           ),hr(style = "border-color:white;margin-bottom:0.25em"),
           tags$div(
               HTML(paste("It allows users to input data like ", 
                          tags$span(style="font-weight:bold; color:#337a87", "income, saving, investment"),
                          " and ",
                          tags$span(style="font-weight:bold; color:#337a87", "expense"),
                          ", in addition to ",
                          tags$span(style="font-weight:bold; color:#337a87", "home buying, car buying"),
                          " and ", 
                          tags$span(style="font-weight:bold; color:#337a87", " child education cost"),
                          ". It then does all the calculation and forecasting based on the input and assumptions.",
                          sep = "")), style = "font-size:1.2em"
           ),hr(style = "border-color:white;margin-bottom:0.25em"),
           tags$div(
               HTML(paste("Finally, it project users' future income and expenses allowing users to see their net worth at the age of retirement and whether at any point in their life, they are in debt."))
               ,style = "font-size:1.2em"), 
           hr(style = "border-color:white;margin-bottom:0.25em"), 
           tags$div(
               HTML(paste("By having a good financial planning ahead of time, young working adults will be able to ", 
                          tags$span(style="font-weight:bold; color:#337a87", "avoid the pitfall of being debt-ridden"),
                          " and have enough savings for a ",
                          tags$span(style="font-weight:bold; color:#337a87", "prosperous retirement life."),
                          sep = "")), style = "font-size:1.2em"
           )
    ))