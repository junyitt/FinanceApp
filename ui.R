library(shiny)
library(shinydashboard)
library(plotly)
library(shinyjs)
library(shinyBS)
library(ECharts2Shiny)
library(DT)
library(readxl)
library(quantmod)
library(dplyr)
library(lubridate)
library(rvest)
library(stringr)
library(ggplot2)
library(expss)
library(quantmod)

#Source UI variables (Input/Buttons/Div etc.)
source("UI_Overview.R")
source("UI_Income.R")
source("UI_Saving.R")
source("UI_Investment.R")
source("UI_Expense.R")
source("UI_Liability.R")
source("UI_Dependency.R")
source("UI_Result.R")

#Sidebar
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Overview",tabName = "overview",icon = icon("search-dollar")),
        menuItem("Income",tabName = "income",icon = icon("wallet")),
        menuItem("Saving",tabName = "saving",icon = icon("coins")),
        menuItem("Investment",tabName = "investment",icon = icon("chart-line")),
        menuItem("Expense",tabName = "expense",icon = icon("receipt") ),
        menuItem("Liability",tabName = "liability",icon = icon("file-invoice-dollar")),
        menuItem("Dependency",tabName = "dependency",icon = icon("child")),
        menuItem("Result",tabName = "result",icon = icon("poll"))
    )
)

#Dashboard 
body <- dashboardBody(
    tabItems(
        tabItem(tabName = "overview",
                tabBox(title = strong("Overview"),
                       id = "tabO", height = "33em", width = "1em",
                       tabPanel("",oTitle,hr(),oInfo)
                )
        ),
        tabItem(tabName = "income",
                tabBox(title = strong("Income"),
                       id = "tabI",height = "30em",width = "1em",
                       tabPanel(strong("Promotion & Increment"),ic_age,ic_month,ic_income,ic_g_rate,
                                ic_g_dur,ic_p_rate,ic_p_dur, ic_years_i,hr(),
                                income_but, hr()),
                       tabPanel(strong("Income Projection Graph"),hr(),
                                plotlyOutput("plot_income"))
                )
        ),
        
        #Saving body
        tabItem(tabName = "saving",
                tabBox(title = strong("Saving"),
                       id = "tabS", height = "60em", width = "1em",
                       tabPanel(strong("Savings"),ic_option,ic_amount,ic_fdrate,ic_years_s, hr(),
                                saving_but,hr(),
                                ic_save_tab),
                       tabPanel(strong("Saving Projection Graph"),hr(),
                                plotlyOutput("plot_saving"))
                )
        ),
        
        #Investment body
        tabItem(tabName = "investment",
                tabBox(title = strong("Investment"),
                       id = "tabInv", height = "170em", width = "1em",
                       tabPanel(strong("Stock Index Investment"),hr(),
                                h5(strong("Percentage to invest for each Market Index (Please refer to Historical Graph & The Monthly Average Return")),ind_amount,
                                ind_hangseng,ind_nasdaq,ind_nikkei,ind_bursa,ind_sp500,hr(),index_but,ind_status,
                                hr(strong("Average Montly Return for each index in %"),ic_index_tab,hr())),
                       tabPanel(strong("Market Index Historical Plot"),hr(),
                                h5(strong("Monthly Performance Graph for each Market Index")),
                                plotlyOutput("bursa_ind"),hr(),plotlyOutput("nikkei_ind"),hr(),plotlyOutput("nasdaq_ind"),hr(),plotlyOutput("sp500_ind"),hr(),plotlyOutput("hangseng_ind")),
                       tabPanel(strong("Market Index Investment Projection Graph"),hr(),
                                plotlyOutput("ind_proj_graph"),index_proj_res)  
                )
        ),
        
        #Expense body
        tabItem(tabName = "expense",
                tabBox(title = strong("Expense"),
                       id = "tabX", height = "20em", width = "1em",
                       tabPanel(strong("Monthly"),mx_food,mx_transport,mx_rent,mx_water,mx_electric,mx_wifi,mx_tel,mx_astro,mx_other,mx_output,hr(),mx_done,hr()),
                       tabPanel(strong("Non-monthly"),nmx_travel,nmx_carIns,nmx_healthIns,nmx_other,nmx_output,hr(),nmx_done,hr()),
                       tabPanel(strong("Pie Chart"),h4(strong("Monthly Expenses"),align = "center"),loadEChartsLibrary(),tags$div(id="mxChart",style = "width:100%;height:400px"), deliverChart(div_id="mxChart"))
                )
        ),
        
        #Liability body
        tabItem(tabName = "liability",
                tabBox(title = strong("Liability"),
                       id = "tabL",height = "90em",width = "1em",
                       tabPanel(strong("House Loan"),
                                lia_houseP,lia_houseDP,lia_houseLP,lia_houseIR,lia_houseAge,lia_houseAdd,
                                hr(strong("Information of House Loans:")),
                                hr(style = "border-color:white;"),
                                lia_houseTab,
                                hr(style = "border-color:white;"),
                                hr(),
                                hr(style = "border-color:white;"),
                                h5(strong("For your reference, the following information are the house loan interest rates for different banks.")),
                                h5(strong("Bank House Loan Interest Rates:")),
                                houseIR_table,
                                h5(em("Source: https://ringgitplus.com/en/home-loan/"))
                       ),
                       tabPanel(strong("Car Loan"),
                                lia_carP,lia_carDP,lia_carLP,lia_carIR,lia_carAge,lia_carAdd,
                                hr(strong("Information of Car Loans:")),
                                hr(style = "border-color:white;"),
                                lia_carTab,
                                hr(style = "border-color:white;"),
                                hr(),
                                hr(style = "border-color:white;"),
                                h5(strong("For your reference, the following information are the car loan interest rates for different banks.")),
                                h5(strong("Bank Car Loan Interest Rates:")),
                                carIR_table,
                                h5(em("Source: https://ringgitplus.com/en/car-loan/"))
                       )
                )
        ),
        
        #Dependency body
        tabItem(tabName = "dependency",
                tabBox(title = strong("Dependency"),
                       id = "tabD",height = "110em",width = "1em",
                       tabPanel("",
                                raiseKidAge,raiseKidOpt,raiseKidAdd,
                                hr(style = "border-color:white;"),
                                dataTableOutput("costTable"),
                                h5(em("Source: https://www.theedgemarkets.com/article/cover-story-cost-raising-child-today/")),
                                hr(style = "border-color:white;"),
                                hr(),
                                plotlyOutput("costGraph")),
                       hr(style = "border-color:white;"),
                       hr(style = "border-color:white;"),
                       hr(strong("Information of Cost of Raising The Kids:")),
                       raiseKidTab
                       
                       # h5(strong("Estimated Cost of Raising A Child in Malaysia")),
                       # raiseKidC_table,
                       # hr(color="grey"),
                       # h5(em("Source: https://www.theedgemarkets.com/article/cover-story-cost-raising-child-today/"))
                )),
        
        # Results
        tabItem(tabName = "result",
                tabBox(title = strong("Result"),
                       id = "tabE",height = "110em",width = "1em",
                       tabPanel(
                           "",
                           resultbutton,
                           # raiseKidAge,raiseKidOpt,raiseKidAdd,
                           hr(style = "border-color:white;"),
                           hr(style = "border-color:white;"),
                           hr(),
                           plotlyOutput("breakdownGraph"),
                           hr(),
                           plotlyOutput("ieGraph"),
                           hr(),
                           plotlyOutput("networthGraph")
                       ),
                       hr(style = "border-color:white;"),
                       hr(style = "border-color:white;")
                ))
    )
)


#Ui
ui <- dashboardPage(
    dashboardHeader(title = "FiNap"),
    sidebar,
    body
)