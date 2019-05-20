# setwd("C:/Users/yinyen/Documents/finApp")
setwd("C:/Users/jy/Desktop/FinanceApp")
# install.packages(c("shiny", 'shinydashboard', 'plotly', 
                   # 'shinyjs', 'shinyBS', 'ECharts2Shiny',
                   # 'DT', 'readxl', 'quantmod'
                   # 'dplyr', 'lubridate'
                   # ))
# install.packages("rvest")
# install.packages("expss")
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
source('function.R')
source("loanRate.r")
source("raiseKidCost.r")
source('Index_Data.R')
######Choobs variable############################################
index_proj_res <- fluidRow(
  column(width = 8,
         dataTableOutput("index_proj"))
)
ind_amount <- fluidRow(
  column(width = 3,
         h5("Investment Amount (RM):")
  ),
  column(width = 2,
         numericInput("indInput",
                      label=NULL,
                      value = 500)
  ))
ind_status<-fluidRow(
  column(width = 10,
      span(textOutput("status"), style="color:red")

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

ind_hangseng <- fluidRow(
  column(width = 3,
         h5("HSI (%):")
  ),
  column(width = 2,
         numericInput("hangseng",
                      label=NULL,
                      value = 20)
  )) 

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
                     selected = "Yearly")
                     
                    
  ))

ic_amount <- fluidRow(
  column(width = 3,
         h5("Saving Amount (RM):")
  ),
  column(width = 2,
         numericInput("numInput",
                      label=NULL,
                      value = 1000)
  ))


ic_years_i <- fluidRow(
  column(width = 3,
         h5("Duration of projection (Years):")
  ),
  column(width = 2,
         numericInput("range",
                      label=NULL,
                      value = 50)
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
ic_fdrate <- fluidRow(
  column(width = 3,
         h5("FD interest rate(%):")
  ),
  column(width = 2,
         numericInput("AIR",
                      label=NULL,
                      value = 5)
  ))

ic_age <- fluidRow(
  column(width = 3,
         h5("Age (Years):")
  ),
  column(width = 2,
         numericInput(inputId = "age",
                      label=NULL,
                      value = 20,
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
ic_proj_tab <- fluidRow(
  column(width = 8,
         dataTableOutput("mytable"))
)

ic_save_tab <- fluidRow(
  column(width = 8,
         dataTableOutput("mytable_2"))
)
ic_index_tab <- fluidRow(
  column(width = 8,
         dataTableOutput("index_tab"))
)

income_but <- fluidRow(
  column(width=3,
  actionButton("action","Income Projection"))
)

saving_but <- fluidRow(
  column(width=3,
         actionButton("action_2","Saving Projection"))
)
index_but <- fluidRow(
  column(width=3,
         actionButton("action_3","Investment Projection"))
)
######################################################################END

#Monthly expense variable
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
         actionButton("doneMX","Done!")
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
           actionButton("doneNMX","Done!")
    )
)

#House loan variable
lia_houseP <- fluidRow(
  column(width = 3,
         h5("Property price (RM):")
  ),
  column(width = 2,
         numericInput(inputId = "Property price (RM)",
                      label = NULL,
                      value = NULL,
                      width = "20em")
  ))

lia_houseDP<-fluidRow(
  column(width = 3,
         h5("Down payment (RM):")
  ),
  column(width = 2,
         numericInput(inputId = "Down payment (RM)",
                      label = NULL,
                      value = NULL,
                      width = "20em")
  ))

lia_houseLP <- fluidRow(
  column(width = 3,
         h5("Loan duration (years):")
  ),
  column(width = 2,
         numericInput(inputId = "Loan duration (years)",
                      label = NULL,
                      value = NULL,
                      width = "20em")
  ))

lia_houseIR <- fluidRow(
  column(width = 3,
         h5("Interest rate (%):")
  ),
  column(width = 2,
         numericInput(inputId = "Interest rate (%)",
                      label = NULL,
                      value = NULL,
                      width = "20em")
  ))

lia_houseAge <- fluidRow(
  column(width = 3,
         h5("Age to buy the house:")
  ),
  column(width = 2,
         numericInput(inputId = "Age",
                      label = NULL,
                      value = NULL,
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
                      value = NULL,
                      width = "20em")
  ))

lia_carDP <- fluidRow(
  column(width = 3,
         h5("Down payment (RM):")
  ),
  column(width =2,
         numericInput(inputId = "Down payment (RM) ",
                      label = NULL,
                      value = NULL,
                      width = "20em")
  ))

lia_carLP <- fluidRow(
  column(width = 3,
         h5("Loan duration (years):")
  ),
  column(width =2,
         numericInput(inputId = "Loan duration (years) ",
                      label = NULL,
                      value = NULL,
                      width = "20em")
  ))

lia_carIR <- fluidRow(
  column(width = 3,
         h5("Interest rate (%):")
  ),
  column(width =2,
         numericInput(inputId = "Interest rate (%) ",
                      label = NULL,
                      value = NULL,
                      width = "20em")
  ))

lia_carAge <- fluidRow(
  column(width = 3,
         h5("Age to buy the car:")
  ),
  column(width = 2,
         numericInput(inputId = "Age ",
                      label = NULL,
                      value = NULL,
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

#Dependency variable
raiseKidAge <- fluidRow(
  column(width = 6,
         numericInput(inputId = "Age (to have kid)",
                      label = h5("Age to have the kid:"),
                      value = NULL,
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

#Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Income",tabName = "income",icon = icon("wallet")),
    menuItem("Expense",tabName = "expense",icon = icon("receipt") ),
    menuItem("Liability",tabName = "liability",icon = icon("file-invoice-dollar")),
    menuItem("Dependency",tabName = "dependency",icon = icon("child"))
  )
)

#Dashboard 
body <- dashboardBody(
  tabItems(
    ###choobs section
    tabItem(tabName = "income",
            tabBox(title = strong("Income"),
                   id = "tabI",height = "180em",width = "1em",
                   tabPanel(strong("Promotion & Increment"),ic_age,ic_month,ic_income,ic_g_rate,
                            ic_g_dur,ic_p_rate,ic_p_dur, ic_years_i,hr(),
                            income_but, hr(),
                            ic_proj_tab),
                   tabPanel(strong("Income Projection Graph"),hr(),
                            plotlyOutput("plot_income")),
                   tabPanel(strong("Savings"),ic_option,ic_amount,ic_fdrate,ic_years_s, hr(),
                            saving_but,hr(),
                            ic_save_tab),
                   tabPanel(strong("Saving Projection Graph"),hr(),
                            plotlyOutput("plot_saving")),
                   tabPanel(strong("Stock Index Investment"),hr(),
                            h5(strong("Percentage to invest for each Market Index (Please refer to Historical Graph & The Monthly Average Return")),ind_amount,
                            ind_hangseng,ind_nasdaq,ind_nikkei,ind_bursa,ind_sp500,index_but,ind_status,
                            hr(strong("Average Montly Return for each index in %"),ic_index_tab,hr())),
                  tabPanel(strong("Market Index Historical Plot"),hr(),
                            h5(strong("Monthly Performance Graph for each Market Index")),
                            plotlyOutput("bursa_ind"),hr(),plotlyOutput("nikkei_ind"),hr(),plotlyOutput("nasdaq_ind"),hr(),plotlyOutput("sp500_ind"),hr(),plotlyOutput("hangseng_ind")),
                  tabPanel(strong("Market Index Investment Projection Graph"),hr(),
                           plotlyOutput("ind_proj_graph"),index_proj_res)  
                  
                   
                   
                   )
              
                   ),
    ####sectionend
    
    #Expense body
    tabItem(tabName = "expense",
            tabBox(title = strong("Expense"),
                   id = "tabX", height = "20em", width = "1em",
                   tabPanel(strong("Monthly"),mx_food,mx_transport,mx_rent,mx_water,mx_electric,mx_wifi,mx_tel,mx_astro,mx_other,mx_output,mx_done),
                   tabPanel(strong("Non-monthly"),nmx_travel,nmx_carIns,nmx_healthIns,nmx_other,nmx_output,nmx_done),
                   tabPanel(strong("Pie Chart"),h4(strong("Monthly Expenses"),align = "center"),loadEChartsLibrary(),tags$div(id="mxChart",style = "width:100%;height:400px"), deliverChart(div_id="mxChart"))
            )
    ),
    
    #Liability body
    tabItem(tabName = "liability",
            tabBox(title = strong("Liability"),
                   id = "tabL",height = "110em",width = "1em",
                   tabPanel(strong("House Loan"),
                            lia_houseP,lia_houseDP,lia_houseLP,lia_houseIR,lia_houseAge,lia_houseAdd,
                            hr(style = "border-color:white;"),
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
                            hr(style = "border-color:white;"),
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
            ))
  )
)


#Ui
ui <- dashboardPage(
  dashboardHeader(title = "Financial Calculator"),
  sidebar,
  body
)

#Server
if (interactive()){
  server <- function(input, output,session){
    #################################################Market index logic

    ##################################################new start
    Result <- reactiveValues()
    v <- reactiveValues(data = NULL)
    observeEvent(input$action, {
      # print(interest)
      v$incomedata <- income_proj(input$init_income,input$growth_rate,input$growth_duration,input$range)
      print(v$sum)
      res$df <- final_df(as.numeric(input$age),as.numeric(input$range),as.numeric(input$age_m),as.numeric(input$growth_rate),as.numeric(input$growth_duration),as.numeric(input$promo_rate),as.numeric(input$promo_time),as.numeric(input$init_income))
      Result$Income_DF <- res$df %>% select(Date = as.character.date., Age = age_ot,
                                            Month = month_ot, Income = proj_income)
    })
    output$mytable = DT::renderDataTable({
        res$df
    })
    
    output$plot_income <- renderPlotly({
      if (is.null(v)) return()
      df <- res$df
      a <- list(
        title = "Income projection in RM"
      )
      b<-list(
        title="Time in Age-Months"
      )
      plot_ly(df, x = df$date, y = df$proj_income,type='scatter',mode='lines')%>%
        layout(
          title = paste('Income Projection for',input$range, "years in RM"),
          xaxis = list(
            # type = 'category',
            title = 'Months'
          ),
          yaxis = list(
            title = 'Income Projection in RM'
          )
        )
    })

    
    ##########################################################################start
      res<-reactiveValues(df=data.frame())
      
      sav<-reactiveValues(dfs=data.frame())
      #######
      
      
      v2 <- reactiveValues(data = NULL)
      observeEvent(input$action_2, {
        if (input$var == 'Monthly'){
          interest<-input$AIR/12
          range_2 <- input$range_2*12
        }
        else{
          interest<-input$AIR
          range_2<-input$range_2
        }
        # print(interest)
        sav$dfs <- saving_proj(input$numInput,interest,range_2)
        v2$data <- saving_proj(input$numInput,input$AIR,input$range_2)
      
          output$mytable_2 = DT::renderDataTable({
            # final_df(as.numeric(input$age),as.numeric(input$range),as.numeric(input$age_m),as.numeric(input$growth_rate),as.numeric(input$growth_duration),as.numeric(input$promo_rate),as.numeric(input$promo_time),as.numeric(input$init_income))
            # if (is.null(v2)) return(data.frame())
            # if (is.null(sav$dfs)) return(data.frame())
            if (input$var == 'Monthly'){
              title_1<-'Months'
              colnames(sav$dfs)<-c(title_1,'Projection')
              Result$FDSavings <- monthly_saving_df_convert(age = input$age, month = as.numeric(input$age_m), n = input$range, month_df = sav$dfs)
              return(Result$FDSavings)
            }
            else{
              title_1<-'Years'
              colnames(sav$dfs)<-c(title_1,'Projection')
              Result$FDSavings <- yearly_saving_df_convert(age = input$age, month = as.numeric(input$age_m), n = input$range, year_df = sav$dfs)
              return(Result$FDSavings)
            }
          })
      
      })
      
      output$plot_saving <- renderPlotly({
        if (is.null(v2)) return()
        df_save<-sav$dfs
        # print(df_save)
        # print(df_save)
        if (input$var == 'Monthly'){
          title_1<-'Months'
          plot_ly(df_save,x=df_save$Months,y=df_save$Projection,type='scatter',mode='lines')%>%
            layout(
              title = paste('Saving Projection for',input$range_2, "years in RM"),
              xaxis = list(
                # type = 'category',
                title = title_1
              ),
              yaxis = list(
                title = 'Saving Projection in RM'
              )
            )
        }
        else{
          title_1<-'Years'
          plot_ly(df_save,x=df_save$Years,y=df_save$Projection,type='scatter',mode='lines')%>%
            layout(
              title = paste('Saving Projection for',input$range_2, "years in RM"),
              xaxis = list(
                # type = 'category',
                title = title_1
              ),
              yaxis = list(
                title = 'Saving Projection in RM'
              )
            )
        }

        
        #df_save<-sav$dfs
        #plot_ly(df_save,x=df_save$year,y=df_save$data,type='scatter',mode='lines')
        #plot(x=c(0:(length(v$data)-1)),y=v$data,type='l',ylab='Savings in RM',xlab='Year',main=paste('Projection of Savings over',input$range,' years'))
        # plotly(x=c(0:(length(v$data)-1)),y=v$data,type='scatter',mode='lines',  
        # layout(title = 'Saving Projection'))
      })
      #############################################################
      v3 <- reactiveValues(data = NULL)
      ind<-reactiveValues(dfs=data.frame())
      
      observeEvent(input$action_3, {
        # print(interest)
        v3$sum<-sum(input$sp500,input$bursa,input$nasdaq,input$nikkei,input$hangseng)
        prop_list<-c(input$hangseng/100,input$nasdaq/100,input$nikkei/100,input$bursa/100,input$sp500/100)
        
        prop_name<-c('Hang Seng','Nasdaq','Nikkei','Bursa','SP500')
        prop<-data.frame(prop_name,prop_list)
        index_df<-index_data(input$range*12)
        ind_interest<-(1+sum(index_df$`Average Montly Return`*prop$prop_list)/100)
        
        output$status <- renderText({ 
          if (v3$sum!=100){
            'Investment proportion does not add up to 100!'
          }else{
            paste('Projected yearly returns:',sprintf("%.2f", 100*(ind_interest-1)), "%")
            # ind$dfs <- saving_proj(input$indInput,ind_interest,range)
          }
        })
        # print((ind_interest-1)*100)
        # print(input$indInput)
        # print(input$range*12)
        output$index_proj = DT::renderDataTable({
          index_market<-saving_proj(input$indInput,(ind_interest-1)*100,input$range*12)
          names(index_market)<-c('Months','Projection')
          index_market
        })
        output$ind_proj_graph <- renderPlotly({
          index_market<-saving_proj(input$indInput,(ind_interest-1)*100,input$range*12)
          
          names(index_market)<-c('Months','Projection')
          
          Result$Investment_DF <- convert_to_age_month_df(age = input$age, month = as.numeric(input$age_m), n = input$range, 
                                  vect = index_market$Projection, column_name = "Investment")
          
          plot_ly(index_market, x = index_market$Months, y =index_market$Projection,type='scatter',mode='lines')%>%
            layout(
              title = 'Market Index Investment Projection',
              xaxis = list(
                title = 'Months'
              ),
              yaxis = list(
                title = 'Projected Amount'
              )
              , height = 400, width = 800 
            )
        })
      })
      # output$proj_res <- renderPlotly({
      #   index_market<-saving_proj(input$indInput,(ind_interest-1)*100,input$range*12)
      #   plot_ly(index_market, x = row.names(IXIC_), y =IXIC_$Adjusted,type='scatter',mode='lines')%>%
      #     layout(
      #       title = 'NASDAQ 10 Years Historical Monthly Data',
      #       xaxis = list(
      #         title = 'Date'
      #       ),
      #       yaxis = list(
      #         title = 'Price'
      #       )
      #       , height = 400, width = 800 
      #     )
      # })
      output$index_tab = DT::renderDataTable({
        # final_df(as.numeric(input$age),as.numeric(input$range),as.numeric(input$age_m),as.numeric(input$growth_rate),as.numeric(input$growth_duration),as.numeric(input$promo_rate),as.numeric(input$promo_time),as.numeric(input$init_income))
        index_df<-index_data(input$range*12)
      })
      output$sp500_ind <- renderPlotly({
        GSPC_<-sp500_graph(input$range*12)
        plot_ly(GSPC_, x = row.names(GSPC_), y =GSPC_$Adjusted,type='scatter',mode='lines')%>%
          layout(
            title = 'SP500 10 Years Historical Monthly Data',
            xaxis = list(
              title = 'Date'
            ),
            yaxis = list(
              title = 'Price'
            )
            , height = 400, width = 800
          )
      })
      output$nasdaq_ind <- renderPlotly({
        IXIC_<-nasdaq_graph(input$range*12)
        plot_ly(IXIC_, x = row.names(IXIC_), y =IXIC_$Adjusted,type='scatter',mode='lines')%>%
          layout(
            title = 'NASDAQ 10 Years Historical Monthly Data',
            xaxis = list(
              title = 'Date'
            ),
            yaxis = list(
              title = 'Price'
            )
            , height = 400, width = 800 
          )
      })
      output$nikkei_ind <- renderPlotly({
        N225_<-nikkei_graph(input$range*12)
        plot_ly(N225_, x = row.names(N225_), y =N225_$Adjusted,type='scatter',mode='lines')%>%
          layout(
            title = 'NIKKEI 10 Years Historical Monthly Data',
            xaxis = list(
              title = 'Date'
            ),
            yaxis = list(
              title = 'Price'
            )
            , height = 400, width = 800 
          )
      })
      output$bursa_ind <- renderPlotly({
        KLSE_<-bursa_graph(input$range*12)
        plot_ly(KLSE_, x = row.names(KLSE_), y =KLSE_$Adjusted,type='scatter',mode='lines')%>%
          layout(
            title = 'BURSA 10 Years Historical Monthly Data',
            xaxis = list(
              title = 'Date'
            ),
            yaxis = list(
              title = 'Price'
            )
            , height = 400, width = 800 
          )
      })
      output$hangseng_ind <- renderPlotly({
        HSI_<-hangseng_graph(input$range*12)
        plot_ly(HSI_, x = row.names(HSI_), y =HSI_$Adjusted,type='scatter',mode='lines')%>%
          layout(
            title = 'HANGSENG 10 Years Historical Monthly Data',
            xaxis = list(
              title = 'Date'
            ),
            yaxis = list(
              title = 'Price'
            )
            , height = 400, width = 800 
          )
      })
      
      
      ################END######################################################################  
    
      # Show other monthly expenses
      mXT_data <- reactiveValues(mXT_list = list())
      calcMX_other <- reactiveValues(calcMX_list = list())
      
      observeEvent(input$addMX,{
          name <- input$otherMX
          amount <- input$otherMX_amount
          if(name == "" || is.na(amount)){
              MX_other <- "Please make sure the inputs are valid."
          }else{
              mXT_data$mXT_list[name] <- amount
              
              MX_other <- sapply(names(mXT_data$mXT_list), FUN = function(k){
                  paste0(k, ": RM ",  sprintf("%.2f", mXT_data$mXT_list[[k]]))
                  # paste0(k, " (RM) : ", mXT_data$mXT_list[[k]])
              })
              
              MX_other <- paste(MX_other, collapse = "; ")
          }
          output$mxOut <- renderUI(MX_other)
      })
      
      # Sum of monthly expenses
      observeEvent(input$doneMX,{
          mx_fix <- vector()
          mx_fix <- c(input$mx1,input$mx2,input$mx3,input$mx4,input$mx5,input$mx6,input$mx7,input$mx8)
          fixed_mexpense_total <- sum(mx_fix)
          
          if(!is.null(names(mXT_data$mXT_list))){
              other_mexp_amount_vector <- sapply(names(mXT_data$mXT_list), FUN = function(k){
                  return(mXT_data$mXT_list[[k]])
              })
          }else{
              other_mexp_amount_vector <- 0
          }
          
          # other_mexpense_total <- sum_other_mexpense(other_mexp_amount_vector)
          
          other_mexpense_total <- sum(other_mexp_amount_vector)
          
          total_mexp <- sum(fixed_mexpense_total,other_mexpense_total)
          
          Result$MExp_DF <- get_monthly_expense_df(age = input$age, month = as.numeric(input$age_m), n = input$range, total_monthly_expense = total_mexp)
      }
      )
      
      
      # Show other non-monthly expenses       
      nmXT_data <- reactiveValues(nmXT_list = list())
      NMX_other <- vector()
      
      observeEvent(input$addNMX,{
          name <- input$otherNMX
          amount <- input$otherNMX_amount
          period <- input$otherNMX_period
          if(name == "" || is.na(amount) || is.na(period)){
              NMX_other <- "Please make sure the inputs are valid."
          }else{
              i <- length(nmXT_data$nmXT_list)
              nmXT_data$nmXT_list[[i+1]] <- list(name = name, amount = amount, period = period)
              
              for (j in 1:length(nmXT_data$nmXT_list)){
                  entry <- nmXT_data$nmXT_list[[j]]
                  name <- entry$name
                  amount <- entry$amount
                  period <- entry$period
                  NMX_other <- c(NMX_other, paste0(name,": RM ", sprintf("%.2f", amount), " every ", period, " month(s)"))
              }
              NMX_other <- paste(NMX_other, collapse = "; ")
          }
          output$nmxOut<-renderUI(NMX_other)
      })
      
      #Sum of non-monthly expenses
      observeEvent(input$doneNMX,{
          nmx_fix <- vector()
          nmx_fix <- c(input$nmx1,input$nmx2,input$nmx3)
          nmxt_fix <- c(input$nmxt1,input$nmxt2,input$nmxt3)
          
          fixed_nmexpense_total <- sum(nmx_fix/nmxt_fix)
          
          other_nmexpense_total <- 0
          if(length(nmXT_data$nmXT_list)!=0){
              
              for (j in 1:length(nmXT_data$nmXT_list)){
                  other_nmexp_amount_vector <- nmXT_data$nmXT_list[[j]]$amount
                  other_nmexp_period_vector <- nmXT_data$nmXT_list[[j]]$period
                  other_nmexpense_total <- other_nmexpense_total + (other_nmexp_amount_vector/other_nmexp_period_vector)
              }
          }else{
              other_nmexp_amount_vector <- 0
              other_nmexp_period_vector <- 0
              other_nmexpense_total <- 0
          }
          
          NMExp_list <- nmXT_data$nmXT_list
          N <- length(nmXT_data$nmXT_list)
          for(j in 1:3){
              temp_list <- list()
              temp_list[["amount"]] <- nmx_fix[j]
              temp_list[["period"]] <- nmxt_fix[j]
              NMExp_list[[N+j]] <- temp_list
          }
          total_nmexp <- sum(c(fixed_nmexpense_total,other_nmexpense_total))
          Result$NMExp_DF <- get_non_monthly_expense_df(age = input$age, month = as.numeric(input$age_m), n = input$range, list_of_expenses = NMExp_list)
      }
      )
    
      # Pie chart of monthly expenses      
      dat <- reactive({
          if(!is.null(names(mXT_data$mXT_list))){
              other_mexp_amount_vector <- sapply(names(mXT_data$mXT_list), FUN = function(k){
                  return(mXT_data$mXT_list[[k]])
              })
          }else{
              other_mexp_amount_vector <- 0
          }
          
          other_mexpense_total <- sum(other_mexp_amount_vector)
          
          dat <-(c(rep("Food",req(input$mx1)),
                   rep("Transport",req(input$mx2)),
                   rep("Rent",req(input$mx3)),
                   rep("Water bill", req(input$mx4)),
                   rep("Electric bill",req(input$mx5)),
                   rep("Wi-Fi bill",req(input$mx6)),
                   rep("Telephone bill",req(input$mx7)),
                   rep("Astro bill",req(input$mx8)),
                   rep("Other",req(other_mexpense_total))
          ))
      })
      
    observeEvent(dat(),
                 {renderPieChart(div_id = "mxChart",
                                 data = dat())
                  })
    
    ##########################################
    # Liability
    ##########################################
    LoanData <- reactiveValues(df = data.frame(), car_df = data.frame())
    # Show input of information of HOUSE LOAN
    observeEvent(input$addLH,{
        hData <- sapply(houseDetails,function(x) input[[x]])
        LoanData$df <- rbind(LoanData$df, t(hData))
        
        output$houseInput <- renderDataTable({
            as.data.frame(LoanData$df)
        })
    })
    
    #House loan interest rate for different banks
    output$houseLoanRate <- renderDataTable(houseLoanRate,
                                            options = list(orderClasses = TRUE,
                                                           lengthMenu = c(10,30,50),
                                                           pageLength = 5))
    
    # Show input of information of car loan
     observeEvent(input$addLC,{
        cData <- sapply(carDetails,function(x) input[[x]])
        LoanData$car_df <- rbind(LoanData$car_df, t(cData))
        
        output$carInput <- renderDataTable({
            as.data.frame(LoanData$car_df)
        })
    })
    
    # Car loan interest rate for different banks
    output$carLoanRate <- renderDataTable(carLoanRate,
                                          options = list(orderClasses = TRUE,
                                                         lengthMenu = c(10,30,50),
                                                         pageLength = 5))
    
    
    ########################################
    # Dependency
    ########################################
    DepData <- reactiveValues(df = data.frame())
    # Show information of cost of raising kids
    
    observeEvent(input$addDK,{
        kData <- sapply(kidDetails,function(z) input[[z]])
        DepData$df <- rbind(DepData$df, t(kData))
        
        output$kidInput <- renderDataTable({
            as.data.frame(DepData$df)
        })
    })
    
    # Options of cost of raising kids (low,middle,high)
    incomeGroup <- reactive({
      if(input$"Cost of raising a kid (by income group)" == "Low Income Group"){
        dftab <- costLow_table
        dfgraph <- graphLow
      }
      else if(input$"Cost of raising a kid (by income group)" == "Middle Income Group"){
        dftab <- costMid_table
        dfgraph <- graphMid
      }
      else if(input$"Cost of raising a kid (by income group)" == "High Income Group"){
        dftab <- costHigh_table
        dfgraph <- graphHigh
      }
      return(list(dftab = dftab, dfgraph =  dfgraph))
    })
    
    # Show options of cost of raising kids (table)
    output$costTable <- renderDataTable(
      incomeGroup()$dftab, options= list(dom = "t",pageLength = 5))
    
    # Show options of cost of raising kids (table)
    output$costGraph <- renderPlotly(
      incomeGroup()$dfgraph
    )
    
    # Calculate cost of raising kids 
    # observeEvent(input$addDK,{
    #     
    #    table <- calc_costKid(age_yr=as.numeric(input$age),duration=as.numeric(input$range),
    #                              age_mth=as.numeric(input$age_m),age_kid=as.numeric(input$"Age (to have kid)") ,
    #                              incomeGrp= as.character(input$"Cost of raising a kid (by income group)"))
    # }
    # )
    
  }}




shinyApp(ui = ui, server = server)