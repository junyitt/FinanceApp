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

source('function.R')
source("loanrate.R")
source("raisekidcost.R")
source('index_data.R')
source('generate_result_plots.R')

server <- function(input, output,session){
    #################################################Market index logic
    
    ##################################################new start
    Result <- reactiveValues()
    
    
    
    v <- reactiveValues(data = NULL)
    observeEvent(input$action, {
        # Initialize Result
        Result$Income_DF <- generate_empty_age_df(age_yr = as.numeric(input$age), duration = as.numeric(input$range),
                                                  age_mth = as.numeric(input$age_m), add_column = "Income")
        Result$FDSavings <- generate_empty_age_df(age_yr = as.numeric(input$age), duration = as.numeric(input$range),
                                                  age_mth = as.numeric(input$age_m), add_column = "FDSavings")
        Result$Investment_DF <- generate_empty_age_df(age_yr = as.numeric(input$age), duration = as.numeric(input$range),
                                                      age_mth = as.numeric(input$age_m), add_column = "Investment")
        Result$MExp_DF <- generate_empty_age_df(age_yr = as.numeric(input$age), duration = as.numeric(input$range),
                                                age_mth = as.numeric(input$age_m), add_column = "MonthlyExpense")
        Result$NMExp_DF <- generate_empty_age_df(age_yr = as.numeric(input$age), duration = as.numeric(input$range),
                                                 age_mth = as.numeric(input$age_m), add_column = "NonMonthlyExpense")
        Result$House_DF <- generate_empty_age_df(age_yr = as.numeric(input$age), duration = as.numeric(input$range),
                                                 age_mth = as.numeric(input$age_m), add_column = "HouseLoanAmount")
        Result$Car_DF <- generate_empty_age_df(age_yr = as.numeric(input$age), duration = as.numeric(input$range),
                                               age_mth = as.numeric(input$age_m), add_column = "CarLoanAmount")
        Result$Kid_DF <- generate_empty_age_df(age_yr = as.numeric(input$age), duration = as.numeric(input$range),
                                               age_mth = as.numeric(input$age_m), add_column = "kidCost")
        
        
        v$incomedata <- income_proj(input$init_income,input$growth_rate,input$growth_duration,input$range)
        res$df <- final_df(as.numeric(input$age),as.numeric(input$range),as.numeric(input$age_m),as.numeric(input$growth_rate),as.numeric(input$growth_duration),as.numeric(input$promo_rate),as.numeric(input$promo_time),as.numeric(input$init_income))
        Result$Income_DF <- res$df %>% select(Age = age_ot,
                                              Month = month_ot, Income = proj_income)
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
        sav$dfs <- saving_proj(input$numInput,interest,range_2)
        v2$data <- saving_proj(input$numInput,input$AIR,input$range_2)
        
        output$mytable_2 = DT::renderDataTable({
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
        
    })
    #############################################################
    v3 <- reactiveValues(data = NULL)
    ind<-reactiveValues(dfs=data.frame())
    
    observeEvent(input$action_3, {
        v3$sum<-sum(input$sp500,input$bursa,input$nasdaq,input$nikkei,input$hangseng)
        prop_list<-c(input$hangseng/100,input$nasdaq/100,input$nikkei/100,input$bursa/100,input$sp500/100)
        
        prop_name<-c('Hang Seng','Nasdaq','Nikkei','Bursa','SP500')
        prop<-data.frame(prop_name,prop_list)
        index_df<-index_data(input$range*12)
        ind_interest<-(1+sum(index_df$`Average Montly Return`*prop$prop_list)/100)
        
        index_market<-saving_proj(input$indInput,(ind_interest-1)*100,input$range*12)
        
        names(index_market)<-c('Months','Projection')
        
        Result$Investment_DF <- convert_to_age_month_df(age = input$age, month = as.numeric(input$age_m), n = input$range, 
                                                        vect = index_market$Projection, column_name = "Investment")
        
        output$status <- renderText({ 
            if (v3$sum!=100){
                'Investment proportion does not add up to 100!'
            }else{
                paste('Projected yearly returns:',sprintf("%.2f", 100*(ind_interest-1)), "%")
            }
        })
        
        output$index_proj = DT::renderDataTable({
            index_market<-saving_proj(input$indInput,(ind_interest-1)*100,input$range*12)
            names(index_market)<-c('Months','Projection')
            index_market
        })
        
        output$ind_proj_graph <- renderPlotly({
            index_market<-saving_proj(input$indInput,(ind_interest-1)*100,input$range*12)
            
            names(index_market)<-c('Months','Projection')
            
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
    
    output$index_tab = DT::renderDataTable({
        index_df<-index_data(input$range*12)
        month_ret <- index_df[,2]
        
        index_df[,2] <- paste(round(as.numeric(month_ret),2), "%")
        index_df
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
        Result$House_DF <- get_overall_loan_amount(LoanData$df, input$range, input$age, input$age_m, "house")
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
        
        Result$Car_DF <- get_overall_loan_amount(LoanData$car_df, input$range, input$age, input$age_m, "car")
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
        
        Result$Kid_DF <- get_overall_cost_kid(DepData$df, input$age, input$range, input$age_m)
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
    
    
    #################################
    # Result
    #################################
    plot_data <- reactiveValues(plist = list())
    observeEvent(input$runresult,{
        rlist <- reactiveValuesToList(Result)
        plot_data$plist <- generate_final_plots(rlist = rlist)
        
        output$breakdownGraph <- renderPlotly(
            plot_data$plist[[1]]
        )
        output$ieGraph <- renderPlotly(
            plot_data$plist[[2]]
        )
        output$networthGraph <- renderPlotly(
            plot_data$plist[[3]]
        )
    })
    
}