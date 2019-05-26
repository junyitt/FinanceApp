library(plotly)
library(dplyr)

generate_final_data <- function(rlist, inflation_rate = 0, exp_growth_rate = 0){
    rlist$Investment_DF <- rlist$Investment_DF %>% mutate(Investment = c(0,diff(Investment)))
    
    rlist2 <- lapply(1:length(rlist), FUN = function(j){
        d <- rlist[[j]]
        if(j == 1){
            return(d)
        }else{
            return(d[,3, drop = F])
        }
    })
    
    data <- do.call(cbind, rlist2)
    N <- nrow(data)
    
    ir <- inflation_rate/12 + 1
    er <- exp_growth_rate + 1
    
    ir_factor <- ir^(1:N)
    er_factor <- er^(1:N)
    
    data[, "Age2"] <- data[, "Age"] + data[, "Month"]/12
    data[, "TotalIncome"] <- data[, "FDSavings"] + data[, "Investment"] + data[, "Income"] 
    data[, "TotalExpense"] <- data[, "MonthlyExpense"]*ir_factor*er_factor + data[, "NonMonthlyExpense"]*ir_factor*er_factor + 
        data[, "CarLoanAmount"] +
        data[, "HouseLoanAmount"] +
        data[, "kidCost"]*ir_factor*er_factor
    
    data[, "NetIncome"] <- data[, "TotalIncome"] - data[, "TotalExpense"]
    data[, "NetIncomePos"] <- data[, "NetIncome"]
    data[, "NetIncomeNeg"] <- data[, "NetIncome"]
    data[data[, "NetIncome"]  < 0, "NetIncomePos"] <- 0
    data[data[, "NetIncome"]  >= 0, "NetIncomeNeg"] <- 0
    data[, "CumulativeTotalIncome"] <- cumsum(data[, "TotalIncome"])
    data[, "CumulativeTotalExpense"] <- cumsum(data[, "TotalExpense"])
    data[, "NetWorth"] <- cumsum(data[, "NetIncome"])
    data[, "NetWorthPos"] <- data[, "NetWorth"]
    data[, "NetWorthNeg"] <- data[, "NetWorth"]
    data[data[, "NetWorth"] < 0, "NetWorthPos"] <- 0
    data[data[, "NetWorth"] >= 0, "NetWorthNeg"] <- 0
    return(data)
}

generate_final_plots <- function(data){
    p <- plot_ly(data, x = ~Age2, y = ~Income, type = 'scatter', mode = 'line', 
                 legendgroup = 'group1', name = 'Income - Income') %>%
        add_trace(y = ~FDSavings, legendgroup = 'group1', name = 'Income - FD Savings') %>%
        add_trace(y = ~Investment, legendgroup = 'group1', name = 'Income - Investment') %>%
        add_trace(y = ~MonthlyExpense, legendgroup = 'group2', name = 'Expenses - Monthly Expense') %>%
        add_trace(y = ~NonMonthlyExpense, legendgroup = 'group2', name = 'Expenses - Non-Monthly Expense') %>%
        add_trace(y = ~CarLoanAmount, legendgroup = 'group2', name = 'Expenses - Car') %>%
        add_trace(y = ~HouseLoanAmount, legendgroup = 'group2', name = 'Expenses - House') %>%
        add_trace(y = ~kidCost, legendgroup = 'group2', name = 'Expenses - Dependencies') %>% 
        layout(
            title = "Breakdown of Income and Expense", 
            yaxis = list(title = "Amount (RM)"),
            xaxis = list(title="Age (Year)")
        )
    
    p2 <- plot_ly(data) %>% 
        add_lines(x = ~Age2, y = ~TotalIncome, legendgroup = 'group1', name = 'Total Income') %>%
        add_lines(x = ~Age2, y = ~TotalExpense, legendgroup = 'group1', name = 'Total Expense') %>%
        add_trace(x = ~Age2, y = ~NetIncomePos,
                  legendgroup = 'group2', name = 'Net Income - Positive',
                  marker = list(color = 'rgba(50, 171, 96, 0)',
                                line = list(color = 'rgba(50, 171, 96, 0.7)')
                  ),
                  type = 'scatter', mode = 'lines', fill = 'tozeroy') %>%
        add_trace(x = ~Age2, y = ~NetIncomeNeg,
                  marker = list(color = 'rgba(219, 64, 82, 0)',
                                line = list(color = 'rgba(219, 64, 82, 1.0)')
                  ),
                  legendgroup = 'group2', name = 'Net Income - Negative',
                  type = 'scatter', mode = 'lines', fill = 'tozeroy') %>%
        layout(
            title = "Total Income and Expense per Month", 
            yaxis = list(title = "Amount (RM)"),
            xaxis = list(title="Age (Year)")
        )
    
    ay <- list(
        tickfont = list(color = "green"),
        overlaying = "y",
        side = "right",
        title = "Net Worth (RM)"
    )
    
    p3 <- plot_ly(data) %>% 
        add_lines(x = ~Age2, y = ~CumulativeTotalIncome, line = list(dash = "dash"), name = "Income") %>%
        add_lines(x = ~Age2, y = ~CumulativeTotalExpense, line = list(dash = "dash"), name = "Expense") %>%
        add_trace(x = ~Age2, y = ~NetWorthPos,
                  legendgroup = 'group2', name = 'Net Worth - Positive',
                  marker = list(color = 'rgba(50, 171, 96, 0)',
                                line = list(color = 'rgba(50, 171, 96, 0.7)')
                  ),
                  yaxis = "y2",
                  type = 'scatter', mode = 'lines', fill = 'tozeroy') %>%
        add_trace(x = ~Age2, y = ~NetWorthNeg,
                  marker = list(color = 'rgba(219, 64, 82, 0)',
                                line = list(color = 'rgba(219, 64, 82, 1.0)')
                  ),
                  yaxis = "y2",
                  legendgroup = 'group2', name = 'Net Worth - Negative',
                  type = 'scatter', mode = 'lines', fill = 'tozeroy') %>%
        layout(
            title = "Net Worth", 
            yaxis2 = ay,
            yaxis = list(title = "Amount (RM)"),
            xaxis = list(title = "Age (Year)")
        )
    
    
    first_neg <- min(which(data[,"NetWorth"] < -10000))
    last_neg <- max(which(data[,"NetWorth"] < -10000))
    first_debt <- "You will not have any financial worries!"
    last_debt <- ""
    if(first_neg > 2 & last_neg > 3){
        first_age_month <- data[first_neg, c("Age", "Month")]
        last_age_month <- data[last_neg, c("Age", "Month")]
        first_debt <- paste0("You may first be in debt at age: ", first_age_month[1], " years ", first_age_month[2], " months.")
        last_debt <- paste0("You may last be in debt at age: ", last_age_month[1], " years ", last_age_month[2], " months.")
    }
    
    final_net_worth <- data[nrow(data),"NetWorth"]
    final_age <- data[nrow(data),"Age"]
    avg_available_monthly_spending <- final_net_worth/20/12
    nw <- paste0("You will have a net worth of RM", round(final_net_worth,2), " at the age of ", final_age + 1, " years.")
    mspend <- paste0("You could afford to spend RM", round(avg_available_monthly_spending,2), " per month for 20 years of retirement life.")
   
    return(list(p,p2,p3, first_debt, last_debt, nw, mspend))
}


income_exp_breakdown_plot <- function(data, selected_age){
    data2 <- data %>% 
        filter(Age == selected_age) %>% 
        arrange(Month) %>% 
        mutate(Month2 =  c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))
    data2$Month2 <- factor(data2$Month2, levels = data2[["Month2"]])
    
    title1 <- paste0("Income and Expense Breakdown at Age of ", selected_age, " Years")
    
    p <- plot_ly(data2, x = ~Month2, y = ~Income, type = 'bar', name = 'Income', marker = list(color = 'rgb(82, 239, 231)')) %>%
        add_trace(y = ~FDSavings, name = 'Savings Interest', marker = list(color = 'rgb(98, 239, 145)')) %>%
        add_trace(y = ~Investment, name = 'Investment Yield', marker = list(color = 'rgb(134, 244, 171)')) %>%
        add_trace(y = ~MonthlyExpense, name = 'Monthly Expense', marker = list(color = 'rgb(244, 207, 134)')) %>%
        add_trace(y = ~NonMonthlyExpense, name = 'Non-Monthly Expense', marker = list(color = 'rgb(244, 188, 78)')) %>%
        add_trace(y = ~kidCost, name = 'Dependencies Expense', marker = list(color = 'rgb(244, 168, 19)')) %>%
        add_trace(y = ~HouseLoanAmount, name = 'House Loan', marker = list(color = 'rgb(244, 56, 19)')) %>%
        add_trace(y = ~CarLoanAmount, name = 'Car Loan', marker = list(color = 'rgb(239, 109, 83)')) %>%
        add_trace(y = ~NetIncome, name = 'Net Income', marker = list(color = 'rgb(224, 131, 252)')) %>%
        layout(xaxis = list(title = "Months", tickangle = -45),
               yaxis = list(title = "Amount (RM)"),
               title = title1, 
               margin = list(b = 100),
               barmode = 'group')
    
    p
    
}


## Examples
# setwd("C:/Users/yinyen/Documents/finApp")
load("rlist.rda")
data <- generate_final_data(rlist)
income_exp_breakdown_plot(data, 27)
# plotly_list <- generate_final_plots(rlist)
