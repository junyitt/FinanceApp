library(plotly)

generate_final_plots <- function(rlist){
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
    data[, "Age2"] <- data[, "Age"] + data[, "Month"]/12
    data[, "TotalIncome"] <- data[, "FDSavings"] + data[, "Investment"] + data[, "Income"] 
    data[, "TotalExpense"] <- data[, "MonthlyExpense"] + data[, "NonMonthlyExpense"] + 
        data[, "CarLoanAmount"] +
        data[, "HouseLoanAmount"] +
        data[, "kidCost"] 
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
            title = "Total Income and Expense", 
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
    
    return(list(p,p2,p3))
}

## Examples
# setwd("C:/Users/yinyen/Documents/finApp")
# load("templist.rda")
# plotly_list <- generate_final_plots(rlist)
