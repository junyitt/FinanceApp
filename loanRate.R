library(rvest)
library(stringr)
library(dplyr)

#Interest rate of house loan
urlHL <- "https://ringgitplus.com/en/home-loan/"
webpageHL <- as.data.frame(read_html(urlHL)%>% html_table(fill=TRUE))

numextract <- function(string){
    str_extract(string,"\\-*\\d+\\.*\\d*")
}

rateHL <- numextract(webpageHL$Interest.Rate)

houseLoanRate <- data.frame(webpageHL$Bank.logo,rateHL)
names(houseLoanRate) <- c("Bank Loan Product","Interest Rate")
houseLoanRate <- arrange(houseLoanRate,.by_group = "Bank Loan Product")


#Interest rate of car loan
urlCL <- "https://ringgitplus.com/en/car-loan/"
webpageCL <- as.data.frame(read_html(urlCL)%>% html_table(fill=TRUE))

rateCL <- numextract(webpageCL$Interest.Rate)

carLoanRate <- data.frame(webpageCL$Bank.logo,rateCL)
names(carLoanRate) <- c("Bank Loan Product","Interest Rate")
carLoanRate <- arrange(carLoanRate,.by_group = "Bank Loan Product")


##Variable explain (for own use)
#principal - (can be found @ input$"Property price (RM)" for house OR input$"Car price (RM) " for car)
#downPayment - (can be found @ input$"Down payment (RM)" for house OR input$"Down payment (RM) " for car)
#loanDuration - (can be found @ input$"Loan duration (years)" for house OR input$"Loan duration (years) " for car)
#interestRate - (can be found @ input$"Interest rate (%)" for house OR input$"Interest rate (%) " for car)
#age_yr - age(year) (can be found @ input$age)
#duration - duration of projection(yrs) (can be found @ input$range)
#age_mth - age(year) (can be found @ input$age_m)
#age_loan - age to buy house/car (can be found @ input$"Loan duration (years)" for house OR input$"Loan duration (years) " for car)

#Function of calculating house and car loan
calc_houseloanAmount <- function(principal,downPayment,loanDuration,interestRate,age_yr,duration,age_mth,age_loan){
    loanPerMonth = (principal-downPayment)/((1-(1/((1+((interestRate/100)/12))^(loanDuration*12))))/((interestRate/100)/12))
    
    age_Selected <- c(age_yr:(age_yr+duration))
    age_yrSelected <- c(rep(age_Selected[1],times=(12-age_mth)),
                        rep(age_Selected[2:length(age_Selected)],rep(12,(length(age_Selected)-1)))
    )
    age_yrSelected <- age_yrSelected[1:(duration*12)]
    month_Selected <- c(age_mth:11,rep(c(0:11),(length(age_Selected)-1)))
    month_Selected <- month_Selected[1:(duration*12)]
    age_loanIndex <- match(age_loan,age_yrSelected)
    
    loan_amt <- c(rep(0,times = (length(age_yrSelected))))
    loan_amt[age_loanIndex] <- c(downPayment+loanPerMonth)
    loan_amt[(age_loanIndex+1):(age_loanIndex+((loanDuration*12)-1))] <- c(rep(loanPerMonth,times = ((loanDuration*12)-1)))
    
    loanAmt_table <- cbind.data.frame(Age = age_yrSelected,Month = month_Selected) %>%
        mutate(HouseLoanAmount = loan_amt)
    return(loanAmt_table)
}

calc_carloanAmount <- function(principal,downPayment,loanDuration,interestRate,age_yr,duration,age_mth,age_loan){
    A = principal - downPayment
    B = loanDuration
    C = interestRate
    X = C/100*A*B
    loanPerMonth = (A + X) / (B *12)
    
    age_Selected <- c(age_yr:(age_yr+duration))
    age_yrSelected <- c(rep(age_Selected[1],times=(12-age_mth)),
                        rep(age_Selected[2:length(age_Selected)],rep(12,(length(age_Selected)-1)))
    )
    age_yrSelected <- age_yrSelected[1:(duration*12)]
    month_Selected <- c(age_mth:11,rep(c(0:11),(length(age_Selected)-1)))
    month_Selected <- month_Selected[1:(duration*12)]
    age_loanIndex <- match(age_loan,age_yrSelected)
    
    loan_amt <- c(rep(0,times = (length(age_yrSelected))))
    loan_amt[age_loanIndex] <- c(downPayment+loanPerMonth)
    loan_amt[(age_loanIndex+1):(age_loanIndex+((loanDuration*12)-1))] <- c(rep(loanPerMonth,times = ((loanDuration*12)-1)))
    
    loanAmt_table <- cbind.data.frame(Age = age_yrSelected,Month = month_Selected) %>%
        mutate(CarLoanAmount = loan_amt)
    return(loanAmt_table)
    
}

get_overall_loan_amount <- function(loan_df, duration, age, age_mth){
    df_list <- apply(loan_df, 1, FUN = function(j){
        calc_carloanAmount(principal=j[1],
                           downPayment=j[2],
                           loanDuration=j[3],
                           interestRate=j[4],
                           age_yr= age,
                           duration=duration, 
                           age_mth=as.numeric(age_mth),
                           age_loan=j[5])
        
    })
    amount_df <- sapply(df_list, FUN = function(j){
        j$CarLoanAmount
    })
    df2 <- df_list[[1]][,c("Age", "Month")]
    df2$CarLoanAmount <- rowSums(amount_df)
    return(df2)    
}


######FOR TESTING PURPOSE############
# cdf <- calc_loanAmount(principal=60000,
#                 downPayment=6000,
#                 loanDuration=30,
#                 interestRate=4.5,
#                 age_yr=20,
#                 duration=50,
#                 age_mth=7,
#                 age_loan=22)
####################################



# cdf2 <- calc_carloanAmount(principal=60000,
#                            downPayment=6000,
#                            loanDuration=30,
#                            interestRate=4.5,
#                            age_yr=20,
#                            duration=50,
#                            age_mth=7,
#                            age_loan=22)
