library(rvest)
library(stringr)
library(dplyr)

## Interest rate of house loan
urlHL <- "https://ringgitplus.com/en/home-loan/"
webpageHL <- as.data.frame(read_html(urlHL)%>% html_table(fill=TRUE))

numextract <- function(string){
    str_extract(string,"\\-*\\d+\\.*\\d*")
}

rateHL <- numextract(webpageHL$Interest.Rate)

houseLoanRate <- data.frame(webpageHL$Bank.logo,rateHL)
names(houseLoanRate) <- c("Bank Loan Product","Interest Rate")
houseLoanRate <- arrange(houseLoanRate,.by_group = "Bank Loan Product")


##Interest rate of car loan
urlCL <- "https://ringgitplus.com/en/car-loan/"
webpageCL <- as.data.frame(read_html(urlCL)%>% html_table(fill=TRUE))

rateCL <- numextract(webpageCL$Interest.Rate)

carLoanRate <- data.frame(webpageCL$Bank.logo,rateCL)
names(carLoanRate) <- c("Bank Loan Product","Interest Rate")
carLoanRate <- arrange(carLoanRate,.by_group = "Bank Loan Product")
