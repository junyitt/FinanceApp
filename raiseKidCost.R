library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(expss)

#Table of cost of raising a kid in Malaysia
raiseKidCost <- as.data.frame(read_excel("raiseKidCost.xlsx"))

#Cost of raising a kid for different income groups
costPerYear <- gsub(",","",
                    raiseKidCost$`Cost of Living per Year (RM)`) 
costPerYear <- regmatches(costPerYear, 
                          gregexpr("([0-9]+)", 
                          costPerYear))

costPerPhase <- gsub(",","",
                     raiseKidCost$`Total Cost of Living per Phase (RM)`)
costPerPhase <- regmatches(costPerPhase, 
                           gregexpr("([0-9]+)", 
                           costPerPhase))

#For low income group
costPerYear_low <- lapply(costPerYear,
                           FUN = function(x){min(as.numeric(x))})
costPerPhase_low <- lapply(costPerPhase,
                           FUN = function(x){min(as.numeric(x))})
costLow_table <- raiseKidCost %>% 
                 select(`Age / Education Level`) %>% 
                 mutate("Cost of Living per Year (RM)" = costPerYear_low,
                        "Total Cost of Living per Phase (RM)" = costPerPhase_low)%>%
                 set_caption ("Table of Cost of Raising a Kid (Low Income Group)")
                 

age <- c(0:21)
cLow <- c(rep(costPerYear_low[[1]],times = 7),
          rep(costPerYear_low[[2]],times = 6),
          rep(costPerYear_low[[3]],times = 5),
          rep(costPerYear_low[[4]],times = 4))
cLowTab <- data.frame(Age = age, livingCost = cLow)
graphLow <- plot_ly(cLowTab,x = cLowTab$Age,y = cLowTab$livingCost,mode = "lines",type = "scatter",line = list(shape = "hv")) %>%
            layout(title = "Living Cost per Year (Low Income Group)",
                   xaxis = list(title = "Age",showline = T,linecolor = "rgb(235,235,235)",mirror = T),
                   yaxis = list(title = "Living Cost",showline = T,linecolor = "rgb(235,235,235)",range = c(0,40000),mirror = T)
                   )
    
#For middle income group
costPerYear_mid <- lapply(costPerYear,
                          FUN = function(x){mean(as.numeric(x))})
costPerPhase_mid <- lapply(costPerPhase,
                           FUN = function(x){mean(as.numeric(x))})
costMid_table <- raiseKidCost %>% 
                 select(`Age / Education Level`) %>% 
                 mutate("Cost of Living per Year (RM)" = costPerYear_mid,
                        "Total Cost of Living per Phase (RM)" = costPerPhase_mid)
cMid <- c(rep(costPerYear_mid[[1]],times = 7),
          rep(costPerYear_mid[[2]],times = 6),
          rep(costPerYear_mid[[3]],times = 5),
          rep(costPerYear_mid[[4]],times = 4))
cMidTab <- data.frame(Age = age, livingCost = cMid)
graphMid <- plot_ly(cMidTab,x = cMidTab$Age,y = cMidTab$livingCost,mode = "lines",type = "scatter",line = list(shape = "hv")) %>%
                    layout(title = "Living Cost per Year (Middle Income Group)",
                           xaxis = list(title = "Age",showline = T,linecolor = "rgb(235,235,235)",mirror = T),
                           yaxis = list(title = "Living Cost",showline = T,linecolor = "rgb(235,235,235)",range = c(0,90000),mirror = T)
                           )
#For high income group
costPerYear_high <- lapply(costPerYear,
                          FUN = function(x){max(as.numeric(x))})
costPerPhase_high <- lapply(costPerPhase,
                            FUN = function(x){max(as.numeric(x))})
costHigh_table <- raiseKidCost %>% 
                  select(`Age / Education Level`) %>% 
                  mutate ("Cost of Living per Year (RM)" = costPerYear_high,
                          "Total Cost of Living per Phase (RM)" = costPerPhase_high)
cHigh <- c(rep(costPerYear_high[[1]],times = 7),
          rep(costPerYear_high[[2]],times = 6),
          rep(costPerYear_high[[3]],times = 5),
          rep(costPerYear_high[[4]],times = 4))
cHighTab <- data.frame(Age = age, livingCost = cHigh)
graphHigh <- plot_ly(cHighTab,x = cHighTab$Age,y = cHighTab$livingCost,mode = "lines",type = "scatter",line = list(shape = "hv")) %>%
                     layout(title = "Living Cost per Year (High Income Group)",
                     xaxis = list(title = "Age",titlefont = list(size = 13), showline = T,linecolor = "rgb(235,235,235)",mirror = T),
                     yaxis = list(title = "Living Cost",titlefont = list(size = 13),showline = T,linecolor = "rgb(235,235,235)",range = c(0,160000),mirror = T)
                     )


##Variable explain (for own use)
#age_yr - age(year) (can be found @ input$age)
#duration - duration of projection(yrs) (can be found @ input$range)
#age_mth - age(year) (can be found @ input$age_m)
#age_kid - age to have kid (can be found @ input$"Age (to have kid)")
#incomeGrp - low,middle,high income group (can be found @ input$"Cost of raising a kid (by income group)")

#Function of cost of raising kids 
calc_costKid <- function(age_yr,duration,age_mth,age_kid,incomeGrp){
    age_Selected <- c(age_yr:(age_yr+duration))
    age_yrSelected <- c(rep(age_Selected[1],times=(12-age_mth)),
                        rep(age_Selected[2:length(age_Selected)],rep(12,(length(age_Selected)-1)))
                        )
    age_yrSelected <- age_yrSelected[1:(duration*12)]
    month_Selected <- c(age_mth:11,rep(c(0:11),(length(age_Selected)-1)))
    month_Selected <- month_Selected[1:(duration*12)]
    age_kidIndex <- match(age_kid,age_yrSelected)
    cost_kid <- c(rep(0,times = (length(age_yrSelected))))

    if (incomeGrp == "Low Income Group"){
        cost_kid[age_kidIndex:(age_kidIndex+(72+72+60+48-1))] <- c((rep((37300/12),times = (6*12))),
                                                                    (rep((8400/12),times =(6*12))),
                                                                    (rep((13200/12),times =(5*12))),
                                                                    (rep((13200/12),times =(4*12)))
                                                                    )
    }
    else if(incomeGrp == "Middle Income Group"){
            cost_kid[age_kidIndex:(age_kidIndex+(72+72+60+48-1))] <- c((rep((42650/12),times = (6*12))),
                                                                        (rep((21700/12),times =(6*12))),
                                                                        (rep((29600/12),times =(5*12))),
                                                                        (rep((86600/12),times =(4*12)))
                                                                        )
    }
    else if(incomeGrp == "High Income Group"){
            cost_kid[age_kidIndex:(age_kidIndex+(72+72+60+48-1))] <- c((rep((48000/12),times = (6*12))),
                                                                         (rep((35000/12),times =(6*12))),
                                                                         (rep((46000/12),times =(5*12))),
                                                                         (rep((160000/12),times =(4*12)))
                                                                         )
    }
    
    LEN <- length(age_yrSelected)
    costKid_table <- cbind.data.frame(Age = age_yrSelected, Month = month_Selected) %>%
                     mutate(kidCost = cost_kid[1:LEN])
    return(costKid_table)
}

get_overall_cost_kid <- function(kid_df, age_yr, duration, age_mth){
    df_list <- apply(kid_df, 1, FUN = function(j){
        calc_costKid(
                    age_yr=as.numeric(age_yr),
                    duration=as.numeric(duration),
                    age_mth=as.numeric(age_mth),
                    age_kid=as.numeric(j[1]),
                    incomeGrp= as.character(j[2])
                    )
    })
    if(length(df_list) == 1){
        return(df_list[[1]])
    }
    amount_df <- sapply(df_list, FUN = function(j){
        j$kidCost
    })
    df2 <- df_list[[1]][,c("Age", "Month")]
    df2$kidCost <- rowSums(amount_df)
    return(df2)
}

######FOR TESTING PURPOSE############
# calc_costKid(age_yr = 20,
#              duration = 40,
#              age_mth = 5,
#              age_kid = 21,
#              incomeGrp = "High Income Group")
####################################
# 
# kid_df <- calc_costKid(age_yr = 20,
#                    duration = 40,
#                    age_mth = 5,
#                    age_kid = 21,
#                    incomeGrp = "High Income Group")
# kid_df2 <- calc_costKid(age_yr = 20,
#                    duration = 40,
#                    age_mth = 5,
#                    age_kid = 21,
#                    incomeGrp = "Low Income Group")
# 
# new_kid_cost_var <- paste0("kidCost", ncol(kid_df) - 2)
# kid_df[, new_kid_cost_var] <- kid_df2$kidCost
