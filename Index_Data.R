library(quantmod)
library(dplyr)
library(plotly)
library(lubridate)
sp500_graph<-function(x){
  end<-as.Date(Sys.Date())
  start<-end-months(x)
  sp500<-getSymbols("^GSPC", src = "yahoo", from = start, to = end,periodicity = 'monthly')
  GSPC_<-data.frame(GSPC)
  GSPC_<-GSPC_%>%na.omit()%>%select(GSPC.Adjusted)
  names(GSPC_)<-'Adjusted'
  return(GSPC_)
}
nasdaq_graph<-function(x){
  end<-as.Date(Sys.Date())
  start<-end-months(x)
  nasdaq<-getSymbols("^IXIC", src = "yahoo", from = start, to = end,periodicity = 'monthly')
  IXIC_<-data.frame(IXIC)
  IXIC_<-IXIC_%>%na.omit()%>%select(IXIC.Adjusted)
  names(IXIC_)<-'Adjusted'
  return(IXIC_)
}

nikkei_graph<-function(x){
  end<-as.Date(Sys.Date())
  start<-end-months(x)
  nikkei<-getSymbols("^N225", src = "yahoo", from = start, to = end,periodicity = 'monthly')
  N225_<-data.frame(N225)
  N225_<-N225_%>%na.omit()%>%select(N225.Adjusted)
  names(N225_)<-'Adjusted'
  return(N225_)
}

bursa_graph<-function(x){
  end<-as.Date(Sys.Date())
  start<-end-months(x)
  bursa<-getSymbols("^KLSE", src = "yahoo", from = start, to = end,periodicity = 'monthly')
  KLSE_<-data.frame(KLSE)
  KLSE_<-KLSE_%>%na.omit()%>%select(KLSE.Adjusted)
  names(KLSE_)<-'Adjusted'
  return(KLSE_)
}

hangseng_graph<-function(x){
  end<-as.Date(Sys.Date())
  start<-end-months(x)
  hangseng<-getSymbols("^HSI", src = "yahoo", from = start, to = end,periodicity = 'monthly')
  HSI_<-data.frame(HSI)
  HSI_<-HSI_%>%na.omit()%>%select(HSI.Adjusted)
  names(HSI_)<-'Adjusted'
  return(HSI_)
}

index_data<-function(x){
  end<-as.Date(Sys.Date())
  start<-end-months(x)
  # start <- as.Date("2007-12-31") 
  # end <- as.Date("2018-12-31")
  sp500<-getSymbols("^GSPC", src = "yahoo", from = start, to = end,periodicity = 'monthly')
  GSPC_<-data.frame(GSPC)
  GSPC_<-GSPC_%>%na.omit()%>%select(GSPC.Adjusted)
  names(GSPC_)<-'Adjusted'
  nasdaq<-getSymbols("^IXIC", src = "yahoo", from = start, to = end,periodicity = 'monthly')
  IXIC_<-data.frame(IXIC)
  IXIC_<-IXIC_%>%na.omit()%>%select(IXIC.Adjusted)
  names(IXIC_)<-'Adjusted'
  nikkei<-getSymbols("^N225", src = "yahoo", from = start, to = end,periodicity = 'monthly')
  N225_<-data.frame(N225)
  N225_<-N225_%>%na.omit()%>%select(N225.Adjusted)
  names(N225_)<-'Adjusted'
  bursa<-getSymbols("^KLSE", src = "yahoo", from = start, to = end,periodicity = 'monthly')
  KLSE_<-data.frame(KLSE)
  KLSE_<-KLSE_%>%na.omit()%>%select(KLSE.Adjusted)
  names(KLSE_)<-'Adjusted'
  hangseng<-getSymbols("^HSI", src = "yahoo", from = start, to = end,periodicity = 'monthly')
  HSI_<-data.frame(HSI)
  HSI_<-HSI_%>%na.omit()%>%select(HSI.Adjusted)
  names(HSI_)<-'Adjusted'
  pct <- function(x) {x / lag(x) - 1}
  mean_change <-function(x){
    pct(x)%>%na.fill(0)%>%mean()*100
  }
  
  mean_HSI<-mean_change(HSI_$Adjusted)
  mean_IXIC<-mean_change(IXIC_$Adjusted)
  mean_N225<-mean_change(N225_$Adjusted)
  mean_KLSE<-mean_change(KLSE_$Adjusted)
  mean_GSPC<-mean_change(GSPC_$Adjusted)
  index_list<-c(mean_HSI,mean_IXIC,mean_N225,mean_KLSE,mean_GSPC)
  index_name<-c('Hang Seng','Nasdaq','Nikkei','Bursa','SP500')
  index_df<-data.frame(index_name,index_list)
  names(index_df)<-c('Name','Average Montly Return')
  return(index_df)
}
# set.seed(5)
# result<-cumprod(1+sample(change,size=50))
# plot(result,type='l')
# mean(result)
# plot(HSI$HSI.Adjusted,type='l')
# HSI %>% filter(HSI.Adjusted)
# sample(HSI$HSI.Adjusted,size=50)
