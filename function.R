future_val<-function(p,r,n){
  returnValue(p*(((1+r/100)**n)-1)/(r/100))

}
saving_proj<-function(p,r,n){
  initmoney<-p
  rate<-r
  num<-n
  data<-seq(1,n)
  # print(data)
  future_val2<-function(x){
    returnValue(round(initmoney*(((1+rate/100)**x)-1)/(rate/100),2))
    
  }
  

  data_res<-sapply(data,future_val2)
  # init=0
  # for (i in 1:n){
  #   init=future_val(p,r,i)
  #   data[i+1]<-init
  # }
  month<-c(1:(length(data_res)))
  return(data.frame(month,data_res))

  data<-rep(0,n+1)
  init=0
  for (i in 1:n){
    init=init+future_val(p,r,i)
    data[i+1]<-init
  }
  return(data)
}
#plot(data,type='l')
#saving_proj(1000,5,30)

#income_proj(3000,5,2,5)
# income_proj<-function(income,r,year,n,pr,p){
#   if(missing(p) & missing(pr)){
#     data<-rep(0,(n%/%year)+1)
#     init=income
#     data[1]<-income
#     for (i in 1:(n%/%year)){
#       init=init*(1+(r/100))
#       data[i+1]<-init
#     }
#     return(data)
#   }
#   else{
#     
#   }
# }
# income_proj(3000,5,2,10)
income_proj<-function(income,r,year,n,pr,p){
  if(missing(p) & missing(pr)){
    data<-rep(0,(n%/%year)+1)
    init=income
    data[1]<-income
    for (i in 1:(n%/%year)){
      init=init*(1+(r/100))
      data[i+1]<-init
    }
    return(data)
  }
  else{
    
  }
}
income_proj(3000,5,2,10)
final_df<-function(age,n,month,ir,growth_d,pr,pt,init_inc){
  age_c<-c(age:(age+n))
  age_ot<-c(rep(age_c[1],times=(12-month)),rep(age_c[2:length(age_c)],rep(12,(length(age_c)-1))))
  age_ot<-age_ot[1:(n*12)]
  month_ot<- c(month:11,rep(c(0:11),(length(age_c)-1)))
  month_ot<-month_ot[1:(n*12)]
  #print(age_ot)
  #print(month_ot)
  ####
  temp<-n%/%growth_d
  r<-ir
  temp_1<-c(0:temp)
  #print(temp_1)
  temp_2<-sapply(temp_1,function(x){ return ((1+ir/100)**x)})
  temp_3<-rep(temp_2,each=(growth_d*12))
  growth_r<-temp_3[1:(n*12)]
  #print(length(growth_r))
  ##
  temp_p<-n%/%pt
  r<-pr
  temp_p_1<-c(0:temp_p)
  temp_p_2<-sapply(temp_p_1,function(x){return ((1+pr/100)**x)})
  temp_p_3<-rep(temp_p_2,each=(pt*12))
  promo_r<-temp_p_3[1:(n*12)]
  ##
  init_income<-rep(init_inc,n*12)
  
  combine_r<-growth_r*promo_r
  proj_income<-init_income*combine_r
  #print(age_ot)
 # print(month_ot)
  date<-paste(age_ot,month_ot,sep='-')
  #print(combine_r)
#  print(date)
  ##
  age_df<-data.frame(as.character(date),age_ot,month_ot,growth_r,promo_r,combine_r,proj_income)
  return(age_df)
}
x<-final_df(20,11,0,5,2,10,5,3000)
access_df<-function(df,x){
  return(df[x])
}

