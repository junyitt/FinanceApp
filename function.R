future_val<-function(p,r,n){
  returnValue(p*(((1+r/100)**n)-1)/(r/100))
  
}
saving_proj<-function(p,r,n){
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
final_df<-function(age,n,month,ir,growth_d,pr,pt){
  age_c<-c(age:(age+n))
  age_ot<-c(rep(age_c[1],times=(12-month)),rep(age_c[2:length(age_c)],rep(12,(length(age_c)-1))))
  age_ot<-age_ot[1:(n*12)]
  month_ot<- c(month:11,rep(c(0:11),(length(age_c)-1)))
  month_ot<-month_ot[1:(n*12)]
  #print(age_ot)
  #print(month_ot)
  ####
  growth_r<-rep(0,12*n)
  temp<-n%/%growth_d
  temp_1<-c(1:temp)
  temp_2<-temp_1*12*growth_d
  growth_r[temp_2]<-ir
  #print(length(growth_r))
  ##
  promo_r<-rep(0,12*n)
  temp_p<-n%/%pt
  temp_p_1<-c(1:temp_p)
  temp_p_2<-temp_p_1*12*pt
  promo_r[temp_p_2]<-pr
  ##

  ##
  age_df<-data.frame(age_ot,month_ot,growth_r,promo_r)
  return(age_df)
}


