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
income_proj<-function(income,r,year,n){
  data<-rep(0,(n%/%year)+1)
  init=income
  data[1]<-income
  for (i in 1:(n%/%year)){
    init=init*(1+(r/100))
    data[i+1]<-init
  }
  return(data)
}
income_proj(3000,5,2,10)
