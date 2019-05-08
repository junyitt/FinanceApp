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
saving_proj(1000,5,30)
