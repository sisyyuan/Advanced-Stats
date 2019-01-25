#Q1
dbinom(12,30,1/3)    
#pro = 0.11
plot(dbinom(0:30,30,1/3))
mean = 30*1/3     
#expected mean = 10
variance = 30*1/3*(1-1/3)     
#variance = 6.67

#Q2
plot(dbinom(0:100,100,0.4),xlab = "Number of Patients Survived",ylab = "Pro")
binom.test(53,100,0.4)$p.value  
#p-value = 0.01036
binom.test(53,100,0.4,alternative = "greater") 
sum(dbinom(53:100,100,0.4))
#p-value = 0.0057

#Q3
Getpvalue = function(p)
{
  myVals = rbinom(1000,10000,p)  #one line code 
  pvalue = rep(-1,length(myVals))
  for (i in 1:length(myVals))
  {
    pvalue[i] = binom.test(myVals[i],10000,p)$p.value 
  }
hist(pvalue,main = paste("Expected Value = ",p))    #uniform distribution
return(c(mean(myVals),var(myVals)))
}
Getpvalue(1/2)
# mean = 5000.122,very close to actual mean: 5000
# variance = 2567.071, close to actual variance: 2500
Getpvalue(0.51)
Getpvalue(0.49)
Getpvalue(0.1)
### The distributions of p-value for the null hypothesis are all uniform distributions no matter what value of p. 