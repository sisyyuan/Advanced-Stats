die = c(1,2,3,4,5,6)
p = c(rep(0.1,5),0.5)
E = die*p
Mean = sum(E)
Mean
Variance = sum(p*(Mean-die)^2)
Variance


rollLoadedDie = function(x)
{
  rolls = vector(length = x,mode = "double")
  for (i in 1:x)
  {
    rolls[i] = sample(1:6,1,replace = T,prob = p)
  }
  return (rolls)
}
MyRoll = rollLoadedDie(10000)
mean(MyRoll)
var(MyRoll)

hist(rollLoadedDie(10000),breaks = seq(0.5,6.5,by=1))

hist(rollLoadedDie(1000000),breaks=seq(0.5,6.5,by=1))


trialSize = c(5,10,15,20,25,30,40,50,100,200,300,400,500,1000,2000,3000,4000,5000,10000,20000,30000,100000)
means = vector(length = length(trialSize),mode = "double")
variances = vector(length = length(trialSize),mode = "double")

for (i in 1:length(trialSize))
{
  MyRoll = rollLoadedDie(trialSize[i])
  means[i] = mean(MyRoll)
  variances[i] = var(MyRoll)
}
plot(log10(trialSize),means)
lines(log10(trialSize),rep(Mean,length(trialSize)))

plot(log10(trialSize),variances)
lines(log10(trialSize),rep(Variance,length(trialSize)))



