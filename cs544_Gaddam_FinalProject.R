library("googleVis")
library("plotly")
library("sampling")
getwd()
setwd("/Users/gaddamnitish/Desktop/CS544_Gaddam_FinalProject")
getwd()
dir()
data.info<-read.csv("/Users/gaddamnitish/Desktop/CS544_Gaddam_FinalProject/Homicide.csv")
options(max.print = 999999999)
data.info

#Checking any missing values
is.na(data.info)

#Now we try and deduce the Categorical Data like Victim_Race and State

#Pie chart for Victim race
race <- as.data.frame(table(data.info$Victim.Race))
head(race)
pie <- plot_ly(race,labels=race$Var1,values=race$Freq,type='pie')
pie

#Bar Plot for State 
state<-as.data.frame(table(data.info$State))
head(state)
bar<-plot_ly(x=state$Var1,y=state$Freq,type="bar", name="Bar Plot for State-wise Crimes")
bar


#Numerical Value Victim_Age
age<-data.info$Victim.Age
mean(age)
median(age)
range(age)
diff(range(age))
var(age)
sd(age)
a<-fivenum(age)
a
summary(age)

#plot-Histogram for Victim Ages
ages <- as.data.frame(table(as.numeric(data.info$Victim.Age)))
head(ages)
hist<-plot_ly(x=ages$Var1,y=ages$Freq,type="histogram")
hist

#Boxplot for Victim Ages
boxplot(data.info$Victim.Age,horizontal = FALSE,xaxt="n",ylab="Ages of Victims", outline=FALSE,
        main="Boxplot for Victim Ages")
axis(side=1,at=a,labels=TRUE,las=2)

summary(age)


#Central Limit Theorem

#samples of 5,20,30,40

par(mfrow=c(3,3))
samples<-nrow(data.info)
samples.size<-c(5,20,30,40)
xbar<-numeric(samples)
for(c in samples.size){
  for(i in 1:samples){
    xbar[i]<-mean(rnorm(samples.size,mean=34.40,sd=29.55))
  }
  hist(xbar,prob=TRUE,breaks=5,xlim=c(0,5),main=paste("sample size",c), col="blue", 
       ylab="Number of Victims", xlab="Age of Victims") 
  mean(xbar[c])
  sd(xbar[c])
}



#various sampling methods

#simple random sampling with replacement- 
s<-srswr(500,nrow(data.info))
s[s!=0]
rows<-(1:nrow(data.info))[s!=0]
rows<-rep(rows,s[s!=0])
sample.1<-data.info[rows,]
head(sample.1)
table(sample.1$Victim.Age)


#simple random sampling without replacement
s<-srswor(500,nrow(data.info))
sample.2<-data.info[s!=0,]
head(sample.2)
table(sample.2$Victim.Age)

#Systematic sampling
N<-nrow(data.info)
N
n<-500
k<-ceiling(N/n)
k
#random item from first group
r<-sample(k,1)
r
#select every kth item
seq(r,by=k,length=n)
sample.3<-data.info[s,]
head(sample.3)
table(data.info$Victim.Age)

#stratified Sampling
data<-data.frame(Victim.Race=data.info$Victim.Race, Victim.Age=data.info$Victim.Age)
head(data)
st.1<-strata(data,stratanames=c("Victim.Race"),size=rep(8,5),method="srswor",description=TRUE)

#cluster Sampling
c1<-cluster(data,c("Victim.Age"),size=8,method="srswor")
c1.sample<-getdata(data,c1)
head(c1.sample)
table(c1.sample$Victim.Age)



#Confidence interval
set.seed(150)

pop.mean <- 34.40
pop.sd <- 29.55

x <- rnorm(nrow(data.info), mean = pop.mean, sd = pop.sd)
x <- as.integer(x)

sample.size <- 80

sd.sample.means <- pop.sd/sqrt(sample.size)
sd.sample.means

sample.data <- sample(x, size=sample.size)
head(sample.data)

xbar <- mean(sample.data)
xbar

conf<-c(90,80)
alpha<- 1- conf/100
result <- data.frame()
for(i in alpha){ 
  # Conf Level, alpha, CI_Lower, CI_Upper
  result <- rbind(result, c(100*(1-i),i, 
                            xbar - qnorm(1-i/2)*sd.sample.means, 
                            xbar + qnorm(1-i/2)*sd.sample.means))  
}
colnames(result) <- c("Conf Level", "alpha", "CI_Lower", "CI_Upper")
result
#poplation mean lies between the range of both the confidence intervals

