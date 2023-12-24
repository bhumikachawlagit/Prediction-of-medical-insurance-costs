dat = insurance
class(dat)
dat = as.data.frame(dat)

#Describing data:
summary(dat)
head(dat)
glimpse(dat)

#Data visualization :
install.packages("plotly")
library(plotly)

Children = dat$children
plot_ly(x = ~ Children,type = "histogram")
plot_ly(x = ~insurance$age , type = "histogram")
plot_ly(x = ~insurance$bmi, type = "histogram")

freq = table(dat$smoker)
plot_ly(x = c("no","yes"),y = freq , type = "bar")

hist(dat$bmi,probability = TRUE,ylim=c(0,0.1),col="light blue")
curve(dnorm(x,30.6634,sqrt(37.18788)),add = TRUE,lwd = 2)

mean(insurance$bmi) + 3*sqrt(var(insurance$bmi))
length(insurance$bmi[insurance$bmi > 48.95796 ]/length(insurance$bmi))
#4 outliers?

summary(dat$charges)
cv = sqrt(var(dat$charges))/mean(dat$charges)
cv
plot_ly(x = ~dat$charges , type = "histogram")
hist(dat$charges,col = "light blue")
#Outliers?

#Coding columns with categorical data in numeric form :
for(i in 1:nrow(dat))
  {
    if(dat$smoker[i] == "yes")
      {
        dat$smoker1[i] <- 0}
    else 
      { dat$smoker1[i] <- 1}
      i = i + 1
}
##OR:
dat$smoker1 = ifelse(dat$smoker == "yes",0,1)
dat$sex = ifelse(dat$sex == "female",0,1)
head(dat)
plot_ly(x = ~dat$smoker1,y = table(dat$smoker1),type = "bar")
plot_ly(x = ~dat$sex,y = table(dat$sex),type = "bar")

table(dat$region)

##Using 'One hot encoding' technique here for column of region:
install.packages("caret")
library(caret)
library(data.table)
#Given a formula and initial data set, the class dummyVars gathers 
#all the information needed to produce a full set of dummy variables 
#for any data set.
#The predict function produces a data frame.


dmy = dummyVars("~ region",data = dat,fullRank = T)
dat2 = data.frame(predict(dmy,newdata = dat))
dat2 = cbind(dat,dat2)
head(dat2)

#Model fitting :

dat2 = dat2[,-c(5,6)]
x = dat2[,-c(5)]   #Independent variables
Y = dat2[,c(5)]       #Dependent variable  
cormat = cor(dat2)
corrplot(cormat,method=c("col"),addCoef.col = TRUE)
glimpse(x)
head(x)


#Standardisation :

dat3 = data.frame(scale(dat2[,-c(2,5,6,7:9)],center=TRUE,scale=TRUE))
dat3 = cbind(dat3,dat2[,c(2,6,7:9)])
#dat3 = dat3[,-c(7)]
head(dat3)
x = dat3

##Splitting the data into test and train data :

set.seed(100000)
samplee = sample.int(n=nrow(x),size = floor(0.3*nrow(x)),replace = F)
x_test = x[samplee,]
x_train = x[-samplee,]
Y_test = Y[samplee] 
Y_train = Y[-samplee]
Y_test = as.data.frame(Y_test)
Y_train = as.data.frame(Y_train)

#Model-fitting :

dat_full = data.frame(x_train,Y_train)
head(dat_full)

age = dat_full$age
bmi = dat_full$bmi
children = dat_full$children
sex = dat_full$sex
smoker1 = dat_full$smoker1
regse = dat_full$regionsoutheast
regsw = dat_full$regionsouthwest
regnw = dat_full$regionsouthwest

## Forward selection :

result1 = lm(Y_train ~ 1,data = dat_full)

step(result1,scope = ~ age*bmi*children*smoker1*
       sex*regse*regsw*regnw,
     direction = "forward",test="Chisq")
##Backward selection:

result1 = lm(Y_train ~ smoker1 + age + bmi + children + regsw + 
               regse + smoker1:bmi + bmi:regse + age:bmi + smoker1:children
               + bmi:children,data = dat_full)

step(result1,scope = ~. ,direction = "backward",test = "F")

##Step wise method:

result1 = lm(Y_train ~ smoker1 + age + bmi + children + regsw + 
               regse + smoker1:bmi + bmi:regse + age:bmi + smoker1:children
             + bmi:children,data = dat_full)

step(result1,direction = "both",test = "Chisq")


#Finalizing the model :

z = c()
for(i in 1:NROW(insurance$region))
{
  if(insurance$region[i] == "southwest")
  {
    z[i] = "a"
  }
  else if(insurance$region[i]  == "southeast")
  {
    z[i] = "b"
  }
  else if( insurance$region[i] == "northwest")
  {z[i] = "c"}
  else{z[i] = "d"}
  i = i + 1
}
z = as.factor(z)
visual = data.frame(dat[,3],dat[,7],z)

attach(visual)
plot(visual$bmi,visual$charges,col=c("orange","dark green","light green","light pink")[z]
     ,pch=16,xlab = c("BMI"),ylab = c("Charges"),
     main = c("Plot of BMI vs Charges with a factor of region included"))
detach(visual)
legend(x = "topright",legend = c("southwest","southeast","northwest","northeast"), 
       col = c("orange","dark green","light green","light pink"),pch = 16)

charges2 = c()
for(i in 1:NROW(insurance))
{
  if(insurance$charges[i] == "southwest")
  {charges2[i] = insurance$charges[i]}
  else
  {charges2[i] = 0}
}
length(insurance$charges[insurance$charges > mean(insurance$charges)])
length(charges2[charges2 > mean(insurance$charges)])/420
#se = 10%| 31%
#sw = 6% | 20%
#nw = 7% | 23%
#ne = 8% | 26%

barplot(c(0.31,0.20,0.23,0.26),names.arg = c("Southeast","Southwest","Northeast","Northwest"),col="light blue")


bmi2 = c()
for(i in 1:NROW(insurance))
  {
  if(insurance$region[i] == "southeast")
  {bmi2[i] = insurance$bmi[i]}
  else
  {bmi2[i] = 0}
}


length(insurance$bmi[insurance$bmi>40])
length(bmi2[bmi2 > 40])/91
median(bmi2[bmi2 != 0])
mean(bmi2[bmi2 != 0])
#se = 60.4%
#med = 33.33
#avg = 33.35
#sw = 14.3%
#med = 30.3
#avg = 30.60
#ne = 16.5%
#med = 28.88
#avg = 29.17
#nw = 8.8%
#med = 28.88
#avg = 29.20

cf = c()
for(i in 1:NROW(insurance))
{
  if(insurance$sex[i] == "female")
  {cf[i] = insurance$charges[i]}
  else
  {cf[i] = 0}
}

cm = c()
for(i in 1:NROW(insurance))
{
  if(insurance$sex[i] == "male")
  {cm[i] = insurance$charges[i]}
  else
  {cm[i] = 0}
}

mean(cf)
mean(cm)

median(cf[cf!=0])
median(cm[cm!=0])

age = dat_full$age
bmi = dat_full$bmi
children = dat_full$children
sex = dat_full$sex
smoker1 = dat_full$smoker1
regse = dat_full$regionsoutheast
regsw = dat_full$regionsouthwest
regnw = dat_full$regionsouthwest

result3 = lm( Y_train ~ smoker1 + age + bmi + children +
              bmi:smoker1 , data = dat_full)


summary(result3)
anova(result3)
t = ( -1032.8)/ 402.5
qt(0.05,937-8-1,lower.tail = FALSE)
#ANOVA (for individual significance):
result4 = lm(Y_train ~ regsw, data = dat_full)
anova(result4)
anova(result3)

anvp<-anvp[!is.na(anvp)]

#Predictions :

age = dat_full$age
bmi = dat_full$bmi
children = dat_full$children
sex = dat_full$sex
smoker1 = dat_full$smoker1
regionsoutheast = dat_full$regionsoutheast
regionsouthwest = dat_full$regionsouthwest


test = data.frame(x_test[,-c(4,6,7,8)])


predict((result3),test)
predictions = predict((result3),test)
pred = data.frame(predictions)
pred = pred[1:401,1]

RMSE = sqrt(mean((pred - Y_test)^2))
print(RMSE)


#Checking for assumptions :

#Error distribution :

sm = summary(result3)  
Residuals =  Y_test - pred
Standardized_residuals = (Residuals-mean(Residuals))/(sqrt(anv$`Mean Sq`[8]))
data.frame( Std_residuals = head(Standardized_residuals))
qqnorm(Residuals)
qqline(Residuals,col="orange",lwd = 2)
shapiro.test(Residuals)
mean(Residuals)

qqnorm(Standardized_residuals)
qqline(Standardized_residuals,col="red",lwd=2)
mean(Standardized_residuals)
mean(Residuals)

###############################################################
Y_train2 = (Y_train)^2
Y_train2 = data.frame(Y_train2)
rm(dat_full)
dat_full = data.frame(x_train,Y_train2)
result3 = lm(dat_full$Y_train ~ 1,data = dat_full)
step(result3,scope = ~ age*bmi*children*smoker1*
       sex*regse*regsw*regnw,
     direction = "forward",test="F")


result3 = lm(dat_full$Y_train ~ smoker1 + bmi + age + smoker1:bmi + 
               smoker1:age + bmi:age + smoker1:bmi:age, data = dat_full)
summary(result3)
anova(result3)


test = x_test[,c(1,2,5)]
colnames(test)[6] <- c("regse")
pred = predict(result3,test)
length(pred[pred<0])

p = c()
for(i in 1:NROW(pred))
{ 
  ind = i
  if(pred[i] < 0)
  { p[i] = ind}
}
p
p<-p[!is.na(p)]
p
pred = pred[-c(p)]
pred = sqrt(pred)
Y_test = Y_test[['Y_test']]
residlog = Y_test - pred
residlog = as.vector(residlog)
qqnorm(residlog)
qqline(residlog)
hist(residlog)
plot_ly(x = ~residlog, type = "histogram")
##################################################################

#Autocorrelation :

#H0 : No correlation among residuals. 
#vs H1  :Residuals are auto correlated.

durbinWatsonTest(result3)
#We cannot reject H0, thus the residuals aren't correlated.

#Checking if heteroscedasticity is present:

plot(pred,Residuals,pch=16,xlab = c("Predictions/Fitted values"),
     main=c("Residuals vs Fitted plot"))

abline(h = 0,col="orange",lwd=2)
bptest(result3)


#No heteroscedasticity present. 

#Checking Multicollinearity:

VIF = car::vif(result3)
?car::vif(result3)
barplot(c(VIF[1:5]),names.arg=c("Age","BMI","Children",
                                "Smoking status","Region SE"),
                          horiz = TRUE,col="orange",xlim = c(0,10))
abline(v=10,col="red",lwd=2)


# Viewing prediction :
x_test=x_test[,-c(6:8)]
sample = sample.int(NROW(x_test),size = 10,replace = FALSE)
accuracy = x_test[sample,]
accuracy2 = data.frame(predict(result3,newdata = accuracy))
accuracy = data.frame(accuracy,Prediction = accuracy2[,1],Real_values = Y_test[sample])
accuracy
err = (abs(accuracy$Prediction - accuracy$Real_values))
accuracy = data.frame(accuracy,Absolute_error = round(err))
accuracy
mean(err)
RMSE = sqrt(mean((accuracy$Real_values - accuracy$Prediction )^2))
RMSE
range(insurance$charges)


set.seed(100000)
samplee = sample.int(n=nrow(x),size = floor(0.3*nrow(x)),replace = F)
x_test = x[samplee,]
Y_test = Y[samplee] 




#############################################################################
boxplot(insurance$bmi)
plot_ly(x = ~insurance$bmi, type = "box")

insurance$bmi[insurance$bmi>48]

p = ifelse(insurance$bmi>48,1,0)
table(p)

p = c()
NROW(insurance$bmi)
for(i in 1:NROW(insurance$bmi))
{ 
  ind = i
  if(insurance$bmi[i] > 48)
   { p[i] = ind}
  i = i + 1
}
p
p<-p[!is.na(p)]
p

plot_ly(x = ~insurance$children,type = "box")
plot_ly(x = ~insurance$age,type = "box")


result4 = lm(formula = Y_train ~ age + bmi + children + smoker1 + smoker1:bmi
             +age:smoker1, 
             data = dat_full)
summary(result4)
#SSR:
3884.7*(x_train[,1] - mean(x_train$age)) 

age = data.frame(dat_full$age)
bptest(result3,varformula = ~dat_full$regionsouthwest)

saveRDS(result3,file = "rb")
readRDS(result3,file = "rb")

##############################################################################

























