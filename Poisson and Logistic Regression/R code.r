#Άσκηση 1

#1ο ερώτημα
asfalies<- read.table("c:/asfalies.txt",header=TRUE)
attach(asfalies)
asfalies
asfalies$cartype <- factor(asfalies$cartype)
mod <- glm(y ~ cartype + agecat + district + offset(log(n)), data=asfalies, family = 'poisson')
summary(mod)

step(mod, method="backward", test="Chisq")
step(glm(asfalies$y ~ 1 + offset(log(asfalies$n)), family="poisson"), direction = 'forward', scope =(~ asfalies$cartype + asfalies$agecat + asfalies$district))
step(glm(asfalies$y ~ 1 + offset(log(asfalies$n)), family="poisson"), direction = 'both', scope =(~ asfalies$cartype + asfalies$agecat + asfalies$district))
anova(mod, test="Chisq")

#2ο ερώτημα
confint.default(mod)
(exp(confint.default(mod))-1)*100

#3ο ερώτημα

#pearson & deviance residuals
library(car)
residualPlots(mod, type= 'pearson', pch=19)
residualPlots(mod, type= 'deviance', pch=19)

residualDeviance <- residuals(mod, type = 'deviance')
residualPearson <- residuals(mod, type = 'pearson')
par(mfrow=c(1,2))
plot(residualPearson, ylab ='Pearson Residuals', pch=19)
plot(residualDeviance, ylab ='Deviance Residuals', pch=19)

par(mfrow=c(1,2))
qqnorm(residualPearson, pch=19, ylab ='Sample Quantiles/ Pearson')
qqline(residualPearson)
qqnorm(residualDeviance, pch=19, ylab ='Sample Quantiles/ Deviance')
qqline(residualDeviance)

par(mfrow=c(1,2))
plot(fitted.values(mod), residualPearson, xlab='Fitted Values', ylab='Pearson Residuals', pch=19)
abline(h=0)
plot(fitted.values(mod), residualDeviance, xlab='Fitted Values', ylab='Deviance Residuals', pch=19)
abline(h=0)

fitted.values(mod)[fitted.values(mod)>200]
       4        8       12 
323.0070 892.9206 506.5655 

#hat values 
plot(hatvalues(mod), pch=19)
abline(h=0.375, col="blue")
hatvalues(mod)

# cooks distance
plot(cooks.distance(mod), pch=19)
abline(h=1, col="blue")

#υπόλοιπα πιθανοφάνειας
plot(rstudent(mod), pch=19)
abline(h=0, col="blue")


#4ο ερώτημα

mod1 <- glm(y ~ cartype + agecat + district + agecat*cartype + offset(log(n)), data=asfalies, family = 'poisson')
summary(mod1)

mod2 <- glm(y ~ cartype + agecat + district + district*cartype + offset(log(n)), data=asfalies, family = 'poisson')
summary(mod2)

mod3 <- glm(y ~ cartype + agecat + district + agecat*district + offset(log(n)), data=asfalies, family = 'poisson')
summary(mod3)

1-pchisq(mod$deviance, mod$df.residual)
1-pchisq(mod1$deviance, mod$df.residual)
1-pchisq(mod2$deviance, mod$df.residual)
1-pchisq(mod3$deviance, mod$df.residual)

new_model <- asfalies[-c(1,4,8,11,12),]
mod4 <- glm(y ~ cartype + agecat + district + offset(log(n)), data=new_model, family = 'poisson')
summary(mod4)
1-pchisq(mod4$deviance, mod$df.residual)

#Άσκηση 2

#1ο ερώτημα
leukaemia<- read.table("c:/leukaemia.txt",header=TRUE)
attach(leukaemia)
leukaemia
mod1 <- glm(response ~ age + smear + infiltrate + index + blasts + temperature, family='binomial')
summary(mod1)

anova(mod1,test="Chisq")

step(mod1, method="backward", test="Chisq")
step(glm(response ~ 1, family="binomial"), direction = 'forward', scope =(~ age + infiltrate + index + temperature + smear + blasts), test="Chisq" )
step(glm(response ~ 1, family="binomial"), direction = 'both', scope =(~ age + infiltrate + index + temperature + smear + blasts), test="Chisq")

mod2<-glm(response ~ age + infiltrate + index + temperature, family='binomial')
mod3<-glm(response ~ age + index + temperature, family='binomial')
summary(mod2)
summary(mod2)
anova(mod3,mod2, test="Chisq")

1-pchisq(mod3$deviance, mod3$df.residual)

#2ο ερώτημα 
#γραφικές παραστάσεις μερικών υπολοίπων
library(car)
crPlots(mod3)

#υπόλοιπα deviance
library(hnp)
hnp(residuals(mod3), pch=19) #για τα τυποποιημένα: hnp(rstandard(mod3), pch=19)

#hat values
plot(hatvalues(mod3), pch=19)
abline(h=8/51, col="blue")
hatvalues(mod3)

#cooks distance
plot(cooks.distance(mod3), pch=19)

#υπόλοιπα πιθανοφάνειας
plot(rstudent(mod3), pch=19)
abline(h=0, col="blue")


#3ο ερώτημα 
confint.default(mod3)
(exp(confint.default(mod3))-1)*100
options(scipen = 999)
df<- (exp(confint.default(mod3))-1)*100
df[-1,]


#4ο ερώτημα 
library(pROC)
roc(response, fitted.values(mod3), smooth=TRUE, plot=TRUE, ci=TRUE)