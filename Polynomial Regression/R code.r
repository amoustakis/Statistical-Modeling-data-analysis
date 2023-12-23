#1η άσκηση 
#Ερώτημα1

file1<- read.table("c:/vehicles.txt",header=TRUE)
attach(file1)
file1

mydata<-subset(file1,select=-c(car))
mydata

cormat<-cor(mydata)
cormat

library(corrplot)

testcor=cor.mtest(mydata) 

corrplot(cormat, type = "upper", tl.col = "black", p.mat = testcor$p, insig = 'p-value',sig.level = 0) 

library(car)

mod1<-lm(mpg~cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb)
summary(mod1)

vif(mod1)
corrplot.mixed(cor(mydata), upper.col = COL2('PRGn'),lower.col = 'Dark green')
plot(mod1, which=2, pch=19)
par(mfrow = c(2,2) )
plot(mod1, pch=19)

rstandard(mod1)
rstudent(mod1)

plot(hatvalues(mod1), pch=19) # plot(ols_leverage(mod1))
hatvalues(mod1)
library(olsrr)
ols_plot_cooksd_bar(mod1)
ols_plot_dffits(mod1)
ols_plot_dfbetas(mod1)

#Ερώτημα 2

mod1backward <- step(mod1, direction = 'backward', test = 'F')
summary(mod1backward)
modforward <- step(lm(mpg~1),mpg~cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb,direction = "forward", test="F")
summary(modforward)
modboth <- step(lm(mpg~1),mpg~cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb,direction = "both", test="F")
summary(modboth)
data.frame(ols_step_all_possible(mod1)) 


#Ερώτημα 3

experimental<- lm(mpg~wt+qsec+am)
par(mfrow = c(2,2))
plot(experimental, pch=19)

avPlots(experimental)
crPlots(experimental)


#εδώ έκανα πολλές δοκιμές για να καταλήξω στο final που παρουσιάζεται παρακάτω

wtlog<-log(wt)
wtlog<-log(qsec)
final<- lm(mpg~wtlog+qseclog)
summary(final)
AIC(final)
 
par(mfrow = c(2,2))
plot(final, pch=19)

avPlots(final)
crPlots(final)
confint(final, level=0.95)
vif(final)

ols_plot_cooksd_bar(final)
ols_plot_dffits(final)
ols_plot_dfbetas(final)
 
newdata = data.frame(wtlog=1.2925,qseclog=2.8495)
predict(final, newdata, interval="predict", level=.95)

#Ridge&Lasso

library(caret)
library(glmnet)
library(mlbench)
library(psych)

file1<- read.table("c:/vehicles.txt",header=TRUE)
file1<- read.table("c:/vehicles.txt",header=TRUE)
attach(file1)
mydata<-subset(file1,select=-c(car))
str(mydata)

pairs.panels(mydata, cex = 2)

set.seed(222)
ind <- sample(2, nrow(mydata), replace = T, prob = c(0.8, 0.2))
train <- mydata[ind==1,]
test <- mydata[ind==2,]
custom <- trainControl(method = "repeatedcv",number = 10,repeats = 5,verboseIter = T)
set.seed(1234)
lm <- train(mpg~., train, method = 'lm', trControl=custom)
lm$results
lm
summary(lm)
plot(lm$finalModel)

#Ridge
set.seed(1234)
ridge<- train(mpg~., train, method = 'glmnet', tuneGrid=expand.grid(alpha=0, lambda=seq(3, 7), length=5), trControl=custom)
plot(ridge)
plot(ridge$finalModel, xvar = "lambda", label = T)
plot(ridge$finalModel, xvar = 'dev', label=T)
plot(varImp(ridge, scale=T))

#Lasso
set.seed(1234)
lasso<-train(mpg~., train, method = 'glmnet', tuneGrid=expand.grid(alpha=1, lambda=seq(0.5, 1.5, length=5)), trControl=custom)
plot(lasso)
plot(lasso$finalModel, xvar = "lambda", label = T)
plot(lasso$finalModel, xvar = 'dev', label=T)
plot(varImp(lasso, scale=T))

#2η άσκηση

#transform the data based on the model 
library(data.table)
groupsAB <- fread("C:/groupsAB.txt")
groups<-data.table(NULL)
groups$Y <- groupsAB$weight
groups$X1 <- groupsAB$height
groups$X2 <- ifelse(groupsAB$gender =="F", 0, 1)
groups$X3 <- groups$X1 * groups$X2

mod1 <- lm(Y ~ X1 + X2 +X3, data = groups)
summary(mod1)
AIC(mod1)

mod2<-lm(Y~X1 +X2, data = groups)
summary(mod2)
AIC(mod2)

mod3 <- lm(Y~X1, data = groups)
summary(mod3)
AIC(mod3)

anova(mod1,mod2, test="F")
anova(mod2,mod3, test="F")

y1 <- groups$Y[groups$X2 == 0]
xx1 <- groups$X1[groups$X2 == 0]
y2 <- groups$Y[groups$X2 == 1]
xx2 <- groups$X1[groups$X2 == 1]
plot(xx1,y1 , main=expression(paste("Weight vs Height")), xlab="Height", ylab="Weight", col="blue",pch=19, xlim=c(1.3,2.2), ylim=c(50,130) )
abline(lm(y1~xx1), col="blue")
points(xx2,y2,col="red",pch=19)
abline(lm(y2~xx2), col="red")
legend("bottomright", c("Males", "Females"), col=c("red","blue"), lty=1:1)