#************************************************************************************
#Lab exercise FU-Berlin
#************************************************************************************
#This execise contains:
#1. Visualization data: pairs, hist, simple plots
#2. Linear regression: 
#   2.1. Simple LR: analysis of the relationship O3-meteovar
#   2.2. Multiple linear regression: fit the best model with more than one predictor
#3. Logistic regression model to analyse high ozone levels (part2, optional)
#************************************************************************************
# Author: NOF      
#  up. 04.2020
#************************************************************************************

#Load library
library(MASS)  # general packages 
library(stats)
# library(Hmisc)
library(ggplot2) # plots
library(dplyr)

#Read the file (adding the right path)
mydata  <- read.csv("data/data_year_o3.csv")
#Look the data
str(mydata)
head(mydata)
# Convert date to Date for plotting
mydata$date <- as.Date(mydata$date)
# Create months
mydata$mon <- format(mydata$date, "%m")

#########################
#Data visualization
#Simple plots
########################
plot(mydata$o3,type="l",col="orange",xlab="days",ylab="MDA8 O3 (ppb)",main="MDA8 O3")
hist(mydata$o3,main="Distribution of Ozone",col='skyblue',xlab="MDA8 O3")
# Other options of plots
# ggplot2
ggplot2::ggplot(mydata, aes(x=date, y=o3, group=1)) + 
  geom_line(color="red") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y-%m")

#Scatter plots:

plot(mydata$tmax,mydata$o3,pch = 19, col = "blue", main = "Ozone vs Tx", xlab = "Tx (k)", ylab = "O3 (ppb)")

#Add fit lines
abline(lm(o3~tmax,mydata),col="red") # regression line (y~x) 

# visualize all historigrams-graphs at once (alternative)
# hist(dat) #library(Hmisc)
#
pairs(mydata[,c(3:9,2)],panel=panel.smooth,col='black',cex=0.5,cex.labels=1)

#another way to make the plots
#lower panel:# panel.smooth function is built in.
source("plot_panel.R") #add correct path
pairs(mydata[,c(3:9,2)], cex.labels =0.9,
      lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.hist)


# We can create a season column
mydata <- mydata%>%mutate(season=ifelse(mon>="01" & mon<="02" | mon=="12", "DJF",
                                        ifelse(mon>="03" & mon<="05", "MAM",
                                               ifelse(mon>="06" & mon<="08", "JJA",
                                                      ifelse(mon>="09" & mon<="11", "SON",NA)))))
# Another way to plot correlations
require(ggcorrplot)
corr <- round(cor(mydata[,c(3:9,2)]), 3)
ggcorrplot(corr)


##################################
# Split data into seasons
# 1. Analyse seasonal cycle
# 2. Daily cycle
#################################


# We plot each season 
# create a subset for each season and visualise
data_jja <- subset(mydata, season=="JJA")
data_mam <- subset(mydata, season=="MAM")
plot(data_jja$o3,type="l",col="orange",xlab="days",ylab="MDA8 O3 (ppb)",main="MDA8 O3")
# It can be done for each season
# We can use ggplot and facet seasons
ggplot2::ggplot(mydata, aes(x=date, y=o3))+ 
                        geom_point() + facet_grid(~season)

# Plot the monthly cycle
# 
boxplot(mydata$o3~mydata$mon)
# Similarly with ggplot2
ggplot2::ggplot(mydata, aes(x=mon, y=o3))+ geom_boxplot(fill="blue")

# The rest of the variables can be also assessed like this
ggplot2::ggplot(mydata, aes(x=mon, y=tmax))+ geom_boxplot(fill="blue")

############################################################################

#*******************
#Linear regression:
#*******************
# Before starting, we will split the data into seasons and we restrict the analysis to
# summer (JJA). Repeat the steps for MAM for comparison
# Let's use  data_jja
#Fitting different models
#null model
# Remove the last column to avoid problems 
data_jja$season <- NULL
data_mam$season <- NULL
#
m0  <- lm(o3~.,data=data_jja,na.action=na.omit)

#linear regression
m1  <- lm(o3~tmax,data=data_jja,na.action=na.omit) #or you can start by adding a different parameter (Tx,RH...)
#check model output
summary(m1)


#Adding more variables:
m2 <- update(m1,~.+rh)
m3 <- update(m2,~.+blh)
m4 <- update(m3,~.+ssrd)


#Comparing models:

#1.Anova test:
#Comparing null model with the model fitted with new variables
# m1 is statistically significant (Tx contribute to o3 variance)
anova(m0,m1)

#comparison of more models
anova(m1,m2,m3)
anova(m3,m4)


#Other way for comparison models:
#2.AIC criterion: 
# When comparing two models, the smaller the AIC, the better the fit. 
# This is the basis of automated model simplification using step
AIC(m1,m2)
AIC(m2,m3)
AIC(m1,m2,m3,m4)

#******************************************************
# Multiple linear regression analysis:
#******************************************************
#Linear regression analysis with more variables

pred.names <- names(data_jja[-(which(names(data_jja)%in%c("o3","date","mon")))])
# These are the covariates
print(pred.names)
#One way to write the form of the equation:
form  <- (paste("o3~",paste(pred.names,collapse='+'),sep=""))
form <- as.formula(form)
#Add the form to the equation
fit <- lm(form, data=data_jja,na.action=na.omit)
# fit <- lm(o3 ~ LO3+Tx+Gh+RH+SR+ST+TC+U10, data=dat,na.action=na.omit)
summary(fit)
# Other useful functions 
coef <- coefficients(fit) # model coefficients
fitted(fit) # predicted values
confint(fit, level=0.95) # CIs for model parameters 
residuals(fit) # residuals
anova(fit) # anova table 

#Fit more models
fit1 <- update(fit,~.-ssrd)
fit2 <- update(fit1,~.-tcc)
fit3 <- update(fit2,~.-ws)

#*************
#Plotting fit:
#*************
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
hist(data_jja$o3,main="Distribution of Ozone",xlab="O3")

sresid <- studres(fit) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit,col="red")

plot(residuals(fit),xlab="residuals",ylab=""); 
title("Simple Residual Plot")

acf(residuals(fit), main = ""); 
title("Residual Autocorrelation Plot ");

#or plot summary(model)
#Visualizing all plots at once
par(mfrow=c(2,2))
plot(fit)

#reset par conditions
par(mfrow=c(1,1))


##################################################
# Model selection
# Find a best subset of predictors to fit the model:
# StepAIC 
##################################################
#direction-> "both", "backward","forward"
model <- stepAIC(fit,direction="both")
summary(model)

#Repeat the plots for the final model
#Plot the model
plot(model)

# for spring repeat the same steps:
fit_mam   <- lm(form, data=data_mam,na.action=na.omit)
model_mam <- stepAIC(fit_mam,direction="both")
summary(model_mam)
#######################
# Variable importance
#######################
require(relaimpo)
# calculate relative importance
relImportance <- calc.relimp(model, type = "lmg", rela = F)  
relImportance_mam <- calc.relimp(model_mam, type = "lmg", rela = F)  

# Sort
cat('Relative Importances: \n')
df.rimpo <- data.frame("jja"=sort(round(relImportance$lmg, 3), decreasing=TRUE))
df.rimpo$variable <- rownames(df.rimpo)
# for mam
df.rimpo.mam <- data.frame("mam"=sort(round(relImportance_mam$lmg, 3), decreasing=TRUE))
df.rimpo.mam$variable <- rownames(df.rimpo.mam)
# plots
ggplot2::ggplot(df.rimpo, aes(x=variable, y=rimpo))+ geom_bar(stat = "identity") 

#******************************
# Logistic regression: glm
#******************************
# conditions, high O3 levels (how many days with O3>50 or 60 ppb)
ths            <- 50 # change to 60 and see how the number of excendances changes
data_jja$date.f  <- 1:length(data_jja$date)
data_jja$o3_50   <- ifelse(data_jja$o3>=ths, "orange", "forestgreen")
plot(o3~date.f, data=data_jja, type="h", col=o3_50)
abline(h=ths, lty=2, col="red")

#Analyse ozone exceedances >50ppb


#Convert the outcome(o3) into binary data:
data_jja$o3 <- ifelse(data_jja$o3>ths,1,0)

#Fit logistic model

fitglm <- glm(o3~tmax+rh+ssrd+blh+Direction+ws,data=data_jja,family=binomial())
#summary of model
summary(fitglm)
#Apply model selection
modelglm <- stepAIC(fitglm,direction="both")
summary(modelglm)
plot(modelglm)

# Compute pseudo R-square
modelChi  <- modelglm$null.deviance - modelglm$deviance
pseudo.R2 <- modelChi / modelglm$null.deviance
pseudo.R2


newdata <- data_jja
newdata$pred.o50 <- fitglm$fitted.values

pred  <- predict(modelglm,type="response") # gives us the probability


# For variable importance
require(caret)

varImp(modelglm, scale=T)


