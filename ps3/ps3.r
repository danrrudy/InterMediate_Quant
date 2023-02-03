#Daniel Rudy
#Intermediate Quantitative Methods
#30 January 2023
#Problem Set 3
#This file is designed to output all answers to the console via source("ps3.r")

#rm(list=ls())
library(foreign)
library(haven)
#library(rtools)
library(ggplot2)
library(car)
library(marginaleffects)
#library(linearHypothesis)
#===functions===
summary_stats <- function(var){
   mean <- mean(var)
   median <- median(var)
   sd <- sd(var)
   cat("Mean: ",mean, "\n")
   cat("Median: ",median, "\n")
   cat("Standard Deviation: ",sd, "\n\n")
}

header <- function(num){
	cat("\n\n--Problem ",num,"--\n\n")
}

#===end functions===
df<-read.dta("Problem Set 3.dta")


header(1)
model1 <- lm(Y ~ X1 + X2 + X3,data=df)
model2 <- lm(Y ~ X1 + X2 + X3 + X1*X3 + X2*X3,data=df)
print(summary(model1))
print(summary(model2))

message("\n\n Linear Hypothesis")
print(linearHypothesis(model2, c("X1:X3=0", "X2:X3=0")))

#var.test(lm(X1:X3 ~ 0, data = df),lm(X2:X3~0, data = df))
message("\n\n Marginal Effects")
print(marginaleffects(model1, newdata = datagrid(X2=0, X3 = c(0,1), model = model1), variables = c("X1")))
print(marginaleffects(model2, newdata = datagrid(X2=0, X3 = c(0,1), model = model2), variables = c("X1")))
#linearHypothesis(mod)
#linearHypothesis(mod)

p = plot_cme(model2, effect = "X1", condition= list("X2", "X3" = c(0,1)),conf.level=0.9)
X11() #This and
plot(p)  #This together will draw the plot in a new window

#var.test(lm(X1 + X2 + X3,data=df),data=df)
#var.test(lm(X1 + X2 + X3 + X1*X3 + X2*X3,data=df),data=df)