#Lecture Notes
#6 February 2023
#Problems with data

library(car)
library(foreign)
library(lmtest)

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
	cat("\n\n== Problem ",num,"==\n\n")
}

Section_Counter <- 0
lite_header <- function(text){
	Section_Counter <<- Section_Counter+1
	cat("\n --",Section_Counter,text,"--\n")
}

#===end functions===

#df<-read.dta("Problem Set 4 - Problem 1 Data.dta")
#lite_header("Input data")
#print(head(df))
#This is not the correct data for the lecture slides?

x1 <- rnorm(1000)
x2 <- rnorm(1000) #not defined in the lecture slides, assuming normal dist
x3 <- rnorm(1000) #not defined in the lecture slides, assuming normal dist
x4 <- rnorm(1000)
x5 <- rnorm(1000)
y <- 2+5*x1 + rnorm(1000, sd=10)

lite_header("Basic Linear Model")
print(summary(lm(y~x1)))

#Overview of a nonlinear relationship
y.polynomial <- 2+x1-10*x1^2 + rnorm(1000, sd = 10)
lite_header("Basic Quadratic Model (squared term ommitted)")
print(summary(lm(y.polynomial~x1)))

lite_header("Basic Quadratic Model")
print(summary(lm(y.polynomial~x1 + I(x1^2))))
par(mfrow=c(1,2))
residualPlot(lm(y.polynomial ~ x1), main="Squared Term ommitted")
residualPlot(lm(y.polynomial~x1 + I(x1^2)), main="Quadratic")

#Partial Residuals
lite_header("Partial Residuals - linear")
y.multiple <- 2+ 5*x1 - 10*x1^2 + 10*x2-x3 + rnorm(1000, sd=10)
print(summary(lm(y.multiple ~ x1 + x2 + x3)))
residualPlots(lm(y.multiple ~ x1 + x2 + x3), main="Linear terms only")

lite_header("Partial Residuals - Quadratic")
print(summary(lm(y.multiple ~ x1 + I(x1^2) + x2 + x3)))
dev.new()
residualPlots(lm(y.multiple ~ x1 + I(x1^2) + x2 + x3), main="With quadratic term")

#Nonnormality

lite_header("Nonnormal model")
x <- rnorm(1000)
y <- 1+x+3*x^2-4*x^3 + rnorm(1000, sd=5)
wrong_model <- lm(y ~ x1)
print(shapiro.test(wrong_model$res))
#Shapiro-wilk examines the null of a sample coming forma normal distribution
#dev.new()
#plot(wrong_model, main="Q-Q: wrong model")
#Q-Q plots of residuals graphically plot the quantiles of the residuals

#Ommitted Variable Bias
#The Wald test compares two models assuming one is fully nested inside the other
#resettest(model, power = low:high) is the function to use for real-world data
mod.ovb <- lm(y ~ x1)
mod.ovb2 <- lm(y ~ x1 + I(mod.ovb$fit^2)+I(mod.ovb$fit^3)+I(mod.ovb$fit^4))
lite_header("Wald (manual OVB")
print(waldtest(mod.ovb, mod.ovb2))
lite_header("Reset Test (auto OVB)")
print(resettest(mod.ovb, power=2:4))

#Nested Models
#A Model is Nested if it can be reduced to another model by imposing lienar restrictions
# i.e. it contains all of the terms of the other model, and optionally additional terms
#This can be tested for with an F test where the additional vars are held constant (linear restriction)
#Models are NOT nested if they each contain variables the other does not
#In this case, we cannot use an F test
# Must instead use an Encompassing Model Test or a J Test
#For GLMs use the Vuong test or [...]

#An emcompassing model includes all of the terms (minus error) from both, and we can compare this to the originals pairwise
x1 <- rnorm(1000)
x2 <- rnorm(1000)
x3 <- rnorm(1000) 
x4 <- rnorm(1000)
x5 <- rnorm(1000)
y <- 3 + 4*x1 - 0.5*x2 + rnorm(1000, sd=5)

mod.nest1 <- lm(y ~ x1 + x2 + x5)
mod.nest2 <- lm(y ~ x1 + x3 + x4)
mod.nestbig <- lm(y ~ x1 + x2 + x3 + x4 + x5)

lite_header("Wald Test")
print(waldtest(mod.nest1, mod.nestbig))
print(waldtest(mod.nest2, mod.nestbig))
print(linearHypothesis(mod.nestbig, c("x3=0", "x4=0")))

lite_header("J Test")
print(jtest(mod.nest1, mod.nest2))
#In a one-sided case, i.e. Mi + fitted(Mj) is significant but not
# Mj + fitted(Mi), then we reject the insignificaant in favor of the significant
#The Coefficient itself in the J test is not valuable, just its significance

lite_header("Outliers, Leverage, and Influence")
#Outliers have large residuals
##This can be any point with a significantly different dependent variable (off the regression line)
#Points that significantly impact the regression have Leverage
##This can be any point with a significantly different independent variable (away from clusters)
#Outliers with Leverage have high Influence
##Points that, if removed, would cause a significant change in the regression model

#Bonferroni Correction accounts for outliers by increasing the T or Z statistic necessary to reject a hypothesis
#in particularly large samples, since we would assume 5% to fall outside
n <- 500
x1 <- rnorm(n)
x2 <- rnorm(n)
y <- 3 - 2*x1 + 7*x2 + rnorm(n, sd=5)
dF <- data.frame(y, x1, x2)

mod.bon <- lm(y~ x1 + x2, data = dF)

bon.crit <- qt(.05/(n*2), n-4, lower.tail = FALSE)

h.crit <- 2*mean(hatvalues(mod.bon))

cooks.crit <- qf(1-0.95,3,n-3)

dF$influential <- 0
print(dF$influential[which ((abs(rstudent(mod.bon)) >= bon.crit & hatvalues(mod.bon) >= h.crit) | cooks.distance(mod.bon) >= cooks.crit)] <- 1)
dev.new()
influencePlot(mod.bon, main="No Outliers expected")

#Once troublesoem variables are identified, check first for coding errors
#If coding errors can't be fixed, remove the points
#Then look for omitted variables or other potential sources of variation.
#Then check for influence on the model. If they aren't can leave as is
#If they are impactful, decision tobe made
#Removing them is questionable if they can't be prioven to be insignificant, and the cahnge should be explained
#Include model with outliers included in an appendix

