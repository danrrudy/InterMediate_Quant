#Lecture Notes 1.23.2023
#Focusing on regression this week
#In the social sciences, targeting R^2 to 1 is unlikely to succeed due to the inherently nonlinear and stochastic nature of our observations
#"If you get anythign above a 0.25, people think you're doing great"
#F test is a null that all of the coefficients are equal to 0 other than the intercept
#read.dta("filename") loads data into a df
library(foreign)
df <- read.dta("Problem Set 2 - Problem 1.dta")
lm(Y ~ X1 + X2 + X3,data=df)
#lm produces the linear model, which can be saved into a variable, or wrapped into the summary frame to view summary stats
summary(lm(Y ~ X1 + X2 + X3,data=df))

model <- lm(Y ~ X1 + X2 + X3,data=df)
#confint returns a confidence interval for each of the independent vatiables
confint(model)
#print command is needed to return output via source() command, likely will need to add this to each non-console outputting ocmmand in assignments
print(confint(model))
#R syntax note:
#need to wrap t.test with the with(df, vars) command  to pull vars directly form a df, comma structure will nto grab it
#if using fixed effects as a regression coefficient, one will need to be dropped if it is a complete list to avoid linear definciency
#For problem 3 in the PS, yes, we can use OLS despite violating the homoskedacisity assumption as long as we use different standard errors(which we are able to do realtively easily)