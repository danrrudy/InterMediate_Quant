#Daniel Rudy
#GSPIA Intermediate Quant
#27 January 2023
#Problem Set 2
#This file is designed to output all answers to the console via source("ps2.r")

rm(list=ls())
library(foreign)
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


df<-read.dta("Problem Set 2 - Problem 1.dta")
print(head(df))

message("===Problem Set 2===")

header(1)

message("X1 Summary Stats:")
summary_stats(df$X1)
message("X2 Summary Stats:")
summary_stats(df$X2)
message("X3 Summary Stats:")
summary_stats(df$X3)

message("\n X1 Population Stats:")
message("mean: 0")
message("median: 0")
message("Standard Deviation: 1")
message("\n X2 Population Stats:")
message("mean: 0")
message("median: 0")
message("Standard Deviation: 1")
message("\n X3 Population Stats:")
message("mean: 0.3")
message("median: 0")
message("Standard Deviation: .21")

message("Model 1\n")
print(summary(lm(Y ~ X1 + X3,data=df)))
message("As shown above, Model 1 is estimated to be: y=2.884-1.998x_1-0.460x_3+e")
message("\n\n")
print(summary(lm(Y ~ X1 + X2 + X3,data=df)))
message("As shown above, Model 2 is estimated to be: y=2.927-1.993x_1+4.188x_2-0.810x_3+e")
message("\n\n")
message("OLS is an appropriate estimator, full explanation is in an attached document per the instructions")
header(2)
message("OLS is not an appropriate estimator in this case, as the assumption of homoskedasticity is violated")
message("We see this in the construction of x_3, as it is explicitly correlated with the error term.")
message("Because of this, errors will increase with larger values of x_3, and is likely to give biased, incorrect estimates")
header(3)
message("In this case, the OLS assumption of homoskedasticity because while the error terms are independent, they have different variances.")
message("OLS may still be an appropraite estimator if we instead use robust standard errors")
message("If uncorrected, this is likely to produce a pattern in the residuals")
