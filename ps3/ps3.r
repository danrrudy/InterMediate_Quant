#Daniel Rudy
#Intermediate Quantitative Methods
#30 January 2023
#Problem Set 3
#This file is designed to output all answers to the console via source("ps3.r")

rm(list=ls())
library(foreign)
library(marginaleffects)
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