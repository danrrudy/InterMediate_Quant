#Daniel Rudy
#GSPIA Intermediate Quant
#27 January 2023
#Problem Set 2
#source(filename to run output in console)
#===functions===



#===end functions===
rm(list=ls())
summary_stats <- function(var){
   mean <- mean(var)
   median <- median(var)
   sd <- sd(var)
   cat("Mean: ",mean, "\n")
   cat("Median: ",median, "\n")
   cat("Standard Deviation: ",sd, "\n")
}

header <- function(num){
	cat("\n\n--Problem ",num,"--\n\n")
}




library(foreign)
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

print(summary(lm(Y ~ X1 + X3,data=df)))
message("\n\n")
print(summary(lm(Y ~ X1 + X2 + X3,data=df)))
header(2)


