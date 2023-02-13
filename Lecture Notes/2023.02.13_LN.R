#Lecture Notes
#13 February 2023
#Indicator Variables

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

df<-read.dta("Problem Set 5.dta")
lite_header("Input data")
print(head(df))

#The garden of forking paths
#when you start to make model dcisions, you can lead down a very different path from someone else considering the same data
#Overall results may end up the same, but different paths have different challenges, requirements, etc.
#Think proof systems in analysis, abstract algebra, etc.

#Very hard to find valuable instrumental variables that meet the relevant criteria
#Instrumentalvariables are very reliant on knowing the content of the data youre analyzing
#Requiresfinding a variable Z that strongly correlates with the real predictor X but does not have a causal relationship with the predicted value Y
# "A lot of instruments are going to be bad"
#Rain is often used as an instrument: we don't impact it directly, it doesn't impact the outcome directly, but it is bad because it has a high 'W' factor because it has a strong impact on a number of other factors that are important for predicting Y
#If we use instrumental variables, you need to know the substance, and understand why it only works through X, no W factor, and does not directly regress with Y
#So generally speaking, looking at the root causes of variations of Y via literature review,
#Comparisons with the existing factors of analysis and confirming no covariance could create a clear pathway for setting a factor Z

#Relevance: there needs to be some significant relationship X ~ Z
#Excludability: Need to confirm that Z does not directly cuase the output, which requiresa narrative in addition to statistics
#Exogeneity: Need to confirm that htere is not an omitted atiable W that the Z correlates with, which requires a narrative showing a complete argument

#IV regression is easy to do, hard to justify

#"A necessary but not sufficient condition for having an instrument that can satisfy the exclusion restriction is if people are confused when you tell them about the instrument's relationship to the outcome"
##Scott Cunningham, Causal Inference, the Mixtape p. 123

#Draft numbers were a common IV
#Under criticism because people on the boundary of grade entry points can be significant

#IC methods can introduce inherent restrictiosn in the analysis
#In the case of the LIH example, the IV is only interpretable in the case that there are two children (not <>2) and they are in a state with a housing policy that varies like this
#Sometimes called Local Average Treatment Effects (LATE)

#May get a negative R^2 in IV regression
#This is because in IV the Residaul Sum of Squares (RSS) and Total Sum of Squares (TSS) do not come from the same model, so it is possible for their ratio to be outside [0,1]

#Negative R^2 means predictive power is being sacrificed for explanatory

#Does IV have issues with replicability?
##it can in the sense that, substantively weaker implements can get very different results. The key here is to be able to explain the variation
##Could jut be random noise

#Do not want to reject Sargen test

#Hausman test can check if an instrument is even plausible
#We regress the endogenous vaariable on its instruments and save the residuals
#We then include the residuals in the OLS regression we estimated with an assumption of endogeneity
#We then run a test against the null that the the resudual coeddieient is 0. If it is rejected, there is endogeneity.
#This does not shwo the Y ~ Z relationship, just the Y ~ X validity


