#Daniel Rudy
#GSPIA Intermediate Quant
#22 November 2023
# --- Problem 1 ---
message("Problem 1:")
message("The standard Deviation of the sampling distribution of the sample mean is equal to the population standard deviation divded by sqrt(n) \n")
#3/sqrt(100)=.3

# --- Problem 2 ---
message("Problem 2:")

cat("normal approximation", pnorm(270,mean=300, sd=15), "\n")
#Output: [1] 0.02275013  

cat("true probability:", sum(dbinom(x=c(0:269),size=1200,prob=.25)), "\n\n")
#Output: [1] 0.02009287  

# --- Problem 3 ---
message("Problem 3:")
x <- c(186,181,176,149,184,190,158,139,175,148,152,111,141,153,190,157,131,149,135,132)
xmean <- mean(x)
#mean: 156.85
xsd <- sd(x)
#sd: 22.64201
xse <- xsd/sqrt(20)
#se= 5.0629
alpha <- .10
t = qt(p=alpha/2, df=19, lower.tail=F)
CI <- c(xmean-(t*xse), xmean+(t*xse))
cat("CI: ",CI, "\n")
#Output: [1] 148.0956 165.6044  

# --- Problem 4 ---
message("Problem 4: \n")
x <- c(15.6,18.6,18.3,20.1,21.5,18.4,19.1,20.4,19.0)
print(t.test(x,mu = 20, alternative = "less"))
message("\n")
#p=.05402

# --- Problem 5 ---
message("Problem 5 on paper \n")

# --- Problem 6 ---
message("Problem 6:")
A <- c(1.23,1.42,1.41,1.62,1.55,1.51,1.60,1.76)
B <- c(1.76,1.41,1.87,1.49,1.67,1.81)

message("In this case, we are comparing the means of two samples, in a one-sided t-test, where both sets are numeric and independent of each other. Because the samples are of different individuals, a paired test would be inappropriate. R by default assumes that the population variances are not equal, which is a fair assumption here. The problem specifies the alternate hypothesis as the mean concentration of B is larger than that of A, i.e. A<B. \n")
print(t.test(A,B,alternative="less"))
#p=.06392

message("since p<.10, we can reject the null hypothesis, and accept the alternate hypothesis that the mean concentration of B is larger than that of A. \n")

# --- Problem 7 ---
message("Problem 7")

message("We use the z-test for comparing proportions, via prop.test() in R. It is appropriate to use the continuity correction (default) due to the low sample size")
print(prop.test(c(12,4),c(18,12), alternative="two.sided"))

message("the p value is 0.1558 > .05, so we cannot reject the null hypothesis and are unable to conclude that the proportions are significantly different")