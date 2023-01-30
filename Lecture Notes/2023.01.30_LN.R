#2023.01.30_LN
#For peoblesm of violated assumptions, only need to show which assumption is violated and why
#Generally, F tests check overall model effect
#Probability of an error = Sigma_{forall(x)}(P(type I)+P(Type II))
#F-test combines all of the null hypotheses that a given coefficient is zero into one H_0^F that ALL coefficients are zero
#Determines whether the model predicts better than one iwth no coefficient


#Notes Section:

#Interaction Terms
#"Not quite linear"
#If we ahve a regression like:
#X_1: Hours Worked
#X_2: Level of Education
#I=b_0+b_1(x_1)+b_2(x_2)+b_3(x_1x_2)
#We may assume:
#b_1>0, clearly
#b_2>0, clearly
#b_3<0, because hgiher education reduces the impact of hours worked on income, since high-ed jobs are typically salaried, breaking the b_1 relationship down
#Conditional relationship will be covereed next week - highly leveraged data poitns nad how they can impact the model fit
#Might also want to include a nonlinear term that accounts for diminishing returns
#Use F test for next problem set
#Testing a joint linear hypothesis:
#More straightforward han it seems
#Just run a F-test on the two coefficients under consideration

#In-class code:
library(foreign)
library(car)
set.seed(2023)
x1 <- rnorm(1000)
x2 <- rnorm(1000)
x3 <- rnorm(1000)
e <-rnorm(1000, sd=2)
Y <- 3-2*x1+4.25*x2+x3-0.9*x3*x1+e
df <- data.frame(Y, x1, x2, x3)

library(marginaleffects) #can use this package to find the arginal effects of one variable on another at various test values

#Alternative is pulling a table is to plot them and generate residuals around regression model as marginal effect plot
X1 <- norm
model <- lm(Y ~ x1*x2*x3, data=df)
print(summary(model))
print(summary(marginaleffects(model, newdata = datagrid(x2=2, x3= 0, model = model), variables = c("x1"))))
p = plot_cme(model, effect = "x2", condition = list("x1", "x3"=c(0,1)), conf.level=0.9)
X11()	#This and
plot(p)	#This together will draw the plot in a new window

