#check the currect working directory and set the right directory of csv
getwd()
setwd("E:/Manju/MBA/Assignments/capstone")

#read the insurance database and analyse the data available, summaries the data
ins=read.csv("insurance.csv")

#Check the type of each variable and print first few observations,
str(ins)
head(ins)

#all categorical variables to numeric equivalents - factor type 
ins$Gender= as.factor(ins$Gender)
ins$smoker=as.factor(ins$smoker)
ins$region =as.factor(ins$region)

str(ins)
head(ins)

#Check if there any missing values in the dataset,

sum(is.na(ins))
colSums(is.na(ins))

#Fit a multiple linear regression,

dim(ins)

multiple_lm1 <- lm(charges ~ age + Gender + bmi + children + smoker + region,data = ins)
summary(multiple_lm1)
library(car)
vif(multiple_lm1)
RSE1=sigma(multiple_lm1)/mean(ins$charges)
RSE1

#remove Gender, region, 
multiple_lm2 <- lm(charges ~ bmi + age + smoker + children,data = ins)
summary(multiple_lm2)
vif(multiple_lm2)
RSE2=sigma(multiple_lm2)/mean(ins$charges)
RSE2



par(mfrow=c(2,2))
plot(multiple_lm2)

#Let's look at the correlation between each predictor (BMI, age and smoker) and the response (charges)
cor(ins$charges,ins$children)
cor(ins$charges,ins$bmi)
cor(ins$charges,ins$age)

library(plyr)
ins$smoker <- revalue(ins$smoker, c("yes"=1))
ins$smoker <- revalue(ins$smoker, c("no"=0))
ins$smoker <- as.numeric(ins$smoker)
cor(ins$charges,ins$smoker)

#Check for multicollinearity and remove the variables with VIF more than or equal to 10.

library(corrplot)
library(dplyr)
myvars <- ins %>% 
  select(bmi, age, smoker, children)
myvars$smoker <- as.numeric(as.character(myvars$smoker))
corrplot(cor(myvars), method = "number", tl.col="black")
corrplot(cor(myvars), add=T, type="lower", method="number",
         col="black", diag=F, tl.pos="n", cl.pos="n")



#List the significant variables, interpret the adjusted R square plot the residuals.
#Check if there is any pattern or is it randomly distributed?