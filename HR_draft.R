peo = read.csv('C:/Users/31221/Desktop/Term1/Applied Statis/final_pro/HR-Employee-Attrition - HR-Employee-Attrition.csv')
peo
summary(peo)
#Age, Education, JobLevel,JobRole



library(tidyverse)
library(gridExtra)
library(dslabs)
library(magrittr)

library(tidyverse)
#Age, Education, JobLevel,JobRole
Attr_bol = ifelse(peo$Attrition == 'Yes', 1,0)
peo2 = data.frame(peo, Attr_bol)

cor(peo2$Age,peo2$Attr_bol)
cor(peo2$Education,peo2$Attr_bol)
cor(peo2$JobLevel, peo2$Attr_bol)
# show no co with education, if your job level is high, and your age is older, Attrition seems low

#age and attrition
hist(peo$Age, label=TRUE, col="blue", ylim=c(0,800))

counts <- table(data1$Attrition, data1$Department)
barplot(counts, main="Employee Distribution by Department and Attrition Rate",
        xlab="Department", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

#jobroles and attrition
jor <- table(peo2$Attrition, peo2$JobRole)
barplot(jor, main="Employee Distribution by JobRole and Attrition Rate",
        xlab="JobRole",
        legend = rownames(jor), beside=TRUE)

#Joblevel
jol <- table(peo2$Attrition,peo2$JobLevel)
barplot(jol, main="Employee Distribution by JobLevel and Attrition Rate",
        xlab = 'JobLevel',
        legend = rownames(jol), beside = TRUE)

#Education
edu <- table(peo2$Attrition,peo2$Education)
barplot(edu, main="Employee Distribution by Education and Attrition Rate",
        xlab = 'Education',
        legend = rownames(edu), beside = TRUE)


#logistic
hrlog = glm(peo$Attrition~Age+as.factor(BusinessTravel)+Education+DistanceFromHome+EmployeeCount+ EmployeeNumber ,data = peo)




#Age



agec = cut(peo2$age,breaks, labels = NULL, include.lowest = FALSE,right = TRUE, dig.lab = 10)

#learn
plot_correlation(emp_data, type = 'continuous','Review.Date')
plot_density(emp_data)
#download package "dataexplorer"

emp_data <- peo

install.packages('caTools')
library('caTools')
#Renaming column Age
colnames(emp_data)[1] <- "age"

#Converting Attrition to 0/1 from No/Yes
emp_data$Attrition <- as.numeric(emp_data$Attrition)-1

#Splitting the dataset into training and testing datasets
set.seed(123)
split = sample.split(emp_data, SplitRatio = 0.7)
training_set = subset(emp_data, split == TRUE)
test_set = subset(emp_data, split == FALSE)

model <-  glm(Attrition ~ age+ as.factor(OverTime) + as.factor(StockOptionLevel) + DistanceFromHome + 
                MonthlyIncome + as.factor(JobInvolvement) + YearsAtCompany:PercentSalaryHike + YearsSinceLastPromotion +  
                as.factor(EnvironmentSatisfaction), family = binomial(link = 'logit'), training_set)
summary(model)
emp_pred <- test_set
emp_pred$prob <- predict.glm(model, test_set, type="response")
emp_pred$prediction <- ifelse(emp_pred$prob >= 0.2, 1, 0)
#AIC: 875.83, MAPE: 13
confusionMatrix(as.factor(emp_pred$prediction), as.factor(emp_pred$Attrition))


model <-  glm(Attrition ~ age+ as.factor(OverTime) + as.factor(StockOptionLevel) + DistanceFromHome + 
                MonthlyIncome + as.factor(JobInvolvement) + YearsAtCompany:PercentSalaryHike + YearsSinceLastPromotion +  
                as.factor(EnvironmentSatisfaction), family = binomial(link = 'logit'), training_set)
summary(model)
emp_pred <- test_set
emp_pred$prob <- predict.glm(model, test_set, type="response")
emp_pred$prediction <- ifelse(emp_pred$prob >= 0.14, 1, 0)

confusionMatrix(as.factor(emp_pred$prediction), as.factor(emp_pred$Attrition))

look <- subset(peo, peo$Education == 1)
look
look$Department
summary(peo$MonthlyIncome)
summary(peo$Age)
type(peo$Age)

