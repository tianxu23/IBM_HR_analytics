setwd("C:/Users/31221/Desktop/Term1/Applied Statis/final_pro")
emp_data <- read.csv("Employee-Attrition.csv")

#Basic summary of raw data
summary(emp_data)

#####DATA CLEANING AND EDA#####

install.packages('DataExplorer') 
library(DataExplorer)

#Some variables whose value is not changing, so standard deviation of those variables is Zero
#So it is not Singficant for analysis
#The variables are Employee count, Over18, StandardHours
#Employee Number also not significant; therefore, we can remove it as well
emp_data <- emp_data[,-c(9,10,22,27)]

#Renaming column Age
colnames(emp_data)[1] <- "age"

#Plotting the correlation and density plots
plot_correlation(emp_data, type = 'continuous','Review.Date')
plot_density(emp_data)

#Job involvement through bar chart
ggplot(emp_data, aes(x=JobInvolvement)) + geom_bar() + facet_wrap(~Attrition)

#Monthly Income through histogram
ggplot(emp_data, aes(x=MonthlyIncome)) + geom_histogram(binwidth = 5000) + facet_wrap(~Attrition)

#Monthly Income through Box plot
ggplot(emp_data, aes(x=Attrition, y = MonthlyIncome)) + geom_boxplot()

#People who attrite are younger, 50% of them attrite between late 20 and late 30
#This makes sense because people usually switch jobs at this time in life
ggplot(emp_data, aes(x=Attrition, y=age)) +
  geom_boxplot(aes(x=Attrition, y=age)) +
  ggtitle("Age Distribution According to Attrition Rate")+
geom_text(label=x)

#Graph: Highest attrition rate in the frequent travellers
frequency <- table(emp_data$Attrition, emp_data$BusinessTravel)
graph1 <- barplot(frequency, main="Employee Distribution by BusinessTravel and Attrition Rate",
                  xlab="Department", col=c("darkblue","red"), beside=TRUE,
                  legend = rownames(counts),args.legend = list(x = "topleft", bty = "n"))

#Graph of Department vs. Attribution: Higher attrition rate in HR and sales, lower in R&D 
counts <- table(emp_data$Attrition, emp_data$Department)
graph1<-barplot(counts, main="Employee Distribution by Department and Attrition Rate",
                xlab="Department", col=c("darkblue","red"),
                legend = rownames(counts), beside=TRUE)

#Age and attrition
hist(emp_data$age, label=TRUE, col="blue", ylim=c(0,800))

#Attrition by Department
counts <- table(emp_data$Attrition, emp_data$Department)
barplot(counts, main="Employee Distribution by Department and Attrition Rate",
        xlab="Department", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

#Jobroles and attrition
jor <- table(emp_data$Attrition, emp_data$JobRole)
barplot(jor, main="Employee Distribution by JobRole and Attrition Rate",
        xlab="JobRole",
        legend = rownames(jor), beside=TRUE)

#Joblevel and Attrition
jol <- table(emp_data$Attrition, emp_data$JobLevel)
barplot(jol, main="Employee Distribution by JobLevel and Attrition Rate",
        xlab = 'JobLevel',
        legend = rownames(jol), beside = TRUE)

#Education and 
edu <- table(emp_data$Attrition, emp_data$Education)
barplot(edu, main="Employee Distribution by Education and Attrition Rate",
        xlab = 'Education',
        legend = rownames(edu), beside = TRUE)

#####DATA PREPARATION#####

#Converting Attrition to 0/1 from No/Yes
emp_data$Attrition <- as.numeric(emp_data$Attrition)-1

#Splitting the dataset into training and testing datasets
install.packages("caTools")
library(caTools)
set.seed(123)
split = sample.split(emp_data, SplitRatio = 0.7)
training_set = subset(emp_data, split == TRUE)
test_set = subset(emp_data, split == FALSE)

#####MODEL BUILDING#####

#Linear model using the selected variables
model_linear <- lm(Attrition ~ as.factor(OverTime)+as.factor(StockOptionLevel) + DistanceFromHome + 
               MonthlyIncome + as.factor(JobInvolvement) +YearsAtCompany:PercentSalaryHike + YearsSinceLastPromotion+ 
               as.factor(EnvironmentSatisfaction), training_set)

#Logistic regression using the final selected variables
model_log <-  glm(Attrition ~ age+ as.factor(OverTime) + as.factor(StockOptionLevel) + DistanceFromHome + 
                MonthlyIncome + as.factor(JobInvolvement) + YearsAtCompany:PercentSalaryHike + YearsSinceLastPromotion +  
                as.factor(EnvironmentSatisfaction), family = binomial(link = 'logit'), training_set)

#Getting the model statistics
summary(model_log)

#Predicting Attrition using the developed model on the test dataset
emp_pred <- test_set
emp_pred$prob <- predict.glm(model_log, test_set, type="response")

#If the probability estimate is greater than 0.14 then 1 else 0
emp_pred$prediction <- ifelse(emp_pred$prob >= 0.14, 1, 0)

#Dividing the results into 3 categories: Will not attrite, Can attrite and Will attrite
emp_pred$attrite_pred <- ifelse(emp_pred$prob < 0.14, "Will not attrite", ifelse(emp_pred$prob >= 0.14 & emp_pred$prob < 0.5, "Can attrite", "Will Attrite"))

#Showing the prediction split for 'Attrition' for each category
table(emp_pred$attrite_pred)

#Making the Confusion Matrix
install.packages("caret")
library(caret)
confusionMatrix(as.factor(emp_pred$prediction), as.factor(emp_pred$Attrition))


