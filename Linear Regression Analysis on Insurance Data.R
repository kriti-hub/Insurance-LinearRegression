#Lab 1:  Assignment1 Problem 2 Linear regression
#Class:  DSC 424
#Name:  Kriti Srivastava
#Date:   9/24/20202



#Libraries
library(Hmisc) #Describe Function
library(psych) #Multiple Functions for Statistics and Multivariate Analysis
library(GGally) #ggpairs Function
library(ggplot2) #ggplot2 Functions
library(vioplot) #Violin Plot Function
library(corrplot) #Plot Correlations
library(DescTools) #VIF Function
library(leaps) #Best Set Linear Regression Functions

#Set Working Directory
#--------------------------------
setwd("C:/Users/Kriti/Downloads/")

#Read in Datasets
#-----------------------------
insuranceDataset <- read.csv(file="insurance_dataset.csv", header=TRUE, sep=",")
head(insuranceDataset)

#Check Sample Size and Number of Variables
#------------------------------------------------------
dim(insuranceDataset)


#Show the structure of the data
#-------------------------------------
str(insuranceDataset)


#Show names of the variables
#-------------------------------
names(insuranceDataset)

#Check for Missing Values (i.e. NAs)
#--------------------------------------------
#For All Variables
sum(is.na(insuranceDataset))

#Create new subsets of data
#------------------------------------------
insuranceDataset2 <- insuranceDataset[,c(1,3:5,7,9,10)]
insuranceDataset2

head(insuranceDataset2)
dim(insuranceDataset2)
names(insuranceDataset2)
str(insuranceDataset2)

# adding dummy variables for region
#-------------------------------------------
insuranceDataset2$dummy_NW <- ifelse(insuranceDataset2$region_num == 2 , 1,0)
insuranceDataset2$dummy_SE <- ifelse(insuranceDataset2$region_num == 3 , 1,0)
insuranceDataset2$dummy_SW <- ifelse(insuranceDataset2$region_num == 4, 1,0)

#Create new subsets of data
#------------------------------------------
insuranceDataset3 <- insuranceDataset2[,c(7,1:5,8:10)]
head(insuranceDataset3)
dim(insuranceDataset3)
names(insuranceDataset3)
str(insuranceDataset3)

#Statistical description
#------------------------------------------
library(psych)
describe(insuranceDataset3)

summary(insuranceDataset3$expenses)

#library(GGally)
#GGpairs
ggpairs(insuranceDataset2)

#Boxplots
p2<-ggplot(insuranceDataset, aes(y=expenses, x=sex)) +
  geom_boxplot(col="blue") +
  labs(
    title="Sex Distribution",
    y="Expenses", x="Gender")
p2


#Histograms of Expenses
#GGPlot Histogram with Normal Curve
----------------------------------------
library(ggplot2)
x <- seq(0, 75000, length.out=1000)
df <- with(insuranceDataset, data.frame(x = x, y = dnorm(x, mean(bmi), sd(bmi))))



p <-ggplot(insuranceDataset) + geom_histogram(aes(x=expenses, y = ..density..), binwidth=1000, colour="black", fill="blue") +
  labs(
    title="Expenses",
    y="Density",
    x="Expenses")

p

p1 = p+geom_line(data = df, aes(x = x, y = y), color = "red")
p1


#Histograms of BMI
#GGPlot Histogram with Normal Curve
library(ggplot2)
x <- seq(0, 80, length.out=50)
df <- with(insuranceDataset, data.frame(x = x, y = dnorm(x, mean(bmi), sd(bmi))))



p <-ggplot(insuranceDataset) + geom_histogram(aes(x=bmi, y = ..density..), binwidth= 5, colour="black", fill="blue") +
  labs(
    title="BMI",
    y="Density",
    x="BMI")

p

p1 = p+geom_line(data = df, aes(x = x, y = y), color = "red")
p1



#Check for Multicollinearity with Correlations
#------------------------------------------------------

M<-cor(insuranceDataset3, method="spearman")
M

corrplot(cor(insuranceDataset3,method="spearman"), method = "number")


#Create Initial Linear Regression Model with Enter Method
#--------------------------------------------------------------
model1 <- lm(expenses ~ ., data=insuranceDataset3)
model1

summary(model1)
#Check VIF
VIF(model1)

#Transforming expenses:
#------------------------------------
insuranceDataset3$ln_expenses <- log(insuranceDataset3$expenses)
head(insuranceDataset3)

# Creating new subset by removing expenses
#---------------------------------------------------------

insuranceDataset4 <-   insuranceDataset3[,c(10,2:9)]
head(insuranceDataset4)

#Check for Multicollinearity with Correlations after transformation
#----------------------------------------------------------------
M<-cor(insuranceDataset4, method="spearman")
M

corrplot(cor(insuranceDataset4,method="spearman"), method = "number")



#Histogram of ln_expenses
#-----------------------------------

library(ggplot2)
x <- seq(0, 13, length.out=100)
df <- with(insuranceDataset3, data.frame(x = x, y = dnorm(x, mean(ln_expenses), sd(ln_expenses))))



p <-ggplot(insuranceDataset3) + geom_histogram(aes(x=ln_expenses, y = ..density..), binwidth=.5, colour="black", fill="blue") +
  labs(
    title="Histogram of log(expenses)",
    y="Density",
    x="ln_Expenses")

p

p1 = p+geom_line(data = df, aes(x = x, y = y), color = "red")
p1

#Create  Linear Regression Model after Expenses Transformation with Enter Method
#--------------------------------------------------------------
model2 <- lm(ln_expenses ~ ., data=insuranceDataset4)
model2

#Summary of model2
#-------------------------------
summary(model2)
#Check VIF
VIF(model2)

#Create new subsets of data after transforming expenses and removing dummy_NW
#-----------------------------------------------------------------------------------
head(insuranceDataset4)
insuranceDataset5 <- insuranceDataset4[,c(1:6,8:9)]
head(insuranceDataset5)

#Create  Linear Regression Model after Expenses Transformation, removing dummy_NW with Enter Method
#------------------------------------------------------------------------------------------------------
model3 <- lm(ln_expenses ~ ., data=insuranceDataset5)
model3

#Summary of model3
#-------------------------------
summary(model3)
#Check VIF
VIF(model3)

#Creating Automatic Models
#---------------------------------------
null = lm(ln_expenses ~ 1, data=insuranceDataset5)
null

full = lm(ln_expenses ~ ., data=insuranceDataset5)
full


#Forward Regression
#-------------------------------------
Forward = step(null, scope = list(lower=null, upper=full), direction="forward")
summary(Forward)


#library(GGally)
#GGpairs
ggpairs(insuranceDataset2)

#Boxplots
p2<-ggplot(insuranceDataset, aes(y=expenses, x=sex, fill=region )) +
  facet_wrap(~region)+ 
  geom_boxplot() +
  theme(axis.title.x = element_text(face = "bold", size = 15))+
  theme(axis.title.y = element_text(face = "bold",size = 15))+
  ggtitle("Boxplot of men and wonem expenses over region")
  
p2



#Violin Plots
library(vioplot)
x1 <- insuranceDataset$expenses[insuranceDataset$region=="northeast"]
x2 <- insuranceDataset$expenses[insuranceDataset$region=="northwest"]
x3 <- insuranceDataset$expenses[insuranceDataset$region=="southeast"]
x4 <- insuranceDataset$expenses[insuranceDataset$region=="southwest"]

vioplot(x1, x2, x3, x4, names=c("northeast", "northwest", "southeast", "southwest"), 
        col=c("blue","green", "yellow","pink"))
title("Violin Plots of Insurance Expenses by Region")
ylab("Expenses")




