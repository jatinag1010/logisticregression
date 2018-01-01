#############################HR ANALYTICS CASE STUD###################
################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
################################################################

### Business Understanding:

# XYZ company is facing huge attrition which is impacting its business
# the company has maintained a database containing information about 4000 employees in 5 files
# Employee survey data has employee feedback on work environmnt,Job Satisfaction and Work life balance
# General file containes all the information related to past and current employee like : age , attrition, Education,department and history of employement
# Manager survey file contains feedback about the employee by his reporting manager
# In time & Out time : contains information about employee attendance , leaves 

## AIM:

# Idenitifying important variable for this huge attrirtion  
# How to fiz the root cause of the attirtion. 


################################################################
### Data Understanding

# Install and Load the required packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(knitr)
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)


# Loading 5 files
emp_data<- read.csv("employee_survey_data.csv", stringsAsFactors = F)
general_data<- read.csv("general_data.csv", stringsAsFactors = F)
manager_data<- read.csv("manager_survey_data.csv", stringsAsFactors = F)
in_time<- read.csv("in_time.csv", stringsAsFactors = F)
out_time<- read.csv("out_time.csv", stringsAsFactors = F)


# in_time and out_time have missing columns name , this columns is employee_id

colnames(in_time)[1] <- "EmployeeID"
colnames(out_time)[1] <- "EmployeeID"

# Collate the data together in one single file
length(unique(emp_data$EmployeeID))   # 4410, confirming EmployeeID is key 
length(unique(general_data$EmployeeID)) # 4410, confirming EmployeeID is key
length(unique(in_time$EmployeeID)) # 4410, confirming EmployeeID is key
length(unique(manager_data$EmployeeID)) # 4410, confirming EmployeeID is key
length(unique(out_time$EmployeeID))# 4410, confirming EmployeeID is key


# Merging employee_survey data , manager_survey_data , general data :

setdiff(emp_data$EmployeeID,general_data$EmployeeID) # Identical customerID across these datasets
setdiff(emp_data$EmployeeID,manager_data$EmployeeID) # Identical customerID across these datasets

hrdata<- merge(emp_data,general_data, by="EmployeeID", all = F)
hrdata<- merge(hrdata,manager_data, by="EmployeeID", all = F)
View(hrdata)

#Data cleaning

#Cleanning in_time and out_time data :
  
###################Converting the time to the Date Format####################################################
in_time[,2:ncol(in_time)]<-sapply(in_time[,-1],function(x){as.POSIXlt(x,format="%Y-%m-%d %H:%M:%S")})
out_time[,2:ncol(out_time)]<-sapply(out_time[,-1],function(x){as.POSIXlt(x,format="%Y-%m-%d %H:%M:%S")})
#############################################################################################################
###############################Calculating the difference between the out time and in time###################

diff_time<-out_time[,-1]-in_time[,-1]
diff_time<-sapply(diff_time,function(x){round(as.double(x,na.rm=TRUE))})
#Calculating the avrage time Spent in office for each employee
mean<-rowMeans(diff_time,na.rm = TRUE)
hrdata<-cbind(hrdata,mean)

#Data Cleaning
#########################################################################################
sapply(hrdata,function(x){sum(is.na(x))})


#There are 19 NA values in this attribute which need to be imputed
#We choose to follow the below logic
#If NumberCompaniesWorked=0 if the TotalWorkingYears-YearsAtCompany=0
#If NumberCompaniesWorked=1 if the TotalWorkingYears-YearsAtCompany>0
#NA values are present in column Environment satisfaction,JobSatisfaction,WorkLifeBalance,NumCompaniesWorked,TotalWorkingYears
hrdata[which(is.na(hrdata$NumCompaniesWorked)),"NumCompaniesWorked"]<-ifelse(hrdata[which(is.na(hrdata$NumCompaniesWorked)),"TotalWorkingYears"]-hrdata[which(is.na(hrdata$NumCompaniesWorked)),"YearsAtCompany"],1,0)


#There are 9 NA values
#Imputing these values with totalworkingyears=NumofCompanies*YearsAtCompany
na_rows<-which(is.na(hrdata$TotalWorkingYears))
hrdata[na_rows,"TotalWorkingYears"]<-hrdata[na_rows,"YearsAtCompany"]*hrdata[na_rows,"NumCompaniesWorked"]



#Replacing the NA value in Environment satisfaction with modal value of the column which is 3
hrdata[which(is.na(hrdata$EnvironmentSatisfaction)),"EnvironmentSatisfaction"]<-3

#Replacing the NA value in JobSatisfaction with modal value of the column which is 4
hrdata[which(is.na(hrdata$JobSatisfaction)),"JobSatisfaction"]<-4

#Replacing the NA value in WorkLifeBalance with modal value of the column which is 3
hrdata[which(is.na(hrdata$WorkLifeBalance)),"WorkLifeBalance"]<-3
#Removing Outliers
########################################################################################

# Age Variable:

Boxplot(hrdata$Age) # No outliers

# DistanceFromHome
Boxplot(hrdata$DistanceFromHome) # No outliers


# MonthlyIncome

Boxplot(hrdata$MonthlyIncome)

quantile(hrdata$MonthlyIncome,seq(0,1,0.01)) # # observe outliers between 91% and 100%
#There are outliers in the Monthly income data.
hrdata$MonthlyIncome[which(hrdata$MonthlyIncome>137757)]<-137756

# PercentSalaryHike:

Boxplot(hrdata$PercentSalaryHike) #No Outliers

# YearsAtCompany

Boxplot(hrdata$YearsAtCompany)

quantile(hrdata$YearsAtCompany,seq(0,1,0.01)) # outliets between 92% to 98%

hrdata$YearsAtCompany[which(hrdata$YearsAtCompany>18)]<-17

# YearsSinceLastPromotion

Boxplot(hrdata$YearsSinceLastPromotion)

quantile(hrdata$YearsSinceLastPromotion,seq(0,1,0.01))

hrdata$YearsSinceLastPromotion[which(hrdata$YearsSinceLastPromotion>7)]<-7

# YearsWithCurrManager
Boxplot(hrdata$YearsWithCurrManager)

quantile(hrdata$YearsWithCurrManager,seq(0,1,0.01))

hrdata$YearsWithCurrManager[which(hrdata$YearsWithCurrManager>13)]<-13


# TotalWorkingYears
Boxplot(hrdata$TotalWorkingYears)
quantile(hrdata$TotalWorkingYears,seq(0,1,0.01))
hrdata$TotalWorkingYears[which(hrdata$TotalWorkingYears>28)]<-28





# Data Understanding
########################################################################################################

#Checking the parameters Job Involvement
ggplot(hrdata,aes(x=JobInvolvement,fill=factor(Attrition)))+geom_bar(position="fill")
#Job Involvement #1 and #4 have high attrition rate.

#Checking the parameters Performance Rating
ggplot(hrdata,aes(x=factor(PerformanceRating),fill=factor(Attrition)))+geom_bar(position="fill")
#Performance Rating 4 has higher attrition Rate

#Checking the parameters EnvironmentSatisfaction
ggplot(hrdata,aes(x=factor(EnvironmentSatisfaction),fill=factor(Attrition)))+geom_bar(position="fill")
#1 has a higher attrition rate.It follows a continous decreasing attrition rate with 4 
#having the least attrition rate.

#Checking the parameters Job Satisfaction
ggplot(hrdata,aes(x=factor(JobSatisfaction),fill=factor(Attrition)))+geom_bar(position="fill")
#1 has a higher attrition rate. It follows a continous decreasing attrition rate with 4
#having the least attrition rate.

#Checking the parameters Work Life Balance
ggplot(hrdata,aes(x=factor(WorkLifeBalance),fill=factor(Attrition)))+geom_bar(position="fill")
#WorkLife Balance with 1 and 4 have a higher attrition rate.

#We will take The ordinal categorical variables
#1. Environment Satisfaction
#2. Job Satisfaction
#3. WorkLife Balance
#4.Job Involvement
#5. Performance Rating as dummy variables because the impact of each value is different so the coeffecient of each
#ordinal categorical value will be different





#Age values has an impact on the Attrition
ggplot(hrdata,aes(x=factor(Attrition),y=Age,fill=factor(Attrition)))+geom_boxplot()

#With Rising age the attrition decreases. The variable has an impact on attrition 
#and should be considered in predicting Attrition


#Checking the levels in Business Travel
summary(as.factor(hrdata$BusinessTravel))
ggplot(hrdata,aes(x=factor(Attrition),fill=factor(BusinessTravel)))+geom_bar(position = "fill" )

# People who have left the company(Attrition=Yes) has higher percentage of people who "Travel_Frequently".


hrdata<-unite(hrdata,Travel_marstatus,BusinessTravel,MaritalStatus,sep="_",remove=FALSE)
ggplot(hrdata,aes(x=factor(Travel_marstatus),fill=factor(Attrition)))+geom_bar(position = "fill" )+theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5),legend.position="right")
#As per the Business Understanding, Business Travel can be used to create a Interaction variable
#with Marital Status. People who are married and travels frequently are more likely to have a high 
#attrition rate or #people who are single and travel rarely are likely to have a high attrition rate


ggplot(hrdata,aes(x=Department,fill=factor(Attrition)))+geom_bar(position = "fill" )+theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5),legend.position="right")
#Human Resources Department have a higher attrition rate

#Checking the variable DistanceFromHome
summary(hrdata$DistanceFromHome)
ggplot(hrdata,aes(x=factor(Attrition),y=DistanceFromHome))+geom_boxplot()
#Higher values of Distance seems to create more chances of Attrition for a employee
#Any outlier treatment is not required as we do not see any data point outside the whisker

#If we bin the variable into different bins 10-12,12-14,14-16 and check if there is a trend which 
#we can see in the attrition of the employee
ggplot(hrdata,aes(x=DistanceFromHome,fill=factor(Attrition)))+geom_histogram(binwidth = 2,position = "fill")
#We do not see any trend in Attrition based on DistancefromHome apart from sudden spikes in 10-12,13-15
#These spikes cannot be explained by business Understanding so we leave the variable as untreated and 
#continuous

#Checking the variable Education
summary(as.factor(hrdata$Education))
ggplot(hrdata,aes(x=Education,fill=factor(Attrition)))+geom_bar(position = "fill")
#Education category 2 has a higher attrition rate



#The attrition rate is higher for Education field like "HumanResources". 
ggplot(hrdata,aes(x=EducationField,fill=factor(Attrition)))+geom_bar(position="fill")+theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5),legend.position="right")


#Checking the variable Gender
summary(as.factor(hrdata$Gender))
#There are no NA values
ggplot(hrdata,aes(x=Gender,fill=factor(Attrition)))+geom_bar(position="fill")
#Checking if the gender and distance from home makes a significant impact on the attrition rate
ggplot(hrdata,aes(x=factor(Gender),y=DistanceFromHome,fill=factor(Attrition)))+geom_boxplot()

#Relative Percentage for male and female attrition is the same therefore not creating a 
#Interaction variable with the gender and distance from home

#Creating levels (0,1) based on the gender
hrdata$Gender<-as.factor(hrdata$Gender)
levels(hrdata$Gender)<-c(1,0)
hrdata$Gender<- as.numeric(levels(hrdata$Gender))[hrdata$Gender]
View(hrdata)

#Checking the factor Job level
ggplot(hrdata,aes(x=factor(JobLevel),fill=factor(Attrition)))+geom_bar(position="fill")
#Job level 2 and Job level 4 have a higher attrition rate than other job levels

#Checking the variable Job Role
ggplot(hrdata,aes(x=factor(JobRole),fill=factor(Attrition)))+geom_bar(position="fill")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position="right")
#Reasearch Director and Research scientist have a higher attrition rate than other Job ROles

#Checking the variable Job Role and Job Level
ggplot(hrdata,aes(x=factor(JobLevel),fill=factor(Attrition)))+geom_bar()+facet_wrap(~JobRole)
#With the JobRole and JobLevel the attrition rate changes
#We can create a Interaction variable which is a combination of job Role and Job Level to study
#the impact of combination of these variables

hrdata<-unite(hrdata,Role.Level,JobRole,JobLevel,sep="_",remove=FALSE)
ggplot(hrdata,aes(x=Role.Level,fill=factor(Attrition)))+geom_bar(position = "fill" )+theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5),legend.position="right")


#Checking the variable Marital Status
ggplot(hrdata,aes(x=factor(MaritalStatus),fill=factor(Attrition)))+geom_bar(position="fill")
#SIngle people have more attrition rate as per the data analysis

#Checking the variable Monthly income
summary(hrdata$MonthlyIncome)

ggplot(hrdata,aes(x=factor(Attrition),y=MonthlyIncome))+geom_boxplot()
#Monthly income does not seem to have signficant impact on the Attrition
#Checking the jobRole,joblevel with the monthly income on Attrition Rate
ggplot(hrdata,aes(x=factor(Role.Level),y=MonthlyIncome,fill=factor(Attrition)))+geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5),legend.position="right")

#With the same "Role.level" the attrition Rate differs with the variation in the monthly income

#Checking the variable NumCompaniesWorked


#Mapping the Attrition with the NumCompanies worked in past
ggplot(hrdata,aes(x=factor(NumCompaniesWorked),fill=factor(Attrition)))+geom_bar(position="fill")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5),legend.position="right")

#There is no specific trend between attrition rate and number of companies worked

#Checking the variable PercentSalaryHike with the attrition rate

ggplot(hrdata,aes(x=factor(Attrition),y=PercentSalaryHike))+geom_boxplot()
#With only the variable PercentSalaryHike and attrition rate there is no visible difference

#Now,We want to check the PercentSalaryhike with Role.Level and attrition rate
ggplot(hrdata,aes(x=Role.Level,y=PercentSalaryHike,fill=factor(Attrition)))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5),legend.position="right")

#We can see the impact of percentSalary Hike on Attrition Rate is different for different Role.Level
#For some Role.Level the increase of percentSalary Hike has increased the attrition rate
#For some other Role.Level the increase of percentSalary.Hike has decreased the attrition rate

#We can think of segmenting the data with respect to the Role.Level so that we get better 
#predictability in the model

#Checking the variable Stock option level
ggplot(hrdata,aes(x=StockOptionLevel,fill=factor(Attrition)))+geom_bar(position="fill")
#There is no clear trend of Stockoption level with the attrition rate
#Stock option level is not an absolute value. It is an ordered categorical variable

#Checking the impact on Attrition with respect to Role
ggplot(hrdata,aes(x=factor(StockOptionLevel),fill=factor(Attrition)))+geom_bar(position="fill")+facet_wrap(~JobRole)
#The impact on attrition of the Stock option level with respect to the Job Role is different

#Checking the Stockoption level impact on Attrition with respect to Role.Level
ggplot(hrdata,aes(x=factor(StockOptionLevel),fill=factor(Attrition)))+geom_bar(position="fill")+facet_wrap(~Role.Level)
#The impact on attrition of the Stock option level with respect to the Role.Level is different

#Checking the variable Total working years
summary(hrdata$TotalWorkingYears)

#Checking the attrition rate with respect to the Totalworkingyears
ggplot(hrdata,aes(x=factor(Attrition),y=TotalWorkingYears))+geom_boxplot()
#With the growing number of years of experience the attrition on an average goes down

#Checking the variable TrainingTimesLastYear
sum(is.na(hrdata$TrainingTimesLastYear))
ggplot(hrdata,aes(x=factor(Attrition),y=TrainingTimesLastYear))+geom_boxplot()
#There is no impact on Attrition with respect to the TrainingTimesLastYear

#Checking the variable YearsAtCompany
sum(is.na(hrdata$YearsAtCompany))
ggplot(hrdata,aes(x=factor(Attrition),y=YearsAtCompany))+geom_boxplot()
#With the growing number of years at Company the attrition rate decreases

#Checking the variable YearsSinceLastPromotion
sum(is.na(hrdata$YearsSinceLastPromotion))
ggplot(hrdata,aes(x=factor(Attrition),y=YearsSinceLastPromotion))+geom_boxplot()

#Checking the variable YearsWithCurrManager
ggplot(hrdata,aes(x=factor(Attrition),y=YearsWithCurrManager))+geom_boxplot()

#With more number of Years with CurrManager the Attrition is less

#############################################################################################################
ggplot(hrdata,aes(x=factor(Attrition),y=mean))+geom_boxplot()

#Increase in Mean time of staying in office increases the probability of attrition

#############################################################################################################
#Finding out the leave patterns for each employee
#We want to categorize the leaves in 2 types
#Bucket1
  #Leave is taken on a  consecutive days to weekend i.e Monday or Friday or next to holiday
  # If the leave is taken on a consecutive days like 10th october,11th october,12 th ocotober, then it wall in bucket  1

#Bucket2
  #Leave taken on a weekday other than Monday or Friday
#Based on our business understanding we want to find out if the leave taken on a day
#other than Monday or Friday is contributing to the increase in Attrition
#############################################################################
#Dates on which Holiday was taken by each employee


holiday<-apply(in_time,1,function(x){colnames(in_time)[which(is.na(x))]})
holiday<-sapply(holiday,function(x){gsub("X","",x)})
holiday<-sapply(holiday,function(x){as.POSIXlt(x,format="%Y.%m.%d")})
#Holiday is a list containing the dates on which holiday is taken by each employee
#tmp_df is a temporary data frame. It contains the list of dates of holidays of a single employee
#The Holidays dates are then used to find the weekday. The weekday( 2 or 6) and to find out if the
#leaves are on consecutive days
for(num in 1:nrow(hrdata)){
   tmp_df=as.data.frame(holiday[num])
   tmp_df[,1]<-as.Date(format(tmp_df[,1],"%Y-%m-%d"))
   tmp_df[,2]<-wday(tmp_df[,1])
    for (index in 2:nrow(tmp_df)){
        tmp_df[index,3]<-tmp_df[index,1]-tmp_df[index-1,1]
    }
    tmp_df[1,3]<-0
    for (counter in 1:nrow(tmp_df)){
#We will check if the holiday is taken on a consecutive days
#When the holiday is taken on consecutive day, it might be for personal reason rather than searching for a job      
        if(tmp_df[counter,3]==1) {
        tmp_df[counter,4]=2
        tmp_df[counter-1,4]=2
         }
#We are checking here whether the holiday is on monday or friday. If it is monday or friday then it might
#be for personal reason rather than for searching for job      
      if(tmp_df[counter,2]==2 | tmp_df[counter,2]==6) {
        tmp_df[counter,4]=1
      }
    } 
         hrdata[num,"ConsecutiveLeaves"]<-length(which(tmp_df[,4]==2 |tmp_df[,4]==1))
         hrdata[num,"WeekdayLeaves(Tue/Wed/Thrus)"]<-length(which(is.na(tmp_df[,4])))
         rm(tmp_df)
  }

#Checking the attrition rate with the mean number of working hours   
ggplot(hrdata,aes(x=Attrition,y=mean))+geom_boxplot()
#The people who are working for more hours are having a high attrition rate.
#The company is loosing good employees who are working more as a result of attrition
ggplot(hrdata,aes(x=Attrition,y="WeekdayLeaves(Tue/Wed/Thrus)"))+geom_boxplot()


#We are not binning any continous variable for the 1st model as binning continous variables
#decreases the predictability of the model

#Removing the variables Employee Count,Over18
hrdata<-hrdata[,-c(13,21,23)]

################################################################################################
#We need to create dummy variables now for the categorical variables in the HR Analytics data set
################################################################################################

# creating a dataframe of categorical features

hrdata_cat<- hrdata[,c(2,3,4,7,8,9,11,12,14,15,16,17,21,27,28)]

# converting categorical attributes to factor
hrdata_fact<- data.frame(sapply(hrdata_cat, function(x) factor(x)))
str(hrdata_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(hrdata_fact, 
                            function(x) data.frame(model.matrix(~x,data =hrdata_fact))[,-1]))

# converting target variable telecom from No/Yes character to factorwith levels 0/1 
hrdata$Attrition<- ifelse(hrdata$Attrition=="Yes",1,0)
hrdata_final<-hrdata[,-c(2,3,4,7,8,9,11,12,14,15,16,17,21,27,28)]
hrdata_final<- cbind(hrdata_final,dummies) 

#Scaling the continous variables
hrdata_final[,c(2,4,6,7:16)]<-sapply(hrdata_final[,c(2,4,6,7:16)],function(x){scale(x)})
#######################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(hrdata_final$Attrition, SplitRatio = 0.7)

train = hrdata_final[indices,]

test = hrdata_final[!(indices),]

########################################################################

#Initial model
model_1 = glm(Attrition ~ .-EmployeeID, data = train, family = "binomial")
summary(model_1) 


model_2<- stepAIC(model_1, direction="both")
summary(model_2)

#Removing variable YearsAtCompany because it is not significant and VIF is high

model_3<-glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
      NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
        YearsSinceLastPromotion + YearsWithCurrManager + 
      mean + Travel_marstatus.xNon.Travel_Single + Travel_marstatus.xTravel_Frequently_Divorced + 
      Travel_marstatus.xTravel_Frequently_Married + Travel_marstatus.xTravel_Frequently_Single + 
      Travel_marstatus.xTravel_Rarely_Divorced + Travel_marstatus.xTravel_Rarely_Married + 
      Travel_marstatus.xTravel_Rarely_Single + Department.xResearch...Development + 
      Department.xSales + Education.x3 + Education.x4 + Education.x5 + 
      Role.Level.xHealthcare.Representative_2 + Role.Level.xHealthcare.Representative_4 + 
      Role.Level.xHuman.Resources_1 + Role.Level.xHuman.Resources_2 + 
      Role.Level.xLaboratory.Technician_2 + Role.Level.xLaboratory.Technician_3 + 
      Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 + 
      Role.Level.xManager_3 + Role.Level.xManufacturing.Director_3 + 
      Role.Level.xManufacturing.Director_4 + Role.Level.xManufacturing.Director_5 + 
      Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
      Role.Level.xResearch.Director_3 + Role.Level.xResearch.Scientist_1 + 
      Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
      Role.Level.xSales.Executive_2 + Role.Level.xSales.Executive_3 + 
      Role.Level.xSales.Representative_4 + StockOptionLevel.x1 + 
      EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
      EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
      JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
      WorkLifeBalance.x4 + JobInvolvement.x3 + EducationField.xLife.Sciences, 
    family = "binomial", data = train)
summary(model_3)

#The variable Travel_marstatus.Travel_rarely_married has a high vif but is significant
#With the EDA analysis we know that for this categorical value the attrition rate is very less ,
#To remove multicollonearity we need to remove variables with high VIF

model_4<-glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
               NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + 
               mean + Travel_marstatus.xNon.Travel_Single + Travel_marstatus.xTravel_Frequently_Divorced + 
               Travel_marstatus.xTravel_Frequently_Married + Travel_marstatus.xTravel_Frequently_Single + 
               Travel_marstatus.xTravel_Rarely_Divorced  + Travel_marstatus.xTravel_Rarely_Single + Department.xResearch...Development + 
               Department.xSales + Education.x3 + Education.x4 + Education.x5 + 
               Role.Level.xHealthcare.Representative_2 + Role.Level.xHealthcare.Representative_4 + 
               Role.Level.xHuman.Resources_1 + Role.Level.xHuman.Resources_2 + 
               Role.Level.xLaboratory.Technician_2 + Role.Level.xLaboratory.Technician_3 + 
               Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 + 
               Role.Level.xManager_3 + Role.Level.xManufacturing.Director_3 + 
               Role.Level.xManufacturing.Director_4 + Role.Level.xManufacturing.Director_5 + 
               Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
               Role.Level.xResearch.Director_3 + Role.Level.xResearch.Scientist_1 + 
               Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
               Role.Level.xSales.Executive_2 + Role.Level.xSales.Executive_3 + 
               Role.Level.xSales.Representative_4 + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 + EducationField.xLife.Sciences, 
             family = "binomial", data = train)
summary(model_4)
vif(model_4)
#We can see that after removing the variable #Travel_marstatus.xNon.Travel_Single the VIF of many varaibles
#have come down as below
#1.120260
#Travel_marstatus.xTravel_Frequently_Divorced 
#1.155466 
#Travel_marstatus.xTravel_Frequently_Married 
#1.191957 
#Travel_marstatus.xTravel_Frequently_Single 
#1.295026 
#Travel_marstatus.xTravel_Rarely_Divorced 
#1.206799 
#Travel_marstatus.xTravel_Rarely_Single 
#1.415284 

#Department sales may be overlapping with Role.Level parameter where Sales Role is taken care that is why it must
#be having high VIF.Removing the variable Department sales

model_5<-glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
               NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + 
               mean + Travel_marstatus.xNon.Travel_Single + Travel_marstatus.xTravel_Frequently_Divorced + 
               Travel_marstatus.xTravel_Frequently_Married + Travel_marstatus.xTravel_Frequently_Single + 
               Travel_marstatus.xTravel_Rarely_Divorced  + Travel_marstatus.xTravel_Rarely_Single + Department.xResearch...Development + 
                Education.x3 + Education.x4 + Education.x5 + 
               Role.Level.xHealthcare.Representative_2 + Role.Level.xHealthcare.Representative_4 + 
               Role.Level.xHuman.Resources_1 + Role.Level.xHuman.Resources_2 + 
               Role.Level.xLaboratory.Technician_2 + Role.Level.xLaboratory.Technician_3 + 
               Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 + 
               Role.Level.xManager_3 + Role.Level.xManufacturing.Director_3 + 
               Role.Level.xManufacturing.Director_4 + Role.Level.xManufacturing.Director_5 + 
               Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
               Role.Level.xResearch.Director_3 + Role.Level.xResearch.Scientist_1 + 
               Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
               Role.Level.xSales.Executive_2 + Role.Level.xSales.Executive_3 + 
               Role.Level.xSales.Representative_4 + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 + EducationField.xLife.Sciences, 
             family = "binomial", data = train)
summary(model_5)
vif(model_5)

# The variable "Department.xResearch...Development has become insignificant therefore removing
#it as the last variable for the department parameter
model_6<-glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
               NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + 
               mean + Travel_marstatus.xNon.Travel_Single + Travel_marstatus.xTravel_Frequently_Divorced + 
               Travel_marstatus.xTravel_Frequently_Married + Travel_marstatus.xTravel_Frequently_Single + 
               Travel_marstatus.xTravel_Rarely_Divorced  + Travel_marstatus.xTravel_Rarely_Single +  
               Education.x3 + Education.x4 + Education.x5 + 
               Role.Level.xHealthcare.Representative_2 + Role.Level.xHealthcare.Representative_4 + 
               Role.Level.xHuman.Resources_1 + Role.Level.xHuman.Resources_2 + 
               Role.Level.xLaboratory.Technician_2 + Role.Level.xLaboratory.Technician_3 + 
               Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 + 
               Role.Level.xManager_3 + Role.Level.xManufacturing.Director_3 + 
               Role.Level.xManufacturing.Director_4 + Role.Level.xManufacturing.Director_5 + 
               Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
               Role.Level.xResearch.Director_3 + Role.Level.xResearch.Scientist_1 + 
               Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
               Role.Level.xSales.Executive_2 + Role.Level.xSales.Executive_3 + 
               Role.Level.xSales.Representative_4 + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 + EducationField.xLife.Sciences, 
             family = "binomial", data = train)
summary(model_6)
vif(model_6)
#As seen from the EDA analysis,the variable worklifeBalance has an impact on the attrition rate
#The VIF value is nearly 3 therefore taking decision not to remove it in the next model

#As seen from the EDA analysis, the variable distance from Home has no significant
#impact on the attrition. As the variable "distancefromHome" is not significant we are removing it

model_7<-glm(formula = Attrition ~ Age + MonthlyIncome + 
               NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + 
               mean + Travel_marstatus.xNon.Travel_Single + Travel_marstatus.xTravel_Frequently_Divorced + 
               Travel_marstatus.xTravel_Frequently_Married + Travel_marstatus.xTravel_Frequently_Single + 
               Travel_marstatus.xTravel_Rarely_Divorced  + Travel_marstatus.xTravel_Rarely_Single +  
               Education.x3 + Education.x4 + Education.x5 + 
               Role.Level.xHealthcare.Representative_2 + Role.Level.xHealthcare.Representative_4 + 
               Role.Level.xHuman.Resources_1 + Role.Level.xHuman.Resources_2 + 
               Role.Level.xLaboratory.Technician_2 + Role.Level.xLaboratory.Technician_3 + 
               Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 + 
               Role.Level.xManager_3 + Role.Level.xManufacturing.Director_3 + 
               Role.Level.xManufacturing.Director_4 + Role.Level.xManufacturing.Director_5 + 
               Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
               Role.Level.xResearch.Director_3 + Role.Level.xResearch.Scientist_1 + 
               Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
               Role.Level.xSales.Executive_2 + Role.Level.xSales.Executive_3 + 
               Role.Level.xSales.Representative_4 + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 + EducationField.xLife.Sciences, 
             family = "binomial", data = train)
summary(model_7)
vif(model_7)

#Now checking the coeffecients of each parameter and their significance
#Age has negative coeffecient and is signficant and matches with EDA analysis that with
#rising age probability of Attrition decreases

# MonthlyIncome has negative coeffecient which means with rising Income that the 
# probability of attrition decreases . The  p-value of monthly income is not significant


#Removing the variable Monthly Income
model_8<-glm(formula = Attrition ~ Age  + 
               NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + 
               mean + Travel_marstatus.xNon.Travel_Single + Travel_marstatus.xTravel_Frequently_Divorced + 
               Travel_marstatus.xTravel_Frequently_Married + Travel_marstatus.xTravel_Frequently_Single + 
               Travel_marstatus.xTravel_Rarely_Divorced  + Travel_marstatus.xTravel_Rarely_Single +  
               Education.x3 + Education.x4 + Education.x5 + 
               Role.Level.xHealthcare.Representative_2 + Role.Level.xHealthcare.Representative_4 + 
               Role.Level.xHuman.Resources_1 + Role.Level.xHuman.Resources_2 + 
               Role.Level.xLaboratory.Technician_2 + Role.Level.xLaboratory.Technician_3 + 
               Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 + 
               Role.Level.xManager_3 + Role.Level.xManufacturing.Director_3 + 
               Role.Level.xManufacturing.Director_4 + Role.Level.xManufacturing.Director_5 + 
               Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
               Role.Level.xResearch.Director_3 + Role.Level.xResearch.Scientist_1 + 
               Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
               Role.Level.xSales.Executive_2 + Role.Level.xSales.Executive_3 + 
               Role.Level.xSales.Representative_4 + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 + EducationField.xLife.Sciences, 
             family = "binomial", data = train)
summary(model_8)
vif(model_8)

#Checking the variable NumCompaniesWorked
#It has positive coeffecient which implies that with the increase in the number of 
#companies worked the attrition rate increases.
#Checking the variable TotalWorkingYears
#It has the negative coeffecient. It matches with our EDA that with the increase
#in TotalWorkingYears the attrition decreases
#Checking the variable TrainingTimesLastYear
#It has the negative coeffecient. It matches with our EDA that with the increase
#in TotalWorkingYears the attrition decreases.
#Checking the variable YearsSinceLastPromotion
#It has the positive coeffecient and matches with our EDA that with the increase
#in years since last promotion the attrition increases
#Checking the variable YearswithCurrManager 
#It has positive coefficient and matches with our EDA that with the increase in years
#withCurrent Manager the attrition decreases
#Checking the variable Mean
#It has the positive coeffecient which matches with our EDA that with the increase in
#mean working hours the attrition increases
#Checking the variable Travel_marstatus.xNon.Travel_Single
#It is insignificant variable and matches with EDA that it is not significant
#Removing the variable Travel_marstatus.xNon.Travel_Single

model_9<-glm(formula = Attrition ~ Age  + 
               NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + 
               mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
               Travel_marstatus.xTravel_Frequently_Married + Travel_marstatus.xTravel_Frequently_Single + 
               Travel_marstatus.xTravel_Rarely_Divorced  + Travel_marstatus.xTravel_Rarely_Single +  
               Education.x3 + Education.x4 + Education.x5 + 
               Role.Level.xHealthcare.Representative_2 + Role.Level.xHealthcare.Representative_4 + 
               Role.Level.xHuman.Resources_1 + Role.Level.xHuman.Resources_2 + 
               Role.Level.xLaboratory.Technician_2 + Role.Level.xLaboratory.Technician_3 + 
               Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 + 
               Role.Level.xManager_3 + Role.Level.xManufacturing.Director_3 + 
               Role.Level.xManufacturing.Director_4 + Role.Level.xManufacturing.Director_5 + 
               Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
               Role.Level.xResearch.Director_3 + Role.Level.xResearch.Scientist_1 + 
               Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
               Role.Level.xSales.Executive_2 + Role.Level.xSales.Executive_3 + 
               Role.Level.xSales.Representative_4 + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 + EducationField.xLife.Sciences, 
             family = "binomial", data = train)
summary(model_9)
vif(model_9)

#Checking the variable Travel_marstatus.xTravel_Frequently_Married
#It is insignificant variable and matches with EDA that it is not significant
#Removing the variable Travel_marstatus.xTravel_Frequently_Married
model_10<-glm(formula = Attrition ~ Age  + 
               NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + 
               mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                Travel_marstatus.xTravel_Frequently_Single + 
               Travel_marstatus.xTravel_Rarely_Divorced  + Travel_marstatus.xTravel_Rarely_Single +  
               Education.x3 + Education.x4 + Education.x5 + 
               Role.Level.xHealthcare.Representative_2 + Role.Level.xHealthcare.Representative_4 + 
               Role.Level.xHuman.Resources_1 + Role.Level.xHuman.Resources_2 + 
               Role.Level.xLaboratory.Technician_2 + Role.Level.xLaboratory.Technician_3 + 
               Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 + 
               Role.Level.xManager_3 + Role.Level.xManufacturing.Director_3 + 
               Role.Level.xManufacturing.Director_4 + Role.Level.xManufacturing.Director_5 + 
               Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
               Role.Level.xResearch.Director_3 + Role.Level.xResearch.Scientist_1 + 
               Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
               Role.Level.xSales.Executive_2 + Role.Level.xSales.Executive_3 + 
               Role.Level.xSales.Representative_4 + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 + EducationField.xLife.Sciences, 
             family = "binomial", data = train)
summary(model_10)
vif(model_10)
#Removing the variable Travel_marstatus.xTravel_Rarely_divorced as it is insignificant

model_11<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                Travel_marstatus.xTravel_Frequently_Single + 
                Travel_marstatus.xTravel_Rarely_Single +  
                Education.x3 + Education.x4 + Education.x5 + 
                Role.Level.xHealthcare.Representative_2 + Role.Level.xHealthcare.Representative_4 + 
                Role.Level.xHuman.Resources_1 + Role.Level.xHuman.Resources_2 + 
                Role.Level.xLaboratory.Technician_2 + Role.Level.xLaboratory.Technician_3 + 
                Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 + 
                Role.Level.xManager_3 + Role.Level.xManufacturing.Director_3 + 
                Role.Level.xManufacturing.Director_4 + Role.Level.xManufacturing.Director_5 + 
                Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
                Role.Level.xResearch.Director_3 + Role.Level.xResearch.Scientist_1 + 
                Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
                Role.Level.xSales.Executive_2 + Role.Level.xSales.Executive_3 + 
                Role.Level.xSales.Representative_4 + StockOptionLevel.x1 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + EducationField.xLife.Sciences, 
              family = "binomial", data = train)
summary(model_11)
vif(model_11)

#Removing the variable Education.x3 as it is insignificant and it matches with our EDA 
#analysis
model_12<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                Travel_marstatus.xTravel_Frequently_Single + 
                Travel_marstatus.xTravel_Rarely_Single +  
                Education.x4 + Education.x5 + 
                Role.Level.xHealthcare.Representative_2 + Role.Level.xHealthcare.Representative_4 + 
                Role.Level.xHuman.Resources_1 + Role.Level.xHuman.Resources_2 + 
                Role.Level.xLaboratory.Technician_2 + Role.Level.xLaboratory.Technician_3 + 
                Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 + 
                Role.Level.xManager_3 + Role.Level.xManufacturing.Director_3 + 
                Role.Level.xManufacturing.Director_4 + Role.Level.xManufacturing.Director_5 + 
                Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
                Role.Level.xResearch.Director_3 + Role.Level.xResearch.Scientist_1 + 
                Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
                Role.Level.xSales.Executive_2 + Role.Level.xSales.Executive_3 + 
                Role.Level.xSales.Representative_4 + StockOptionLevel.x1 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + EducationField.xLife.Sciences, 
              family = "binomial", data = train)
summary(model_12)
vif(model_12)
#Removing the variable Education.x4
model_13<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                Travel_marstatus.xTravel_Frequently_Single + 
                Travel_marstatus.xTravel_Rarely_Single +  
                Education.x5 + 
                Role.Level.xHealthcare.Representative_2 + Role.Level.xHealthcare.Representative_4 + 
                Role.Level.xHuman.Resources_1 + Role.Level.xHuman.Resources_2 + 
                Role.Level.xLaboratory.Technician_2 + Role.Level.xLaboratory.Technician_3 + 
                Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 + 
                Role.Level.xManager_3 + Role.Level.xManufacturing.Director_3 + 
                Role.Level.xManufacturing.Director_4 + Role.Level.xManufacturing.Director_5 + 
                Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
                Role.Level.xResearch.Director_3 + Role.Level.xResearch.Scientist_1 + 
                Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
                Role.Level.xSales.Executive_2 + Role.Level.xSales.Executive_3 + 
                Role.Level.xSales.Representative_4 + StockOptionLevel.x1 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + EducationField.xLife.Sciences, 
              family = "binomial", data = train)
summary(model_13)
vif(model_13)
#Removing the variable Education.x5
model_14<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                Travel_marstatus.xTravel_Frequently_Single + 
                Travel_marstatus.xTravel_Rarely_Single +  
                Role.Level.xHealthcare.Representative_2 + Role.Level.xHealthcare.Representative_4 + 
                Role.Level.xHuman.Resources_1 + Role.Level.xHuman.Resources_2 + 
                Role.Level.xLaboratory.Technician_2 + Role.Level.xLaboratory.Technician_3 + 
                Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 + 
                Role.Level.xManager_3 + Role.Level.xManufacturing.Director_3 + 
                Role.Level.xManufacturing.Director_4 + Role.Level.xManufacturing.Director_5 + 
                Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
                Role.Level.xResearch.Director_3 + Role.Level.xResearch.Scientist_1 + 
                Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
                Role.Level.xSales.Executive_2 + Role.Level.xSales.Executive_3 + 
                Role.Level.xSales.Representative_4 + StockOptionLevel.x1 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + EducationField.xLife.Sciences, 
              family = "binomial", data = train)
summary(model_14)
vif(model_14)
#Removing the variable Role.Level.xHealthcare.Representative_4 as its signficance is less than .05
model_15<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                Travel_marstatus.xTravel_Frequently_Single + 
                Travel_marstatus.xTravel_Rarely_Single +  
                Role.Level.xHealthcare.Representative_2  + 
                Role.Level.xHuman.Resources_1 + Role.Level.xHuman.Resources_2 + 
                Role.Level.xLaboratory.Technician_2 + Role.Level.xLaboratory.Technician_3 + 
                Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 + 
                Role.Level.xManager_3 + Role.Level.xManufacturing.Director_3 + 
                Role.Level.xManufacturing.Director_4 + Role.Level.xManufacturing.Director_5 + 
                Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
                Role.Level.xResearch.Director_3 + Role.Level.xResearch.Scientist_1 + 
                Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
                Role.Level.xSales.Executive_2 + Role.Level.xSales.Executive_3 + 
                Role.Level.xSales.Representative_4 + StockOptionLevel.x1 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + EducationField.xLife.Sciences, 
              family = "binomial", data = train)
summary(model_15)
vif(model_15)

#Removing the variable Role.Level.xHuman.Resources_2 as its signficance is less than .05
model_16<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                Travel_marstatus.xTravel_Frequently_Single + 
                Travel_marstatus.xTravel_Rarely_Single +  
                Role.Level.xHealthcare.Representative_2  + 
                Role.Level.xHuman.Resources_1 +  
                Role.Level.xLaboratory.Technician_2 + Role.Level.xLaboratory.Technician_3 + 
                Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 + 
                Role.Level.xManager_3 + Role.Level.xManufacturing.Director_3 + 
                Role.Level.xManufacturing.Director_4 + Role.Level.xManufacturing.Director_5 + 
                Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
                Role.Level.xResearch.Director_3 + Role.Level.xResearch.Scientist_1 + 
                Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
                Role.Level.xSales.Executive_2 + Role.Level.xSales.Executive_3 + 
                Role.Level.xSales.Representative_4 + StockOptionLevel.x1 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + EducationField.xLife.Sciences, 
              family = "binomial", data = train)
summary(model_16)
vif(model_16)

#Removing the variable Role.Level.xManager_3 as its signficance is less than .05
model_17<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                Travel_marstatus.xTravel_Frequently_Single + 
                Travel_marstatus.xTravel_Rarely_Single +  
                Role.Level.xHealthcare.Representative_2  + 
                Role.Level.xHuman.Resources_1 +  
                Role.Level.xLaboratory.Technician_2 + Role.Level.xLaboratory.Technician_3 + 
                Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 + 
                Role.Level.xManufacturing.Director_3 + 
                Role.Level.xManufacturing.Director_4 + Role.Level.xManufacturing.Director_5 + 
                Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
                Role.Level.xResearch.Director_3 + Role.Level.xResearch.Scientist_1 + 
                Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
                Role.Level.xSales.Executive_2 + Role.Level.xSales.Executive_3 + 
                Role.Level.xSales.Representative_4 + StockOptionLevel.x1 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + EducationField.xLife.Sciences, 
              family = "binomial", data = train)
summary(model_17)
vif(model_17)

#Removing the variable Role.Level.xManufacturing.Director_3 as its significance is greater than .05
model_18<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                Travel_marstatus.xTravel_Frequently_Single + 
                Travel_marstatus.xTravel_Rarely_Single +  
                Role.Level.xHealthcare.Representative_2  + 
                Role.Level.xHuman.Resources_1 +  
                Role.Level.xLaboratory.Technician_2 + Role.Level.xLaboratory.Technician_3 + 
                Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 +
                Role.Level.xManufacturing.Director_4 + Role.Level.xManufacturing.Director_5 + 
                Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
                Role.Level.xResearch.Director_3 + Role.Level.xResearch.Scientist_1 + 
                Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
                Role.Level.xSales.Executive_2 + Role.Level.xSales.Executive_3 + 
                Role.Level.xSales.Representative_4 + StockOptionLevel.x1 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + EducationField.xLife.Sciences, 
              family = "binomial", data = train)
summary(model_18)
vif(model_18)

#Removing the variable Role.Level.xManufacturing.Director_3 as its significance is greater than .05
model_19<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                Travel_marstatus.xTravel_Frequently_Single + 
                Travel_marstatus.xTravel_Rarely_Single +  
                Role.Level.xHealthcare.Representative_2  + 
                Role.Level.xHuman.Resources_1 +  
                Role.Level.xLaboratory.Technician_2 + Role.Level.xLaboratory.Technician_3 + 
                Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 +
                Role.Level.xManufacturing.Director_4  + 
                Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
                Role.Level.xResearch.Director_3 + Role.Level.xResearch.Scientist_1 + 
                Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
                Role.Level.xSales.Executive_2 + Role.Level.xSales.Executive_3 + 
                Role.Level.xSales.Representative_4 + StockOptionLevel.x1 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + EducationField.xLife.Sciences, 
              family = "binomial", data = train)
summary(model_19)
vif(model_19)

#Removing the variable Role.Level.xResearch.Director_3
model_20<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                Travel_marstatus.xTravel_Frequently_Single + 
                Travel_marstatus.xTravel_Rarely_Single +  
                Role.Level.xHealthcare.Representative_2  + 
                Role.Level.xHuman.Resources_1 +  
                Role.Level.xLaboratory.Technician_2 + Role.Level.xLaboratory.Technician_3 + 
                Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 +
                Role.Level.xManufacturing.Director_4  + 
                Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
                Role.Level.xResearch.Scientist_1 + 
                Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
                Role.Level.xSales.Executive_2 + Role.Level.xSales.Executive_3 + 
                Role.Level.xSales.Representative_4 + StockOptionLevel.x1 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + EducationField.xLife.Sciences, 
              family = "binomial", data = train)
summary(model_20)
vif(model_20)
#Removing the variable Role.Level.xSales.Representative_4 as it is not significant
model_21<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                Travel_marstatus.xTravel_Frequently_Single + 
                Travel_marstatus.xTravel_Rarely_Single +  
                Role.Level.xHealthcare.Representative_2  + 
                Role.Level.xHuman.Resources_1 +  
                Role.Level.xLaboratory.Technician_2 + Role.Level.xLaboratory.Technician_3 + 
                Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 +
                Role.Level.xManufacturing.Director_4  + 
                Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
                Role.Level.xResearch.Scientist_1 + 
                Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
                Role.Level.xSales.Executive_2 + Role.Level.xSales.Executive_3 + 
                StockOptionLevel.x1 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + EducationField.xLife.Sciences, 
              family = "binomial", data = train)
summary(model_21)
vif(model_21)
#Removing the variable StockOptionLevel.x1 as it is not significant
model_22<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                Travel_marstatus.xTravel_Frequently_Single + 
                Travel_marstatus.xTravel_Rarely_Single +  
                Role.Level.xHealthcare.Representative_2  + 
                Role.Level.xHuman.Resources_1 +  
                Role.Level.xLaboratory.Technician_2 + Role.Level.xLaboratory.Technician_3 + 
                Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 +
                Role.Level.xManufacturing.Director_4  + 
                Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
                Role.Level.xResearch.Scientist_1 + 
                Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
                Role.Level.xSales.Executive_2 + Role.Level.xSales.Executive_3 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + EducationField.xLife.Sciences, 
              family = "binomial", data=train)
summary(model_22)
vif(model_22)
#Removing the variable JobInvolvement.x3 as it is not significant
model_23<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                Travel_marstatus.xTravel_Frequently_Single + 
                Travel_marstatus.xTravel_Rarely_Single +  
                Role.Level.xHealthcare.Representative_2  + 
                Role.Level.xHuman.Resources_1 +  
                Role.Level.xLaboratory.Technician_2 + Role.Level.xLaboratory.Technician_3 + 
                Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 +
                Role.Level.xManufacturing.Director_4  + 
                Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
                Role.Level.xResearch.Scientist_1 + 
                Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
                Role.Level.xSales.Executive_2 + Role.Level.xSales.Executive_3 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4  + EducationField.xLife.Sciences, 
              family = "binomial", data=train)
summary(model_23)
vif(model_23)

#Removing the variable EducationField.xLife.Sciences as it is not significant
model_24<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                Travel_marstatus.xTravel_Frequently_Single + 
                Travel_marstatus.xTravel_Rarely_Single +  
                Role.Level.xHealthcare.Representative_2  + 
                Role.Level.xHuman.Resources_1 +  
                Role.Level.xLaboratory.Technician_2 + Role.Level.xLaboratory.Technician_3 + 
                Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 +
                Role.Level.xManufacturing.Director_4  + 
                Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
                Role.Level.xResearch.Scientist_1 + 
                Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
                Role.Level.xSales.Executive_2 + Role.Level.xSales.Executive_3 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4  , 
              family = "binomial", data=train)
summary(model_24)
vif(model_24)

#Removing the variable Role.Level.xHuman.Resources_1 as it is not significant
model_25<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                Travel_marstatus.xTravel_Frequently_Single + 
                Travel_marstatus.xTravel_Rarely_Single +  
                Role.Level.xHealthcare.Representative_2  + 
                Role.Level.xLaboratory.Technician_2 + Role.Level.xLaboratory.Technician_3 + 
                Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 +
                Role.Level.xManufacturing.Director_4  + 
                Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
                Role.Level.xResearch.Scientist_1 + 
                Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
                Role.Level.xSales.Executive_2 + Role.Level.xSales.Executive_3 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4  , 
              family = "binomial", data=train)
summary(model_25)
vif(model_25)

#Removing the variable Role.Level.xHealthcare.Representative_2  as it is not signifcant

model_26<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                Travel_marstatus.xTravel_Frequently_Single + 
                Travel_marstatus.xTravel_Rarely_Single +  
                Role.Level.xLaboratory.Technician_2 + Role.Level.xLaboratory.Technician_3 + 
                Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 +
                Role.Level.xManufacturing.Director_4  + 
                Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
                Role.Level.xResearch.Scientist_1 + 
                Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
                Role.Level.xSales.Executive_2 + Role.Level.xSales.Executive_3 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4  , 
              family = "binomial", data=train)
summary(model_26)
vif(model_26)

#Removing the variable Role.Level.xSales.Executive_3
model_27<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                Travel_marstatus.xTravel_Frequently_Single + 
                Travel_marstatus.xTravel_Rarely_Single +  
                Role.Level.xLaboratory.Technician_2 + Role.Level.xLaboratory.Technician_3 + 
                Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 +
                Role.Level.xManufacturing.Director_4  + 
                Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
                Role.Level.xResearch.Scientist_1 + 
                Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
                Role.Level.xSales.Executive_2 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4  , 
              family = "binomial", data=train)
summary(model_27)
vif(model_27)
# Removing the variable Role.Level.xLaboratory.Technician_2
model_28<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                Travel_marstatus.xTravel_Frequently_Single + 
                Travel_marstatus.xTravel_Rarely_Single +Role.Level.xLaboratory.Technician_3 + 
                Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 +
                Role.Level.xManufacturing.Director_4  + 
                Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
                Role.Level.xResearch.Scientist_1 + 
                Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
                Role.Level.xSales.Executive_2 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4  , 
              family = "binomial", data=train)
summary(model_28)
vif(model_28)

# Removing the variable Role.Level.xLaboratory.Technician_3
model_29<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                Travel_marstatus.xTravel_Frequently_Single + 
                Travel_marstatus.xTravel_Rarely_Single  + 
                Role.Level.xLaboratory.Technician_4 + Role.Level.xManager_2 +
                Role.Level.xManufacturing.Director_4  + 
                Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
                Role.Level.xResearch.Scientist_1 + 
                Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
                Role.Level.xSales.Executive_2 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4  , 
              family = "binomial", data=train)
summary(model_29)
vif(model_29)
# Removing the variable Role.Level.xLaboratory.Technician_4
model_30<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                Travel_marstatus.xTravel_Frequently_Single + 
                Travel_marstatus.xTravel_Rarely_Single  + 
                Role.Level.xManager_2 +
                Role.Level.xManufacturing.Director_4  + 
                Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
                Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
                Role.Level.xSales.Executive_2 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4  , 
              family = "binomial", data=train)
summary(model_30)
vif(model_30)

#Removing the variable Role.Level.xManager_2 based on the significance
model_31<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                Travel_marstatus.xTravel_Frequently_Single + 
                Travel_marstatus.xTravel_Rarely_Single  + 
                Role.Level.xManufacturing.Director_4  + 
                Role.Level.xResearch.Director_1 + Role.Level.xResearch.Director_2 + 
                Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
                Role.Level.xSales.Executive_2 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4  , 
              family = "binomial", data=train)
summary(model_31)
vif(model_31)

#Removing the variable Role.Level.xResearch.Director_2  based on the significance
model_32<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                Travel_marstatus.xTravel_Frequently_Single + 
                Travel_marstatus.xTravel_Rarely_Single  + 
                Role.Level.xManufacturing.Director_4  + Role.Level.xResearch.Director_1 +  
                Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
                Role.Level.xSales.Executive_2 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4  , 
              family = "binomial", data=train)
summary(model_32)
vif(model_32)



#Removing the variable Role.Level.xManufacturing.Director_4  based on significance
model_33<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                Travel_marstatus.xTravel_Frequently_Single + 
                Travel_marstatus.xTravel_Rarely_Single  + Role.Level.xResearch.Director_1 +  
                Role.Level.xResearch.Scientist_2 + Role.Level.xSales.Executive_1 + 
                Role.Level.xSales.Executive_2 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4  , 
              family = "binomial", data=train)
summary(model_33)
vif(model_33)


#Removing the variable Role.Level.xResearch.Scientist_2  based on significance
model_34<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                Travel_marstatus.xTravel_Frequently_Single + 
                Travel_marstatus.xTravel_Rarely_Single  + Role.Level.xResearch.Director_1 +  
                Role.Level.xSales.Executive_1 + 
                Role.Level.xSales.Executive_2 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4  , 
              family = "binomial", data=train)
summary(model_34)
vif(model_34)
#Removing the variable Role.Level.xSales.Executive_1  based on significance
model_35<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                Travel_marstatus.xTravel_Frequently_Single + 
                Travel_marstatus.xTravel_Rarely_Single  + Role.Level.xResearch.Director_1 +  
                Role.Level.xSales.Executive_2 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4  , 
              family = "binomial", data=train)
summary(model_35)
vif(model_35)
#We can see the 2 variables Travel_marstatus.xTravel_Frequently_Single & Travel_marstatus.xTravel_Rarely_Single
#which means if the marital status is single then the attrition rate will be more
#Removing the varaibles Travel_marstatus.xTravel_Frequently_Single & Travel_marstatus.xTravel_Rarely_Single and 
#Adding a variable marital status as single
model_36<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                MaritalStatus.xSingle + Role.Level.xResearch.Director_1 +  
                Role.Level.xSales.Executive_2 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4  , 
              family = "binomial", data=train)
summary(model_36)
vif(model_36)

#Removing the variables Role.Level.xSales.Executive_2 based on the significance

model_37<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                MaritalStatus.xSingle + Role.Level.xResearch.Director_1 +  
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3+WorkLifeBalance.x4 , 
              family = "binomial", data=train)
summary(model_37)
vif(model_37)


#Removing the variables WorkLifeBalance.x4 based on the significance

model_38<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                MaritalStatus.xSingle + Role.Level.xResearch.Director_1 +  
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 , 
              family = "binomial", data=train)
summary(model_38)
vif(model_38)

#Removing the variables WorkLifeBalance.x2 based on the significance
model_39<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                MaritalStatus.xSingle + Role.Level.xResearch.Director_1 +  
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4  + WorkLifeBalance.x3 , 
              family = "binomial", data=train)
summary(model_39)
vif(model_39)

#Removing the variables WorkLifeBalance.x3 based on the significance
model_40<-glm(formula = Attrition ~ Age  + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                mean  + Travel_marstatus.xTravel_Frequently_Divorced + 
                MaritalStatus.xSingle + Role.Level.xResearch.Director_1 +  
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 , 
              family = "binomial", data=train)
summary(model_40)
vif(model_40)

final_model<-model_40
#################################################################################
#Finding out the sensitivity,specificity and accuracy for the model
test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])
test_pred_attrition <- factor(ifelse(test_pred >= 0.20, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))
table(test_actual_attrition,test_pred_attrition)
test_conf<- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

#########################################################################################
# Let's Choose the cutoff value. 

# Let's find out the optimal probability cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.05)]

# If we choose a cutoff value of 0.162 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.162, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test)
#The optimal value of cutoff to maximize the value of specificity,senstivity and accuracy
# is .162
#The value of accuracy at the optimal value 0.72
#The value of sensitivity at the optimal value 0.75
#The value of specificity at the optimal value 0.71




##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test1<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
#Plottting the gain chart
ggplot(attrition_decile,aes(x=factor(bucket),y=Cumresp,group=1))+geom_point(shape=3)+geom_line(color="blue")+geom_text(aes(label=Cumresp))

#########################################################################################
#The company wants to zero in on the employees who have probability of leaving the company

#The company wants to reduce the false negatives.Therefore the sensitivity value has to be optimized
#We attempt to choose a cutoff value of .05
test_cutoff_attrition_2 <- factor(ifelse(test_pred >=0.05, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition_2, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc
#0.420257 
sens
#0.9248826 
spec
#0.3234234 

#We need to check the discriminatory power of this model
test_cutoff_attrition_2 <- ifelse(test_cutoff_attrition_2=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition_2, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)

#Max value of KS table =.248 therefore we reject this model

########################################################################################
#Model 3 with cut-off value =0.1
#########################################################################################
#The company wants to zero in on the employees who have probability of leaving the company

#The company wants to reduce the false negatives.Therefore the sensitivity value has to be optimized
#We attempt to choose a cutoff value of .10
test_cutoff_attrition_3 <- factor(ifelse(test_pred >=0.10, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))
conf_final <- confusionMatrix(test_cutoff_attrition_3, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc
#0.60
sens
#0.84
spec
#0.55

#We need to check the discriminatory power of this model
test_cutoff_attrition_3 <- ifelse(test_cutoff_attrition_3=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition_3, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)

#Max value of KS table =.39 therefore we reject this model

# The company can choose a cutoff value between .10 and .16 based on their 
#requirement of seinstivity
########################################################################################














