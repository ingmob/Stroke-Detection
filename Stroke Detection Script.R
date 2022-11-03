#setting download timeout to 100000 seconds
options(timeout=100000)

#INSTALLING/LOADING LIBRARIES
# Note: this process could take a couple of minutes
# Note: this process could take a couple of minutes
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(performanceEstimation)) install.packages("performanceEstimation", repos = "http://cran.us.r-project.org")
if(!require(BBmisc)) install.packages("BBmisc", repos = "http://cran.us.r-project.org")
if(!require(checkmate)) install.packages("checkmate", repos = "http://cran.us.r-project.org")
if(!require(parallelMap)) install.packages("parallelMap", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(readr)
library(ggthemes)
library(kableExtra)
library(knitr)
library(performanceEstimation)
library(BBmisc)
library(checkmate)
library(parallelMap)
#DOWNLOADING ZIPPED FILE CONTAINING DATA FROM KAGGLE
# THE FILE IS ORIGINALLY FROM KAGGLE, BUT I UPLOADED ON MEDIAFIRE BECAUSE THE KAGGLE LINK TIMES OUT
#https://www.kaggle.com/datasets/fedesoriano/stroke-prediction-dataset
url<-"https://download2293.mediafire.com/9bhxmvx7f0eg/j3vopmoi35zp86o/archive.zip"
dl <- tempfile()
download.file(url, dl,mode = "wb")
raw_data<-read_csv(unzip(dl,"healthcare-dataset-stroke-data.csv"),col_names=TRUE)


#Changing column names to all small letters for convenience
colnames(raw_data)<-c("id","gender","age","hypertension","heart_disease","ever_married","work_type","residence_type","avg_glucose_level","bmi","smoking_status","stroke")

#CREATING A DATA FRAME FOR ALL PATIENTS WHO ACTUALLY HAD A STROKE
had_stroke<- raw_data%>%filter(stroke==1)
#ANALYSIS FOR EACH PREDICTOR INCLUDING PLOTS
#ID
# CHECK THAT LENGTH OF UNIQUE ID's is the same as the length of the data
length(unique(raw_data$id))==nrow(raw_data)
#Removing id since it has no bearing on whether a person has stroke or not.
raw_data<-raw_data%>%select(!id)

#GENDER
#CREATING TABLE SHOWING DISTRIBUTION OF GENDERS IN THE DATA
gender_dist<- data.frame(gender=c("Male","Female","Other"),proportion=c(mean(raw_data$gender=="Male")
                                                                        ,mean(raw_data$gender=="Female"),mean(raw_data$gender=="Other")))
gender_dist<-gender_dist%>%mutate(no_of_entries=proportion*nrow(raw_data))
#REMOVING ENTRY WITH gender="Other"
raw_data<-raw_data%>%filter(gender!="Other")
#CONVERTING GENDER TO A FACTOR
raw_data<- raw_data%>%mutate(gender=as.factor(gender))
#DISTRIBUTION OF GENDERS WITH STROKE
gender_with_stroke<- data.frame(gender=c("Male","Female"),proportion=c(mean(had_stroke$gender=="Male"),
                                                                       mean(had_stroke$gender=="Female")))

#AGE
#Fuction to check if entries are whole numbers
is.wholenumber<- function(x){
  x==round(x)
}
#Getting proportion of decimals in age data
prop_decimal_age<-(1-mean(sapply(raw_data$age, is.wholenumber)))*100
#Minimum and Maximum Ages
min_age<- min(raw_data$age)
max_age<- max(raw_data$age)
#Converting age to integer values
raw_data<-raw_data%>%mutate(age=round(age))%>%mutate(age=as.integer(age))
#Minimum, Average and Maximum Stroke victim ages
min_age_stroke<-min(had_stroke$age)
mean_age_stroke<- mean(had_stroke$age)
max_age_stroke<-max(had_stroke$age)
#Plot of Age Distribution of All Patients Segmented by Stroke Status
raw_data%>%mutate(stroke=ifelse(stroke==1,TRUE,FALSE))%>%ggplot(aes(age,group=stroke,fill=stroke))+
  geom_histogram(binwidth = 1, color="darkgoldenrod4")+theme_solarized_2()+xlab("Age")+ylab("Count")+
  ggtitle("Age Distribution of All Patients")+scale_fill_discrete(name = "Stroke Victim?")+
  theme(legend.title = element_text(colour="black", size=10,face="bold"))+
  theme(legend.text = element_text(colour="black", size=8,face="bold"))+
  scale_x_continuous(expand = c(0,0),n.breaks = 18)+scale_y_continuous(expand = c(0,0),n.breaks = 10)

#HYPERTENSION
#Hypertension and Heart Disease correlation
hyp_hd_cor<-cor(raw_data$hypertension,raw_data$heart_disease)
#Total number of hypertensive patients
all_hypertensive<- sum(raw_data$hypertension)
# Hypertensive patients who had stroke
stroke_hypertensive<-sum(had_stroke$hypertension)
#Percentage of heart disease patients who had stroke
per_hypertensive_stroke<- (stroke_hypertensive/all_hypertensive)*100
#Percentage of stroke patients with hypertension
stroke_prop_hypert<-mean(had_stroke$hypertension)*100
#Percentage of stroke patients in total data set
all_prop_hypert<-mean(raw_data$hypertension)*100

#HEART DISEASES
#Total number of patients with heart diseases
all_hd<- sum(raw_data$heart_disease)
# Hypertensive patients who had stroke
stroke_hd<-sum(had_stroke$heart_disease)
#Percentage of heart disease patients who had stroke
per_hd_stroke<- (stroke_hd/all_hd)*100
#Percentage of stroke patients with hypertension
stroke_prop_hd<-mean(had_stroke$heart_disease)*100
#Percentage of stroke patients in total data set
all_prop_hd<-mean(raw_data$heart_disease)*100

#EVER MARRIED
#Converting to factor
raw_data<-raw_data%>%mutate(ever_married=as.factor(ever_married))
#Percentage of married in total dat
mean_married_total<-mean(raw_data$ever_married=="Yes")*100
#Percentage of all patients over or exactly 30
mean_above_30<-mean(raw_data$age>=30)
#Percentage of  married that had stroke
had_stroke_married<- mean(had_stroke$ever_married=="Yes")*100
#Correlation between age and marriage status as 1 for married and 0 for not married
marriage_age_cor<-cor(raw_data$age,ifelse(raw_data$ever_married=="Yes",1,0))
#Removing ever married predictor
raw_data<-raw_data%>%select(!ever_married)

#WORK TYPE
#Distribution of work types in data
work_dist<- table(raw_data$work_type)
#Table of all workers in Never worked section
never_worked<- raw_data%>%filter(work_type=="Never_worked")
#creating table for percentage of workers in each role that had hypertension
percentage_with_stroke<- 
  data.frame(government_job=sum(had_stroke$work_type=="Govt_job")/sum(raw_data$work_type=="Govt_job"),
             private=sum(had_stroke$work_type=="Private")/sum(raw_data$work_type=="Private"),
             self_employed_job=sum(had_stroke$work_type=="Self-employed")/sum(raw_data$work_type=="Self-employed"))
percentage_with_stroke<-transpose(percentage_with_stroke)
percentage_with_stroke<-percentage_with_stroke%>%mutate(V1=V1*100)
colnames(percentage_with_stroke)<- "percentage with stroke"
rownames(percentage_with_stroke)<- c("Government Work","Private","Self-employed")
# Coverting Work type to 0 for unemployed and 1 for employed 
raw_data<- raw_data%>%mutate(work_type=ifelse(work_type=="children"|work_type=="Never_worked",0,1))
raw_data<- raw_data%>%select(!work_type)
#RESIDENCE TYPE
#DISTRIBUTION OF PATIENTS IN RURAL AND URBAN AREAS FOR ALL PATIENTS AND STROKE VICTIMS
perc_stroke_rural<- mean(had_stroke$residence_type=="Rural")*100
perc_stroke_urban<- mean(had_stroke$residence_type=="Urban")*100
perc_total_rural<- mean(raw_data$residence_type=="Rural")*100
perc_total_urban<-mean(raw_data$residence_type=="Urban")*100
#Converting residence type to factor
raw_data<- raw_data%>% mutate(residence_type=as.factor(residence_type))

#GLUCOSE LEVEL
#Plot for distribution of glucose levels, showing high, low and optimal levels 
#Add line for high blood sugar
raw_data%>%
  mutate(Blood_Sugar_Level=ifelse(avg_glucose_level<=70,"Low Blood Sugar Level"
                                  ,ifelse(avg_glucose_level>=140,"High Blood Sugar","Optimal Blood Sugar")))%>%
  ggplot(aes(avg_glucose_level,fill=Blood_Sugar_Level))+geom_histogram(binwidth = 0.9)+
  theme_solarized_2()+xlab("Average Glucose/Sugar Level")+ylab("Count")+
  ggtitle("Distribution of Sugar Levels for All Patients")+scale_fill_discrete(name = "Sugar Level")+
  theme(legend.title = element_text(colour="black", size=10,face="bold"))+
  theme(legend.text = element_text(colour="black", size=8,face="bold"))+
  scale_x_continuous(expand = c(0,0),n.breaks = 18)+scale_y_continuous(expand = c(0,0),n.breaks = 10)+geom_vline(xintercept=140, col = "black")
#TIBBLE Showing most patients are actually have optimal average glucose levels
prop_sug_level<-raw_data%>%mutate(Blood_Sugar_Level=ifelse(avg_glucose_level<=70,"Low Blood Sugar Level"
                                                           ,ifelse(avg_glucose_level>=140,"High Blood Sugar","Optimal Blood Sugar")))%>%
  group_by(Blood_Sugar_Level)%>%summarise(percentage=n()*100/nrow(raw_data))  
#Plot for distribution of glucose levels of stroke victims
had_stroke%>%
  mutate(Blood_Sugar_Level=ifelse(avg_glucose_level<=70,"Low Blood Sugar Level"
                                  ,ifelse(avg_glucose_level>=140,"High Blood Sugar","Optimal Blood Sugar")))%>%
  ggplot(aes(avg_glucose_level,fill=Blood_Sugar_Level))+geom_histogram(binwidth = 4)+
  theme_solarized_2()+xlab("Average Glucose/Sugar Level")+ylab("Count")+
  ggtitle("Distribution of Sugar Levels for Stroke Victims")+scale_fill_discrete(name = "Sugar Level")+
  theme(legend.title = element_text(colour="black", size=10,face="bold"))+
  theme(legend.text = element_text(colour="black", size=8,face="bold"))+
  scale_x_continuous(expand = c(0,0),n.breaks = 18)+scale_y_continuous(expand = c(0,0),n.breaks = 10)+geom_vline(xintercept=140, col = "black")
#TIBBLE Showing greater percentage of high blood sugar patients amongst stroke victims
prop_sug_level2<-had_stroke%>%
  mutate(Blood_Sugar_Level=ifelse(avg_glucose_level<=70,"Low Blood Sugar Level"
                                  ,ifelse(avg_glucose_level>=140,"High Blood Sugar","Optimal Blood Sugar")))%>%
  group_by(Blood_Sugar_Level)%>%summarise(percentage=n()*100/nrow(had_stroke))
#PERCENTAGE OF HIGH SUGAR LEVEL PATIENTS WITH STROKE
perc_hbs_stroke<- (sum(raw_data$avg_glucose_level>=140)/sum(had_stroke$avg_glucose_level>=140))*100

#BMI
#Converting bmi from character to Numeric
raw_data<-raw_data%>%mutate(bmi=as.numeric(bmi))
#Percentage of NA's roughly 4%
na_prop_bmi<- mean(is.na(raw_data$bmi))*100
#Removing rows with N/A values
raw_data<-raw_data%>%filter(!is.na(bmi))
#Recreating data for patients that had stroke
had_stroke<- raw_data%>%filter(stroke==1)
#Plot of distribution of BMI values grouped by whether patient had stroke or not finish plot
raw_data%>%mutate(stroke=ifelse(stroke==1,TRUE,FALSE))%>%ggplot(aes(bmi,group=stroke,fill=stroke))+   
  geom_histogram(binwidth = 3)+theme_solarized_2()+xlab("BMI")+ylab("Count")+
  ggtitle("Distribution of BMI for all patients")+scale_fill_discrete(name = "Has Stroke?")+
  theme(legend.title = element_text(colour="black", size=10,face="bold"))+
  theme(legend.text = element_text(colour="black", size=8,face="bold"))+
  scale_x_continuous(expand = c(0,0),n.breaks = 18)+scale_y_continuous(expand = c(0,0),n.breaks = 10)+geom_vline(xintercept=24.9, col = "black")
#percentage of underweight patients in data
mean(raw_data$bmi<18.5)

#SMOKING STATUS
#Converting smoking status to factor
raw_data<-raw_data%>%mutate(smoking_status=as.factor(smoking_status))
#percentage of smoking victims in total data and amongst stroke victims
mean(raw_data$smoking_status=="formerly smoked"|raw_data$smoking_status=="smokes")
mean(had_stroke$smoking_status=="formerly smoked"|had_stroke$smoking_status=="smokes")

#STROKE
#CLASSIFICATION PROBLEM Converting Stroke to a factor
raw_data<-raw_data%>%mutate(stroke=as.factor(raw_data$stroke))


#USING SMOTE FOR INBALANCED CLASSIFICATION
smoted_data<-smote(stroke~., raw_data, perc.over = 20, k = 100, perc.under = 1)
#prop_stroke_victims
mean(smoted_data$stroke=="1")
#SPLITTING DATA INTO TEST TRAIN AND VALIDATION
#Test and Train Data 50% each of new data set
test_index<- createDataPartition(y = smoted_data$stroke, times = 1, p = 0.5, list = FALSE)
test_set<- smoted_data%>%dplyr::slice(test_index)
train_set<-smoted_data%>%dplyr::slice(-test_index)


#MODELING
#KNN IMPLEMENTATION WITH SMOTE
knn_train<-train(stroke~.,method="knn",data =train_set,tuneGrid=data.frame(k = seq(5, 50, 5)))
knn_prediction<-predict(knntest,test_set,type="raw")
knn_conf_matrix<-confusionMatrix(knn_prediction,test_set$stroke)

#RANDOM FOREST IMPLEMENTATION WITH SMOTE
rf_train<-train(stroke~.,method="rf",data =train_set)
rf_prediction<-predict(rftest,test_set,type="raw")
rf_conf_matrix<-confusionMatrix(rf_prediction,test_set$stroke)

#PLOT OF K TUNING PARAMETERS
ggplot(knn_train,highlight=TRUE)+theme_solarized_2()+xlab("K Values")+ylab("Accuracy")+
  ggtitle("K-Nearest Neighbours Tuning")+
  scale_x_continuous(expand = c(0,0),n.breaks = 18)+scale_y_continuous(expand = c(0,0),n.breaks = 10)

#CREATING CONFUSION MATRIX TABLE FOR KNN
knn_tab<-knn_conf_matrix[["table"]]
colnames(knn_tab)<-c("Actually 0","Actually 1")
rownames(knn_tab)<-c("Predicted 0","Predicted 1")
#CREATING CONFUSION MATRIX TABLE FOR RANDOM FOREST
rf_tab<-rf_conf_matrix[["table"]]
colnames(rf_tab)<-c("Actually 0","Actually 1")
rownames(rf_tab)<-c("Predicted 0","Predicted 1")

