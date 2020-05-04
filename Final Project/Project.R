# Data Analytics final project
# read data and eda
options(stringsAsFactors = FALSE)
library(lubridate)
library(ggplot2)
library(maps)
library(RColorBrewer)
library(dplyr)
library(e1071)
library(RColorBrewer)
library(corrplot)
data<-read.csv("/Users/wangyuyan/Documents/王语嫣/Lally/Data Analytics/Project/NYPD_Complaint_Data_Current__Year_To_Date_.csv")
head(data)
dim(data)
# Get rid of unnecessary features
data_clean<-subset(data,selec=c("CMPLNT_NUM","ADDR_PCT_CD","BORO_NM","CMPLNT_FR_DT","CMPLNT_FR_TM","LAW_CAT_CD","OFNS_DESC","Latitude","Longitude"))
attach(data_clean)
#data_clean<-subset(data,selec=c("ADDR_PCT_CD","BORO_NM","CMPLNT_FR_DT","CMPLNT_FR_TM","OFNS_DESC","Latitude","Longitude"))

str(data_clean)
# Dealing with time and date

DATE1<-as.Date(data_clean$CMPLNT_FR_DT,'%m/%d/%Y')
data_clean$dayofweek<-weekdays(DATE1)
TIME1<-strptime(as.character(data_clean$CMPLNT_FR_TM),"%H:%M:%S")
TIME1
data_clean$year<-year(DATE1)
data_clean$month<-month(DATE1)
data_clean$day<-day(DATE1)
data_clean$hour<-hour(TIME1)
data_clean$hour
data_clean$minute<-minute(TIME1)
data_clean$minute
# Separate the data to different time group with a interval of 2 hours

data_clean$timeGroup[data_clean$hour<2]<-"0~2"
data_clean$timeGroup[data_clean$hour>=2&data_clean$hour<4]<-"2-4"
data_clean$timeGroup[data_clean$hour>=4&data_clean$hour<6]<-"4-6"
data_clean$timeGroup[data_clean$hour>=6&data_clean$hour<8]<-"6-8"
data_clean$timeGroup[data_clean$hour>=8&data_clean$hour<10]<-"8-10"
data_clean$timeGroup[data_clean$hour>=10&data_clean$hour<12]<-"10-12"
data_clean$timeGroup[data_clean$hour>=12&data_clean$hour<14]<-"12-14"
data_clean$timeGroup[data_clean$hour>=14&data_clean$hour<16]<-"14-16"
data_clean$timeGroup[data_clean$hour>=16&data_clean$hour<18]<-"16-18"
data_clean$timeGroup[data_clean$hour>=18&data_clean$hour<20]<-"18-20"
data_clean$timeGroup[data_clean$hour>=20&data_clean$hour<22]<-"20-22"
data_clean$timeGroup[data_clean$hour>=22]<-"22-0"
data_clean=data_clean %>% mutate_if(is.character, as.factor)
data_clean <- droplevels(data_clean)
# write.csv(data_clean,file="/Users/wangyuyan/dataclean.csv")


# check if there is any null values
sum(is.na(data_clean))


# EDA& Visualization
# Since there are too many crimes, I will focus on the top 20
crime<-c("PETIT LARCENY",
"HARRASSMENT 2",
"ASSAULT 3 & RELATED OFFENSES","CRIMINAL MISCHIEF & RELATED OF GRAND LARCENY",
"FELONY ASSAULT",
"OFF. AGNST PUB ORD SENSBLTY & MISCELLANEOUS PENAL LAW",
"DANGEROUS DRUGS",
"ROBBERY",
"BURGLARY",
"OFFENSES AGAINST PUBLIC ADMINI","SEX CRIMES",
"VEHICLE AND TRAFFIC LAWS",
"DANGEROUS WEAPONS",
"GRAND LARCENY OF MOTOR VEHICLE",
"FORGERY",
"INTOXICATED & IMPAIRED DRIVING","THEFT-FRAUD",
"CRIMINAL TRESPASS")
allcrime<-unique(data_clean$OFNS_DESC)
indexVector <- !(is.element(data_clean$OFNS_DESC, crime))
cbind(data_clean, indexVector)
data.new<-data_clean[indexVector,]
dim(data.new)
# Visualization
detach(data_clean)
str(data.new)
data.cor<-data.new[,c("ADDR_PCT_CD","Latitude","Longitude","year","month","day","hour","minute")]
# correlation between numeric data
corrplot(cor(data.cor), type="upper", order="hclust",col=brewer.pal(n=8, name="Reds"),title = "Correlation Between Numerical Variables",tl.col="Black")
?corrplot
# ggplot(data.new, aes(OFNS_DESC)) +geom_bar(color = '#7b9eb3')
# Frequency by time zone
ggplot(data.new, aes(reorder(timeGroup,rep(1,length(timeGroup)),sum))) +geom_bar(aes(fill=OFNS_DESC))+theme(legend.position = "none")+title("Crime by Time Group")
ggplot(data.new, aes(reorder(BORO_NM,rep(1,length(BORO_NM)),sum))) +geom_bar(aes(fill=OFNS_DESC))+theme(legend.position = "none")+title("Crime by Boro")
ggplot(data.new, aes(reorder(dayofweek,rep(1,length(dayofweek)),sum))) +geom_bar(aes(fill=OFNS_DESC))+theme(legend.position = "none")+title("Crime by Boro")
ggplot(data.new, aes(reorder(ADDR_PCT_CD,rep(1,length(ADDR_PCT_CD)),sum))) +geom_bar(aes(fill=OFNS_DESC))+theme(legend.position = "none")+title("Crime by preict")

count(data.new[data.new$OFNS_DESC=="Grand Larceny"])
sum(is.na(data_clean))
# ggplot(data.new,aes(data.new$hour))+geom_abline(aes(OFNS_EDSC))
# ggplot(data = data.new, mapping = aes(x = factor(data.new$hour), y = data.new$OFNS_EDSC, group = 1)) + geom_line() + xlab('Hours')
?geom_line()
ggplot(data.new, aes(x=yea)) + geom_line()

# 记得单独截图legend
# freq by district


# 

# 
# Create Maps

# Modeling
library(class)
datacelan<-read.csv("/Users/wangyuyan/dataclean.csv")

sub<-sample(1:nrow(datacelan),round(nrow(datacelan)*1/2))
length(sub)
train<-datacelan[sub,]
test<-datacelan[-sub,]
response<-data.frame(Cat=train$OFNS_DESC)
crime1<-as.character(unique(train$OFNS_DESC))      
crime1<-sort(crime)
dim(train)
# Random Forest
library(randomForest)
train <- droplevels(train)
str(train)
sum(is.na(train))
rf1<-randomForest(OFNS_DESC~BORO_NM+month+day+timeGroup, data=train,importance=TRUE,ntree=30)
summary(rf1)
data-clean$importance
varImpPlot(data_clean,main="Variable Importance")
# Naive Bayes
nb1 <- naiveBayes(OFNS_DESC ~ ., data=train)
summary(nb1)
predict_nb<-predict(nb1,test)

# SVM
svm1<-svm(OFNS_DESC~BORO_NM+month+day+timeGroup,data=train)
predict_svm<-predict(svm1,test)


