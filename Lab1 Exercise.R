# Exercise
# getting data
gpw<-read.csv("/Users/wangyuyan/Documents/王语嫣/Lally/Data Analytics/GPW3_GRUMP_SummaryInformation_2010.csv")
install.packages('readxl')
readxl::read_xls("/Users/wangyuyan/Documents/王语嫣/Lally/Data Analytics/EPI/2010EPI_data.xls")
data()

EPI_data <- read.csv("/Users/wangyuyan/Documents/王语嫣/Lally/Data Analytics/EPI/2010EPI_data.csv",skip = 1)
View(EPI_data)
head(gpw)
attach(EPI_data)
install.packages('XQuartz')
fix(EPI_data) #launches a simple data editor and test it
EPI #try to print out values EPI_data$EPI
tf<- is.na(EPI) #records True values if the value is NA
E <- EPI[!tf] #filter out NA values

#exploring the distribution

summary(EPI) #stats
head(EPI_data)

fivenum(EPI,na.rm=TRUE)
stem(EPI) #stem leaf plot
hist(EPI) #histgram
hist(EPI,seq(30.,95.,1.0), prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw="SJ")) # or try bw=“SJ” 
rug(EPI)
help(rug)

plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) #density
par(pty="s") 
qqnorm(EPI); qqline(EPI) #QQ plot


x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

#fitting a distribution
boxplot(EPI,DALY) 

#intercompare:EPI, ENVHEALTH, ECOSYSTEM, DALY, AIR_H, WATER_H, AIR_EWATER_E, BIODIVERSITY
boxplot(EPI,ENVHEALTH,ECOSYSTEM,DALY,AIR_H,WATER_H,AIR_E,WATER_E,BIODIVERSITY)

help("distributions")

# Exercise 2
# Conditional filtering
EPILand <- EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland,seq(30.,95.,1.0),prob=TRUE)
hist(No_surface_water)
hist(No_surface_water,seq)
EPI_South_Asia <- EPI[EPI_regions=="South Asia"]
EPI_South_Asia

#GPW3_GRUMP EXERCISE 
View(gpw)
summary(gpw)
fivenum(gpw$Num.Settlement.Points,na.rm = TRUE)
boxplot(gpw$Num.Settlement.Points)
hist(gpw$Num.Settlement.Points)
stem(gpw$Num.Settlement.Points)

#filtering
gpwEU=gpw$Num.Settlement.Points[gpw$ContinentName=="Europe"]
gpwEU
