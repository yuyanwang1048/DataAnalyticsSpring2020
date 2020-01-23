# Exercise
# getting data
read.csv("/Users/wangyuyan/Documents/王语嫣/Lally/Data Analytics/GPW3_GRUMP_SummaryInformation_2010.csv")
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



