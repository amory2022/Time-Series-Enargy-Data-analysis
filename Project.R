#create a dataframe from our dataset

mydata_2<- read.csv("D:/R_studio/crush2/Data files/opsd_germany_daily.txt",header = TRUE,row.names = "Date")

#Exployer the dataset

mydata_2

#looking a part of dataframe using head() and tail()

head(mydata_2) 

tail(mydata_2)

#view the data in a tabular format

view(mydata_2)
 
#Knowing the number of rows and columns

dim(mydata_2)

#check the datatype of each column in dataframe

str(mydata_2)

#looking at the date colum 

head(mydata_2$Date)

#will not show data as its index

#looking at row names(indexes)

row.names(mydata_2)

#accessing a specific row
mydata_2["2008-08-24",]

summary(mydata_2)

#Dataframe Without Date column

mydata_3<-read.csv("D:/R_studio/crush2/Data files/opsd_germany_daily.txt",header = TRUE)

mydata_3

 #Exployer the Data column

str(mydata_3$Date)

#convert into date format

x<-as.Date(mydata_3$Date)
head(x)
class(x)
str(x)

#create year ,month,day columns

year<- as.numeric(format(x,'%Y'))
head(year)

month<-as.numeric(format(x,'%m'))
head(month)

day<-as.numeric(format(x,'%d'))
head(day)

head(mydata_3)

#add columns to the exisiting dataframe

mydata_3<-cbind(mydata_2,year,month,day)

head(mydata_3)

mydata_3[1:3,]#look at the first three rows

head(sample(mydata_3),8)

#create line plot of the full time series of Germany is daily 
#electricity consumption ,using  the dataframe plot() method.


#using plot

#option  1:

plot(mydata_3$year,mydata_3$Consumption,type = 'l',xlab = "year",
     ylab = "Consumption")

#option  2:

plot(mydata_3$year,mydata_3$Consumption,type = 'l',xlab = "year",
     ylab = "Consumption",lty=1,ylim = c(800,1700),xlim = c(2006,2018))


#Better options

#option 3:
#for one plot/window
par(mfrow=c(1,1))

plot(mydata_3[,2])


#option  4:
plot(mydata_3[,2],xlab = "year",ylab ="Consumption" )
plot(mydata_3[,2],xlab = "year",ylab ="Consumption",type = "l",lwd=2,col="blue" )
plot(mydata_3[,2],xlab = "year",ylab ="Consumption",type = "l",lwd=2,xlim = c(0,2018))
plot(mydata_3[,2],xlab = "year",ylab ="Consumption",type = "l",lwd=2,xlim = c(2006,2018))
plot(mydata_3[,2],xlab = "year",ylab ="Consumption",type = "l",lwd=2,
     xlim = c(2006,2018),ylim = c(900,2000),main = "Consumption Graph")


#taking log values of Consumption and take differences of logs

plot(10*diff(log(mydata_3[,2])),xlab = "year",ylab = "Consumption",type = "l",
     lwd=2,ylim = c(-5,5),main = "Consumption Graph",col="orange")


#using ggplot()

install.packages("ggplot2")
library(ggplot2)


#option 1:
ggplot(mydata_3,type="o")+geom_line(aes(x=year,y=Consumption))

#option 2:
ggplot(data = mydata_3,aes(x=year,y=Consumption,group=1))+
  geom_line()+geom_point()

#option 3:
ggplot(data = mydata_3,aes(x=year,y=Consumption,group=1))+geom_line(linetype="dashed")+geom_point()
ggplot(data = mydata_3,mapping =aes(x=year,y=Consumption,col="red"))+geom_point()


#plot the data considering the solar and wind time series too

#wind column
min(mydata_3[,3],na.rm=T)
max(mydata_3[,3],na.rm=T)

#Consumption column
min(mydata_3[,2],na.rm=T)
max(mydata_3[,2],na.rm=T)

#Solar
min(mydata_3[,4],na.rm=T)
max(mydata_3[,4],na.rm=T)

#wind+solar
min(mydata_3[,5],na.rm=T)
max(mydata_3[,5],na.rm=T)

#for multiple plots
par(mfrow=c(3,1))

#or
plot1<-plot(mydata_3[,2],xlab = "year",ylab = "Daily Totals (GWH)",type = "l",
            lwd=2,main = "Consumption",col="orange",ylim = c(840,1750))


plot1<-plot(mydata_3[,1],mydata_3[,2],xlab = "year",ylab="Daily Totals (GWH)",type="l",
            lwd=2,main="Consumption",col="orange",ylim=c(840,1750))


#test
plot2<-plot(mydata_3[,4],xlab = "year",ylab ="Daily Totals (GWH)",type = "l",
            main = "Solar",ylim = c(0,500),col="blue")

plot2<-plot(mydata_3[,1],mydata_3[,2],xlab = "year",ylab ="Daily Totals (GWH)",type = "l",
            main = "Solar",ylim = c(0,500),col="blue")


plot3<-plot(mydata_3[,3],xlab = "year",ylab ="Daily Totals (GWH)",type = "l",
            lwd=2,main = "Wind",ylim = c(0,900),col="red")

plot3<-plot(mydata_3[,1],mydata_3[,3],xlab = "year",ylab ="Daily Totals (GWH)",type = "l",
            lwd=2,main = "Wind",ylim = c(0,900),col="red")



#let’s plot time series in a single year to investigate further
str(mydata_3)
x<-as.Date(mydata_3$Date)
head(x)
class(x)
str(x)

#to convert date column into date format
MDYdate<-as.Date(x,format="%m/%d/%Y")

str(MDYdate)

mydata_4<-cbind(MDYdate,mydata_3)
head(MDYdate)

head(mydata_4)
str(mydata_4)

#look at the specific data
mydata_4<-subset(mydata_4,subset=mydata_4$MDYdate>='2017-01-01'&mydata_4$MDYdate<='2017-12-31')



