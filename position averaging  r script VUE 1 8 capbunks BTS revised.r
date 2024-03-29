#mean position algorithm in R
#by colin simpfendorfer, michelle heupel, esben olsen, even moland
#
#read libraries
#
library(chron)
library(maptools)
library (adehabitatHR)
library (adehabitatLT)
library(PBSmapping)
library(rgdal)
library(ks)
library(lattice)
 

##set working directory
setwd("C:/Users/jc167551/Dropbox/Heron grs/14793")
#setwd("C:/Users/jc167551/Dropbox/Cap Bunk vrl/BTS")
#
#read in a tag data file
#
tagdata <- read.csv(choose.files(),header=TRUE,sep=",")
#summary(tagdata)
#
names(tagdata)[names(tagdata)=="Receiver"]<-"Receiver.S.N.x"
names(tagdata)[names(tagdata)=="Transmitter"]<-"IDx"
names(tagdata)[names(tagdata)=="�..Date.and.Time..UTC."]<-"Date.Time"
names(tagdata)[names(tagdata)=="Sensor.Value"]<-"Depth"
#remove extra characters
IDy<-strsplit(as.character(tagdata$IDx), "-")
RSNy<-strsplit(as.character(tagdata$Receiver.S.N.x),"-")
n<-length(IDy)
IDz<-array(0,n)
RSNz<-array(0,n)
for(i in 1:n){
	IDz[i]<-IDy[[i]][3]
	RSNz[i]<-RSNy[[i]][2]
}
IDv<-data.frame(IDz)
RSNv<-data.frame(RSNz)
tagdata$ID<-as.numeric(IDv$IDz)
tagdata$Receiver.S.N.<-as.numeric(RSNv$RSNz)


#create character value for fiel names
fname<-as.character(tagdata$ID)
fname<-unique(fname)
#remove unwanted fields
tagdata$Transmitter.Name<-NULL
tagdata$Transmitter.Serial<-NULL
tagdata$Sensor.Unit<-NULL
tagdata$Station.Name<-NULL
tagdata$Latitude<-NULL
tagdata$Longitude<-NULL
tagdata$Receiver.S.N.x<-NULL
tagdata$IDx<-NULL
#This leaves the date-time field, tag code number and receiver numbers
#
#format the date and time correctly for R
dt<-as.POSIXlt(strptime(as.character(tagdata$Date.Time),"%Y-%m-%d %H:%M:%S"))  
#dtnum<-as.POSIXct(dt,origin="1960/01/01",tmz="UTC") #make sure it is in UTC which comes out of VUE
dtnum<-as.POSIXct(dt,origin="1960/01/01")
#
#convert to local time by adding or substracting the number of seconds difference
#for qld add 36000
#for norway add 3600
timediff=36000
dtnumlocal<-dtnum+timediff
#currently need to ignore the fact that the dtnumlocal says it is UTC 
#once the time difference has been applied
#
#add the dtnumlocal to the dataframe with the other information
finaldata<-data.frame(dtnumlocal,tagdata$ID,tagdata$Receiver.S.N.,tagdata$Depth)
#
#
#  need to read in the details of the location and serial numbers of the stations
#  file should have serial, latitude, longitude, name as headers 
#  and info below (lat and long in decimal degrees)
#
statinfo<-read.csv(choose.files(),header=TRUE,sep=",")
statinfo
#convert findate
statinfo$findat<-as.POSIXlt(strptime(as.character(statinfo$Findate),"%d/%m/%Y"))
statinfo$Findate<-NULL
statinfo$startdat<-as.POSIXlt(strptime(as.character(statinfo$Startdate),"%d/%m/%Y"))
statinfo$Startdate<-NULL

#
#  add the lat and long to the data frame with the detection information so that each detection
#  has the stations lat and long associated with it. 
#  this will enable mean positions to be calculated very simply when the time block is defined
#
mergedata<-merge(finaldata,statinfo, by.x = "tagdata.Receiver.S.N.", by.y = "Serial")
mergedata<-subset(mergedata,(mergedata$findat>mergedata$dtnumlocal&mergedata$startdat<mergedata$dtnumlocal))
#sort the mergedata data frame so it is in time order
mergedatasorted<-mergedata[order(mergedata$dtnumlocal),]
#

mergedatasorted$hour<-as.factor(hours(mergedatasorted$dtnumlocal))
mergedatasorted$month<-as.factor(as.numeric(format(mergedatasorted$dtnumlocal,"%m")))
mergedatasorted$mon<-as.factor(as.numeric(cut(mergedatasorted$dtnumlocal, "months")))
mergedatasorted$dom<-days(mergedatasorted$dtnumlocal)
mergedatasorted$month<-as.factor(format(mergedatasorted$dtnumlocal,"%m"))
mergedatasorted$year<-as.factor(format(mergedatasorted$dtnumlocal,"%Y"))
mergedatasorted$day<-as.Date(mergedatasorted$dtnumlocal)
mergedatasorted$tag<-as.factor(fname)
mergedatasorted$doy<-as.numeric(strftime(mergedatasorted$dtnumlocal, format = "%j"))
mergedatasorted$dom<-days(mergedatasorted$dtnumlocal)
mergedatasorted$mon<-as.numeric(cut(mergedatasorted$dtnumlocal, "months")) 

fname2<-paste("MDSdata",fname,sep="")
fname2<-paste(fname2,".csv",sep="")
write.csv(mergedatasorted,file=fname2)




#run through mergedata with the set time frame to calculate the mean postions
#
#set the time frame for position averaging
#
timestep=7200
#
#get the start date and time from the file
#
starttime<-mergedatasorted$dtnumlocal[1]
bt<-nrow(mergedatasorted)
#bt<-b[1]
endtime<-mergedatasorted$dtnumlocal[bt]
starttime
endtime
#
#calculate the start of the day of the first detection
#
st<-trunc(starttime,"day")
st
#
#calculate the date at the end of the data set
#
et<-trunc(endtime,"day")+86400
et
#
#add a timestep (in seconds) 30 minutes is 1800 seconds
#
#nexttime<-st+timestep
#nexttime
#
#generate a sequence from start to end times by the specified time step
#
ex<-seq(from=st, to=et, by=timestep)
ex2<-ex+(timestep/2)
#ex2
#
#loop through all possible time steps to calculate mean positions
#
#calculate the number of elements in the ex frame (c)
#
c<-length(ex2)
c
#
#start a loop of length c
#
mergedatasorted$reefnum<-array(0,bt)
for (i in 1:bt){
	if (mergedatasorted$Reef[i]=="HIR"){mergedatasorted$reefnum[i]<-1}
	if (mergedatasorted$Reef[i]=="SYR"){mergedatasorted$reefnum[i]<-2}
	if (mergedatasorted$Reef[i]=="OTR"){mergedatasorted$reefnum[i]<-3}
	}

#remove data from after last full download (March 2015)
finishup<-as.POSIXlt(strptime(as.character("10/03/2015"),"%d/%m/%Y"))
mergedatasorted<-mergedatasorted[-which(mergedatasorted$dtnumlocal>finishup),]
finaldata<-finaldata[-which(finaldata$dtnumlocal>finishup),]

hab<-array("N",c)
habnum<-array(0,c)

meanlat<-0
meanlong<-0
numhits<-0
rec<-0
meandepth<-0
reefkm<-0
for(i in 1:c){
	#get the data from the time period	
	calcframe<-mergedatasorted[mergedatasorted$dtnumlocal<=ex[i+1]&mergedatasorted$dtnumlocal>ex[i],]
	#calculate the COA
	meanlat[i]<-mean(as.numeric(calcframe$Latitude))	#mean latitude
	meanlong[i]<-mean(as.numeric(calcframe$Longitude))	#mean longitude
	meandepth[i]<-mean(as.numeric(calcframe$tagdata.Depth)) #mean depth
	#numbers of receivers etc
	ex2[i]
	z<-dim(calcframe)					#calculate the number of detections heard (numhits)
	numhits[i]<-z[1]
	y<-factor(calcframe$tagdata.Receiver.S.N)	#calculate the number of receivers on which it was heard (rec)
	rec[i]<-length(unique(y))
	#calculate reef km
		#remove lagoon receviers
		calcframe2<-calcframe[calcframe$Reefkm>0,]
		reefkm[i]<-mean(as.numeric(calcframe2$Reefkm))
	#wortk out if period includes lagoon
	if(max(calcframe$Reefkm)>0) {
		hab[i]<-"R"
		habnum[i]<-10*max(calcframe$reefnum)
		}
	if(min(calcframe$Reefkm)<0) {
		hab[i]<-"L"
		habnum[i]<--10*min(calcframe$reefnum)

		}
      }
#finish loop
#
#place results into a single data frame (results) which includes all time steps

results<-data.frame(ex2,meanlat,meanlong,meandepth,reefkm,rec,numhits,hab,habnum)
results$ex2<-results$ex2-36000
results$depthcat<-trunc(results$meandepth/5)
results$datetime<-results$ex2
results$month<-as.factor(format(results$datetime,"%m"))
results$year<-as.factor(format(results$datetime,"%Y"))
results$hour<-as.factor(format(results$datetime,"%H"))
results$day<-as.Date(results$datetime)
results$tag<-as.factor(fname)
results$doy<-as.numeric(strftime(results$datetime, format = "%j"))
plot(results$doy, -results$meandepth)



#results
#
#new data frame with only time steps with positions (results2)
results2<-results[which(results$rec>0),]
#rename ex2 to datetime
results2$datetime<-results2$ex2
results2$ex2<-NULL
results2$datetime+36000
results2$month<-as.factor(format(results2$datetime,"%m"))
results2$year<-as.factor(format(results2$datetime,"%Y"))
results2$hour<-as.factor(format(results2$datetime,"%H"))
results2$day<-as.Date(results2$datetime)
results2$tag<-as.factor(fname)
results2$doy<-as.numeric(strftime(results2$datetime, format = "%j"))
results2$dom<-days(results2$datetime)
results2$mon<-as.numeric(cut(results2$datetime, "months")) 
results2$hab<-as.factor(results2$hab)



#calculate the 2D kernel
dd<-data.frame(results2$reefkm,results2$meandepth)
##auto bandwidth selection

H.pi2<-array(c(254552.40455,31.3245429,31.3245429,0.6484481),c(2,2))
#H.pi2 <- Hpi(dd,binned=TRUE)*1
ddhat<-kde(dd,H=H.pi2)


dev.new(width=12, height=4)
par( mfcol= c(1, 1))

#heron plot
par(mar=c(5,5,3,2))
plot(ddhat,cont=c(95),drawpoints=TRUE,col="black",xlab="Reef distance (m)", ylab="Depth (m)",col.pt="grey",cex=0.6,xlim=c(0,27000),ylim=c(30,0),main="Heron") 
plot(ddhat,cont=c(50),add=TRUE,col="dark orange")
plot(ddhat,cont=c(75),add=TRUE,col="dark green")
plot(ddhat,cont=c(25),add=TRUE,col="red")
polygon(c(0,0,5968,5968),c(0,30,30,0), col="green", border = "green",density=0.01) 
polygon(c(19773,19773,27000,27000),c(0,30,30,0), col="green", border = "green",density=0.01) 
polygon(c(5970,5970,19773,19773),c(0,30,30,0), col="yellow", border = "yellow",density=0.01)



#results2
#plot location of receivers and the positions calculated
pdf("locationplot.pdf")
par( mfcol= c(1, 1))
plot(statinfo$Longitude,statinfo$Latitude,col="green",pch=2,xlab="Longitude",ylab="Latitude",asp=0.7,cex=0.7)
points(results2$meanlong,results2$meanlat)
dev.off()
#
#write out data file with mean positions
fname1<-paste("PAV",fname,sep="")
fname1<-paste(fname1,".csv",sep="")
results3<-results2
write.csv(results2,file=fname1)
#
#save(results3,file="test.Rdata")

#animate the plot
par( mfcol= c(1, 1))
for (i in 1:c){
plot(statinfo$Longitude,statinfo$Latitude,col="green",pch=2,xlab="Longitude",ylab="Latitude",main=results$ex2[i],asp=0.7,cex=0.7)
points(results$meanlong[i],results$meanlat[i],cex=(1+(results$depthcat[i]/2)))
for (j in 1:50000){ss<-j^2}
}

#display a depth histogram
pdf("Depth histograms.pdf")
par( mfcol= c(1, 2))
hist(results2$meandepth,main="Mean",xlab="Depth (m)")
hist(tagdata$Depth,main="Individual",xlab="Depth (m)")
dev.off()

#depth by reef/lagoon
pdf("Depth by habitat.pdf")
par( mfcol= c(1, 1))
plot(mergedatasorted$Habitat,-mergedatasorted$tagdata.Depth,xlab="Habitat",ylab="Depth (m)")
dev.off()

#depth histograpm between habitats
lagoon<-subset(mergedatasorted, Habitat=="L")
reef<-subset(mergedatasorted, Habitat=="R")
pdf("Depth hist by habitat.pdf")
par( mfcol= c(1, 2))
hist(lagoon$tagdata.Depth,main="Lagoon",xlab="Depth (m)")
hist(reef$tagdata.Depth,main="Reef",xlab="Depth (m)")
dev.off()


#explore depth by time of day
pdf("Depth by date.pdf")
par( mfcol= c(1, 1))
plot(finaldata$dtnumlocal,-finaldata$tagdata.Depth,ylab="Depth (m)",xlab="Date")
dev.off()

finaldata$hour<-as.factor(hours(finaldata$dtnumlocal+36000))
finaldata$month<-as.factor(as.numeric(format(finaldata$dtnumlocal,"%m")))
finaldata$mon<-as.factor(as.numeric(cut(finaldata$dtnumlocal, "months")))
finaldata$dom<-days(finaldata$dtnumlocal)


#heat map

aaa<-aggregate( . ~ dom+month+hour, FUN=function(x) c(mn=mean(x)), data=finaldata)
levelplot(-aaa$tagdata.Depth~aaa$dom+aaa$hour|aaa$month,xlab="Day of the month",ylab="Time of day",main="",index.cond=list(c(9,10,11,12,5,6,7,8,1,2,3,4)))


ccc<-aggregate( . ~ dom+mon+hour, FUN=function(x) c(mn=mean(x)), data=finaldata)
levelplot(-ccc$tagdata.Depth~ccc$dom+ccc$hour|ccc$mon,xlab="Day of the month",ylab="Time of day",main="")

colpal <- colorRampPalette(c("green","yellow","red")) 
colseq <- seq(-30,30,by=10) 
bbb<-aggregate( . ~ dom+mon+hour, FUN=function(x) c(mn=mean(x)), data=results2)
levelplot(results2$habnum~results2$dom+results2$hour|results2$mon,xlab="Day of the month",ylab="Time of day",main="",
  col.regions = colpal(7),  colorkey = list(at = colseq, labels=list(at=colseq)) )

levelplot(results2$habnum~results2$dom+results2$hour|results2$mon,xlab="Day of the month",ylab="Time of day",main="")


#explore day by reefkm
#plot(results2$doy,results2$reefkm,xlab="Day of the year",ylab="Reef distance (m)",ylim=c(0,29000))
#plot(results2$day,results2$reefkm,xlab="Date",ylab="Reef distance (m)",ylim=c(0,29000))
#plot(results2$day,results2$reefkm,type="b")


#explore by habitat type
plot(results2$day,results2$habnum)

habitat<-table(results2$hab)
sink("Periods by habitat.txt")
habitat
sink()



#create a trajectory
#1. decimal degrees
locdd<-data.frame(cbind(results2$meanlong,results2$meanlat))
names(locdd)<-c("X", "Y")
hrtraj<- as.ltraj(xy = locdd[, c("X", "Y")], date = results2$datetime, id = "15940")
pdf("Trajectory.pdf")
plot(hrtraj,xlab="Longitude",ylab="Latitude")
dev.off()
#2. utm
locdd<-data.frame(cbind(results2$meanlong,results2$meanlat))
names(locdd)<-c("X", "Y")
attr(locdd,"zone")<-55
attr(locdd,"projection")<-"LL"
locutm<-convUL(locdd,km=FALSE)
utmcoord<-data.frame(cbind(locutm$X,locutm$Y))
names(utmcoord)<-c("X", "Y")
trajutm<- as.ltraj(xy = utmcoord[, c("X", "Y")], date = results2$datetime, id = "15940")
plot(trajutm)
c<-locutm$X
d<-locutm$Y
cd<-cbind(c,d)
locutmsp<-SpatialPoints(cd)

#convert station lcoaitns to UTM
statdd<-data.frame(cbind(statinfo$Longitude,statinfo$Latitude))
names(statdd)<-c("X", "Y")
attr(statdd,"zone")<-55
attr(statdd,"projection")<-"LL"
statutm<-convUL(statdd,km=FALSE)


#make a home range
#1. kud
#x<-results2$meanlong
#y<-results2$meanlat
#xy<-cbind(x,y)
#xysp<-SpatialPoints(xy)
kud <- kernelUD(locutmsp, h = 400, grid=100,extent=0.5)
#windows()
#image(kud)
ver95 <- getverticeshr(kud,95)
ver50 <- getverticeshr(kud,50)
pdf("KUDplot.pdf")
plot(statutm$X,statutm$Y,col="green",pch=2,xlab="Longitude",ylab="Latitude",asp=0.7,cex=0.7)
plot(ver95, add = TRUE, lwd = 1,asp=0.7)
plot(ver50, add = TRUE, lwd = 2,asp=0.7)
dev.off()
kudarea <- kernel.area(kud, percent = seq(50, 95, by = 5),unin="m",unout="km2")
sink("KUD areas.txt")
kudarea
sink()



#end of script
