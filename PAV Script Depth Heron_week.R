#
library (adehabitatHR)
library (adehabitatLT)
library(chron)
library(ks)
library(lattice)
library(maptools)
library(PBSmapping)
library(rgdal)

#######################
######To join files in one excel file
#setwd("C:/Users/asus/OneDrive - James Cook University/FIELDWORK/Data/Ch4_Telemetry/Heron/Final Merge_Heron/Vue Tag Export_ Depth/Heron_PAV_all")

setwd("C:/Users/jc311121/OneDrive - James Cook University/FIELDWORK/Data/Ch4_Telemetry/Heron/Final Merge_Heron/Vue Tag Export_ Depth/Heron_KUD_week_all")
filenames<- list.files()
doit<-do.call("rbind", lapply(filenames, read.csv, header = TRUE))
write.csv(doit,file="Heron_kud_week_all.csv")


################
####below creates a function that allows you to make folders, further below (good for creating folders for each ind.)
folder<-function(z,y){
  z<-paste(y,what,sep="_")
  FN<-substitute(z)
  FN<-as.character(FN)
  x<-paste(getwd(),FN,sep="/")
  dir.create(x)
}

##set working directory
#setwd("C:/Users/asus/OneDrive - James Cook University/FIELDWORK/Data/Ch4_Telemetry/Heron/Final Merge_Heron/Vue Tag Export_ Depth/Test")

setwd("C:/Users/jc311121/OneDrive - James Cook University/FIELDWORK/Data/Ch4_Telemetry/Heron/Final Merge_Heron/Vue Tag Export_ Depth/Test")


tagdata <- read.csv("1210P.csv")
#head(tagdata)
#str(tagdata)

### SCRIPT IS FOR WEEKLY LOOP
## need to use mergedatasorted files for it to create new PAV files

## START LOOP TO GET ALL PAV FILES FROM WD
csv <- list.files()
csv

## LOOP STARTS
for(j in 1:length(csv)){
  #setwd("C:/Users/asus/OneDrive - James Cook University/FIELDWORK/Data/Ch4_Telemetry/Heron/Final Merge_Heron/Vue Tag Export_ Depth/Test")
  setwd("C:/Users/jc311121/OneDrive - James Cook University/FIELDWORK/Data/Ch4_Telemetry/Heron/Final Merge_Heron/Vue Tag Export_ Depth/Test")
  tagdata <- read.csv(csv[j],header=TRUE,sep=",")
  
  
  
  head(tagdata)
  #str(tagdata)
  #get the data from the time period	
  
  #setwd("C:/Users/jc167551/Dropbox/Cap Bunk vrl/BTS")
  
  #read in a tag data file
  #
  #tagdata <- read.csv(choose.files(),header=TRUE,sep=",")
  #summary(tagdata)
  #
  names(tagdata)[names(tagdata)=="Receiver"]<-"Receiver.S.N.x"
  names(tagdata)[names(tagdata)=="Transmitter"]<-"IDx"
  names(tagdata)[names(tagdata)=="Date..UTC."]<-"Dates"
  names(tagdata)[names(tagdata)=="Time..UTC."]<-"Times"
  names(tagdata)[names(tagdata)=="Sensor.Value"]<-"Depth"
  
  
 # head(tagdata)
  ## need to create date and time column merged
  tagdata$Date.Time <- as.POSIXct(paste(tagdata$Dates, tagdata$Times), format="%Y-%m-%d %H:%M:%S")
  ## even though it says AEST its actually UTC
 # head(tagdata)
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
  
  
  #create character value for field names
  fname<-as.character(tagdata$ID)
  fname<-unique(fname)
  
  ##########################
  
  #setwd("C:/Users/asus/OneDrive - James Cook University/FIELDWORK/Data/Ch4_Telemetry/Heron/Final Merge_Heron/Vue Tag Export_ Depth/Heron_KUD_week")
  
  setwd("C:/Users/jc311121/OneDrive - James Cook University/FIELDWORK/Data/Ch4_Telemetry/Heron/Final Merge_Heron/Vue Tag Export_ Depth/Heron_KUD_week")
  
  ###below uses the function started above
  what<-"KUD_week"
  folder(folder_name,fname) # create a folder for each individual
  folder_name<-paste(fname,what,sep="_")
  wd2<-paste(getwd(),folder_name,sep="/")
  setwd(wd2) # set new folder for each individual
  
  ##############################
  
  #remove unwanted fields
  tagdata$Transmitter.Name<-NULL
  tagdata$Transmitter.Serial<-NULL
  tagdata$Sensor.Unit<-NULL
  tagdata$Station.Name<-NULL
  tagdata$Latitude<-NULL
  tagdata$Longitude<-NULL
  tagdata$Receiver.S.N.x<-NULL
  tagdata$IDx<-NULL
  tagdata$Date..UTC..1 <- NULL
  tagdata$Time..UTC..1 <- NULL
  
  str(tagdata)
  
  #This leaves the date-time field, tag code number and receiver numbers
  #
  #format the date and time correctly for R
  dt<-as.POSIXlt(strptime(as.character(tagdata$Date.Time),"%Y-%m-%d %H:%M:%S"))  
  dt
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
  head(finaldata)
  #
  #
  #  need to read in the details of the location and serial numbers of the stations
  #  file should have serial, latitude, longitude, name as headers 
  #  and info below (lat and long in decimal degrees)
  #
  #statinfo<-read.csv("C:/Users/asus/OneDrive - James Cook University/FIELDWORK/Data/Ch4_Telemetry/Heron/Final Merge_Heron/Heron vps set up file (1).csv")
  statinfo<-read.csv("C:/Users/jc311121/OneDrive - James Cook University/FIELDWORK/Data/Ch4_Telemetry/Heron/Final Merge_Heron/Heron vps set up file (1).csv")
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
  mergedatasorted$week<-as.numeric(strftime(mergedatasorted$dtnumlocal, format = "%U"))
  head(mergedatasorted)
  
  ##remove X columns
  
  mergedatasorted$X<-NULL
  mergedatasorted$X.1<-NULL
  mergedatasorted$X.2<-NULL
  mergedatasorted$X.3<-NULL
  mergedatasorted$X.4<-NULL
  mergedatasorted$X.5<-NULL
  mergedatasorted$X.6<-NULL
  
  mergedatasorted
  head(mergedatasorted)
  
  ##creating and exporting a new file of merge data sorted to the working directory
  #setwd("F:/FIELDWORK/Data/Ch4_Telemetry/Heron/Final Merge_Heron/Vue Tag Export_ Depth/Results")
  fname2<-paste("MDSdata",fname,sep="")
  fname2<-paste(fname2,".csv",sep="")
  write.csv(mergedatasorted,file=fname2)
  
  #run through mergedata with the set time frame to calculate the mean postions
  #
  ## LOOP FOR WEEKLY
  weekly <- strftime(mergedatasorted$dtnumlocal, "%Y-%m-%U") ## used for all data 
  mergedatasorted$weekly <- weekly
  M <- split(mergedatasorted, weekly) ## used for all data, creates separate files for each week, another form of subsetting
  l <- length(M) ## number of rows dataframe
  m <- unique(weekly) # this is for loop  
  head(mergedatasorted)
  
  M_data <- M
  for (q in 26:30){ #for (q in 1:l)don't use 'j' here it is already being used for your individual
    print(paste('q loop=',q))
  
  M_data <- M[[q]] #you had 1 here instead of 'q' so it was only taking first week.
  head(M_data)
  
  if(nrow(M_data)>20) { # so it will only  run if number of detections each week is less than 20...is that right? I changed it to more than 20 here.
  
  ## START LOOP FOR MONTH HERE (above time step)
  ## this uses the mergedatasorted file and make PAV files for each month
  #monthly <- strftime(mergedatasorted$dtnumlocal, "%Y-%m") ## used for all data 
  #mergedatasorted$monthly <- monthly
  #M <- split(mergedatasorted, monthly) ## used for all data, creates separate files for each month, another form of subsetting
  #l <- length(M) ## number of rows dataframe
  #m <- unique(monthly) # this is for loop
  #head(mergedatasorted)
  
  #for (j in 1:l){
    
   # M_data <- M[[j]]
    
    #if(nrow(M_data)>20){ ## only run code on the data that has nrows in month > 20
      
      
      ## test loop by making j= number , if that works - all good!
      
      #set the time frame for position averaging 7200- is setting average depth position every 2 hours, I think I would like every 15 mins to start with , thats 900 time steps
      #
      timestep=900
      #
      #get the start date and time from the file
      #
      starttime<-M_data$dtnumlocal[1] 
      starttime24 <- starttime + 86400
      bt<-nrow(M_data)
      if(bt>0){ #otherwise if endtime is within first day which i deleted it will not be logical
      bt
      #bt<-b[1]
      endtime<-M_data$dtnumlocal[bt]
      starttime
      starttime24
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
      if((endtime-starttime24)<0) next ## HERE I WANT IT TO SAY, IF THERE IS NO VALUE FOR WEEK SKIP IT, OR IF THERE IS NO TIME FOR WEEK SKIP TO THE NEXT ONE
      #
      #add a timestep (in seconds) 30 minutes is 1800 seconds
      #
      #nexttime<-st+timestep
      #nexttime
      #
      #generate a sequence from start to end times by the specified time step
      #
      #if(endtime-starttime24>0.5) { #need this in case there are not more than one time step in datapoints between starttime+24 hours and endtime
      ex<-seq(from=starttime24, to=endtime, by=timestep)
      #if((endtime-starttime24)<0) next - MAYBE USE FOR MURRAYS STEP
      ex
      ex2<-ex+(timestep/2)
      #ex2
      
      #loop through all possible time steps to calculate mean positions
      #
      #calculate the number of elements in the ex frame (c)
      #
      c<-length(ex2)
      c
      ## need to add a column of 'location' to mergedatasorted to say its heron
      
      M_data$Reef <- "HIR"
      M_data
      str(M_data)
      
      # loop da loop is separating data into lots of 15 minute bunches to get movements over 15 mins
      #start a loop of length c
      # call Reef "HIR" -1 (better when we have merged heron and opal)
      M_data$reefnum<-array(0,bt)
      
      
      ##remove data from after last full download (March 2015)
      #finishup<-as.POSIXlt(strptime(as.character("10/03/2015"),"%d/%m/%Y"))
      #mergedatasorted<-mergedatasorted[-which(mergedatasorted$dtnumlocal>finishup),]
      #finaldata<-finaldata[-which(finaldata$dtnumlocal>finishup),]
      
      finaldata <- M_data
      head(finaldata)
      
      hab<-array("N",c)
      habnum<-array(0,c)
      
      meanlat<-0
      meanlong<-0
      numhits<-0
      rec<-0
      meandepth<-0
      reefkm<-0
      for(i in 1:c){
        print(paste('i=',i))
        #get the data from the time period	
        calcframe<-M_data[M_data$dtnumlocal<=ex[i+1]&M_data$dtnumlocal>ex[i],]
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
      ### can plot different things this graph is just depth and day of year
      ## RESULTS2 ARE: Position averaging data - this reduces autocorrelation 
      ## MERGEDATA SORTED: Is raw data - (issues for independence and autocorrelation)
      
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
      #plot(results$weekly, -results$meandepth)
      
      
      #results 2 removes NA's and just deals with the data where ind is present
      #
      
      
      #new data frame with only time steps with positions (results2)
      results2<-results[which(results$rec>0),]
      #rename ex2 to datetime
      results2$dtnumlocal<-results2$ex2
      results2$ex2<-NULL
      results2$datetime+36000
      results2$month<-as.factor(format(results2$datetime,"%m"))
      results2$year<-as.factor(format(results2$datetime,"%Y"))
      results2$hour<-as.factor(format(results2$datetime,"%H"))
      results2$weekly<-as.factor(format(results2$datetime,"%U"))
      results2$day<-as.Date(results2$dtnumlocal)
      results2$tag<-as.factor(fname)
      results2$doy<-as.numeric(strftime(results2$datetime, format = "%j"))
      results2$dom<-days(results2$datetime)
      results2$mon<-as.numeric(cut(results2$datetime, "months")) 
      results2$hab<-as.factor(results2$hab)
      
      
      fname1b <- paste(fname, m[q], "weekly_depth", ".pdf", sep="_")
      pdf(file=fname1b, width=11, height=8)
      
      
      #plot(results2$weekly, results2$meandepth)
      #head(results2)
      
      
      ##there is NA at bottom of data set, so need this line to remove it
      last <- nrow(results2)
      results2 <- results2[-last,]
      
      #calculate the 2D kernel ## 
      ## cant use this cause of reefkm
      #dd<-data.frame(results2$reefkm,results2$meandepth)
      ##auto bandwidth selection
      
      #H.pi2<-array(c(254552.40455,31.3245429,31.3245429,0.6484481),c(2,2)) ##THIS IS FOR KERNEL
      #H.pi2 <- Hpi(dd,binned=TRUE)*1
      #ddhat<-kde(dd,H=H.pi2)
      
      
      #dev.new(width=12, height=4)
      #par( mfcol= c(1, 1))
      
      #heron plot
      #par(mar=c(5,5,3,2))
      #plot(ddhat,cont=c(95),drawpoints=TRUE,col="black",xlab="Reef distance (m)", ylab="Depth (m)",col.pt="grey",cex=0.6,xlim=c(0,27000),ylim=c(30,0),main="Heron") 
      #plot(ddhat,cont=c(50),add=TRUE,col="dark orange")
      #plot(ddhat,cont=c(75),add=TRUE,col="dark green")
      #plot(ddhat,cont=c(25),add=TRUE,col="red")
      #polygon(c(0,0,5968,5968),c(0,30,30,0), col="green", border = "green",density=0.01) 
      #polygon(c(19773,19773,27000,27000),c(0,30,30,0), col="green", border = "green",density=0.01) 
      #polygon(c(5970,5970,19773,19773),c(0,30,30,0), col="yellow", border = "yellow",density=0.01)
      
      #results2
      #plot location of receivers and the positions calculated
      #pdf("locationplot.pdf")
      par( mfcol= c(1, 1))
      plot(statinfo$Longitude,statinfo$Latitude,col="green",pch=2,xlab="Longitude",ylab="Latitude",asp=0.7,cex=0.7)
      points(results2$meanlong,results2$meanlat)
      ##dev.off() this is to save .pdf
      #
      #write out data file with mean positions
      ## PAV may not be impacted by month loop
      #fname1<-paste("PAV",fname,sep="")
      #fname1<-paste(fname1,".csv",sep="")
      #results3<-results2
      #write.csv(results2,file=fname1)
      #
      #save(results3,file="test.Rdata")
      
      #animate the plot
      #par( mfcol= c(1, 1))
      #for (i in 1:c){
      #plot(statinfo$Longitude,statinfo$Latitude,col="green",pch=2,xlab="Longitude",ylab="Latitude",main=results$ex2[i],asp=0.7,cex=0.7)
      #points(results$meanlong[i],results$meanlat[i],cex=(1+(results$depthcat[i]/2)))
      #for (j in 1:50000){ss<-j^2}
      #}
      
      #display a depth histogram
      #pdf("Depth histograms.pdf")
      #par( mfcol= c(1, 2))
      #hist(results2$meandepth,main="Mean",xlab="Depth (m)") ## PAV DATA
      #hist(tagdata$Depth,main="Individual",xlab="Depth (m)") ## RAW DATA
      
      ## create an 'if' statement to run loop to say if true / false
      
      if(length(unique(results2$meanlong))>1){
        if(nrow(results2)>=20){ ##use this to say if the number of rows (detections) are < 20 in a week, dont bother analysing
        
        #create a trajectory
        #1. decimal degrees
        locdd<-data.frame(cbind(results2$meanlong,results2$meanlat))
        names(locdd)<-c("X", "Y")
        hrtraj<- as.ltraj(xy = locdd[, c("X", "Y")], date = results2$datetime, id = "15940")## what is this id no?
        #pdf("Trajectory.pdf")
        plot(hrtraj,xlab="Longitude",ylab="Latitude")
        
        #2. utm
        locdd<-data.frame(cbind(results2$meanlong,results2$meanlat))
        names(locdd)<-c("X", "Y")
        attr(locdd,"zone")<-55
        attr(locdd,"projection")<-"LL"
        locutm<-convUL(locdd,km=FALSE)
        utmcoord<-data.frame(cbind(locutm$X,locutm$Y))
        names(utmcoord)<-c("X", "Y")
        trajutm<- as.ltraj(xy = utmcoord[, c("X", "Y")], date = results2$datetime, id = results2$tag)
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
        ## h- smoothing factor 
        ## package 'ks' - READ UP
        kud <- kernelUD(locutmsp, h = 100, grid = 100, extent= 10) ## ask colin/michelle about h smoothing factor , ** read Matley's paper
        #windows()
        image(kud)
        
        ver95 <- getverticeshr(kud,95)
        ver50 <- getverticeshr(kud,50)
        ver95
        ver50
        kudarea <- kernel.area(kud, percent = seq(50, 95, by=5),unin="m",unout="km2")
        #kudarea
        
        #vud <- getvolumeUD(kud)
        #vud
        #pdf("KUDplot.pdf")
        par( mfcol= c(1, 1))
        plot(statutm$X,statutm$Y,col="green",pch=2,xlab="Longitude",ylab="Latitude",asp=0.7,cex=0.7)
        plot(ver95, add = TRUE, lwd = 1,asp=0.7)
        plot(ver50, add = TRUE, lwd = 2,asp=0.7)
        
        #if you want jpeg as output, to save ggplots and mroe advanced plotting: e.g. ggsave ** REMEMEMBER **
        
        ## extract the minimum distance of trout movement in study
        
        # Convert the ltraj object into a spatial lines dataframe
        results2.traj <- ltraj2sldf(trajutm,byid=TRUE)
        
        # Extract the lengths of these lines in meters ***
        
        results2.traj$Distance <- SpatialLinesLengths(results2.traj)
        
        Dist_km <- results2.traj$Distance/1000
        
        ## output of distance and homerange into text file 
        ## output of distance and homerange into text file 
        week_value <- as.numeric(strsplit(m[q],"-")[[1]][3])
        year_value<- as.numeric(strsplit(m[q], "-")[[1]][1])
        month_value <- as.numeric(strsplit(m[q],"-") [[1]][2])
        homerange <- data.frame(fname, m[q], kudarea, nrow(results2), Dist_km, week_value, year_value, month_value)
        write.csv(homerange, paste(fname, m[q], "_kud_week.csv", sep="_"))
        #write.csv(results3, paste("dist", fname, m[j], ".csv", sep="_"))
      }###in case there are only 1 location
      }##in case there are not 6 locations
      
      ## so this is if length meanlong =1 tell R to do this...
      if(length(unique(results2$meanlong))==1){
        #if(nrow(results2)>=5){
        kudarea <- matrix(12345,2) ##this automatically assigns homerange as 12345 so I know that it is no good
        Dist_km <- matrix(0,2)
        week_value <- as.numeric(strsplit(m[q],"-")[[1]][3])
        year_value<- as.numeric(strsplit(m[q], "-")[[1]][1])
        month_value <- as.numeric(strsplit(m[q],"-") [[1]][2])
        homerange <- data.frame(fname, m[q], kudarea, nrow(results2), Dist_km, week_value, year_value, month_value)
        write.csv(homerange, paste(fname, m[q], "_kud_week.csv", sep="_"))
        
      }
      
      dev.off()
      #sink()
      }#this is for the new starttime24 line i added
      }#this is for new bt<0 line i added
  }
  } # i think this is closing the weekly loop?
#} DELETED THIS BRACKET 8/3/18

## getting error from this script because there are not enough detection points for month=11 to make a home range estimate

