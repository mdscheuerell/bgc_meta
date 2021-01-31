#Simple flux program - written by John Campbell 1/25/2021

#Install the "zoo" package
#install.packages("zoo")

#Use the zoo library
library("zoo")

#Remove all objects from the current workspace
rm(list=ls())

#Manually set begin and end dates - Must start and end on a day with a chemistry value
begin_date <- "1973-05-28"
end_date <- "2010-06-01"

#Import the streamflow file
streamflow_mm <- read.csv(file="D:/Uncertainty/Ecosphere/Ecosphere Ca paper/streamflow flux calculation/streamflow_mm.csv", 
header=TRUE, colClasses=c("character","numeric"), sep=",")

#Import the chemistry file
streamchem_mgL <- read.csv(file="D:/Uncertainty/Ecosphere/Ecosphere Ca paper/streamflow flux calculation/streamchem_mgL.csv", 
                      header=TRUE, colClasses=c("character","numeric"), sep=",")

#Replace missing values with NA
is.na(streamflow_mm) <- streamflow_mm==-99

#Format the date
streamflow_mm$DATE <- as.Date(streamflow_mm$DATE,"%m/%d/%Y")

#Add a column for Jun 1 WY
streamflow_mm$YEAR <- as.numeric(format(streamflow_mm$DATE, "%Y"))
streamflow_mm$WY=ifelse((streamflow_mm$YEAR%%4==0 & streamflow_mm$YEAR%%100!=0) | streamflow_mm$YEAR%%400==0,format(streamflow_mm$DATE - 152,"%Y"),format(streamflow_mm$DATE - 151,"%Y")) 

#Add a column for month
streamflow_mm$MONTH <- as.numeric(format(streamflow_mm$DATE, "%m"))

#reorders the columns
streamflow_mm <- streamflow_mm[c("DATE","WY","MONTH","str_mm")]

#Format the chemistry data
streamchem_mgL$DATE <- as.Date(streamchem_mgL$DATE,"%m/%d/%Y")

#Merge streaflow and chemistry data
streamflux <- merge(streamflow_mm, streamchem_mgL, by='DATE', all=TRUE)

#subset to start and end with a chemistry value
streamflux <- streamflux[(streamflux$DATE >= paste(begin_date) & 
                                  streamflux$DATE <= paste(end_date)),]

#infill chemistry using average of two closest values
streamflux$str_Ca_mgl_fill_avg <- c((na.locf(streamflux$str_Ca_mgl) + rev(na.locf(rev(streamflux$str_Ca_mgl))))/2)

#calculates daily flux (kg/ha) for average interpolation
streamflux$str_Ca_kgha <- c((streamflux$str_mm * streamflux$str_Ca_mgl_fill_avg)/100)

head(streamflux,5)
tail(streamflux,5)

#sum flux by month
streamflux_monthly <- aggregate(list(str_Ca_kgha=streamflux$str_Ca_kgha), by=list(MONTH=streamflux$MONTH,WY=streamflux$WY), FUN=sum)


#reorders the columns
streamflux_monthly <- streamflux_monthly[c("WY","MONTH","str_Ca_kgha")]

#sum flux by water year
streamflux_wateryear <- aggregate(list(str_Ca_kgha=streamflux$str_Ca_kgha), by=list(WY=streamflux$WY), FUN=sum)

#reorders the columns
streamflux_wateryear <- streamflux_wateryear[c("WY","str_Ca_kgha")]

#write the output
write.csv(streamflux_monthly, file = "D:/Uncertainty/Ecosphere/Ecosphere Ca paper/streamflow flux calculation/streamflux_monthly.csv", row.names=FALSE)
write.csv(streamflux_wateryear, file = "D:/Uncertainty/Ecosphere/Ecosphere Ca paper/streamflow flux calculation/streamflux_wateryear.csv", row.names=FALSE)



