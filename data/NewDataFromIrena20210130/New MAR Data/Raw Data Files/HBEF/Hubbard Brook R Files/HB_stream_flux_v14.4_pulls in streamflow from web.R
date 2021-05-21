# This program calculates HB stream fluxes (g/Ha) using a comma delimited 
# chemistry concentration file and a daily watershed flow (mm) file.
# written by John Campbell and Bob Smith

#Install packages
#install.packages("zoo")
#install.packages("plyr")

#Use the libraries
library("zoo")
library("plyr")

# remove all objects from the current workspace
rm(list=ls())

# set working directory to current directory
current_path <- 
"D:/hubbard_brook_data/HB fluxes/HB_Stream_flux_R_program"
#"E:/HB fluxes/HB_Stream_flux_R_program"

setwd(current_path)

######Change these prior to running the program#################################

#Manually set end date
end_date <- "2019-06-02"

#Manually select watershed
#watershed <- "9"

#Loop through all watersheds (bracket at the end of the program also)
for (ws in c(1,2,3,4,5,6,7,8,9)){
  watershed <- paste(ws)

#Automatically set begin date
if(watershed==7 || watershed==9){
    begin_date <- as.Date("1995-05-30")
  } else {
    if(watershed==8){
#Big gap in data      
#      begin_date <- as.Date("1974-07-15")
      begin_date <- as.Date("1995-05-30")      
    } else {begin_date <- as.Date("1963-06-01")
    }}

#reads in the streamflow file and creates subfiles
#stream_vol_in <- read.csv(file="HBEF_DailyStreamflow_1956-2020.csv")

#stream_vol_in <- data.frame(stream_vol_in)

#stream_vol_in$DATE <- as.Date(stream_vol_in$DATE, "%Y-%m-%d")

# Package ID: knb-lter-hbr.2.11 Cataloging System:https://pasta.edirepository.org.
# Data set title: Hubbard Brook Experimental Forest: Daily Streamflow by Watershed, 1956 - present.
# Data set creator:    - USDA Forest Service, Northern Research Station 
# Contact:    -  Hubbard Brook Ecosystem Study  - hbr-im@lternet.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/2/11/1254d17cbd381556c05afa740d380e78" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


stream_vol_in <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "DATE",     
                 "WS",     
                 "Streamflow",     
                 "Flag"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert stream_vol_in$DATE dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1DATE<-as.Date(stream_vol_in$DATE,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1DATE) == length(tmp1DATE[!is.na(tmp1DATE)])){stream_vol_in$DATE <- tmp1DATE } else {print("Date conversion failed for stream_vol_in$DATE. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1DATE) 
if (class(stream_vol_in$WS)!="factor") stream_vol_in$WS<- as.factor(stream_vol_in$WS)
if (class(stream_vol_in$Streamflow)=="factor") stream_vol_in$Streamflow <-as.numeric(levels(stream_vol_in$Streamflow))[as.integer(stream_vol_in$Streamflow) ]               
if (class(stream_vol_in$Streamflow)=="character") stream_vol_in$Streamflow <-as.numeric(stream_vol_in$Streamflow)
if (class(stream_vol_in$Flag)=="factor") stream_vol_in$Flag <-as.numeric(levels(stream_vol_in$Flag))[as.integer(stream_vol_in$Flag) ]               
if (class(stream_vol_in$Flag)=="character") stream_vol_in$Flag <-as.numeric(stream_vol_in$Flag)

# Convert Missing Values to NA for non-dates

stream_vol_in$Flag <- ifelse((trimws(as.character(stream_vol_in$Flag))==trimws("NA")),NA,stream_vol_in$Flag)               
suppressWarnings(stream_vol_in$Flag <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_vol_in$Flag))==as.character(as.numeric("NA"))),NA,stream_vol_in$Flag))


# Here is the structure of the input data frame:
str(stream_vol_in)                            
attach(stream_vol_in)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(DATE)
summary(WS)
summary(Streamflow)
summary(Flag) 
# Get more details on character variables

summary(as.factor(stream_vol_in$WS))
detach(stream_vol_in)     

head(stream_vol_in,5)

#subset by watershed
stream_vol_in <- stream_vol_in[(stream_vol_in$WS==watershed),]

stream_vol_in <- stream_vol_in[(stream_vol_in$DATE >= paste(begin_date) & 
                                  stream_vol_in$DATE <= paste(end_date)),]

stream_vol_in <- stream_vol_in[, !(colnames(stream_vol_in) %in% c("WS"))]

write.csv(stream_vol_in, file=paste("ws",watershed,"_str_flow.csv", sep=""), row.names=FALSE)


################################################################################

# reads in stream chemistry file
stream_chem <- read.table(file=paste("ws",watershed,"_stream_chem.csv", sep=""),
                    header=TRUE,sep=",", 
                    colClasses=c("numeric","character","character","character",
                                 "character","numeric","numeric","numeric",
                                 "numeric","numeric", "numeric","numeric",
                                 "numeric","numeric","numeric","numeric",
                                 "numeric","numeric","numeric","numeric",
                                 "numeric","numeric","numeric","numeric",
                                 "numeric","numeric","numeric","numeric",
                                 "numeric","numeric","numeric","numeric",
                                 "numeric","numeric","numeric","numeric",
                                 "character","character","character","numeric",
                                 "numeric","numeric","character"))

# reads in streamflow file
stream_flow <- read.table(file=paste("ws",watershed,"_str_flow.csv", sep=""),
                          header=TRUE,sep=",", 
                          colClasses=c("character","numeric"))

# convert stream chem dates
stream_chem$DATE <- as.Date(stream_chem$Date,"%m/%d/%Y")
stream_chem <- subset(stream_chem, select = -c(Date,Day))

# Deals with -888.88 values
fill_NAs <- function(test_NA) {
  # fill in missing NAs with appropriate chemistry
  lenData <- nrow(test_NA)
  firstNA <- TRUE
  lastNA <- FALSE
  # this for loop is for concentration columns numbers 
  # and has to be changed if addition or deletion of future columns
  for (l in 7:31) {
    # this for loop is for rows in the data set
    for (i in 1:(lenData-1)) {
      # looking for NA values
      if (is.na(test_NA[i,l])) {
        if(firstNA) {
          first <- i
          # next statement handles cases where a run of NAs consists of one
          if (!is.na(test_NA[i+1,l]))
          {
             last <- first
             if (test_NA[first-1,l] < 0 && test_NA[last+1,l] < 0)
               test_NA[i,l] <- test_NA[last+1,l]
             else if (test_NA[first-1,l] < 0 && test_NA[last+1,l] >= 0)
               test_NA[i,l] <- test_NA[first-1,l]
             else if (test_NA[first-1,l] >= 0 && test_NA[last+1,l] < 0)
               test_NA[i,l] <- test_NA[last+1,l]
             else 
               test_NA[i,l] <- (test_NA[first-1,l] + test_NA[last+1,l])/2             
          }
          else {
          firstNA <- FALSE
          lastNA <- TRUE
        }
        }
        else if (lastNA) {
          # looking for last NA in a run of NAs by finding next good chem data
          if (is.na(test_NA[i,l]) && !is.na(test_NA[i+1,l])) {
            last <- i
            for (rep in first:last) {
              if (test_NA[first-1,l] < 0 && test_NA[last+1,l] < 0)
                test_NA[rep,l] <- test_NA[last+1,l]
              else if (test_NA[first-1,l] < 0 && test_NA[last+1,l] >= 0)
                test_NA[rep,l] <- test_NA[first-1,l]
              else if (test_NA[first-1,l] >= 0 && test_NA[last+1,l] < 0)
                test_NA[rep,l] <- test_NA[last+1,l]
              else 
                test_NA[rep,l] <- (test_NA[first-1,l] + test_NA[last+1,l])/2
            }
            firstNA <-TRUE
            lastNA <- FALSE
          }
        }
      }
    }
  }
  return (test_NA)
}



# makeYearMonth adds variables Year and Month to stream_monthly_flux_gHa
# These two variables are constructed from the chararcter varialble Year_Month
# which was the aggregating varible used to contruct the stream_monthly_flux_gHa
# dataset. The function returns the updated version of stream_monthly_flux_gHa
makeYearMonth <- function(monthly_data)
{
  temp_Date <- monthly_data$Year_Month
  tYear <- substr(temp_Date,1,4)
  tMonth <-  substr(temp_Date,6,7)
  Year <- as.numeric(tYear)
  Month <- as.numeric(tMonth)

  Year <- as.matrix(Year)
  Month <- as.matrix(tMonth)
  
  monthly_data <- cbind(monthly_data,Year,Month)
  
  monthly_data <- 
    monthly_data[c("Year","Month","Year_Month","flow_mm","Ca_flux","Mg_flux",
                   "K_flux","Na_flux","Al_Ferron_flux","TMAl_flux","OMAl_flux",
                   "Al_ICP_flux","NH4_flux","H_flux","SO4_flux","NO3_flux",
                   "Cl_flux","PO4_flux","DOC_flux","TDN_flux","DON_flux",
                   "DIC_flux","SiO2_flux","Mn_flux","Fe_flux","F_flux",
                   "ANC_flux","SpecCond_flux","TheoryCond_flux")]
  return (monthly_data) 
} 

# missingData assigns NA to concentrations equal to -999 missing data
missingData <- function(stream_chem)
{
  len <- nrow(stream_chem)
  
for (i in 1:len)
{  
  if (!is.na(stream_chem$Ca[i]) && stream_chem$Ca[i] < -900)
    stream_chem$Ca[i] <- NA
  if (!is.na(stream_chem$Mg[i]) && stream_chem$Mg[i] < -900)
    stream_chem$Mg[i] <- NA
  if (!is.na(stream_chem$K[i]) && stream_chem$K[i] < -900)
    stream_chem$K[i] <- NA
  if (!is.na(stream_chem$Na[i]) && stream_chem$Na[i] < -900)
    stream_chem$Na[i] <- NA
  if (!is.na(stream_chem$Al_Ferron[i]) && stream_chem$Al_Ferron[i] < -900)
    stream_chem$Al_Ferron[i] <- NA
  if (!is.na(stream_chem$TMAl[i]) && stream_chem$TMAl[i] < -900)
    stream_chem$TMAl[i] <- NA
  if (!is.na(stream_chem$OMAl[i]) && stream_chem$OMAl[i] < -900)
    stream_chem$OMAl[i] <- NA
  if (!is.na(stream_chem$Al_ICP[i]) && stream_chem$Al_ICP[i] < -900)
    stream_chem$Al_ICP[i] <- NA
  if (!is.na(stream_chem$NH4[i]) && stream_chem$NH4[i] < -900)
    stream_chem$NH4[i] <- NA
  if (!is.na(stream_chem$pH[i]) && stream_chem$pH[i] < -900)
    stream_chem$pH[i] <- NA
  if (!is.na(stream_chem$SO4[i]) && stream_chem$SO4[i] < -900)
    stream_chem$SO4[i] <- NA
  if (!is.na(stream_chem$NO3[i]) && stream_chem$NO3[i] < -900)
    stream_chem$NO3[i] <- NA
  if (!is.na(stream_chem$Cl[i]) && stream_chem$Cl[i] < -900)
    stream_chem$Cl[i] <- NA
  if (!is.na(stream_chem$PO4[i]) && stream_chem$PO4[i] < -900)
    stream_chem$PO4[i] <- NA
  if (!is.na(stream_chem$DOC[i]) && stream_chem$DOC[i] < -900)
    stream_chem$DOC[i] <- NA
  if (!is.na(stream_chem$TDN[i]) && stream_chem$TDN[i] < -900)
    stream_chem$TDN[i] <- NA
  if (!is.na(stream_chem$DON[i]) && stream_chem$DON[i] < -900)
    stream_chem$DON[i] <- NA
  if (!is.na(stream_chem$DIC[i]) && stream_chem$DIC[i] < -900)
    stream_chem$DIC[i] <- NA  
  if (!is.na(stream_chem$SiO2[i]) && stream_chem$SiO2[i] < -900)
    stream_chem$SiO2[i] <- NA 
  if (!is.na(stream_chem$Mn[i]) && stream_chem$Mn[i] < -900)
    stream_chem$Mn[i] <- NA
  if (!is.na(stream_chem$Fe[i]) && stream_chem$Fe[i] < -900)
      stream_chem$Fe[i] <- NA
  if (!is.na(stream_chem$F[i]) && stream_chem$F[i] < -900)
        stream_chem$F[i] <- NA
  if (!is.na(stream_chem$ANC[i]) && stream_chem$ANC[i] < -900)
        stream_chem$ANC[i] <- NA  
  if (!is.na(stream_chem$SpecCond[i]) && stream_chem$SpecCond[i] < -900)
        stream_chem$SpecCond[i] <- NA
  if (!is.na(stream_chem$TheoryCond[i]) && stream_chem$TheoryCond[i] < -900)
        stream_chem$TheoryCond[i] <- NA
}
return (stream_chem) 
} 

# this function returns a missing -888 value with the appropriate number of 
# decimals places for any NA value for either monthly
# volwt conc or WY volwt conc. The function matches column names using the 
# column number from function fix_NA.
missingNum <- function(df,c) {
  if (colnames(df)[c] == "volwt_Al_Ferron" | colnames(df)[c] == 
      "volwt_TMAl" | colnames(df)[c] == "volwt_OMAl" | colnames(df)[c] == 
      "volwt_NH4" | colnames(df)[c] == "volwt_NO3" | colnames(df)[c] == 
      "volwt_PO4" 
    | colnames(df)[c] == "volwt_ANC" | colnames(df)[c] == 
    "volwt_SpecCond" | colnames(df)[c] == "volwt_TheoryCond")
    return(-888.888)
  else if (colnames(df)[c] == "volwt_H")
    return(-888.8888)  
  else
    return(-888.88)
}

# This function loops through row and columns looking for NA values in the 
# volwt conc files for month and WY data. The function calls
# function missingNum to fill in NA with -888 and appropriate decimal places. 
fix_NA <- function(testdf) {
  rowLen <- nrow(testdf)
  colLen <- ncol(testdf)
  for (r in 1:rowLen) { 
    for (c in 1:colLen) {
      if (is.na(testdf[r,c])) {
        if (is.numeric(testdf[r,c]))
          testdf[r,c] <- missingNum(testdf,c)
      }
    }
  }
  return(testdf)
}

stream_chem_passed <- subset(stream_chem,stream_chem$Pass == "T")

stream_chem_passed <- stream_chem_passed[c("WS","DATE","EST","Ca","Mg","K","Na",
                                           "Al_Ferron","TMAl","OMAl","Al_ICP",
                                           "NH4","pH","SO4","NO3","Cl","PO4",
                                           "DOC","TDN","DON","DIC","SiO2","Mn",
                                           "Fe","F","ANC","Cation","Anion",
                                           "SpecCond","TheoryCond","Temp_C",
                                           "GageHt","Flow","Hydro","FieldCode",
                                           "VarCode","IonBal","IonErr","Qcode",
                                           "Pass")]

stream_chem_miss_passed <- missingData(stream_chem_passed)

#converts DIC in uM/L to mg/L
convert_DIC <- function(stream_chem)
{
  DIC_mgL <- (stream_chem$DIC*12.01115)/1000

  return (DIC_mgL)
}

#converts pH units to H+ mg/l
convert_pH <- function(stream_chem)
{
  temp <- -(stream_chem$pH)
  H <- 10**(temp)*1000   
  return (H)
}

stream_calc_pH <- function(pH,H)
{
  # converts H mg/l to pH to fill in missing (NA) pH
  missing <- is.na(pH)
  
  if (missing)
  {
    temp <- log10(H/1000)*-1
    pH <- temp
    return (pH)
  }
  else # pH not missing just return pH
    return (pH)
}

daily_stream_calc_pH <- function(pH,H)
{

# the daily conc file averages the daily pH Data by sample dates. 
# It needs to be recalculated using the daily H+ concentration. -888 for H+ 
# returns a pH value of -888.88 which indicates that the data wasn't sampled

  if (H < 0) 
    pH <- -888.88
  else
  { temp <- log10(H/1000)*-1
    pH <-temp }
    return (pH)
}
                                 
# filling in missing chem conc with average of good before & after values
# can't average pH must convert to H+
stream_chem_miss_passed$H <- 
  ifelse(is.na(stream_chem_miss_passed$pH),NA,
         ifelse((stream_chem_miss_passed$pH < 0),
                -888.88,convert_pH(stream_chem_miss_passed)))
stream_chem_miss_passed$H <- 
  ifelse(is.na(stream_chem_miss_passed$H),
         c((na.locf(stream_chem_miss_passed$H) + 
              rev(na.locf(rev(stream_chem_miss_passed$H))))/2),
         stream_chem_miss_passed$H)                                 
tail(stream_chem_miss_passed,5)
# convert DIC in uM to mg/L
stream_chem_miss_passed$DIC <- 
  ifelse(is.na(stream_chem_miss_passed$DIC),NA,
         ifelse((stream_chem_miss_passed$DIC < 0),-888,
                convert_DIC(stream_chem_miss_passed)))
stream_chem_miss_passed$Ca <- 
  c((na.locf(stream_chem_miss_passed$Ca) + 
       rev(na.locf(rev(stream_chem_miss_passed$Ca))))/2)
stream_chem_miss_passed$Mg <- 
  c((na.locf(stream_chem_miss_passed$Mg) + 
       rev(na.locf(rev(stream_chem_miss_passed$Mg))))/2)
stream_chem_miss_passed$K <- 
  c((na.locf(stream_chem_miss_passed$K) + 
       rev(na.locf(rev(stream_chem_miss_passed$K))))/2)
stream_chem_miss_passed$Na <- 
  c((na.locf(stream_chem_miss_passed$Na) + 
       rev(na.locf(rev(stream_chem_miss_passed$Na))))/2)
stream_chem_miss_passed$Al_Ferron <- 
  c((na.locf(stream_chem_miss_passed$Al_Ferron) + 
       rev(na.locf(rev(stream_chem_miss_passed$Al_Ferron))))/2)
stream_chem_miss_passed$TMAl <- 
  c((na.locf(stream_chem_miss_passed$TMAl) + 
       rev(na.locf(rev(stream_chem_miss_passed$TMAl))))/2)
stream_chem_miss_passed$OMAl <- 
  c((na.locf(stream_chem_miss_passed$OMAl) + 
       rev(na.locf(rev(stream_chem_miss_passed$OMAl))))/2)
stream_chem_miss_passed$Al_ICP <- 
  c((na.locf(stream_chem_miss_passed$Al_ICP) + 
       rev(na.locf(rev(stream_chem_miss_passed$Al_ICP))))/2)
stream_chem_miss_passed$NH4 <- 
  c((na.locf(stream_chem_miss_passed$NH4) + 
       rev(na.locf(rev(stream_chem_miss_passed$NH4))))/2)
stream_chem_miss_passed$SO4 <- 
  c((na.locf(stream_chem_miss_passed$SO4) + 
       rev(na.locf(rev(stream_chem_miss_passed$SO4))))/2)
stream_chem_miss_passed$NO3 <- 
  c((na.locf(stream_chem_miss_passed$NO3) + 
       rev(na.locf(rev(stream_chem_miss_passed$NO3))))/2)
stream_chem_miss_passed$Cl <- 
  c((na.locf(stream_chem_miss_passed$Cl) + 
       rev(na.locf(rev(stream_chem_miss_passed$Cl))))/2)
stream_chem_miss_passed$PO4 <- 
  c((na.locf(stream_chem_miss_passed$PO4) + 
       rev(na.locf(rev(stream_chem_miss_passed$PO4))))/2)
stream_chem_miss_passed$DOC <- 
  c((na.locf(stream_chem_miss_passed$DOC) + 
       rev(na.locf(rev(stream_chem_miss_passed$DOC))))/2)
stream_chem_miss_passed$TDN <- 
  c((na.locf(stream_chem_miss_passed$TDN) + 
       rev(na.locf(rev(stream_chem_miss_passed$TDN))))/2)
stream_chem_miss_passed$DON <- 
  c((na.locf(stream_chem_miss_passed$DON) + 
       rev(na.locf(rev(stream_chem_miss_passed$DON))))/2)
stream_chem_miss_passed$DIC <- 
  c((na.locf(stream_chem_miss_passed$DIC) + 
       rev(na.locf(rev(stream_chem_miss_passed$DIC))))/2)
stream_chem_miss_passed$SiO2 <- 
  c((na.locf(stream_chem_miss_passed$SiO2) + 
       rev(na.locf(rev(stream_chem_miss_passed$SiO2))))/2)
stream_chem_miss_passed$Mn <- 
  c((na.locf(stream_chem_miss_passed$Mn) + 
       rev(na.locf(rev(stream_chem_miss_passed$Mn))))/2)
stream_chem_miss_passed$Fe <- 
  c((na.locf(stream_chem_miss_passed$Fe) + 
       rev(na.locf(rev(stream_chem_miss_passed$Fe))))/2)
stream_chem_miss_passed$F <- 
  c((na.locf(stream_chem_miss_passed$F) + 
       rev(na.locf(rev(stream_chem_miss_passed$F))))/2)
stream_chem_miss_passed$ANC <- 
  c((na.locf(stream_chem_miss_passed$ANC) + 
       rev(na.locf(rev(stream_chem_miss_passed$ANC))))/2)
stream_chem_miss_passed$SpecCond <- 
  c((na.locf(stream_chem_miss_passed$SpecCond) + 
       rev(na.locf(rev(stream_chem_miss_passed$SpecCond))))/2)
stream_chem_miss_passed$TheoryCond <- 
  c((na.locf(stream_chem_miss_passed$TheoryCond) + 
       rev(na.locf(rev(stream_chem_miss_passed$TheoryCond))))/2)

stream_chem_miss_passed <- 
  stream_chem_miss_passed[c("WS","DATE","EST","Ca","Mg","K","Na","Al_Ferron",
                            "TMAl","OMAl","Al_ICP","NH4","SO4","NO3","Cl","PO4",
                           "DOC","TDN","DON","DIC","SiO2","Mn","Fe","F","ANC",
                           "Cation","Anion","SpecCond","TheoryCond","Temp_C",
                           "GageHt","Flow","Hydro","FieldCode","VarCode",
                           "IonBal","IonErr","Qcode","Pass","H","pH")]
head(stream_chem_miss_passed,5)
tail(stream_chem_miss_passed,5)
# Streamflow
#stream_flow$DATE <- as.Date(stream_flow$DATE,"%Y-%m-%d")

# next line would be used if we retained percent_est_obs
#stream_flow <- subset(stream_flow, select = -c(Date,percent_est_obs))

#put date 1st
stream_flow <- stream_flow[c("DATE","Streamflow")]
head(stream_flow,5)
#rename columns
colnames(stream_flow) <- c("DATE","flow_mm")



temp_date <- stream_flow$DATE
Year <- substr(temp_date,1,4)
Year <- as.matrix(Year)
Month <-  substr(temp_date,6,7)
Month <- as.matrix(Month)
Year_Month <- paste(Year,Month,sep="-")
Year_Month <- as.matrix(Year_Month)

# water year June 1st - May 31st
# assign water year depending if month < 6
water_year <- function(Year,Month)
{
  tYear <- as.numeric(Year)
  tMon <- as.numeric(Month)
  WYear <- ifelse(tMon<6,tYear-1,tYear)
  return (WYear)
}

WY <- water_year(Year,Month)
WY <- as.matrix(WY)

stream_flow <- cbind(stream_flow,Year,Month,Year_Month,WY)
stream_flow <- stream_flow[c("DATE","Year","Month","Year_Month","WY","flow_mm")]

stream_flow$DATE <- as.Date(stream_flow$DATE, "%Y-%m-%d")

stream_dailyChemConc <- merge(stream_flow, stream_chem_miss_passed, by='DATE', 
                              all=TRUE)

head(stream_dailyChemConc,5)

stream_dailyChemConc <- 
  subset(stream_dailyChemConc, select = 
           -c(WS,EST,Cation,Anion,Temp_C,GageHt,Flow,Hydro,FieldCode,VarCode,
              IonBal,IonErr,Qcode,Pass))

stream_dailyChemConc_filled <- fill_NAs(stream_dailyChemConc)

len_conc <- nrow(stream_dailyChemConc_filled)

head(stream_dailyChemConc_filled,5)
tail(stream_dailyChemConc_filled,5)

for (i in 1:len_conc)
{
  stream_dailyChemConc_filled$pH[i] <- 
    daily_stream_calc_pH(stream_dailyChemConc_filled$pH[i],
                         stream_dailyChemConc_filled$H[i])
}

stream_dailyChemConc_filled <- 
  stream_dailyChemConc_filled[(stream_dailyChemConc_filled$DATE >= 
                                 paste(begin_date) & 
                                 stream_dailyChemConc_filled$DATE <= 
                                 paste(end_date)),]

stream_daily_flux_gHa <- stream_dailyChemConc_filled

stream_daily_flux_gHa <- 
  stream_daily_flux_gHa[c("DATE","Year","Month","Year_Month","WY","flow_mm",
                          "Ca","Mg","K","Na","Al_Ferron","TMAl","OMAl",
                          "Al_ICP","NH4","pH","H","SO4","NO3","Cl","PO4","DOC",
                          "TDN","DON","DIC","SiO2","Mn","Fe","F","ANC",
                          "SpecCond","TheoryCond")]

stream_daily_flux_gHa[c("Ca_flux","Mg_flux","K_flux","Na_flux","Al_Ferron_flux",
                        "TMAl_flux","OMAl_flux","Al_ICP_flux","NH4_flux",
                        "H_flux","SO4_flux","NO3_flux","Cl_flux","PO4_flux",
                        "DOC_flux","TDN_flux","DON_flux","DIC_flux","SiO2_flux",
                        "Mn_flux","Fe_flux","F_flux","ANC_flux","SpecCond_flux",
                        "TheoryCond_flux")] <-NA

stream_daily_flux_gHa$Ca_flux <- 
  ifelse(stream_daily_flux_gHa$Ca < 
           0,NA,stream_daily_flux_gHa$Ca*stream_daily_flux_gHa$flow_mm*10)
stream_daily_flux_gHa$Mg_flux <- 
  ifelse(stream_daily_flux_gHa$Mg < 0,NA,
         stream_daily_flux_gHa$Mg*stream_daily_flux_gHa$flow_mm*10)
stream_daily_flux_gHa$K_flux <- 
  ifelse(stream_daily_flux_gHa$K < 0,NA,
         stream_daily_flux_gHa$K*stream_daily_flux_gHa$flow_mm*10)
stream_daily_flux_gHa$Na_flux <- 
  ifelse(stream_daily_flux_gHa$Na < 0,NA,
         stream_daily_flux_gHa$Na*stream_daily_flux_gHa$flow_mm*10)
stream_daily_flux_gHa$Al_Ferron_flux <- 
  ifelse(stream_daily_flux_gHa$Al_Ferron < 0,NA,
         stream_daily_flux_gHa$Al_Ferron*stream_daily_flux_gHa$flow_mm*10)
stream_daily_flux_gHa$TMAl_flux <- 
  ifelse(stream_daily_flux_gHa$TMAl < 0,NA,
         stream_daily_flux_gHa$TMAl*stream_daily_flux_gHa$flow_mm*10)
stream_daily_flux_gHa$OMAl_flux <- 
  ifelse(stream_daily_flux_gHa$OMAl < 0,NA,
         stream_daily_flux_gHa$OMAl*stream_daily_flux_gHa$flow_mm*10)
stream_daily_flux_gHa$Al_ICP_flux <- 
  ifelse(stream_daily_flux_gHa$Al_ICP < 0,NA,
         stream_daily_flux_gHa$Al_ICP*stream_daily_flux_gHa$flow_mm*10)
stream_daily_flux_gHa$NH4_flux <- 
  ifelse(stream_daily_flux_gHa$NH4 < 0,NA,
         stream_daily_flux_gHa$NH4*stream_daily_flux_gHa$flow_mm*10)
stream_daily_flux_gHa$SO4_flux <- 
  ifelse(stream_daily_flux_gHa$SO4 < 0,NA,
         stream_daily_flux_gHa$SO4*stream_daily_flux_gHa$flow_mm*10)
stream_daily_flux_gHa$NO3_flux <- 
  ifelse(stream_daily_flux_gHa$NO3 < 0,NA,
         stream_daily_flux_gHa$NO3*stream_daily_flux_gHa$flow_mm*10)
stream_daily_flux_gHa$Cl_flux <- 
  ifelse(stream_daily_flux_gHa$Cl < 0,NA,
         stream_daily_flux_gHa$Cl*stream_daily_flux_gHa$flow_mm*10)
stream_daily_flux_gHa$PO4_flux <- 
  ifelse(stream_daily_flux_gHa$PO4 < 0,NA,
         stream_daily_flux_gHa$PO4*stream_daily_flux_gHa$flow_mm*10)
stream_daily_flux_gHa$DOC_flux <- 
  ifelse(stream_daily_flux_gHa$DOC < 0,NA,
         stream_daily_flux_gHa$DOC*stream_daily_flux_gHa$flow_mm*10)
stream_daily_flux_gHa$TDN_flux <- 
  ifelse(stream_daily_flux_gHa$TDN < 0,NA,
         stream_daily_flux_gHa$TDN*stream_daily_flux_gHa$flow_mm*10)
stream_daily_flux_gHa$DON_flux <- 
  ifelse(stream_daily_flux_gHa$DON < 0,NA,
         stream_daily_flux_gHa$DON*stream_daily_flux_gHa$flow_mm*10)
stream_daily_flux_gHa$DIC_flux <- 
  ifelse(stream_daily_flux_gHa$DIC < 0,NA,
         stream_daily_flux_gHa$DIC*stream_daily_flux_gHa$flow_mm*10)
stream_daily_flux_gHa$SiO2_flux <- 
  ifelse(stream_daily_flux_gHa$SiO2 < 0,NA,
         stream_daily_flux_gHa$SiO2*stream_daily_flux_gHa$flow_mm*10)
stream_daily_flux_gHa$Mn_flux <- 
  ifelse(stream_daily_flux_gHa$Mn < 0,NA,
         stream_daily_flux_gHa$Mn*stream_daily_flux_gHa$flow_mm*10)
stream_daily_flux_gHa$Fe_flux <- 
  ifelse(stream_daily_flux_gHa$Fe < 0,NA,
         stream_daily_flux_gHa$Fe*stream_daily_flux_gHa$flow_mm*10)
stream_daily_flux_gHa$F_flux <- 
  ifelse(stream_daily_flux_gHa$F < 0,NA,
         stream_daily_flux_gHa$F*stream_daily_flux_gHa$flow_mm*10)
stream_daily_flux_gHa$ANC_flux <- 
  ifelse(stream_daily_flux_gHa$ANC < -888,NA,
         stream_daily_flux_gHa$ANC*stream_daily_flux_gHa$flow_mm*10)
stream_daily_flux_gHa$H_flux <- 
  ifelse(stream_daily_flux_gHa$H < 0,NA,
         stream_daily_flux_gHa$H*stream_daily_flux_gHa$flow_mm*10)
stream_daily_flux_gHa$SpecCond_flux <-
  ifelse(stream_daily_flux_gHa$SpecCond < 0,NA,
         stream_daily_flux_gHa$SpecCond*stream_daily_flux_gHa$flow_mm*10)
stream_daily_flux_gHa$TheoryCond_flux <- 
  ifelse(stream_daily_flux_gHa$TheoryCond < 0,NA,
         stream_daily_flux_gHa$TheoryCond*stream_daily_flux_gHa$flow_mm*10)

stream_daily_flux_gHa_new <- 
  stream_daily_flux_gHa[c("DATE","Year","Month","Year_Month","WY","flow_mm",
                          "Ca_flux","Mg_flux","K_flux","Na_flux",
                          "Al_Ferron_flux","TMAl_flux","OMAl_flux",
                          "Al_ICP_flux","NH4_flux","H_flux","SO4_flux",
                          "NO3_flux","Cl_flux","PO4_flux","DOC_flux",
                          "TDN_flux","DON_flux","DIC_flux","SiO2_flux",
                          "Mn_flux","Fe_flux","F_flux","ANC_flux",
                          "SpecCond_flux","TheoryCond_flux")]

#This section deletes the last month if there partial daily data
stream_daily_flux_gHa_new$Year <- 
  as.numeric(as.character(stream_daily_flux_gHa_new$Year))
min_year <- min(stream_daily_flux_gHa_new$Year)
max_year <- max(stream_daily_flux_gHa_new$Year)
stream_daily_flux_gHa_new$Month <- 
  as.numeric(as.character(stream_daily_flux_gHa_new$Month))

Jan_min<-length(which(stream_daily_flux_gHa_new$Year==min_year & 
                        stream_daily_flux_gHa_new$Month==1))
Feb_min<-length(which(stream_daily_flux_gHa_new$Year==min_year & 
                        stream_daily_flux_gHa_new$Month==2))
Mar_min<-length(which(stream_daily_flux_gHa_new$Year==min_year & 
                        stream_daily_flux_gHa_new$Month==3))
Apr_min<-length(which(stream_daily_flux_gHa_new$Year==min_year & 
                        stream_daily_flux_gHa_new$Month==4))
May_min<-length(which(stream_daily_flux_gHa_new$Year==min_year & 
                        stream_daily_flux_gHa_new$Month==5))
Jun_min<-length(which(stream_daily_flux_gHa_new$Year==min_year & 
                        stream_daily_flux_gHa_new$Month==6))
Jul_min<-length(which(stream_daily_flux_gHa_new$Year==min_year & 
                        stream_daily_flux_gHa_new$Month==7))
Aug_min<-length(which(stream_daily_flux_gHa_new$Year==min_year & 
                        stream_daily_flux_gHa_new$Month==8))
Sep_min<-length(which(stream_daily_flux_gHa_new$Year==min_year & 
                        stream_daily_flux_gHa_new$Month==9))
Oct_min<-length(which(stream_daily_flux_gHa_new$Year==min_year & 
                        stream_daily_flux_gHa_new$Month==10))
Nov_min<-length(which(stream_daily_flux_gHa_new$Year==min_year & 
                        stream_daily_flux_gHa_new$Month==11))
Dec_min<-length(which(stream_daily_flux_gHa_new$Year==min_year & 
                        stream_daily_flux_gHa_new$Month==12))

Jan_max<-length(which(stream_daily_flux_gHa_new$Year==max_year & 
                        stream_daily_flux_gHa_new$Month==1))
Feb_max<-length(which(stream_daily_flux_gHa_new$Year==max_year & 
                        stream_daily_flux_gHa_new$Month==2))
Mar_max<-length(which(stream_daily_flux_gHa_new$Year==max_year & 
                        stream_daily_flux_gHa_new$Month==3))
Apr_max<-length(which(stream_daily_flux_gHa_new$Year==max_year & 
                        stream_daily_flux_gHa_new$Month==4))
May_max<-length(which(stream_daily_flux_gHa_new$Year==max_year & 
                        stream_daily_flux_gHa_new$Month==5))
Jun_max<-length(which(stream_daily_flux_gHa_new$Year==max_year & 
                        stream_daily_flux_gHa_new$Month==6))
Jul_max<-length(which(stream_daily_flux_gHa_new$Year==max_year & 
                        stream_daily_flux_gHa_new$Month==7))
Aug_max<-length(which(stream_daily_flux_gHa_new$Year==max_year & 
                        stream_daily_flux_gHa_new$Month==8))
Sep_max<-length(which(stream_daily_flux_gHa_new$Year==max_year & 
                        stream_daily_flux_gHa_new$Month==9))
Oct_max<-length(which(stream_daily_flux_gHa_new$Year==max_year & 
                        stream_daily_flux_gHa_new$Month==10))
Nov_max<-length(which(stream_daily_flux_gHa_new$Year==max_year & 
                        stream_daily_flux_gHa_new$Month==11))
Dec_max<-length(which(stream_daily_flux_gHa_new$Year==max_year & 
                        stream_daily_flux_gHa_new$Month==12))

stream_daily_flux_gHa_new <- subset(stream_daily_flux_gHa_new, 
                                    !(Year==min_year & Month==1 & Jan_min<31))
stream_daily_flux_gHa_new <- subset(stream_daily_flux_gHa_new, 
                                    !(Year==min_year & Month==2 & Feb_min<28))
stream_daily_flux_gHa_new <- subset(stream_daily_flux_gHa_new, 
                                    !(Year==min_year & Month==3 & Mar_min<31))
stream_daily_flux_gHa_new <- subset(stream_daily_flux_gHa_new, 
                                    !(Year==min_year & Month==4 & Apr_min<30))
stream_daily_flux_gHa_new <- subset(stream_daily_flux_gHa_new, 
                                    !(Year==min_year & Month==5 & May_min<31))
stream_daily_flux_gHa_new <- subset(stream_daily_flux_gHa_new, 
                                    !(Year==min_year & Month==6 & Jun_min<30))
stream_daily_flux_gHa_new <- subset(stream_daily_flux_gHa_new, 
                                    !(Year==min_year & Month==7 & Jul_min<31))
stream_daily_flux_gHa_new <- subset(stream_daily_flux_gHa_new, 
                                    !(Year==min_year & Month==8 & Aug_min<31))
stream_daily_flux_gHa_new <- subset(stream_daily_flux_gHa_new, 
                                    !(Year==min_year & Month==9 & Sep_min<30))
stream_daily_flux_gHa_new <- subset(stream_daily_flux_gHa_new, 
                                    !(Year==min_year & Month==10 & Oct_min<31))
stream_daily_flux_gHa_new <- subset(stream_daily_flux_gHa_new, 
                                    !(Year==min_year & Month==11 & Nov_min<30))
stream_daily_flux_gHa_new <- subset(stream_daily_flux_gHa_new, 
                                    !(Year==min_year & Month==12 & Dec_min<31))

stream_daily_flux_gHa_new <- subset(stream_daily_flux_gHa_new, 
                                    !(Year==max_year & Month==1 & Jan_max<31))
stream_daily_flux_gHa_new <- subset(stream_daily_flux_gHa_new, 
                                    !(Year==max_year & Month==2 & Feb_max<28))
stream_daily_flux_gHa_new <- subset(stream_daily_flux_gHa_new, 
                                    !(Year==max_year & Month==3 & Mar_max<31))
stream_daily_flux_gHa_new <- subset(stream_daily_flux_gHa_new, 
                                    !(Year==max_year & Month==4 & Apr_max<30))
stream_daily_flux_gHa_new <- subset(stream_daily_flux_gHa_new, 
                                    !(Year==max_year & Month==5 & May_max<31))
stream_daily_flux_gHa_new <- subset(stream_daily_flux_gHa_new, 
                                    !(Year==max_year & Month==6 & Jun_max<30))
stream_daily_flux_gHa_new <- subset(stream_daily_flux_gHa_new, 
                                    !(Year==max_year & Month==7 & Jul_max<31))
stream_daily_flux_gHa_new <- subset(stream_daily_flux_gHa_new, 
                                    !(Year==max_year & Month==8 & Aug_max<31))
stream_daily_flux_gHa_new <- subset(stream_daily_flux_gHa_new, 
                                    !(Year==max_year & Month==9 & Sep_max<30))
stream_daily_flux_gHa_new <- subset(stream_daily_flux_gHa_new, 
                                    !(Year==max_year & Month==10 & Oct_max<31))
stream_daily_flux_gHa_new <- subset(stream_daily_flux_gHa_new, 
                                    !(Year==max_year & Month==11 & Nov_max<30))
stream_daily_flux_gHa_new <- subset(stream_daily_flux_gHa_new, 
                                    !(Year==max_year & Month==12 & Dec_max<31))

stream_monthly_flux_gHa <- 
  aggregate(stream_daily_flux_gHa_new[c("flow_mm","Ca_flux","Mg_flux","K_flux",
                                        "Na_flux","Al_Ferron_flux","TMAl_flux",
                                        "OMAl_flux","Al_ICP_flux","NH4_flux",
                                        "H_flux","SO4_flux","NO3_flux",
                                        "Cl_flux","PO4_flux","DOC_flux",
                                        "TDN_flux","DON_flux","DIC_flux",
                                        "SiO2_flux","Mn_flux","Fe_flux",
                                        "F_flux","ANC_flux","SpecCond_flux",
                                        "TheoryCond_flux")]
            ,by=list(stream_daily_flux_gHa_new$Year_Month),sum)

colnames(stream_monthly_flux_gHa) <- c("Year_Month","flow_mm","Ca_flux",
                                       "Mg_flux","K_flux","Na_flux",
                                       "Al_Ferron_flux","TMAl_flux",
                                       "OMAl_flux","Al_ICP_flux","NH4_flux",
                                       "H_flux","SO4_flux","NO3_flux","Cl_flux",
                                       "PO4_flux","DOC_flux","TDN_flux",
                                       "DON_flux","DIC_flux","SiO2_flux",
                                       "Mn_flux","Fe_flux","F_flux","ANC_flux",
                                       "SpecCond_flux","TheoryCond_flux")

# code below adds varibles Year and Month into the aggreated monthly flux
stream_monthly_flux_gHa <- makeYearMonth(stream_monthly_flux_gHa)
stream_monthly_volwt_conc <- stream_monthly_flux_gHa
stream_monthly_flux_gHa <- fix_NA(stream_monthly_flux_gHa)

#creates a subdirectory called results if it doesn't already exist
ifelse(!dir.exists(paste(current_path,"/results",sep = "")), 
       dir.create(paste(current_path,"/results",sep = "")), FALSE)

#strips out SpecCond and TheoryCond from the monthly flux table dataframe
stream_monthly_flux_gHa <- 
  subset(stream_monthly_flux_gHa, select = 
           -c(ANC_flux,SpecCond_flux,TheoryCond_flux))

stream_monthly_flux_gHa$H_flux <- 
  round(stream_monthly_flux_gHa$H_flux,digits = 4)

stream_monthly_volwt_conc$volwt_Ca <- 
  ifelse(is.na(stream_monthly_volwt_conc$Ca_flux),NA,
         stream_monthly_volwt_conc$Ca_flux/
           (stream_monthly_volwt_conc$flow_mm*10))
stream_monthly_volwt_conc$volwt_Mg <- 
  ifelse(is.na(stream_monthly_volwt_conc$Mg_flux),NA,
         stream_monthly_volwt_conc$Mg_flux/
           (stream_monthly_volwt_conc$flow_mm*10))
stream_monthly_volwt_conc$volwt_K <- 
  ifelse(is.na(stream_monthly_volwt_conc$K_flux),NA,
         stream_monthly_volwt_conc$K_flux/
           (stream_monthly_volwt_conc$flow_mm*10))
stream_monthly_volwt_conc$volwt_Na <- 
  ifelse(is.na(stream_monthly_volwt_conc$Na_flux),NA,
         stream_monthly_volwt_conc$Na_flux/
           (stream_monthly_volwt_conc$flow_mm*10))
stream_monthly_volwt_conc$volwt_Al_Ferron <- 
  ifelse(is.na(stream_monthly_volwt_conc$Al_Ferron_flux),NA,
         stream_monthly_volwt_conc$Al_Ferron_flux/
           (stream_monthly_volwt_conc$flow_mm*10))
stream_monthly_volwt_conc$volwt_TMAl <- 
  ifelse(is.na(stream_monthly_volwt_conc$TMAl_flux),NA,
         stream_monthly_volwt_conc$TMAl_flux/
           (stream_monthly_volwt_conc$flow_mm*10))
stream_monthly_volwt_conc$volwt_OMAl <- 
  ifelse(is.na(stream_monthly_volwt_conc$OMAl_flux),NA,
         stream_monthly_volwt_conc$OMAl_flux/
           (stream_monthly_volwt_conc$flow_mm*10))
stream_monthly_volwt_conc$volwt_Al_ICP <- 
  ifelse(is.na(stream_monthly_volwt_conc$Al_ICP_flux),NA,
         stream_monthly_volwt_conc$Al_ICP_flux/
           (stream_monthly_volwt_conc$flow_mm*10))
stream_monthly_volwt_conc$volwt_NH4 <- 
  ifelse(is.na(stream_monthly_volwt_conc$NH4_flux),NA,
         stream_monthly_volwt_conc$NH4_flux/
           (stream_monthly_volwt_conc$flow_mm*10))
stream_monthly_volwt_conc$volwt_SO4 <- 
  ifelse(is.na(stream_monthly_volwt_conc$SO4_flux),NA,
         stream_monthly_volwt_conc$SO4_flux/
           (stream_monthly_volwt_conc$flow_mm*10))
stream_monthly_volwt_conc$volwt_NO3 <- 
  ifelse(is.na(stream_monthly_volwt_conc$NO3_flux),NA,
         stream_monthly_volwt_conc$NO3_flux/
           (stream_monthly_volwt_conc$flow_mm*10))
stream_monthly_volwt_conc$volwt_Cl <- 
  ifelse(is.na(stream_monthly_volwt_conc$Cl_flux),NA,
         stream_monthly_volwt_conc$Cl_flux/
           (stream_monthly_volwt_conc$flow_mm*10))
stream_monthly_volwt_conc$volwt_PO4 <- 
  ifelse(is.na(stream_monthly_volwt_conc$PO4_flux),NA,
         stream_monthly_volwt_conc$PO4_flux/
           (stream_monthly_volwt_conc$flow_mm*10))
stream_monthly_volwt_conc$volwt_DOC <- 
  ifelse(is.na(stream_monthly_volwt_conc$DOC_flux),NA,
         stream_monthly_volwt_conc$DOC_flux/
           (stream_monthly_volwt_conc$flow_mm*10))
stream_monthly_volwt_conc$volwt_TDN <- 
  ifelse(is.na(stream_monthly_volwt_conc$TDN_flux),NA,
         stream_monthly_volwt_conc$TDN_flux/
           (stream_monthly_volwt_conc$flow_mm*10))
stream_monthly_volwt_conc$volwt_DON <- 
  ifelse(is.na(stream_monthly_volwt_conc$DON_flux),NA,
         stream_monthly_volwt_conc$DON_flux/
           (stream_monthly_volwt_conc$flow_mm*10))
stream_monthly_volwt_conc$volwt_DIC <- 
  ifelse(is.na(stream_monthly_volwt_conc$DIC_flux),NA,
         stream_monthly_volwt_conc$DIC_flux/
           (stream_monthly_volwt_conc$flow_mm*10))
stream_monthly_volwt_conc$volwt_SiO2 <- 
  ifelse(is.na(stream_monthly_volwt_conc$SiO2_flux),NA,
         stream_monthly_volwt_conc$SiO2_flux/
           (stream_monthly_volwt_conc$flow_mm*10))
stream_monthly_volwt_conc$volwt_Mn <- 
  ifelse(is.na(stream_monthly_volwt_conc$Mn_flux),NA,
         stream_monthly_volwt_conc$Mn_flux/
           (stream_monthly_volwt_conc$flow_mm*10))
stream_monthly_volwt_conc$volwt_Fe <- 
  ifelse(is.na(stream_monthly_volwt_conc$Fe_flux),NA,
         stream_monthly_volwt_conc$Fe_flux/
           (stream_monthly_volwt_conc$flow_mm*10))
stream_monthly_volwt_conc$volwt_F <- 
  ifelse(is.na(stream_monthly_volwt_conc$F_flux),NA,
         stream_monthly_volwt_conc$F_flux/
           (stream_monthly_volwt_conc$flow_mm*10))
stream_monthly_volwt_conc$volwt_ANC <- 
  ifelse(is.na(stream_monthly_volwt_conc$ANC_flux),NA,
         stream_monthly_volwt_conc$ANC_flux/
           (stream_monthly_volwt_conc$flow_mm*10))
stream_monthly_volwt_conc$volwt_H <- 
  ifelse(is.na(stream_monthly_volwt_conc$H_flux),NA,
         stream_monthly_volwt_conc$H_flux/
           (stream_monthly_volwt_conc$flow_mm*10))
stream_monthly_volwt_conc$volwt_SpecCond <- 
  ifelse(is.na(stream_monthly_volwt_conc$SpecCond_flux),NA,
         stream_monthly_volwt_conc$SpecCond_flux/
           (stream_monthly_volwt_conc$flow_mm*10))
stream_monthly_volwt_conc$volwt_TheoryCond <- 
  ifelse(is.na(stream_monthly_volwt_conc$TheoryCond_flux),NA,
         stream_monthly_volwt_conc$TheoryCond_flux/
           (stream_monthly_volwt_conc$flow_mm*10))

stream_monthly_volwt_conc$volwt_pH <- NA

len_Conc <- nrow(stream_monthly_volwt_conc)

#fill in missing pH from H+ mg/l
for (i in 1:len_Conc)
{
  stream_monthly_volwt_conc$volwt_pH[i] <- 
    stream_calc_pH(stream_monthly_volwt_conc$volwt_pH[i],
                   stream_monthly_volwt_conc$volwt_H[i])
}

volwt_Ca <- round(stream_monthly_volwt_conc$volwt_Ca,digits = 2)
volwt_Mg <- round(stream_monthly_volwt_conc$volwt_Mg,digits = 2)
volwt_K <- round(stream_monthly_volwt_conc$volwt_K,digits = 2)
volwt_Na <- round(stream_monthly_volwt_conc$volwt_Na,digits = 2)
volwt_Al_Ferron <- round(stream_monthly_volwt_conc$volwt_Al_Ferron,digits = 3)
volwt_TMAl <- round(stream_monthly_volwt_conc$volwt_TMAl,digits = 3)
volwt_OMAl <- round(stream_monthly_volwt_conc$volwt_OMAl,digits = 3)
volwt_Al_ICP <- round(stream_monthly_volwt_conc$volwt_Al_ICP,digits = 2)
volwt_NH4 <- round(stream_monthly_volwt_conc$volwt_NH4,digits = 3)
volwt_SO4 <- round(stream_monthly_volwt_conc$volwt_SO4,digits = 2)
volwt_NO3 <- round(stream_monthly_volwt_conc$volwt_NO3,digits = 3)
volwt_Cl <- round(stream_monthly_volwt_conc$volwt_Cl,digits = 2)
volwt_PO4 <- round(stream_monthly_volwt_conc$volwt_PO4,digits = 3)
volwt_DOC <- round(stream_monthly_volwt_conc$volwt_DOC,digits = 2)
volwt_TDN <- round(stream_monthly_volwt_conc$volwt_TDN,digits = 2)
volwt_DON <- round(stream_monthly_volwt_conc$volwt_DON,digits = 2)
volwt_DIC <- round(stream_monthly_volwt_conc$volwt_DIC,digits = 2)
volwt_SiO2 <- round(stream_monthly_volwt_conc$volwt_SiO2,digits = 2)
volwt_Mn <- round(stream_monthly_volwt_conc$volwt_Mn,digits = 2)
volwt_Fe <- round(stream_monthly_volwt_conc$volwt_Fe,digits = 2)
volwt_F <- round(stream_monthly_volwt_conc$volwt_F,digits = 2)
volwt_ANC <- round(stream_monthly_volwt_conc$volwt_ANC,digits = 2)
volwt_H <- round(stream_monthly_volwt_conc$volwt_H,digits = 4)
volwt_pH <- round(stream_monthly_volwt_conc$volwt_pH,digits = 2)
volwt_SpecCond <- round(stream_monthly_volwt_conc$volwt_SpecCond,digits = 2)
volwt_TheoryCond <- round(stream_monthly_volwt_conc$volwt_Theory,digits = 2)

stream_monthly_volwt_conc$volwt_Ca <- volwt_Ca
stream_monthly_volwt_conc$volwt_Mg <- volwt_Mg
stream_monthly_volwt_conc$volwt_K <- volwt_K
stream_monthly_volwt_conc$volwt_Na <- volwt_Na
stream_monthly_volwt_conc$volwt_Al_Ferron <- volwt_Al_Ferron
stream_monthly_volwt_conc$volwt_TMAl <- volwt_TMAl
stream_monthly_volwt_conc$volwt_OMAl <- volwt_OMAl
stream_monthly_volwt_conc$volwt_Al_ICP <- volwt_Al_ICP
stream_monthly_volwt_conc$volwt_NH4 <- volwt_NH4
stream_monthly_volwt_conc$volwt_SO4 <- volwt_SO4
stream_monthly_volwt_conc$volwt_NO3 <- volwt_NO3
stream_monthly_volwt_conc$volwt_Cl <- volwt_Cl
stream_monthly_volwt_conc$volwt_PO4 <- volwt_PO4
stream_monthly_volwt_conc$volwt_DOC <- volwt_DOC
stream_monthly_volwt_conc$volwt_TDN <- volwt_TDN
stream_monthly_volwt_conc$volwt_DON <- volwt_DON
stream_monthly_volwt_conc$volwt_DIC <- volwt_DIC
stream_monthly_volwt_conc$volwt_SiO2 <- volwt_SiO2
stream_monthly_volwt_conc$volwt_Mn <- volwt_Mn
stream_monthly_volwt_conc$volwt_Fe <- volwt_Fe
stream_monthly_volwt_conc$volwt_F <- volwt_F
stream_monthly_volwt_conc$volwt_ANC <- volwt_ANC
stream_monthly_volwt_conc$volwt_H <- volwt_H
stream_monthly_volwt_conc$volwt_pH <- volwt_pH
stream_monthly_volwt_conc$volwt_SpecCond <- volwt_SpecCond
stream_monthly_volwt_conc$volwt_TheoryCond <- volwt_TheoryCond

stream_monthly_volwt_conc <- 
  subset(stream_monthly_volwt_conc, select = 
           -c(Ca_flux,Mg_flux,K_flux,Na_flux,Al_Ferron_flux,TMAl_flux,OMAl_flux,
              Al_ICP_flux,NH4_flux,H_flux,SO4_flux,NO3_flux,Cl_flux,PO4_flux,
              DOC_flux,TDN_flux,DON_flux,DIC_flux,SiO2_flux,Mn_flux,Fe_flux,
              F_flux,ANC_flux,SpecCond_flux,TheoryCond_flux))

colnames(stream_monthly_volwt_conc) <- 
  c("Year","Month","Year_Month","flow_mm","volwt_Ca","volwt_Mg","volwt_K",
    "volwt_Na","volwt_Al_Ferron","volwt_TMAl","volwt_OMAl","volwt_Al_ICP",
    "volwt_NH4","volwt_SO4","volwt_NO3","volwt_Cl","volwt_PO4","volwt_DOC",
    "volwt_TDN","volwt_DON","volwt_DIC","volwt_SiO2","volwt_Mn","volwt_Fe",
    "volwt_F","volwt_ANC","volwt_H","volwt_SpecCond","volwt_TheoryCond",
    "volwt_pH")

stream_monthly_volwt_conc <- fix_NA(stream_monthly_volwt_conc)

#strips out TheoryCond from the monthly volume-weighted concentration dataframe
stream_monthly_volwt_conc <- subset(stream_monthly_volwt_conc, 
                                    select = -c(volwt_TheoryCond))

#reorders column names
stream_monthly_volwt_conc <- 
  stream_monthly_volwt_conc[c("Year","Month","Year_Month","flow_mm","volwt_Ca",
                              "volwt_Mg","volwt_K","volwt_Na","volwt_Al_Ferron",
                              "volwt_TMAl","volwt_OMAl","volwt_Al_ICP",
                              "volwt_NH4","volwt_SO4","volwt_NO3","volwt_Cl",
                              "volwt_PO4","volwt_DOC","volwt_TDN","volwt_DON",
                              "volwt_DIC","volwt_SiO2","volwt_Mn","volwt_Fe",
                              "volwt_F","volwt_H","volwt_pH","volwt_SpecCond",
                              "volwt_ANC")]

# creates a directory for results if it does not already exist
#ifelse(!dir.exists(paste(current_path),"/results",sep = ""), 
#       dir.create(paste(current_path),"/results",sep = ""), FALSE)

out_monthly_volwt_Name <-
  paste("results/ws",stream_chem$WS[1],"_stream_monthly_volwt_conc.csv",sep="")
out_monthly_flux_Name <-
  paste("results/ws",stream_chem$WS[1],"_stream_monthly_flux_gHa.csv",sep="")

write.csv(stream_monthly_flux_gHa, 
          file = out_monthly_flux_Name,row.names=FALSE)
write.csv(stream_monthly_volwt_conc, 
          file = out_monthly_volwt_Name,row.names=FALSE)

#Strips the last WY off if it is incomplete
WY_day_count <- count(stream_daily_flux_gHa_new, "WY")
WY_day_count_head <- head(WY_day_count,1)
WY_day_count_tail <- tail(WY_day_count,1)
first_year<-ifelse(WY_day_count_head$freq <= 364,
                   WY_day_count_head$WY+1,WY_day_count_head$WY)
last_year<-ifelse(WY_day_count_tail$freq <= 364,
                  WY_day_count_tail$WY-1,WY_day_count_tail$WY)
stream_daily_flux_gHa_new <- subset(stream_daily_flux_gHa_new,
                                    !(stream_daily_flux_gHa_new$WY<first_year))
stream_daily_flux_gHa_new <- subset(stream_daily_flux_gHa_new,
                                    !(stream_daily_flux_gHa_new$WY>last_year))

stream_WY_flux_gHa <- 
  aggregate(stream_daily_flux_gHa_new[c("flow_mm","Ca_flux","Mg_flux","K_flux",
                                        "Na_flux","Al_Ferron_flux","TMAl_flux",
                                        "OMAl_flux","Al_ICP_flux","NH4_flux",
                                        "H_flux","SO4_flux","NO3_flux",
                                        "Cl_flux","PO4_flux","DOC_flux",
                                        "TDN_flux","DON_flux","DIC_flux",
                                        "SiO2_flux","Mn_flux","Fe_flux",
                                        "F_flux","ANC_flux","SpecCond_flux",
                                        "TheoryCond_flux")],
            by=list(stream_daily_flux_gHa_new$WY),sum)

colnames(stream_WY_flux_gHa) <- c("WaterYear","flow_mm","Ca_flux","Mg_flux",
                                  "K_flux","Na_flux","Al_Ferron_flux",
                                  "TMAl_flux","OMAl_flux","Al_ICP_flux",
                                  "NH4_flux","H_flux","SO4_flux","NO3_flux",
                                  "Cl_flux","PO4_flux","DOC_flux","TDN_flux",
                                  "DON_flux","DIC_flux","SiO2_flux","Mn_flux",
                                  "Fe_flux","F_flux","ANC_flux","SpecCond_flux",
                                  "TheoryCond_flux")

stream_WY_volwt_conc <- stream_WY_flux_gHa

stream_WY_flux_gHa <- fix_NA(stream_WY_flux_gHa)

stream_WY_volwt_conc$volwt_Ca <- 
  ifelse(is.na(stream_WY_volwt_conc$Ca_flux),NA,
         stream_WY_volwt_conc$Ca_flux/(stream_WY_volwt_conc$flow_mm*10))
stream_WY_volwt_conc$volwt_Mg <- 
  ifelse(is.na(stream_WY_volwt_conc$Mg_flux),NA,
         stream_WY_volwt_conc$Mg_flux/(stream_WY_volwt_conc$flow_mm*10))
stream_WY_volwt_conc$volwt_K <- 
  ifelse(is.na(stream_WY_volwt_conc$K_flux),NA,
         stream_WY_volwt_conc$K_flux/(stream_WY_volwt_conc$flow_mm*10))
stream_WY_volwt_conc$volwt_Na <- 
  ifelse(is.na(stream_WY_volwt_conc$Na_flux),NA,
         stream_WY_volwt_conc$Na_flux/(stream_WY_volwt_conc$flow_mm*10))
stream_WY_volwt_conc$volwt_Al_Ferron <- 
  ifelse(is.na(stream_WY_volwt_conc$Al_Ferron_flux),NA,
         stream_WY_volwt_conc$Al_Ferron_flux/(stream_WY_volwt_conc$flow_mm*10))
stream_WY_volwt_conc$volwt_TMAl <- 
  ifelse(is.na(stream_WY_volwt_conc$TMAl_flux),NA,
         stream_WY_volwt_conc$TMAl_flux/(stream_WY_volwt_conc$flow_mm*10))
stream_WY_volwt_conc$volwt_OMAl <- 
  ifelse(is.na(stream_WY_volwt_conc$OMAl_flux),NA,
         stream_WY_volwt_conc$OMAl_flux/(stream_WY_volwt_conc$flow_mm*10))
stream_WY_volwt_conc$volwt_Al_ICP <- 
  ifelse(is.na(stream_WY_volwt_conc$Al_ICP_flux),NA,
         stream_WY_volwt_conc$Al_ICP_flux/(stream_WY_volwt_conc$flow_mm*10))
stream_WY_volwt_conc$volwt_NH4 <-
  ifelse(is.na(stream_WY_volwt_conc$NH4_flux),NA,
         stream_WY_volwt_conc$NH4_flux/(stream_WY_volwt_conc$flow_mm*10))
stream_WY_volwt_conc$volwt_SO4 <- 
  ifelse(is.na(stream_WY_volwt_conc$SO4_flux),NA,
         stream_WY_volwt_conc$SO4_flux/(stream_WY_volwt_conc$flow_mm*10))
stream_WY_volwt_conc$volwt_NO3 <- 
  ifelse(is.na(stream_WY_volwt_conc$NO3_flux),NA,
         stream_WY_volwt_conc$NO3_flux/(stream_WY_volwt_conc$flow_mm*10))
stream_WY_volwt_conc$volwt_Cl <- 
  ifelse(is.na(stream_WY_volwt_conc$Cl_flux),NA,
         stream_WY_volwt_conc$Cl_flux/(stream_WY_volwt_conc$flow_mm*10))
stream_WY_volwt_conc$volwt_PO4 <- 
  ifelse(is.na(stream_WY_volwt_conc$PO4_flux),NA,
         stream_WY_volwt_conc$PO4_flux/(stream_WY_volwt_conc$flow_mm*10))
stream_WY_volwt_conc$volwt_DOC <- 
  ifelse(is.na(stream_WY_volwt_conc$DOC_flux),NA,
         stream_WY_volwt_conc$DOC_flux/(stream_WY_volwt_conc$flow_mm*10))
stream_WY_volwt_conc$volwt_TDN <- 
  ifelse(is.na(stream_WY_volwt_conc$TDN_flux),NA,
         stream_WY_volwt_conc$TDN_flux/(stream_WY_volwt_conc$flow_mm*10))
stream_WY_volwt_conc$volwt_DON <- 
  ifelse(is.na(stream_WY_volwt_conc$DON_flux),NA,
         stream_WY_volwt_conc$DON_flux/(stream_WY_volwt_conc$flow_mm*10))
stream_WY_volwt_conc$volwt_DIC <- 
  ifelse(is.na(stream_WY_volwt_conc$DIC_flux),NA,
         stream_WY_volwt_conc$DIC_flux/(stream_WY_volwt_conc$flow_mm*10))
stream_WY_volwt_conc$volwt_SiO2 <- 
  ifelse(is.na(stream_WY_volwt_conc$SiO2_flux),NA,
         stream_WY_volwt_conc$SiO2_flux/(stream_WY_volwt_conc$flow_mm*10))
stream_WY_volwt_conc$volwt_Mn <- 
  ifelse(is.na(stream_WY_volwt_conc$Mn_flux),NA,
         stream_WY_volwt_conc$Mn_flux/(stream_WY_volwt_conc$flow_mm*10))
stream_WY_volwt_conc$volwt_Fe <- 
  ifelse(is.na(stream_WY_volwt_conc$Fe_flux),NA,
         stream_WY_volwt_conc$Fe_flux/(stream_WY_volwt_conc$flow_mm*10))
stream_WY_volwt_conc$volwt_F <- 
  ifelse(is.na(stream_WY_volwt_conc$F_flux),NA,
         stream_WY_volwt_conc$F_flux/(stream_WY_volwt_conc$flow_mm*10))
stream_WY_volwt_conc$volwt_ANC <- 
  ifelse(is.na(stream_WY_volwt_conc$ANC_flux),NA,
         stream_WY_volwt_conc$ANC_flux/(stream_WY_volwt_conc$flow_mm*10))
stream_WY_volwt_conc$volwt_H <- 
  ifelse(is.na(stream_WY_volwt_conc$H_flux),NA,
         stream_WY_volwt_conc$H_flux/(stream_WY_volwt_conc$flow_mm*10))
stream_WY_volwt_conc$volwt_SpecCond <- 
  ifelse(is.na(stream_WY_volwt_conc$SpecCond_flux),NA,
         stream_WY_volwt_conc$SpecCond_flux/(stream_WY_volwt_conc$flow_mm*10))
stream_WY_volwt_conc$volwt_TheoryCond <- 
  ifelse(is.na(stream_WY_volwt_conc$TheoryCond_flux),NA,
         stream_WY_volwt_conc$TheoryCond_flux/(stream_WY_volwt_conc$flow_mm*10))

volwt_Ca <- round(stream_WY_volwt_conc$volwt_Ca,digits = 2)
volwt_Mg <- round(stream_WY_volwt_conc$volwt_Mg,digits = 2)
volwt_K <- round(stream_WY_volwt_conc$volwt_K,digits = 2)
volwt_Na <- round(stream_WY_volwt_conc$volwt_Na,digits = 2)
volwt_Al_Ferron <- round(stream_WY_volwt_conc$volwt_Al_Ferron,digits = 3)
volwt_TMAl <- round(stream_WY_volwt_conc$volwt_TMAl,digits = 3)
volwt_OMAl <- round(stream_WY_volwt_conc$volwt_OMAl,digits = 3)
volwt_Al_ICP <- round(stream_WY_volwt_conc$volwt_Al_ICP,digits = 2)
volwt_NH4 <- round(stream_WY_volwt_conc$volwt_NH4,digits = 3)
volwt_SO4 <- round(stream_WY_volwt_conc$volwt_SO4,digits = 2)
volwt_NO3 <- round(stream_WY_volwt_conc$volwt_NO3,digits = 3)
volwt_Cl <- round(stream_WY_volwt_conc$volwt_Cl,digits = 2)
volwt_PO4 <- round(stream_WY_volwt_conc$volwt_PO4,digits = 3)
volwt_DOC <- round(stream_WY_volwt_conc$volwt_DOC,digits = 2)
volwt_TDN <- round(stream_WY_volwt_conc$volwt_TDN,digits = 2)
volwt_DON <- round(stream_WY_volwt_conc$volwt_DON,digits = 2)
volwt_DIC <- round(stream_WY_volwt_conc$volwt_DIC,digits = 2)
volwt_SiO2 <- round(stream_WY_volwt_conc$volwt_SiO2,digits = 2)
volwt_Mn <- round(stream_WY_volwt_conc$volwt_Mn,digits = 2)
volwt_Fe <- round(stream_WY_volwt_conc$volwt_Fe,digits = 2)
volwt_F <- round(stream_WY_volwt_conc$volwt_F,digits = 2)
volwt_ANC <- round(stream_WY_volwt_conc$volwt_ANC,digits = 2)
volwt_SpecCond <- round(stream_WY_volwt_conc$volwt_SpecCond,digits = 2)
volwt_TheoryCond <- round(stream_WY_volwt_conc$volwt_TheoryCond,digits = 2)
volwt_H <- round(stream_WY_volwt_conc$volwt_H,digits = 10)
# volwt_pH <- round(stream_WY_volwt_conc$volwt_pH,digits = 2)

stream_WY_volwt_conc$volwt_Ca <- volwt_Ca
stream_WY_volwt_conc$volwt_Mg <- volwt_Mg
stream_WY_volwt_conc$volwt_K <- volwt_K
stream_WY_volwt_conc$volwt_Na <- volwt_Na
stream_WY_volwt_conc$volwt_Al_Ferron <- volwt_Al_Ferron
stream_WY_volwt_conc$volwt_TMAl <- volwt_TMAl
stream_WY_volwt_conc$volwt_OMAl <- volwt_OMAl
stream_WY_volwt_conc$volwt_Al_ICP <- volwt_Al_ICP
stream_WY_volwt_conc$volwt_NH4 <- volwt_NH4
stream_WY_volwt_conc$volwt_SO4 <- volwt_SO4
stream_WY_volwt_conc$volwt_NO3 <- volwt_NO3
stream_WY_volwt_conc$volwt_Cl <- volwt_Cl
stream_WY_volwt_conc$volwt_PO4 <- volwt_PO4
stream_WY_volwt_conc$volwt_DOC <- volwt_DOC
stream_WY_volwt_conc$volwt_TDN <- volwt_TDN
stream_WY_volwt_conc$volwt_DON <- volwt_DON
stream_WY_volwt_conc$volwt_DIC <- volwt_DIC
stream_WY_volwt_conc$volwt_SiO2 <- volwt_SiO2
stream_WY_volwt_conc$volwt_Mn <- volwt_Mn
stream_WY_volwt_conc$volwt_Fe <- volwt_Fe
stream_WY_volwt_conc$volwt_F <- volwt_F
stream_WY_volwt_conc$volwt_ANC <- volwt_ANC
stream_WY_volwt_conc$volwt_SpecCond <- volwt_SpecCond
stream_WY_volwt_conc$volwt_TheoryCond <- volwt_TheoryCond
stream_WY_volwt_conc$volwt_H <- volwt_H
# stream_WY_volwt_conc$volwt_pH <- volwt_pH

stream_WY_volwt_conc <- 
  subset(stream_WY_volwt_conc, select = -c(Ca_flux,Mg_flux,K_flux,Na_flux,
                                           Al_Ferron_flux,TMAl_flux,OMAl_flux,
                                           Al_ICP_flux,NH4_flux,H_flux,SO4_flux,
                                           NO3_flux,Cl_flux,PO4_flux,DOC_flux,
                                           TDN_flux,DON_flux,DIC_flux,SiO2_flux,
                                           Mn_flux,Fe_flux,F_flux,ANC_flux,
                                           SpecCond_flux,TheoryCond_flux))

stream_WY_volwt_conc <- 
  stream_WY_volwt_conc[c("WaterYear","flow_mm","volwt_Ca","volwt_Mg","volwt_K",
                         "volwt_Na","volwt_Al_Ferron","volwt_TMAl","volwt_OMAl",
                         "volwt_Al_ICP","volwt_NH4","volwt_SO4","volwt_NO3",
                         "volwt_Cl","volwt_PO4","volwt_DOC","volwt_TDN",
                         "volwt_DON","volwt_DIC","volwt_SiO2","volwt_Mn",
                         "volwt_Fe","volwt_F","volwt_ANC","volwt_H",
                         "volwt_SpecCond","volwt_TheoryCond")]

stream_WY_volwt_conc$volwt_pH <- NA

len_Conc <- nrow(stream_WY_volwt_conc)

# fill in missing pH from H+ mg/l
for (i in 1:len_Conc)
{
  stream_WY_volwt_conc$volwt_pH[i] <- 
    stream_calc_pH(stream_WY_volwt_conc$volwt_pH[i],
                   stream_WY_volwt_conc$volwt_H[i])
}

volwt_H <- round(stream_WY_volwt_conc$volwt_H,digits = 4)
stream_WY_volwt_conc$volwt_H <- volwt_H

volwt_pH <- round(stream_WY_volwt_conc$volwt_pH,digits = 2)
stream_WY_volwt_conc$volwt_pH <- volwt_pH

stream_WY_volwt_conc <- fix_NA(stream_WY_volwt_conc)

# reorders column names
stream_WY_volwt_conc <- 
  stream_WY_volwt_conc[c("WaterYear","flow_mm","volwt_Ca","volwt_Mg","volwt_K",
                         "volwt_Na","volwt_Al_Ferron","volwt_TMAl","volwt_OMAl",
                         "volwt_Al_ICP","volwt_NH4","volwt_SO4","volwt_NO3",
                         "volwt_Cl","volwt_PO4","volwt_DOC","volwt_TDN",
                         "volwt_DON","volwt_DIC","volwt_SiO2","volwt_Mn",
                         "volwt_Fe","volwt_F","volwt_H","volwt_pH",
                         "volwt_SpecCond","volwt_ANC")]

# strips out SpecCond and TheoryCond from the WY flux table dataframe
stream_WY_flux_gHa <- 
  subset(stream_WY_flux_gHa, 
         select = -c(SpecCond_flux,TheoryCond_flux,ANC_flux))

out_WY_volwt_Name <-paste("results/ws",
                          stream_chem$WS[1],"_stream_WY_volwt_conc.csv",sep="")
out_WY_Name <-paste("results/ws",
                    stream_chem$WS[1],"_stream_WY_flux_gHa.csv",sep="")

write.csv(stream_WY_flux_gHa, file = out_WY_Name,row.names=FALSE)
write.csv(stream_WY_volwt_conc, file = out_WY_volwt_Name,row.names=FALSE)

# counts the number of days per year as a check for duplicates 
# in the chemistry or stream file
WY_day_count <- count(stream_daily_flux_gHa_new, "WY")
print(WY_day_count)

}
