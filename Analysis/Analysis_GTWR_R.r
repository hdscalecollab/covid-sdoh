library(GWmodel)
library(sp)
library(maptools)
library(rgdal)
library(tictoc)
library(progress)

###---------------------------------------------------- ###
### Data Prep
###---------------------------------------------------- ###

setwd("C:/Users/jyang/Documents/Covid_MUAP_GTWR/")

### Read Data
census_case=read.csv("FullData/case_var_all0915.csv",sep=",",header=T)
ZCTA_SDCounty=read.csv("FullData/ZCTA_Update_SDCounty_Table.csv",sep=",",header=T)

### Filter case data by SD County ZCTAs
library(tidyverse)
census_case <- filter(census_case,
                       ZCTA %in% ZCTA_SDCounty$ZCTA)

### Convert date column to Date format
# census_case$Week=as.POSIXct(census_case$Week,tryFormats=c("%Y-%m-%d"))  # to POSIXct format
census_case$Week=as.Date(census_case$Week,tryFormats=c("%Y-%m-%d"))     # to Date format  

### Filter data by time
library(dplyr)
census_case <- census_case %>% filter(between(Week, as.Date('2020-04-01'), as.Date('2020-06-30')))


### Read Spatial Polygon
ca_zcta_shp=readShapePoly("FullData/ZCTA_Update_SDCounty_4326.shp")


### Prepare a spatial data frame with time stamp
tic("Creating Spatial df: MERGE")
covidST_zcta = merge(ca_zcta_shp,census_case,by="ZCTA",all=T,duplicateGeoms = TRUE)
toc()

### Export spatial dataframe to file for backup (faster for future usage)
# writeOGR(obj=covidST_zcta, dsn="Export", layer="covidST_zcta_SPdf_week", driver="ESRI Shapefile") # this is in geographical projection
writeOGR(obj=covidST_zcta, dsn="Export", layer="covidST_zcta_SDCounty_SPdf_week_20200401_20200630", driver="ESRI Shapefile") # this is in geographical projection


# > names(covidST_zcta@data)
# [1] "ZCTA"          "ZCTA5CE10"     "Area"          "Week"          "total_cases"   "white_r"      
# [7] "black_r"       "hisp_r"        "asian_r"       "total_r"       "MUAP"          "RUCA"         
# [13] "PopDens"       "NoHighSchool"  "Poverty"       "Unemploy"      "EmployService" "EnglshLssWll" 
# [19] "Uninsured"     "Disability"    "Crowding"      "HouseSize"     "Hispanic"      "NonHisBlk"    
# [25] "ForgnBorn"     "Over65"        "Obesity"       "Diabetes"      "COPD"          "Smoking"      
# [31] "PhysHthNGd"    "SlfHlthFP"     "Asthma"        "CHD"          

View(covidST_zcta@data)



###---------------------------------------------------- ###
### GTWR Model
###---------------------------------------------------- ###

###Optional:  read Space-Time dataframe directly from file/backup
# muap_model_data=readShapePoly("Export/covidST_zcta_SPdf_week.shp")

muap_model_data=covidST_zcta[!is.na(covidST_zcta@data$total_r),] #not sure if any other var has NAs after merging, need check

### Convert week data to time format
muap_model_data@data$Week=as.POSIXlt(muap_model_data@data$Week,tryFormats=c("%Y-%m-%d"))


#################################################
######### DISTANCE Matrix Option 1   ############
###  Use default setting for st matrix ##########
#################################################

#### TEST with SMALL
# ca_zcta_shp_test_Loop <- subset(ca_zcta_shp, ZCTA==90014 | ZCTA==90015 )
# random_ZCTA_list = sample(unique(muap_model_data@data$ZCTA), 3)
# random_ZCTA_list = sample(unique(muap_model_data@data$ZCTA), 0.1*length(unique(muap_model_data@data$ZCTA)))
# small_test_model_data = subset(muap_model_data, ZCTA %in% random_ZCTA_list) 
# Dist_mat=st.dist(dp.locat=coordinates(small_test_model_data),obs.tv=small_test_model_data@data$Week,
#                  longlat=T,ksi=0,t.units="weeks")


#### FULL RUN
tic("Calculating Distance Matrix:")
Dist_mat=st.dist(dp.locat=coordinates(muap_model_data),obs.tv=muap_model_data@data$Week,
                 longlat=T,ksi=0,t.units="weeks")
# Dist_mat=gw.dist(dp.locat=coordinates(muap_model_data))
toc()

tic("Write Distance Matrix to File:")
# write.csv(Dist_mat, "Dist_mat_for_covidST_zcta_weekly.csv", quote=FALSE, row.names=FALSE)
# write.csv(Dist_mat, "Dist_mat_for_covidST_zcta_SDCounty_weekly.csv", quote=FALSE, row.names=FALSE)
write.csv(Dist_mat, "Dist_mat_for_covidST_zcta_SDCounty_weekly_20200401_20200630.csv", quote=FALSE, row.names=FALSE)
toc()



#################################################
######### DISTANCE Matrix Option 2   ############
### Define spatial distance and temporal distance
#################################################

library(dplyr)

tic("Calculating Distance Matrix:")

# ca_zcta_shp@data=ca_zcta_shp@data%>%arrange(ZCTA)
muap_model_data@data=muap_model_data@data%>%arrange(Week,ZCTA)
#s_dMat <- gw.dist(dp.locat=coordinates(ca_zcta_shp), p=2, theta=0, longlat=F)
s_dMat2 <- gw.dist(dp.locat=coordinates(muap_model_data), p=2, theta=0, longlat=T)

### Units could be changed to "months"
t_dMat <- ti.distm(obs.tv=muap_model_data@data$Week, units="weeks")

#distance matrix with pre-defined spatial distance and temporal distance
#Dist_mat2=st.dist(dp.locat=coordinates(ca_muap),s.dMat=s_dMat,t.dMat=t_dMat)
Dist_mat3=st.dist(dp.locat=coordinates(muap_model_data),s.dMat=s_dMat2,t.dMat=t_dMat)

toc()


#################################################
######### Run GTWR MODEL   ######################
### Option 1: Without predefined s/t matrix   ###
#################################################

# !!! Test on the only variable in the test data -- % hispanic

###test on without predefined s/t matrix
tic("Calculating Bandwidth for GTWR:")
# bw = bw.gtwr(total_r~DP05_0071PE,
#              muap_model_data, approach="CV",kernel="gaussian", obs.tv=muap_model_data@data$Week,
#              adaptive=TRUE, p=2, lamda=0.05, 
#              theta=0, longlat=F,ksi=0,st.dMat=Dist_mat)
bw = bw.gtwr(total_r~Hispanic,
             muap_model_data, approach="CV",kernel="gaussian", obs.tv=muap_model_data@data$Week,
             adaptive=TRUE, p=2, lamda=0.05, 
             theta=0, longlat=T,ksi=0,st.dMat=Dist_mat)
toc()


#muap_model_data@data$Week=as.Date(muap_model_data@data$Week)
drops <- c("ZCTA", "ZCTA5CE10", "Area", "total_cases", "white_r", "black_r", "hisp_r", "asian_r",  
           "MUAP", "RUCA", "PopDens", "NoHighSchool", "Poverty", "Unemploy", "EmployService", "EnglshLssWll", "Uninsured", 
           "Disability", "Crowding", "HouseSize", "NonHisBlk", "ForgnBorn", "Over65", "Obesity", "Diabetes", 
           "COPD", "Smoking", "PhysHthNGd", "SlfHlthFP", "Asthma", "CHD") # list of col names
muap_model_data_selected <- muap_model_data[,!(names(muap_model_data) %in% drops)] #remove columns "AREA" and "PERIMETER"


tic("Fit GTWR Model:")
# gtwr_fit=gtwr(total_r~DP05_0071PE,
#               data=muap_model_data,
#               obs.tv=muap_model_data@data$Week,
#               st.bw=bw,
#               adaptive=TRUE,
#               kernel='gaussian',
#               st.dMat=Dist_mat)
# gtwr_fit=gtwr(total_r~Hispanic,
#               data=muap_model_data,
#               obs.tv=muap_model_data@data$Week,
#               st.bw=bw,
#               adaptive=TRUE,
#               kernel='gaussian',
#               st.dMat=Dist_mat)
gtwr_fit=gtwr(total_r~Hispanic,
              data=muap_model_data_selected,
              obs.tv=muap_model_data_selected@data$Week,
              st.bw=bw,
              adaptive=TRUE,
              kernel='gaussian',
              st.dMat=Dist_mat)

zcta_gtwr=gtwr_fit$SDF
View(zcta_gtwr@data)
# write.csv(zcta_gtwr@data,"C:/Users/yanjiacao/Dropbox/COVID_MUAP/GTWR_nopredefine.csv",row.names=F)
write.csv(zcta_gtwr@data,"GTWR_noPredefinedSTDist_SDCounty_weekly_20200401_20200630.csv",row.names=F)
gtwr_fit$GTW.diagnostic
toc()




#################################################
######### Run GTWR MODEL   ######################
### Option 2: WITH predefined s/t matrix      ###
#################################################

####test with predefined s/t matrix
bw2 = bw.gtwr(total_r~DP05_0071PE,
              muap_model_data, approach="CV",kernel="gaussian", obs.tv=muap_model_data@data$Week,
              adaptive=TRUE, p=2, lamda=0.05, 
              theta=0, longlat=F,ksi=0,st.dMat=Dist_mat3)

#muap_model_data@data$Week=as.Date(muap_model_data@data$Week)
gtwr_fit2=gtwr(total_r~DP05_0071PE,
               data=muap_model_data,
               obs.tv=muap_model_data@data$Week,
               st.bw=bw2,
               adaptive=TRUE,
               kernel='gaussian',
               st.dMat=Dist_mat3)

zcta_gtwr2=gtwr_fit2$SDF
View(zcta_gtwr2@data)
# write.csv(zcta_gtwr2@data,"C:/Users/yanjiacao/Dropbox/COVID_MUAP/GTWR_predefine.csv",row.names=F)
write.csv(zcta_gtwr@data,"GTWR_predefinedSTDist_SDCounty_weekly_20200401_20200630.csv",row.names=F)

### check model diagnostic
gtwr_fit2$GTW.diagnostic