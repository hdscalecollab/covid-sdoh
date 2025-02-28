## import libraries
library(pander)
panderOptions("table.alignment.default", function(df) ifelse(sapply(df, is.numeric), 
                                                             "right", "left"))
panderOptions("table.split.table", Inf)
# panderOptions('big.mark', ',')
panderOptions("keep.trailing.zeros", TRUE)

library(magrittr)
library(tidyr)
library(dplyr)

####### variable selection #################

## correlation matrix and previous code for LASSO
library(corrplot)
library(Hmisc)
library(lme4)
library(nlme)

## Read data              
census_case=read.csv("case_var_all_0915.csv",sep=",",header=T)

lme_data=census_case%>%select(total_r,ForgnBorn,NonHisBlk,Hispanic,Crowding,Disability,Uninsured,
                              EmployService,Unemploy,Poverty,NoHighSchool,PopDens,Over65,Obesity,MUAP,RUCA,Week,ZCTA)

## correlation matrix

## Recode for RUCA variable
var_to_plot=lme_data%>%select(-Week,-ZCTA,-total_r)
var_to_plot$RUCA[var_to_plot$RUCA=="Urban"]<-1
var_to_plot$RUCA[var_to_plot$RUCA=="Large Rural"]<-2
var_to_plot$RUCA[var_to_plot$RUCA=="Small Rural"]<-3
var_to_plot$RUCA[var_to_plot$RUCA=="Isolated Small Rural"]<-4

## correlation matrix using Pearson's correlation coefficient
M <- rcorr(as.matrix(var_to_plot), type="pearson")
Mr <- M$r
Mp <- M$P
oldp <- par(cex=.75)

## plot correlation matrix
Mcorr <- corrplot(Mr, type = "upper", order = "hclust", addCoef.col = "black",insig = "blank",
                  p.mat = Mp, sig.level = 0.05, tl.cex = 1, tl.col = "black",
                  cl.cex=0.8, number.cex=0.8,title = "")

##### #### NOTE: NOT USED IN MANUSCRIPT #############
####### use GLMM to select variables ###########
##### scale the variables
lme_data[c(2:14)]<-scale(lme_data[c(2:14)])
var_sel_test=lme(total_r~ForgnBorn+NonHisBlk+Hispanic+Crowding+Disability+Uninsured+EmployService+Unemploy+Poverty+
                   NoHighSchool+PopDens+as.factor(MUAP)+as.factor(RUCA)+Over65+Obesity,
                 random=~1|ZCTA,
                 data=lme_data)
summary(var_sel_test)



              
############# spatial lag regression with random effect ################
library("spdep")
library("sp")
library("Matrix")
library("splm")
library(maptools)
library(rgdal)

## read data
census_case=read.csv("case_var_all0915.csv",sep=",",header=T)

## write regression equation, RUCA and MUAP are categorical data
fm<-total_r~ForgnBorn+NonHisBlk+Hispanic+Crowding+
  Disability+Uninsured+EmployService+Unemploy+Poverty+
  NoHighSchool+PopDens+as.factor(MUAP)+as.factor(RUCA)+Over65+Obesity

## read spatial data
ca_shp=readShapePoly("ZCTA_Update.shp")
              
## define spatial neighbor using 5km as distance threshold
## d2 refers to diatance threashold, unit:km
dnn_nb=dnearneigh(coordinates(ca_shp), longlat = TRUE,d1=0, d2=5)
              
## run spatial lag regression with random effect using the regression formula above and neighbor setting
sararremod <- spml(formula = fm, data = census_case, index = NULL, 
                   listw = nb2listw(dnn_nb,zero.policy=TRUE),  model = "random", lag = TRUE, 
                   spatial.error = "none")
summary(sararremod)
