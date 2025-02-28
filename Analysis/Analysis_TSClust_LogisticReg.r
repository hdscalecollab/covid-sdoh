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
library(data.table)


##########################################################################
########################## TS Clustering #################################
##########################################################################

#test for ts cluster
library(dtwclust)
#library(TSclust)
library(ggplot2)
library(clue)
library(stringr)

setwd('D:/000_User_Documents/COH/COVID_MUAP/TS Clutering/')


#### Data pre-processing

## Read Covid Case data by ZCTA
# case_pop_full=read.csv("Google Drive/My Drive/MUAP COVID/case_var_all0915.csv",sep=",",header=T)
case_pop_full=read.csv("case_var_all0915.csv",sep=",",header=T)

## Tag ZCTA with 0 cases through out 
mean_of_ZCTA = case_pop_full%>%
  group_by(ZCTA) %>%
  summarise(Case_mean = mean(total_cases, na.rm=FALSE))

## record ZCTAs with 0 population and will not be analyzed further
mean_of_ZCTA$keep_in_Clus <- ifelse(mean_of_ZCTA$Case_mean > 0, 1, 0)
case_pop_full <- merge(case_pop_full,mean_of_ZCTA, by='ZCTA', all.x = TRUE)

## format dataset for ts clustering
tsclust_data = case_pop_full%>%filter(keep_in_Clus==1)%>%select(ZCTA,Week,total_r)%>%spread(Week,total_r)

## Previous work for splitting muap and non-muap
# tsclust_data_muap = case_pop_full%>%filter(keep_in_Clus==1, MUAP==1)%>%select(ZCTA,Week,total_r)%>%spread(Week,total_r)
# tsclust_data_nonMupa = case_pop_full%>%filter(keep_in_Clus==1, MUAP==0)%>%select(ZCTA,Week,total_r)%>%spread(Week,total_r)

# length(unique(tsclust_data_muap$ZCTA))
# length(unique(tsclust_data_nonMupa$ZCTA))



############# TS Clustering (ALL) ################

## distance method -- euclidean or dtw_basic
dist_fun="euclidean"

## test for optimal number of clusters (from 2 to 7)
data_pc_k <- tsclust(tsclust_data[,-1], 
                     k=2L:7L, 
                     type="p",
                     preproc=zscore, 
                     distance=dist_fun, 
                     centroid="pam")

names(data_pc_k) <- paste0("k_",2L:7L)
cvi_result <- sapply(data_pc_k,cvi, type="internal")
cvi_result
cvi_result.df <- as.data.frame(cvi_result)
# temporarily save the results in case we want to check later
write.csv(cvi_result.df,"cvi_result_20230414.csv",row.names=T)

## move index column as first column
cvi_result.df <- cbind(CV_Index = rownames(cvi_result.df), cvi_result.df)
rownames(cvi_result.df) <- 1:nrow(cvi_result.df)

## follow standard and get the col names of min/max of a row
c_1=colnames(cvi_result.df[which.max(cvi_result.df[1,])])
c_2=colnames(cvi_result.df[which.max(cvi_result.df[2,])])
c_3=colnames(cvi_result.df[which.max(cvi_result.df[3,])])
c_4=colnames(cvi_result.df[which.min(cvi_result.df[4,])])
c_5=colnames(cvi_result.df[which.min(cvi_result.df[5,])])
c_6=colnames(cvi_result.df[which.max(cvi_result.df[6,])])
c_7=colnames(cvi_result.df[which.min(cvi_result.df[7,])])
c_n=c(c_1,c_2,c_3,c_4,c_5,c_6,c_7)
c_n.table=table(c_n)
names(c_n.table)[which.max(c_n.table)] #get the optimal number of clusters



## define k (number of clusters) using the result above
k_n=4L #we use 4 clusters in this study
              
## run time-series clustering
model_ts <- tsclust(tsclust_data[,-1],type="p",preproc=zscore,
                    k = k_n, 
                    distance = dist_fun, #seed=899,
                    centroid = "pam"
                    #control=hierarchical_control(method="average")
)

Cluster_Number <- c("Cluster 1", "Cluster 2", "Cluster 3","Cluster 4")



## label each ZCTA with the cluster they belong to
tsclust_data$cluster=model_ts@cluster

## a snap view of how many zctas in each cluster
tsclust_data%>%group_by(cluster)%>%summarise(count=n())
write.csv(tsclust_data%>%select(ZCTA,cluster), "case_all_eucl_k4_20230414.csv", row.names = F)

##########compute mean/median for each cluster at each year
## read data
# data_for_ts=read.csv("Google Drive/My Drive/MUAP COVID/case_all_eucl3.csv",sep=",",header=T)

## Alternatively, use data from above code
data_for_ts=tsclust_data
zip_cl_all=data_for_ts%>%gather(Week,case_pop,2:123)

## Format data to be plotted
case_plot_all <-
  zip_cl_all %>% group_by(cluster, Week) %>% 
  summarise(case = median(case_pop)) %>% #median value for euclidean distance
  setNames(c("Cluster", "Week", "Case")) %>%
  spread(Cluster, Case)%>%setNames(c("Week",Cluster_Number[1:k_n]))%>%
  gather("Cluster","Case",-Week)%>% #add a line re state mean
  mutate(StateMean="No")

## Compute state mean values
state_mean_all=zip_cl_all%>%group_by(Week)%>%
  summarise(case=median(case_pop))%>%
  setNames(c("Week","Case"))%>%
  mutate(Cluster="State median",StateMean="Yes")

## combine state mean with the remaining data
all_to_plot=rbind(case_plot_all,state_mean_all)
head(all_to_plot$Week)

## format week column if above result looks weird
#all_to_plot$Week=substring(all_to_plot$Week,2)
#all_to_plot$Week <- gsub("\\.", "/", all_to_plot$Week)

## otherwise go to this step -- format week
all_to_plot$Week=as.Date(all_to_plot$Week)

color_values=c('#FF73DF','#FFAA00','#73B2FF','#ABCD66','Grey24')


# ggplot(all_to_plot, aes(x = Week, y = Case, group=Cluster)) +
#   geom_line(aes(color = Cluster,linetype=StateMean), size = 1) +
#   # geom_point(aes(color = Cluster)) +
#   theme_bw() + #blank background
#   scale_color_manual(values=color_values[1:4])+
#   ylab("Median case counts per 100,000 population")+
#   scale_x_date(name = 'Week', date_breaks = '2 months')+
#   theme(legend.title=element_text(size=13),legend.text=element_text(size=12))+
#   theme(axis.text.x=element_text(angle = 45, hjust = 1))+
#   guides(linetype=F)

## plot time series clusters (4 clusters)
p <- ggplot(all_to_plot, aes(x = Week, y = Case, group=Cluster)) +
  geom_line(aes(color = Cluster,linetype=StateMean), size = 1) +
  # geom_point(aes(color = Cluster)) +
  # theme_bw() + #blank background
  # scale_color_manual(values=color_values[1:4])+
  ggtitle('All ZCTA')+
  ylab("Median case counts per 100,000 population")+
  scale_color_manual(values=color_values)+
  scale_x_date(name = 'Week', date_breaks = '2 months')+
  theme(legend.title=element_text(size=13),legend.text=element_text(size=12))+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  guides(linetype=F)+
  theme(legend.position = c(0.15, 0.75))+
  list()
p

ggsave("All_ZCTA_20230414.png", width = 10, height = 6, dpi = 600)



              
###########################################################################
###### NOTE: the following work was not presented in the manuscript #######
###########################################################################

############# TS Clustering (MUAP) ################
#distance method -- euclidean or dtw_basic
dist_fun="euclidean"

data_pc_k <- tsclust(tsclust_data_muap[,-1], 
                     k=2L:7L, 
                     type="p",
                     preproc=zscore, 
                     distance=dist_fun, 
                     centroid="pam")

names(data_pc_k) <- paste0("k_",2L:7L)
cvi_result <- sapply(data_pc_k,cvi, type="internal")
cvi_result
cvi_result.df <- as.data.frame(cvi_result)
# write.csv(cvi_result.df,"Google Drive/My Drive/MUAP COVID/cvi_result0923.csv",row.names=F)
write.csv(cvi_result.df,"cvi_result20221006_muap.csv",row.names=T)

#move index column as first column
cvi_result.df <- cbind(CV_Index = rownames(cvi_result.df), cvi_result.df)
rownames(cvi_result.df) <- 1:nrow(cvi_result.df)
#follow standard and get the col names of min/max of a row
c_1=colnames(cvi_result.df[which.max(cvi_result.df[1,])])
c_2=colnames(cvi_result.df[which.max(cvi_result.df[2,])])
c_3=colnames(cvi_result.df[which.max(cvi_result.df[3,])])
c_4=colnames(cvi_result.df[which.min(cvi_result.df[4,])])
c_5=colnames(cvi_result.df[which.min(cvi_result.df[5,])])
c_6=colnames(cvi_result.df[which.max(cvi_result.df[6,])])
c_7=colnames(cvi_result.df[which.min(cvi_result.df[7,])])
c_n=c(c_1,c_2,c_3,c_4,c_5,c_6,c_7)
c_n.table=table(c_n)
names(c_n.table)[which.max(c_n.table)]


#define k using the result above
k_n=4L
model_ts <- tsclust(tsclust_data_muap[,-1],type="p",preproc=zscore,
                    k = k_n, 
                    distance = dist_fun, #seed=899,
                    centroid = "pam"
                    #control=hierarchical_control(method="average")
)

# Cluster_Number <- c("Cluster 1", "Cluster 2", "Cluster 3")
Cluster_Number <- c("Cluster 1", "Cluster 2", "Cluster 3","Cluster 4")


#display zips and which cluster they belong to
tsclust_data_muap$cluster=model_ts@cluster

##a snap view of how many zctas in each cluster
tsclust_data_muap%>%group_by(cluster)%>%summarise(count=n())
# write.csv(tsclust_data%>%select(ZCTA,cluster), "Google Drive/My Drive/MUAP COVID/case_all_eucl3.csv", row.names = F)
write.csv(tsclust_data_muap%>%select(ZCTA,cluster), "case_muap_eucl_k4_20221006.csv", row.names = F)

##########compute mean/median for each cluster at each year
#read data
#data_for_ts=read.csv("Google Drive/My Drive/MUAP COVID/case_all_eucl3.csv",sep=",",header=T)

#use data from above code
data_for_ts_muap=tsclust_data_muap
zip_cl_muap=data_for_ts_muap%>%gather(Week,case_pop,2:123)
case_plot_muap <-
  zip_cl_muap %>% group_by(cluster, Week) %>% 
  summarise(case = median(case_pop)) %>% #median value for euclidean distance
  setNames(c("Cluster", "Week", "Case")) %>%
  spread(Cluster, Case)%>%setNames(c("Week",Cluster_Number[1:k_n]))%>%
  gather("Cluster","Case",-Week)%>% #add a line re state mean
  mutate(StateMean="No")

state_mean = state_mean_all
# state_mean=zip_cl%>%group_by(Week)%>%
#   summarise(case=median(case_pop))%>%
#   setNames(c("Week","Case"))%>%
#   mutate(Cluster="State median",StateMean="Yes")

#combine state mean with the remaining data
all_to_plot_muap=rbind(case_plot_muap,state_mean)
head(all_to_plot_muap$Week)

##format week column if above result looks weird
#all_to_plot$Week=substring(all_to_plot$Week,2)
#all_to_plot$Week <- gsub("\\.", "/", all_to_plot$Week)

##otherwise go to this step
all_to_plot_muap$Week=as.Date(all_to_plot_muap$Week)

color_values=c('#FF73DF','#FFAA00','#73B2FF','#ABCD66','grey40')


# ggplot(all_to_plot, aes(x = Week, y = Case, group=Cluster)) +
#   geom_line(aes(color = Cluster,linetype=StateMean), size = 1) +
#   # geom_point(aes(color = Cluster)) +
#   theme_bw() + #blank background
#   scale_color_manual(values=color_values[1:4])+
#   ylab("Median case counts per 100,000 population")+
#   scale_x_date(name = 'Week', date_breaks = '2 months')+
#   theme(legend.title=element_text(size=13),legend.text=element_text(size=12))+
#   theme(axis.text.x=element_text(angle = 45, hjust = 1))+
#   guides(linetype=F)


# p <- ggplot(all_to_plot_muap%>%filter(StateMean == 'No'), aes(x = Week, y = Case, group=Cluster)) +
p <- ggplot(all_to_plot_muap, aes(x = Week, y = Case, group=Cluster)) +
  geom_line(aes(color = Cluster,linetype=StateMean), size = 1) +
  # geom_point(aes(color = Cluster)) +
  # theme_bw() + #blank background
  # scale_color_manual(values=color_values[1:4])+
  ggtitle('MUAP')+
  ylab("Median case counts per 100,000 population")+
  scale_color_manual(values=color_values)+
  scale_x_date(name = 'Week', date_breaks = '2 months')+
  theme(legend.title=element_text(size=13),legend.text=element_text(size=12))+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  guides(linetype='none')+
  theme(legend.position = c(0.15, 0.75))+
  list()
p

ggsave("MUAP.png", width = 10, height = 6, dpi = 600)




############# TS Clustering (Non-MUAP) ################


#distance method -- euclidean or dtw_basic
dist_fun="euclidean"

data_pc_k <- tsclust(tsclust_data_nonMupa[,-1], 
                     k=2L:7L, 
                     type="p",
                     preproc=zscore, 
                     distance=dist_fun, 
                     centroid="pam")

names(data_pc_k) <- paste0("k_",2L:7L)
cvi_result <- sapply(data_pc_k,cvi, type="internal")
cvi_result
cvi_result.df <- as.data.frame(cvi_result)
# write.csv(cvi_result.df,"Google Drive/My Drive/MUAP COVID/cvi_result0923.csv",row.names=F)
write.csv(cvi_result.df,"cvi_result20221006_nonMuap.csv",row.names=T)

#move index column as first column
cvi_result.df <- cbind(CV_Index = rownames(cvi_result.df), cvi_result.df)
rownames(cvi_result.df) <- 1:nrow(cvi_result.df)
#follow standard and get the col names of min/max of a row
c_1=colnames(cvi_result.df[which.max(cvi_result.df[1,])])
c_2=colnames(cvi_result.df[which.max(cvi_result.df[2,])])
c_3=colnames(cvi_result.df[which.max(cvi_result.df[3,])])
c_4=colnames(cvi_result.df[which.min(cvi_result.df[4,])])
c_5=colnames(cvi_result.df[which.min(cvi_result.df[5,])])
c_6=colnames(cvi_result.df[which.max(cvi_result.df[6,])])
c_7=colnames(cvi_result.df[which.min(cvi_result.df[7,])])
c_n=c(c_1,c_2,c_3,c_4,c_5,c_6,c_7)
c_n.table=table(c_n)
names(c_n.table)[which.max(c_n.table)]



#define k using the result above
k_n=4L
model_ts <- tsclust(tsclust_data_nonMupa[,-1],type="p",preproc=zscore,
                    k = k_n, 
                    distance = dist_fun, #seed=899,
                    centroid = "pam"
                    #control=hierarchical_control(method="average")
)

Cluster_Number <- c("Cluster 1", "Cluster 2", "Cluster 3","Cluster 4")


#display zips and which cluster they belong to
tsclust_data_nonMupa$cluster=model_ts@cluster

##a snap view of how many zctas in each cluster
tsclust_data_nonMupa%>%group_by(cluster)%>%summarise(count=n())
write.csv(tsclust_data_nonMupa%>%select(ZCTA,cluster), "case_nonMuap_eucl_k4_20221006.csv", row.names = F)

##########compute mean/median for each cluster at each year
#read data
#data_for_ts_nonMuap=read.csv("Google Drive/My Drive/MUAP COVID/case_all_eucl3.csv",sep=",",header=T)

#use data from above code
data_for_ts_nonMuap=tsclust_data_nonMupa
zip_cl_nonMuap=data_for_ts_nonMuap%>%gather(Week,case_pop,2:123)
#zip_cl$Year=str_sub(zip_cl$Year,2,5)
case_plot_nonMuap <-
  zip_cl_nonMuap %>% group_by(cluster, Week) %>% 
  summarise(case = median(case_pop)) %>% #median value for euclidean distance
  setNames(c("Cluster", "Week", "Case")) %>%
  spread(Cluster, Case)%>%setNames(c("Week",Cluster_Number[1:k_n]))%>%
  gather("Cluster","Case",-Week)%>% #add a line re state mean
  mutate(StateMean="No")

state_mean = state_mean_all
# state_mean=zip_cl_nonMuap%>%group_by(Week)%>%
#   summarise(case=median(case_pop))%>%
#   setNames(c("Week","Case"))%>%
#   mutate(Cluster="State median",StateMean="Yes")

#combine state mean with the remaining data
all_to_plot_nonMuap=rbind(case_plot_nonMuap,state_mean)
head(all_to_plot_nonMuap$Week)

##format week column if above result looks weird
#all_to_plot$Week=substring(all_to_plot$Week,2)
#all_to_plot$Week <- gsub("\\.", "/", all_to_plot$Week)

##otherwise go to this step
all_to_plot_nonMuap$Week=as.Date(all_to_plot_nonMuap$Week)

# color_values=c('#FF73DF','#FFAA00','#73B2FF','#ABCD66','#FF7F7F','#FFF000','#A65628','#B3B3B3','#5C2A2E')
color_values=c('#FF73DF','#FFAA00','#73B2FF','#ABCD66','grey40')


# ggplot(all_to_plot, aes(x = Week, y = Case, group=Cluster)) +
#   geom_line(aes(color = Cluster,linetype=StateMean), size = 1) +
#   # geom_point(aes(color = Cluster)) +
#   theme_bw() + #blank background
#   scale_color_manual(values=color_values[1:4])+
#   ylab("Median case counts per 100,000 population")+
#   scale_x_date(name = 'Week', date_breaks = '2 months')+
#   theme(legend.title=element_text(size=13),legend.text=element_text(size=12))+
#   theme(axis.text.x=element_text(angle = 45, hjust = 1))+
#   guides(linetype=F)

p <- ggplot(all_to_plot_nonMuap, aes(x = Week, y = Case, group=Cluster)) +
  geom_line(aes(color = Cluster,linetype=StateMean), size = 1) +
  # geom_point(aes(color = Cluster)) +
  # theme_bw() + #blank background
  # scale_color_manual(values=color_values[1:4])+
  ggtitle('Non-MUAP')+
  ylab("Median case counts per 100,000 population")+
  scale_color_manual(values=color_values)+
  scale_x_date(name = 'Week', date_breaks = '2 months')+
  theme(legend.title=element_text(size=13),legend.text=element_text(size=12))+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  guides(linetype='none')+
  theme(legend.position = c(0.15, 0.75))+
  list()
p

ggsave("Non-MUAP.png", width = 10, height = 6, dpi = 600)




############### Plotting All together #############################
case_plot_muap$MUAP = 'MUAP'
case_plot_muap$Cluster_label = paste0("MUAP:", case_plot_muap$Cluster)
case_plot_nonMuap$MUAP = 'Non-MUAP'
case_plot_nonMuap$Cluster_label = paste0("Non-MUAP:", case_plot_muap$Cluster)
state_mean_1 = state_mean
state_mean_1$MUAP = 'MUAP'
state_mean_1$Cluster_label = 'State median' 
state_mean_2 = state_mean
state_mean_2$MUAP = 'Non-MUAP'
state_mean_2$Cluster_label = 'State median' 

all_to_plot_combo =rbind(case_plot_muap,case_plot_nonMuap)
all_to_plot_combo =rbind(all_to_plot_combo,state_mean_1)
all_to_plot_combo =rbind(all_to_plot_combo,state_mean_2)

all_to_plot_combo$Week=as.Date(all_to_plot_combo$Week)


### Get Colors
library("RColorBrewer")

colors_8 = brewer.pal(8, "Paired")
# colors_8 = brewer.pal(8, "Dark1")
# [1] "#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FB9A99" "#E31A1C"
# [7] "#FDBF6F" "#FF7F00" "#CAB2D6" "#6A3D9A" "#FFFF99" "#B15928"

# Display that palette:
# display.brewer.pal(8, "Paired")
# display.brewer.pal(8, "Set1")
# display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE, colorblindFriendly=FALSE)                   

# color_values=c('#FF73DF','#FFAA00','#73B2FF','#ABCD66','grey40')
colors_9 <- append(colors_8, 'grey40')

p <- ggplot(all_to_plot_combo, aes(x = Week, y = Case, group=Cluster_label)) +
  geom_line(aes(color = Cluster_label,linetype=StateMean), size = 0.8) +
  # geom_point(aes(color = Cluster_label), size= 1.2)+
  facet_grid(. ~ MUAP) +
  # geom_point(aes(color = Cluster)) +
  theme_bw() + #blank background
  # scale_color_manual(values=color_values[1:4])+
  # ggtitle('COVID-19 Infection Rates by ZCTA')+
  ylab("COVID Case rate (per 100,000)")+
  scale_color_manual(values=colors_9)+
  scale_x_date(name = 'Week', date_breaks = '2 months', date_labels= "%Y-%b-%d")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))+
  theme(legend.title=element_text(size=16),
        legend.text=element_text(size=14))+
  # theme(axis.text.x=element_text(angle = 90, vjust= 0))+
  theme(axis.text.x=element_text(angle = 90, vjust=0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 20))+
  guides(linetype='none', color=guide_legend(title="ZCTA Clusters"))+
  # theme(legend.position = c(0.15, 0.75))+
  list()
p

ggsave("Combo_whiteBG_Paired.png", width = 19, height = 5, dpi = 600)








              


##########################################################################
########### Logistic Regression from TS Clust results ####################
##########################################################################

# read data -- non-MUAP data as an example
input_data=read.csv("case_nonMuap_eucl_k4_20221006.csv",sep=",",header=T)
names(input_data)


#variable names -- pay attention to the order of variables
# covar=c("Constant",
#         "% Foreign born, not US citizen",
#         "% Non-Hispanic or Latino African American",
#         "% Hispanic or Latino",
#         "crowding",
#         "% disability",
#         "% Uninsured",
#         "% Employed in Services",
#         "% Unemployed",
#         "% Families below poverty level",
#         "% Did not graduate high school",
#         "Population density",
#         "MUAP",
#         "Large Rural",
#         "Small Rural",
#         "Urban",
#         "% over 65",
#         "% obesity"
# )


covar=c("Constant",
        "% Foreign born, not US citizen",
        "% Non-Hispanic or Latino African American",
        "% Hispanic or Latino",
        "Crowding",
        "Avg. household size",
        "% Disability",
        "% Uninsured",
        "% Employed in services",
        "% Unemployed",
        "% Families below poverty level",
        "% Did not graduate high school",
        "% Speak English less than very well",
        "Population density",
        "Large Rural",
        "Small Rural",
        "Urban",
        "% over 65",
        "% obesity"
)

case_pop_covar=case_pop_full%>%filter(Week=="2020-01-20")
# merge case and SES data and cluster info
SES_case <- merge(input_data,case_pop_covar, by = "ZCTA",all.x=T)

# create an empty data frame to store coefficients
coeff_all=data.frame()
names(case_pop_covar)

# loop through clusters to do logistic regression
for (i in 1:length(unique(input_data$cluster))){
  cluster_data=SES_case%>%mutate(ctg=case_when(cluster==i~1,TRUE~0))
  fit_model=glm(ctg~ForgnBorn+NonHisBlk+Hispanic+Crowding+HouseSize+Disability+Uninsured+EmployService+Unemploy+Poverty+
                  NoHighSchool+EnglshLssWll+PopDens+as.factor(RUCA)+Over65+Obesity,
                family="binomial",data=cluster_data)
  fit_result=data.frame(summary(fit_model)['coefficients'])%>%cbind((confint(fit_model)))%>%
    setNames(c("Coefficient","Std.Error","z_value","P_value","LL","UL"))%>%
    select(Coefficient,P_value,LL,UL)%>%mutate(cluster=i,SES_var=covar)
  coeff_all <- rbindlist(list(coeff_all, fit_result), use.names = T)
}

# test code for one cluster in the loop
# cluster_data=SES_case%>%mutate(ctg=case_when(cluster==3~1,TRUE~0))
# fit_model=glm(ctg~ForgnBorn+NonHisBlk+Hispanic+Crowding+Disability+Uninsured+EmployService+Unemploy+Poverty+
#                NoHighSchool+PopDens+as.factor(MUAP)+as.factor(RUCA)+Over65+Obesity,
#              family="binomial",data=cluster_data)
# fit_model$aic

# test for multi-collinearity               
#model2=aov(cluster~UnEmplRt+NoBachRt+BlwPvtRt+AgeUn18Rt+MinorRt+RUCA_recode,data=SES_case)
#summary(fit_model)

#library(car)
#VIF for multicollinearity
#car::vif(fit_model)



#formate result
coeff_all$Coefficient=round(exp(coeff_all$Coefficient),2)
coeff_all$P_value=round(coeff_all$P_value,3)
coeff_all$LL=round(exp(coeff_all$LL),2)
coeff_all$UL=round(exp(coeff_all$UL),2)
coeff_all$Interval=paste(coeff_all$LL,coeff_all$UL,sep=", ")
coeff_all$result=paste(coeff_all$Coefficient, " (",coeff_all$Interval,")")
coeff_to_save=coeff_all%>%select(SES_var,result,cluster)%>%spread(cluster,result)%>%
  cbind(coeff_all%>%select(SES_var,P_value,cluster)%>%spread(cluster,P_value))
write.csv(coeff_to_save,"logisreg_nonMuap_v2_20221007.csv",row.names=F)
