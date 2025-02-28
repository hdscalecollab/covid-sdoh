## COVID-SDOH Project 
Overview:


### Analysis Workflow
1. Time series clustering -- we first run time series clustering using Euclidean to group ZCTAs with similar trend of incidence during the 122 weeks. There are 4 clusters identified. This process reveals the ZCTAs of peak values in each wave.

1. Variable selection -- we first performed correlation matrix for each pair of the variables. When the coefficients > 0.5 (p<0.05), we removed the variables. Then we performed spatial lag regression with random effect. This analysis is specifically suitable for exploring the relationship between dynamic a outcome variable and static explorataroy variables. We keep the variables with significance (p < 0.05). However, we also kept the commorbidity of obesity based on a domain expert on the team. 

1. GTWR model -- we further explore the relationship between SDOH variables and COVID-19 incidence at a higher resolution. We used the two-step golden-section search function to determine the model badwidth and spatio-temporal scale based on the small-sampled adjusted AIC values. The model returned a correlation coefficient at each time point in each ZCTA. 

1. Visualize output of GTWR --
     1) we visualized temporal trend of coefficient between incidence and each variable (% crowding, % employed in service, % no high school degree, % Hispanic and % non-Hispanic Black) for the counties that had the highest medium COVID-19 incidence during the study period (Stanislaus, Kings, Los Angeles and Sacramento). This is medium coefficient of ZCTAs in each county.
     2) we mapped medium coefficients for each variable during each wave at ZCTA level.


### File structures in this repository:

#### /Analysis
- *Analysis_TSClust_LogisticReg.r* : 
- *VariabelSelection.r* : Creating correlation matirx plot based on Pearson correlation among all variables.
- *Analysis_GTWR_Python.ipynb* : Jupyter Notebook showing the entire workflow of running GTWR model on COVID weekly data 
- (backup)*Analysis_GTWR_R.r* :   R script of the workflow of running GTWR model on COVID weekly data 

#### /Visualization
- *GTWR_Choropleth_v2.ipynb* :
- *GTWR_Results_Visuals_[single_var]_v3.ipynb* : Temporal line graph of GTWR coefficient for each variable

#### /Ouput
- 
