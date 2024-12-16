remove(list=ls())

library(Metrics)
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(viridis)
library(scales)
library(stringr)
library(utils)
library(ggpmisc)
#library(odbc)
library(DBI)
library(lubridate)
library(dbhydroR)
library(gghighlight)
library(moments)


#----------------------------------------------Import Data--------------------------------------------------
#Sample Results Data  
sample_results_final <- read_excel("./Data/erdp_water_quality_query_time_preservation.xlsx")

#Analytes in project    -add remaining parameters
analytes <- c("TN","TPO4","TOC","COLOR","OPO4","CL","SO4","NO2","NOX","SIO2","TDPO4","DOC","TDN","NH4","NA","MG","CA","K","TDSAL","TDSFE") #NO3 and harness are calculated so excluded

# Tidy Data ---------------------------------------------------------------

# P numbers in project
`Immediate Samples` <- c("P96685","P96686","P96821","P96824","P96827","P96830")
`8-Hour Samples` <- c("P96681","P96683","P96822","P96825","P96828","P96831")
`24-Hour Samples` <- c("P96682","P96684","P96823","P96826","P96829","P102126")

# P numbers in mineral groups
`Low Mineral Group` <- c("P96685","P96686","P96681","P96683","P96682","P96684")
`Moderate Mineral Group` <- c("P96827","P96828","P96829","P96830","P96831","P102126")
`High Mineral Group` <- c("P96821","P96824","P96822","P96825","P96823","P96826")

#Analytes with normal distributions
analytes_Normal_8 <- c("CA","CL","MG","NA","SIO2")         #Normal based on shapiro-wilks for 8-hour data
analytes_Normal_24 <- c("CA","DOC","K","NA","TN","TOC")    #based on shapiro-wilks for 24-hour data

analytes_nonparametric_8 <- c("COLOR","DOC","K","NO2","OPO4","SO4","TDN","TDSFE","TN","TOC","TPO4","NH4")   #test  removed TDSAL due to lab method
analytes_nonparametric_24 <- c("CL","COLOR","MG","NO2","OPO4","SIO2","SO4","TDN","TDSFE","TPO4","NH4") #test removed TDSAL due to lab method

analytes_non_constant_variance_8 <- c("NOX","TDPO4")  #analytes with non constant variance. 8 hour removed TDSAL due to lab method
analytes_non_constant_variance_24 <- c("NOX","TDPO4") #analytes with non constant variance. 24 hour removed TDSAL due to lab method

#Projects in scope of work
Projects <- c("EVPA","OLIT","RTBG","EVER","WCA1T","WCA-2A")

# Tidy Data ---------------------------------------------------------------

Sample_Results <- sample_results_final %>%
rename(SAMPLE_ID="FLDSID",COLLECT_DATE="SDATE",STATION="SITE",SAMPLE_TYPE="TYPE",TEST_NAME="PARAMETER",VALUE="RESULT",UNITS="UNIT") #rename ERDP variables to DBHYDRO format


Sample_Results_Tidy <- Sample_Results %>%  
filter(is.na(FQC)) %>%
mutate(P_num=substr(`SAMPLE_ID`,1,nchar(`SAMPLE_ID`)-2)) %>%  #remove dash numbers from P# 
mutate(`Preservation Time`=ifelse(P_num %in% `Immediate Samples`,"Immediate Samples","")) %>%   #Group samples by preservation time
mutate(`Preservation Time`=ifelse(P_num %in% `8-Hour Samples`,"8-Hour Samples",`Preservation Time`))  %>%
mutate(`Preservation Time`=ifelse(P_num %in% `24-Hour Samples`,"24-Hour Samples",`Preservation Time`))  %>%
mutate(`Mineral Group`=ifelse(P_num %in% `Low Mineral Group`,"Low Mineral Group","")) %>%   #Group samples by preservation time
mutate(`Mineral Group`=ifelse(P_num %in% `Moderate Mineral Group`,"Moderate Mineral Group",`Mineral Group`))  %>%
mutate(`Mineral Group`=ifelse(P_num %in% `High Mineral Group`,"High Mineral Group",`Mineral Group`))  %>%
filter(nchar(`Preservation Time`)>0) %>%  #remove data not in study
filter(TEST_NAME %in% analytes) %>%   #Remove analytes not in study
filter(SAMPLE_TYPE=="SURFACE WATER") %>%
mutate(`UNCERTAINTY`=as.numeric(substr(`UNCERTAINTY`,4,str_length(`UNCERTAINTY`)))) %>%
mutate(`VALUE` = case_when(`TEST_NAME`=="TDSAL"~ VALUE/1000,`TEST_NAME`=="TDSFE"~ VALUE/1000,TRUE ~ as.numeric(as.character(.$VALUE)))) %>%   #Convert ug/l to mg/l
mutate(`MDL` = case_when(`TEST_NAME`=="TDSAL"~ `MDL`/1000,`TEST_NAME`=="TDSFE"~ `MDL`/1000,TRUE ~ as.numeric(as.character(.$MDL)))) %>%       #convert mdl to mg/l from ug/l
mutate(`PQL` = case_when(`TEST_NAME`=="TDSAL"~ `PQL`/1000,`TEST_NAME`=="TDSFE"~ `PQL`/1000,TRUE ~ as.numeric(as.character(.$PQL)))) %>%       #convert PQL to mg/l from ug/l
mutate(`PQL` = case_when(`TEST_NAME`=="TDSAL"~ `PQL`/1000,`TEST_NAME`=="TDSFE"~ `PQL`/1000,TRUE ~ as.numeric(as.character(.$PQL)))) %>%       #convert PQL to mg/l from ug/l
mutate(`UNCERTAINTY` = case_when(`TEST_NAME`=="TDSAL"~ `UNCERTAINTY`/1000,`TEST_NAME`=="TDSFE"~ `UNCERTAINTY`/1000,TRUE ~ as.numeric(as.character(.$UNCERTAINTY)))) %>%     #convert mdl to mg/l from ug/l
select(P_num,SAMPLE_ID,MDL,PQL,STATION,SAMPLE_TYPE,COLLECT_DATE,UNCERTAINTY,TEST_NAME,VALUE,UNITS,`Preservation Time`,`Mineral Group`,REMARK) 

write.csv(Sample_Results_Tidy,file="./Data/Sample_Results_Tidy.csv",row.names=FALSE)



#Split DF into 3 separate DFs for joining later
Immediate_Results <- Sample_Results_Tidy %>%
filter(`Preservation Time`== "Immediate Samples") %>%         
rename(Value_15_min=VALUE)

`8-Hour_Results` <- Sample_Results_Tidy %>%
filter(`Preservation Time`== "8-Hour Samples") %>%
rename(Value_8_hour=VALUE)

`24-Hour_Results` <- Sample_Results_Tidy %>%
filter(`Preservation Time`== "24-Hour Samples") %>%
rename(Value_24_hour=VALUE)

k<-1000

#Join DFs into single DF arranged wide format with uncertainty parameter
All_results_Uncertainty <- Immediate_Results %>%
full_join(`8-Hour_Results`,by=c("STATION", "COLLECT_DATE","TEST_NAME")) %>%
full_join(`24-Hour_Results`,by=c("STATION", "COLLECT_DATE","TEST_NAME")) %>%
select(`Mineral Group`,STATION,MDL,PQL,SAMPLE_TYPE,COLLECT_DATE,TEST_NAME,UNCERTAINTY, UNCERTAINTY.x,UNCERTAINTY.y,UNITS,Value_15_min,Value_8_hour,Value_24_hour,REMARK.x,REMARK.y,REMARK) %>%
rename(`Immediate Sample Uncertainty`=UNCERTAINTY.x,`8-Hour Sample Uncertainty`=UNCERTAINTY.y,`24-Hour Sample Uncertainty`=UNCERTAINTY,`Immediate Sample Remark`=REMARK.x,`8-Hour Sample Remark`=REMARK.y,`24-Hour Sample Remark`=REMARK) %>%
mutate(Value_15_min=ifelse(Value_15_min >MDL & Value_15_min<=PQL,PQL,Value_15_min)) %>% #Transforms values below the PQL to the PQL  
mutate(Value_8_hour=ifelse(Value_8_hour >MDL & Value_8_hour<=PQL,PQL,Value_8_hour)) %>%  #Transforms values below the PQL to the PQL
mutate(Value_24_hour=ifelse(Value_24_hour >MDL & Value_24_hour<=PQL,PQL,Value_24_hour)) %>% #Transforms values below the PQL to the PQL
mutate(`Difference (15-Min - 8-Hour)`=signif(Value_15_min-Value_8_hour,digits=5)) %>%  #Calculated differnce between 15 min and 8- hour  
mutate(`Difference (15-Min - 24-Hour)`=signif(Value_15_min-Value_24_hour,digits=5)) %>% #Calculated diffence between 15 min and 24- hour 
mutate(`Log10 Difference (15-Min - 8-Hour)`=log10(Value_15_min*k)-log10(Value_8_hour*k)) %>%
mutate(`Log10 Difference (15-Min - 24-Hour)`=log10(Value_15_min*k)-log10(Value_24_hour*k)) %>% #Calculated log differences between 15 min and 24 hour. If difference is 0 return 0   
mutate(`SQRT Difference (15-Min - 8-Hour)`=sqrt(Value_15_min)-sqrt(Value_8_hour)) %>%
mutate(`SQRT Difference (15-Min - 24-Hour)`=sqrt(Value_15_min)-sqrt(Value_24_hour)) %>%  #Calculated SQRT differences between 15 min and 24 hour. If difference is 0 return 0   
mutate(`Cubic Root Difference (15-Min - 8-Hour)`=(Value_15_min)^(1/3)-(Value_8_hour)^(1/3)) %>%
mutate(`Cubic Root Difference (15-Min - 24-Hour)`=(Value_15_min)^(1/3)-(Value_24_hour)^(1/3)) %>%
mutate(`(15-min and 8-hour)/2`=(Value_15_min+Value_8_hour)/2) %>%
mutate(`(15-min and 24-hour)/2`=(Value_15_min+Value_24_hour)/2) %>%
mutate(`8-Hour Percent Error`=100*`Difference (15-Min - 8-Hour)`/(`(15-min and 8-hour)/2`)) %>%         #Percent Error 8-hour
mutate(`24-Hour Percent Error`=100*`Difference (15-Min - 24-Hour)`/`(15-min and 24-hour)/2`) %>%       #Percent Error 24-hour    
mutate(Date=date(COLLECT_DATE))

write.csv(All_results_Uncertainty,file="./Data/All_results_Uncertainty.csv",row.names=FALSE)

# Censored Data Analysis -----------------------------------------------------------

sample_results_remarks <- sample_results_final %>%
filter(is.na(FQC)) %>%
filter(PARAMETER %in% analytes) %>% 
group_by(PARAMETER,REMARK) %>%
summarise(n=n(),Percent=percent(n()/108))

sample_results_censored <- sample_results_final %>%
filter(is.na(FQC)) %>%
filter(PARAMETER %in% analytes) %>%   #Remove analytes not in study
mutate(`Non-detect`=case_when(str_detect(REMARK,"U")==TRUE ~ "Below MDL", str_detect(REMARK,"I")==TRUE ~ "Below PQL" )) 

All_results_LOA_8_hour_test <-  All_results_LOA_8_hour %>%
gather("Time","Value",11:13)

ggplot(All_results_LOA_8_hour_test,aes(Time,Value,color=Time))+geom_boxplot()+facet_wrap(~TEST_NAME,scales="free")+  
geom_hline(aes(yintercept = MDL.x), colour ="black" ,size = 1,linetype="dashed")

sample_results_censored_summary <-  sample_results_censored%>%
group_by(PARAMETER,`Non-detect`) %>%
summarise(n=n(),Percent=percent(n()/108))

ggplot(filter(All_results_Uncertainty,TEST_NAME=="NOX"),aes(Value_15_min))+geom_histogram(binwidth = .03)

ggplot(filter(All_results_Uncertainty,TEST_NAME=="TDSAL"),aes(Value_15_min,Value_8_hour))+geom_jitter(width=.0002,height = 0,alpha=.5,shape=21)+scale_y_continuous(breaks = scales::pretty_breaks(n = 25))

#create DF of uncensored values
All_results_Uncertainty_uncensored <- All_results_Uncertainty %>% #censored values removed
mutate(`Non-detect`=ifelse(str_detect(`Immediate Sample Remark`,"U")==TRUE | str_detect(`8-Hour Sample Remark`,"U")==TRUE |  str_detect(`24-Hour Sample Remark`,"U")==TRUE,"Below MDL",NA)) %>%
filter(is.na(`Non-detect`))

