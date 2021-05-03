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
library(odbc)
library(DBI)
library(lubridate)
library(dbhydroR)
library(gghighlight)


#----------------------------------------------Import Data--------------------------------------------------
#Sample Results Data  
Sample_Results <- read_excel("C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/Sample Results Provisional.xlsx")
Sample_Results <- read_excel("C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/Sample Results.xlsx")
sample_results_final <- read_excel("C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/erdp_water_quality_query_time_preservation.xlsx")

# P numbers in project
`Immediate Samples` <- c("P96685","P96686","P96821","P96824","P96827","P96830")
`8-Hour Samples` <- c("P96681","P96683","P96822","P96825","P96828","P96831")
`24-Hour Samples` <- c("P96682","P96684","P96823","P96826","P96829","P102126")

# P numbers in mineral groups
`Low Mineral Group` <- c("P96685","P96686","P96681","P96683","P96682","P96684")
`Moderate Mineral Group` <- c("P96827","P96828","P96829","P96830","P96831","P102126")
`High Mineral Group` <- c("P96821","P96824","P96822","P96825","P96823","P96826")
#Analytes in project    -add remaining parameters
analytes <- c("TN","TPO4","TOC","COLOR","OPO4","CL","SO4","NO2","NOX","SIO2","TDPO4","DOC","TDN","NH4","NA","MG","CA","K","TDSAL","TDSFE") #NO3 and harness are calculated so excluded

# Tidy Data ---------------------------------------------------------------

Sample_Results <- sample_results_final %>%
  rename(SAMPLE_ID="FLDSID",COLLECT_DATE="SDATE",STATION="SITE",SAMPLE_TYPE="TYPE",TEST_NAME="PARAMETER",VALUE="RESULT",UNITS="UNIT") #rename ERDP variables to DBHYDRO format



Sample_Results_Clean <- Sample_Results %>%  
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
  #filter(SAMPLE_TYPE=="SAMP") %>%  #for provisional data 
  select(P_num,SAMPLE_ID,MDL,STATION,SAMPLE_TYPE,COLLECT_DATE,UNCERTAINTY,TEST_NAME,VALUE,UNITS,`Preservation Time`,`Mineral Group`) 


#Split DF into 3 seperate DFs for joining later
Immediate_Results <- Sample_Results_Clean %>%
  filter(`Preservation Time`== "Immediate Samples") %>%         
  rename(Value_15_min=VALUE)

`8-Hour_Results` <- Sample_Results_Clean %>%
  filter(`Preservation Time`== "8-Hour Samples") %>%
  rename(Value_8_hour=VALUE)

`24-Hour_Results` <- Sample_Results_Clean %>%
  filter(`Preservation Time`== "24-Hour Samples") %>%
  rename(Value_24_hour=VALUE)

#Join DFs into single DF arranged wide format
All_results <- Immediate_Results %>%
  full_join(`8-Hour_Results`,by=c("STATION", "COLLECT_DATE","TEST_NAME")) %>%
  full_join(`24-Hour_Results`,by=c("STATION", "COLLECT_DATE","TEST_NAME")) %>%
  select(`Mineral Group`,STATION,MDL,SAMPLE_TYPE,COLLECT_DATE,TEST_NAME,UNITS,Value_15_min,Value_8_hour,Value_24_hour) %>%
  mutate(`15-Min vs 8-Hour`=Value_15_min-Value_8_hour) %>%  #Calculated differnce between 15 min and 8- hour  
  mutate(`15-Min vs 24-Hour`=Value_15_min-Value_24_hour) %>% #Calculated diffence between 15 min and 24- hour 
  drop_na()  #Drop any row with a missing vaulue in any column

#Join DFs into single DF arranged wide format with uncertainty parameter
All_results_Uncertainty <- Immediate_Results %>%
  full_join(`8-Hour_Results`,by=c("STATION", "COLLECT_DATE","TEST_NAME")) %>%
  full_join(`24-Hour_Results`,by=c("STATION", "COLLECT_DATE","TEST_NAME")) %>%
  select(`Mineral Group`,STATION,MDL,SAMPLE_TYPE,COLLECT_DATE,TEST_NAME,UNCERTAINTY, UNCERTAINTY.x,UNCERTAINTY.y,UNITS,Value_15_min,Value_8_hour,Value_24_hour) %>%
  rename(`Immediate Sample Uncertainty`=UNCERTAINTY.x,`8-Hour Sample Uncertainty`=UNCERTAINTY.y,`24-Hour Sample Uncertainty`=UNCERTAINTY) %>%
  mutate(`Immediate Sample Uncertainty`=as.numeric(substr(`Immediate Sample Uncertainty`,4,str_length(`Immediate Sample Uncertainty`)))) %>%
  mutate(`8-Hour Sample Uncertainty`=as.numeric(substr(`8-Hour Sample Uncertainty`,4,str_length(`8-Hour Sample Uncertainty`)))) %>%
  mutate(`24-Hour Sample Uncertainty`=as.numeric(substr(`24-Hour Sample Uncertainty`,4,str_length(`24-Hour Sample Uncertainty`)))) %>%
  mutate(`15-Min vs 8-Hour`=Value_15_min-Value_8_hour) %>%  #Calculated differnce between 15 min and 8- hour  
  mutate(`15-Min vs 24-Hour`=Value_15_min-Value_24_hour) %>% #Calculated diffence between 15 min and 24- hour 
  mutate(`8-Hour Preservation`=`15-Min vs 8-Hour`/`Immediate Sample Uncertainty`) %>%
  mutate(`24-Hour Preservation`=`15-Min vs 24-Hour`/`Immediate Sample Uncertainty`) %>%
  mutate(Date=date(COLLECT_DATE))   %>%
  drop_na()

# Table Data --------------------------------------------------------------

Summary_Stats_8_Hour <- All_results_Uncertainty %>%
  group_by(TEST_NAME) %>%
  summarise(n=n(),`Immediate mean`=mean(Value_15_min),`8-hour mean`=mean(Value_8_hour),`Min Difference`=min(`15-Min vs 8-Hour`),`Max Difference`=max(`15-Min vs 8-Hour`),
            `Median Difference`=median(`15-Min vs 8-Hour`),`Mean Difference`=mean(`15-Min vs 8-Hour`), `Mean % Difference`= percent(mean((Value_8_hour-Value_15_min)/mean(Value_15_min))),
            Units=min(UNITS),MDL=min(MDL), `Std Dev`=sd(`15-Min vs 8-Hour`),`Wilcox Rank Signed`=wilcox.test(Value_15_min,Value_8_hour, paired=TRUE,exact=FALSE)$p.value,
            `T-Test`=t.test(Value_15_min,Value_8_hour, paired=TRUE,exact=FALSE)$p.value,
            `Shapiro Wilks`=shapiro.test(`15-Min vs 8-Hour`)$p.value,
            `MDL/Mean Diff`=min(MDL)/mean(`15-Min vs 8-Hour`),`8-Hour Outside Uncertainty`=sum(ifelse(abs(`8-Hour Preservation`) >1,1,0)),
            `Uncertainty to Diff Ratio 8-Hour`=mean(`8-Hour Preservation`),RMSE=rmse(Value_15_min,Value_8_hour),RSE=rse(Value_15_min,Value_8_hour)) %>%
  mutate_if(is.numeric, round,digits=3) %>%
  mutate_if(is.numeric, format,scientific = FALSE)

Summary_Stats_8_Hour %>% #calculate total # of results and # of differnces greater than uncertainty
  summarise(total=sum(as.numeric(n)),Outsiders=sum(as.numeric(`8-Hour Outside Uncertainty`)))   

Stat_sig_8 <- Summary_Stats_8_Hour  %>% #Statistical Significance by Parameter
  mutate(`Statistically Significant 8-hour`=ifelse(`Wilcox Rank Signed`<.05,"yes","no"))  %>%
  select(TEST_NAME,`Statistically Significant 8-hour`)


Summary_Stats_24_Hour <- All_results_Uncertainty %>%
  group_by(TEST_NAME) %>%
  summarise(n=n(),`Immediate mean`=mean(Value_15_min),`24-hour mean`=mean(Value_24_hour),`Min Difference`=min(`15-Min vs 24-Hour`),`Max Difference`=max(`15-Min vs 24-Hour`),
            `Median Difference`=median(`15-Min vs 24-Hour`),`Mean Difference`=mean(`15-Min vs 24-Hour`),`Mean % Difference`= percent(mean((Value_24_hour-Value_15_min)/mean(Value_15_min))),
            Units=min(UNITS),MDL=min(MDL),`Std Dev`=sd(`15-Min vs 24-Hour`),`Wilcox Rank Signed`=wilcox.test(Value_15_min,Value_24_hour, paired=TRUE,exact=FALSE)$p.value,
            `T-Test`=t.test(Value_15_min,Value_24_hour, paired=TRUE,exact=FALSE)$p.value,
            `Shapiro Wilks`=shapiro.test(`15-Min vs 24-Hour`)$p.value,
            `MDL/Mean Diff`=min(MDL)/mean(`15-Min vs 24-Hour`),
            `24-Hour Outside Uncertainty`=sum(ifelse(abs(`24-Hour Preservation`) >1,1,0)),
            `Uncertainty to Diff Ratio 24-Hour`=mean(`24-Hour Preservation`),RMSE=rmse(Value_15_min,Value_24_hour),RSE=rse(Value_15_min,Value_24_hour)) %>%
  mutate_if(is.numeric, round,digits=3) %>%
  mutate_if(is.numeric, format,scientific = FALSE)

Summary_Stats_24_Hour %>% #calculate total # of results and # of differnces greater than uncertainty
  summarise(total=sum(as.numeric(n)),Outsiders=sum(as.numeric(`24-Hour Outside Uncertainty`)))   

Stat_sig_24 <- Summary_Stats_24_Hour  %>% #Statistical Significance by Parameter
  mutate(`Statistically Significant 24-hour`=ifelse(`Wilcox Rank Signed`<.05,"yes","no"))  %>%
  select(TEST_NAME,`Statistically Significant 24-hour`)

Stat_sig <- Stat_sig_8 %>%  
  left_join(Stat_sig_24)

All_results_Uncertainty_w_Stat_sig <- All_results_Uncertainty %>%
  left_join(Stat_sig)



write.csv(bind_rows(Summary_Stats_8_Hour,Summary_Stats_24_Hour), file = "Results.csv")



# Plot Data ---------------------------------------------------------------
formula <- y ~ x
#15 min vs 8 hour   Jittered to prevent overplotting
ggplot(All_results,aes(Value_15_min,Value_8_hour))+geom_jitter(shape=21)+facet_wrap(~TEST_NAME,scales="free",ncol=3)+
  geom_smooth(method="lm",se=TRUE,formula = formula)+stat_poly_eq(aes(label = paste( ..rr.label.., sep = "~~~")), label.x.npc = "right", formula = formula, parse = TRUE, size = 3)+
  labs(y="8-Hour Preservation",x="15-Minute Preservation",title="Linear Regression: 15-Minute vs 8-Hour Preservation" )

ggsave("8-Hour Regression.jpeg", plot = last_plot(), path ="C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/Figures",width = 8.5, height = 11, units = "in", dpi = 300, limitsize = TRUE)

#15 min vs 24 hour
ggplot(All_results,aes(Value_15_min,Value_24_hour))+geom_point(shape=21)+facet_wrap(~TEST_NAME,scales="free",ncol=3)+
  geom_smooth(method="lm",se=TRUE,formula = formula)+stat_poly_eq(aes(label = paste( ..rr.label.., sep = "~~~")), label.x.npc = "right", formula = formula, parse = TRUE, size = 3)+
  labs(y="24-Hour Preservation",x="15-Minute Preservation",title="Linear Regression: 15-Minute vs 24-Hour Preservation" )

ggsave("24-Hour Regression.jpeg", plot = last_plot(), path ="C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/Figures",width = 8.5, height = 11, units = "in", dpi = 300, limitsize = TRUE)

#Distributions of the differences  
ggplot(gather(All_results_Uncertainty,Difference,Value,`15-Min vs 24-Hour`,`15-Min vs 8-Hour`),aes(Value,fill=Difference))+geom_density(alpha=.8)+facet_wrap(~TEST_NAME,scale="free",ncol = 3)+
  scale_fill_manual(values=c("#66c2a5","#fc8d62"),labels=c("24-Hour Preservation", "8-Hour Preservation"))+geom_vline(xintercept = 0)+theme(legend.position="bottom")+
  labs(y="Density",x="Difference from 15-Minute Preservation",title="Difference from 15-Minute Preservation by Analyte" )


ggsave("Density.jpeg", plot = last_plot(), path ="C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/Figures",width = 8.5, height = 11, units = "in", dpi = 300, limitsize = TRUE)


ggplot(gather(All_results_Uncertainty,Difference,Value,`15-Min vs 24-Hour`,`15-Min vs 8-Hour`),aes(Value,fill=Difference))+geom_histogram(alpha=.8)+facet_wrap(~TEST_NAME,scale="free")+
  scale_fill_manual(values=c("#66c2a5","#fc8d62"))+scale_color_manual(values=c("#b3e2cd","#fdcdac"))+geom_vline(xintercept = 0)

#Residuals vs Unceratainty for all parameters
ggplot(gather(All_results_Uncertainty_w_Stat_sig,"Preservation Time",Value,`24-Hour Preservation`,`8-Hour Preservation`),aes(TEST_NAME,Value,color=`Preservation Time`))+geom_boxplot()+
  scale_color_manual(values=c("#66c2a5","#fc8d62"))+theme_bw()+geom_abline(aes(slope=0,intercept =1),linetype = "dashed")+geom_abline(aes(slope=0,intercept =-1),linetype = "dashed")+
  labs(y="Ratio of Residual to Laboratory Uncertainty",x="Analyte",title="Ratio of Residuals to Laboratory Uncertainty by Analyte" )+
  annotate("point", x = c(14.8,15.2,3.2,17.2,13.8,11.8,7.8), y = c(0,0,0,0,0,0,0),shape=8,color="red")+
  theme(legend.position="bottom", axis.text.x=element_text(size = 11,angle = 90),axis.text.y=element_text(size = 11),text=element_text(size = 16))

ggsave("Residuals vs Unceratainty.jpeg", plot = last_plot(), path ="C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/Figures",width = 8.5, height = 11, units = "in", dpi = 300, limitsize = TRUE)


#Common Plot elements 
Plot1 <- geom_point(aes(Value_15_min,`15-Min vs 24-Hour`,fill="24-hour"),shape=21,size=5,alpha=.8)
Plot2 <- geom_point(aes(Value_15_min,`15-Min vs 8-Hour`,fill="8-hour"),shape=21,size=5,alpha=.8)
Plot3 <- scale_fill_manual(values=c("#b3e2cd","#fdcdac"),name="Preservation Time") 
Plot4 <- geom_smooth(aes(Value_15_min,(`Immediate Sample Uncertainty`+`8-Hour Sample Uncertainty`+`24-Hour Sample Uncertainty`)/3),color="#386cb0",se=FALSE,linetype = "dashed")
Plot5 <- geom_smooth(aes(Value_15_min,-(`Immediate Sample Uncertainty`+`8-Hour Sample Uncertainty`+`24-Hour Sample Uncertainty`)/3),color="#386cb0",se=FALSE,linetype = "dashed")
Plot6 <- geom_abline(aes(slope=0,intercept = min(MDL,na.rm=TRUE)),color="#e41a1c")
Plot7 <- geom_abline(aes(slope=0,intercept = -min(MDL,na.rm=TRUE)),color="#e41a1c")
Plot8 <- geom_blank(aes(y = max(`Immediate Sample Uncertainty`)))
Plot9 <- geom_jitter(aes(Value_15_min,`15-Min vs 24-Hour`,fill="24-hour"),shape=21,size=5,alpha=.8)
Plot10 <- geom_jitter(aes(Value_15_min,`15-Min vs 8-Hour`,fill="8-hour"),shape=21,size=5,alpha=.8)

#Calcium
ggplot(filter(All_results_Uncertainty,TEST_NAME =="CA"),aes(Value_15_min,y=y))+
  Plot1+Plot2+Plot3+Plot4+Plot5+Plot6+Plot7+Plot8+
  annotate(x=50,y=.75,label="MDL",geom = "text",color="#e41a1c" )+
  annotate(x=45,y=4.5,label="Uncertainty",geom = "text" ,color="#386cb0")+theme_bw()+theme(legend.position="bottom")+
  labs(x="mg/L",y="Difference (mg/L)",title="Dissolved Calcium: Residuals by Concentration" )

ggsave("CA.jpeg", plot = last_plot(), path ="C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/Figures",width = 8.5, height = 4, units = "in", dpi = 300, limitsize = TRUE)

#Chloride
ggplot(filter(All_results_Uncertainty,TEST_NAME =="CL"),aes(Value_15_min,y=y))+
  Plot1+Plot2+Plot3+Plot4+Plot5+Plot6+Plot7+Plot8+
  annotate(x=65,y=4.6,label="Uncertainty",geom = "text" ,color="#386cb0")+
  annotate(x=75,y=1.5,label="MDL",geom = "text",color="#e41a1c" )+theme_bw()+theme(legend.position="bottom")+
  labs(x="mg/L",y="Difference (mg/L)",title="Chloride: Residuals by Concentration" )

ggsave("Chloride.jpeg", plot = last_plot(), path ="C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/Figures",width = 8.5, height = 4, units = "in", dpi = 300, limitsize = TRUE)

#Color jitter
ggplot(filter(All_results_Uncertainty,TEST_NAME =="COLOR"),aes(Value_15_min,y=y))+
  Plot9+Plot10+Plot3+Plot4+Plot5+Plot6+Plot7+Plot8+
  annotate(x=67,y=13,label="Uncertainty",geom = "text" ,color="#386cb0")+
  annotate(x=80,y=1.75,label="MDL",geom = "text",color="#e41a1c" )+theme_bw()+theme(legend.position="bottom")+
  labs(x="mg/L",y="Difference (mg/L)",title="Color: Residuals by Concentration" )

ggsave("Color.jpeg", plot = last_plot(), path ="C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/Figures",width = 12.8, height = 7.2, units = "in", dpi = 300, limitsize = TRUE)

#DOC
ggplot(filter(All_results_Uncertainty,TEST_NAME =="DOC"),aes(Value_15_min,y=y))+
  Plot1+Plot2+Plot3+Plot4+Plot5+Plot6+Plot7+Plot8+
  annotate(x=30,y=2,label="Uncertainty",geom = "text" ,color="#386cb0")+
  annotate(x=27.5,y=1,label="MDL",geom = "text",color="#e41a1c" )+theme_bw()+theme(legend.position="bottom")+
  labs(x="mg/L",y="Difference (mg/L)",title="Dissolved Organic Carbon: Residuals by Concentration" )

ggsave("DOC.jpeg", plot = last_plot(), path ="C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/Figures",width = 8.5, height = 4, units = "in", dpi = 300, limitsize = TRUE)

#K jitter
ggplot(filter(All_results_Uncertainty,TEST_NAME =="K"),aes(Value_15_min,y=y))+
  Plot10+Plot9+Plot3+Plot4+Plot5+Plot6+Plot7+Plot8+
  annotate(x=8,y=.55,label="Uncertainty",geom = "text" ,color="#386cb0")+
  annotate(x=8.5,y=.15,label="MDL",geom = "text",color="#e41a1c" )+theme_bw()+theme(legend.position="bottom")+
  labs(x="mg/L",y="Difference (mg/L)",title="Dissolved Potassium: Residuals by Concentration" )


ggsave("K.jpeg", plot = last_plot(), path ="C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/Figures",width = 8.5, height = 4, units = "in", dpi = 300, limitsize = TRUE)

#MG
ggplot(filter(All_results_Uncertainty,TEST_NAME =="MG"),aes(Value_15_min,y=y))+
  geom_point(aes(Value_15_min,`15-Min vs 24-Hour`,fill="24-hour"),shape=21,size=5,alpha=.8)+
  geom_point(aes(Value_15_min,`15-Min vs 8-Hour`,fill="8-hour"),shape=21,size=5,alpha=.8)+
  scale_fill_manual(values=c("grey50","grey90"),name="Treatment") +
  geom_smooth(aes(Value_15_min,(`Immediate Sample Uncertainty`+`8-Hour Sample Uncertainty`+`24-Hour Sample Uncertainty`)/3),color="grey20",se=FALSE,linetype = "dashed")+
  geom_smooth(aes(Value_15_min,-(`Immediate Sample Uncertainty`+`8-Hour Sample Uncertainty`+`24-Hour Sample Uncertainty`)/3),color="grey20",se=FALSE,linetype = "dashed")+
  geom_abline(aes(slope=0,intercept = min(MDL,na.rm=TRUE)),color="grey20")+
  geom_abline(aes(slope=0,intercept = -min(MDL,na.rm=TRUE)),color="grey20")+
  geom_blank(aes(y = max(`Immediate Sample Uncertainty`)))+
  annotate(x=25,y=1.5,label="Uncertainty",geom = "text" ,color="black")+
  annotate(x=17,y=.2,label="MDL",geom = "text",color="black" )+theme_bw()+theme(legend.position="bottom")+
  labs(x=expression(Concentration~(mg~L^{-1})),y=expression(Differences~(mg~L^{-1})),title="" )


ggsave("Fig 10- dissolved MG.jpeg", plot = last_plot(),width = 8, height = 5, units = "in", dpi = 300, limitsize = TRUE)


#NOX
ggplot(filter(All_results_Uncertainty,TEST_NAME =="NOX"),aes(Value_15_min,y=y))+
  geom_jitter(aes(Value_15_min,`15-Min vs 24-Hour`,fill="24-hour"),shape=21,size=5,alpha=.8,width = 0.003,height =  0.002)+
  geom_jitter(aes(Value_15_min,`15-Min vs 8-Hour`,fill="8-hour"),shape=21,size=5,alpha=.8,width = 0.003,height =  0)+
  scale_fill_manual(values=c("grey50","grey90"),name="Treatment") +
  geom_smooth(aes(Value_15_min,(`Immediate Sample Uncertainty`+`8-Hour Sample Uncertainty`+`24-Hour Sample Uncertainty`)/3),color="grey20",se=FALSE,linetype = "dashed")+
  geom_smooth(aes(Value_15_min,-(`Immediate Sample Uncertainty`+`8-Hour Sample Uncertainty`+`24-Hour Sample Uncertainty`)/3),color="grey20",se=FALSE,linetype = "dashed")+
  geom_abline(aes(slope=0,intercept = min(MDL,na.rm=TRUE)),color="grey20")+
  geom_abline(aes(slope=0,intercept = -min(MDL,na.rm=TRUE)),color="grey20")+
  geom_blank(aes(y = max(`Immediate Sample Uncertainty`)))+
  annotate(x=.15,y=.012,label="Uncertainty",geom = "text" ,color="grey20")+
  annotate(x=.2,y=.006,label="MDL",geom = "text",color="grey20")+theme_bw()+theme(legend.position="bottom")+
  labs(x=expression(Concentration~(mg~L^{-1})),y=expression(Differences~(mg~L^{-1})),title="" )


ggsave("Fig 11- NOx.jpeg", plot = last_plot(),width = 8, height = 5, units = "in", dpi = 300, limitsize = TRUE)


#Sodium
ggplot(filter(All_results_Uncertainty,TEST_NAME =="NA"),aes(Value_15_min,y=y))+
  Plot1+Plot2+Plot3+Plot4+Plot5+Plot6+Plot7+Plot8+
  annotate(x=70,y=4.5,label="Uncertainty",geom = "text" ,color="#386cb0")+
  annotate(x=72,y=.75,label="MDL",geom = "text",color="#e41a1c" )+theme_bw()+theme(legend.position="bottom")+
  labs(x="mg/L",y="Difference (mg/L)",title="Dissolved Sodium: Residuals by Concentration" )


ggsave("NA.jpeg", plot = last_plot(), path ="C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/Figures",width = 8.5, height = 4, units = "in", dpi = 300, limitsize = TRUE)

#Ammonium
ggplot(filter(All_results_Uncertainty,TEST_NAME =="NH4"),aes(Value_15_min,y=y))+
  Plot10+Plot9+Plot3+Plot4+Plot5+Plot6+Plot7+Plot8+
  annotate(x=.4,y=.026,label="Uncertainty",geom = "text" ,color="#386cb0")+
  annotate(x=.5,y=.008,label="MDL",geom = "text",color="#e41a1c" )+theme_bw()+theme(legend.position="bottom")+
  labs(x="mg/L",y="Difference (mg/L)",title="NH4: Residuals by Concentration" )


ggsave("NH4.jpeg", plot = last_plot(), path ="C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/Figures",width = 8.5, height = 4, units = "in", dpi = 300, limitsize = TRUE)

#NO2 Jittered
ggplot(filter(All_results_Uncertainty,TEST_NAME =="NO2"),aes(Value_15_min,y=y))+
  Plot9+Plot10+Plot3+Plot4+Plot5+Plot6+Plot7+Plot8+
  annotate(x=.0025,y=.0022,label="Uncertainty",geom = "text" ,color="#386cb0")+
  annotate(x=.0025,y=-.0022,label="MDL",geom = "text",color="#e41a1c" )+theme_bw()+theme(legend.position="bottom")+
  labs(x="mg/L",y="Difference (mg/L)",title="Nitrite: Residuals by Concentration" )

ggsave("NO2.jpeg", plot = last_plot(), path ="C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/Figures",width = 8.5, height = 4, units = "in", dpi = 300, limitsize = TRUE)

#NO3  jitter
ggplot(filter(All_results_Uncertainty,TEST_NAME =="NO3"),aes(Value_15_min,y=y))+
  Plot9+Plot10+Plot3+Plot4+Plot5+Plot6+Plot7+Plot8+
  #annotate(x=.005,y=.0022,label="Uncertainty",geom = "text" ,color="#386cb0")+
  annotate(x=.005,y=-.005,label="MDL",geom = "text",color="#e41a1c" )+
  labs(x="mg/L",y="Difference (mg/L)",title="Nitrate: Residuals by Concentration" )+
  theme_bw()

ggsave("NO3.jpeg", plot = last_plot(), path ="C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/Figures",width = 8.5, height = 4, units = "in", dpi = 300, limitsize = TRUE)

#NOX  Jittered
ggplot(filter(All_results_Uncertainty,TEST_NAME =="NOX"),aes(Value_15_min,y=y))+
  Plot9+Plot10+Plot3+Plot4+Plot5+Plot6+Plot7+Plot8+
  annotate(x=.15,y=.012,label="Uncertainty",geom = "text" ,color="#386cb0")+
  annotate(x=.2,y=.006,label="MDL",geom = "text",color="#e41a1c" )+theme_bw()+theme(legend.position="bottom")+
  labs(x="mg/L",y="Difference (mg/L)",title="Nitrate and Nitrite (NOx): Residuals by Concentration" )+
  
  
  ggsave("NOX.jpeg", plot = last_plot(), path ="C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/Figures",width = 8.5, height = 4, units = "in", dpi = 300, limitsize = TRUE)

#OPO4 Jitter
ggplot(filter(All_results_Uncertainty,TEST_NAME =="OPO4"),aes(Value_15_min,y=y))+
  Plot9+Plot10+Plot3+Plot6+Plot7+Plot8+ #Plot4+PLot5
  annotate(x=.052,y=.005,label="Uncertainty",geom = "text" ,color="#386cb0")+
  geom_smooth(aes(Value_15_min,-(`Immediate Sample Uncertainty`+`8-Hour Sample Uncertainty`+`24-Hour Sample Uncertainty`)/3),color="#386cb0",se=FALSE,method="lm",linetype = "dashed")+
  geom_smooth(aes(Value_15_min,(`Immediate Sample Uncertainty`+`8-Hour Sample Uncertainty`+`24-Hour Sample Uncertainty`)/3),color="#386cb0",se=FALSE,method="lm",linetype = "dashed")+
  annotate(x=.05,y=-.0023,label="MDL",geom = "text",color="#e41a1c" )+theme_bw()+theme(legend.position="bottom")+
  labs(x="mg/L",y="Difference (mg/L)",title="OPO4: Residuals by Concentration" )

ggsave("OPO4.jpeg", plot = last_plot(), path ="C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/Figures",width = 8.5, height = 4, units = "in", dpi = 300, limitsize = TRUE)

#SIO2
ggplot(filter(All_results_Uncertainty,TEST_NAME =="SIO2"),aes(Value_15_min,y=y))+
  Plot1+Plot2+Plot3+Plot4+Plot5+Plot6+Plot7+Plot8+
  annotate(x=11,y=.65,label="Uncertainty",geom = "text" ,color="#386cb0")+
  annotate(x=12,y=.1,label="MDL",geom = "text",color="#e41a1c" )+theme_bw()+theme(legend.position="bottom")+
  labs(x="mg/L",y="Difference (mg/L)",title="Dissolved Silica: Residuals by Concentration" )

ggsave("SIO2.jpeg", plot = last_plot(), path ="C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/Figures",width = 8.5, height = 4, units = "in", dpi = 300, limitsize = TRUE)

#SO4
ggplot(filter(All_results_Uncertainty,TEST_NAME =="SO4"),aes(Value_15_min,y=y))+
  Plot1+Plot2+Plot3+Plot4+Plot5+Plot6+Plot7+Plot8+
  annotate(x=40,y=1.9,label="Uncertainty",geom = "text" ,color="#386cb0")+
  annotate(x=35,y=.25,label="MDL",geom = "text",color="#e41a1c" )+theme_bw()+theme(legend.position="bottom")+
  labs(x="mg/L",y="Difference (mg/L)",title="Sulfate: Residuals by Concentration" )

ggsave("SO4.jpeg", plot = last_plot(), path ="C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/Figures",width = 8.5, height = 4, units = "in", dpi = 300, limitsize = TRUE)


#SO4 with labels
ggplot(filter(All_results_Uncertainty,TEST_NAME =="SO4"),aes(Value_15_min,y=y))+
  Plot3+Plot4+Plot5+Plot6+Plot7+Plot8+
  annotate(x=22,y=.9,label="Uncertainty",geom = "text" ,color="#386cb0")+ geom_label(aes(Value_15_min,`15-Min vs 8-Hour`,label=COLLECT_DATE),shape=21,size=5,alpha=.8)+
  annotate(x=21.5,y=.25,label="MDL",geom = "text",color="#e41a1c" )+geom_label(aes(Value_15_min,`15-Min vs 24-Hour`,label=COLLECT_DATE),shape=21,size=5,alpha=.8)+
  labs(x="mg/L",y="Difference (mg/L)",title="Sulfate: Difference from Immediate Preservation with 8-Hour and 24-Hour Preservation Time Delays by Concentration" )+theme_bw()+theme(legend.position="bottom")

#TDN
ggplot(filter(All_results_Uncertainty,TEST_NAME =="TDN"),aes(Value_15_min,y=y))+
  Plot1+Plot2+Plot3+Plot4+Plot5+Plot6+Plot7+Plot8+
  annotate(x=2,y=.185,label="Uncertainty",geom = "text" ,color="#386cb0")+
  annotate(x=2.25,y=.03,label="MDL",geom = "text",color="#e41a1c" )+
  labs(x="mg/L",y="Difference (mg/L)",title="Total Dissolved Nitrogen: Residuals by Concentration" )+theme_bw()+theme(legend.position="bottom")

ggsave("TDN.jpeg", plot = last_plot(), path ="C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/Figures",width = 8.5, height = 4, units = "in", dpi = 300, limitsize = TRUE)

#TDPO4 Jitter
ggplot(filter(All_results_Uncertainty,TEST_NAME =="TDPO4"),aes(Value_15_min,y=y))+
  Plot9+Plot10+Plot3+Plot4+Plot5+Plot6+Plot7+Plot8+
  annotate(x=.07,y=.008,label="Uncertainty",geom = "text" ,color="#386cb0")+
  annotate(x=.07,y=.0024,label="MDL",geom = "text",color="#e41a1c" )+
  labs(x="mg/L",y="Difference (mg/L)",title="Total Dissolved P: Residuals by Concentration" )+theme_bw()+theme(legend.position="bottom")

ggsave("TDPO4.jpeg", plot = last_plot(), path ="C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/Figures",width = 8.5, height = 4, units = "in", dpi = 300, limitsize = TRUE)

#TDSAL Jitter
ggplot(filter(All_results_Uncertainty,TEST_NAME =="TDSAL"),aes(Value_15_min,y=y))+
  Plot3+Plot6+Plot7+Plot8+Plot9+Plot10+ #Plot4+Plot5
  annotate(x=12.5,y=9,label="Uncertainty",geom = "text" ,color="#386cb0")+
  geom_smooth(aes(Value_15_min,-(`Immediate Sample Uncertainty`+`8-Hour Sample Uncertainty`+`24-Hour Sample Uncertainty`)/3),color="#386cb0",se=FALSE,method="lm",linetype = "dashed")+
  geom_smooth(aes(Value_15_min,(`Immediate Sample Uncertainty`+`8-Hour Sample Uncertainty`+`24-Hour Sample Uncertainty`)/3),color="#386cb0",se=FALSE,method="lm",linetype = "dashed")+
  annotate(x=8.5,y=9,label="MDL",geom = "text",color="#e41a1c" )+
  labs(x="ug/L",y="Difference (ug/L)",title="Aluminum: Residuals by Concentration" )+theme_bw()+theme(legend.position="bottom")

ggsave("TDSAL.jpeg", plot = last_plot(), path ="C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/Figures",width = 8.5, height = 4, units = "in", dpi = 300, limitsize = TRUE)

#TOC
ggplot(filter(All_results_Uncertainty,TEST_NAME =="TOC"),aes(Value_15_min,y=y))+
  Plot1+Plot2+Plot3+Plot4+Plot5+Plot6+Plot7+Plot8+
  annotate(x=29,y=2.6,label="Uncertainty",geom = "text" ,color="#386cb0")+
  annotate(x=30,y=.95,label="MDL",geom = "text",color="#e41a1c" )+
  labs(x="mg/L",y="Difference (mg/L)",title="Total Organic Carbon: Residuals by Concentration" )+theme_bw()+theme(legend.position="bottom")

ggsave("TOC.jpeg", plot = last_plot(), path ="C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/Figures",width = 8.5, height = 4, units = "in", dpi = 300, limitsize = TRUE)

#TDSFE #jittered
ggplot(filter(All_results_Uncertainty,TEST_NAME =="TDSFE"),aes(Value_15_min,y=y))+
  Plot3+Plot4+Plot5+Plot6+Plot7+Plot8+Plot9+Plot10+
  annotate(x=7,y=3.5,label="Uncertainty",geom = "text" ,color="#386cb0")+
  annotate(x=5,y=3.5,label="MDL",geom = "text",color="#e41a1c" )+
  labs(x="ug/L",y="Difference (ug/L)",title="Dissolved Iron: Residuals by Concentration" )+theme_bw()+theme(legend.position="bottom")

ggsave("TDSFE.jpeg", plot = last_plot(), path ="C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/Figures",width = 8.5, height = 4, units = "in", dpi = 300, limitsize = TRUE)

#TN
ggplot(filter(All_results_Uncertainty,TEST_NAME =="TN"),aes(Value_15_min,y=y))+
  Plot1+Plot2+Plot3+Plot4+Plot5+Plot6+Plot7+Plot8+
  annotate(x=2,y=.17,label="Uncertainty",geom = "text" ,color="#386cb0")+
  annotate(x=2.5,y=.04,label="MDL",geom = "text",color="#e41a1c" )+
  labs(x="mg/L",y="Difference (mg/L)",title="Total Nitrogen: Residuals by Concentration" )+theme_bw()+theme(legend.position="bottom")

ggsave("TN.jpeg", plot = last_plot(), path ="C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/Figures",width = 8.5, height = 4, units = "in", dpi = 300, limitsize = TRUE)

#TPO4
ggplot(filter(All_results_Uncertainty,TEST_NAME =="TPO4"),aes(Value_15_min,y=y))+
  Plot1+Plot2+Plot3+Plot4+Plot5+Plot6+Plot7+Plot8+
  annotate(x=.09,y=.008,label="Uncertainty",geom = "text" ,color="#386cb0")+
  annotate(x=.13,y=.0026,label="MDL",geom = "text",color="#e41a1c" )+
  labs(x="mg/L",y="Difference (mg/L)",title="Total P: Residuals by Concentration" )+theme_bw()+theme(legend.position="bottom")

ggsave("TPO4.jpeg", plot = last_plot(), path ="C:/Users/mpowers/OneDrive - South Florida Water Management District/Project Management/WQTT/Processing Time SxS/Figures",width = 8.5, height = 4, units = "in", dpi = 300, limitsize = TRUE)


# Test code ---------------------------------------------------------------

#Test direct connetion to LIMSP 

download.packages(pkgs = "dbhydroR", 
                  destdir = ".",
                  type = "source")

servfull <- "http://my.sfwmd.gov/dbhydroplsql/water_quality_data.report_full"


#DSN=limsp;UID=pub;DBQ=LIMS2P;DBA=W;APA=T;EXC=F;FEN=T;QTO=F;FRC=10;FDL=10;LOB=T;RST=T;BTD=F;BNF=F;BAM=IfAllSuccessful;
#NUM=NLS;DPM=F;MTS=T;MDI=F;CSR=F;FWC=F;FBS=64000;TLO=O;MLD=0;ODA=F;STE=F;TSZ=8192;

#DSN=wrep;UID=pub;;DBQ=WREP;DBA=W;APA=T;EXC=F;FEN=T;QTO=T;FRC=10;FDL=10;LOB=T;RST=T;BTD=F;BNF=F;BAM=IfAllSuccessful;
#NUM=NLS;DPM=F;MTS=T;MDI=F;CSR=F;FWC=F;FBS=64000;TLO=O;MLD=0;ODA=F;STE=F;TSZ=8192;

#DBA="W";APA="T";EXC="F";FEN="T";QTO="F";FRC=10;FDL=10;LOB="T";RST="T";BTD="F";BNF="F",
#BAM="IfAllSuccessful";NUM="NLS";DPM="F";MTS="T";MDI="F";CSR="F";FWC="F";FBS=64000;TLO=O;MLD=0;ODA=F;STE=F;TSZ=8192; 


conn<- odbcConnect("wrep", uid="pub", pwd="pub", believeNRows=FALSE)

Tables_Hydro <- sqlTables(conn, schema="DMDBASE")

#Columns_Hydro <- as.data.frame(colnames(sqlFetch(conn, "Table in DMDBASE")))
#DATA <- sqlQuery(conn,paste(SQL statement from MS Excel)


con <- dbConnect(odbc::odbc(), DSN="lims2p", UID="pub", PWD= "pub",DBQ="LIMS2P")

con <- dbConnect(odbc::odbc(), DSN="wrep", UID="pub", PWD= "pub",DBQ="c:/users/mpowers/appdata/roaming/microsoft/queries/WREP")

con <- DBI::dbConnect(odbc::odbc(),  Driver   = "PostgreSQL Driver",DSN="wrep",Server = "c:/users/mpowers/appdata/roaming/microsoft/queries/WREP", UID = "pub",PWD = "pub",Port = 1433)







#Difference between 15min and 8 hour by concentration
ggplot(All_results,aes(Value_15_min,`15-Min vs 8-Hour`))+geom_point(shape=21)+facet_wrap(~TEST_NAME, scales="free")
#geom_abline(aes(slope=0,intercept = min(MDL)))+geom_blank(aes(y = max(MDL)))

#Difference between 15min and 24 hour by concentration
ggplot(All_results,aes(Value_15_min,`15-Min vs 24-Hour`))+geom_point(shape=21)+facet_wrap(~TEST_NAME,scales="free")+ geom_abline(aes(slope=0,intercept = mean(MDL),na.rm=TRUE))

#boxplots faceted  24-hour
ggplot(All_results,aes(TEST_NAME,`15-Min vs 24-Hour`))+geom_boxplot()+facet_wrap(~TEST_NAME, scales="free")+geom_abline(aes(slope=0,intercept = mean(MDL)),color="red")+
  geom_abline(aes(slope=0,intercept = -mean(MDL)),color="red")+geom_blank(aes(y = min(MDL)))

#boxplots faceted 8-hour
ggplot(All_results,aes(TEST_NAME,`15-Min vs 8-Hour`))+geom_boxplot()+facet_wrap(~TEST_NAME, scales="free")+geom_abline(aes(slope=0,intercept = mean(MDL)),color="red")+
  geom_abline(aes(slope=0,intercept = -mean(MDL)),color="blue")+geom_blank(aes(y = min(MDL)))#+
#scale_y_continuous(expand = c(0, 0))+geom_blank(aes(y = c(min(MDL),max(MDL))))

#boxplots


ggplot(All_results,aes(TEST_NAME,`15-Min vs 24-Hour`))+geom_boxplot()

ggplot(filter(All_results,TEST_NAME=="COLOR"),aes(sample=`15-Min vs 8-Hour`))+geom_qq()

ggplot(All_results,aes(`15-Min vs 24-Hour`))+geom_histogram()+facet_wrap(~TEST_NAME, scales="free")

ggplot(All_results,aes(`15-Min vs 24-Hour`,TEST_NAME,fill=TEST_NAME))+geom_density_ridges2()

#Residuals of 24-hour and MDL for NO2 and OPO4
ggplot(filter(All_results,TEST_NAME %in% c("NO2","OPO4")),aes(Value_15_min,`15-Min vs 24-Hour`,color=TEST_NAME,fill=TEST_NAME))+geom_point(shape=21)+ 
  geom_abline(aes(slope=0,intercept = min(MDL)))+geom_blank(aes(y = min(MDL)))+annotate(x=.001,y=.0018,label="MDL",geom = "text" )+
  labs(x="mg/L",y="15min Preservation - 24Hour Presevation (mg/L)" )  

#Residuals of 8-hour and MDL
ggplot(filter(All_results,TEST_NAME %in% c("NO2","OPO4")),aes(Value_15_min,`15-Min vs 8-Hour`,color=TEST_NAME,fill=TEST_NAME))+geom_point(shape=21)+ 
  geom_abline(aes(slope=0,intercept = min(MDL)))+geom_blank(aes(y = min(MDL)))+annotate(x=.001,y=.0018,label="MDL",geom = "text" )+
  labs(x="mg/L",y="15min Preservation - 8Hour Presevation (mg/L)" )

ggplot(filter(All_results,TEST_NAME %in% c("NH4","NOX","NO3")),aes(Value_15_min,`15-Min vs 8-Hour`,color=TEST_NAME,fill=TEST_NAME))+geom_point(shape=21)+ 
  geom_abline(aes(slope=0,intercept = min(MDL)))+geom_blank(aes(y = min(MDL)))+annotate(geom = "text", x=.001,y=.0046,label="MDL")+
  labs(x="mg/L",y="15min Preservation - 8Hour Presevation (mg/L)" )  

ggplot(filter(All_results,TEST_NAME %in% c("K","MG")),aes(Value_15_min,`15-Min vs 8-Hour`,color=TEST_NAME,fill=TEST_NAME))+geom_point(shape=21)+ 
  geom_abline(aes(slope=0,intercept = min(MDL)))+geom_blank(aes(y = min(MDL)))+annotate(geom = "text", x=.001,y=.018,label="MDL")+
  labs(x="mg/L",y="15min Preservation - 8Hour Presevation (mg/L)" )  

ggplot(filter(All_results,TEST_NAME %in% c("CA")),aes(Value_15_min,`15-Min vs 8-Hour`,color=TEST_NAME,fill=TEST_NAME))+geom_point(shape=21)+ 
  geom_abline(aes(slope=0,intercept = min(MDL)))+geom_blank(aes(y = min(MDL)))+annotate(geom = "text", x=.001,y=.28,label="MDL")+
  labs(x="mg/L",y="15min Preservation - 8Hour Presevation (mg/L)" )  

ggplot(filter(All_results,TEST_NAME %in% c("TOC","DOC")),aes(Value_15_min,`15-Min vs 8-Hour`,color=TEST_NAME,fill=TEST_NAME))+geom_point(shape=21)+ 
  geom_abline(aes(slope=0,intercept = min(MDL)))+geom_blank(aes(y = min(MDL)))+annotate(geom = "text", x=.001,y=.75,label="MDL")+
  labs(x="mg/L",y="15min Preservation - 8Hour Presevation (mg/L)" )  

ggplot(filter(All_results_Uncertainty,TEST_NAME %in% c("TPO4")),aes(Value_15_min,`15-Min vs 8-Hour`))+geom_point(shape=21,size=3,color="red")+ 
  geom_point(data=filter(All_results_Uncertainty,TEST_NAME %in% c("TPO4")),aes(Value_15_min,`Immediate Sample Uncertainty`),color="blue")+
  geom_abline(aes(slope=0,intercept = min(MDL)))+geom_blank(aes(y = max(`Immediate Sample Uncertainty`)))#+annotate(x=.001,y=.0018,label="MDL",geom = "text" )+
labs(x="mg/L",y="15min Preservation - 8Hour Presevation (mg/L)" )

