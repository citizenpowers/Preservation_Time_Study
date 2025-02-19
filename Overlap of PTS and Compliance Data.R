#The goal of this script is to explore the overlap between the PTS data set and compliance data set
#Two objectives. (A) Calculate percent of compliance data within PTS range. (B) Calculate percent of compliance range that PTS covers. 
#1a.) Calculate range of PTS data
#2a.) Flag compliance data within range of PTS data
#3a.) For each analyte calculate percent of compliance data within range of PTS data

#1b.) Calculate range of compliance data
#2b.) Calculate the percent of compliance range PTS covers using the product of 1a and 1b. 

#1c.) Calculate the percent of compliance data within 10x of sample bias

library(readxl)
library(tidyverse)
library(scales)
library(gghighlight)
library(ggridges)
library(ggpmisc)
library(cowplot)

#----------------------------------------------Import Data--------------------------------------------------
#PTS Data
sample_results_final <- read_excel("./Data/erdp_water_quality_query_time_preservation.xlsx")

#Compliance data from marsh stations
EVPA_Data <- read_excel("./Data/Replicate Sample Data.xlsx", sheet = "EVPA")
RTBG_Data <- read_excel("./Data/Replicate Sample Data.xlsx", sheet = "RTBG")
OLIT_Data <- read_excel("./Data/Replicate Sample Data.xlsx", sheet = "OLIT")
WCA1T_Data <- read_excel("./Data/Replicate Sample Data.xlsx", sheet = "WCA1T")
WCA2A_Data <- read_excel("./Data/Replicate Sample Data.xlsx", sheet = "WCA-2A")


#List of study analytes in ERDP format
analytes_ERDP <- c("TN","TPO4","TOC","COLOR","OPO4","CL","SO4","NO2","NOX","SIO2","TDPO4","DOC","TDN","NH4","NA","MG","CA","K","TDSAL","TDSFE") #NO3 and harness are calculated so excluded

#Bias from delayed sample sets
Bias_Data <- read_csv("./Data/Summary_Table.csv") %>% mutate(TEST_NAME=if_else(is.na(TEST_NAME),"NA",TEST_NAME))

# Tidy Data ---------------------------------------------------------------

#Convert PTS data stored in ERDP format to DBHYDRO format to match the compliance data 
Sample_Results <- sample_results_final %>%
rename(SAMPLE_ID="FLDSID",COLLECT_DATE="SDATE",STATION="SITE",SAMPLE_TYPE="TYPE",TEST_NAME="PARAMETER",VALUE="RESULT",UNITS="UNIT") %>% #rename ERDP variables to DBHYDRO format 
filter(STATION != "EB") 
# 1a.) Calculate range of PTS data -----------------------------------------

#Min and Max in PTS dataset for each parameter
PTS_Range <- Sample_Results %>%
filter(STATION != "EB")  %>%
group_by(TEST_NAME) %>%
summarise(min_pts=min(VALUE,na.rm=T),max_pts=max(VALUE,na.rm=T),
median=median(VALUE,na.rm=T),mean=mean(VALUE,na.rm=T),SD=round(sd(VALUE,na.rm=T),digits=3),
IQR_2.5=quantile(VALUE,0.025),IQR_25=quantile(VALUE,.25),IQR_75=quantile(VALUE,.75) ,IQR_25=quantile(VALUE,.25),IQR_75=quantile(VALUE,.75),IQR_97.5=quantile(VALUE,.975),IQR_99=quantile(VALUE,.99)) %>%
mutate(Range_pts=max_pts-min_pts)


# 2.)  Flag compliance data within range of PTS data ---------------------------

Compliance_Marsh_Data <- EVPA_Data  %>% bind_rows(RTBG_Data)   %>% bind_rows(OLIT_Data)   %>% bind_rows(WCA1T_Data)  %>% bind_rows(WCA2A_Data)  %>%
filter(MATRIX=="SW",SAMPLE_TYPE_NEW!="EB",SAMPLE_TYPE_NEW!="FCEB",COLLECT_METHOD=="G",is.na(FLAG),SAMPLE_TYPE_NEW=="SAMP" | SAMPLE_TYPE_NEW=="RS" ) %>% 
mutate(VALUE=abs(VALUE)) %>%  #Compliance samples reported as negative values when below detection limits
mutate(TEST_NAME= case_when(TEST_NAME=="PHOSPHATE, ORTHO AS P" ~"OPO4",             #rename to ERDP format
                              TEST_NAME=="PHOSPHATE, TOTAL AS P" ~"TPO4",
                              TEST_NAME=="PHOSPHATE, DISSOLVED AS P" ~"TDPO4",
                              TEST_NAME=="IRON, TOTAL" ~"TDSFE", 
                              TEST_NAME=="SILICA" ~"SIO2",
                              TEST_NAME=="HARDNESS AS CACO3" ~"Hardness",
                              TEST_NAME=="ALKALINITY, TOT, CACO3" ~"Alkalinity",  
                              TEST_NAME=="CARBON, DISSOLVED ORGANIC" ~"DOC",
                              TEST_NAME=="AMMONIA-N" ~"NH4",
                              TEST_NAME=="KJELDAHL NITROGEN, DIS" ~ "TDN",
                              TEST_NAME=="NITRATE+NITRITE-N" ~ "NOX",
                              TEST_NAME=="CARBON, TOTAL ORGANIC" ~ "TOC",
                              TEST_NAME=="KJELDAHL NITROGEN, TOTAL" ~ "TN",
                              TEST_NAME=="SODIUM" ~"NA",  
                              TEST_NAME=="MAGNESIUM" ~"MG",
                              TEST_NAME=="CALCIUM" ~"CA",
                              TEST_NAME=="CHLORIDE" ~"CL",
                              TEST_NAME=="POTASSIUM" ~"K", 
                              TEST_NAME=="SULFATE" ~ "SO4",
                              TEST_NAME=="NITRITE-N" ~ "NO2",
                              TRUE ~ as.character(TEST_NAME))) %>%
filter(TEST_NAME  %in% analytes_ERDP ) %>%
left_join(PTS_Range,by="TEST_NAME") %>%                                             #join min_pts and max_pts from PTS data 
mutate(Flag= case_when(TEST_NAME=="OPO4" & between(`VALUE`,min_pts,max_pts) ~"Inside",      #flag data that is within PTS data range
                         TEST_NAME=="TPO4" & between(`VALUE`,min_pts,max_pts) ~"Inside",
                         TEST_NAME=="TDPO4"& between(`VALUE`,min_pts,max_pts) ~"Inside",
                         TEST_NAME=="TDSFE"& between(`VALUE`,min_pts,max_pts) ~"Inside", 
                         TEST_NAME=="COLOR"& between(`VALUE`,min_pts,max_pts) ~"Inside", 
                         TEST_NAME=="SIO2" & between(`VALUE`,min_pts,max_pts) ~"Inside",
                         TEST_NAME=="DOC" & between(`VALUE`,min_pts,max_pts) ~"Inside",
                         TEST_NAME=="NH4" & between(`VALUE`,min_pts,max_pts) ~"Inside",
                         TEST_NAME=="TDN" & between(`VALUE`,min_pts,max_pts) ~"Inside",
                         TEST_NAME=="NOX" & between(`VALUE`,min_pts,max_pts )~"Inside",
                         TEST_NAME=="TOC" & between(`VALUE`,min_pts,max_pts) ~"Inside",
                         TEST_NAME=="TN" & between(`VALUE`,min_pts,max_pts )~"Inside",
                         TEST_NAME=="NA" & between(`VALUE`,min_pts,max_pts) ~"Inside",  
                         TEST_NAME=="MG" & between(`VALUE`,min_pts,max_pts) ~"Inside",
                         TEST_NAME=="CA" & between(`VALUE`,min_pts,max_pts) ~"Inside",
                         TEST_NAME=="CL" & between(`VALUE`,min_pts,max_pts) ~"Inside",
                         TEST_NAME=="K" & between(`VALUE`,min_pts,max_pts) ~"Inside", 
                         TEST_NAME=="SO4" & between(`VALUE`,min_pts,max_pts) ~"Inside",
                         TEST_NAME=="NO2" & between(`VALUE`,min_pts,max_pts) ~"Inside",
                         TRUE ~ as.character("Outside"))) %>%
select(STATION_ID, DATE_COLLECTED, TEST_NAME, VALUE, UNITS,min_pts,max_pts, PQL, MDL,Flag,REMARK_CODE)  


# 3a.) Calculate percent of compliance data within PTS range -------------------

#Percent of Marsh data within the range of PTS data
Percent_in_range <- Compliance_Marsh_Data %>%
group_by(TEST_NAME) %>% 
summarise(n(),`Total Samples`=sum(!is.na(VALUE)),`Samples PTS Range`=sum(Flag=="Inside"),
`Percent within range`=`Samples PTS Range`/`Total Samples`,
`Samples less than min`=sum(if_else(VALUE<min_pts,1,0)),`Percent less than min`=`Samples less than min`/`Total Samples`,
`Samples greater than max`=sum(if_else(VALUE>max_pts,1,0)),`Percent greater than max`=`Samples greater than max`/`Total Samples`) %>%
mutate(across(matches('Percent'), percent))



# 1b.) Calculate range of compliance data  --------------------------------

#Range of data in PTS dataset for each parameter
Compliance_Range <- Compliance_Marsh_Data %>%
group_by(TEST_NAME) %>%
summarise(min_comp=min(VALUE,na.rm=T),max_comp=max(VALUE,na.rm=T),median=median(VALUE,na.rm=T),mean=mean(VALUE,na.rm=T),SD=round(sd(VALUE,na.rm=T),digits=3),
IQR_2.5=quantile(VALUE,0.025),IQR_5=quantile(VALUE,0.05),IQR_25=quantile(VALUE,.25),IQR_75=quantile(VALUE,.75) ,IQR_25=quantile(VALUE,.25),IQR_75=quantile(VALUE,.75),IQR_97.5=quantile(VALUE,.975),IQR_99=quantile(VALUE,.99)) %>%
mutate(Range_comp=max_comp-min_comp)

write.csv(Compliance_Range,file="./Data/Compliance_Range.csv",row.names=FALSE)


Compliance_Mode <- Compliance_Marsh_Data %>%
select(TEST_NAME,VALUE)  %>%
group_by(TEST_NAME,VALUE) %>%
count() 

Compliance_Sample_Totals <- Compliance_Marsh_Data %>%
select(TEST_NAME,VALUE)  %>%
group_by(TEST_NAME) %>%
count() 



# 2b.) Calculate the percent of compliance data the PTS covers ------------

Combined_range <- Compliance_Range %>%
left_join(PTS_Range) %>%
mutate(Range=Range_pts/Range_comp)

# 1c.) Calculate the percent of compliance data within 10x of sample bias

Qualified_Data <- left_join(Compliance_Marsh_Data, select(Bias_Data,TEST_NAME,`8-Hour Bias`,`24-Hour Bias` ), by="TEST_NAME") %>%
mutate(Qualified_8_Hour=if_else(VALUE<=10*abs(`8-Hour Bias`),"Qualified",""))  %>%
mutate(Qualified_24_Hour=if_else(VALUE<=10*abs(`24-Hour Bias`),"Qualified",""))  

# 2c.) Create figures to visualize qualified data

#Histograms Distributions of the differences  
ggplot(filter(Qualified_Data,TEST_NAME=="NH4"),aes(VALUE,fill=Qualified_8_Hour))+
geom_histogram(position="identity",alpha=.65)+facet_wrap(~TEST_NAME,scale="free",ncol = 3)+theme_bw()+
gghighlight()  
#scale_fill_manual(values=c("#66c2a5","#fc8d62"),labels=c("24-Hour Preservation", "8-Hour Preservation"))+
geom_vline(xintercept = 0)+theme(legend.position="bottom")+
labs(y="Count",x="mg/l",title="Histogram of differences of 8 and 24-hour from 15-minute preservation by Analyte" )

ggplot(filter(Qualified_Data,TEST_NAME=="NH4"), aes(x = VALUE, y = TEST_NAME)) + geom_density_ridges()+
gghighlight(VALUE>0.1) 

#####Power analysis




#####################################TEST

#Silica interference in measurement of TP
silica_interference <- Compliance_Marsh_Data %>%
select(DATE_COLLECTED,TEST_NAME,VALUE)  %>%
filter(TEST_NAME %in% c("TPO4","SIO2")) %>%
pivot_wider(names_from=TEST_NAME,values_from=VALUE,values_fn =mean) %>%
drop_na()

ggplot(silica_interference,aes(SIO2,TPO4))+geom_point()+theme_bw() +stat_poly_line() +
stat_poly_eq() +scale_x_log10()+scale_y_log10()

#Distribution of data in compliance data
Dist_NH4 <-ggplot(filter(Compliance_Marsh_Data,TEST_NAME=="NH4"),aes(VALUE))+
geom_histogram(position="identity",fill="#f1a340",color="grey20")+theme_bw()+
labs(title="NH4")+theme(axis.title.x=element_blank(),axis.title.y=element_blank())

Dist_Ca <-ggplot(filter(Compliance_Marsh_Data,TEST_NAME=="CA"),aes(VALUE))+
geom_histogram(position="identity",fill="#f1a340",color="grey20")+theme_bw()+
labs(title="Ca")+theme(axis.title.x=element_blank(),axis.title.y=element_blank())

Dist_Cl <-ggplot(filter(Compliance_Marsh_Data,TEST_NAME=="CL"),aes(VALUE))+
geom_histogram(position="identity",fill="#f1a340",color="grey20")+theme_bw()+
labs(title="Cl")+theme(axis.title.x=element_blank(),axis.title.y=element_blank())

Dist_Color <-ggplot(filter(Compliance_Marsh_Data,TEST_NAME=="COLOR"),aes(VALUE))+
geom_histogram(position="identity",fill="#f1a340",color="grey20")+theme_bw()+
labs(title="Color")+theme(axis.title.x=element_blank(),axis.title.y=element_blank())

Dist_TDSFE <-ggplot(filter(Compliance_Marsh_Data,TEST_NAME=="TDSFE"),aes(VALUE))+
geom_histogram(position="identity",fill="#f1a340",color="grey20")+theme_bw()+
labs(title="TDSFE")+theme(axis.title.x=element_blank(),axis.title.y=element_blank())

Dist_Mg <-ggplot(filter(Compliance_Marsh_Data,TEST_NAME=="MG"),aes(VALUE))+
geom_histogram(position="identity",fill="#f1a340",color="grey20")+theme_bw()+
labs(title="Mg")+theme(axis.title.x=element_blank(),axis.title.y=element_blank())

Dist_NOx <-ggplot(filter(Compliance_Marsh_Data,TEST_NAME=="NOX"),aes(VALUE))+
geom_histogram(position="identity",fill="#f1a340",color="grey20",binwidth=0.025)+theme_bw()+
labs(title="NOx")+theme(axis.title.x=element_blank(),axis.title.y=element_blank())

Dist_NO2 <-ggplot(filter(Compliance_Marsh_Data,TEST_NAME=="NO2"),aes(VALUE))+
geom_histogram(position="identity",fill="#f1a340",color="grey20",binwidth=0.002)+theme_bw()+
labs(title="NO2")+theme(axis.title.x=element_blank(),axis.title.y=element_blank())

Dist_DOC <-ggplot(filter(Compliance_Marsh_Data,TEST_NAME=="DOC"),aes(VALUE))+
geom_histogram(position="identity",fill="#f1a340",color="grey20")+theme_bw()+
labs(title="DOC")+theme(axis.title.x=element_blank(),axis.title.y=element_blank())

Dist_K <-ggplot(filter(Compliance_Marsh_Data,TEST_NAME=="K"),aes(VALUE))+
geom_histogram(position="identity",fill="#f1a340",color="grey20")+theme_bw()+
labs(title="K")+theme(axis.title.x=element_blank(),axis.title.y=element_blank())

Dist_OPO4 <-ggplot(filter(Compliance_Marsh_Data,TEST_NAME=="OPO4"),aes(VALUE))+
geom_histogram(position="identity",fill="#f1a340",color="grey20",binwidth=0.025)+theme_bw()+
labs(title="OPO4")+theme(axis.title.x=element_blank(),axis.title.y=element_blank())

Dist_NA <-ggplot(filter(Compliance_Marsh_Data,TEST_NAME=="NA"),aes(VALUE))+
geom_histogram(position="identity",fill="#f1a340",color="grey20")+theme_bw()+
labs(title="Na")+theme(axis.title.x=element_blank(),axis.title.y=element_blank())

Dist_SIO2 <-ggplot(filter(Compliance_Marsh_Data,TEST_NAME=="SIO2"),aes(VALUE))+
geom_histogram(position="identity",fill="#f1a340",color="grey20")+theme_bw()+
labs(title="SIO2")+theme(axis.title.x=element_blank(),axis.title.y=element_blank())

Dist_SO4 <-ggplot(filter(Compliance_Marsh_Data,TEST_NAME=="SO4"),aes(VALUE))+
geom_histogram(position="identity",fill="#f1a340",color="grey20")+theme_bw()+
labs(title="SO4")+theme(axis.title.x=element_blank(),axis.title.y=element_blank())

Dist_TN <-ggplot(filter(Compliance_Marsh_Data,TEST_NAME=="TN"),aes(VALUE))+
geom_histogram(position="identity",fill="#f1a340",color="grey20")+theme_bw()+
labs(title="TN")+theme(axis.title.x=element_blank(),axis.title.y=element_blank())

Dist_TOC <-ggplot(filter(Compliance_Marsh_Data,TEST_NAME=="TOC"),aes(VALUE))+
geom_histogram(position="identity",fill="#f1a340",color="grey20")+theme_bw()+
labs(title="TOC")+theme(axis.title.x=element_blank(),axis.title.y=element_blank())

Dist_TDN <-ggplot(filter(Compliance_Marsh_Data,TEST_NAME=="TDN"),aes(VALUE))+
geom_histogram(position="identity",fill="#f1a340",color="grey20")+theme_bw()+
labs(title="TDN")+theme(axis.title.x=element_blank(),axis.title.y=element_blank())

Dist_TDP <-ggplot(filter(Compliance_Marsh_Data,TEST_NAME=="TDPO4"),aes(VALUE))+
geom_histogram(position="identity",fill="#f1a340",color="grey20",binwidth=0.005)+theme_bw()+
labs(title="TDP")+theme(axis.title.x=element_blank(),axis.title.y=element_blank())

Dist_TP <-ggplot(filter(Compliance_Marsh_Data,TEST_NAME=="TPO4"),aes(VALUE))+#facet_wrap(~TEST_NAME)+
geom_histogram(position="identity",fill="#f1a340",color="grey20",binwidth=0.025)+theme_bw()+
labs(title="TPO4")+theme(axis.title.x=element_blank(),axis.title.y=element_blank())



plot_grid(Dist_NH4,Dist_Ca,Dist_Cl,Dist_Color,Dist_TDSFE,Dist_Mg,Dist_NOx,Dist_NO2,Dist_DOC,Dist_TOC,Dist_OPO4,Dist_K,Dist_SIO2,Dist_NA,Dist_SO4,Dist_TN,Dist_TDN ,Dist_TDP,Dist_TP, 
labels = c('a', 'b','c', 'd','e', 'f','g', 'h','i', 'j','k', 'l','m', 'n','o', 'p','q', 'r','s'),label_size = 11,ncol=4, align = 'v',label_x = .90)+
draw_label(expression(mg/L), x = 0.5, y = 0.012, hjust = .5, angle = 0, size = 14, ) +
draw_label("Count", x = 0.015, y = 0.6, vjust = .5, angle = 90, size = 14)  

ggsave("./Figures/Analyte Distributions in environment.jpeg",plot=last_plot(),height=11,width=8.5,units="in")

