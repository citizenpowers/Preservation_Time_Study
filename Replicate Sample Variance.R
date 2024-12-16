.libPaths( c( .libPaths(), "C:/Users/mpowers/OneDrive - South Florida Water Management District 1/WQTT/R/Packages") )  #path to packages that could not be installed at admin protected location
remove(list=ls()) #removes all objects from project

library(EnvStats) 
library(readr)
library(Metrics)
library(moments)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(viridis)
library(scales)
library(stringr)
library(utils)
library(ggpmisc)
library(lubridate)
library(gghighlight)
library(ggrepel)
# library(quantileCI)


# Import Data -------------------------------------------------------------

Sample_Results_Tidy <- read_csv("Data/Sample_Results_Tidy.csv")

EVPA_Data <- read_excel("./Data/Replicate Sample Data.xlsx", sheet = "EVPA")
RTBG_Data <- read_excel("./Data/Replicate Sample Data.xlsx", sheet = "RTBG")
OLIT_Data <- read_excel("./Data/Replicate Sample Data.xlsx", sheet = "OLIT")
WCA1T_Data <- read_excel("./Data/Replicate Sample Data.xlsx", sheet = "WCA1T")
WCA2A_Data <- read_excel("./Data/Replicate Sample Data.xlsx", sheet = "WCA-2A")

analytes_DBHYDRO <- c("KJELDAHL NITROGEN, TOTAL","PHOSPHATE, TOTAL AS P","CARBON, TOTAL ORGANIC","COLOR","PHOSPHATE, ORTHO AS P",
              "CHLORIDE","SULFATE","NITRITE-N","NITRATE+NITRITE-N","SILICA",
              "PHOSPHATE, DISSOLVED AS P","CARBON, DISSOLVED ORGANIC","KJELDAHL NITROGEN, DIS","AMMONIA-N","SODIUM",
              "MAGNESIUM","CALCIUM","POTASSIUM","IRON, TOTAL") #NO3 and hardness are calculated so excluded. TDSAL excluded due to lab method

#Analytes in project PTS  
analytes <- c("TN","TPO4","TOC","COLOR","OPO4","CL","SO4","NO2","NOX","SIO2","TDPO4","DOC","TDN","NH4","NA","MG","CA","K","TDSAL","TDSFE") #NO3 and harness are calculated so excluded

Critical_values_Table_Paired_Data  <- read_excel("./Data/Critical values Table Paired Data.xlsx")


# Tidy Data ---------------------------------------------------------------

Replicates <- EVPA_Data  %>%
bind_rows(RTBG_Data)   %>%
bind_rows(OLIT_Data)   %>%
bind_rows(WCA1T_Data)  %>%
bind_rows(WCA2A_Data)  %>%
mutate(Date=as.Date(DATE_COLLECTED))  %>%
mutate(`Station and Time`= paste(STATION_ID,Date))  %>%
filter(SAMPLE_TYPE_NEW=="RS") %>%
select(`Station and Time`)

All_Data <- EVPA_Data  %>%
bind_rows(RTBG_Data)   %>%
bind_rows(OLIT_Data)   %>%
bind_rows(WCA1T_Data)  %>%
bind_rows(WCA2A_Data)  %>%
filter(MATRIX=="SW" ) %>%
filter(SAMPLE_TYPE_NEW=="SAMP" | SAMPLE_TYPE_NEW=="RS") %>%
filter(SAMPLE_TYPE_NEW!="FCEB") %>% 
filter(SAMPLE_TYPE_NEW!="EB") %>% 
filter(COLLECT_METHOD=="G") %>% 
filter(is.na(FLAG))%>%
filter(TEST_NAME  %in% analytes_DBHYDRO) %>%
mutate(VALUE=ifelse(VALUE>MDL & VALUE<=PQL,PQL,VALUE)) %>%                   #Transforms values below the PQL to the PQL  #mutate(VALUE=ifelse(VALUE<MDL,MDL,VALUE)) %>%                                #Transforms values below the MDL to the MDL
mutate(VALUE=if_else(TEST_NAME == "IRON, TOTAL",VALUE/1000,VALUE)) %>%       #Converts Iron to mg/l from ug/l
mutate(Date=as.Date(DATE_COLLECTED))  %>%
mutate(`Station and Time`= paste(STATION_ID,Date))  %>%
semi_join(Replicates, by="Station and Time") %>%
mutate(VALUE=abs(VALUE)) %>%  #Compliance samples reported as negative values when below detection limits
mutate(Flag= case_when(TEST_NAME=="PHOSPHATE, ORTHO AS P" & between(`VALUE`,.002,.126) ~"Inside",    
                         TEST_NAME=="PHOSPHATE, TOTAL AS P" & between(`VALUE`,.006,.217) ~"Inside",
                         TEST_NAME=="PHOSPHATE, DISSOLVED AS P"& between(`VALUE`,.004,.145) ~"Inside",
                         TEST_NAME=="IRON, TOTAL"& between(`VALUE`,.003,.012) ~"Inside", 
                         TEST_NAME=="COLOR"& between(`VALUE`,56,93) ~"Inside", 
                         TEST_NAME=="SILICA" & between(`VALUE`,8.6,16.2) ~"Inside",
                         TEST_NAME=="CARBON, DISSOLVED ORGANIC" & between(`VALUE`,21.8,38.2) ~"Inside",
                         TEST_NAME=="AMMONIA-N" & between(`VALUE`,.012,1.084) ~"Inside",
                         TEST_NAME=="KJELDAHL NITROGEN, DIS" & between(`VALUE`,1.31,2.53) ~"Inside",
                         TEST_NAME=="NITRATE+NITRITE-N" & between(`VALUE`,.005,.284 )~"Inside",
                         TEST_NAME=="CARBON, TOTAL ORGANIC" & between(`VALUE`,21.9,38.7) ~"Inside",
                         TEST_NAME=="KJELDAHL NITROGEN, TOTAL" & between(`VALUE`,1.38,3.27 )~"Inside",
                         TEST_NAME=="SODIUM" & between(`VALUE`,36.8,113.6) ~"Inside",  
                         TEST_NAME=="MAGNESIUM" & between(`VALUE`,11,34.4) ~"Inside",
                         TEST_NAME=="CALCIUM" & between(`VALUE`,28.7,82.8) ~"Inside",
                         TEST_NAME=="CHLORIDE" & between(`VALUE`,59.4,169) ~"Inside",
                         TEST_NAME=="POTASSIUM" & between(`VALUE`,5.5,10.7) ~"Inside", 
                         TEST_NAME=="SULFATE" & between(`VALUE`,18.5,66.6) ~"Inside",
                         TEST_NAME=="NITRITE-N" & between(`VALUE`,.002,.008) ~"Inside",
                         TRUE ~ as.character("Outside"))) 


#Range of data in PTS dataset for each parameter
PTS_Range <- Sample_Results_Tidy %>%
group_by(TEST_NAME) %>%
summarise(min=min(VALUE,na.rm=T),max=max(VALUE,na.rm=T))


Samples <- All_Data %>%
select(PROJECT_CODE,STATION_ID,Date,TEST_NAME,SAMPLE_TYPE_NEW,VALUE) %>%
filter(SAMPLE_TYPE_NEW=="SAMP")

reps <- All_Data %>%
mutate(Time=format(strptime(DATE_COLLECTED, "%Y-%m-%d %H:%M:%S"), "%H%M")) %>%
select(PROJECT_CODE,STATION_ID,Date,TEST_NAME,SAMPLE_TYPE_NEW,VALUE,DATE_COLLECTED,Time) %>%
filter(SAMPLE_TYPE_NEW=="RS") %>%
group_by(PROJECT_CODE,STATION_ID,Date,TEST_NAME) %>%
mutate(`Rep Number`=dense_rank(Time))

Rep1 <- reps %>%
filter(`Rep Number`==1)

Rep2 <- reps %>%
filter(`Rep Number`==2)

All_Data_wide <- select(Rep1,PROJECT_CODE,STATION_ID,Date,TEST_NAME,VALUE)  %>%
left_join(select(Rep2,PROJECT_CODE,STATION_ID,Date,TEST_NAME,VALUE), by =c("PROJECT_CODE","STATION_ID","Date","TEST_NAME")) %>%
left_join(select(Samples,PROJECT_CODE,STATION_ID,Date,TEST_NAME,VALUE), by =c("PROJECT_CODE","STATION_ID","Date","TEST_NAME")) %>%
rename(`Replicate 1`=VALUE,`Replicate 2`=VALUE.x,`Replicate 3`=VALUE.y) %>%
#mutate(mean = (`Replicate 1`+`Replicate 2`+`Replicate 3`)/(is.finite(`Replicate 1`)+is.finite(`Replicate 2`)+is.finite(`Replicate 3`))) %>%  #using all 3 reps
mutate(mean = (`Replicate 1`+`Replicate 2`)/(is.finite(`Replicate 1`)+is.finite(`Replicate 2`))) %>%             #using only 2 replicates
mutate(`Diff rep 1 - rep 2`=ifelse(is.finite(`Replicate 1`) && is.finite(`Replicate 2`),`Replicate 1`-`Replicate 2`,NA)) 
#mutate(`Diff rep 1 - rep 3`=ifelse(is.finite(`Replicate 1`) && is.finite(`Replicate 3`),`Replicate 1`-`Replicate 3`,NA)) %>%
#mutate(`Diff rep 2 - rep 3`=ifelse(is.finite(`Replicate 2`) && is.finite(`Replicate 3`),`Replicate 2`-`Replicate 3`,NA)) 
#mutate(`Diff rep 2`=ifelse(is.finite(`Replicate 2`),`Replicate 2`-mean,NA)) 
#mutate(`Diff rep 3`=ifelse(is.finite(`Replicate 3`),`Replicate 3`-mean,NA)) 


All_Data_long <- All_Data_wide %>%
gather("Differences","Value", 9) %>%                                               #change gathered columns depending on using 2 or 3 reps
ungroup() %>% 
mutate(TEST_NAME= case_when(TEST_NAME=="PHOSPHATE, ORTHO AS P" ~"OPO4",    
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
                              TRUE ~ as.character(TEST_NAME)))



# Compare range of PTS data to Compliance Data ----------------------------

Analyte_Range <- EVPA_Data  %>%
  bind_rows(RTBG_Data)   %>%
  bind_rows(OLIT_Data)   %>%
  bind_rows(WCA1T_Data)  %>%
  bind_rows(WCA2A_Data)  %>%
  filter(MATRIX=="SW" ) %>%
  filter(SAMPLE_TYPE_NEW!="FCEB") %>% 
  filter(SAMPLE_TYPE_NEW!="EB") %>% 
  filter(COLLECT_METHOD=="G") %>% 
  filter(is.na(FLAG))%>%
  mutate(TEST_NAME= case_when(TEST_NAME=="PHOSPHATE, ORTHO AS P" ~"OPO4",    
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
  filter(TEST_NAME  %in% analytes) %>%
  dplyr::select(TEST_NAME,VALUE,UNITS) %>%
  mutate(SOURCE="Compliance") %>%
  bind_rows(Sample_Results %>% filter(is.na(FQC)) %>%  select(TEST_NAME,VALUE,UNITS)  %>% mutate(SOURCE="PTS") %>% filter(TEST_NAME %in% analytes)  )

analytes <- c("TN","TPO4","TOC","COLOR","OPO4","CL","SO4","NO2","NOX","SIO2","TDPO4","DOC","TDN","NH4","NA","MG","CA","K","TDSFE") #NO3 and hardness are calculated so excluded. TDSAL excluded due to lab method


ggplot(Analyte_Range,aes(VALUE,fill=SOURCE))+geom_histogram(color="grey20")+facet_wrap(~TEST_NAME,scales="free")+theme_bw()
ggplot(Analyte_Range,aes(VALUE,fill=SOURCE))+geom_area(stat = "bin")+facet_wrap(~TEST_NAME,scales="free")+theme_bw()
ggplot(Analyte_Range,aes(VALUE,fill=SOURCE))+geom_boxplot()+facet_wrap(~TEST_NAME,scales="free")+theme_bw()


ggplot(filter(Analyte_Range, TEST_NAME=="TPO4"),aes(VALUE,fill=SOURCE))+geom_boxplot()+facet_wrap(~TEST_NAME,scales="free")+theme_bw()+coord_cartesian(xlim=c(0,0.1))
ggplot(filter(Analyte_Range, TEST_NAME=="TPO4"),aes(VALUE,fill=SOURCE))+geom_histogram(color="grey20",binwidth=.01)+facet_wrap(~TEST_NAME,scales="free")+theme_bw()+coord_cartesian(xlim=c(0,0.1))

analyte_range_table <- Analyte_Range %>%
group_by(TEST_NAME,SOURCE,UNITS) %>%
summarise(n(),min=min(abs(VALUE),na.rm=T),mean=mean(VALUE,na.rm=T),max=max(VALUE,na.rm=T),Q0.05=quantile(VALUE,.05),Q0.25=quantile(VALUE,.25),Q0.5=quantile(VALUE,.5),Q0.75=quantile(VALUE,.75),Q0.95=quantile(VALUE,.95),
range=max-min)  


# Replicate Variance vs Preservation time variance ------------------------

Replicates_vs_PTS <-All_results_Uncertainty %>%
select(TEST_NAME,`Difference (15-Min - 8-Hour)`,`Difference (15-Min - 24-Hour)`) %>%
gather("Differences","Value", 2:3) %>%
mutate(PROJECT_CODE= case_when(Differences=="Difference (15-Min - 8-Hour)"~ "8-Hour Samples",Differences=="Difference (15-Min - 24-Hour)"~"24-Hour Samples")) %>%
mutate(Group= case_when(Differences=="Difference (15-Min - 8-Hour)"~ "8-Hour Samples",Differences=="Difference (15-Min - 24-Hour)"~"24-Hour Samples")) %>%
bind_rows(mutate(select(ungroup(All_Data_long),PROJECT_CODE,TEST_NAME,Differences,Value),Group="Replicates")) 


# Approximate Normality ---------------------------------------------------
#normality figures QQ plot
ggplot(Replicates_vs_PTS,aes(sample=Value))+geom_qq()+facet_grid(Group~TEST_NAME)

ggplot(Replicates_vs_PTS,aes(Value))+geom_histogram()+facet_grid(Group~TEST_NAME,scales="free")

ggplot(filter(Replicates_vs_PTS,TEST_NAME=="CA"),aes(Value,fill=Group))+geom_density(alpha=.5) +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="CL"),aes(Value,fill=Group))+geom_density(alpha=.5) +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="COLOR"),aes(Value,fill=Group))+geom_density(alpha=.5) +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="DOC"),aes(Value,fill=Group))+geom_density(alpha=.5) +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="K"),aes(Value,fill=Group))+geom_density(alpha=.5) +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="MG"),aes(Value,fill=Group))+geom_density(alpha=.5) +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="NA"),aes(Value,fill=Group))+geom_density(alpha=.5) +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="NH4"),aes(Value,fill=Group))+geom_density(alpha=.5) +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="NO2"),aes(Value,fill=Group))+geom_density(alpha=.5) +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="NOX"),aes(Value,fill=Group))+geom_density(alpha=.5) +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="OPO4"),aes(Value,fill=Group))+geom_density(alpha=.5) +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="SIO2"),aes(Value,fill=Group))+geom_density(alpha=.5) +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="SO4"),aes(Value,fill=Group))+geom_density(alpha=.5) +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="TDN"),aes(Value,fill=Group))+geom_density(alpha=.5) +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="TDPO4"),aes(Value,fill=Group))+geom_density(alpha=.5) +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="TDSFE"),aes(Value,fill=Group))+geom_density(alpha=.5) +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="TN"),aes(Value,fill=Group))+geom_density(alpha=.5) +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="TOC"),aes(Value,fill=Group))+geom_density(alpha=.5) +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="TPO4"),aes(Value,fill=Group))+geom_density(alpha=.5) +theme_bw()+scale_color_brewer(type = "qual")

ggplot(filter(Replicates_vs_PTS,TEST_NAME=="CA"),aes(Value,fill=Group))+geom_histogram(alpha=.5,position="identity") +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="CL"),aes(Value,fill=Group))+geom_histogram(alpha=.5,position="identity") +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="COLOR"),aes(Value,fill=Group))+geom_histogram(alpha=.5,position="identity") +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="DOC"),aes(Value,fill=Group))+geom_histogram(alpha=.5,position="identity") +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="K"),aes(Value,fill=Group))+geom_histogram(alpha=.5,position="identity") +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="MG"),aes(Value,fill=Group))+geom_histogram(alpha=.5,position="identity") +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="NA"),aes(Value,fill=Group))+geom_histogram(alpha=.5,position="identity") +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="NH4"),aes(Value,fill=Group))+geom_histogram(alpha=.5,position="identity") +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="NO2"),aes(Value,fill=Group))+geom_histogram(alpha=.5,position="identity") +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="NOX"),aes(Value,fill=Group))+geom_histogram(alpha=.5,position="identity") +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="OPO4"),aes(Value,fill=Group))+geom_histogram(alpha=.5,position="identity") +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="SIO2"),aes(Value,fill=Group))+geom_histogram(alpha=.5,position="identity") +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="SO4"),aes(Value,fill=Group))+geom_histogram(alpha=.5,position="identity") +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="TDN"),aes(Value,fill=Group))+geom_histogram(alpha=.5,position="identity") +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="TDPO4"),aes(Value,fill=Group))+geom_histogram(alpha=.5,position="identity",binwidth = .00075) +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="TDSFE"),aes(Value,fill=Group))+geom_histogram(alpha=.5,position="identity") +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="TN"),aes(Value,fill=Group))+geom_histogram(alpha=.5,position="identity") +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="TOC"),aes(Value,fill=Group))+geom_histogram(alpha=.5,position="identity") +theme_bw()+scale_color_brewer(type = "qual")
ggplot(filter(Replicates_vs_PTS,TEST_NAME=="TPO4"),aes(Value,fill=Group))+geom_histogram(alpha=.5,position="identity") +theme_bw()+scale_color_brewer(type = "qual")

Summary_Stats_group <- Replicates_vs_PTS %>%
  group_by(TEST_NAME,Group) %>%
  summarise(n=n(),
            `Difference is zero`=sum(na.omit(Value) == 0),
            `Percent zero difference`=sum(na.omit(Value) == 0)/n(),
            `Normal`=shapiro.test(`Value`)$p.value,
            `Sample Variance (s^2)`=var(Value,na.rm = TRUE), 
            `sqrt(Sample Variance )`=sqrt(var(Value,na.rm = TRUE)), 
            sd=sd(Value,na.rm = TRUE),     
            #`sd calculated`=sqrt(var(Value,na.rm = TRUE)),
            SE=sd(Value,na.rm = TRUE)/sqrt(n()),
            #SE2=sqrt(sd(Value,na.rm = TRUE)^2/n()),
            `t score`=qt(.975,df=(n()-1)),
            Mean=mean(Value,na.rm = TRUE),
            `Mean Deviation`=mean(abs(Value),na.rm = TRUE),
            `Z Dist 2.5%`=mean(Value,na.rm = TRUE)-2*sd(Value,na.rm = TRUE),
            `2.5%`=quantile(Value,0.025,na.rm = TRUE),
            `LOA Lower`=Mean-2*sd(Value,na.rm = TRUE),                              #Calculate lower limit of agreement
            `Lower Median CI`=t.test(Value,na.rm=TRUE)$conf.int[1],
            `Lower mean CI`=Mean-`t score`*SE,
            `Upper mean CI`=Mean+`t score`*SE,
            `Wilcox Lower Median CI`=wilcox.test(Value,alternative="two.sided",correct=TRUE,conf.int=TRUE,conf.level=0.95)$conf.int[1],
            median=median(Value,na.rm = TRUE),
            median2=quantile(Value,0.5,na.rm = TRUE),
            `Upper Median CI`=t.test(Value,na.rm=TRUE)$conf.int[2],
            `  Upper Mean`=Mean+1.99*SE,
            `Wilcox Upper Median CI`=wilcox.test(Value,alternative="two.sided",correct=TRUE,conf.int=TRUE,conf.level=0.95)$conf.int[2],
            `97.5%`=quantile(Value,0.975,na.rm = TRUE),
            `LOA Upper`=Mean+2*sd(Value,na.rm = TRUE),                              #Calculate Upper limit of agreement
            `Z Dist 97.5%`=Mean+2*sd(Value,na.rm = TRUE),
            `Critical Value 0.025`=  Critical_values_Table_Paired_Data[[n(),2]],
            `Critical Value 0.975`=  Critical_values_Table_Paired_Data[[n(),6]],
            `Upper CI 95%  Carkeet`= Critical_values_Table_Paired_Data[[n(),6]]*sd(Value,na.rm = TRUE),      #carkeet 
            `Upper CI 90%  Carkeet`=2.4844*sd(Value,na.rm = TRUE),      #careet CI  `
            `Upper SD CI formula`= sqrt((var(Value) *(n()-1) ) / qchisq(0.975, (n()-1), lower.tail = FALSE)),
            `Upper SD CI CHI`=sqrt(varTest(Value, sigma.squared =`Sample Variance (s^2)`,alternative = "two.sided",conf.level = 0.95)$conf.int[2]), 
            `Upper SD CI BA`=1.99*SE,
            `Upper CI  pop var`=(`Sample Variance (s^2)`*(n()-1))/qchisq(.975, n()-1),
            `Upper CI - EnvStat method`= as.numeric(eqnpar(x=Value, p=.95, ci=TRUE, ci.method="normal.approx",approx.conf.level=0.90)$interval$limits[2]),
            #`Upper CI Boot Top `=quantile_confint_boot(na.omit(Value), p=.975, conf.level=0.90,R=999, type=1)[2],      #boot strap method for CI
            #  `Upper CI Boot bottom`=quantile_confint_boot(na.omit(Value), p=.975, conf.level=0.90,R=999, type=1)[1],    #boot strap method for CI
            `diff+1.96s`=sqrt(3*sd(Value,na.rm = TRUE)^2/n()),
            `diff-1.96s`=-sqrt(3*sd(Value,na.rm = TRUE)^2/n()),
            `Lower 95% CI Carkeet`= Critical_values_Table_Paired_Data[[n(),2]]*sd(Value,na.rm = TRUE),   # from Carkeet table two 
            `Lower 90% CI Carkeet`=1.66615*sd(Value,na.rm = TRUE),
            `lower SD CI BA`=-1.99*SE,
            `lower CI  pop var`=(`Sample Variance (s^2)`*(n()-1))/qchisq(.025, n()-1),
            `Lower SD CI CHI`=sqrt(varTest(Value, sigma.squared = 0.5,alternative = "two.sided",conf.level = 0.95)$conf.int[1]),
            # `Lower CI Boot bottom`=quantile_confint_boot(na.omit(Value), p=.025, conf.level=0.90,R=999, type=1)[1],  #boot strap method for CI
            #  `Lower CI Boot top`=quantile_confint_boot(na.omit(Value), p=.025, conf.level=0.90,R=999, type=1)[2],     #boot strap method for CI
            `Lower formula`= sqrt((var(Value) *(n()-1 )) / qchisq(0.025, (n()-1), lower.tail = FALSE)))  %>%                           #Calculate the lower limit of the CI for SD
  mutate_if(is.numeric, round,4) %>%
  #mutate_if(is.numeric, format,nsmall=3,scientific=FALSE) %>%
  mutate(Scale= case_when(TEST_NAME %in% c("OPO4","TDPO4","NO2","TDSFE")~ "very small", TEST_NAME %in% c("NH4","NOX","TPO4")~ "small",TEST_NAME %in% c("K","TN","TDN")~ "medium small",
                          TEST_NAME %in% c("TOC","MG","SIO2")~ "medium",TEST_NAME %in% c("DOC","SO4","NA")~ "medium large",TEST_NAME %in% c("CL","CA","COLOR")~ "large")) 

write.csv(Summary_Stats_group,"Summary Stats.csv")


#MG BA Plot Figure 2 in manuscript
BA_MG <-   filter(All_results_Uncertainty,TEST_NAME=="MG")

ggplot(BA_MG ,aes((Value_15_min+Value_8_hour)/2,`Difference (15-Min - 8-Hour)`))+
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = .545, ymax =.866), alpha = 0.3,fill="#fff7bc")+
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -.517, ymax = -.938), alpha = 0.3,fill="#fff7bc")+  
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -.0976, ymax = 0.125), alpha = 0.3,fill="#fed98e")+
  geom_hline(yintercept= 0.014, colour ="black" ,size = 1,linetype="dashed")+
  geom_hline(yintercept= 0.673, colour = "#f1a340" ,linetype ="longdash",size = 1)+
  geom_hline(yintercept= -0.645, colour = "#f1a340" ,linetype ="longdash",size = 1)+
  geom_point(shape=21,size=3 ,color="black",fill="#5ab4ac")+
  #geom_errorbar(aes(x=TEST_NAME,ymin =.545, ymax =.866 ),color="#636363")+ 
  annotate(x=18,y=-0.025,label="Bias with 95% Confidence Interval",geom = "text" ,color="#636363")+
  annotate(x=15,y=.72,label="Upper LOA with 95% Confidence Interval",geom = "text" ,color="#636363")+
  annotate(x=15,y=-.6,label="Lower LOA with 95% Confidence Interval",geom = "text",color="#636363" )+theme(legend.position="bottom")+  
  labs(title = "Absolute Difference Bland Altman Plot:\nDifferences in Dissolved Magnesium in Samples Preserved within 15 Minutes and 8 Hours", x=expression("Paired Sample Mean "~(mg~L^{-1})), y=expression("Paired Sample Differences "~(mg~L^{-1})))+theme_bw()

ggsave("Dissolved mg BA plot example.jpeg",plot=last_plot(),height=5,width=8,units="in")

Summary_Stats_group$TEST_NAME = factor(Summary_Stats_group$TEST_NAME, levels=c("CL","COLOR","CA","NA","SO4","DOC","TOC","MG","SIO2","K","TN","TDN","TDSFE","NH4","NOX","TPO4","OPO4","TDPO4","NO2") )
Summary_Stats_group$Group = factor(Summary_Stats_group$Group, levels=c("Replicates","8-Hour Samples","24-Hour Samples") )

#Small
ggplot(filter(Summary_Stats_group,Scale=="small"))+
  geom_crossbar(aes(x=Group,y=Mean, ymin = `LOA Lower`, ymax = `LOA Upper`,colour=Group),position = position_dodge(width  =.95),size=1)+
  geom_hline(yintercept = 0, colour = "black" ,linetype ="longdash",size = .75)+
  geom_errorbar(aes(x=Group, ymin =`Lower mean CI`, ymax =`Upper mean CI` ),color="#636363")+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  geom_errorbar(aes(x=Group, ymin = Mean+`Upper CI 95%  Carkeet`, ymax =  Mean+`Lower 95% CI Carkeet` ,fill=Group),color="#636363")+         #Possible error of lower LOA Exact CI -Careet
  geom_errorbar(aes(x=Group, ymin = Mean-`Lower 95% CI Carkeet`, ymax =  Mean-`Upper CI 95%  Carkeet` ,fill=Group),color="#636363")+         #Possible error of upper LOA  Exact CI -Careet
  facet_grid(col=vars(TEST_NAME),scales="free")+scale_color_brewer(type = "qual")+
  theme_bw()+theme(legend.position="bottom",axis.text.x=element_blank())+labs(title = "", x="", y="mg/l")+scale_y_continuous(breaks = scales::pretty_breaks(n = 14))

ggsave("Precision by Analyte and Project -Small 2.jpeg",plot=last_plot(),height=5,width=8,units="in")

#Different Confidence Intervals
ggplot(filter(Summary_Stats_group,Scale=="small"))+
  geom_crossbar(aes(x=Group,y=Mean, ymin = `LOA Lower`, ymax = `LOA Upper`,colour=Group),position = position_dodge(width  =.95),size=1)+
  geom_hline(yintercept = 0, colour = "black" ,linetype ="longdash",size = .75)+
  geom_errorbar(aes(x=Group, ymin =`Lower mean CI`, ymax =`Upper mean CI` ,fill=Group),color="#636363")+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  #geom_errorbar(aes(x=Group, ymin =`LOA Lower`-`Upper CI 95%  Carkeet`, ymax = `LOA Lower`+`Lower 95% CI Carkeet` ,fill=Group),color="#636363")+         #Possible error of lower LOA Exact CI -Careet
  #geom_errorbar(aes(x=Group, ymin =`LOA Upper`-`Lower 95% CI Carkeet`, ymax = `LOA Upper`+`Upper CI 95%  Carkeet` ,fill=Group),color="#636363")+         #Possible error of upper LOA  Exact CI -Careet
  #geom_errorbar(aes(x=Group, ymin =`LOA Upper`-`Lower SD CI CHI`, ymax = `LOA Upper`+`Upper SD CI CHI` ,fill=Group),color="#636363")+         #Possible error of upper LOA estimate using chi distribution approach
  #geom_errorbar(aes(x=Group, ymin =`LOA Lower`-`Upper SD CI CHI`, ymax = `LOA Lower`+`Lower SD CI CHI` ,fill=Group),color="#636363")+       #Possible error of lower LOA estimate using chi distribution approach
  geom_errorbar(aes(x=Group, ymin =`LOA Lower`-`diff+1.96s`, ymax = `LOA Lower`+`diff+1.96s` ,fill=Group),color="red")+         #Possible error of lowerLOA estimate using BA approach
  geom_errorbar(aes(x=Group, ymin =`LOA Upper`-`diff+1.96s`, ymax = `LOA Upper`+`diff+1.96s` ,fill=Group),color="red")+          #Possible error of upper LOA estimate using BA approach
  geom_errorbar(aes(x=Group, ymin =`Mean`+`Upper CI 95%  Carkeet`, ymax = Mean+`Lower 95% CI Carkeet` ,fill=Group),color="blue")+         #Possible error of lower LOA Exact CI -Careet
  geom_errorbar(aes(x=Group, ymin =Mean-`Lower 95% CI Carkeet`, ymax = Mean-`Upper CI 95%  Carkeet` ,fill=Group),color="blue")+         #Possible error of upper LOA  Exact CI -Careet
  #geom_errorbar(aes(x=Group, ymin =`LOA Upper`-`Lower 90% CI Carkeet`, ymax = `LOA Upper`+`Upper CI 90%  Carkeet` ,fill=Group),color="green")  +       #Possible error of upper LOA  Exact CI -Careet
  #geom_errorbar(aes(x=Group, ymin =`LOA Lower`+`Lower CI Boot bottom`, ymax = `LOA Lower`-`Lower CI Boot top` ,fill=Group),color="purple")+         #Boot CI
  #geom_errorbar(aes(x=Group, ymin =`LOA Upper`-`Upper CI Boot bottom`, ymax = `LOA Upper`+`Upper CI Boot Top ` ,fill=Group),color="purple")  +       #Boot CI
  facet_grid(col=vars(TEST_NAME),scales="free")+scale_color_brewer(type = "qual")+
  theme_bw()+theme(legend.position="bottom",axis.text.x=element_blank())+labs(title = "", x="", y="mg/l")+scale_y_continuous(breaks = scales::pretty_breaks(n = 14))

ggsave("Precision by Analyte and Project -Small 2.jpeg",plot=last_plot(),height=5,width=8,units="in")


#very small
ggplot(filter(Summary_Stats_group,Scale=="very small"))+
  geom_crossbar(aes(x=Group,y=Mean, ymin = `LOA Lower`, ymax = `LOA Upper`,colour=Group),position = position_dodge(width  =.95),size=1)+
  geom_hline(yintercept = 0, colour = "black" ,linetype ="longdash",size = .75)+
  geom_errorbar(aes(x=Group, ymin =`Lower mean CI`, ymax =`Upper mean CI` ,fill=Group),color="#636363")+ #ERrror bar for bias
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  facet_grid(col=vars(TEST_NAME),scales="free")+scale_color_brewer(type = "qual")+
  theme_bw()+theme(legend.position="bottom",axis.text.x=element_blank())+labs(title = "", x="", y="mg/l")+scale_y_continuous(breaks = scales::pretty_breaks(n = 14)) +
  geom_errorbar(aes(x=Group, ymin = Mean+`Upper CI 95%  Carkeet`, ymax =  Mean+`Lower 95% CI Carkeet` ,fill=Group),color="#636363")+         #Possible error of lower LOA Exact CI -Careet
  geom_errorbar(aes(x=Group, ymin = Mean-`Lower 95% CI Carkeet`, ymax =  Mean-`Upper CI 95%  Carkeet` ,fill=Group),color="#636363")+         #Possible error of upper LOA  Exact CI -Careet
  
  ggsave("Precision by Analyte and Project - Very Small 2.jpeg",plot=last_plot(),height=5,width=8,units="in")


#medium small
ggplot(filter(Summary_Stats_group,Scale=="medium small"))+
  geom_crossbar(aes(x=Group,y=Mean, ymin = `LOA Lower`, ymax = `LOA Upper`,colour=Group),position = position_dodge(width  =.95),size=1)+
  geom_hline(yintercept = 0, colour = "black" ,linetype ="longdash",size = .75)+
  geom_errorbar(aes(x=Group, ymin =`Lower mean CI`, ymax =`Upper mean CI` ,fill=Group),color="#636363")+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  geom_errorbar(aes(x=Group, ymin = Mean+`Upper CI 95%  Carkeet`, ymax =  Mean+`Lower 95% CI Carkeet` ,fill=Group),color="#636363")+         #Possible error of lower LOA Exact CI -Careet
  geom_errorbar(aes(x=Group, ymin = Mean-`Lower 95% CI Carkeet`, ymax =  Mean-`Upper CI 95%  Carkeet` ,fill=Group),color="#636363")+         #Possible error of upper LOA  Exact CI -Careet
  facet_grid(col=vars(TEST_NAME),scales="free")+scale_color_brewer(type = "qual")+
  theme_bw()+theme(legend.position="bottom",axis.text.x=element_blank())+labs(title = "", x="", y="mg/l")+scale_y_continuous(breaks = scales::pretty_breaks(n = 14))

ggsave("Precision by Analyte and Project - Medium Small 2.jpeg",plot=last_plot(),height=5,width=8,units="in")


#medium
ggplot(filter(Summary_Stats_group,Scale=="medium"))+
  geom_crossbar(aes(x=Group,y=Mean, ymin = `LOA Lower`, ymax = `LOA Upper`,colour=Group),position = position_dodge(width  =.95),size=1)+
  geom_hline(yintercept = 0, colour = "black" ,linetype ="longdash",size = .75)+
  geom_errorbar(aes(x=Group, ymin =`Lower mean CI`, ymax =`Upper mean CI` ,fill=Group),color="#636363")+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  geom_errorbar(aes(x=Group, ymin = Mean+`Upper CI 95%  Carkeet`, ymax =  Mean+`Lower 95% CI Carkeet` ,fill=Group),color="#636363")+         #Possible error of lower LOA Exact CI -Careet
  geom_errorbar(aes(x=Group, ymin = Mean-`Lower 95% CI Carkeet`, ymax =  Mean-`Upper CI 95%  Carkeet` ,fill=Group),color="#636363")+         #Possible error of upper LOA  Exact CI -Careet
  facet_grid(col=vars(TEST_NAME),scales="free")+scale_color_brewer(type = "qual")+
  theme_bw()+theme(legend.position="bottom",axis.text.x=element_blank())+labs(title = "", x="", y="mg/l")+scale_y_continuous(breaks = scales::pretty_breaks(n = 14))

ggsave("Precision by Analyte and Project - Medium 2.jpeg",plot=last_plot(),height=5,width=8,units="in")


#medium large
ggplot(filter(Summary_Stats_group,Scale=="medium large"))+
  geom_crossbar(aes(x=Group,y=Mean, ymin = `LOA Lower`, ymax = `LOA Upper`,colour=Group),position = position_dodge(width  =.95),size=1)+
  geom_hline(yintercept = 0, colour = "black" ,linetype ="longdash",size = .75)+
  geom_errorbar(aes(x=Group, ymin =`Lower mean CI`, ymax =`Upper mean CI` ,fill=Group),color="#636363")+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  geom_errorbar(aes(x=Group, ymin = Mean+`Upper CI 95%  Carkeet`, ymax =  Mean+`Lower 95% CI Carkeet` ,fill=Group),color="#636363")+         #Possible error of lower LOA Exact CI -Careet
  geom_errorbar(aes(x=Group, ymin = Mean-`Lower 95% CI Carkeet`, ymax =  Mean-`Upper CI 95%  Carkeet` ,fill=Group),color="#636363")+         #Possible error of upper LOA  Exact CI -Careet
  facet_grid(col=vars(TEST_NAME),scales="free")+scale_color_brewer(type = "qual")+
  theme_bw()+theme(legend.position="bottom",axis.text.x=element_blank())+labs(title = "", x="", y="mg/l")+scale_y_continuous(breaks = scales::pretty_breaks(n = 14))

ggsave("Precision by Analyte and Project - Medium large 2.jpeg",plot=last_plot(),height=5,width=8,units="in")


#large
ggplot(filter(Summary_Stats_group,Scale=="large"))+
  geom_crossbar(aes(x=Group,y=Mean, ymin = `LOA Lower`, ymax = `LOA Upper`,colour=Group),position = position_dodge(width  =.95),size=1)+
  geom_hline(yintercept = 0, colour = "black" ,linetype ="longdash",size = .75)+
  geom_errorbar(aes(x=Group, ymin =`Lower mean CI`, ymax =`Upper mean CI` ,fill=Group),color="#636363")+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  geom_errorbar(aes(x=Group, ymin = Mean+`Upper CI 95%  Carkeet`, ymax =  Mean+`Lower 95% CI Carkeet` ,fill=Group),color="#636363")+         #Possible error of lower LOA Exact CI -Careet
  geom_errorbar(aes(x=Group, ymin = Mean-`Lower 95% CI Carkeet`, ymax =  Mean-`Upper CI 95%  Carkeet` ,fill=Group),color="#636363")+         #Possible error of upper LOA  Exact CI -Careet
  facet_grid(col=vars(TEST_NAME),scales="free")+scale_color_brewer(type = "qual")+
  theme_bw()+theme(legend.position="bottom",axis.text.x=element_blank())+labs(title = "", x="", y="mg/l, PCU")+scale_y_continuous(breaks = scales::pretty_breaks(n = 14))

ggsave("Precision by Analyte and Project - large.jpeg",plot=last_plot(),height=5,width=8,units="in")



#all analytes
ggplot(Summary_Stats_group)+  
  geom_crossbar(aes(x=Group,y=Mean, ymin = `LOA Lower`, ymax = `LOA Upper`,color=Group),position = position_dodge(width  =.95),size=1)+
  geom_hline(yintercept = 0, colour = "black" ,linetype ="longdash",size = .75)+
  geom_errorbar(aes(x=Group, ymin =`Lower Median CI`, ymax =`Upper Median CI` ,fill=Group),color="dark grey")+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  geom_errorbar(aes(x=Group, ymin = Mean+`Upper CI 95%  Carkeet`, ymax =  Mean+`Lower 95% CI Carkeet` ,fill=Group),color="#636363")+         #Possible error of lower LOA Exact CI -Careet
  geom_errorbar(aes(x=Group, ymin = Mean-`Lower 95% CI Carkeet`, ymax =  Mean-`Upper CI 95%  Carkeet` ,fill=Group),color="#636363")+         #Possible error of upper LOA  Exact CI -Careet
  facet_wrap(~TEST_NAME,scales="free",nrow=4)+scale_color_brewer(type = "qual")+
  theme_bw()+theme(legend.position="bottom")+labs(title = "Precision by Analyte and Project", x="Analyte", y="mg/l")+scale_y_continuous(breaks = scales::pretty_breaks(n = 14))


Replicates_vs_PTS_Wilcox <--All_results_Uncertainty %>%
  select(TEST_NAME,`Difference (15-Min - 8-Hour)`,`Difference (15-Min - 24-Hour)`) %>%
  bind_rows(select(ungroup(All_Data_long),PROJECT_CODE,TEST_NAME,`Diff 2 and 3`,`Diff 1 and 3`))   

filter(All_Data_wide,TEST_NAME=="PHOSPHATE, TOTAL AS P")$`Diff 1 and 2`

TPO4_8_Hour <-t.test(filter(All_results_Uncertainty,TEST_NAME=="TPO4")$`Difference (15-Min - 8-Hour)`,gather(filter(All_Data_wide,TEST_NAME=="PHOSPHATE, TOTAL AS P"),"Differences","Value", 8:10)$`Value`, var.equal=FALSE, paired=FALSE)
TPO4_24_hour <-t.test(filter(All_results_Uncertainty,TEST_NAME=="TPO4")$`Difference (15-Min - 24-Hour)`,gather(filter(All_Data_wide,TEST_NAME=="PHOSPHATE, TOTAL AS P"),"Differences","Value", 8:10)$`Value`, var.equal=FALSE, paired=FALSE)
cl_8_Hour <-t.test(filter(All_results_Uncertainty,TEST_NAME=="CL")$`Difference (15-Min - 8-Hour)`,gather(filter(All_Data_wide,TEST_NAME=="CHLORIDE"),"Differences","Value", 8:10)$`Value`, var.equal=FALSE, paired=FALSE)
cl_24_hour <-t.test(filter(All_results_Uncertainty,TEST_NAME=="CL")$`Difference (15-Min - 24-Hour)`,gather(filter(All_Data_wide,TEST_NAME=="CHLORIDE"),"Differences","Value", 8:10)$`Value`, var.equal=FALSE, paired=FALSE)
NH4_8_Hour <-t.test(filter(All_results_Uncertainty,TEST_NAME=="NH4")$`Difference (15-Min - 8-Hour)`,gather(filter(All_Data_wide,TEST_NAME=="AMMONIA-N"),"Differences","Value", 8:10)$`Value`, var.equal=FALSE, paired=FALSE)
NH4_24_hour <-t.test(filter(All_results_Uncertainty,TEST_NAME=="NH4")$`Difference (15-Min - 24-Hour)`,gather(filter(All_Data_wide,TEST_NAME=="AMMONIA-N"),"Differences","Value", 8:10)$`Value`, var.equal=FALSE, paired=FALSE)
test  <-t.test(filter(All_results_Uncertainty,TEST_NAME=="TDSFE")$`Difference (15-Min - 24-Hour)`,gather(filter(All_Data_wide,TEST_NAME=="PHOSPHATE, TOTAL AS P"),"Differences","Value", 8:10)$`Value`, var.equal=FALSE, paired=FALSE)

Summary_Stats$TEST_NAME = factor(Summary_Stats$TEST_NAME, levels=c("TDSFE","COLOR","CL","CA","NA","MG","SIO2","SO4","TOC","DOC","K","NH4","NO2","NOX","OPO4","TDPO4","TPO4","TDN","TN") )




#precision by project
ggplot(Summary_Stats )+
  geom_crossbar(aes(x=PROJECT_CODE,y=median, ymin = `2.5%`, ymax = `97.5%`,colour=PROJECT_CODE),position = position_dodge(width  =.95),size=1)+
  geom_hline(yintercept = 0, colour = "black" ,linetype ="longdash",size = .75)+facet_wrap(~TEST_NAME,scales = "free")+scale_color_brewer(type = "qual")+
  theme_bw()+theme(legend.position="bottom")+labs(title = "Precision by Analyte and Project", x="Analyte", y="mg/l")+scale_y_continuous(breaks = scales::pretty_breaks(n = 14))+
  theme(axis.text.x=element_text(angle=90,hjust=1))

#precision by group 
ggplot(Summary_Stats_group )+
  geom_crossbar(aes(x=Group,y=median, ymin = `2.5%`, ymax = `97.5%`,colour=Group),position = position_dodge(width  =.95),size=1)+
  geom_hline(yintercept = 0, colour = "black" ,linetype ="longdash",size = .75)+facet_wrap(~TEST_NAME,scales = "free")+scale_color_brewer(type = "qual")+
  theme_bw()+theme(legend.position="bottom")+labs(title = "Precision by Analyte and Project", x="Analyte", y="mg/l")+scale_y_continuous(breaks = scales::pretty_breaks(n = 14))+
  theme(axis.text.x=element_text(angle=90,hjust=1))







# Visualize ---------------------------------------------------------------

#Differences by parameters
#Density plots
ggplot(Replicates_vs_PTS,aes(Value,fill=Group))+geom_density(alpha=.5)+facet_wrap(~TEST_NAME,scales = "free")+theme_bw()+scale_fill_brewer(type = "qual")+
  stat_function(fun=dnorm)#,args=list(mean=mean(Replicates_vs_PTS$Value,na.rm = TRUE), sd=sd(Replicates_vs_PTS$Value,na.rm=TRUE)))

ggplot(Replicates_vs_PTS,aes(Value,fill=Group))+geom_density(alpha=.5)+facet_wrap(~TEST_NAME,scales = "free")+theme_bw()+scale_fill_brewer(type = "qual")+
  stat_function(fun=dnorm,color="red")#,args=list(mean=mean(Replicates_vs_PTS$Value,na.rm = TRUE), sd=sd(Replicates_vs_PTS$Value,na.rm=TRUE)))


ggplot(Replicates_vs_PTS,aes(Value,fill=Group))+geom_density(alpha=.5)+facet_wrap(~TEST_NAME,scales = "free")+theme_bw()+scale_fill_brewer(type = "qual")+
  geom_vline(data=Summary_Stats_group,aes(xintercept=median,color=Group),alpha=.6)+ #medians 
  geom_vline(data=Summary_Stats_group,aes(xintercept=median+sd,color=Group),alpha=.6) +# + 1SD 
  geom_vline(data=Summary_Stats_group,aes(xintercept=median-sd,color=Group),alpha=.6) # - 1SD 

#Boxplots
ggplot(Replicates_vs_PTS,aes(TEST_NAME,Value,fill=PROJECT_CODE))+geom_boxplot(alpha=.5)+theme_bw()+scale_fill_brewer(type = "qual")+facet_wrap(~TEST_NAME,scales = "free",nrow = 3)+
  labs(x="")  #theme(axis.text.x=element_text(angle=90,hjust=1))

large_LOA <- c("CL","NA","CA","SO4","COLOR","DOC","TOC","MG","K","SIO2")
small_LOA <- c("TN","TDN","NH4")
very_small_LOA <- c("TDSFE","TPO4","NO2","OPO4")
percentage_LOA <- c("TDPO4","NOX")

ggplot(filter(Replicates_vs_PTS,TEST_NAME %in% large_LOA),aes(TEST_NAME,Value,fill=Group))+geom_boxplot(alpha=.5)+theme_bw()+scale_fill_brewer(type = "qual")+facet_wrap(~TEST_NAME,scales = "free",nrow = 3)+
  labs(x="")  #theme(axis.text.x=element_text(angle=90,hjust=1))

ggplot(filter(Replicates_vs_PTS,TEST_NAME %in% small_LOA),aes(TEST_NAME,Value,fill=Group))+geom_boxplot(alpha=.5)+theme_bw()+scale_fill_brewer(type = "qual")+labs(x="")  #theme(axis.text.x=element_text(angle=90,hjust=1))

ggplot(filter(Replicates_vs_PTS,TEST_NAME %in% very_small_LOA),aes(TEST_NAME,Value,fill=Group))+geom_boxplot(alpha=.5)+theme_bw()+scale_fill_brewer(type = "qual")+labs(x="")  #theme(axis.text.x=element_text(angle=90,hjust=1))

ggplot(filter(Replicates_vs_PTS,TEST_NAME %in% percentage_LOA),aes(TEST_NAME,Value,fill=Group))+geom_boxplot(alpha=.5)+theme_bw()+scale_fill_brewer(type = "qual")+labs(x="")  #theme(axis.text.x=element_text(angle=90,hjust=1))


# Tidy bootstrapping ------------------------------------------------------

Replicates_vs_PTS

# Nest the data
Replicates_vs_PTS_nest <- Replicates_vs_PTS  %>%
  filter(TEST_NAME=="TPO4") %>%
  select(Group,Value) %>%
  # First I group the data by species 
  group_by(Group) %>%
  # Then I nest the dataframe
  nest()


# Define the bootstrap function
boot_median <- function(d, i) {
  median(d[i],na.rm = TRUE)
}

# Add a list column called "booted" containing the object produced by `
# applying `boot::boot` over the data in the "data" list-column.
Replicates_vs_PTS_nest_boot <-  Replicates_vs_PTS_nest %>%
  mutate(booted = purrr::map(.x = data, # The list-column containing <S3: tibble>
                             ~ boot(data = .x$Value, # The <S3 tibble> column being sampled
                                    statistic = boot_median, # The user-defined function
                                    R = 10000, # The number of replicates
                                    stype = "i")))

Replicates_vs_PTS_nest_boot_ci <- Replicates_vs_PTS_nest_boot  %>%
  mutate(booted_ci = purrr::map(.x = booted,  ~ boot.ci(.x,conf = 0.95,type ="norm" )))  # Calculate a BCa interval

# Inspect the structure of an <S3 bootci> object
str(Replicates_vs_PTS_nest_boot_ci $booted_ci[[1]])


# Add a three column called "statistic" (point estimate), "lower_ci" (2.5% CI), and 
# "upper_ci" (97.5% CI), which are populated by data extracted using `purrr::map` 
# from the <S3 bootci> objects in the "booted_ci" list-column. 
Replicates_vs_PTS_nest_booted <- Replicates_vs_PTS_nest_boot_ci %>%
  mutate(statistic = purrr::map(.x = booted_ci, # The list-column containing <S3 bootci> objects
                                ~ .x$t0), # The point estimate
         lower_ci = purrr::map(.x = booted_ci,
                               ~ .x$normal[[2]]), # The value of the lower 2.5% limit
         upper_ci = purrr::map(.x = booted_ci,
                               ~ .x$normal[[3]])) %>% # The value of teh upper 97.5% limit
  # Drop the list-columns (no longer needed)
  select(-data, -booted, -booted_ci) %>%
  # Unnest the dataframe
  unnest()




# Bootstrap  --------------------------------------------------------------


#bootstrap 
null_distn <- filter(All_Data_long, TEST_NAME=="PHOSPHATE, TOTAL AS P") %>% 
  specify(response =Value) %>% 
  hypothesize(null = "point", mu = 0) %>% 
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "mean") 

one_poll_boot %>% 
  summarize(p_value = mean(stat >= mean_tp) * 2) 

one_poll_boot <- sample_n(ungroup(filter(All_Data_long, TEST_NAME=="PHOSPHATE, TOTAL AS P")), 36,replace=TRUE) %>%
  specify(response = Value) %>%
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "mean")

mean_tp <- mean(filter(All_Data_long, TEST_NAME=="PHOSPHATE, TOTAL AS P")$Value,na.rm = TRUE)

one_poll_boot_stat <- one_poll_boot %>%
  summarize(lower = mean_tp - 2 * sd(stat),upper = mean_tp + 2 * sd(stat))

ggplot(data = one_poll_boot, mapping = aes(x = stat)) + 
  geom_density() + 
  geom_vline( xintercept = mean_tp, color = "red") 

get_confidence_interval(one_poll_boot,level=.95)

bootstrapper <-  function(n,Parameter)  {
  
  t1 <- sample_n(ungroup(filter(All_Data_long, TEST_NAME==Parameter)), n,replace=TRUE) %>%
    specify(response = Value) %>%
    generate(reps = 1000, type = "bootstrap") %>% 
    calculate(stat = "mean") 
  
  CI <-get_confidence_interval(t1,level=.95)
  return(CI)
}


test <- data.frame(lower=numeric(),VAR2=numeric())

i<-0
while(i < 100)
{
  test=rbind(test,cbind(bootstrapper(36,"TPO4"),i))  
  i<-i+1
}

tp_variance_n_6 <- test
tp_variance_n_36 <- test
tp_variance_n_100 <- test

ggplot(data = tp_variance_n_6 , mapping = aes(x =`2.5%`)) + 
  geom_histogram() + 
  geom_vline( xintercept =-0.004, color = "red") 

tp_data <-filter(All_Data_long, TEST_NAME=="TPO4")

test <-sample_n(tp_data, 1000,replace=TRUE)


# Balanced Data -----------------------------------------------------------


Balanced_pts_data <- All_results_Uncertainty %>%
  group_by(TEST_NAME) %>%
  sample_n( 500,replace=TRUE)

Replicates_vs_PTS_balanced <-Balanced_pts_data %>%
  select(TEST_NAME,`Difference (15-Min - 8-Hour)`,`Difference (15-Min - 24-Hour)`) %>%
  gather("Differences","Value", 2:3) %>%
  mutate(PROJECT_CODE= case_when(Differences=="Difference (15-Min - 8-Hour)"~ "8-Hour Samples",    
                                 Differences=="Difference (15-Min - 24-Hour)"~"24-Hour Samples")) %>%
  mutate(Group= case_when(Differences=="Difference (15-Min - 8-Hour)"~ "8-Hour Samples",    
                          Differences=="Difference (15-Min - 24-Hour)"~"24-Hour Samples")) %>%
  bind_rows(mutate(select(ungroup(All_Data_long),PROJECT_CODE,TEST_NAME,Differences,Value),Group="Replicates")) 

Summary_Stats_group_balanced <- Replicates_vs_PTS %>%
  group_by(TEST_NAME,Group) %>%
  summarise(`Normal`=shapiro.test(`Value`)$p.value,n=n(),sd=sd(Value,na.rm = TRUE),SE=sd(Value,na.rm = TRUE)/sqrt(n()),`Boot TP 2.5%`=mean(Value,na.rm = TRUE)-2*sd, `2.5%`=quantile(Value,0.025,na.rm = TRUE),`5%`=quantile(Value,0.05,na.rm = TRUE),Mean=mean(Value,na.rm = TRUE),median=median(Value,na.rm = TRUE),`95%`=quantile(Value,0.95,na.rm = TRUE),`97.5%`=quantile(Value,0.975,na.rm = TRUE),`Boot TP 97.5%`=Mean+2*sd) %>%
  mutate_if(is.numeric, round,digits=4) 

ggplot(Summary_Stats_group_balanced )+
  geom_crossbar(aes(x=Group,y=median, ymin = `2.5%`, ymax = `97.5%`,colour=Group),position = position_dodge(width  =.95),size=1)+
  geom_hline(yintercept = 0, colour = "black" ,linetype ="longdash",size = .75)+facet_wrap(~TEST_NAME,scales = "free")+scale_color_brewer(type = "qual")+
  theme_bw()+theme(legend.position="bottom")+labs(title = "Precision by Analyte and Project", x="Analyte", y="mg/l")+scale_y_continuous(breaks = scales::pretty_breaks(n = 14))+
  theme(axis.text.x=element_text(angle=90,hjust=1))

Balanced_pts_data_vs_original <- Balanced_pts_data %>%
  mutate(`Data Set`="Balanced") %>%
  bind_rows(mutate(All_results_Uncertainty,`Data Set`="Original")) %>%
  select(`Data Set`,TEST_NAME,`Difference (15-Min - 8-Hour)`,`Difference (15-Min - 24-Hour)`) %>%
  gather("Differences","Value", 3:4) %>%
  mutate(PROJECT_CODE= case_when(Differences=="Difference (15-Min - 8-Hour)"~ "8-Hour Samples", Differences=="Difference (15-Min - 24-Hour)"~"24-Hour Samples")) %>%
  mutate(Group= case_when(Differences=="Difference (15-Min - 8-Hour)"~ "8-Hour Samples",  Differences=="Difference (15-Min - 24-Hour)"~"24-Hour Samples"))

Summary_Stats_group_balanced_vs_original <- Balanced_pts_data_vs_original %>%
  group_by(TEST_NAME,`Data Set`) %>%
  summarise(n=n(),sd=sd(Value,na.rm = TRUE),SE=sd(Value,na.rm = TRUE)/sqrt(n()),`Boot TP 2.5%`=mean(Value,na.rm = TRUE)-2*sd, `2.5%`=quantile(Value,0.025,na.rm = TRUE),`5%`=quantile(Value,0.05,na.rm = TRUE),Mean=mean(Value,na.rm = TRUE),median=median(Value,na.rm = TRUE),`95%`=quantile(Value,0.95,na.rm = TRUE),`97.5%`=quantile(Value,0.975,na.rm = TRUE),`Boot TP 97.5%`=Mean+2*sd)

ggplot(Summary_Stats_group_balanced_vs_original )+
  geom_crossbar(aes(x=`Data Set`,y=median, ymin = `2.5%`, ymax = `97.5%`,colour=`Data Set`),position = position_dodge(width  =.95),size=1)+
  geom_hline(yintercept = 0, colour = "black" ,linetype ="longdash",size = .75)+facet_wrap(~TEST_NAME,scales = "free")+scale_color_brewer(type = "qual")+
  theme_bw()+theme(legend.position="bottom")+labs(title = "Precision by Analyte and Project", x="Analyte", y="mg/l")+scale_y_continuous(breaks = scales::pretty_breaks(n = 14))+
  theme(axis.text.x=element_text(angle=90,hjust=1))


ggplot(Summary_Stats_group_balanced_vs_original)+
  geom_crossbar(aes(x=`Data Set`,y=median, ymin = median-sd, ymax = median+sd,colour=`Data Set`),position = position_dodge(width  =.95),size=1)+
  geom_hline(yintercept = 0, colour = "black" ,linetype ="longdash",size = .75)+facet_wrap(~TEST_NAME,scales = "free")+scale_color_brewer(type = "qual")+
  theme_bw()+theme(legend.position="bottom")+labs(title = "Precision by Analyte and Project", x="Analyte", y="mg/l")+scale_y_continuous(breaks = scales::pretty_breaks(n = 14))+
  theme(axis.text.x=element_text(angle=90,hjust=1))

#normality figures QQ plot
ggplot(Balanced_pts_data_vs_original,aes(sample=Value))+geom_qq()+facet_grid(Group~TEST_NAME)





# Manuscript figures ------------------------------------------------------
#MG BA Plot Figure 2 in manuscript
BA_MG <-   filter(All_results_Uncertainty,TEST_NAME=="MG")

ggplot(BA_MG ,aes((Value_15_min+Value_8_hour)/2,`Difference (15-Min - 8-Hour)`))+
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = .545, ymax =.866), alpha = 0.3,fill="grey90")+
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -.517, ymax = -.938), alpha = 0.3,fill="grey90")+  
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -.0976, ymax = 0.125), alpha = 0.3,fill="grey70")+
  geom_hline(yintercept= 0.014, colour ="black" ,size = 1,linetype="dashed")+
  geom_hline(yintercept= 0.673, colour = "grey10" ,linetype ="longdash",size = 1)+
  geom_hline(yintercept= -0.645, colour = "grey10" ,linetype ="longdash",size = 1)+
  geom_point(shape=21,size=3 ,color="black",fill="grey50")+
  scale_y_continuous(breaks = pretty_breaks(n = 8),limits = c(-1,1))+scale_x_continuous(breaks = pretty_breaks(n = 6),limits = c(10,35))+
  annotate(x=18,y=-0.025,label="Bias with 95% Confidence Interval",geom = "text" ,color="grey10")+
  annotate(x=15,y=.72,label="Upper LOA with 95% Confidence Interval",geom = "text" ,color="grey10")+
  annotate(x=15,y=-.6,label="Lower LOA with 95% Confidence Interval",geom = "text",color="grey10" )+theme(legend.position="bottom")+  
  labs( x=expression("Paired Sample Mean "~(mg~L^{-1})), y=expression("Paired Sample Differences "~(mg~L^{-1})))+theme_bw()

ggsave("Figure 2 BA plot mg.jpeg",plot=last_plot(),height=5,width=8,units="in")

Summary_Stats_group_labels$TEST_NAME = factor(Summary_Stats_group$TEST_NAME, levels=c("CL","COLOR","CA","NA","SO4","DOC","TOC","MG","SIO2","K","TN","TDN","TDSFE","NH4","NOX","TPO4","OPO4","TDPO4","NO2") )
Summary_Stats_group_labels$Group = factor(Summary_Stats_group_labels$Group, levels=c("Replicate Samples","8-Hour Samples","24-Hour Samples") )

Summary_Stats_group_labels <- Summary_Stats_group %>%
  mutate(Group=case_when(Group=="Replicates"~"Replicate Samples", TRUE ~ as.character(Group))) %>%  
  mutate(TEST_NAME= case_when(TEST_NAME=="OPO4" ~"Orthophosphate",    
                              TEST_NAME=="TPO4" ~"Total Phosphorus",
                              TEST_NAME=="TDPO4" ~"Total Dissolved Phosphorus",
                              TEST_NAME=="TDSFE" ~"Dissolved Iron", 
                              TEST_NAME=="SIO2" ~"Dissolved Silica",
                              TEST_NAME=="HARDNESS AS CACO3" ~"Hardness",
                              TEST_NAME=="ALKALINITY, TOT, CACO3" ~"Alkalinity",  
                              TEST_NAME=="DOC" ~"Dissolved Organic Carbon",
                              TEST_NAME=="NH4" ~"Ammonia",
                              TEST_NAME=="TDN" ~ "Total Dissolved Nitrogen",
                              TEST_NAME=="NOX" ~ "Nitrate + Nitrite ",
                              TEST_NAME=="TOC" ~ "Total Organic Carbon",
                              TEST_NAME=="TN" ~ "Total Nitrogen",
                              TEST_NAME=="NA" ~"Dissolved Sodium",  
                              TEST_NAME=="MG" ~"Dissolved Magnesium",
                              TEST_NAME=="CA" ~"Dissolved Calcium",
                              TEST_NAME=="CL" ~"Chloride",
                              TEST_NAME=="K" ~"Dissolved Potassium", 
                              TEST_NAME=="SO4" ~ "Sulfate",
                              TEST_NAME=="NO2" ~ "Nitrite",
                              TEST_NAME=="COLOR" ~ "Color",
                              TRUE ~ as.character(TEST_NAME)))




#Small abridged BA plots Figure 7
ggplot(filter(Summary_Stats_group_labels,Scale=="small"))+
  geom_crossbar(aes(x=Group,y=Mean, ymin = `LOA Lower`, ymax = `LOA Upper`,fill=Group),position = position_dodge(width  =.95),size=.5,fatten=2)+
  scale_fill_manual(values=c("grey90","grey70","grey50"))+
  geom_errorbar(aes(x=Group, ymin =`Lower mean CI`, ymax =`Upper mean CI` ),color="grey25",width=.75)+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  geom_errorbar(aes(x=Group, ymin = Mean+`Upper CI 95%  Carkeet`, ymax =  Mean+`Lower 95% CI Carkeet` ,fill=Group),color="grey25",width=.75)+         #Possible error of lower LOA Exact CI -Careet
  geom_errorbar(aes(x=Group, ymin = Mean-`Lower 95% CI Carkeet`, ymax =  Mean-`Upper CI 95%  Carkeet` ,fill=Group),color="grey25",width=.75)+         #Possible error of upper LOA  Exact CI -Careet
  geom_hline(yintercept = 0, linetype ="longdash",size = .5,color="grey25")+
  facet_grid(col=vars(TEST_NAME),scales="free")+
  theme_bw()+theme(legend.position="bottom",axis.text.x=element_blank(),legend.title = element_blank())+labs(title = "", x="", y=expression(~(mg~L^{-1})))+scale_y_continuous(breaks = scales::pretty_breaks(n = 14))

ggsave("Fig 7 Abridged BA NH4 NOX TPO4.jpeg",plot=last_plot(),height=5,width=8,units="in")


#very small
ggplot(filter(Summary_Stats_group_labels,Scale=="very small"))+
  geom_crossbar(aes(x=Group,y=Mean, ymin = `LOA Lower`, ymax = `LOA Upper`,fill=Group),position = position_dodge(width  =.95),size=.5,fatten=2)+
  scale_fill_manual(values=c("grey90","grey70","grey50"))+
  geom_errorbar(aes(x=Group, ymin =`Lower mean CI`, ymax =`Upper mean CI` ),color="grey25",width=.75)+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  geom_errorbar(aes(x=Group, ymin = Mean+`Upper CI 95%  Carkeet`, ymax =  Mean+`Lower 95% CI Carkeet` ,fill=Group),color="grey25",width=.75)+         #Possible error of lower LOA Exact CI -Careet
  geom_errorbar(aes(x=Group, ymin = Mean-`Lower 95% CI Carkeet`, ymax =  Mean-`Upper CI 95%  Carkeet` ,fill=Group),color="grey25",width=.75)+         #Possible error of upper LOA  Exact CI -Careet
  geom_hline(yintercept = 0, linetype ="longdash",size = .5,color="grey25")+
  facet_grid(col=vars(TEST_NAME),scales="free")+
  theme_bw()+theme(legend.position="bottom",axis.text.x=element_blank(),legend.title = element_blank())+labs(title = "", x="", y=expression(~(mg~L^{-1})))+scale_y_continuous(breaks = scales::pretty_breaks(n = 14))

ggsave("Fig 8 Abridged BA TDSFE OPO4 TDPO4 NO2.jpeg",plot=last_plot(),height=5,width=8,units="in")


#FIg 6 medium small
ggplot(filter(Summary_Stats_group_labels,Scale=="medium small"))+
  geom_crossbar(aes(x=Group,y=Mean, ymin = `LOA Lower`, ymax = `LOA Upper`,fill=Group),position = position_dodge(width  =.95),size=.5,fatten=2)+
  scale_fill_manual(values=c("grey90","grey70","grey50"))+
  geom_errorbar(aes(x=Group, ymin =`Lower mean CI`, ymax =`Upper mean CI` ),color="grey25",width=.75)+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  geom_errorbar(aes(x=Group, ymin = Mean+`Upper CI 95%  Carkeet`, ymax =  Mean+`Lower 95% CI Carkeet` ,fill=Group),color="grey25",width=.75)+         #Possible error of lower LOA Exact CI -Careet
  geom_errorbar(aes(x=Group, ymin = Mean-`Lower 95% CI Carkeet`, ymax =  Mean-`Upper CI 95%  Carkeet` ,fill=Group),color="grey25",width=.75)+         #Possible error of upper LOA  Exact CI -Careet
  geom_hline(yintercept = 0, linetype ="longdash",size = .5,color="grey25")+
  facet_grid(col=vars(TEST_NAME),scales="free")+
  theme_bw()+theme(legend.position="bottom",axis.text.x=element_blank(),legend.title = element_blank())+labs(title = "", x="", y=expression(~(mg~L^{-1})))+scale_y_continuous(breaks = scales::pretty_breaks(n = 14))

ggsave("Fig 6 Abridged BA K TDN TN.jpeg",plot=last_plot(),height=5,width=8,units="in")


#Fig 5 Abridged BA MG SIO2 TOC medium
ggplot(filter(Summary_Stats_group_labels,Scale=="medium"))+
  geom_crossbar(aes(x=Group,y=Mean, ymin = `LOA Lower`, ymax = `LOA Upper`,fill=Group),position = position_dodge(width  =.95),size=.5,fatten=2)+
  scale_fill_manual(values=c("grey90","grey70","grey50"))+
  geom_errorbar(aes(x=Group, ymin =`Lower mean CI`, ymax =`Upper mean CI` ),color="grey25",width=.75)+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  geom_errorbar(aes(x=Group, ymin = Mean+`Upper CI 95%  Carkeet`, ymax =  Mean+`Lower 95% CI Carkeet` ,fill=Group),color="grey25",width=.75)+         #Possible error of lower LOA Exact CI -Careet
  geom_errorbar(aes(x=Group, ymin = Mean-`Lower 95% CI Carkeet`, ymax =  Mean-`Upper CI 95%  Carkeet` ,fill=Group),color="grey25",width=.75)+         #Possible error of upper LOA  Exact CI -Careet
  geom_hline(yintercept = 0, linetype ="longdash",size = .5,color="grey25")+
  facet_grid(col=vars(TEST_NAME),scales="free")+
  theme_bw()+theme(legend.position="bottom",axis.text.x=element_blank(),legend.title = element_blank())+labs(title = "", x="", y=expression(~(mg~L^{-1})))+scale_y_continuous(breaks = scales::pretty_breaks(n = 14))

ggsave("Fig 5 Abridged BA MG SIO2 TOC.jpeg",plot=last_plot(),height=5,width=8,units="in")


#Fig 4 DOC Sodium Sulfate medium large
ggplot(filter(Summary_Stats_group_labels,Scale=="medium large"))+
  geom_crossbar(aes(x=Group,y=Mean, ymin = `LOA Lower`, ymax = `LOA Upper`,fill=Group),position = position_dodge(width  =.95),size=.5,fatten=2)+
  scale_fill_manual(values=c("grey90","grey70","grey50"))+
  geom_errorbar(aes(x=Group, ymin =`Lower mean CI`, ymax =`Upper mean CI` ),color="grey25",width=.75)+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  geom_errorbar(aes(x=Group, ymin = Mean+`Upper CI 95%  Carkeet`, ymax =  Mean+`Lower 95% CI Carkeet` ,fill=Group),color="grey25",width=.75)+         #Possible error of lower LOA Exact CI -Careet
  geom_errorbar(aes(x=Group, ymin = Mean-`Lower 95% CI Carkeet`, ymax =  Mean-`Upper CI 95%  Carkeet` ,fill=Group),color="grey25",width=.75)+         #Possible error of upper LOA  Exact CI -Careet
  geom_hline(yintercept = 0, linetype ="longdash",size = .5,color="grey25")+
  facet_grid(col=vars(TEST_NAME),scales="free")+
  theme_bw()+theme(legend.position="bottom",axis.text.x=element_blank(),legend.title = element_blank())+labs(title = "", x="", y=expression(~(mg~L^{-1})))+scale_y_continuous(breaks = scales::pretty_breaks(n = 14))

ggsave("Fig 4 DOC Sodium Sulfate.jpeg",plot=last_plot(),height=5,width=8,units="in")


#Fig 3 Cl Color CA large
ggplot(filter(Summary_Stats_group_labels,Scale=="large"))+
  geom_crossbar(aes(x=Group,y=Mean, ymin = `LOA Lower`, ymax = `LOA Upper`,fill=Group),position = position_dodge(width  =.95),size=.5,fatten=2)+
  scale_fill_manual(values=c("grey90","grey70","grey50"))+
  geom_errorbar(aes(x=Group, ymin =`Lower mean CI`, ymax =`Upper mean CI` ),color="grey25",width=.75)+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  geom_errorbar(aes(x=Group, ymin = Mean+`Upper CI 95%  Carkeet`, ymax =  Mean+`Lower 95% CI Carkeet` ,fill=Group),color="grey25",width=.75)+         #Possible error of lower LOA Exact CI -Careet
  geom_errorbar(aes(x=Group, ymin = Mean-`Lower 95% CI Carkeet`, ymax =  Mean-`Upper CI 95%  Carkeet` ,fill=Group),color="grey25",width=.75)+         #Possible error of upper LOA  Exact CI -Careet
  geom_hline(yintercept = 0, linetype ="longdash",size = .5,color="grey25")+
  facet_grid(col=vars(TEST_NAME),scales="free")+
  theme_bw()+theme(legend.position="bottom",axis.text.x=element_blank(),legend.title = element_blank())+labs(title = "", x="", y=expression(~(mg~L^{-1})))+scale_y_continuous(breaks = scales::pretty_breaks(n = 14))

ggsave("Fig 3 Cl Color CA.jpeg",plot=last_plot(),height=5,width=8,units="in")


