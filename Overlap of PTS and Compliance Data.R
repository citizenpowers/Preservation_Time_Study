#The goal of this script is to explore the overlap between the PTS data set and compliance data set
#Two objectives. (A) Calculate percent of compliance data within PTS range. (B) Calculate percent of compliance range that PTS covers. 
#1a.) Calculate range of PTS data
#2a.) Flag compliance data within range of PTS data
#3a.) For each analyte calculate percent of compliance data within range of PTS data

#1b.) Calculate range of compliance data
#2b.) Calculate the percent of compliance range PTS covers using the product of 1a and 1b. 


library(readxl)
library(tidyverse)
library(scales)
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

# Tidy Data ---------------------------------------------------------------

#Convert PTS data stored in ERDP format to DBHYDRO format to match the compliance data 
Sample_Results <- sample_results_final %>%
rename(SAMPLE_ID="FLDSID",COLLECT_DATE="SDATE",STATION="SITE",SAMPLE_TYPE="TYPE",TEST_NAME="PARAMETER",VALUE="RESULT",UNITS="UNIT") #rename ERDP variables to DBHYDRO format

# 1a.) Calculate range of PTS data -----------------------------------------

#Min and Max in PTS dataset for each parameter
PTS_Range <- Sample_Results %>%
filter(STATION != "EB")  %>%
group_by(TEST_NAME) %>%
summarise(min_pts=min(VALUE,na.rm=T),max_pts=max(VALUE,na.rm=T)) %>%
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
select(STATION_ID, DATE_COLLECTED, TEST_NAME, VALUE, UNITS,min_pts,max_pts, PQL, MDL,Flag)  


# 3.) Calculate percent of compliance data within PTS range -------------------

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
summarise(min_comp=min(VALUE,na.rm=T),max_comp=max(VALUE,na.rm=T),median=median(VALUE,na.rm=T),SD=round(sd(VALUE,na.rm=T),digits=3)) %>%
mutate(Range_comp=max_comp-min_comp)



# 2b.) Calculate the percent of compliance data the PTS covers ------------

Combined_range <- Compliance_Range %>%
left_join(PTS_Range) %>%
mutate(Range=Range_pts/Range_comp)
