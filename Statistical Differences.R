#Goal of this script is to evaluate the statistical differences between treatments


library(tidyverse)
library(multcompView)
library(broom)
library(Metrics)
library(readxl)
library(writexl)
library(ggridges)
library(viridis)
library(scales)
library(utils)
library(ggpmisc)
#library(odbc)
library(DBI)
library(dbhydroR)
library(gghighlight)
library(moments)


# Import Data -------------------------------------------------------------


Sample_Results_Tidy <- read_csv("Data/Sample_Results_Tidy.csv")
All_results_Uncertainty <-read_csv("./Data/All_results_Uncertainty.csv")

# Tidy Data. Adjust column names so they can be used in models ---------------------------------------------------------------

Sample_Results_Model_Data <- Sample_Results_Tidy %>%
mutate(Preservation_Time=case_when(`Preservation Time`=="8-Hour Samples"~"8_Hour_Samples",
                                   `Preservation Time`=="24-Hour Samples"~"24_Hour_Samples",
                                   `Preservation Time`=="Immediate Samples"~"15_Minute_Samples")) %>% #remove spaces
mutate(TEST_NAME=ifelse(is.na(TEST_NAME),"Sodium",TEST_NAME))

# Run ANOVA test ----------------------------------------------------------

#Run ANOVA and Tukey's HSD on concentrations
Test_Name_Anova_models <- Sample_Results_Model_Data  %>%
nest_by(TEST_NAME) %>% 
mutate(Model = list(lm(VALUE ~ `Preservation_Time`, data = data))) %>%   #create model
mutate(Tukey_HSD=list(TukeyHSD(aov(Model)))) %>%             #Tukey's pairwise comparison
mutate(Letters=list(multcompLetters4(Model, Tukey_HSD)$Preservation_Time$Letters)) %>% #Label ecotopes that are not sig different with same letter
mutate(Letters2=list(as.list(Letters)))  %>%                                 #create list that can be unnested. Multicomplist will not unnest.  
unnest_wider(Letters2) %>%
pivot_longer(names_to="Preservation Time",values_to="Letter",6:8) %>%
select("Preservation Time","TEST_NAME","Letter") 

# calculate stats table for each analyte ----------------------------------
  
  #calculate stats table for each analyte
  LOA <- All_results_Uncertainty %>%
    group_by(TEST_NAME) %>%
    summarise(
      `Pearson Correlation 8-hour`=cor(Value_15_min, Value_8_hour, method = "pearson"),
      `Pearson Correlation 24-hour`=cor(Value_15_min, Value_24_hour, method = "pearson"),
      `Spearman Correlation 8-hour`=cor(Value_15_min, Value_8_hour, method = "spearman"),
      `Spearman Correlation 24-hour`=cor(Value_15_min, Value_24_hour, method = "spearman"),
      `8-Hour r squared`=cor(Value_15_min, Value_8_hour)^2,
      `8-Hour Accuracy`=round(mean(Value_8_hour/Value_15_min),digits=3),
      `8-Hour Mean Percent Error`=round(mean(abs(`8-Hour Percent Error`)),digits=4),
      `8-hour effect size`=abs(mean(`Difference (15-Min - 8-Hour)`)/sd(Value_8_hour)),
      `8-Hour Min`=round(min(Value_8_hour),digits=4),                                              #calculate min 8-hour
      `8-Hour Max`=round(max(Value_8_hour),digits=4),                                              #calculate max 8-hour
      `8-Hour Bias`=round(mean(`Difference (15-Min - 8-Hour)`),digits=4),                          #calculate Bias 8-hour
      `8-Hour Min Difference`=round(min(`Difference (15-Min - 8-Hour)`),digits=4),                 #calculate min difference 8-hour
      `8-Hour Max Difference`=round(max(`Difference (15-Min - 8-Hour)`),digits=4),                 #calculate max difference 8-hour
      `8-Hour Median Bias`=round(median(`Difference (15-Min - 8-Hour)`),digits=4),                 #calculate Bias median 8-hour
      `8-Hour Mean Percent Error`=round(mean(abs(`8-Hour Percent Error`)),digits=4),               #calculate mean percent 8-hour
      `8-Hour Median Percent Error`=round(median(abs(`8-Hour Percent Error`)),digits=4),           #calculate median percent 8-hour
      `8-Hour Mean log Bias`=round(10^(mean(`Log10 Difference (15-Min - 8-Hour)`)),digits=4),      #calculate mean of log differences
      `8-Hour Uncertainty as Percent Error`=round(mean(abs(`Immediate Sample Uncertainty`/Value_8_hour)*100),digits=4),     #calculate percent error of uncertainty
      `8-Hour SQRT Bias`=round((mean(`SQRT Difference (15-Min - 8-Hour)`)),digits=4),              #calculate mean of SQRT differences
      `8-Hour Cubic Root Bias`=round((mean(`Cubic Root Difference (15-Min - 8-Hour)`)),digits=4),  #calculate mean of cubic root differences
      `8-Hour SD`=round(sd(`Difference (15-Min - 8-Hour)`),digits=4),                              #calculate Standard Deviation of differences
      `8-Hour log SD`=round(sd(`Log10 Difference (15-Min - 8-Hour)`),digits=4),                    #calculate Standard Deviation of differences Log
      `8-Hour SQRT SD`=round(sd(`SQRT Difference (15-Min - 8-Hour)`),digits=4),                    #calculate Standard Deviation of differences SQRT
      `8-Hour Cubic Root SD`=round(sd(`Cubic Root Difference (15-Min - 8-Hour)`),digits=4),        #calculate Standard Deviation of differences SQRT
      `8-Hour LOA Lower`=round(mean((`Difference (15-Min - 8-Hour)`)-1.96*`8-Hour SD`),digits=4),  #Calculate lower limit of agreement
      `8-Hour LOA Upper`=round(mean((`Difference (15-Min - 8-Hour)`)+1.96*`8-Hour SD`),digits=4),  #Calculate upper limit of agreement
      `8-Hour LOA 2.5%`=round(quantile(`8-Hour Percent Error`,.025),digits=4),                     #Calculate lower nonparametric limit of agreement
      `8-Hour LOA 97.5%`=round(quantile(`8-Hour Percent Error`,.975),digits=4),                    #Calculate upper non-paramteric limit of agreement
      `8-Hour LOA Log Lower`=round(10^(mean((`Log10 Difference (15-Min - 8-Hour)`)-1.96*`8-Hour log SD`)),digits=4),  #Calculate lower limit of agreement
      `8-Hour LOA Log Upper`=round(10^(mean((`Log10 Difference (15-Min - 8-Hour)`)+1.96*`8-Hour log SD`)),digits=4),  #Calculate lower limit of agreement
      `8-Hour LOA SQRT Lower`=round((mean((`SQRT Difference (15-Min - 8-Hour)`)-1.96*`8-Hour SQRT SD`)),digits=4),  #Calculate lower limit of agreement
      `8-Hour LOA SQRT Upper`=round((mean((`SQRT Difference (15-Min - 8-Hour)`)+1.96*`8-Hour SQRT SD`)),digits=4),  #Calculate lower limit of agreement
      `8-Hour LOA Cubic Root Lower`  =round((mean((`Cubic Root Difference (15-Min - 8-Hour)`)-1.96*`8-Hour Cubic Root SD`)),digits=4),  #Calculate lower limit of agreement
      `8-Hour LOA Cubic Root Upper`=round((mean((`Cubic Root Difference (15-Min - 8-Hour)`)+1.96*`8-Hour Cubic Root SD`)),digits=4),  #Calculate lower limit of agreement
      `24-Hour r squared`=cor(Value_15_min, Value_24_hour)^2,
      `24-Hour Accuracy`=round(mean(Value_24_hour/Value_15_min),digits=3),
      `24-hour effect size`=abs(mean(`Difference (15-Min - 24-Hour)`)/sd(Value_24_hour)),
      `24-Hour Min`=round(min(Value_24_hour),digits=4),                                            #calculate min 24-hour
      `24-Hour Max`=round(max(Value_24_hour),digits=4),                                            #calculate max 24-hour
      `24-Hour Bias`=round(mean(`Difference (15-Min - 24-Hour)`),digits=4),                        #calculate Bias 24-hour
      `24-Hour Median Bias`=round(median(abs(`Difference (15-Min - 24-Hour)`)),digits=4),               #calculate median Bias 24-hour
      `24-Hour Mean Percent Error`=round(mean(abs(`24-Hour Percent Error`)),digits=4),                  #calculate mean percent 24-hour
      `24-Hour Median Percent Error`=round(median(`24-Hour Percent Error`),digits=4),              #calculate median percent 24-hour
      `24-Hour Uncertainty as Percent Error`=round(mean(abs(`24-Hour Sample Uncertainty`/Value_24_hour)*100),digits=4),     #calculate percent error of uncertainty
      `24-Hour Mean log Bias`=round(10^(mean(`Log10 Difference (15-Min - 24-Hour)`)),digits=4),           #calculate mean of log differences
      `24-Hour SQRT Bias`=round((mean(`SQRT Difference (15-Min - 24-Hour)`)),digits=4),           #calculate mean of SQRT differences
      `24-Hour SD`=round(sd(`Difference (15-Min - 24-Hour)`),digits = 3),                          #calculate Standard Deviation of differences
      `24-Hour log SD`=round(sd(`Log10 Difference (15-Min - 24-Hour)`),digits=4),      
      `24-Hour LOA Lower`=mean(`Difference (15-Min - 24-Hour)`)-1.96*`24-Hour SD`,                 #Calculate lower limit of agreement using normal distribution
      `24-Hour LOA Upper`=mean(`Difference (15-Min - 24-Hour)`)+1.96*`24-Hour SD`,                 #Calculate upper limit of agreement using normal distribution
      `24-Hour LOA 2.5%`=round(quantile(`24-Hour Percent Error`,.025),digits=4),                   #Calculate lower nonparametric limit of agreement
      `24-Hour LOA 97.5%`=round(quantile(`24-Hour Percent Error`,.975),digits=4),                  #Calculate upper non-paramteric limit of agreement
      `24-Hour LOA Log Lower`=round(10^(mean((`Log10 Difference (15-Min - 24-Hour)`)-1.96*`24-Hour log SD`)),digits=4),  #Calculate lower limit of agreement
      `24-Hour LOA Log Upper`=round(10^(mean((`Log10 Difference (15-Min - 24-Hour)`)+1.96*`24-Hour log SD`)),digits=4),  #Calculate lower limit of agreement
      `8-Hour Precision`=paste("+/-",as.character((`8-Hour LOA Upper`-`8-Hour LOA Lower`)/2)),     #precision 8-hour
      `24-Hour Precision`=paste("+/-",as.character((`24-Hour LOA Upper`-`24-Hour LOA Lower`)/2)),  #precision 24-hour
      `SE 8-Hour Bias`=`8-Hour SD`/sqrt(n()),                                                      #Standard Error bias 8-hour
      `SE 24-Hour Bias`=`24-Hour SD`/sqrt(n()),                                                    #Standard Error bias 24-hour
      `SE 8-Hour LOA`=sqrt(3*`8-Hour SD`/n()),                                                     #Standard Error for LOA 8-hour
      `SE 24-Hour LOA`=sqrt(3*`24-Hour SD`/n()),                                                   #Standard Error for LOA 24-hour
      `8-Hour Bias 95% Upper CI`=round(`8-Hour Bias`+2.03*`SE 8-Hour Bias`,digits=4),              #95% CI of bias 8-hour upper
      `8-Hour Bias 95% Lower CI`=round(`8-Hour Bias`-2.03*`SE 8-Hour Bias`,digits = 4),            #95% CI of bias 8-hour lower
      `24-Hour Bias 95% Upper CI`=round(`24-Hour Bias`+2.03*`SE 24-Hour Bias`,digits=4),           #95% CI of bias 24-hour upper
      `24-Hour Bias 95% Lower CI`=round(`24-Hour Bias`-2.03*`SE 24-Hour Bias`,digits=4),           #95% CI of bias 24-hour lower
      `8-Hour 95% Upper CI Top`=round(`8-Hour LOA Upper`+2.03*`SE 8-Hour LOA`,digits=4),           #95% CI of LOA Top 8-hour
      `8-Hour 95% Upper CI Bottom`=round(`8-Hour LOA Upper`-2.03*`SE 8-Hour LOA`,digits=4),        #95% CI of LOA Bottom 8-hour
      `8-Hour 95% Lower CI Top`=round(`8-Hour LOA Lower`+2.03*`SE 8-Hour LOA`,digits=4),           #95% CI of LOA Top 8-hour
      `8-Hour 95% Lower CI Bottom`=round(`8-Hour LOA Lower`-2.03*`SE 8-Hour LOA`,digits=4),        #95% CI of LOA Bottom 8-hour
      `24-Hour 95% Upper CI Top`=round(`24-Hour LOA Upper`+2.03*`SE 24-Hour LOA`,digits=4),        #95% CI of LOA Top 24-hour
      `24-Hour 95% Upper CI Bottom`=round(`24-Hour LOA Upper`-2.03*`SE 24-Hour LOA`,digits=4),     #95% CI of LOA Bottom 24-hour
      `24-Hour 95% Lower CI Top`=round(`24-Hour LOA Lower`+2.03*`SE 24-Hour LOA`,digits=4),        #95% CI of LOA Top 24-hour
      `24-Hour 95% Lower CI Bottom`=round(`24-Hour LOA Lower`-2.03*`SE 24-Hour LOA`,digits=4),     #95% CI of LOA Bottom 24-hour
      `8-Hour Bias CI`=paste(as.character(`24-Hour Bias 95% Lower CI`),"to",as.character(`8-Hour Bias 95% Upper CI`)),   #8-hour Bias CI +- 
      `24-Hour Bias CI`=paste(as.character(`24-Hour Bias 95% Lower CI`),"to",as.character(`24-Hour Bias 95% Upper CI`)), #24-hour Bias CI +- 
      `8-Hour Shapiro Wilks`=ifelse(`8-Hour SD`>0,shapiro.test(`Difference (15-Min - 8-Hour)`)$p.value,NA),              #Shapiro-Wilks test of normality 8-hour      
      `24-Hour Shapiro Wilks`=ifelse(`24-Hour SD`>0,shapiro.test(`Difference (15-Min - 24-Hour)`)$p.value,NA),            #Shapiro-Wilks test of normality 24-hour
      `8-Hour Skewness`=skewness(`Difference (15-Min - 8-Hour)`),                                  #Skewness 8-hour
      `24-Hour Skewness`=skewness(`Difference (15-Min - 24-Hour)`),                                #Skewness 24-hour
      `8-Hour Kurtosis`=kurtosis(`Difference (15-Min - 8-Hour)`),                                  #Kurtosis 8-hour
      `24-Hour Kurtosis`=kurtosis(`Difference (15-Min - 24-Hour)`),                                #Kurtosis 24-hour
      MDL=min(MDL),                                                                               #Minimum MDL
      PQL=min(PQL),                                                                               #Minimum PQL
      `Mean Uncertainty`=mean(`Immediate Sample Uncertainty`),                                     #Mean sample uncertainty
      `8-Hour Wilcox Ranked Sign`=wilcox.test(Value_15_min,Value_8_hour,paired = TRUE)$p.value,                 #Wilcoxon ranked sign test 8-hour      
      `24-Hour Wilcox Ranked Sign`=wilcox.test(Value_15_min,Value_24_hour,paired = TRUE)$p.value,               #Wilcoxon ranked sign test 24-hour
      `8-Hour Exceedences`=sum(ifelse(abs(`Difference (15-Min - 8-Hour)`) >`Immediate Sample Uncertainty`,1,0)),     #Number of difference that Exceed Uncertainty 8-hour  
      `24-Hour Exceedences`=sum(ifelse(abs(`Difference (15-Min - 24-Hour)`) >`Immediate Sample Uncertainty`,1,0)),   #Number of difference that Exceed Uncertainty 24-HOur
      `8-Hour Inside Line of Equality`=ifelse(between(0,`8-Hour Bias 95% Lower CI`,`8-Hour Bias 95% Upper CI`),"Inside","Outside"),
      `24-Hour Inside Line of Equality`=ifelse(between(0,`24-Hour Bias 95% Lower CI`,`24-Hour Bias 95% Upper CI`),"Inside","Outside"),
      `8-Hour Exceedences 20% RPD`=sum(ifelse(abs(`8-Hour Percent Error`) >20,1,0)),   #Number of difference that Exceed 8-hour 20 RPD
      `24-Hour Exceedences 20% RPD`=sum(ifelse(abs(`24-Hour Percent Error`) >20,1,0)),  #Number of difference that Exceed 20-hour 20 RPD
      ) %>%  #Line of Equality inside 95% CI 8-hour
    mutate_if(is.numeric, round,digits=4) 
  
  write.csv(LOA,file="./Data/Bias and Precision.csv",row.names=FALSE)
  
  
  
  Summary_Table <- LOA %>%
  select( TEST_NAME,`Pearson Correlation 8-hour`, `Pearson Correlation 24-hour`,`8-Hour Bias`,`8-Hour Accuracy`,`24-Hour Bias`,`24-Hour Accuracy`,`8-Hour Mean Percent Error`,`24-Hour Mean Percent Error`,
          `8-Hour Wilcox Ranked Sign`,`24-Hour Wilcox Ranked Sign`,`8-hour effect size`,`24-hour effect size`,`8-Hour r squared`,`24-Hour r squared`,
        `8-Hour LOA Lower`,`8-Hour LOA Upper`,`24-Hour LOA Lower`,`24-Hour LOA Upper`)
  
  write.csv(Summary_Table,file="./Data/Summary_Table.csv",row.names=FALSE)
  
  
  
  #calculate stats table for each analyte using non-parametric method
  Non_parametric_results <- All_results_Uncertainty  %>%
    group_by(TEST_NAME) %>%
    mutate(`8-Hour Rank`=row_number(`Difference (15-Min - 8-Hour)`)) %>% #rank differnces
    mutate(`Difference (15-Min - 8-Hour)`=ifelse(`8-Hour Rank`< 2,NA,ifelse(`8-Hour Rank`> 35,NA,`Difference (15-Min - 8-Hour)`))) %>%  #remove highest and lowest 2 values
    mutate(`24-Hour Rank`=row_number(`Difference (15-Min - 24-Hour)`)) %>%
    mutate(`Difference (15-Min - 24-Hour)`=ifelse(`24-Hour Rank`< 2,NA,ifelse(`24-Hour Rank`> 35,NA,`Difference (15-Min - 24-Hour)`))) %>%   #remove highest and lowest 2 values
    summarise(
      `Nonparametric 8-Hour LOA Lower`=min(`Difference (15-Min - 8-Hour)`,na.rm = TRUE),
      `Nonparametric 8-Hour LOA Upper`=max(`Difference (15-Min - 8-Hour)`,na.rm = TRUE),
      `Nonparametric 24-Hour LOA Lower`=min(`Difference (15-Min - 24-Hour)`,na.rm = TRUE),
      `Nonparametric 24-Hour LOA Upper`=max(`Difference (15-Min - 24-Hour)`,na.rm = TRUE),
      `Nonparameteric 8-hour Bias`=median(`Difference (15-Min - 8-Hour)`,na.rm = TRUE),
      `Nonparameteric 24-hour Bias`=median(`Difference (15-Min - 24-Hour)`,na.rm = TRUE),
      `Nonparameteric 8-Hour Precision`=paste("+/-",as.character((`Nonparametric 8-Hour LOA Upper`-`Nonparametric 8-Hour LOA Lower`)/2)),
      `Nonparameteric 24-Hour Precision`=paste("+/-",as.character((`Nonparametric 24-Hour LOA Upper`-`Nonparametric 24-Hour LOA Lower`)/2)))
  
  #join paramateric and nonparametric results
  Table_results <- LOA %>%
    left_join(Non_parametric_results, by="TEST_NAME")
  
  write.csv(Table_results,file="./Data/Table_results.csv",row.names=FALSE)
  
  #Join LOA data to sample data for figures
  All_results_LOA <- All_results_Uncertainty %>%
    left_join(Table_results,by="TEST_NAME") %>%
    gather("Preservation Time","Difference",`Difference (15-Min - 8-Hour)`,`Difference (15-Min - 24-Hour)`) 
  
  write.csv(All_results_LOA,file="./Data/All_results_LOA.csv",row.names=FALSE)
  