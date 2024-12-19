# Goal of this script is to evaluate distribution of data for the analytes in the study
#Objectives.  
#1.) Import Data
#2.) Tidy Data
#3.) Test for normality 
#4.) Create histogram and density plots


library(cowplot)




# 1.) Import Data -------------------------------------------------------------

Distribution_data <- read_csv("./Data/All_results_Uncertainty.csv")



# 2.) Tidy data ---------------------------------------------------------------

Distribution_data_Tidy <- Distribution_data %>%
mutate(TEST_NAME=ifelse(is.na(TEST_NAME),"NA",TEST_NAME))  %>%
filter(TEST_NAME != "TDSAL") %>%
select(TEST_NAME, `Difference (15-Min - 8-Hour)`,`Difference (15-Min - 24-Hour)`) 


# Table of normality tests ------------------------------------------------

Summary_table_norma_diist <- Distribution_data_Tidy %>%
group_by(TEST_NAME) %>%
summarise_all(.funs = funs(statistic = shapiro.test(.)$statistic, 
                             p.value = shapiro.test(.)$p.value))


# 4.) Create Histograms -------------------------------------------------------

#Histograms Distributions of the differences  
ggplot(gather(Distribution_data_Tidy,Difference,Value,`Difference (15-Min - 8-Hour)`,`Difference (15-Min - 24-Hour)`),aes(Value,fill=Difference))+
geom_histogram(position="identity",alpha=.65,color="grey60")+facet_wrap(~TEST_NAME,scale="free",ncol = 3)+theme_bw()+
scale_fill_manual(values=c("#66c2a5","#fc8d62"),labels=c("24-Hour Preservation", "8-Hour Preservation"))+geom_vline(xintercept = 0)+theme(legend.position="bottom")+
labs(y="Count",x="Difference from 15-Minute Preservation",title="Histogram of differences of 8 and 24-hour from 15-minute preservation by Analyte" )

ggsave("./Figures/Histogram of differences.jpeg",plot=last_plot(),height=11,width=8.5,units="in")


#Density Distributions of the differences  
ggplot(gather(Distribution_data_Tidy,Difference,Value,`Difference (15-Min - 8-Hour)`,`Difference (15-Min - 24-Hour)`),aes(Value,fill=Difference))+
geom_density(alpha=.5)+facet_wrap(~TEST_NAME,scale="free",ncol = 3)+theme_bw()+
scale_fill_manual(values=c("#66c2a5","#fc8d62"),labels=c("24-Hour Preservation", "8-Hour Preservation"))+geom_vline(xintercept = 0)+theme(legend.position="bottom")+
labs(y="Density",x="Difference from 15-Minute Preservation",title="Difference from 15-Minute Preservation by Analyte" )

#Distributions of the differences  
ggplot(gather(Distribution_data_Tidy,Difference,Value,`Difference (15-Min - 8-Hour)`,`Difference (15-Min - 24-Hour)`),aes(sample=Value,color=Difference))+
geom_qq()+geom_qq_line()+facet_wrap(~TEST_NAME,scale="free",ncol = 3)+theme_bw()+
scale_fill_manual(values=c("#66c2a5","#fc8d62"),labels=c("24-Hour Preservation", "8-Hour Preservation"))+geom_vline(xintercept = 0)+theme(legend.position="bottom")+
labs(y="Density",x="Difference from 15-Minute Preservation",title="Difference from 15-Minute Preservation by Analyte" )

