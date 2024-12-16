#This Script creates figures



# import data -------------------------------------------------------------

All_results_LOA <- read_csv("./Data/All_results_LOA.csv")

#Analytes with normal distributions
analytes_Normal_8 <- c("CA","CL","MG","NA","SIO2")         #Normal based on shapiro-wilks for 8-hour data
analytes_Normal_24 <- c("CA","DOC","K","NA","TN","TOC")    #based on shapiro-wilks for 24-hour data



# themes and format ------------------------------------------------------------------
#create Bland altman figures
options(scipen=999)
Color1 <- "#f1a340"
Color2 <- "#5ab4ac"
Color3 <- "#fed98e"
Color4 <- "#fff7bc"
Top_border <-"dark green"
Bottom_border <-"dark blue"
formula <- y~x
formula1 <- y~x  #for use in percent difference equations

# Figures -----------------------------------------------------------------

#8-Hour  Fig 1
ggplot(filter(All_results_LOA,TEST_NAME %in% analytes_Normal_8,`Preservation Time`=="Difference (15-Min - 8-Hour)"),aes(`(15-min and 8-hour)/2`,`Difference`))+
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = `8-Hour 95% Upper CI Bottom`, ymax = `8-Hour 95% Upper CI Top`), alpha = 0.3,fill=Color4)+
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = `8-Hour 95% Lower CI Bottom`, ymax = `8-Hour 95% Lower CI Top`), alpha = 0.3,fill=Color4)+  
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = `8-Hour Bias 95% Lower CI`, ymax = `8-Hour Bias 95% Upper CI`), alpha = 0.3,fill=Color3)+
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = `8-Hour 95% Upper CI Bottom`, ymax = `8-Hour 95% Upper CI Top`), alpha = 0.3,color=Top_border,fill=NA)+
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = `8-Hour 95% Lower CI Bottom`, ymax = `8-Hour 95% Lower CI Top`), alpha = 0.3,color=Bottom_border,fill=NA)+
  geom_hline(aes(yintercept = `8-Hour Bias`), colour ="black" ,size = 1,linetype="dashed")+
  geom_hline(aes(yintercept = `8-Hour LOA Lower`), colour = Color1 ,linetype ="longdash",size = 1)+
  geom_hline(aes(yintercept = `8-Hour LOA Upper`), colour = Color1 ,linetype ="longdash",size = 1)+
  geom_jitter(shape=21,size=3,height = 0.0001,width = .001 ,color="black",fill=Color2 )+
  geom_smooth(method = "lm",color="black")+stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),label.x.npc = "right", label.y.npc = 0.25, formula = formula, parse = TRUE, size = 3)+
  theme_bw()+theme(legend.position="bottom")+
  labs(title = "Differences between 15-Minute and 8-Hour Preservation Times", x="15-Minute and 8-Hour Mean (mg/l)", y="Differences 15-Minute - 8-Hour (mg/l)")+
  facet_wrap(~TEST_NAME,scales="free",nrow =3)

ggsave("Fig 1 Bland_Altman_plot_8_hour_normal.jpeg",plot=last_plot(),height=4.5,width=8,units="in")

#24-Hour  Fig 2
ggplot(filter(All_results_LOA,TEST_NAME %in% analytes_Normal_24,`Preservation Time`=="Difference (15-Min - 24-Hour)"),aes(`(15-min and 24-hour)/2`,`Difference`))+
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = `24-Hour 95% Upper CI Bottom`, ymax = `24-Hour 95% Upper CI Top`), alpha = 0.3,fill=Color4)+
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = `24-Hour 95% Lower CI Bottom`, ymax = `24-Hour 95% Lower CI Top`), alpha = 0.3,fill=Color4)+  
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = `24-Hour Bias 95% Lower CI`, ymax = `24-Hour Bias 95% Upper CI`), alpha = 0.3,fill=Color3)+
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = `24-Hour 95% Upper CI Bottom`, ymax = `24-Hour 95% Upper CI Top`), alpha = 0.3,color=Top_border,fill=NA)+
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = `24-Hour 95% Lower CI Bottom`, ymax = `24-Hour 95% Lower CI Top`), alpha = 0.3,color=Bottom_border,fill=NA)+
  geom_hline(aes(yintercept = `24-Hour Bias`), colour = "black" ,size = 1,linetype="dashed")+
  geom_hline(aes(yintercept = `24-Hour LOA Lower`), colour = Color1 ,linetype ="longdash",size = 1)+
  geom_hline(aes(yintercept = `24-Hour LOA Upper`), colour = Color1 ,linetype ="longdash",size = 1)+
  geom_jitter(shape=21,size=3,height = 0.0001,width = .001 ,color="black",fill=Color2 )+
  geom_smooth(method = "lm",color="black")+stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),label.x.npc = "right", label.y.npc = 0.25, formula = formula, parse = TRUE, size = 3)+
  theme_bw()+theme(legend.position="bottom")+labs(title = "Differences between 15-Minute and 24-Hour Preservation Times", x="15-Minute and 24-Hour Mean (mg/l)", y="Differences 15-Minute - 24-Hour (mg/l)")+
  facet_wrap(~TEST_NAME,scales="free",nrow =5)

ggsave("Fig 2 Bland_Altman_plot_24_hour_normal.jpeg",plot=last_plot(),height=4.5,width=8,units="in")

#8-hour nonparametric   Fig 3
ggplot(filter(All_results_LOA,TEST_NAME %in% analytes_nonparametric_8,`Preservation Time`=="Difference (15-Min - 8-Hour)"),aes(`(15-min and 8-hour)/2`,`Difference`))+
  geom_jitter(height = 0,shape=21,size=3,width=.001 ,color="black",fill=Color2 )+
  geom_hline(aes(yintercept = `8-Hour Median Bias`), colour ="black" ,size = 1,linetype="dashed")+
  geom_hline(aes(yintercept = `Nonparametric 8-Hour LOA Lower`), colour = Color1 ,linetype ="longdash",size = 1)+
  geom_hline(aes(yintercept = `Nonparametric 8-Hour LOA Upper`), colour = Color1 ,linetype ="longdash",size = 1)+
  #geom_smooth(method = "lm",color="black")+stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),label.x.npc = "right", label.y.npc = 0.25, formula = formula, parse = TRUE, size = 3)+
  theme_bw()+theme(legend.position="bottom")+labs(title = "Differences between 15-Minute and 8-Hour Preservation Times", x="15-Minute and 8-Hour Mean (mg/l, PCU)", y="Differences 15-Minute - 8-Hour (mg/l,PCU)")+
  facet_wrap(~TEST_NAME,scales="free",nrow =5)+scale_y_continuous(breaks = scales::pretty_breaks(n = 4) )+scale_x_continuous(breaks = scales::pretty_breaks(n = 5))

ggsave("Fig 3 Bland_Altman_plot_8_hour_nonparametric.jpeg",plot=last_plot(),height=5,width=8,units="in")

#24hour nonparametric   Fig 4
ggplot(filter(All_results_LOA,TEST_NAME %in% analytes_nonparametric_24,`Preservation Time`=="Difference (15-Min - 24-Hour)"),aes(`(15-min and 24-hour)/2`,`Difference`))+
  geom_jitter(shape=21,size=3,height = 0,width = .001 ,color="black",fill=Color2 )+
  geom_hline(aes(yintercept = `24-Hour Median Bias`), colour = "black" ,size = 1,linetype="dashed")+
  geom_hline(aes(yintercept = `Nonparametric 24-Hour LOA Lower`), colour = Color1 ,linetype ="longdash",size = 1)+
  geom_hline(aes(yintercept = `Nonparametric 24-Hour LOA Upper`), colour = Color1 ,linetype ="longdash",size = 1)+
  #geom_smooth(method = "lm",color="black")+stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),label.x.npc = "right", label.y.npc = 0.25, formula = formula, parse = TRUE, size = 3)+
  theme_bw()+theme(legend.position="bottom")+labs(title = "Differences between 15-Minute and 24-Hour Preservation Times", x="15-Minute and 24-Hour Mean (mg/l, PCU)", y="Differences 15-Minute - 24-Hour (mg/l,PCU)")+
  facet_wrap(~TEST_NAME,scales="free",ncol =3)+scale_y_continuous(breaks = scales::pretty_breaks(n = 4))+scale_x_continuous(breaks = scales::pretty_breaks(n = 5))

ggsave("Fig 4 Bland_Altman_plot_24_hour_nonparametric.jpeg",plot=last_plot(),height=5,width=8,units="in")

#8-hour nonparametric Percent Error   Fig 5
ggplot(filter(All_results_LOA,TEST_NAME %in% analytes_non_constant_variance_8, `Preservation Time`=="Difference (15-Min - 8-Hour)"),aes(`(15-min and 8-hour)/2`,(`8-Hour Percent Error`)))+
  geom_jitter(shape=21,size=2,height=0, width =.001,color="black",fill=Color2 )+
  geom_hline(aes(yintercept = `8-Hour Median Percent Error`), colour = "black" ,size = 1,linetype="dashed")+
  geom_hline(aes(yintercept = `8-Hour LOA 2.5%`), colour = Color1 ,linetype ="longdash",size = 1)+
  geom_hline(aes(yintercept = `8-Hour LOA 97.5%`), colour = Color1 ,linetype ="longdash",size = 1)+
  #geom_smooth(method = "lm",color="black")+stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),label.x.npc = "right", label.y.npc = 0.25, formula = formula, parse = TRUE, size = 3)+
  theme_bw()+theme(legend.position="bottom")+labs(title = "Percent Differences between 15-Minute \nand 8-Hour Preservation Times", x="15-Minute and 8-Hour Mean (mg/l)", y="Percent Difference (%)")+
  facet_wrap(~TEST_NAME,scales="free",nrow =2)+scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("Fig 5 Bland_Altman_plot_8_hour_nonparametric.jpeg",plot=last_plot(),height=4,width=4,units="in")

#24hour nonparametric  percent error  Fig 6
ggplot(filter(All_results_LOA,TEST_NAME %in%  analytes_non_constant_variance_24,`Preservation Time`=="Difference (15-Min - 24-Hour)"),aes(`(15-min and 24-hour)/2`,(`24-Hour Percent Error`)))+
  geom_jitter(shape=21,size=3,height=0, width =.00 ,color="black",fill=Color2 )+
  geom_hline(aes(yintercept = `24-Hour Median Percent Error`), colour = "black" ,size = 1,linetype="dashed")+
  geom_hline(aes(yintercept = `24-Hour LOA 97.5%`), colour = Color1 ,linetype ="longdash",size = 1)+
  geom_hline(aes(yintercept = `24-Hour LOA 2.5%`), colour = Color1 ,linetype ="longdash",size = 1)+
  #geom_smooth(method = "lm",color="black")+stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),label.x.npc = "right", label.y.npc = 0.25, formula = formula, parse = TRUE, size = 3)+
  theme_bw()+theme(legend.position="bottom")+labs(title = "Percent Differences between 15-Minute \nand 24-Hour Preservation Times", x="15-Minute and 24-Hour Mean (mg/l)", y="Percent Difference (%)")+
  facet_wrap(~TEST_NAME,scales="free",nrow =2)+scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+scale_x_continuous(breaks = scales::pretty_breaks(n = 5))

ggsave("Fig 6 Bland_Altman_plot_24_hour_nonparametric.jpeg",plot=last_plot(),height=4,width=4,units="in")

#Fig7 8-hour log transformed differences
ggplot(filter(All_results_LOA,TEST_NAME %in%  analytes_non_constant_variance_8,`Preservation Time`=="Difference (15-Min - 8-Hour)"),aes(`(15-min and 8-hour)/2`*1000,10^`Log10 Difference (15-Min - 8-Hour)`))+
  geom_jitter(shape=21,size=3,height=.0, width =.5 ,color="black",fill=Color2 )+
  geom_hline(aes(yintercept = `8-Hour Mean log Bias`), colour = "black" ,size = 1,linetype="dashed")+
  geom_hline(aes(yintercept = `8-Hour LOA Log Upper`), colour = Color1 ,linetype ="longdash",size = 1)+
  geom_hline(aes(yintercept = `8-Hour LOA Log Lower`), colour = Color1 ,linetype ="longdash",size = 1)+
  geom_smooth(method = "lm",color="black")+stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),label.x.npc = "right", label.y.npc = 0.25, formula = formula, parse = TRUE, size = 3)+
  theme_bw()+theme(legend.position="bottom")+labs(title = "Log Transformed Differences between 15-Minute and 8-Hour Preservation Times", x="15-Minute and 24-Hour Mean (mg/l)", y="Difference (log 15-Minute - log 8-Hour)")+
  facet_wrap(~TEST_NAME,scales="free",nrow =2)+scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+scale_x_continuous(breaks = scales::pretty_breaks(n = 5))

#Fig8 24-hourlog transformed differences
ggplot(filter(All_results_LOA_24_hour,TEST_NAME %in%  analytes_non_constant_variance_24),aes(`(15-min and 24-hour)/2`*1000,10^`Log10 Difference (15-Min - 24-Hour)`))+
  geom_jitter(shape=21,size=3,height=.0, width =.5 ,color="black",fill=Color2 )+
  geom_hline(aes(yintercept = `24-Hour Mean log Bias`), colour = "black" ,size = 1,linetype="dashed")+
  geom_hline(aes(yintercept = `24-Hour LOA Log Upper`), colour = Color1 ,linetype ="longdash",size = 1)+
  geom_hline(aes(yintercept = `24-Hour LOA Log Lower`), colour = Color1 ,linetype ="longdash",size = 1)+
  geom_smooth(method = "lm",color="black")+stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),label.x.npc = "right", label.y.npc = 0.25, formula = formula, parse = TRUE, size = 3)+
  theme_bw()+theme(legend.position="bottom")+labs(title = "Log Transformed Differences between 15-Minute and 24-Hour Preservation Times", x="15-Minute and 24-Hour Mean (mg/l)", y="Difference (log 15-Minute - log 8-Hour)")+
  facet_wrap(~TEST_NAME,scales="free",nrow =2)+scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+scale_x_continuous(breaks = scales::pretty_breaks(n = 5))

#Fig 9  8-hour SQRT transformed differences
ggplot(filter(All_results_LOA_24_hour,TEST_NAME %in%  analytes_non_constant_variance_8),aes(`(15-min and 8-hour)/2`,`SQRT Difference (15-Min - 8-Hour)`))+
  geom_jitter(shape=21,size=3,height=.0, width =.001 ,color="black",fill=Color2 )+
  geom_hline(aes(yintercept = `8-Hour SQRT Bias`), colour = "black" ,size = 1,linetype="dashed")+
  geom_hline(aes(yintercept = `8-Hour LOA SQRT Lower`), colour = Color1 ,linetype ="longdash",size = 1)+
  geom_hline(aes(yintercept = `8-Hour LOA SQRT Upper`), colour = Color1 ,linetype ="longdash",size = 1)+
  geom_smooth(method = "lm",color="black")+stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),label.x.npc = "right", label.y.npc = 0.25, formula = formula, parse = TRUE, size = 3)+
  theme_bw()+theme(legend.position="bottom")+labs(title = "Square Root Transformed Differences between 15-Minute and 24-Hour Preservation Times", x="15-Minute and 24-Hour Mean (mg/l)", y="Difference (SQRT 15-Minute - SQRT 8-Hour)")+
  facet_wrap(~TEST_NAME,scales="free",nrow =2)+scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+scale_x_continuous(breaks = scales::pretty_breaks(n = 5))


#Fig 10  24-hour SQRT transformed differences
ggplot(filter(All_results_LOA_24_hour,TEST_NAME %in%  analytes_non_constant_variance_24,`Preservation Time`=="Difference (15-Min - 24-Hour)"),aes(`(15-min and 24-hour)/2`,`SQRT Difference (15-Min - 24-Hour)`^2))+
  geom_jitter(shape=21,size=3,height=.0, width =.001 ,color="black",fill=Color2 )+
  geom_hline(aes(yintercept = `24-Hour SQRT Bias`), colour = "black" ,size = 1,linetype="dashed")+
  #geom_hline(aes(yintercept = `24-Hour LOA Log Upper`), colour = Color1 ,linetype ="longdash",size = 1)+
  #geom_hline(aes(yintercept = `24-Hour LOA Log Lower`), colour = Color1 ,linetype ="longdash",size = 1)+
  geom_smooth(method = "lm",color="black")+stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),label.x.npc = "right", label.y.npc = 0.25, formula = formula, parse = TRUE, size = 3)+
  theme_bw()+theme(legend.position="bottom")+labs(title = "Square Root Transformed Differences between 15-Minute and 24-Hour Preservation Times", x="15-Minute and 24-Hour Mean (mg/l)", y="Difference (log 15-Minute - log 8-Hour)")+
  facet_wrap(~TEST_NAME,scales="free",nrow =2)+scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+scale_x_continuous(breaks = scales::pretty_breaks(n = 5))


#Fig 11  8-hour Cubic transformed differences
ggplot(filter(All_results_LOA_24_hour,TEST_NAME %in%  analytes_non_constant_variance_8),aes(`(15-min and 8-hour)/2`,`Cubic Root Difference (15-Min - 8-Hour)`^3))+
  geom_jitter(shape=21,size=3,height=.0, width =.001 ,color="black",fill=Color2 )+
  geom_hline(aes(yintercept = `8-Hour Cubic Root Bias`^3), colour = "black" ,size = 1,linetype="dashed")+
  geom_hline(aes(yintercept = `8-Hour LOA Cubic Root Lower`^3), colour = Color1 ,linetype ="longdash",size = 1)+
  geom_hline(aes(yintercept = `8-Hour LOA Cubic Root Upper`^3), colour = Color1 ,linetype ="longdash",size = 1)+
  geom_smooth(method = "lm",color="black")+stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),label.x.npc = "right", label.y.npc = 0.25, formula = formula, parse = TRUE, size = 3)+
  theme_bw()+theme(legend.position="bottom")+labs(title = "Cubic Root Transformed Differences between 15-Minute and 24-Hour Preservation Times", x="15-Minute and 24-Hour Mean (mg/l)", y="Difference (SQRT 15-Minute - SQRT 8-Hour)")+
  facet_wrap(~TEST_NAME,scales="free",nrow =2)+scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+scale_x_continuous(breaks = scales::pretty_breaks(n = 5))

#Range of LOA 8-hour vs 24-Hour
LOA_table_final$Analyte  = factor(LOA_table_final$Analyte, levels=c("CL","TDPO4","NA","CA","SO4","COLOR","DOC","TOC","NOX","MG","K","SIO2","TN","TDN","NH4","TDSFE","TPO4","NO2","OPO4") )
large_LOA <- c("CL","NA","CA","SO4","COLOR","DOC","TOC","MG","K","SIO2")
small_LOA <- c("TN","TDN","NH4")
very_small_LOA <- c("TDSFE","TPO4","NO2","OPO4")
percentage_LOA <- c("TDPO4","NOX")
ggplot(filter(LOA_table_final,Analyte %in% large_LOA))+
  geom_crossbar(aes(x =Analyte, y=Bias, ymin = `Lower Limit of Agreement`, ymax = `Upper Limit of Agreement`,colour=`Preservation Time`),position = position_dodge(width  =.95),size=1)+
  geom_hline(yintercept = 0, colour = "black" ,linetype ="longdash",size = .75)+
  theme_bw()+theme(legend.position="bottom")+labs(title = "Limits of Agreement by Analyte and Preservation Time", x="Analyte", y="LOA Range (mg/l)")+
  scale_colour_manual(values=c(Color1,Color2),name="")+scale_y_continuous(breaks = scales::pretty_breaks(n = 14))

ggsave("Limits of Agreement by Analyte and Preservation Time - Large Differences.jpeg",plot=last_plot(),height=4,width=8,units="in")

ggplot(filter(LOA_table_final,Analyte %in% small_LOA))+
  geom_crossbar(aes(x =Analyte, y=Bias, ymin = `Lower Limit of Agreement`, ymax = `Upper Limit of Agreement`,colour=`Preservation Time`),position = position_dodge(width  =.95),size=1)+
  geom_hline(yintercept = 0, colour = "black" ,linetype ="longdash",size = .75)+
  theme_bw()+theme(legend.position="bottom")+labs(title = "Limits of Agreement by Analyte\nand Preservation Time", x="Analyte", y="LOA Range (mg/l)")+
  scale_colour_manual(values=c(Color1,Color2),name="")+scale_y_continuous(breaks = scales::pretty_breaks(n = 14))

ggsave("Limits of Agreement by Analyte and Preservation Time - Small Differences.jpeg",plot=last_plot(),height=4,width=4,units="in")

ggplot(filter(LOA_table_final,Analyte %in% very_small_LOA))+
  geom_crossbar(aes(x =Analyte, y=Bias, ymin = `Lower Limit of Agreement`, ymax = `Upper Limit of Agreement`,colour=`Preservation Time`),position = position_dodge(width  =.95),size=1)+
  geom_hline(yintercept = 0, colour = "black" ,linetype ="longdash",size = 1)+
  theme_bw()+theme(legend.position="bottom")+labs(title = "Limits of Agreement by Analyte\nand Preservation Time", x="Analyte", y="LOA Range (mg/l)")+
  scale_colour_manual(values=c(Color1,Color2),name="")+scale_y_continuous(breaks = scales::pretty_breaks(n = 8))

ggsave("Limits of Agreement by Analyte and Preservation Time - Very Small Differences.jpeg",plot=last_plot(),height=4,width=4,units="in")

ggplot(filter(LOA_table_final,Analyte %in% percentage_LOA))+
  geom_crossbar(aes(x =Analyte, y=Bias, ymin = `Lower Limit of Agreement`, ymax = `Upper Limit of Agreement`,colour=`Preservation Time`),position = position_dodge(width  =.95),size=1)+
  geom_hline(yintercept = 0, colour = "black" ,linetype ="longdash",size = 1)+
  theme_bw()+labs(title = "Limits of Agreement by Analyte\nand Preservation Time", x="Analyte", y="LOA Range (%)",legend = "")+theme(legend.position="bottom")+
  scale_colour_manual(values=c(Color1,Color2),name="")+scale_y_continuous(breaks = scales::pretty_breaks(n = 8),labels = scales::percent)

ggsave("Limits of Agreement by Analyte and Preservation Time - Percent Differences.jpeg",plot=last_plot(),height=4,width=3.5,units="in")

#figure example of non constant variance TDPO4 example 
ggplot(filter(All_results_LOA_24_hour,TEST_NAME %in% c("TDPO4")),aes(`(15-min and 24-hour)/2`,`Difference`))+
  geom_hline(aes(yintercept = `24-Hour Bias`), colour = "black" ,size = 1,linetype="dashed")+
  geom_hline(aes(yintercept = `24-Hour LOA Lower`), colour = Color1 ,linetype ="longdash",size = 1)+
  geom_hline(aes(yintercept = `24-Hour LOA Upper`), colour = Color1 ,linetype ="longdash",size = 1)+
  geom_jitter(shape=21,size=3,height = 0.0001,width = .001 ,color="black",fill=Color2 )+
  #geom_smooth(method = "lm",color="black",se=FALSE)+stat_poly_eq(aes(label = paste( ..rr.label.., sep = "~~~")),label.x.npc = "right", label.y.npc = 0.35, formula = formula, parse = TRUE, size = 3)+
  theme_bw()+theme(legend.position="bottom")+labs(title = "Total Dissolved Phosphorus:\nAbsolute Difference Plot", x="15-Minute and 24-Hour Mean (mg/l)", y="Differences 15-Minute - 8-Hour (mg/l)")+
  facet_wrap(~TEST_NAME,scales="free",nrow =5)+scale_x_continuous(breaks = scales::pretty_breaks(n = 5))

ggsave("Example of non constant variance.jpeg",plot=last_plot(),height=4,width=4.5,units="in")

#Percent Error  TDPO4 example 
ggplot(filter(All_results_LOA_24_hour,TEST_NAME %in%  c("TDPO4")),aes(`(15-min and 24-hour)/2`,(`24-Hour Percent Error`)))+
  geom_jitter(shape=21,size=3,height=0.5, width =.001 ,color="black",fill=Color2 )+
  geom_hline(aes(yintercept = `24-Hour Median Percent Error`), colour = "black" ,size = 1,linetype="dashed")+
  geom_hline(aes(yintercept = `24-Hour LOA 97.5%`), colour = Color1 ,linetype ="longdash",size = 1)+
  geom_hline(aes(yintercept = `24-Hour LOA 2.5%`), colour = Color1 ,linetype ="longdash",size = 1)+
  #geom_smooth(method = "lm",color="black",se=FALSE)+stat_poly_eq(aes(label = paste( ..rr.label.., sep = "~~~")),label.x.npc = "right", label.y.npc = 0.25, formula = formula, parse = TRUE, size = 3)+
  theme_bw()+theme(legend.position="bottom")+labs(title = "Total Dissolved Phosphorus:\nPercent Difference Plot", x="15-Minute and 24-Hour Mean (mg/l)", y="Percent Difference (%)")+
  facet_wrap(~TEST_NAME,scales="free",nrow =2)+scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+scale_x_continuous(breaks = scales::pretty_breaks(n = 5))

ggsave("Example of non constant variance percent differnces TDPO4.jpeg",plot=last_plot(),height=4,width=4.5,units="in")

#Example of correlations 
ggplot(filter(All_results_LOA, TEST_NAME %in% c("DOC","CL")),aes(Value_15_min,Value_8_hour,color=TEST_NAME))+
  geom_point(size=1,shape=19,alpha=.5)+
  geom_smooth(method = "lm",se=FALSE,size=.5)+scale_color_manual(values = c(Color1,Color2),labels=c("Chloride","Dissolved Organic Carbon"))+  
  stat_poly_eq(aes(label = paste( ..rr.label.., sep = "~~~")),label.x.npc = "right", label.y.npc = 0.25, formula = y~x, parse = TRUE, size = 3.5)+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(title = "Scatter Plot of Chloride and Dissolved\nOrganic Carbon with Correlations", x="15-Minute Presrevation Time (mg/l)", y="8-Hour Preservation Time (mg/l)",color="Analyte")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+scale_x_continuous(breaks = scales::pretty_breaks(n = 10))  

ggsave("Example of correlations.jpeg",plot=last_plot(),height=4,width=4.5,units="in")

#LOA vs MDL and max
LOA_table_final_MDL <-LOA_table_final %>%
  rename("TEST_NAME"=`Analyte`) %>%
  left_join(LOA,by="TEST_NAME") %>%
  mutate(`Label`=paste(`TEST_NAME`," ",`Preservation Time`))

ggplot(filter(LOA_table_final_MDL,TEST_NAME %in% c("CL","TDSFE","NA","CA","SO4","COLOR","DOC","TOC","MG","K","SIO2","TN","TDN","NH4","TPO4","NO2","OPO4")),aes(MDL,`Upper Limit of Agreement`+abs(`Lower Limit of Agreement`),color=TEST_NAME,label=Label))+
  geom_point(alpha=.5,size=2)+geom_label_repel()+
  theme_bw()+theme(legend.position="none")+scale_x_log10(breaks= c(0,0.002,.005,.01,.02,0.05,.1,.25,.5,1.0))+scale_y_log10()+
  labs(title = "Method Detection Limit Versus the Limits of Agreement by Analyte", x="MDL ", y="LOA Interval")

ggsave("Method Detection Limit Versus the Limits of Agreement by Analyte.jpeg",plot=last_plot(),height=7,width=7.5,units="in")


ggplot(LOA_table_final_MDL,aes(`8-Hour Max`,`Upper Limit of Agreement`+abs(`Lower Limit of Agreement`),color=TEST_NAME,label=Label))+geom_point()+
  theme_bw()+theme(legend.position="bottom")+scale_y_log10()+scale_x_log10()+geom_label_repel()




#QQ Plots
ggplot(All_results_LOA,aes(sample=`Difference`,color=`Preservation Time`))+stat_qq_point()+stat_qq_line()+
  facet_wrap(~TEST_NAME,scales = "free")+theme_bw()+scale_color_manual(values=c("#66c2a5","#fc8d62"),labels=c("24-Hour Preservation", "8-Hour Preservation"))

ggsave("QQplot.jpeg",plot=last_plot(),height=11,width=8,units="in")

#QQ Plots Log transformed
ggplot(gather(All_results_LOA,"Preservation Time","Difference",`Log10 Difference (15-Min - 8-Hour)`,`Log10 Difference (15-Min - 24-Hour)`),aes(sample=`Difference`,color=`Preservation Time`,fill=`Preservation Time`))+stat_qq_point()+stat_qq_line()+stat_qq_band(alpha=.3)+
  facet_wrap(~TEST_NAME,scales = "free")+theme_light()

ggsave("QQplot Log10 Transformed Data.jpeg",plot=last_plot(),height=11,width=8,units="in")

ggplot(All_results_LOA,aes(`Difference`,color=`Preservation Time`,fill=`Preservation Time`))+geom_histogram(alpha=.5,position="identity")+
  facet_wrap(~TEST_NAME,scales="free")+
  geom_freqpoly()

#Distributions of the differences  
ggplot(gather(All_results_Uncertainty,Difference,Value,`Difference (15-Min - 8-Hour)`,`Difference (15-Min - 24-Hour)`),aes(Value,fill=Difference))+
  geom_density(alpha=.5)+facet_wrap(~TEST_NAME,scale="free",ncol = 3)+theme_bw()+
  scale_fill_manual(values=c("#66c2a5","#fc8d62"),labels=c("24-Hour Preservation", "8-Hour Preservation"))+geom_vline(xintercept = 0)+theme(legend.position="bottom")+
  labs(y="Density",x="Difference from 15-Minute Preservation",title="Difference from 15-Minute Preservation by Analyte" )

#Distributions of the differences  
ggplot(gather(All_results_Uncertainty,Difference,Value,`Difference (15-Min - 8-Hour)`,`Difference (15-Min - 24-Hour)`),aes(Value,fill=Difference))+
  geom_histogram(alpha=.7,bins=15)+facet_wrap(~TEST_NAME,scale="free",ncol = 3)+theme_bw()+
  scale_fill_manual(values=c("#66c2a5","#fc8d62"),labels=c("24-Hour Preservation", "8-Hour Preservation"))+geom_vline(xintercept = 0)+theme(legend.position="bottom")+
  labs(y="Density",x="Difference from 15-Minute Preservation",title="Difference from 15-Minute Preservation by Analyte" )

ggsave("Histogram.jpeg",plot=last_plot(),height=11,width=8,units="in")

#Distributions of the log transformed differences  
ggplot(gather(All_results_Uncertainty,Difference,Value,`Log10 Difference (15-Min - 8-Hour)`,`Log10 Difference (15-Min - 24-Hour)`),aes(Value,fill=Difference))+
  geom_histogram()+geom_density(alpha=.5)+facet_wrap(~TEST_NAME,scale="free",ncol = 3)+theme_bw()+
  scale_fill_manual(values=c("#66c2a5","#fc8d62"),labels=c("24-Hour Preservation", "8-Hour Preservation"))+geom_vline(xintercept = 1)+theme(legend.position="bottom")+
  labs(y="Density",x="Difference from 15-Minute Preservation",title="Difference from 15-Minute Preservation by Analyte" )

#Distributions of the percent differences  
ggplot(gather(All_results_Uncertainty,Difference,Value,`8-Hour Percent Error`,`24-Hour Percent Error`),aes(Value,fill=Difference))+
  geom_histogram(alpha=.7,bins=15)+geom_density(alpha=.5)+facet_wrap(~TEST_NAME,scale="free",ncol = 3)+theme_bw()+
  scale_fill_manual(values=c("#66c2a5","#fc8d62"),labels=c("24-Hour Preservation", "8-Hour Preservation"))+geom_vline(xintercept = 0)+theme(legend.position="bottom")+
  labs(y="Density",x="Percent Difference from 15-Minute Preservation",title="Percent Difference from 15-Minute Preservation by Analyte" )

#histograms
#NO2 non-Normal
ggplot(filter(All_results_LOA,TEST_NAME=="NO2"),aes(`Difference`,color=`Preservation Time`,fill=`Preservation Time`))+geom_histogram(binwidth = .001,alpha=.5,position="identity")+
  geom_freqpoly(binwidth = .001)

#NOX non-normal
ggplot(filter(All_results_LOA,TEST_NAME=="NOX"),aes(`Difference`,color=`Preservation Time`,fill=`Preservation Time`))+geom_histogram(binwidth = .001,alpha=.5,position="identity")+
  geom_freqpoly(binwidth = .001)+theme_light()

#OPO4 non-normal
ggplot(filter(All_results_LOA,TEST_NAME=="OPO4"),aes(`Difference`,color=`Preservation Time`,fill=`Preservation Time`))+geom_histogram(binwidth = .0007,alpha=.5,position="identity")+
  geom_freqpoly(binwidth = .001)+theme_light()

#So4 non-normal
ggplot(filter(All_results_LOA,TEST_NAME=="SO4"),aes(`Difference`,color=`Preservation Time`,fill=`Preservation Time`))+geom_histogram(binwidth = .2,alpha=.5,position="identity")+
  geom_freqpoly(binwidth = .2)+theme_light()

#TPO4 normal
ggplot(filter(All_results_LOA,TEST_NAME=="TPO4"),aes(`Difference`,color=`Preservation Time`,fill=`Preservation Time`))+geom_histogram(binwidth = .001,alpha=.5,position="identity")+
  geom_freqpoly(binwidth = .001)+theme_light()

#TDSAL normal
ggplot(filter(All_results_LOA,TEST_NAME=="TDSAL"),aes(`Difference`,color=`Preservation Time`,fill=`Preservation Time`))+geom_histogram(binwidth = .75,alpha=.5,position="identity")+
  geom_freqpoly(binwidth = .75)+theme_light()

#TDSFE normal
ggplot(filter(All_results_LOA,TEST_NAME=="TDSFE"),aes(`Difference`,color=`Preservation Time`,fill=`Preservation Time`))+geom_histogram(binwidth = .75,alpha=.5,position="identity")+
  geom_freqpoly(binwidth = .75)+theme_light()



