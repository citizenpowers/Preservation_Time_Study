# Goal of this script is to create Blabd-Altman Plots for the analytes in the study
#Objectives.  
#1.) Define plot style
#2.) Import Data
#3.) Tidy Data
#3.) Create plots


library(cowplot)


# 1.) Style ---------------------------------------------------------------

options(scipen=999)
Color1 <- "#f1a340"
Color2 <- "#5ab4ac"
Color3 <- "#fed98e"
Color4 <- "#fff7bc"
Top_border <-"dark green"
Bottom_border <-"dark blue"
formula <- y~x


# 2.) Import Data -------------------------------------------------------------

BA_Plot_data <- read_csv("./Data/All_results_Uncertainty.csv")



# 3.) Tidy data ---------------------------------------------------------------

BA_Plot_Tidy <- BA_Plot_data %>%
select(TEST_NAME,Value_8_hour,Value_15_min,Value_24_hour,  `Difference (15-Min - 8-Hour)`,`Difference (15-Min - 24-Hour)`) %>%
mutate(TEST_NAME=ifelse(is.na(TEST_NAME),"Sodium",TEST_NAME))  


# Figures -----------------------------------------------------------------

#BA plot for Ammonia 8-hour
Ammonia_8_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="NH4") ,aes((Value_8_hour+Value_15_min)/2,`Difference (15-Min - 8-Hour)`))+
geom_point(shape=21,size=3 ,color="black",fill=Color2)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 8-Hour)`)+1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)-1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(6))+
labs(title="Ammonia: 8-Hour ",x=expression("Mean 15-Minute and 8-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 8-Hour ("~mg~L^-1*")"))

#BA plot for Ammonia 24-hour
Ammonia_24_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="NH4") ,aes((Value_24_hour+Value_15_min)/2,`Difference (15-Min - 24-Hour)`))+
geom_point(shape=21,size=3 ,color="black",fill=Color2)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 24-Hour)`)+1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)-1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(7))+
labs(title="Ammonia: 24-Hour ",x=expression("Mean 15-Minute and 24-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 24-Hour ("~mg~L^-1*")"))

Ammonia_plot <-plot_grid(Ammonia_8_plot, Ammonia_24_plot, labels = c('A', 'B'), label_size = 12)

ggsave("./Figures/BA_Plots/Bland_Altman_Ammonia.jpeg",plot=last_plot(),height=3.5,width=8,units="in")

#BA plot for Calcium 8-hour
ca_8_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="CA") ,aes((Value_8_hour+Value_15_min)/2,`Difference (15-Min - 8-Hour)`))+
geom_point(shape=21,size=3 ,color="black",fill=Color2)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 8-Hour)`)+1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)-1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(6))+
labs(title="Calcium: 8-Hour ",x=expression("Mean 15-Minute and 8-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 8-Hour ("~mg~L^-1*")"))

#BA plot for Calcium 24-hour
ca_24_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="CA") ,aes((Value_24_hour+Value_15_min)/2,`Difference (15-Min - 24-Hour)`))+
geom_point(shape=21,size=3 ,color="black",fill=Color2)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 24-Hour)`)+1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)-1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(7))+
labs(title="Calcium: 24-Hour ",x=expression("Mean 15-Minute and 24-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 24-Hour ("~mg~L^-1*")"))

ca_plot <-plot_grid(ca_8_plot, ca_24_plot, labels = c('A', 'B'), label_size = 12)

ggsave("./Figures/BA_Plots/Bland_Altman_calcium.jpeg",plot=last_plot(),height=3.5,width=8,units="in")

#BA plot for Chloride 8-hour
cl_8_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="CL") ,aes((Value_8_hour+Value_15_min)/2,`Difference (15-Min - 8-Hour)`))+
geom_point(shape=21,size=3 ,color="black",fill=Color2)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 8-Hour)`)+1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)-1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(6))+
labs(title="Chloride: 8-Hour ",x=expression("Mean 15-Minute and 8-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 8-Hour ("~mg~L^-1*")"))

#BA plot for Calcium 24-hour
cl_24_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="CL") ,aes((Value_24_hour+Value_15_min)/2,`Difference (15-Min - 24-Hour)`))+
geom_point(shape=21,size=3 ,color="black",fill=Color2)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 24-Hour)`)+1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)-1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(7))+
labs(title="Chloride: 24-Hour ",x=expression("Mean 15-Minute and 24-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 24-Hour ("~mg~L^-1*")"))

cl_plot <-plot_grid(cl_8_plot, cl_24_plot, labels = c('A', 'B'), label_size = 12)

ggsave("./Figures/BA_Plots/Bland_Altman_chloride.jpeg",plot=last_plot(),height=3.5,width=8,units="in")

#BA plot for Color 8-hour
color_8_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="COLOR") ,aes((Value_8_hour+Value_15_min)/2,`Difference (15-Min - 8-Hour)`))+
geom_point(shape=21,size=3 ,color="black",fill=Color2)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 8-Hour)`)+1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)-1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(6))+
labs(title="Color: 8-Hour ",x=expression("Mean 15-Minute and 8-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 8-Hour ("~mg~L^-1*")"))

#BA plot for Color 24-hour
color_24_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="COLOR") ,aes((Value_24_hour+Value_15_min)/2,`Difference (15-Min - 24-Hour)`))+
geom_point(shape=21,size=3 ,color="black",fill=Color2)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 24-Hour)`)+1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)-1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(7))+
labs(title="Color: 24-Hour ",x=expression("Mean 15-Minute and 24-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 24-Hour ("~mg~L^-1*")"))

color_plot <-plot_grid(color_8_plot, color_24_plot, labels = c('A', 'B'), label_size = 12)

ggsave("./Figures/BA_Plots/Bland_Altman_color.jpeg",plot=last_plot(),height=3.5,width=8,units="in")

#BA plot for DOC 8-hour
DOC_8_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="DOC") ,aes((Value_8_hour+Value_15_min)/2,`Difference (15-Min - 8-Hour)`))+
geom_point(shape=21,size=3 ,color="black",fill=Color2)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 8-Hour)`)+1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)-1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(6))+
labs(title="Dissolved Organic Carbon: 8-Hour ",x=expression("Mean 15-Minute and 8-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 8-Hour ("~mg~L^-1*")"))

#BA plot for DOC 24-hour
DOC_24_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="DOC") ,aes((Value_24_hour+Value_15_min)/2,`Difference (15-Min - 24-Hour)`))+
geom_point(shape=21,size=3 ,color="black",fill=Color2)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 24-Hour)`)+1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)-1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(7))+
labs(title="Dissolved Organic Carbon: 24-Hour ",x=expression("Mean 15-Minute and 24-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 24-Hour ("~mg~L^-1*")"))

DOC_plot <-plot_grid(DOC_8_plot, DOC_24_plot, labels = c('A', 'B'), label_size = 12)

ggsave("./Figures/BA_Plots/Bland_Altman_DOC.jpeg",plot=last_plot(),height=3.5,width=8,units="in")

#BA plot for Magnesium 8-hour
MG_8_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="MG") ,aes((Value_8_hour+Value_15_min)/2,`Difference (15-Min - 8-Hour)`))+
geom_point(shape=21,size=3 ,color="black",fill=Color2)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 8-Hour)`)+1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)-1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(6))+
labs(title="Magnesium: 8-Hour ",x=expression("Mean 15-Minute and 8-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 8-Hour ("~mg~L^-1*")"))

#BA plot for Magnesium 24-hour
MG_24_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="MG") ,aes((Value_24_hour+Value_15_min)/2,`Difference (15-Min - 24-Hour)`))+
geom_point(shape=21,size=3 ,color="black",fill=Color2)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 24-Hour)`)+1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)-1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(7))+
labs(title="Magnesium: 24-Hour ",x=expression("Mean 15-Minute and 24-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 24-Hour ("~mg~L^-1*")"))

MG_plot <-plot_grid(MG_8_plot, MG_24_plot, labels = c('A', 'B'), label_size = 12)

ggsave("./Figures/BA_Plots/Bland_Altman_MG.jpeg",plot=last_plot(),height=3.5,width=8,units="in")

#BA plot for Nitrite 8-hour
NO2_8_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="NO2") ,aes((Value_8_hour+Value_15_min)/2,`Difference (15-Min - 8-Hour)`))+
geom_jitter(shape=21,size=3 ,color="black",fill=Color2,width=0.00015,height=0.00015)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 8-Hour)`)+1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)-1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(6))+
labs(title="Nitrite: 8-Hour ",x=expression("Mean 15-Minute and 8-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 8-Hour ("~mg~L^-1*")"))

#BA plot for Nitrite 24-hour
NO2_24_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="NO2") ,aes((Value_24_hour+Value_15_min)/2,`Difference (15-Min - 24-Hour)`))+
geom_jitter(shape=21,size=3 ,color="black",fill=Color2,width=0.00015,height=0.00015)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 24-Hour)`)+1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)-1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(6))+
labs(title="Nitrite: 24-Hour ",x=expression("Mean 15-Minute and 24-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 24-Hour ("~mg~L^-1*")"))

NO2_plot <-plot_grid(NO2_8_plot, NO2_24_plot, labels = c('A', 'B'), label_size = 12)

ggsave("./Figures/BA_Plots/Bland_Altman_NO2.jpeg",plot=last_plot(),height=3.5,width=8,units="in")

#BA plot for NOx 8-hour
NOX_8_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="NOX") ,aes((Value_8_hour+Value_15_min)/2,`Difference (15-Min - 8-Hour)`))+
geom_jitter(shape=21,size=3 ,color="black",fill=Color2,width=0.0001,height=0.0001)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 8-Hour)`)+1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)-1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(10))+scale_x_continuous(breaks=pretty_breaks(5))+
labs(title="Nitrate + Nitrite: 8-Hour ",x=expression("Mean 15-Minute and 8-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 8-Hour ("~mg~L^-1*")"))

#BA plot for NOx 24-hour
NOX_24_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="NOX") ,aes((Value_24_hour+Value_15_min)/2,`Difference (15-Min - 24-Hour)`))+
geom_jitter(shape=21,size=3 ,color="black",fill=Color2,width=0.00015,height=0.00015)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 24-Hour)`)+1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)-1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(6))+scale_x_continuous(breaks=pretty_breaks(5))+
labs(title="Nitrate + Nitrite: 24-Hour",x=expression("Mean 15-Minute and 24-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 24-Hour ("~mg~L^-1*")"))

NOX_plot <-plot_grid(NOX_8_plot, NOX_24_plot, labels = c('A', 'B'), label_size = 12)

ggsave("./Figures/BA_Plots/Bland_Altman_NOx.jpeg",plot=last_plot(),height=3.5,width=8,units="in")

#BA plot for Orthophosphate 8-hour
OPO4_8_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="OPO4") ,aes((Value_8_hour+Value_15_min)/2,`Difference (15-Min - 8-Hour)`))+
geom_jitter(shape=21,size=3 ,color="black",fill=Color2,width=0.0002,height=0.00015)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 8-Hour)`)+1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)-1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=c(-.001,0,.001))+scale_x_continuous(breaks=pretty_breaks(5))+
labs(title="Orthophosphate: 8-Hour ",x=expression("Mean 15-Minute and 8-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 8-Hour ("~mg~L^-1*")"))

#BA plot for Orthophosphate 24-hour
OPO4_24_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="OPO4") ,aes((Value_24_hour+Value_15_min)/2,`Difference (15-Min - 24-Hour)`))+
geom_jitter(shape=21,size=3 ,color="black",fill=Color2,width=0.0002,height=0.00015)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 24-Hour)`)+1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)-1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=c(-.002,-.001,0,.001))+expand_limits(y = 0.001)+scale_x_continuous(breaks=pretty_breaks(5))+
labs(title="Orthophosphate: 24-Hour ",x=expression("Mean 15-Minute and 24-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 24-Hour ("~mg~L^-1*")"))

OPO4_plot <-plot_grid(OPO4_8_plot, OPO4_24_plot, labels = c('A', 'B'), label_size = 12)

ggsave("./Figures/BA_Plots/Bland_Altman_OPO4.jpeg",plot=last_plot(),height=3.5,width=8,units="in")

#BA plot for Potassium 8-hour
K_8_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="K") ,aes((Value_8_hour+Value_15_min)/2,`Difference (15-Min - 8-Hour)`))+
geom_point(shape=21,size=3 ,color="black",fill=Color2)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 8-Hour)`)+1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)-1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(6))+
labs(title="Potassium: 8-Hour ",x=expression("Mean 15-Minute and 8-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 8-Hour ("~mg~L^-1*")"))

#BA plot for Potassium 24-hour
K_24_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="K") ,aes((Value_24_hour+Value_15_min)/2,`Difference (15-Min - 24-Hour)`))+
geom_point(shape=21,size=3 ,color="black",fill=Color2)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 24-Hour)`)+1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)-1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(7))+
labs(title="Potassium: 24-Hour ",x=expression("Mean 15-Minute and 24-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 24-Hour ("~mg~L^-1*")"))

K_plot <-plot_grid(K_8_plot, K_24_plot, labels = c('A', 'B'), label_size = 12)

ggsave("./Figures/BA_Plots/Bland_Altman_Potassium.jpeg",plot=last_plot(),height=3.5,width=8,units="in")

#BA plot for Silica 8-hour
SIO2_8_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="SIO2") ,aes((Value_8_hour+Value_15_min)/2,`Difference (15-Min - 8-Hour)`))+
geom_point(shape=21,size=3 ,color="black",fill=Color2)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 8-Hour)`)+1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)-1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(6))+
labs(title="Silica: 8-Hour ",x=expression("Mean 15-Minute and 8-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 8-Hour ("~mg~L^-1*")"))

#BA plot for Silica 24-hour
SIO2_24_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="SIO2") ,aes((Value_24_hour+Value_15_min)/2,`Difference (15-Min - 24-Hour)`))+
geom_point(shape=21,size=3 ,color="black",fill=Color2)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 24-Hour)`)+1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)-1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(7))+
labs(title="Silica: 24-Hour ",x=expression("Mean 15-Minute and 24-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 24-Hour ("~mg~L^-1*")"))

SIO2_plot <-plot_grid(SIO2_8_plot, SIO2_24_plot, labels = c('A', 'B'), label_size = 12)

ggsave("./Figures/BA_Plots/Bland_Altman_Silica.jpeg",plot=last_plot(),height=3.5,width=8,units="in")

#BA plot for Sodium 8-hour
Sodium_8_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="Sodium") ,aes((Value_8_hour+Value_15_min)/2,`Difference (15-Min - 8-Hour)`))+
geom_point(shape=21,size=3 ,color="black",fill=Color2)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 8-Hour)`)+1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)-1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(6))+
labs(title="Sodium: 8-Hour ",x=expression("Mean 15-Minute and 8-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 8-Hour ("~mg~L^-1*")"))

#BA plot for Sodium 24-hour
Sodium_24_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="Sodium") ,aes((Value_24_hour+Value_15_min)/2,`Difference (15-Min - 24-Hour)`))+
geom_point(shape=21,size=3 ,color="black",fill=Color2)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 24-Hour)`)+1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)-1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(7))+
labs(title="Sodium: 24-Hour ",x=expression("Mean 15-Minute and 24-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 24-Hour ("~mg~L^-1*")"))

Sodium_plot <-plot_grid(Sodium_8_plot, Sodium_24_plot, labels = c('A', 'B'), label_size = 12)

ggsave("./Figures/BA_Plots/Bland_Altman_Sodium.jpeg",plot=last_plot(),height=3.5,width=8,units="in")

#BA plot for Sulfate 8-hour
Sulfate_8_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="SO4") ,aes((Value_8_hour+Value_15_min)/2,`Difference (15-Min - 8-Hour)`))+
geom_point(shape=21,size=3 ,color="black",fill=Color2)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 8-Hour)`)+1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)-1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(6))+
labs(title="Sulfate: 8-Hour ",x=expression("Mean 15-Minute and 8-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 8-Hour ("~mg~L^-1*")"))

#BA plot for Sulfate 24-hour
Sulfate_24_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="SO4") ,aes((Value_24_hour+Value_15_min)/2,`Difference (15-Min - 24-Hour)`))+
geom_point(shape=21,size=3 ,color="black",fill=Color2)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 24-Hour)`)+1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)-1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(7))+
labs(title="Sulfate: 24-Hour ",x=expression("Mean 15-Minute and 24-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 24-Hour ("~mg~L^-1*")"))

Sulfate_plot <-plot_grid(Sulfate_8_plot, Sulfate_24_plot, labels = c('A', 'B'), label_size = 12)

ggsave("./Figures/BA_Plots/Bland_Altman_Sulfate.jpeg",plot=last_plot(),height=3.5,width=8,units="in")


#BA plot for Total Dissolved Nitrogen 8-hour
TDN_8_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="TDN") ,aes((Value_8_hour+Value_15_min)/2,`Difference (15-Min - 8-Hour)`))+
geom_point(shape=21,size=3 ,color="black",fill=Color2)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 8-Hour)`)+1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)-1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(6))+
labs(title="Total Dissolved Nitrogen: 8-Hour ",x=expression("Mean 15-Minute and 8-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 8-Hour ("~mg~L^-1*")"))

#BA plot for Total Dissolved Nitrogen 24-hour
TDN_24_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="TDN") ,aes((Value_24_hour+Value_15_min)/2,`Difference (15-Min - 24-Hour)`))+
geom_point(shape=21,size=3 ,color="black",fill=Color2)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 24-Hour)`)+1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)-1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(7))+
labs(title="Total Dissolved Nitrogen: 24-Hour ",x=expression("Mean 15-Minute and 24-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 24-Hour ("~mg~L^-1*")"))

TDN_plot <-plot_grid(TDN_8_plot, TDN_24_plot, labels = c('A', 'B'), label_size = 12)

ggsave("./Figures/BA_Plots/Bland_Altman_TDN.jpeg",plot=last_plot(),height=3.5,width=8,units="in")

#BA plot for Dissolved Iron 8-hour
TDSFE_8_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="TDSFE") ,aes((Value_8_hour+Value_15_min)/2,`Difference (15-Min - 8-Hour)`))+
geom_jitter(shape=21,size=3 ,color="black",fill=Color2,width=0.0002,height=0.00015)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 8-Hour)`)+1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)-1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(6))+
labs(title="Total Dissolved Iron: 8-Hour ",x=expression("Mean 15-Minute and 8-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 8-Hour ("~mg~L^-1*")"))

#BA plot for Total Dissolved Iron 24-hour
TDSFE_24_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="TDSFE") ,aes((Value_24_hour+Value_15_min)/2,`Difference (15-Min - 24-Hour)`))+
geom_jitter(shape=21,size=3 ,color="black",fill=Color2,width=0.0002,height=0.00015)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 24-Hour)`)+1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)-1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(7))+
labs(title="Total Dissolved Iron: 24-Hour ",x=expression("Mean 15-Minute and 24-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 24-Hour ("~mg~L^-1*")"))

TDSFE_plot <-plot_grid(TDSFE_8_plot, TDSFE_24_plot, labels = c('A', 'B'), label_size = 12)

ggsave("./Figures/BA_Plots/Bland_Altman_TDSFE.jpeg",plot=last_plot(),height=3.5,width=8,units="in")


#BA plot for Total Dissolved Phosphorus 8-hour
TDP_8_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="TDPO4") ,aes((Value_8_hour+Value_15_min)/2,`Difference (15-Min - 8-Hour)`))+
geom_jitter(shape=21,size=3 ,color="black",fill=Color2,width=0.000,height=0.00015)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 8-Hour)`)+1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)-1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(6))+scale_x_continuous(breaks=pretty_breaks(8))+
labs(title="Total Dissolved Phosphorus: 8-Hour ",x=expression("Mean 15-Minute and 8-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 8-Hour ("~mg~L^-1*")"))

#BA plot for Total Dissolved Phosphorus 24-hour
TDP_24_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="TDPO4") ,aes((Value_24_hour+Value_15_min)/2,`Difference (15-Min - 24-Hour)`))+
geom_jitter(shape=21,size=3 ,color="black",fill=Color2,width=0.000,height=0.00015)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 24-Hour)`)+1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)-1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(7))+scale_x_continuous(breaks=pretty_breaks(8))+
labs(title="Total Dissolved Phosphorus: 24-Hour ",x=expression("Mean 15-Minute and 24-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 24-Hour ("~mg~L^-1*")"))

TDP_plot <-plot_grid(TDP_8_plot, TDP_24_plot, labels = c('A', 'B'), label_size = 12)

ggsave("./Figures/BA_Plots/Bland_Altman_TDP.jpeg",plot=last_plot(),height=3.5,width=8,units="in")

#BA plot for Total Nitrogen 8-hour
TN_8_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="TN") ,aes((Value_8_hour+Value_15_min)/2,`Difference (15-Min - 8-Hour)`))+
geom_point(shape=21,size=3 ,color="black",fill=Color2)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 8-Hour)`)+1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)-1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(6))+
labs(title="Total Nitrogen: 8-Hour ",x=expression("Mean 15-Minute and 8-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 8-Hour ("~mg~L^-1*")"))

#BA plot for Total Nitrogen 24-hour
TN_24_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="TN") ,aes((Value_24_hour+Value_15_min)/2,`Difference (15-Min - 24-Hour)`))+
geom_point(shape=21,size=3 ,color="black",fill=Color2)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 24-Hour)`)+1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)-1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(7))+
labs(title="Total Nitrogen: 24-Hour ",x=expression("Mean 15-Minute and 24-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 24-Hour ("~mg~L^-1*")"))

TN_plot <-plot_grid(TN_8_plot, TN_24_plot, labels = c('A', 'B'), label_size = 12)

ggsave("./Figures/BA_Plots/Bland_Altman_TN.jpeg",plot=last_plot(),height=3.5,width=8,units="in")

#BA plot for Total Organic Carbon  8-hour
TOC_8_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="TOC") ,aes((Value_8_hour+Value_15_min)/2,`Difference (15-Min - 8-Hour)`))+
geom_point(shape=21,size=3 ,color="black",fill=Color2)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 8-Hour)`)+1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)-1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(6))+
labs(title="Total Organic Carbon: 8-Hour ",x=expression("Mean 15-Minute and 8-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 8-Hour ("~mg~L^-1*")"))

#BA plot for Total Organic Carbon 24-hour
TOC_24_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="TOC") ,aes((Value_24_hour+Value_15_min)/2,`Difference (15-Min - 24-Hour)`))+
geom_point(shape=21,size=3 ,color="black",fill=Color2)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 24-Hour)`)+1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)-1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(7))+
labs(title="Total Organic Carbon: 24-Hour ",x=expression("Mean 15-Minute and 24-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 24-Hour ("~mg~L^-1*")"))

TOC_plot <-plot_grid(TOC_8_plot, TOC_24_plot, labels = c('A', 'B'), label_size = 12)

ggsave("./Figures/BA_Plots/Bland_Altman_TOC.jpeg",plot=last_plot(),height=3.5,width=8,units="in")

#BA plot for Total Phosphorus  8-hour
TP_8_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="TPO4") ,aes((Value_8_hour+Value_15_min)/2,`Difference (15-Min - 8-Hour)`))+
geom_jitter(shape=21,size=3 ,color="black",fill=Color2,width=0.000,height=0.00015)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 8-Hour)`)+1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 8-Hour)`)-1.96*sd(`Difference (15-Min - 8-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(8))+
labs(title="Total Phosphorus: 8-Hour ",x=expression("Mean 15-Minute and 8-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 8-Hour ("~mg~L^-1*")"))

#BA plot for Total Phosphorus 24-hour
TP_24_plot <- ggplot(filter(BA_Plot_Tidy, TEST_NAME=="TPO4") ,aes((Value_24_hour+Value_15_min)/2,`Difference (15-Min - 24-Hour)`))+
geom_jitter(shape=21,size=3 ,color="black",fill=Color2,width=0.000,height=0.00015)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)), colour ="black" ,size = 1,linetype="dashed")+
geom_hline(aes(yintercept =mean(`Difference (15-Min - 24-Hour)`)+1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1)+
geom_hline(aes(yintercept = mean(`Difference (15-Min - 24-Hour)`)-1.96*sd(`Difference (15-Min - 24-Hour)`)), colour = Color1 ,linetype ="longdash",size = 1) +
theme_bw()+theme(legend.position="bottom")+scale_y_continuous(breaks=pretty_breaks(7))+
labs(title="Total Phosphorus: 24-Hour ",x=expression("Mean 15-Minute and 24-Hour ("~mg~L^-1*")"), y=expression("Differences 15-Minute - 24-Hour ("~mg~L^-1*")"))

TP_plot <-plot_grid(TP_8_plot, TP_24_plot, labels = c('A', 'B'), label_size = 12)

ggsave("./Figures/BA_Plots/Bland_Altman_TP.jpeg",plot=last_plot(),height=3.5,width=8,units="in")
