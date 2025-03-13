#Goal of this script is to evaluate equivalence using Two One-Sided Significance Tests (TOST)



library(TOSTER)
library(Tidyverse)
library(confintr)


# Import data -------------------------------------------------------------


Sample_Results_Tidy <- read_csv("Data/Sample_Results_Tidy.csv")

Replicates_Data <- read_csv("./Data/Replicates_Data.csv")

All_results_LOA <- read_csv("Data/All_results_LOA.csv")

All_results_Uncertainty <-read_csv("./Data/All_results_Uncertainty.csv")

# Tidy Data ---------------------------------------------------------------

TOST_Data <- Sample_Results_Tidy %>%
mutate(Preservation_Time=case_when(`Preservation Time`=="8-Hour Samples"~"8_Hour_Samples",
                                     `Preservation Time`=="24-Hour Samples"~"24_Hour_Samples",
                                     `Preservation Time`=="Immediate Samples"~"15_Minute_Samples")) %>% #remove spaces
mutate(TEST_NAME=ifelse(is.na(TEST_NAME),"Sodium",TEST_NAME))

CA_8h <- TOST_Data %>% filter(TEST_NAME=="CA",`Preservation Time`!= "24-Hour Samples")
CA_24h <- TOST_Data %>% filter(TEST_NAME=="CA",`Preservation Time`!= "8-Hour Samples")

TOST_CA_8h = t_TOST(formula = VALUE ~ `Preservation Time`,data = CA_8h,eqb = 1.4,smd_ci = "t",paired = TRUE)
TOST_CA_24h = t_TOST(formula = VALUE ~ `Preservation Time`,data = CA_24h,eqb = 1.4,smd_ci = "t",paired = TRUE)

plot(TOST_CA_8h, type = "simple")
plot(TOST_CA_24h, type = "simple")

NH4_8h <- TOST_Data %>% filter(TEST_NAME=="NH4",`Preservation Time`!= "24-Hour Samples")
NH4_24h <- TOST_Data %>% filter(TEST_NAME=="NH4",`Preservation Time`!= "8-Hour Samples")

TOST_NH4_8h = t_TOST(formula = VALUE ~ `Preservation Time`,data = NH4_8h,eqb =  0.005,smd_ci = "t",paired = TRUE)
TOST_NH4_24h = t_TOST(formula = VALUE ~ `Preservation Time`,data = NH4_24h,eqb = 0.005,smd_ci = "t",paired = TRUE)

plot(TOST_NH4_8h, type = "simple")
plot(TOST_NH4_24h, type = "simple")

DOC_8h <- TOST_Data %>% filter(TEST_NAME=="DOC",`Preservation Time`!= "24-Hour Samples")
DOC_24h <- TOST_Data %>% filter(TEST_NAME=="DOC",`Preservation Time`!= "8-Hour Samples")

TOST_DOC_8h = t_TOST(formula = VALUE ~ `Preservation Time`,data = DOC_8h,eqb = 1.15,smd_ci = "t",paired = TRUE)
TOST_DOC_24h = t_TOST(formula = VALUE ~ `Preservation Time`,data = DOC_24h,eqb =1.15,smd_ci = "t",paired = TRUE)

plot(TOST_DOC_8h, type = "simple")
plot(TOST_DOC_24h, type = "simple")

TOC_8h <- TOST_Data %>% filter(TEST_NAME=="TOC",`Preservation Time`!= "24-Hour Samples")
TOC_24h <- TOST_Data %>% filter(TEST_NAME=="TOC",`Preservation Time`!= "8-Hour Samples")

TOST_TOC_8h = t_TOST(formula = VALUE ~ `Preservation Time`,data = TOC_8h,eqb = 0.59,smd_ci = "t",paired = TRUE)
TOST_TOC_24h = t_TOST(formula = VALUE ~ `Preservation Time`,data = TOC_24h,eqb = 0.59,smd_ci = "t",paired = TRUE)

plot(TOST_TOC_8h, type = "simple")
plot(TOST_TOC_24h, type = "simple")


CA <- All_results_Uncertainty %>% filter(TEST_NAME=="CA")


CA_8h <-t_TOST(x = CA$Value_15_min,y = CA$Value_8_hour,paired = TRUE,eqb = .3)

plot(CA_8h, type = "simple")


# Minimum Imprecision -----------------------------------------------------
SD/sqrt(n)

min_imprecision_table <-All_results_LOA %>%
group_by(TEST_NAME) %>%
summarise(n(),SD=sd(Difference),`min imprecision`=SD/sqrt(n()))




# Analyte Variance --------------------------------------------------------

Replicates_Data_long <- Replicates_Data %>%
pivot_longer(names_to="Rep",values_to="Value",9:11) %>%
group_by(TEST_NAME)  %>%
summarise(n=sum(is.finite(Value)),SD=sd(Value,na.rm=T),qt_99=qt(.99,n-1)*SD/sqrt(n))

qt(0.975,df=n-1)*s/sqrt(n)
qt(0.99,df=360)
