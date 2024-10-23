#Goal of this script is to evaluate the Intraclass Correlation between the treatments


library(psych)
library(irr)


# Import data -------------------------------------------------------------

All_results_Uncertainty <- read.csv("./Data/All_results_Uncertainty.csv")


# Tidy Data ---------------------------------------------------------------


TPO4_ICC <-    All_results_Uncertainty %>%
filter(TEST_NAME =="TPO4") %>%
select(Value_15_min, Value_8_hour, Value_24_hour)


SO4_ICC <-    All_results_Uncertainty %>%
  filter(TEST_NAME =="SO4") %>%
  select(Value_15_min, Value_8_hour, Value_24_hour)

# Calculate ICC -----------------------------------------------------------

icc(TPO4_ICC, model = "oneway", type = "agreement", unit = "single")

icc(SO4_ICC, model = "oneway", type = "agreement", unit = "single")


