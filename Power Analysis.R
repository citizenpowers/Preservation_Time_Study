# The goal of this script is to calculate the power of the study. 




library(pwr)


# effect size calculated by the mean differences between groups divided by the common standard deviation 

pwr.t.test(d = 0.5, power = 0.80, sig.level = 0.05)
pwr.t.test( n=36,power=0.8, sig.level = 0.05)

power <- All_results_Uncertainty %>%
mutate(TEST_NAME=if_else(is.na(TEST_NAME),"NA",TEST_NAME))  %>%
group_by(TEST_NAME) %>%
summarise(n(),
  `8-Hour Bias`=round(mean(`Difference (15-Min - 8-Hour)`),digits=4),
  `8-hour SD`=sd(`Difference (15-Min - 8-Hour)`),
  `24-hour SD`=sd(`Difference (15-Min - 24-Hour)`),
  `SDD 8-hour`=round(sd(`Difference (15-Min - 8-Hour)`)*0.6695719,digits=4 ),    #Smallest detectable difference
  `SDD 24-hour`=round(sd(`Difference (15-Min - 24-Hour)`)*0.6695719,digits=4)   #Smallest detectable difference
  )  

power_tidy <- power %>%
left_join(mutate(select(LOA,TEST_NAME,MDL,PQL),TEST_NAME=if_else(is.na(TEST_NAME),"NA",TEST_NAME)),by="TEST_NAME") %>%
left_join(Compliance_Range, by="TEST_NAME") %>%
mutate(`Power at meaningful effect 8-hour`=pwr.t.test( n=36,d=(IQR_5/10)/`8-hour SD`, sig.level = 0.05)$power)  %>%
mutate(`Power at meaningful effect 24-hour`=pwr.t.test( n=36,d=(IQR_5/10)/`8-hour SD`, sig.level = 0.05)$power)


