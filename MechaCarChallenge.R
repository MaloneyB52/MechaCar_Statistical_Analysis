library(tidyverse)
mecha_data <- read.csv("MechaCar_mpg.csv",stringsAsFactors = F,check.names = F)

mecha_lm <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mecha_data)

summary(mecha_lm)

suspension_data <- read.csv("Suspension_Coil.csv",stringsAsFactors = F, check.names = F)

total_summary <- suspension_data %>%
  summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))

t.test(suspension_data$PSI,mu = 1500)

t.test(subset(suspension_data,Manufacturing_Lot=="Lot1")$PSI,mu = 1500)

t.test(subset(suspension_data,Manufacturing_Lot=="Lot2")$PSI,mu = 1500)

t.test(subset(suspension_data,Manufacturing_Lot=="Lot3")$PSI,mu = 1500)
