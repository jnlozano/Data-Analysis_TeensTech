##########################################################################################
# Specification Curve Analysis and Digital Technology Use
# R-Script 3.1: YRBS Analyse Specification Curves
##########################################################################################

library(tidyverse)

load("2_1_sca_yrbs_results.rda")

####################################################################################
# Number of specifications
####################################################################################
nrow(results_yrbs_sca)

## Amount of results and significant results with dominant sign
results_yrbs_sca_sig <- results_yrbs_sca %>% filter(p_value < 0.05)
pmax(table(sign(results_yrbs_sca$effect))[[-1]],table(sign(results_yrbs_sca$effect))[[1]])
table(sign(results_yrbs_sca_sig$effect))[[-1]]

####################################################################################
# Median effects 
# total, separate x variables, controls/no controls
####################################################################################
results_yrbs_sca %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                               median_effectsize = median(rsqrd, na.rm = TRUE), 
                               median_n = median(number, na.rm = TRUE),
                               median_se = median(standard_error, na.rm = TRUE))


z <- results_yrbs_sca %>% filter(x_variable == "tech") %>% summarise(median_effect = median(effect, na.rm = TRUE),   
                                                                      median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                      median_n = median(number, na.rm = TRUE),
                                                                      median_se = median(standard_error, na.rm = TRUE))


z1 <- results_yrbs_sca %>% filter(controls == "No Controls") %>% summarise(median_effect = median(effect, na.rm = TRUE),   
                                                                     median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                     median_n = median(number, na.rm = TRUE),
                                                                     median_se = median(standard_error, na.rm = TRUE))


a <- results_yrbs_sca %>% filter(x_variable == "q81_n") %>% summarise(median_effect = median(effect, na.rm = TRUE),   #TV
                                                                 median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                 median_n = median(number, na.rm = TRUE),
                                                                 median_se = median(standard_error, na.rm = TRUE))
b <- results_yrbs_sca %>% filter(x_variable == "q82_n") %>%summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                median_n = median(number, na.rm = TRUE),
                                                                median_se = median(standard_error, na.rm = TRUE))


a1 <- results_yrbs_sca %>% filter(x_variable == "qn41") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                  median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                  median_n = median(number, na.rm = TRUE),
                                                                  median_se = median(standard_error, na.rm = TRUE))


a2 <- results_yrbs_sca %>% filter(x_variable == "q11") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                   median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                   median_n = median(number, na.rm = TRUE),
                                                                   median_se = median(standard_error, na.rm = TRUE))
a3 <- results_yrbs_sca %>% filter(x_variable == "qn31") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                   median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                   median_n = median(number, na.rm = TRUE),
                                                                   median_se = median(standard_error, na.rm = TRUE))

a4 <- results_yrbs_sca %>% filter(x_variable == "qn52") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                   median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                   median_n = median(number, na.rm = TRUE),
                                                                   median_se = median(standard_error, na.rm = TRUE))
a5 <- results_yrbs_sca %>% filter(x_variable == "qn58") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                   median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                   median_n = median(number, na.rm = TRUE),
                                                                   median_se = median(standard_error, na.rm = TRUE))

a6 <- results_yrbs_sca %>% filter(x_variable == "qn39") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                   median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                   median_n = median(number, na.rm = TRUE),
                                                                   median_se = median(standard_error, na.rm = TRUE))
a6_ <- results_yrbs_sca %>% filter(x_variable == "q80") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                      median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                      median_n = median(number, na.rm = TRUE),
                                                                      median_se = median(standard_error, na.rm = TRUE))

a7 <- results_yrbs_sca %>% filter(x_variable == "q84") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                   median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                   median_n = median(number, na.rm = TRUE),
                                                                   median_se = median(standard_error, na.rm = TRUE))

a8 <- results_yrbs_sca %>% filter(x_variable == "qnobese") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                   median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                   median_n = median(number, na.rm = TRUE),
                                                                   median_se = median(standard_error, na.rm = TRUE))

a9 <- results_yrbs_sca %>% filter(x_variable == "q13") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                   median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                   median_n = median(number, na.rm = TRUE),
                                                                   median_se = median(standard_error, na.rm = TRUE))
a10 <- results_yrbs_sca %>% filter(x_variable == "q21") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                     median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                     median_n = median(number, na.rm = TRUE),
                                                                     median_se = median(standard_error, na.rm = TRUE))

b1 <- results_yrbs_sca %>% filter(x_variable == "q17") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                   median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                   median_n = median(number, na.rm = TRUE),
                                                                   median_se = median(standard_error, na.rm = TRUE))


b2 <- results_yrbs_sca %>% filter(x_variable == "qn61") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                   median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                   median_n = median(number, na.rm = TRUE),
                                                                   median_se = median(standard_error, na.rm = TRUE))

c <- results_yrbs_sca %>% filter(x_variable == "qn62") %>%summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                     median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                     median_n = median(number, na.rm = TRUE),
                                                                     median_se = median(standard_error, na.rm = TRUE))
d <- results_yrbs_sca %>% filter(x_variable == "q72") %>%summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                     median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                     median_n = median(number, na.rm = TRUE),
                                                                     median_se = median(standard_error, na.rm = TRUE))
e <- results_yrbs_sca %>% filter(x_variable == "q79") %>%summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                     median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                     median_n = median(number, na.rm = TRUE),
                                                                     median_se = median(standard_error, na.rm = TRUE))
f <- results_yrbs_sca %>% filter(x_variable == "q76") %>%summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                     median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                     median_n = median(number, na.rm = TRUE),
                                                                     median_se = median(standard_error, na.rm = TRUE))
g <- results_yrbs_sca %>% filter(x_variable == "q88") %>%summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                     median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                     median_n = median(number, na.rm = TRUE),
                                                                     median_se = median(standard_error, na.rm = TRUE))
h <- results_yrbs_sca %>% filter(x_variable == "q74") %>%summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                     median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                     median_n = median(number, na.rm = TRUE),
                                                                     median_se = median(standard_error, na.rm = TRUE))
i <- results_yrbs_sca %>% filter(x_variable == "q78") %>%summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                   median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                   median_n = median(number, na.rm = TRUE),
                                                                   median_se = median(standard_error, na.rm = TRUE))
i1 <- results_yrbs_sca %>% filter(x_variable == "q69") %>%summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                   median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                   median_n = median(number, na.rm = TRUE),
                                                                   median_se = median(standard_error, na.rm = TRUE))
j <- results_yrbs_sca %>% filter(x_variable == "q87r") %>%summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                   median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                   median_n = median(number, na.rm = TRUE),
                                                                   median_se = median(standard_error, na.rm = TRUE))
k <- results_yrbs_sca %>% filter(x_variable == "q47") %>%summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                   median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                   median_n = median(number, na.rm = TRUE),
                                                                   median_se = median(standard_error, na.rm = TRUE))
l <- results_yrbs_sca %>% filter(x_variable == "q44") %>%summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                   median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                   median_n = median(number, na.rm = TRUE),
                                                                   median_se = median(standard_error, na.rm = TRUE))
m <- results_yrbs_sca %>% filter(x_variable == "q24_n") %>%summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                   median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                   median_n = median(number, na.rm = TRUE),
                                                                   median_se = median(standard_error, na.rm = TRUE))
n <- results_yrbs_sca %>% filter(x_variable == "q18") %>%summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                   median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                   median_n = median(number, na.rm = TRUE),
                                                                   median_se = median(standard_error, na.rm = TRUE))
o <- results_yrbs_sca %>% filter(x_variable == "tech") %>%summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                   median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                   median_n = median(number, na.rm = TRUE),
                                                                   median_se = median(standard_error, na.rm = TRUE))




results_yrbs_sca %>% filter(controls == "Controls") %>% summarise(median_effect = median(effect, na.rm = TRUE), 
                                                                  median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                  median_n = median(number, na.rm = TRUE),
                                                                  median_se = median(standard_error, na.rm = TRUE))
results_yrbs_sca %>% filter(controls == "No Controls") %>% summarise(median_effect = median(effect, na.rm = TRUE),
                                                                     median_effectsize = median(rsqrd, na.rm = TRUE),
                                                                     median_n = median(number, na.rm = TRUE),
                                                                     median_se = median(standard_error, na.rm = TRUE))

names <-
  c(
    "Alcohol(ever)",
    "Drink and Drive",
    "Cigarette(ever)",
    "Heroin(ever)",
    "Injection(ever)",
    "Vaping",
    "Exercise",
    "Sports Team",
    "Obesity",
    "Weapon Carry",
    "Force Sex",
    "Threatened at School",
    "Sex before 13",
    "4+ Sexual Partners",
    "Eat fruit",
    "Eat breakfast",
    "Eat vegetables",
    "hours sleep",
    "Eat potatoes",
    "Drink Milk",
    "Perceived Weight",
    "Asthma",
    "Marijuana",
    "Bingedrinking",
    "Bullied",
    "Fight",
    "technology use"
  )

namesx<-
  c("Technology Use", 'no controls')
#    "TV Use",
#    "Electronic Device Use")

#names <-unlist(names)
#table2 <- rbind(a1,a2,a3,a4,a5,a6,a6_,a7,a8,a9,a10,b1,b2,c,d,e,f,g,h,i,i1,j,k,l,m,n,o)
namesx <- unlist(namesx) 
table2<- rbind(z,z1) #(a,b)
table2$variable <- namesx  #change this names -> namesx
table2$controls <- ControlLabel
table2$items <- ItemsLabel
table2$scale <- ScalesLabel
table2$sex <- SexLabel
table2$year <- '2007-2015'
#write.csv(table2,file=paste("yrbss_tech_2013+.csv",sep=""))
write.table(table2, "yrbss_tech_2013+.csv", sep = ",", col.names = !file.exists("yrbss_tech_2013+.csv"), append = T)
