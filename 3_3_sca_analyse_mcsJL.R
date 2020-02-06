##########################################################################################
# Specification Curve Analysis and Digital Technology Use
# R-Script 3.3: MCS Analyse Specification Curves
# Version 1.JL
##########################################################################################
library(tidyverse)
#################################################
# Bind Both
#################################################
load("2_3_sca_mcs_results_cm.rda")
load("2_3_sca_mcs_results_pr.rda")

results_mcs_sca_cm$respondent <-
  rep("Cohort Member", nrow(results_mcs_sca_cm))
results_mcs_sca_pr$respondent <-
  rep("Parent", nrow(results_mcs_sca_pr))

results_mcs_sca_total <- rbind(results_mcs_sca_cm, results_mcs_sca_pr)
save(results_mcs_sca_total, file = "2_3_sca_mcs_results.rda")

####################################################################################
# Number of specifications
####################################################################################
nrow(results_mcs_sca_total)

####################################################################################
# Median effects table 2
# total, separate x variables, controls/no controls (calls eta-squared "effect size" and stad slop "effect")
# %>% pipes in anad makes nesting run. makes code more transparent? it is in dyplr
####################################################################################
results_mcs_sca_total %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE)) # are the stats pulled from same row?

results_mcs_sca_total %>% filter(respondent == "Parent") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
results_mcs_sca_total %>% filter(respondent == "Cohort Member") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))

a <-results_mcs_sca_total %>% filter(controls == "Controls") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
b <-results_mcs_sca_total %>% filter(controls == "No Controls") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
#c<-results_mcs_sca_total %>% filter(x_variable == "fcsome00r") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
labels <- c("Controls",
            "No Controls")
            #"fcsome00r")
labels <- unlist(labels)
socmed <- rbind(a,b)
socmed$x_varname <- labels
rm(a,b)

if(Mod ==5){
### Sectioned off for each variable set. Currently uses all sections 
a<-results_mcs_sca_total %>% filter(x_variable == "fcsome00r") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
b <-results_mcs_sca_total %>% filter(x_variable == "fcinth00r") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
c <- results_mcs_sca_total %>% filter(x_variable == "fccomh00r") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
d <- results_mcs_sca_total %>% filter(x_variable == "fctvho00r") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
e <-  results_mcs_sca_total %>% filter(x_variable == "tech") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
topvar <- c("fcsome00r",
            "fcinth00r",
            "fccomh00r",
            "fctvho00r",
            "tech")
topvar <- unlist(topvar)
top <- rbind(a,b,c,d,e)
top$x_varname <- topvar
rm(a,b,c,d,e)

######

f <- results_mcs_sca_total %>% filter(x_variable == "fcalfv00r") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
g <- results_mcs_sca_total %>% filter(x_variable == "fccanb00r") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
h <- results_mcs_sca_total %>% filter(x_variable == "fchurt00r") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
i<-results_mcs_sca_total %>% filter(x_variable == "fcares00r") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
j<-results_mcs_sca_total %>% filter(x_variable == "fccycf00r") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
k<-results_mcs_sca_total %>% filter(x_variable == "fchtcm00") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
l<-results_mcs_sca_total %>% filter(x_variable == "fcglas00r") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
m<-results_mcs_sca_total %>% filter(x_variable == "hand") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
n<-results_mcs_sca_total %>% filter(x_variable == "fcfrut00") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
o<-results_mcs_sca_total %>% filter(x_variable == "fcvegi00") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
p<-results_mcs_sca_total %>% filter(x_variable == "sleeptime") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
q<-results_mcs_sca_total %>% filter(x_variable == "fcbrkn00") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
midvar <- c("fcalfv00r",
            "fccanb00r",
            "fchurt00r",
            "fcares00r",
            "fccycf00r",
            "fchtcm00",
            "fcglas00r",
            "hand",
            "fcfrut00",
            "fcvegi00",
            "sleeptime",
            "fcbrkn00")
midvar <- unlist(midvar)
mid <- rbind(f,g,h,i,j,k,l,m,n,o,p,q)
mid$x_varname <- midvar
rm(f,g,h,i,j,k,l,m,n,o,p,q)

####

r <- results_mcs_sca_total %>% filter(x_variable == "fcalcd00") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
s <-results_mcs_sca_total %>% filter(x_variable == "fcalfv00") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
t <- results_mcs_sca_total %>% filter(x_variable == "fcsmok00") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
u<-results_mcs_sca_total %>% filter(x_variable == "fcotdr00") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
v<-results_mcs_sca_total %>% filter(x_variable == "fcphex00") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
w<-results_mcs_sca_total %>% filter(x_variable == "fcwegt00") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
x<-results_mcs_sca_total %>% filter(x_variable == "fcobflg6") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
y<-results_mcs_sca_total %>% filter(x_variable == "fcnufr00") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
z<-results_mcs_sca_total %>% filter(x_variable == "fcsexx00") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
a1<-results_mcs_sca_total %>% filter(x_variable == "fcknif00") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
b1<-results_mcs_sca_total %>% filter(x_variable == "fcpols00") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
c1<-results_mcs_sca_total %>% filter(x_variable == "fptsus00") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
d1<-results_mcs_sca_total %>% filter(x_variable == "fcstol00") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
e1<-results_mcs_sca_total %>% filter(x_variable == "fcquam00") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
f1<-results_mcs_sca_total %>% filter(x_variable == "fcvicf0a") %>% summarise(median_effect = median(effect, na.rm = TRUE), median_effectsize = median(rsqrd, na.rm = TRUE))
botvar <- c("fcalcd00",
            "fcalfv00",
            "fcsmok00",
            "fcotdr00",
            "fcphex00",
            "fcwegt00",
            "fcobflg6",
            "fcnufr00",
            "fcsexx00",
            "fcknif00",
            "fcpols00",
            "fptsus00",
            "fcstol00",
            "fcquam00",
            "fcvicf0a")
botvar <- unlist(botvar)
bot <- rbind(r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1)
bot$x_varname <- botvar
rm(r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1)

end <-rbind(top,mid,bot)
table2 <-end}
####################################################################################
### Table 2 Components (return to get the med for even number of obs)
####################################################################################

table2 <- socmed
table2$PRemotionItems<-EmoItemOnlyLabel
table2$run<-RunNum
table2$CurveFit<-GAMMLabel
table2$CodeVersion<-code_version 
#$IV<-IVtypeLabel #only used for model 3 
table2$ItemsLabel<-ItemsLabel
table2$ScalesLabel<-ScalesLabel
table2$cont<-ControlLabel
table2$SDQincluded<-SDQincludedLabel
table2$sex <- SexLabel
table2$model <- ModLabel
table2$var_name <- "Social Media" 
#table2$SDQLabel <- "combinedSDQ"
#table2$age <- AgeLabel
table2 <- table2 %>% 
  rename(
    median_slope = median_effect,
    median_n2 = median_effectsize
  )

#table2 <- table2[c("sex","median_slope","median_n2","x_varname","SDQincluded","PRemotionItems","run","CodeVersion","ItemsLabel","ScalesLabel","cont","model", "age")]
table2 <- table2[c("sex","var_name", "median_slope","median_n2","x_varname","SDQincluded", "PRemotionItems","run","CodeVersion","ItemsLabel","ScalesLabel","cont","model")]

write.table(table2, "mcs.table_configurations.csv", sep = ",", col.names = !file.exists("table2.csv"), append = T)
#write.csv(table2,file=paste("mcs.config3.csv",sep="")) #only uncomment if you need to initialize 

print("End 3_3_sca_analyse_mcs")