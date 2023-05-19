##LABELS
code_version<-paste("v1.JL_",Sys.Date(), sep="")

setwd("~/Orben/YRBS")
# 2.2.2 set up the labels for the sex
if(Sex == 0 || Sex ==1){ 
  SexLabel<-dplyr:: recode(Sex,'0'='girls', '1'="boys") 
} else {
  SexLabel<-"combined"
}

if(items==0) {
  ItemsLabel<-"4Items"
} else if (items==1){
  ItemsLabel<-"AllItems"
}

# 2.2.4 set up labels for indicator of scale adjustments
if(scales==0) {
  ScalesLabel<-"NoScales"
} else if (scales==1){
  ScalesLabel<-"1Scale"
}

if (LimControls==1){ #was the parameter set to limit the controls?
  ControlLabel<-"Orben's Controls" # mark the analysis as limited to the demographic controls
}
if (LimControls==0){ #was the parameter set to limit the controls?
  ControlLabel<-"Our Controls" # mark the analysis as using the original controls of Orben
}

source("1_1_prep_yrbs_JeanData.R")
source("2_1_sca_yrbs.R")
source("3_1_sca_analyse_yrbs.R")
