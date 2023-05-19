setwd("~/Orben") 
rm(list = ls(all = TRUE)) 

#CONFIG 1
Mod<-3
GAMM<-0  
Scales<-1
Items<-1 
EmoItemOnly<-0 
knot_location<-5 
LimitedControls<- 0
NoSDQHeadAche<-0
SelfHarm<-0
SelfHarmi <-0
IVtype <- 0
combineSDQ <- 1 
dropSDQ<-0 
PrintGraphs<-0 
Scripted<-1
Sex<-0
RunNum<-1
ModLabel <- "Config1"
source("parameters_mcsJL.R")
Sex <- 1 
source("parameters_mcsJL.R")
rm(list = ls(all = TRUE)) 
gc()

# CONFIG 2
Mod<-3
GAMM<-0  
Scales<-1 
Items<-0
EmoItemOnly<-0 
knot_location<-5 
LimitedControls<- 0
NoSDQHeadAche<-0
SelfHarm<-0
SelfHarmi <-0
IVtype <- 0
combineSDQ <- 1 
dropSDQ<-0 
PrintGraphs<-0 
Scripted<-1
Sex<-0
RunNum<-1
ModLabel <- "Config2"
source("parameters_mcsJL.R")
Sex <- 1 
source("parameters_mcsJL.R")
rm(list = ls(all = TRUE)) 
gc()

#CONFIG 3
Mod<-3
GAMM<-0  
Scales<-1
Items<-0
EmoItemOnly<-0 
knot_location<-5 
LimitedControls<- 1
NoSDQHeadAche<-0
SelfHarm<-0
SelfHarmi <-0
IVtype <- 0
combineSDQ <- 1 # combines SDQ scale items. NEW. 
dropSDQ<-0 
PrintGraphs<-0 
#Scripted<-1
Sex<-0
RunNum<-1
ModLabel <- "Config3"
source("parameters_mcsJL.R")
Sex <- 1 
source("parameters_mcsJL.R")
rm(list = ls(all = TRUE)) 
gc()


#CONFIG 4
Mod<-3
GAMM<-0  
Scales<-1
Items<-1
EmoItemOnly<-0 
knot_location<-5 
LimitedControls<- 1
NoSDQHeadAche<-0
SelfHarm<-0
SelfHarmi <-0
IVtype <- 0
combineSDQ <- 1
dropSDQ<-0 
PrintGraphs<-0 
Scripted<-1
Sex<-0
RunNum<-1
ModLabel <- "Config4"
source("parameters_mcsJL.R")
Sex <- 1 
source("parameters_mcsJL.R")
rm(list = ls(all = TRUE)) 
gc()

#CONFIG 5 
Mod<-3
GAMM<-0  
Scales<-1
Items<-0
EmoItemOnly<-0 
knot_location<-5 
LimitedControls<- 1
NoSDQHeadAche<-0
SelfHarm<-0
SelfHarmi <-0
IVtype <- 0
combineSDQ <- 1 
dropSDQ<-1 
PrintGraphs<-0 
Scripted<-1
Sex<-0
RunNum<-1
ModLabel <- "Config5"
source("parameters_mcsJL.R")
Sex <- 1 
source("parameters_mcsJL.R")
rm(list = ls(all = TRUE)) 
gc()

#CONFIG 6 
Mod<-3
GAMM<-0  
Scales<-1
Items<-1
EmoItemOnly<-0 
knot_location<-5 
LimitedControls<- 1
NoSDQHeadAche<-0
SelfHarm<-0
SelfHarmi <-0
IVtype <- 0
combineSDQ <- 1 
dropSDQ<-1
PrintGraphs<-0 
Scripted<-1
Sex<-0
RunNum<-1
ModLabel <- "Config6"
source("parameters_mcsJL.R")
Sex <- 1 
source("parameters_mcsJL.R")
rm(list = ls(all = TRUE)) 
gc()

#CONFIG 7 
Mod<-3
GAMM<-0  
Scales<-1
Items<-0
EmoItemOnly<-0 
knot_location<-5 
LimitedControls<- 1
NoSDQHeadAche<-0
SelfHarm<-1
SelfHarmi <-0
IVtype <- 0
combineSDQ <- 1 
dropSDQ<-1
PrintGraphs<-0 
Scripted<-1
Sex<-0
RunNum<-1
ModLabel <- "Config7"
source("parameters_mcsJL.R")
Sex <- 1 
source("parameters_mcsJL.R")
rm(list = ls(all = TRUE)) 
gc()

#CONFIG 8 
Mod<-3
GAMM<-0  
Scales<-1
Items<-1
EmoItemOnly<-0 
knot_location<-5 
LimitedControls<- 1
NoSDQHeadAche<-0
SelfHarm<-0
SelfHarmi <-1 #SelfHarmi added for differentation between scales and items.   
IVtype <- 0
combineSDQ <- 1 
dropSDQ<-1
PrintGraphs<-0 
Scripted<-1
Sex<-0
RunNum<-1
ModLabel <- "Config8"
source("parameters_mcsJL.R")
Sex <- 1 
source("parameters_mcsJL.R")
rm(list = ls(all = TRUE)) 
gc()
