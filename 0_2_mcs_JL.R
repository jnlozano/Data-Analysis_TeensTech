setwd("~/Orben") 
rm(list = ls(all = TRUE)) 

Mod<-5
GAMM<-0 ; # use original Orben Linear Model = 0, GAMM = 1, piecewise with set knot point = 3 (confirm)
Scales<-1# use the DV scales
Items<-0 #Drop item exploration (the random permutations of item combos)
EmoItemOnly<-1; #Drop SDQ irrelevant q's
knot_location<-5 #gives the knot location for piecewise regresion Mod 3
LimitedControls<- 1 # limits the control variable to just the demographcs
NoSDQHeadAche<-0
SelfHarm<-0
dropSDQ<-0
Scripted<-1
Sex<-0
RunNum<-1

###### Items
Mod<-5
GAMM<-0  
Scales<-0 
Items<-1 
EmoItemOnly<-1 
knot_location<-5 
LimitedControls<- 1  
NoSDQHeadAche<-0
SelfHarm<-0
IVtype <- 0 
dropSDQ<-1 
PrintGraphs<-0 
Scripted<-1
Sex<-0
RunNum<-1


##LABELS
code_version<-paste("v1.JL_",Sys.Date(), sep="")

# 2.2.1 set up the lables for the model type
if(GAMM==0){
  GAMMLabel<-"First Order OLS"
} else if (GAMM==1) {
  GAMMLabel<-"GAMM"
} else if (GAMM==2){
  GAMMLabel <-"Piecewise OLS: One ChangePoint Located Knot"
} else if (GAMM==3){
  GAMMLabel <-paste("Piecewise OLS: One Preset Location Knot at ",knot_location,sep="")
}

# 2.2.2 set up the labels for the sex
if(Sex == 0 || Sex ==1){ 
  SexLabel<-dplyr:: recode(Sex,'0'='girls', '1'="boys") #Create Lables
} else {
  SexLabel<-"combined"
}

# 2.2.3 set up the labels for marker of item inclusion
if(Items==0) {
  ItemsLabel<-"NoItems"
} else if (Items==1){
  ItemsLabel<-"Items"
}

# 2.2.4 set up labels for indicator of scale adjustments
if(Scales==0) {
  ScalesLabel<-"NoNewScls"
} else if (Scales==1){
  ScalesLabel<-"Scales"
  if (NoSDQHeadAche==1){ # drop head ache indicator
    ScalesLabel<-paste(ScalesLabel,"Adj SDQ Emotion",sep="")
  }
  if (SelfHarm==1){ #add selfharm indicator
    ScalesLabel<-paste(ScalesLabel,"W_Slf-hrm",sep="")
  }
}

#########
if (dropSDQ==1) {
  SDQincludedLabel<-"SDQ_NOT_included"
} else {
  SDQincludedLabel<-"Includes_SDQ"
}


# 2.2.5 set up labels for SDQ suite
if (EmoItemOnly==1) { # SDQ is limited to the emotion subscale
  EmoItemOnlyLabel<-"Limited_SDQ_to_Emotion"
} else {
  EmoItemOnlyLabel<-"All SDQ items"
}

###########################################
### 2.3 Modify the analysis for SCA Run ###
###########################################

### 2.3.0
if (Mod==1) {
  
  ### 2.3.0.1 Label constant features of this SCA configurtion
  AnlsVers<-"Orben_Model"
  print(Mod)
  SexLabel<-"Combined"
  
} else {
  
  if (Mod==2) {
    ###select the subset to be used
    AnlsVers<-"SexSpecific"
    print(Mod)
    
  } else {
    if (Mod==3) {
      ###select the subset to be used
      AnlsVers<-"SocMdOnly_SexSpec"
      SexLabel
      if (GAMM==3){
        AnlsVers<-paste("SocMdOnly_SexSpec_wBreak_at_",knot_location,sep="")
      }
      print(Mod)
      
    } else {
      
      if (Mod==5) {
        ###select the subset to be used
        AnlsVers<-"AllCompVariables_SexSpec"
        print(Mod)
      } 
    }
  }
}



#### Label the Limited IV set
if (IVtype == 0) { # limit to just social media
  IVtypeLabel <- "SocMedia"
}
if (IVtype == 1) { # limit to just social media
  IVtypeLabel <- "WeekdayTV"
}

if (IVtype == 2) { # limit to just social media
  IVtypeLabel <- "eGames"
}

if (IVtype == 4) { # limit to just social media
  IVtypeLabel <- "Internet"
}

###

# 2.3.1 Set what adjustments will be made
if (LimitedControls==1){ #was the parameter set to limit the controls?
  ControlLabel<-"Demo's Controls" # mark the analysis as limited to the demographic controls
}
if (LimitedControls==0){ #was the parameter set to limit the controls?
  ControlLabel<-"Orben Controls" # mark the analysis as using the original controls of Orben
}