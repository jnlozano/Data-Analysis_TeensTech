data<-read.csv(file="1_3_prep_mcs_data.csv", header=TRUE, sep = ",")
#print(dim(data))
set.seed(111)

#split the data by sex immediatly. Default is combined. Option to keep only 13 year olds/remove 14 and 15 from data. 
if(Sex == 0 ){
  data <- data[data$fccsex00r==0,]
}else if (Sex ==1){
  data <- data[data$fccsex00r==1,]}

if(age == 1){
  data <- data[data$fccage00==13,]}

#######################################################
# Load libraries
#######################################################
library(mgcv)
library(svMisc)
library(tidyr)
library(dplyr)
library("heplots")
library(segmented)

####################################################################################
# Function "resultsframe"
# takes character vector of outcome variables as an input
# returns the results vector of all specifications
####################################################################################
resultsframe <- function(x_var, y_var) {
  
  # Setup Specification Names
  levels_x <- x_var
  levels_y <- y_var
  
  levels_c <- c("No Controls", "Controls") #original labels
  
  # Calculate Number of Combinations/Analyses to run
  combinations <-
    length(levels_x) * length(levels_y) * length(levels_c)
  
  # Setup results frame
  results_frame <- data.frame(matrix(NA, nrow = combinations, ncol = 9))
  colnames(results_frame) <-
    c(
      "x_variable",
      "y_variable",
      "controls",
      "effect",
      "t_value",
      "p_value",
      "standard_error",
      "number",
      "rsqrd"
    )
  
  # Write combinations into results frame
  results_frame$x_variable <- rep(levels_x, each = nrow(results_frame) / length(levels_x))
  results_frame$y_variable <- rep(rep(levels_y, each = nrow(results_frame) / (length(levels_x) * length(levels_y))), times = length(levels_x))
  results_frame$controls <- rep(rep(rep(levels_c, each = nrow(results_frame) / (length(levels_x) * length(levels_y) * length(levels_c))), times = length(levels_x)), times = length(levels_y))
  return(results_frame)
}


####################################################################################
# Function "curve"
# takes results frame as an input
# returns results frame including the specification curve analysis results
####################################################################################
curve <- function(input) {
  
  results_frame <- input 
  
  print(n)
  
  for (n in 1:nrow(results_frame)) { 
    
    ###########################
    #Display progress on screen
    ###########################
    print(n)
    
    #####################################################
    # Make variables: get the mean of the selected items
    #####################################################
    
    data_short$dv <- 
      rowMeans(subset(data_short, select = results_frame$y_variable[[n]]), #whoa--- row means: making up scales with item combos
               na.rm = FALSE)
    data_short$iv <-
      subset(data_short, select = results_frame$x_variable[[n]])
    
    #################################################
    # Estimate Each Regression Model
    #################################################
    
    if (GAMM==0) {
      
      ### OLS Regression Model ###
      if (results_frame$controls[n] == "No Controls") {
        reg <- lm(scale(dv) ~ scale(iv), data = data_short)
        cont<-"Raw Associations" #Label 
      } else if (results_frame$controls[n] == "Controls") {
        if (LimitedControls==1){
          reg <- lm(
            scale(dv) ~ scale(iv) +
              scale(fd06e00) +            scale(fcpaab00) +
              scale(fpwrdscm) + scale(fdacaq00) + scale(fd05s00) +
              scale(fpclsi00) +                     scale(fdtots00) +
              scale(foede000),
            data = data_short
          )
          
          cont<-"Demo Controls Only" #Use for labeling the output/figures
        } else {
          reg <- lm(
            scale(dv) ~ scale(iv) + scale(edumot) +
              scale(fd06e00) + scale(clpar) + scale(fcpaab00) +
              scale(fpwrdscm) + scale(fdacaq00) + scale(fd05s00) +
              scale(fpclsi00) + scale(fpchti00) + scale(fdkessl) + scale(fdtots00) +
              scale(foede000),
            data = data_short
          )
          cont<-"O&P Controls" 
        }
      }
    }
    
    
    #################################################
    # Extract Variables
    #################################################
    if (GAMM==0){ # GAMM = 0, doesnt require model number
      
      results_frame$t_value[n] <-
        summary(reg)$coef[[2, 3]] %>% {
          ifelse(. == 0, NA, .)
        }
      results_frame$effect[n] <-
        summary(reg)$coef[[2, 1]] %>% {
          ifelse(. == 0, NA, .)
        }
      results_frame$p_value[n] <- summary(reg)$coef[[2, 4]]
      results_frame$standard_error[n] <-
        summary(reg)$coef[[2, 2]] %>% {
          ifelse(. == 0, NA, .)
        }
      results_frame$number[n] <- nobs(reg)
      results_frame$rsqrd[n] <- etasq(reg)["scale(iv)", "Partial eta^2"] 
      
    }
  }
  return(results_frame)
}



####################################################################################
# Execute Specification Curve Analyses for COHORT MEMBERS
# We include digital technology use (x_variables),
# mental well-being (y_variables) and controls
####################################################################################

#######################################################
# X Variables
#######################################################
if (Mod == 5) {
  x_variables <- c(
    "fctvho00r",
    "fccomh00r",
    "fccmex00r",
    "fcinth00r",
    "fcsome00r",
    "tech",
    "fcalfv00r",
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
    "fcbrkn00",
    "fcalcd00",
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
    "fcvicf0a"
    
  )
  x_names <- c(
    "Weekday TV",
    "Weekday Electronic Games",
    "Own Computer",
    "Use Internet at Home",
    "Hours of Social Media Use",
    "tech",
    "Bingedrinking",
    "Smoked Weed",
    "Bullying",
    "Been Arrested",
    "Bicycle Use",
    "Height",
    "Glasses",
    "Handedness",
    "Fruit",
    "Vegetabes",
    "Sleep",
    "Breakfast",
    "alcohol ever",
    "binge drinking ever",
    "cigarette",
    "hard drugs",
    "exercise",
    "weight",
    "obesity",
    "close friend",
    "sexual intercourse",
    "weapon carrying",
    "stopped by police",
    "suspended",
    "shoplifted",
    "fights with mom",
    "sexual assault victim")
  
}

if (Mod >2 & Mod != 5) { 
  if (IVtype == 0) { 
    x_variables <-
      c("fcsome00r") # Social Media
    x_names <-
      c(
        "Hours of Social Media Use"
      )
  }
  if (IVtype == 1) { # limit to just social media
    x_variables <-
      c("fctvho00r") # TV
    x_names <-
      c(
        "Weekday TV"
      )
  }
  
  if (IVtype == 2) { # limit to just social media
    x_variables <-
      c("fccomh00r") # computer games
    x_names <-
      c(
        "Weekday Electronic Games"
      )
  }
  
  if (IVtype == 4) { # limit to just social media
    x_variables <-
      c("fcinth00r") # Internet
    x_names <-
      c(
        "Use Internet at Home"
      )
  }
}

#######################################################
# Y Variables
# There are too many y_variable combinations
# we therefore aim to run about 100,000 tests and
# therefore want to choose about 800 y_variable 
# specifications
#######################################################

### Scales only 
if (Items== 0 & Scales==1) {
  
  #### Choose the three pre-specified variables 
  y1 <-
    c(#these are the Mood and Feelings/depression scale items with one missing mean imputed (a common approach-even if bad, but not worse than some other specifications?)
      "mfq_depress_sc1miss" #these next 3 are not used see below a y var sample creation
    )
  y2 <-
    c(#these are the Mood and Feelings/depression scale items with one missing mean imputed (a common approach-even if bad)
      "wb_sc1miss")
  y3 <-
    c(#these are the Mood and Feelings/depression scale items with one missing mean imputed (a common approach-even if bad)
      "se_sc1miss"
    )
  y4 <-
    c( #these are the Mood and Feelings/depression scale items
      "fcmdsa00r",
      "fcmdsb00r",
      "fcmdsc00r",
      "fcmdsd00r",
      "fcmdse00r",
      "fcmdsf00r",
      "fcmdsg00r",
      "fcmdsh00r",
      "fcmdsi00r",
      "fcmdsj00r",
      "fcmdsk00r",
      "fcmdsl00r",
      "fcmdsm00r"
    )
  y5 <- #these are the self-esteem items
    c("fcsati00r",
      "fcgdql00r",
      "fcdowl00r",
      "fcvalu00r",
      "fcgdsf00r")
  y6 <- #these are the life satisfaction scale items (so this will be composited)
    c("fcscwk00r",
      "fcwylk00r",
      "fcfmly00r",
      "fcfrns00r",
      "fcschl00r",
      "fclife00r")
  
  #### Bind all of these specifcations together
  y_variables_sample_cm <- c( list(y4,y5,y6)) #leaving behind the missing valued set which are not used
  save(y_variables_sample_cm, file = "2_3_sca_mcs_y_sample_cm.rda")
  length(y_variables_sample_cm)
  
  rm(y1) 
  rm(y2)
  rm(y3)
  rm(y4)
  rm(y5)
  rm(y6)
  print("scales only Y's")
}

if (Items== 1 & Scales==0) { # Original Set of Y's (Orben's)
  
  #### choose 806 randomn combinations
  y <-
    c(
      "fcmdsa00r",
      "fcmdsb00r",
      "fcmdsc00r",
      "fcmdsd00r",
      "fcmdse00r",
      "fcmdsf00r",
      "fcmdsg00r",
      "fcmdsh00r",
      "fcmdsi00r",
      "fcmdsj00r",
      "fcmdsk00r",
      "fcmdsl00r",
      "fcmdsm00r",
      "fcsati00r",
      "fcgdql00r",
      "fcdowl00r",
      "fcvalu00r",
      "fcgdsf00r",
      "fcscwk00r",
      "fcwylk00r",
      "fcfmly00r",
      "fcfrns00r",
      "fcschl00r",
      "fclife00r"
    )
  y_variables <-
    (do.call("c", lapply(seq_along(y), function(i)
      combn(y, i, FUN = list))))
  y_variables_sample <-
    sample(y_variables[-(1:length(y))], 806, replace = FALSE)
  
  #### choose all the y variables by themselves
  y4 <- y_variables[1:length(y)]
  
  #### Choose the three pre-specified variables
  y1 <-
    c(
      "fcmdsa00r",
      "fcmdsb00r",
      "fcmdsc00r",
      "fcmdsd00r",
      "fcmdse00r",
      "fcmdsf00r",
      "fcmdsg00r",
      "fcmdsh00r",
      "fcmdsi00r",
      "fcmdsj00r",
      "fcmdsk00r",
      "fcmdsl00r",
      "fcmdsm00r"
    )
  y2 <-
    c("fcsati00r",
      "fcgdql00r",
      "fcdowl00r",
      "fcvalu00r",
      "fcgdsf00r")
  y3 <-
    c("fcscwk00r",
      "fcwylk00r",
      "fcfmly00r",
      "fcfrns00r",
      "fcschl00r",
      "fclife00r")
  
  #### Bind all of these specifcations together
  y_variables_sample_cm <- c(y_variables_sample, y4, list(y1, y2, y3))
  save(y_variables_sample_cm, file = "2_3_sca_mcs_y_sample_cm.rda")
  length(y_variables_sample_cm)
  rm(y_variables)
  rm(y1)
  rm(y2)
  rm(y3)
  rm(y4)
  print("original Y's")
}

###Fidelity X??
if (Items== 1 & Scales==1) { # Original Set of Ys and New Scales. JL: New Scales? The scales are the same 
  
  #### choose 806 randomn combinations
  y <-
    c(
      "fcmdsa00r",
      "fcmdsb00r",
      "fcmdsc00r",
      "fcmdsd00r",
      "fcmdse00r",
      "fcmdsf00r",
      "fcmdsg00r",
      "fcmdsh00r",
      "fcmdsi00r",
      "fcmdsj00r",
      "fcmdsk00r",
      "fcmdsl00r",
      "fcmdsm00r",
      "fcsati00r",
      "fcgdql00r",
      "fcdowl00r",
      "fcvalu00r",
      "fcgdsf00r",
      "fcscwk00r",
      "fcwylk00r",
      "fcfmly00r",
      "fcfrns00r",
      "fcschl00r",
      "fclife00r"
    )
  y_variables <-
    (do.call("c", lapply(seq_along(y), function(i) #can this be mclappyed?
      combn(y, i, FUN = list))))
  y_variables_sample <-
    sample(y_variables[-(1:length(y))], 806, replace = FALSE)
  
  #### choose all the y variables by themselves
  y4 <- y_variables[1:length(y)]
  
  #### CREATE THE PRE-SPEFIED SCALES TO include in the mix. Old title: Choose the three pre-specified variables KMC: What is this? there are more than 3
  y1 <-
    c( #these are the Mood and Feelings/depression scale items
      "fcmdsa00r",
      "fcmdsb00r",
      "fcmdsc00r",
      "fcmdsd00r",
      "fcmdse00r",
      "fcmdsf00r",
      "fcmdsg00r",
      "fcmdsh00r",
      "fcmdsi00r",
      "fcmdsj00r",
      "fcmdsk00r",
      "fcmdsl00r",
      "fcmdsm00r"
    )
  y2 <- #these are the self-esteem items
    c("fcsati00r",
      "fcgdql00r",
      "fcdowl00r",
      "fcvalu00r",
      "fcgdsf00r")
  y3 <- #these are the life satisfaction scale items (so this will be composited)
    c("fcscwk00r",
      "fcwylk00r",
      "fcfmly00r",
      "fcfrns00r",
      "fcschl00r",
      "fclife00r")
  
  
  #### Bind all of these specifcations together
  y_variables_sample_cm <- c(y_variables_sample, y4, list(y1, y2, y3)) #this is a list of what to composite
  save(y_variables_sample_cm, file = "2_3_sca_mcs_y_sample_cm.rda")
  length(y_variables_sample_cm)
  rm(y_variables)
  rm(y1)
  rm(y2)
  rm(y3)
  rm(y4)
}



#######################################################
# Control Variables
#######################################################
controls <-
  c(
    "edumot",
    "fd06e00",
    "clpar",
    "fcpaab00",
    "fpwrdscm",
    "fdacaq00",
    "fd05s00",
    "fpclsi00",
    "fpchti00",
    "fdkessl",
    "fdtots00",
    "foede000"
  )

if (LimitedControls==1) { # Based off what KC and JL discussed. Age included for now. 
  controls <-
    c(
      "fd06e00",
      "fcpaab00",
      "fpwrdscm",
      "fdacaq00",
      "fd05s00",
      "fpclsi00",
      "fdtots00",
      "fccage00", #age or not? JL: doesn't seem to change values of median_effect
      "foede000"
    )
  print("running limited controls")
}

#######################################################
# Subset Data
#######################################################
data_short <-
  data[, c(
    "fctvho00r",
    "fccomh00r",
    "fccmex00r",
    "fcinth00r",
    "fcsome00r",
    "tech",
    "fcalfv00r",
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
    "fcbrkn00",
    "fcalcd00",
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
    "fcvicf0a", ###JL: end of added variables. This currently includes all comparison variables. 
    "fctvho00r",
    "fccomh00r",
    "fccmex00r",
    "fcinth00r",
    "fcsome00r",
    "fcmdsa00r",
    "fcmdsb00r",
    "fcmdsc00r",
    "fcmdsd00r",
    "fcmdse00r",
    "fcmdsf00r",
    "fcmdsg00r",
    "fcmdsh00r",
    "fcmdsi00r",
    "fcmdsj00r",
    "fcmdsk00r",
    "fcmdsl00r",
    "fcmdsm00r",
    "fcsati00r",
    "fcgdql00r",
    "fcdowl00r",
    "fcvalu00r",
    "fcgdsf00r",
    "fcscwk00r",
    "fcwylk00r",
    "fcfmly00r",
    "fcfrns00r",
    "fcschl00r",
    "fclife00r",
    "edumot",
    "fd06e00",
    "clpar",
    "fcpaab00",
    "fpwrdscm",
    "fdacaq00",
    "fd05s00",
    "fpwrdscm",
    "fpclsi00",
    "fpchti00",
    "fdkessl",
    "fdtots00",
    "foede000",
    "tech",
    "mfq_depress_sc", #built into the data but not used in models
    "mfq_depress_sc1miss", #built into the data but not used in models
    "wb_sc" , #built into the data but not used in models
    "wb_sc1miss", #built into the data but not used in models
    "se_sc",#built into the data but not used in models
    "se_sc1miss"#built into the data but not used in models
  )]


#print(y_variables_sample_cm) 

#######################################################
# Run and Save
#######################################################

results_mcs_sca_cm <-
  curve(resultsframe(x_var = x_variables, y_var = y_variables_sample_cm)) #If undefined error, ensure data_short has all variables necessary 
save(results_mcs_sca_cm, file = "2_3_sca_mcs_results_cm.rda")
print("Cohort Member Complete")

####################################################################################
# Execute Specification Curve Analyses for PARENTS
# We include digital technology use (x_variables),
# mental well-being (y_variables) and controls
####################################################################################

#######################################################
# Y Variables
# Similar procedure as above
# We choose less randomn combinations as there
# are more measures and we want equal number of
# specifications for cohort members and parents
#######################################################

if (Items== 1 & Scales==0 ) {
  
  y <-
    c(
      "fpsdpf00",
      "fpsdro00",
      "fpsdhs00",
      "fpsdsr00",
      "fpsdtt00",
      "fpsdsp00",
      "fpsdor00",
      "fpsdmw00",
      "fpsdhu00",
      "fpsdfs00",
      "fpsdgf00",
      "fpsdfb00",
      "fpsdud00",
      "fpsdlc00",
      "fpsddc00",
      "fpsdnc00",
      "fpsdky00",
      "fpsdoa00",
      "fpsdpb00",
      "fpsdvh00",
      "fpsdst00",
      "fpsdcs00",
      "fpsdgb00",
      "fpsdfe00",
      "fpsdte00"
    )
  y_variables <- (do.call("c", lapply(seq_along(y), function(i) combn(y, i, FUN = list))))
  y_variables_sample <- sample(y_variables[-(1:length(y))], 801, replace = FALSE)
  
  # Choose variables by themselves
  y1 <- y_variables[1:length(y)]
  
  # Choose mean variables
  y2 <- c("fconduct")
  y3 <- c("fhyper")
  y4 <- c("fpeer")
  y5 <- c("fprosoc")
  y6 <- c("febdtot") # total
  y7 <- c("femotion")
  y8 <- c("femotion", "fpeer")
  y9 <- c("fconduct", "fhyper")
  
  # bind data
  y_variables_sample_pr <- c(y_variables_sample, y1,
                             list(y2,y3,y4,y5,y6,y7,y8,y9))
  save(y_variables_sample_pr, file = "2_3_sca_mcs_y_sample_pr.rda")
  length(y_variables_sample_pr)
  
}

### Fidelity X, this part is probably wrong... 
if(Items ==1 & Scales ==1){
  y <-
    c("fpsdhs00",
      "fpsdmw00",
      "fpsdud00",
      "fpsdnc00",
      "fpsdfe00"
    )
  
  y_variables <- (do.call("c", lapply(seq_along(y), function(i) combn(y, i, FUN = list)))) # combn makes all combos. KMC: unwind these lines
  if (length(y)>801){ #less combos than 801 and this will fail. why doesn't the indi items get redistributed too? (no, to ensure they are in the anaysis!!!
    y_variables_sample <- sample(y_variables[-(1:length(y))], 801, replace = TRUE) #less combos than 801
  } else {
    y_variables_sample<-y_variables[-(1:length(y))]
  }
  
  # Choose variables by themselves
  y1 <- y_variables[1:length(y)] # these are pulled seperately to ensure they are included because a sample might miss them.
  
  # Choose mean variables KMC: which seems to be required.
  y7 <- c("femotion")
  y8 <-c("fcharm00")
  
  
  # bind data ----KMC is this the point where threading happens...
  y_variables_sample_pr <- c(y_variables_sample, y1, # KMC: is y1 a repeate of y_variables_sample?
                             list(y7))
  if (SelfHarm==1) {
    y_variables_sample_pr <- c(y_variables_sample, y1, # KMC: is y1 a repeate of y_variables_sample?
                               list(y7, y8))
  }
  
  save(y_variables_sample_pr, file = "2_3_sca_mcs_y_sample_pr.rda")
  length(y_variables_sample_pr)
}


### Original Model Specs
if (EmoItemOnly==1){ # Components from emotional item scale only 
  
  
  if (Items== 0 & Scales==1) {# Original Set of Ys (which is items and SDQ scale)
    
    y7 <- c("femotion")
    
    y_variables_sample_pr <- c(
      list(y7)) #limit to femotion
    
    save(y_variables_sample_pr, file = "2_3_sca_mcs_y_sample_pr.rda")
    length(y_variables_sample_pr)
    
  }
}

###################################
### Branch: Just the Scales for Ys
##################################

data_short <- 
  data[, c(
    "fctvho00r",
    "fccomh00r",
    "fccmex00r",
    "fcinth00r",
    "fcsome00r",
    "tech",
    "fcalfv00r",
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
    "fcbrkn00",
    "fcalcd00",
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
    "fcvicf0a", ### end of added variables 
    "fctvho00r",
    "fccomh00r",
    "fccmex00r",
    "fcinth00r",
    "fcsome00r",
    "fpsdpf00",
    "fpsdro00",
    "fpsdhs00",
    "fpsdsr00",
    "fpsdtt00",
    "fpsdsp00",
    "fpsdor00",
    "fpsdmw00",
    "fpsdhu00",
    "fpsdfs00",
    "fpsdgf00",
    "fpsdfb00",
    "fpsdud00",
    "fpsdlc00",
    "fpsddc00",
    "fpsdnc00",
    "fpsdky00",
    "fpsdoa00",
    "fpsdpb00",
    "fpsdvh00",
    "fpsdst00",
    "fpsdcs00",
    "fpsdgb00",
    "fpsdfe00",
    "fpsdte00",
    "fconduct",
    "fhyper",
    "fpeer",
    "fprosoc",
    "febdtot",
    "femotion",
    "edumot",
    "fd06e00",
    "clpar",
    "fcpaab00",
    "fpwrdscm",
    "fdacaq00",
    "fd05s00",
    "fpwrdscm",
    "fpclsi00",
    "fpchti00",
    "fdkessl",
    "fdtots00",
    "foede000",
    "fcharm00" #add in self-harm?
  )]
print("Parent Complete")

# run and save
results_mcs_sca_pr <- curve(resultsframe(x_var = x_variables, y_var = y_variables_sample_pr)) 

if (dropSDQ==1) {
  results_mcs_sca_pr[1:nrow(results_mcs_sca_pr),c(4:9)]<-NA
}

save(results_mcs_sca_pr, file = "2_3_sca_mcs_results_pr.rda")
print("End 2_3_sca_mcs")
