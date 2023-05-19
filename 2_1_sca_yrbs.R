##########################################################################################
# Specification Curve Analysis and Digital Technology Use
# R-Script 2.1: YRBS Make Specification Curves
##########################################################################################

data <- read.csv("1_1_prep_yrbs_data.csv", header=TRUE, sep = ",") # current dataset 

if(Sex == 0 ){
  data <- data[data$sex_n==0,]
}else if (Sex ==1){
  data <- data[data$sex_n==1,]}
#######################################################
# Load libraries
#######################################################
library(tidyr)
library(dplyr)
library(heplots)

####################################################################################
# Function "resultsframe"
# takes character vector of outcome variables as an input
# returns the results vector of all specifications
####################################################################################
resultsframe <- function(x_var, y_var) {
  
  # Setup Specification Names
  levels_x <- x_var
  levels_y <- y_var
  levels_cont <- c("No Controls", "Controls")
  levels_comb <- c("Mean", "1 or more")
  
  # Calculate Number of Combinations/Analyses to run
  combinations <-
    length(levels_x) * length(levels_y) * length(levels_cont) * length(levels_comb)
  
  # Setup results frame
  results_frame <-
    data.frame(matrix(NA, nrow = combinations, ncol = 9))
  colnames(results_frame) <-
    c(
      "x_variable",
      "y_variable",
      "controls",
      "combination",
      "effect",
      "number",
      "p_value",
      "standard_error",
      "rsqrd"
    )
  
  # Write combinations into results frame
  results_frame$x_variable <- rep(levels_x, each = nrow(results_frame) / length(levels_x))
  results_frame$y_variable <- rep(rep(levels_y, each = nrow(results_frame) / (length(levels_x) * length(levels_y))), times = length(levels_x))
  results_frame$controls <- rep(rep(rep(levels_cont,each = nrow(results_frame) / (length(levels_x) * length(levels_y) * length(levels_cont))), times = length(levels_x)), times = length(levels_y))
  results_frame$combination <- rep(rep(rep(rep(levels_comb,each = nrow(results_frame) / (length(levels_comb) * length(levels_x) * length(levels_y) * length(levels_cont))), times = length(levels_x)), times = length(levels_cont)))
  return(results_frame)
  return(combinations)
}

####################################################################################
# Function "curve"
# takes results frame as an input
# returns results frame including the specification curve analysis results
####################################################################################
curve <- function(input) {
  results_frame <- input
  
  for (n in 1:nrow(results_frame)) {
    
    print(n/nrow(results_frame))
    
    #################################################
    # Make variables
    #################################################
    
    # Select Combination of wellbeing Variables
    if (results_frame$combination[n] == "Mean") {
      data_short$dv <-
        rowMeans(subset(data_short, select = results_frame$y_variable[[n]]),
                 na.rm = FALSE)
    } else {
      d1 <-
        apply((subset(data_short, select = results_frame$y_variable[[n]])), 1, sum)
      data_short$dv <- ifelse(d1 < length(results_frame$y_variable[[n]]), 0, ifelse(is.na(d1), NA, 1))
    }
    
    # Select technology
    data_short$iv <- subset(data_short, select = results_frame$x_variable[[n]])
    
    #################################################
    # Run Correlations
    #################################################
    
    if (results_frame$controls[n] == "No Controls") {
      reg <- lm(scale(dv) ~ scale(iv), data = data_short) 
    } else if (results_frame$controls[n] == "Controls") {
        if(LimControls == 1) {
          reg <-
           lm(scale(dv) ~ scale(iv) +scale(race_di), 
              data = data_short)
        }else {
          reg <-
            lm(scale(dv) ~ scale(iv) + as.factor(race4) + as.factor(grade), 
               data = data_short)
        }
    }
    
    #################################################
    # Extract Variables
    #################################################
    
    results_frame$t_value[n] <-
      summary(reg)$coef[[2, 3]] %>% {
        ifelse(. == 0, NA, .)
      }
    results_frame$effect[n] <-
      summary(reg)$coef[[2, 1]] %>% {
        ifelse(. == 0, NA, .)
      }
    results_frame$number[n] <- nobs(reg)
    results_frame$p_value[n] <- summary(reg)$coef[[2, 4]]
    results_frame$standard_error[n] <-
      summary(reg)$coef[[2, 2]] %>% {
        ifelse(. == 0, NA, .)
      }
    results_frame$rsqrd[n] <- etasq(reg)["scale(iv)", "Partial eta^2"]
  }
  return(results_frame)
}

####################################################################################
# Execute Specification Curve Analyses
# We include digital technology use (x_variables),
# mental well-being (y_variables),
# controls and combination methods
####################################################################################

#######################################################
# X Variables
#######################################################
#recode asthma so that 1 = no asthma, 2 not sure, 3 = asthma  JL: taken from 5_1
data$q87r <- ifelse(data$q87 == 1, 3, ifelse(data$q87 == 2, 1, 2))
#x_variables <- c("q81_n", "q82_n")
x_variables <- c('tech')

# x_variables <- c(
#   "qn41", #41 is 40
#   "q11", #11 is 10
#   "qn31", #31 is 30
#   "qn52", #52 is 51
#   "qn58",
#   "qn39", #q39 is q34
#   "q80",
#   "q84",#q84?
#   "qnobese",
#   "q13",
#   "q21", #21 is 19
#   "q17",
#   "qn61",
#   "qn62",
#   "q72", #72 is 71
#   "q79", #79 is 78
#   "q76", #76 is 75
#   "q88", #stays
#   "q74", #74 is 73
#   "q78", #78 is 77
#   "q69", #69 is 68
#   "q87r", #stays
#   "q47", #47 is 46
#   "q44", #stays?
#   "q24_n",
#   "q18", #18 is 17
#   "tech"
# )

#x_names <- c("TV Use", "Electronic Device Use")
x_names <- c('Technology Use') 

# x_names <-
#   c(
#     "Alcohol(ever)",
#     "Drink and Drive",
#     "Cigarette(ever)",
#     "Heroin(ever)",
#     "Injection(ever)",
#     "Vaping",
#     "Exercise",
#     "Sports Team",
#     "Obesity",
#     "Weapon Carry",
#     "Force Sex",
#     "Threatened at School",
#     "Sex before 13",
#     "4+ Sexual Partners",
#     "Eat fruit",
#     "Eat breakfast",
#     "Eat vegetables",
#     "hours sleep",
#     "Eat potatoes",
#     "Drink Milk",
#     "Perceived Weight",
#     "Asthma",
#     "Marijuana",
#     "Bingedrinking",
#     "Bullied",
#     "Fight",
#     "technology use"
#   )
#######################################################
# Y Variables
#######################################################
if(items == 0 & scales == 1){
  y <-  c("q26_n", "q27_n", "q28_n", "q29_nd")#, "q30_nd")
  y1 <- c("suic")
  
  y_variables <-
    (do.call("c", lapply(seq_along(y), function(i)
      combn(y, i, FUN = list))))
  
  y_variables <-c(y_variables,y1) #force in scale 
  
  y_names <-
    c("loneliness",
      "think suicide",
      "plan suicide",
      "commit suicide",
      "jean var")
}

if(items == 1 & scales == 0){
  y <-  c("q26_n", "q27_n", "q28_n", "q29_nd", "q30_nd")
  
  y_variables <-
    (do.call("c", lapply(seq_along(y), function(i)
      combn(y, i, FUN = list))))
  
  y_names <-
    c("loneliness",
      "think suicide",
      "plan suicide",
      "commit suicide",
      "doctor suicide")
}


#######################################################
# Combination Method
#######################################################
c_variables <- c("Means", "Choose 1")
c_names <- 2 # 1 = means, 2 = choose 1

#######################################################
# Controls
#######################################################
controls <- c("race_di", "grade")
s_names <- c("dichotomous race", "grade")

#######################################################
# Subset Data
#######################################################
data_short <- data[, c(
  "qn41", #41 is 40
  "q11", #11 is 10
  "qn31", #31 is 30
  "qn52", #52 is 51
  "qn58",
  "qn39", #q39 is q34
  "q80",
  "q84",#q84?
  "qnobese",
  "q13",
  "q21", #21 is 19
  "q17",
  "qn61",
  "qn62",
  "q72", #72 is 71 
  "q79", #79 is 78 
  "q76", #76 is 75 
  "q88", #stays
  "q74", #74 is 73
  "q78", #78 is 77 
  "q69", #69 is 68  
  "q87r", #stays
  "q47", #47 is 46
  "q44", #stays?
  "q24_n",
  "q18", #18 is 17 
  "tech",
  "q26_n",
  "q27_n",
  "q28_n",
  "q29_nd",
  "q30_nd",
  "year",
  "race_di",
  "race4",
  "suic",
  "grade",
  "q81_n",
  "q82_n"
)]


#######################################################
# Run and Save
#######################################################
results_yrbs_sca <-
  curve(resultsframe(x_var = x_variables, y_var = y_variables))

save(results_yrbs_sca, file = "2_1_sca_yrbs_results.rda")
