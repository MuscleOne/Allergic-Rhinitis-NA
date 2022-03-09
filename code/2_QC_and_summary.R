#### 1. load the required package #####
library(psych)

#### 2. access the data as dataframe ####
# generate a dataframe of items name 
df_raw_input_item = read.csv("data/raw/raw_items.csv")
df_scaled_items = read.csv("data/cleaned/scaled_items.csv")

############## to conduct quality control ######################################
#### 3. computation of the alpha based on raw data #####
alpha_input = df_raw_input_item[-1]

#### the alpha of arct results 
alpha_output_arct = psych::alpha(alpha_input[1:5])
alpha_output_arct

#### the alpha of vas results 
alpha_output_vas = psych::alpha(alpha_input[6:14])
alpha_output_vas

### the alpha of the total results 
alpha_output = psych::alpha(alpha_input, check.keys=TRUE)
alpha_output
# Reliability analysis   
# Call: psych::alpha(x = alpha_input, check.keys = TRUE)
# 
#   raw_alpha std.alpha G6(smc) average_r S/N    ase mean  sd median_r
#        0.86      0.87     0.9      0.32 6.6 0.0087  4.2 1.2     0.31
# 
#   lower alpha upper     95% confidence boundaries
#  0.85 0.86 0.88 


#### 4. case with squaring of vas and then z-scored normalizing ####
alpha_scaled_output = psych::alpha(df_scaled_items[-1])
alpha_scaled_output
# Reliability analysis   
# Call: psych::alpha(x = df_scaled_items[-1])
# 
#   raw_alpha std.alpha G6(smc) average_r S/N  ase    mean  sd median_r
#        0.86      0.86     0.9      0.31 6.2 0.01 2.2e-16 0.6      0.3
# 
#  lower alpha upper     95% confidence boundaries
# 0.84 0.86 0.88

#### 5. create a dataframe to store the results of the arct and vas ############
df_alpha_results = data.frame(
  ARCT = c("0.84(0.81, 0.86)"),
  VAS = c("0.86(0.84, 0.88)"),
  "ARCT and VAS" = c("0.86(0.85, 0.88)")
)
rownames(df_alpha_results) = c("Estimate Alpha, mean, (95% CI)")
df_alpha_results = as.data.frame(t(df_alpha_results)) # apply "as.data.frame" after transposing
df_alpha_results
#                          Estimate Alpha, mean, (95% CI)
# ARCT                                   0.84(0.81, 0.86)
# VAS                                    0.86(0.84, 0.88)
# ARCT.and.VAS                           0.71(0.68, 0.75) 

### write the dataframe into a csv file 
write.csv(df_alpha_results, "outputs/alpha_results_raw.csv")

############## to find summary statistics ######################################

##### 6. prepare column name for further treatment #####
# mode and variation ratio should be reported
colnames(alpha_input)
item_name = stringr::str_replace_all(colnames(alpha_input), c("\\." = " "))
item_name
# [1] "effect on activities"      "make irritable"            "disturb sleep"            
# [4] "need additional treatment" "assess AR control"         "sneezing"                 
# [7] "rhinorrhoea"               "nasal congestion"          "nasal pruritus"           
# [10] "itchy eyes"                "red eyes"                  "watery eyes"              
# [13] "smell disorders"           "eyelid swelling" 


stats_arct_input = alpha_input[1:5]
stats_vas_input = alpha_input[6:14]

##### 7. summary statstics in vas ######
## calculate median and 25th, 75th for vas terms
# a helper function to get 25th, 75th and median of a vecter

get_median = function(v){
  summary(v)[c(2,3,5)]
}

stats_vas_output_raw = t(apply(stats_vas_input, 2, get_median))

### to generate a publish-grade dataframe
stats_vas_output = NULL # create a na dataframe with the same dimension
for (row in 1:nrow(stats_vas_output_raw)) {
  stats_vas_output = 
    c(stats_vas_output, 
      paste0(stats_vas_output_raw[row, 2], "(", 
             stats_vas_output_raw[row, 1], ", ",
             stats_vas_output_raw[row, 3], ")") #paste0
    )
}
names(stats_vas_output) = item_name[6:14]
stats_vas_output = as.data.frame(stats_vas_output)
colnames(stats_vas_output) = c("Median(1st Qu., 3rd Qu.)")
stats_vas_output
#                  Median(1st Qu., 3rd Qu.)
# sneezing                          4(2, 6)
# rhinorrhoea                       4(2, 6)
# nasal congestion               5(2.75, 7)
# nasal pruritus                    5(2, 6)
# itchy eyes                        3(2, 5)
# red eyes                          0(0, 3)
# watery eyes                       1(0, 2)
# smell disorders                   0(0, 0)
# eyelid swelling                   0(0, 1)


##### 8. summary statistics in arct ##########
## a helper function to get mode
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
## a helper function to get Variation ratio
get_VR = function(v) {
  mode = get_mode(v)
  N_mode = length(v[v==mode])
  N_total = length(v)
  VR = 1-(N_mode/N_total)
  return(as.numeric(substr(VR, 1, 5)))
}

# to get mode and vr
arct_mode = apply(stats_arct_input, 2, get_mode)
arct_VR = apply(stats_arct_input, 2, get_VR)

stats_arct_output = data.frame(
  mode = arct_mode,
  VR= arct_VR
)

colnames(stats_arct_output) = c("Mode", "Variation Ratio")
row.names(stats_arct_output) = item_name[1:5]
stats_arct_output
#                           Mode Variation Ratio
# effect on activities         4           0.693
# make irritable               3           0.696
# disturb sleep                4           0.649
# need additional treatment    5           0.360
# assess AR control            2           0.700

#### 9. save the dataframe results as csv files ####
write.csv(stats_vas_output, "outputs/stats_vas_output.csv")
write.csv(stats_arct_output, "outputs/stats_arct_output.csv")

