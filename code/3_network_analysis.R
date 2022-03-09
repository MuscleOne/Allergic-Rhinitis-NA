#### 0. load the required package ####

# # rm(list=ls())
# library(dplyr)
# library(foreign)
# # library("devtools")
# # devtools::install_github("sachaepskamp/bootnet")
# library(bootnet)
# # devtools::install_github("sachaepskamp/qgraph")
# library(qgraph)
# # devtools::install_github("jmbh/mgm")
# library(mgm)
# library(networktools)

## create a function that help check if the required packages are installed
use_package <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

pckg = c("dplyr", "foreign", "mgm", "bootnet", "networktools", "qgraph", "ggplot2", "psych")
for(i in 1:length(pckg)) {
  use_package(pckg[i])
}

#### 1. load the data as dataframe from the csv files ####

# df_scale_input_item = l_sqrt_scale_item$df_sqrt_scale_item
# df_item_name = l_preprocessed_data$df_item_name

# generate a dataframe of items name 
df_scaled_items = read.csv("data/cleaned/scaled_items.csv")
df_item_name = read.csv("data/cleaned/items_name.csv")

#### 2. load the helper function and then visualize the pattern ####
## import the important function and code
plot_items_pattern = function( data.df, colNames=NULL) {
  ##*******************************************************************************
  ## function to plot a vector of continuous values using boxplot or              #
  ##      a vector of discrete values using bar plot                              #
  ## Author: Xiaohua Douglas Zhang, January 2020                                  #
  ## Arguments:                                                                   #
  ##   data.df: a data.frame to be plotted using boxplot or                       #
  ##            a discrete vector to be plotted using bar plot                    #
  ##   colNames: names for the columns in data.df                                 #
  ## Output:                                                                      #
  ##   None                                                                       #
  ##******************************************************************************* 
  
  if( is.null(colNames) ) colNames = colnames(data.df)
  nColumn = length(colNames)
  for(i in 1:nColumn ) {
    x = unlist(data.df[,i])
    if( is.character(x) | is.factor(x) | is.logical(x) ) {
      barplot( table(x), main=colNames[i] )
    } else if( is.numeric(x) ) {
      hist( x, main=colNames[i] )
    } else {
      stop(paste("the", i, "th column of data.df must be character, numeric, factor or logical."))
    }
  }
}

## visualization of the raw data
par(mfrow=c(4,4))
df_scaled_items[,-1] %>% plot_items_pattern
# save the results of the patterns of the items data.
png("figures/exploratory/patterns_scaled_items.png", 
    res=240, width = 4800, height = 4800, units = "px", pointsize = 28)
par(mfrow=c(4,4))
df_scaled_items[,-1] %>% plot_items_pattern
dev.off()

## check the correlation between the items
pairs.panels(df_scaled_items[-1],
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "pearson", # Correlation method (also "spearman" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals
corPlot(df_scaled_items[-1], upper=F, xlas=2,
        main="Correlation plot of the z-score normalized items from the ARCT and VAS")
### save the visualization of the pair correlation
png("figures/exploratory/correlation_plot.png", 
    res=240, width = 2000, height = 1000, units = "px", pointsize = 10)
par(mfrow=c(1,1))
corPlot(df_scaled_items[-1], upper=F, xlas=2,
        main="Correlation plot of the z-score normalized items from the ARCT and VAS")
dev.off()

#### 3. Create names object & group (communities) object for graph #####
vec_item_label = df_item_name[,'var_name']

l_group_label = list("ARCT"=c(1:5), "Nasal symptoms"=c(6:9,13), "ocular symptoms"=c(10:12,14))
ar_nodenames = df_item_name[, 'item_name_en']

#### 4. create a correlation matrix #####
## remenber to remove the patients idx at first
data_ar_cor = df_scaled_items[,-1] %>% cor_auto

#### 5. create a graph object, visualize it and save it #####
### graph object and it visualization is generated via the correlation matrix 
graph_ar_m = data_ar_cor %>% EBICglasso(n=nrow(df_scaled_items)) 
par(mfrow=c(1,1))
graph_ar_g = data_ar_cor %>% qgraph(labels=vec_item_label, graph="glasso", layout="spring", 
                                    vsize=6, cut=0, maximum=.60, sampleSize = nrow(df_scaled_items),
                                    groups=l_group_label, 
                                    color=c("#A31E39", "#485C5A", "#242E35"), 
                                    label.scale=T, label.prop=.8,  borders=F, label.color="#FFE9E8", 
                                    nodeNames = ar_nodenames, legend.cex=.4, 
                                    edge.label.cex=0.8)

# save the visualization of the network model
png("figures/network_model/AR_network.png", 
    res=240, width = 1600, height = 1000, units = "px", pointsize = 10)
par(mfrow=c(1,1))
graph_ar_g %>% plot()
dev.off()

##### 6. Extract the centrality measurements and visualize them ####
# graph_ar_g %>% centralityPlot(include = c("Strength"), theme_bw=F, labels = ar_nodenames)
# extract the values of strength
df_centrality = centralityTable(graph_ar_g, labels=ar_nodenames)
df_strength = df_centrality[df_centrality[, "measure"]=="Strength", c("node", "value", "measure")]

# extract the values of bridge strength
bridge_strength = scale(bridge(graph_ar_m, communities=l_group_label)$`Bridge Strength`)
# df_bridge_strength = NULL
df_bridge_strength = data.frame(
  node = ar_nodenames, 
  value = bridge_strength, 
  measure = rep("Bridge Strength", 14)
)
row.names(df_bridge_strength) = NULL

### row bind the two dataframe together 
df_centrality_statistics = rbind(df_bridge_strength, df_strength)
# df_centrality_statistics[, "node"]

### visualize the centrality statistics
plot_centrality_statistics = 
  ggplot(df_centrality_statistics, aes(x=node, y=value, group=measure)) +
  geom_line(aes(color=measure))+
  geom_point(aes(color=measure))+
  theme(legend.position="top")+
  theme(axis.text.x = element_text(angle = 40, hjust=1))+
  labs(x="Nodes (Items)", y="Z-Score",
       title="Centrality Measure of the Network")
# save the result
png("figures/network_model/centrality_statistics.png", 
    res=240, width = 2000, height = 1000, units = "px", pointsize = 20)
par(mfrow=c(1,1))
plot_centrality_statistics
dev.off()


# #### intercorrelation of different centrality measures
# cen_ar = graph_ar_g %>% centrality
# cor(cen_ar$InDegree, cen_ar$Closeness, method="pearson") # 0.64 spearman=0.53
# cor(cen_ar$InDegree, cen_ar$Betweenness, method="pearson")  # 0.38 spearman=0.37
# cor(cen_ar$Betweenness, cen_ar$Closeness, method="pearson") # 0.73 spearman=0.79

#### 7. Robustness analysis ####

## evaluate the stability of our network model
# convert the name of the nodes, to item number
df_scaled_items_robust = df_scaled_items[, -1]
# colnames(df_scaled_items_robust) = c(1:length(df_scaled_items[, -1]))

### Estimate a network structure via bootnet package, for further bootstrap operation
network_ar = df_scaled_items_robust %>% estimateNetwork(default = "EBICglasso") 

### Bootstrapped network estimation, so that the spread of parameter and centrality estimates can be assessed.
# edge weights bootstrap network ar
boot_ar_1 = network_ar %>% bootnet(nBoots = 1000, 
                                   statistics=c("edge", "strength", "bridgeStrength"), 
                                   communities = l_group_label, nCores = 4) 
# for the case-dropping bootstrap
boot_ar_2 = network_ar %>% bootnet(nBoots = 1000, type = "case", 
                                   statistics=c("strength", "bridgeStrength"), 
                                   communities = l_group_label, nCores = 4) # subsetting bootstrap network 1

# save the ggplot2 object of plotting of boot_ar_1 object
plot_edge_difference = boot_ar_1 %>% plot("edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
plot_boot_centrality = boot_ar_1 %>% plot(c("strength", "bridgeStrength"))

# To obtain the correlation stability coefficient, which denotes the estimated 
# maximum number of cases that can be dropped from the data to retain
corStability(boot_ar_2) # "CS-coefficient should not be below 0.25, and preferably above 0.5" (bootnet paper)
# === Correlation Stability Analysis === 
#   
# Sampling levels tested:
#    nPerson Drop%   n
# 1      102  75.0  95
# 2      134  67.2  96
# 3      165  59.6  86
# 4      197  51.7 104
# 5      229  43.9  90
# 6      261  36.0  96
# 7      292  28.4 115
# 8      324  20.6 114
# 9      356  12.7 102
# 10     388   4.9 102
# 
# Maximum drop proportions to retain correlation of 0.7 in at least 95% of the samples:
#   
# bridgeStrength: 0.75 (CS-coefficient is highest level tested)
# - For more accuracy, run bootnet(..., caseMin = 0.672, caseMax = 1) 
# 
# strength: 0.75 (CS-coefficient is highest level tested)
# - For more accuracy, run bootnet(..., caseMin = 0.672, caseMax = 1) 
# 
# Accuracy can also be increased by increasing both 'nBoots' and 'caseN'.


### save the result of bootstrapping analysis

png("figures/network_model/boot_mean_edge.png", 
    res=240, width = 2000, height = 1000, units = "px", pointsize = 20)
par(mfrow=c(1,1))
plot(boot_ar_1, labels = F, order="sample") 
dev.off()

png("figures/network_model/case_dropping.png", 
    res=240, width = 1000, height = 1000, units = "px", pointsize = 20)
par(mfrow=c(1,1))
plot(boot_ar_2, statistics=c("strength","bridgeStrength"))
dev.off()

png("figures/network_model/edge_differnece.png", 
    res=240, width = 2500, height = 2500, units = "px", pointsize = 15)
par(mfrow=c(1,1))
plot(plot_edge_difference, useDingbats=T) 
dev.off()

png("figures/network_model/boot_centrality.png", 
    res=240, width = 2000, height = 1000, units = "px", pointsize = 20)
par(mfrow=c(1,1))
plot(plot_boot_centrality, useDingbats=FALSE)
dev.off()


# ######## save the result of network analysis #####
# l_NA_boot_results_23Dec = list(
#   boot_ar_1 = boot_ar_1, 
#   boot_ar_2 = boot_ar_2,
#   boot_ar_3 = boot_ar_3, 
#   boot_ar_4 = boot_ar_4
# )
# l_NA_boot_results_23Dec %>% save(file = "results/2021-12-23_l_NA_boot_results_23Dec.RData")
# l_NA_boot_results_23Dec %>% saveRDS(file = "results/2021-12-23_l_NA_boot_results_23Dec.Rds")



