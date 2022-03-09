####### 1. import the neccessary package, data and function #####
plot_items_pattern = function( data.df, colNames=NULL) {
  ##*******************************************************************************
  ## fucntion to plot a vector of continuous values using boxplot or              #
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

df_raw_input_item = read.csv("data/raw/raw_items.csv")

#### 2. visualization of the raw data #####
par(mfrow=c(4,4))
plot_items_pattern(df_raw_input_item[,-1])

# save the visualization result
grDevices::png("figures/exploratory/patterns_raw_items.png", 
    res=240, width = 4800, height = 4800, units = "px", pointsize = 28)
par(mfrow=c(4,4))
plot_items_pattern(df_raw_input_item[,-1])
dev.off()

##### 3. convert the data ######
## split the raw dataframe
df_raw_arct = df_raw_input_item[,1:6]
df_raw_vas = df_raw_input_item[,c(1,7:15)]

## to treat the arct case
df_arct_scale = df_raw_arct
df_arct_scale[,-1] = scale(df_raw_arct[,-1])
mat_arct_scale = as.matrix(df_arct_scale[,-1])
df_arct_scale[,-1] = as.data.frame(-mat_arct_scale)

# visualization the scaled arct data
par(mfrow=c(2,3))
plot_items_pattern(df_raw_arct[,-1])
plot_items_pattern(df_arct_scale[,-1])

## to treat the vas data
df_vas_scale = df_raw_vas

df_vas_scale[,-1] = scale(sqrt(df_raw_vas[,-1]))
par(mfrow=c(2,3))
plot_items_pattern(df_raw_vas[,-1])
plot_items_pattern(df_vas_scale[,-1])

colnames(df_raw_vas)

## merge the data together 
df_scaled_items = merge(df_arct_scale, df_vas_scale, 
                        by.x = "patients_idx", by.y = "patients_idx", 
                        all.x=FALSE, all.y=FALSE)
# ok
head(df_scaled_items)
# head(scaled_items)

# write.csv(df_scaled_items, "data/cleaned/scaled_items.csv", row.names = F)





