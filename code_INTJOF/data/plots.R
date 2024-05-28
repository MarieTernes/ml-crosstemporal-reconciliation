# Script to generate the demand plot for Citi Bike
rm(list=ls())
# Set working directory to the folder where this script is contained in
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Libraries
library(ggplot2) # version 3.5.0
library(FoReco) # version 0.2.6
library(tidyverse) # version 2.0.0

# Function
aggregate_ts_hierachy <- function(X, agg_orders){
  X_agg_list <- vector(mode = "list", length = length(agg_orders))
  names(X_agg_list) <- paste0("m",agg_orders) #c("min30", "hourly","daily","weekly")
  X_agg_list[[1]] = X

  for(tl in 2:(length(agg_orders))){
    X_agg = apply(X, 2, agg_ts, agg_order =  agg_orders[tl])
    if(class(X_agg)[1] == "numeric"){
      X_agg <- t(as.matrix(X_agg))
    }
    X_agg_list[[tl]] = X_agg
  }

  return(X_agg_list)
}

# Load the data
load("citibike.RData")


ncols_info <- 5 # number of columns that contain info
area_data <- citibike$citibike
area_data$halfhour_bin <- rep(1:48, nrow(area_data)/48)

# Cross-sectional information #
n_cs <- 7 # total number of all cross-sectional-units
# Temporal aggregation information #
ts_aggr_orders = c(1,2,3,4,6,8,12,16,24,48) #30-min, 1-hour, 1.5-hour, 2-hour, 3-hour, 4-hour, 6-hour, 8-hour, 12-hour, 24-hour
demand_window <- area_data[,-c(1:ncols_info)] #need to delete away info columns before temporal aggregation
demand_hierarchy <- aggregate_ts_hierachy(demand_window, ts_aggr_orders) # list with elements decreasing in frequency list[[1]] = highest_frequency (30-min),..., list[[n_ts]] = lowest_frequency (weekly)

data_avg_halfhour = matrix(NA, nrow = max(ts_aggr_orders), ncol = n_cs)

for(i in 1:n_cs){
  data_sub = area_data[, c(1:ncols_info, ncols_info+i, ncol(area_data))]

  avg_halfhour <- data_sub %>%
    group_by(halfhour_bin) %>%
    summarise(across(colnames(data_sub)[ncols_info+1], ~ mean(.)))

  data_avg_halfhour[,i] = unlist(avg_halfhour[,2])
}

data = t(data_avg_halfhour)
rownames(data) = c("NYC", paste0('area', 1:6))
combine_data = data
df = data.frame(x = rep(1:48, each = nrow(combine_data)),
                y = c(combine_data),
                method =  rep(rownames(combine_data), ncol(combine_data)))
names(df) = c('time', 'demand', 'area')
df$area = factor(df$area, levels = rownames(combine_data)) # same order as originally

lwd_vector = rep(0.5, nrow(df))
col_vector = rep("#878787", nrow(df)) # '#878787' rep('darkgray', nrow(df)) # "#4682B4" # https://scales.r-lib.org/reference/grey_pal.html
line_vector = rep('solid', nrow(df))
col_vector[((1-1)*48+1):(1*48)]= 'black' # note that gray(0,0) is transparant so the original coloured lines stay as they are
col_vector[which(col_vector=="#878787")]= gray(0,0)

demand_plot = ggplot(data = df, aes(x = time, y = demand, group = area)) +
  ggtitle("Demand for Bikes across H3 cells and New York City") +
  scale_x_continuous(name = '', breaks = seq(from  = 1, by = 2, length = 24),
                     labels = c(paste0(0:11, 'am'), ('12pm'), paste0(1:11, 'pm')))+
  scale_y_continuous(name = ' ') +
  theme_bw(base_size = 15) +
  theme(axis.text.x= element_text(size = 10),
        axis.text.y= element_text(size = 15),
        legend.position = "none") +
  geom_line(aes(color = area)) +
  geom_line(colour =  col_vector, size = lwd_vector, linetype = line_vector)


pdf(file = "Figure6.pdf", width = 10, height = 6)
demand_plot
dev.off()
