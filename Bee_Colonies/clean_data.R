#####################################
#### cleaning bees colonies data ####
#####################################

read_csv_BeesColonies <- function(filename) {
  df <- read.csv(filename, skip=9, header=FALSE, stringsAsFactors=FALSE)
  df <- df[,c(-1,-2)]
  df <- na.omit(df)
  df[df == '(X)'] <- NA
  df[df == '(Z)'] <- 0
  df[df == '-'] <- 0
  colnames(df) <- c('state', 'num_colonies', 'max_colonies', 'lost_colonies', 'percent_lost_colonies',
                    'added_colonies', 'renovated_colonies', 'percent_renovated_colonies')
  df$state <- gsub(" [0-9]/", "", df$state)
  
  # adding the time period (i.e., months) as a column - different for each table
  time_period_lst <- list('Jan-Mar', 'Apr-Jun', 'Jul-Sep', 'Oct-Dec')
  for (i in 1:length(time_period_lst)) {
    if (((as.numeric(substr(filename,8,8))-i) %% 4) == 0) {
      df["time_period"] <- time_period_lst[[i]]
    }
  }
  
  # adding the year as a column
  if (((as.numeric(substr(filename,8,8))-1) %/% 4) == 0) {
    df["year"] <- 2015
  } else {
    df["year"] <- 2016
  }
  
  return(df)
}

read_csv_stressors <- function(filename) {
  df <- read.csv(filename, skip=8, header=FALSE, stringsAsFactors=FALSE)
  df <- df[,c(-1,-2)]
  df[df == ''] <- NA
  df <- na.omit(df)
  df[df == '(Z)'] <- 0
  colnames(df) <- c('state', 'varroa_mites', 'other_pests_parasites', 'diseases', 'pesticides',
                    'other', 'unknown')
  df$state <- gsub(" [0-9]/", "", df$state)
  
  # adding the time period (i.e., months) as a column - different for each table
  time_period_lst <- list('Jan-Mar', 'Apr-Jun', 'Jul-Sep', 'Oct-Dec')
  for (i in 1:length(time_period_lst)) {
    if (((as.numeric(substr(filename,7,8))-i-1) %% 4) == 0) {
      df["time_period"] <- time_period_lst[[i]]
    }
  }
  
  # adding the year as a column
  if (((as.numeric(substr(filename,7,8))-6) %/% 4) == 0) {
    df["year"] <- 2015
  } else {
    df["year"] <- 2016
  }
  
  return(df)
}

# set working directory to folder where all bee colonies csv files are located
setwd('BeeColonies-05-12-2016/')
files = list.files(pattern="*p0[1-5].*.csv")
colonies <- do.call(rbind, lapply(files, read_csv_BeesColonies))
write.csv(colonies, file="../clean_data/colonies.csv", row.names = FALSE)

files = c(list.files(pattern="*p0[6-9].*.csv"), list.files(pattern="*p10.*.csv"))
stressors <- do.call(rbind, lapply(files,read_csv_stressors))
write.csv(stressors, file="../clean_data/stressors.csv", row.names = FALSE)

#############################
#### cleaning honey data ####
#############################

# set working directory back to base directory for project
setwd('..')
honey <- read.csv('Hone-03-22-2016/hony_p03_t003.csv', skip=9, header=FALSE, stringsAsFactors=FALSE)
honey <- honey[,c(-1,-2)]
honey <- na.omit(honey)
colnames(honey) <- c('state', 'honey_producing_colonies_1000', 'yield_per_colony_lbs', 'production_1000lbs',
                     'stock_dec15_1000lbs', 'avg_price_per_lb_cents', 'value_of_production_1000USD')
honey[honey == 'Other States 5/ 6/'] <- 'Other States'
honey[honey == 'United States 6/ 7/'] <- 'United States'
honey["year"] <- 2015

write.csv(honey, file="clean_data/honey.csv", row.names = FALSE)

###################################
#### cleaning pollination data ####
###################################

read_csv_pollination <- function(filename) {
  df <- read.csv(filename, skip=12, header=FALSE, stringsAsFactors=FALSE)
  df <- df[,c(-1,-2)]
  df <- na.omit(df)
  colnames(df) <- c('crop', 'paid_pollinated_acres', 'price_per_acre_USD', 'colonies_used', 
                    'price_per_colony_USD', 'total_value_of_pollination_1000USD')
  df$crop <- trimws(df$crop)
  df$crop <- gsub(" [0-9]/", "", df$crop)
  df$crop_category[df$crop %in% c("Almond")] <- "Tree nuts"
  df$crop_category[df$crop %in% c("Apple", "Cherry", "Peach", "Pear", "Avocado", "Plum", "Other tree fruit")] <- "Tree fruit"
  df$crop_category[df$crop %in% c("Kiwi")] <- "Other fruit"
  df$crop_category[df$crop %in% c("Orange")] <- "Citrus"
  df$crop_category[df$crop %in% c("Watermelon", "Cantaloupe", "Honey dew", "Other melons")] <- "Melons"
  df$crop_category[df$crop %in% c("Blueberry", "Cranberry", "Raspberry", "Other berries")] <- "Berries"
  df$crop_category[df$crop %in% c("Cucumber", "Pumpkin", "Squash", "Other vegetables")] <- "Vegetables"
  df$crop_category[df$crop %in% c("All other", "Misc. crops", "Clover", "Alfalfa", "Sunflower")] <- "Other"
  
  # adding the region as a column
  region_lst <- c('1','2','3','4','5','6 & 7')
  df["region"] <- region_lst[((as.numeric(substr(filename,7,8))-2) %/% 2)]
  
  # adding the year as a column
  if (((as.numeric(substr(filename,7,8))-2) %% 2) == 0) {
    df["year"] <- 2015
  } else {
    df["year"] <- 2016
  }
  
  return(df)
}

# set working directory to folder where all pollination csv files are located
setwd('CostPoll-12-22-2016/')
files = list.files(pattern="cstp_p.*.csv")
pollination <- do.call(rbind, lapply(files, read_csv_pollination))
write.csv(pollination, file="../clean_data/pollination.csv", row.names = FALSE)

