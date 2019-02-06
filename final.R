library(tidyverse)      # for data wrangling (includes dplyr, tidyr, readr)
library(lubridate)      # for easier working with dates and times
library(mlr)            # unified machine learning framework






## additionally, for the packages of algorithms you want to use, you will have to make sure that they're installed
## mlr will then take care of loading them for you.

options(tibble.width = Inf) # display all columns when printing a tibble
set.seed(42) # do not change this line

## IMPORTANT: When you submit the script, it MUST be SELF-CONTAINED i.e.
# it shouldn't rely on code or objects from any other files.
# The following line removes all loaded objects from your environment to ensure nothing else is loaded
rm(list = ls())


###### Helper Functions ===========================================
##kmeans on latitude and longitude in order to create regions of NE
kmeans_method <- function(){
  
  
  d <- read_csv('network_elements.csv')
  d1 <- d %>% select(latitude,longitude)
  
  #replace missing values with mean
  df_imputed <- d1 %>% mlr::impute(
    classes = list( numeric = imputeMean() )
  ) %>% .$data %>% as_tibble() # return
  
  k2 <- kmeans(df_imputed, centers = 40, nstart = 25)
  return(factor(k2$cluster))
  #return(d2)
}
# Get mode (most common value) of a vector:
# NOTE: R's `mode` function does NOT return the statistical mode, it's about internal storage type
# NOTE: If there's multiple modes, this function will return only the first one encountered.
custom_mode <- function(x){
  uniquex <- unique(x)
  uniquex[which.max(tabulate(match(x,uniquex)))]
}

### You do NOT need to change this function
# for each ne, determine whether there is at least one matching unavailability in ua
# between start_date (inclusive) and end_date (exclusive)
# returns: data frame `ne` but with added column 'label' containing 1 if matching ua, 0 otherwise
make_label <- function(ne, ua, interval_start, interval_end){
  # convert dates to times
  interval_start <- as_datetime(interval_start)
  interval_end <- as_datetime(interval_end)
  
  unavailability_by_ne <- ua %>%
    # filter only those uas in the interval
    filter(
      # case 1: ua starts in time frame
      between(start_date, interval_start, interval_end) |
        # case 2: ua ends in time frame
        between(end_date, interval_start, interval_end) |
        # case 3: ua lasts through entire time frame
        (start_date < interval_start & end_date > interval_end)
    ) %>% 
    # if multiple UAs for single NE, use only 1
    distinct(ne_id) %>% 
    mutate(label = 1)
  
  ne_with_uas <-
    # put label 1 whenever an unavailability exists for that NE, NA otherwise
    left_join(ne, unavailability_by_ne, by="ne_id")
  
  # fill NAs (i.e. no ua has been found) with 0
  ne_with_uas$label[is.na(ne_with_uas$label)] <- 0
  
  return(ne_with_uas)
}



##=================       FEATURE GENERATION                 =================##
##                                                                            ##
## You SHOULD change the following function and generate additional features. ##
##                                                                            ##
##----------------------------------------------------------------------------##

# generates features to be used in training using data from ua that's available until cutoff_date
make_features <- function(ne, ua, cutoff_date = '2017-08-01'){
  
  ua<-cbind(ua,val=1)
  
  ## make sure cutoff_date has correct type
  cutoff_date <- as_datetime(cutoff_date)
  # use only ua-information that is known at the cutoff date!
  ua <- ua %>% filter(start_date < cutoff_date) %>%
    # hide 'future information' that wouldn't yet be visible
    mutate(
      end_date = if_else(end_date > cutoff_date, cutoff_date - seconds(1), end_date)
    ) %>% 
    # calculate helper columns for later aggregation into features:
    mutate(
      duration_days = as.numeric(end_date - start_date, units='days')
      ### -----------------------------------------------  ###
      ###     Your code for additional helper columns here ###
      ### -----------------------------------------------  ###
    )
  
  ## Feature Engineering ----
  ##
  ## Now let's generate features, which we will do in the following way:
  ##
  ## Step A. Think of a feature that you want to use. As an example we've implemented the features
  ##    - 'average duration of past outages on the NE'
  ##      - for the entire time frame
  ##      - only for outages in the last three months
  ##    - 'what type of product failed most often at this NE'
  ## Step B. Calculate the feature using the `ua` data
  ## Step C. Add the feature to the `ne` data frame using a left join
  ## Step D. What if no outages have been found for a ne (left join will result in NAs) --> set sensible default values
  
  ##description of new features to be added  
  
  
  ## Implementation for the two example features:
  
  ## Step B ----
  ua_features <- ua %>%
    # we want to do the following calculations separately for every ne_id.
    # Also ensure that for each ne_id, the outages are ordered chronologically
    # (That's important for summary- and window functions such as `last`, `first`, `lag`, `lead`, etc.)
    group_by(ne_id) %>% arrange(start_date) %>%
    # now summarize the outages for that ne into a single value per NE that represents the feature
    summarize(
      # Feature: average duration of outages in hours
      mean_outage_length_hours =  mean(duration_days)*24,#*24
      # Feature: average duration of outages (previous 3 months only) in hours
      mean_outage_length_prev3m_h = mean(
        if_else(                               ## if_else function: fast vectorized implementation
          end_date >= cutoff_date - months(3), ## IF:   outage was in previous 6 months
          duration_days*24,    #*24                ## THEN: put duration in hours into vector
          NA_real_                             ## ELSE: put NA into the vector (if_else ensures type safety, thus must use NA_real_)
        ),
        
        na.rm = TRUE  #ignore NA values when taking mean
      ), 
      
      #Additional Feature: total duration of outages(previous 3 months only) in hours
      total_outage_length_prev3m_h = sum(
        if_else(                               ## if_else function: fast vectorized implementation
          end_date >= cutoff_date - months(3), ## IF:   outage was in previous 6 months
          duration_days*24,    #*24                ## THEN: put duration in hours into vector
          NA_real_                             ## ELSE: put NA into the vector (if_else ensures type safety, thus must use NA_real_)
        ),
        
        na.rm = TRUE  #ignore NA values when taking sum
      ), 
      
      #Additional Feature: total number of unavailabilities per network element (previous three months only)
      total_nr_ua  = sum(
      if_else(                               ## if_else function: fast vectorized implementation
        end_date >= cutoff_date - months(3), ## IF:   outage was in previous 6 months
        val,    #*24                ## THEN: put duration in hours into vector
        NA_real_                             ## ELSE: put NA into the vector (if_else ensures type safety, thus must use NA_real_)
      ),
      
      na.rm = TRUE  #ignore NA values when taking sum
    ), 
      
      
      #Additional Feature: total number of unavailabilities (previous 6 months only) with ua_priority= High
      total_nr_high_prev6m = sum(
        if_else(                               ## if_else function: fast vectorized implementation
          end_date >= cutoff_date - months(15), ## IF:   outage was in previous 6 months
          (ua_priority=="High"),                    ## THEN: put duration in hours into vector
          NA                             ## ELSE: put NA into the vector (if_else ensures type safety, thus must use NA_real_)
        ),
        
        na.rm = TRUE  #ignore NA values when taking mean
      ),
      #Additional Feature: total number of unavailabilities (previous 6 months only) with ua_priority= critical
      total_nr_critical_prev6m = sum(
        if_else(                               ## if_else function: fast vectorized implementation
          end_date >= cutoff_date - months(15), ## IF:   outage was in previous 6 months
          (ua_priority=="Critical"),                    ## THEN: put duration in hours into vector
          NA                             ## ELSE: put NA into the vector (if_else ensures type safety, thus must use NA_real_)
        ),
        
        na.rm = TRUE  #ignore NA values when taking mean
      ),
      
      #Additional Feature: total number of unavailabilities (previous 6 months only) with ua_priority=Low | Medium
      total_nr_lowmedium_prev6m = sum(
        if_else(                               ## if_else function: fast vectorized implementation
          end_date >= cutoff_date - months(15), ## IF:   outage was in previous 6 months
          (ua_priority=="Low") | (ua_priority=="Medium"),                    ## THEN: put duration in hours into vector
          NA                             ## ELSE: put NA into the vector (if_else ensures type safety, thus must use NA_real_)
        ),
        
        na.rm = TRUE  #ignore NA values when taking mean
      ), 
      
      #Additional Feature: 
      total_nr_prodcat2_switch = sum (
        if_else(
          end_date >= cutoff_date - months(15), ## IF:   outage was in previous 15 months
          (product_category_2=="Switch"),                   ## THEN: put duration in hours into vector
          NA                             ## ELSE: put NA into the vector (if_else ensures type safety, thus must use NA_real_)
        ),
        
        na.rm = TRUE  #ignore NA values when taking mean
        
      ),
      #Additional Feature: 
      total_nr_prodcat2_network = sum (
        if_else(
          end_date >= cutoff_date - months(15), ## IF:   outage was in previous 15 months
          (product_category_2=="Network"),                   ## THEN: put duration in hours into vector
          NA                             ## ELSE: put NA into the vector (if_else ensures type safety, thus must use NA_real_)
        ),
        
        na.rm = TRUE  #ignore NA values when taking mean
        
      ),
      
      #Additional Feature: 
      total_nr_prodcat2_connectivity = sum (
        if_else(
          end_date >= cutoff_date - months(15), ## IF:   outage was in previous 15 months
          (product_category_2=="Connectivity"),                   ## THEN: put duration in hours into vector
          NA                             ## ELSE: put NA into the vector (if_else ensures type safety, thus must use NA_real_)
        ),
        
        na.rm = TRUE  #ignore NA values when taking mean
        
      ),
      
      #Additional Feature: 
     # total_nr_prodcat3_bs = sum (
      #  if_else(
       #   end_date >= cutoff_date - months(15), ## IF:   outage was in previous 15 months
       #   (product_category_3=="BS"),                   ## THEN: put duration in hours into vector
        #  NA                             ## ELSE: put NA into the vector (if_else ensures type safety, thus must use NA_real_)
       # ),
        
       # na.rm = TRUE  #ignore NA values when taking mean
        
     # ),
      
      #Additional Feature: 
     # total_nr_prodcat3_Cconnect = count (
      #  if_else(
       #   end_date >= cutoff_date - months(15), ## IF:   outage was in previous 15 months
       #   (product_category_3=="Cross Connect"),                   ## THEN: put duration in hours into vector
       #   NA                             ## ELSE: put NA into the vector (if_else ensures type safety, thus must use NA_real_)
       # ),
        
       # na.rm = TRUE  #ignore NA values when taking mean
        
     # ),
      
      #Additional Feature: 
      #total_nr_prodcat3_LTE = sum (
      #  if_else(
       #   end_date >= cutoff_date - months(15), ## IF:   outage was in previous 15 months
       #   (product_category_3=="LTE enode"),                   ## THEN: put duration in hours into vector
       #   NA                             ## ELSE: put NA into the vector (if_else ensures type safety, thus must use NA_real_)
       # ),
        
      #  na.rm = TRUE  #ignore NA values when taking mean
        
      #),
      
      #Additional Feature: 
      #total_nr_prodcat3_Nodeb = sum(
      #  if_else(
       #   end_date >= cutoff_date - months(15), ## IF:   outage was in previous 15 months
       #   (product_category_3=="Node B"),                   ## THEN: put duration in hours into vector
       #   NA                             ## ELSE: put NA into the vector (if_else ensures type safety, thus must use NA_real_)
       # ),
        
      #  na.rm = TRUE  #ignore NA values when taking mean
        
      #),
      
      #Additional Feature: 
     # total_nr_prodcat3_SDH = sum(
      #  if_else(
       #   end_date >= cutoff_date - months(15), ## IF:   outage was in previous 15 months
        #  (product_category_3=="SDH"),                   ## THEN: put duration in hours into vector
       #   NA                             ## ELSE: put NA into the vector (if_else ensures type safety, thus must use NA_real_)
       # ),
        
       # na.rm = TRUE  #ignore NA values when taking mean
        
     # ),
      
      #Additional Feature: 
      #total_nr_prodcat3_EthMic = count (
      #  if_else(
       #   end_date >= cutoff_date - months(15), ## IF:   outage was in previous 15 months
        #  (product_category_3=="Ethernet Microwave"),                   ## THEN: put duration in hours into vector
        #  NA                             ## ELSE: put NA into the vector (if_else ensures type safety, thus must use NA_real_)
      #  ),
        
      #  na.rm = TRUE  #ignore NA values when taking mean
        
     # ),
      
      
      # Features: statistical mode of product_category_2 and product_category_3
      #mode_product_2 = custom_mode(product_category_2),
      #mode_product_3 = custom_mode(product_category_3),
      
      # Additional Features: 
      #mode_priority = custom_mode(ua_priority),
      #mode_impact= custom_mode(impact),
      #mode_urgency= custom_mode(urgency),
      
      
     # mode_resolution_category_1= custom_mode(resolution_category_1),
      #mode_resolution_category_2= custom_mode(resolution_category_2),
     # mode_resolution_category_3= custom_mode(resolution_category_3),
     # mode_reported_source= custom_mode(reported_source),
     # mode_slm_status= custom_mode(slm_status),
      #mode_assigned_group= custom_mode(assigned_group),
      
      total_nr_caused_inc = sum(
        if_else(                               ## if_else function: fast vectorized implementation
          end_date >= cutoff_date - months(12), ## IF:   outage was in previous 6 months
          (ne_caused_inc),                    ## THEN: put duration in hours into vector
          NA                             ## ELSE: put NA into the vector (if_else ensures type safety, thus must use NA_real_)
        ),
        
        na.rm = TRUE  #ignore NA values when taking mean
      ),
    
    total_urg_high_prev6m = sum(
      if_else(                               ## if_else function: fast vectorized implementation
        end_date >= cutoff_date - months(15), ## IF:   outage was in previous 6 months
        (urgency=="High"),                    ## THEN: put duration in hours into vector
        NA                             ## ELSE: put NA into the vector (if_else ensures type safety, thus must use NA_real_)
      ),
      
      na.rm = TRUE  #ignore NA values when taking mean
    ),
    #Additional Feature: total number of unavailabilities (previous 6 months only) with ua_priority= critical
    total_urg_critical_prev6m = sum(
      if_else(                               ## if_else function: fast vectorized implementation
        end_date >= cutoff_date - months(15), ## IF:   outage was in previous 6 months
        (urgency=="Critical"),                    ## THEN: put duration in hours into vector
        NA                             ## ELSE: put NA into the vector (if_else ensures type safety, thus must use NA_real_)
      ),
      
      na.rm = TRUE  #ignore NA values when taking mean
    ),
    
    #Additional Feature: total number of unavailabilities (previous 6 months only) with ua_priority=Low | Medium
    total_urg_lowmedium_prev6m = sum(
      if_else(                               ## if_else function: fast vectorized implementation
        end_date >= cutoff_date - months(15), ## IF:   outage was in previous 6 months
        (urgency=="Low") | (urgency=="Medium"),                    ## THEN: put duration in hours into vector
        NA                             ## ELSE: put NA into the vector (if_else ensures type safety, thus must use NA_real_)
      ),
      
      na.rm = TRUE  #ignore NA values when taking mean
    ), 
    
    total_imp_large_prev6m = sum(
      if_else(                               ## if_else function: fast vectorized implementation
        end_date >= cutoff_date - months(15), ## IF:   outage was in previous 6 months
        (impact=="Significant/Large"),                    ## THEN: put duration in hours into vector
        NA                             ## ELSE: put NA into the vector (if_else ensures type safety, thus must use NA_real_)
      ),
      
      na.rm = TRUE  #ignore NA values when taking mean
    )
    
    
    
     
     #total_nr_affected_inc = sum(
     #  if_else(                               ## if_else function: fast vectorized implementation
     #    end_date >= cutoff_date - months(12), ## IF:   outage was in previous 6 months
     #    (ne_caused_inc==FALSE),                    ## THEN: put duration in hours into vector
     #    NA                             ## ELSE: put NA into the vector (if_else ensures type safety, thus must use NA_real_)
     #  ),
       
     #  na.rm = TRUE  #ignore NA values when taking mean
    # )
      
      
      
      ### ---------------------------------------------- ###
      ###                                                ###
      ### Your Step B code for additional features here! ###
      ###                                                ###
      ### ---------------------------------------------- ###
    )
  
  ## Step C - add the features to the NE data frame ----
  ne_with_features <- left_join(ne, ua_features, by = "ne_id")
  
  ## Step D - replace NAs with sensible values ----
  
  # the `left_join` above will introduce an NA whenever not a single outage
  # was found for a NE
  
  ne_with_features <- ne_with_features %>% replace_na(
    list(
      mean_outage_length_hours = 0, # no outage --> length to 0 hours,
      mean_outagee_length_prev6m_h = 0,
      #mode_product_2 = 'no outages found',
      #mode_product_3 = 'no outages found',
      
      # Additional Features: 
      total_nr_ua =0,
      total_outage_length_prev6m_h=0,
      
      #mode_priority =  'no outages found',
      #mode_impact=  'no outages found',
      #mode_urgency=  'no outages found',
      
      #mode_resolution_category_1= 'no outages found',
      #mode_resolution_category_2='no outages found',
      #mode_resolution_category_3='no outages found',
      #mode_reported_source= 'no outages found',
      #mode_slm_status= 'no outages found',
      #mode_assigned_group= 'no outage found',
      total_nr_caused_inc= 0,
      total_nr_affected_inc=0,
      
      total_nr_critical_prev6m=0,
      total_nr_high_prev6m=0,
      total_nr_lowmedium_prev6m=0,
      
      total_nr_prodcat2_switch=0,
      total_nr_prodcat2_network=0,
      total_nr_prodcat2_connectivity=0,
      total_imp_large_prev6m=0,
      total_urg_critical_prev6m=0,
      total_urg_high_prev6m=0,
      total_urg_lowmedium_prev6m=0
      
      #total_nr_prodcat3_bs=0,
      #total_nr_prodcat3_Cconnect=0,
      #total_nr_prodcat3_LTE=0,
      #total_nr_prodcat3_Nodeb=0
      #total_nr_prodcat3_SDH=0
     # total_nr_prodcat3_EthMic=0
      ### ---------------------------------------------- ###
      ###                                                ###
      ### Your Step D code for additional features here! ###
      ###                                                ###
      ### ---------------------------------------------- ###
    )
  )
  
  return(ne_with_features)
}


# based on the input data ua, ne and two cutoff dates creates
# - a training set with generated features and labels
# - a holdout set with generated features and labels
# - the test set with generated features. The goal is to predict the correct labels on this set.
create_train_holdout_test <- function(ua, ne,
                                      cutoff_train = "2017-06-01",
                                      cutoff_holdout = "2017-07-01",
                                      cutoff_test = "2017-08-01"
){
  train <- make_features(ne, ua, cutoff_date = cutoff_train)
  holdout <- make_features(ne, ua, cutoff_holdout)
  test <- make_features(ne, ua, cutoff_test)
  
  train <- make_label(train, ua, interval_start = cutoff_train, interval_end = cutoff_holdout)
  holdout <- make_label(holdout, ua, interval_start = cutoff_holdout, interval_end = cutoff_test)
  
  # play a sound when calculation is done (optional, requires package beepr)
  #if (require(beepr)){beepr::beep()}
  
  return(list(train=train, holdout=holdout, test=test))
}



# Execution  ===================================================================


## 1. Load the raw data ========================================================

ua <- read_csv('unavailabilities.csv')

ne1 <- read_csv('network_elements.csv',
                col_types = cols(origin_net = 'c') #specify that origin_net is a character string.
)                                   #Otherwise entry 'T' might be interpreted as 'TRUE'

k<-kmeans_method()
ne<-cbind(ne1,km=c(k))
## Helpful package for Exploration:
# library(summaryTools)
# view(dfSummary(ua), method='browser')



## === 2. Create Train, Holdout, Test Sets =====================================
## for this, we'll be using the functions defined above 
# 
# First, let's set the cutoff parameters
# meaning of the params:
# - set x will use UAs until time cutoff_x for feature generation
# - labels for training set will be 1 iff outage between cutoff_train and cutoff_holdout
# - labels for holdout set will be 1 iff outage between cutoff_holdout and cutoff_test

cutoff_train =   "2017-06-01" 
cutoff_holdout = "2017-07-01"
cutoff_test =    "2017-08-01" # this is the end of your data. You should not change this date.

# this can take a bit of time (~5-10 mins on i7, depending on number and types of features)
sets <- create_train_holdout_test(ua, ne, cutoff_train, cutoff_holdout, cutoff_test)


## (optional) save (load) interim results of previous step to disk, to reuse them later
saveRDS(sets, file = 'generated_sets.Rds')
sets <- readRDS('generated_sets.Rds')

train <- sets$train
holdout <- sets$holdout
test <- sets$test

## 3. Clean and impute the data ==============

# Here you can specify a single imputation method that we will then apply to all three sets in the same manner
# As a basis, we will use the `impute` method provided by the `mlr` package.
# See the mlr cheat sheet or https://mlr.mlr-org.com/articles/tutorial/impute.html for detailed help.
# DO NOT assume that the imputation methods given to you by us example are very useful
my_impute_method <- function(df){
  df_imputed <- df %>% mlr::impute(
    classes = list(
      # for each data type, specify a "standard" imputation method to apply
      ## As an example, we'll set NAs in character columns to 'unknown' and numeric columns to their mean
      character = imputeConstant('unknown'),
      integer = imputeMean(),
      numeric = imputeMean()
    ),
    cols = list(
      # for columns that should NOT use the standard method based on its type, you can overwrite it
      # example: let's impute missing `frequency` values with 0 instead of the mean:
      frequency = imputeConstant(0)
    )
  ) %>% .$data %>% as_tibble() # return a data frame instead of mlr's imputation object for consistency
  
  # imputation using mlr::impute turns `int` into `doubles`
  # this can be problematic for the primary key id column when submitting the solution
  # we'll just set it back to its original integer value
  df_imputed$ne_id = df$ne_id
  
  return(df_imputed)
}

# Apply imputation
train_before <- my_impute_method(train)
holdout_before <- my_impute_method(holdout)
test <- my_impute_method(test)



## 4. Feature Selection ============================
# Specify which columns you want to use in our model.

# the following columns will be IGNORED in training/predicting.
## modify as desired
columns_drop <- c(
  "ne_id", # id column - DO NOT CHANGE this line - if you use it as a feature you'll be disqualified
  "ne_name",
  "site_id",
  "controller_id",
  "city",
  "zip_code",
  "LAC",
  "TAC",
  "antenna_type",
  #"MNC",
  "latitude",
  "longitude"
  #"mode_resolution_category_1",
  #"mode_resolution_category_2",
  #"mode_resolution_category_3",
  #"mode_assigned_group"
  
)


train_before_smote <- train_before %>% 
  select(-columns_drop) %>% 
  mutate_if(is.character, as_factor) %>% 
  mutate(label = factor(label))


holdout<- holdout_before %>% 
  select(-columns_drop) %>% 
  mutate_if(is.character, as_factor) %>% 
  mutate(label = factor(label))

##SMOTE algorithm for training set in order to overcome the imbalanced class label 
task_smote_1 <- makeClassifTask(
  id = 'SMOTE',
  data = train_before_smote, # all data necessary for resampling, here: train+holdout
  target = 'label' # which column do we want to predict
)


smote_rate<-8
sm<-smote(task_smote_1, rate=smote_rate, nn = 5L, standardize = TRUE, alt.logic = FALSE)#rate 2 means that we double the size of minority class
train<-getTaskData(sm)

##END OF SMOTE







## 4b. Preparation for mlr's API =======================


## to use mlr's validation features, we need to provide train and holdout 
## set in a single data frame.
df <- rbind(train, holdout)

train_rows <- 1:nrow(train)
holdout_rows <- 1:nrow(holdout) + nrow(train)

## To use mlr's API, we further need to
## - drop the columns we don't want to use
## - convert characters to factors
## - provide the label column as factors 
## We do this for both the training/holdout set and the test set



#df <- df %>% 
# select(columns_select) %>% #columns_select) %>% 
#mutate_if(is.character, as_factor) %>% 
#  mutate(label = factor(label))



test_df <- test %>% select(-columns_drop) %>% 
  mutate_if(is.character, as_factor)




## extract ne_ids and set - useful to have them for evaluation later 
#ne_indices <- c(train$ne_id, holdout$ne_id)
set_indices <- c(rep('train', nrow(train)), rep('holdout', nrow(holdout)))

#Ignore Naive Bayes for this run
## 5 Setup MLR Environment =======================

# `mlr` is a generic framework for machine learning in R that provides
# a unified interface to scores of other packages
# a basic pipeline in mlr consists of 4 parts
# - the data
# - a task
#     What are we trying to do? In our case we want to do 
#     classification on the `label column`
# - a learner 
#     What model are we applying in order to solve our task?
#     Here, mlr provides wrappers to many available packages, making it easy
#     to switch out and try different algorithms quickly.
#     As an example, we've applied a logistic regression model here, but you can 
#     try any other model you like.
# - a resampling strategy
#     What's the validation strategy?
#     Here we've implemented an out-of-time train-holdout split.

task <- makeClassifTask(
  id = 'predict future outages',
  data = df, # all data necessary for resampling, here: train+holdout
  target = 'label', # which column do we want to predict
  blocking = as_factor(set_indices), #you can ignore this, unless you want to implement you own resampling strategy
  positive=1
)

## set up a learner
## For list of available models see mlr::listLearners() or 
## https://mlr.mlr-org.com/articles/tutorial/integrated_learners.html

learner <- makeLearner(
  id = 'NaiveBayes',
  cl = 'classif.naiveBayes',
  predict.type = 'prob',
  predict.threshold = NULL, # use standard threshold
  fix.factors.prediction = TRUE, # deals with differences factor levels in train and test
  par.vals = list(
    # hyperparameters go here
    # Use the documentation of the learner base package to find out about possible
    # hyperparameters.
    # You can also  use mlr::getParamSet(learner) to see a list (but without explanations)
    laplace = 0
  )
)

## 6. Training and evaluating the model ========

validation_strategy <- makeFixedHoldoutInstance(
  train.inds = train_rows,
  test.inds = holdout_rows,
  size = nrow(df)
)


validation_result <- mlr::resample(
  learner = learner,
  task = task,
  resampling = validation_strategy,
  measures = list(acc, bac), #calculate accuracy and balanced accuracy
  keep.pred = TRUE
)









# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ RANDOM FOREST, LOGISTIC REGRESSION, SVM



## Note: some learners don't work with factor columns but require you to explicitly one-hot-encode first.
## If that's the case for your algorithm, you
## Note: some learners don't work with factor columns but require you to explicitly one-hot-encode first.
## If that's the case for your algorithm, you might get an error like
## >  Error in checkLearnerBeforeTrain(task, learner, weights) : 
## >   Task 'predict future outages' has factor inputs in [columns], but learner [learner] does not support that!
## If this happens to you, mlr provides a wrapper around the learner that can take care of OHE for you.
## To use it, uncomment the following line:
#learner <- makeDummyFeaturesWrapper(learner = learner,  method = "1-of-n")


#learner <- makeLearner("classif.randomForest", predict.type = "response", par.vals = list(ntree = 100, mtry = 5,nodesize=3))


#learner$par.vals <- list(
#  importance = TRUE
#)

# ntree=100
# nodesize", lower = 1
# mtry root(no_variable)
#rf_param <- makeParamSet(
#  makeIntegerParam("ntree",lower = 50, upper = 150),
#  makeIntegerParam("mtry", lower = 4, upper =20 ),
#  makeIntegerParam("nodesize", lower = 1, upper = 100)
#)

#rancontrol <- makeTuneControlRandom(maxit = 10L)
#set_cv <- makeResampleDesc("CV",iters = 2L)
#learner1<- tuneParams(learner = learner, resampling = set_cv, task = task, par.set = rf_param, control = rancontrol, measures = bac)


categorical <- c(
  "origin_net",
  "technology",
  "location_type",
  "urbanity",
  "MNC",
  "total_urg_critical_prev6m",
  "total_urg_high_prev6m" ,
  "total_urg_lowmedium_prev6m",
  "km"
  
  #"mode_product_2",
  #"mode_product_3",
  
  #"mode_priority" ,
  #"mode_impact",
  #"mode_urgency",
  
  #"mode_reported_source",
  #"mode_slm_status"

  
)


df_numerical<- df %>% select(-categorical) 

task_only_numeric <- makeClassifTask(
  id = 'predict future outages',
  data = df_numerical, # all data necessary for resampling, here: train+holdout
  target = 'label', # which column do we want to predict
  blocking = as_factor(set_indices), #you can ignore this, unless you want to implement you own resampling strategy
  positive=1
)


# Decision tree models for boosting
#learner <- makeLearner("classif.xgboost ", predict.type = "response", par.vals = list())
#learner <- makeLearner("classif.ada", predict.type = "response", par.vals = list())

# Logistic regression and Lasso 
#learner <- makeLearner("classif.logreg", predict.type = "response", par.vals = list())
learner <- makeLearner("classif.LiblineaRL1LogReg", predict.type = "response", par.vals = list(cost=0.00177, epsilon=0.00429))


# Hyper-parameter tuning for epsilon and cost
learner$par.vals <- list(
  importance = TRUE
)


lr_param <- makeParamSet(
  makeNumericParam("cost",lower = 0, upper = 1),
  makeNumericParam("epsilon", lower = 0, upper =1)
)

rancontrol <- makeTuneControlRandom(maxit = 100L)
set_cv <- makeResampleDesc("CV",iters = 2L)
learner1<- tuneParams(learner = learner, resampling = set_cv, task =task_only_numeric, par.set = lr_param, control = rancontrol, measures = bac)






#learner <- makeLearner("classif.svm", predict.type = "response", par.vals = list())

## 6. Training and evaluating the model ========


validation_strategy <- makeFixedHoldoutInstance(
  train.inds = train_rows,
  test.inds = holdout_rows,
  size = nrow(df)
)


validation_result <- mlr::resample(
  learner = learner,
  task = task_only_numeric,
  resampling = validation_strategy,
  measures = list(acc, bac), #calculate accuracy and balanced accuracy
  keep.pred = TRUE
)


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

## 6b - Evaluating and Tuning the model

### -------------------------------------------------------------------- ###
### Your code to evaluate and tune the model performance should go here  ###
### -------------------------------------------------------------------- ###

#Basic diagnostics to get you started:
validation_result$runtime

holdout_conf_matrix = mlr::calculateConfusionMatrix(
  pred = validation_result$pred,
  set = 'test', # the name 'test' comes from the mlr package --> actually refers to our holdout set.
  relative = T)

holdout_conf_matrix


## 7. Predict and create submission ====

# Once you're happy with the performance of the model
# - retrain on entire data (train + holdout)
# - make predictions on test set
# - create submission file

train_holdout_before_smote <- rbind(train_before_smote, holdout)



task_smote_2 <- makeClassifTask(
  id = 'SMOTE',
  data = train_holdout_before_smote, # all data necessary for resampling, here: train+holdout
  target = 'label' # which column do we want to predict
)


sm1<-smote(task=task_smote_2 , rate=smote_rate, nn = 5L, standardize = TRUE, alt.logic = FALSE)#rate 2 means that we double the size of minority class
df1<-getTaskData(sm1)
set_indices <- c(rep('train', 131788), rep('holdout', 83509))

#task <- makeClassifTask(
 # id = 'predict future outages',
 # data = df1, # all data necessary for resampling, here: train+holdout
 # target = 'label', # which column do we want to predict
 # blocking = as_factor(set_indices), #you can ignore this, unless you want to implement you own resampling strategy
 # positive=1
#)

#learner <- makeLearner(
 # id = 'NaiveBayes',
 # cl = 'classif.naiveBayes',
 # predict.type = 'prob',
 # predict.threshold = NULL, # use standard threshold
 # fix.factors.prediction = TRUE, # deals with differences factor levels in train and test
 # par.vals = list(
    # hyperparameters go here
    # Use the documentation of the learner base package to find out about possible
    # hyperparameters.
    # You can also  use mlr::getParamSet(learner) to see a list (but without explanations)
   # laplace = 0
  #)
#)
categorical <- c(
  "origin_net",
  "technology",
  "location_type",
  "urbanity",
  "MNC",
  
  "km"
  
  #"mode_product_2",
  #"mode_product_3",
  
  #"mode_priority" ,
  #"mode_impact",
  #"mode_urgency",
  
  #"mode_reported_source",
  #"mode_slm_status"
  
  
)


df1_numerical<- df1 %>% select(-categorical) 

task5 <- makeClassifTask(
  id = 'predict future outages',
  data = df1_numerical, # all data necessary for resampling, here: train+holdout
  target = 'label', # which column do we want to predict
  blocking = as_factor(set_indices), #you can ignore this, unless you want to implement you own resampling strategy
  positive=1
)



learner <- makeLearner("classif.LiblineaRL1LogReg", predict.type = "response", par.vals = list(cost=0.00177, epsilon=0.00429))
test_df_1<- test_df %>% select(-categorical) 

model <- train(learner, task5)
preds <- predict(model, newdata = test_df_1)

test_predicted <- bind_cols(test, preds$data)


## put the predictions into the format accepted by the Analytics-Cup Website

submission <- test_predicted %>% 
  transmute(id=ne_id, prediction=response) %>% 
  arrange(id) 

write_csv(submission,'predictions_Lasso_2.csv')
