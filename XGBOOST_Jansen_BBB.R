###########################################################################################################
#
# Kaggle Instacart competition
# Fabien Vavrand, June 2017
# Simple xgboost starter, score 0.3791 on LB
# Products selection is based on product by product binary classification, with a global threshold (0.21)
# from https://www.kaggle.com/cjansen/instacart-xgboost-starter-lb-0-3808482/code
###########################################################################################################

library(data.table)
library(dplyr)
library(tidyr)


cat("\014")  # clear console
rm(list=ls(all=TRUE))


# Load Data ---------------------------------------------------------------
# path <- "Z:/NotesKOF/PAPERS/PROPACAD/BASKET/DATA/"
# 
# aisles      <- fread(file.path(path, "aisles.csv"))
# departments <- fread(file.path(path, "departments.csv"))
# orderp      <- fread(file.path(path, "order_products__prior.csv"))
# ordert      <- fread(file.path(path, "order_products__train.csv"))
# orders      <- fread(file.path(path, "orders.csv"))
# products    <- fread(file.path(path, "products.csv"))

setwd('c:/Users/sboriss/BBB/PROPACAD/')

path <- "./DATA/"

aisles      <- fread(file.path(path, "aisles.csv"))
departments <- fread(file.path(path, "departments.csv"))
orderp      <- fread(file.path(path, "order_products__prior.csv"))
ordert      <- fread(file.path(path, "order_products__train.csv"))
orders      <- fread(file.path(path, "orders.csv"))
products    <- fread(file.path(path, "products.csv"))


head( orderp )
head( orders )

#shorten orderp and ordert
#orderp <- orderp[1:50000,]
#ordert <- ordert[1:30000,]

# Reshape data ------------------------------------------------------------
aisles$aisle           <- as.factor(aisles$aisle)
departments$department <- as.factor(departments$department)
orders$eval_set        <- as.factor(orders$eval_set)
products$product_name  <- as.factor(products$product_name)

products <- products %>% 
  inner_join(aisles) %>% inner_join(departments) %>% 
  select(-aisle_id, -department_id)

head( products )

#remove (aisles, departments)-csv files from memory
rm(aisles, departments)

#insert column (user_id) into ordert
#match returns a vector of the positions of (first) matches of its first argument in its second.
ordert$user_id <- orders$user_id[ match(ordert$order_id, orders$order_id) ]

head( ordert )

#I think that inner_join leaves out order_id in train and test
orders_products <- orders %>% inner_join(orderp, by = "order_id")

head( orders_products, n = 20 )

rm(orderp)
gc()


# Products ----------------------------------------------------------------
# prd <- orders_products %>%
#   arrange(user_id, order_number, product_id) %>% #arrange rows by variables
#   group_by(user_id, product_id) %>%
#   mutate(product_time = row_number()) %>%
#   ungroup()
# 
# head( prd,  n = 250  )
# 
# prd_temp = head( prd,  n = 250  )
# 
# rm(prd)

prd <- orders_products %>%
  arrange(user_id, order_number, product_id) %>%
  group_by(user_id, product_id) %>%
  mutate(product_time = row_number()) %>% #tracks ordering of a given product_id over orders by a given user_id
  ungroup() %>%
  group_by(product_id) %>% #collect info about each product_id across all user_id
  summarise(
    prod_orders = n(),                          #total number of times product_id was   ordered by all user_id
    prod_reorders = sum(reordered),             #total number of times product_id was reordered by all user_id
    prod_first_orders  = sum(product_time == 1),#total number of times product_id was   ordered for the first  time
    prod_second_orders = sum(product_time == 2),#total number of times product_id was   ordered for the second time
    prod_third_orders  = sum(product_time == 3) #NB! not clear: why third, fourth, etc. orders are not taken into account? -added by BBB
  )

prd$prod_reorder_probability <- prd$prod_second_orders / prd$prod_first_orders
prd$prod_reorder_times       <- 1 + prd$prod_reorders  / prd$prod_first_orders
prd$prod_reorder_ratio       <-     prd$prod_reorders  / prd$prod_orders

prd <- prd %>% select(-prod_reorders, -prod_first_orders, -prod_second_orders, -prod_third_orders)

rm(products)
gc()

# Users -------------------------------------------------------------------
users <- orders %>%
  filter(eval_set == "prior") %>%
  group_by(user_id) %>%
  summarise(
    user_orders = max(order_number),
    user_period = sum(days_since_prior_order, na.rm = T),                 #how long a user_id was a customer of instacart
    user_mean_days_since_prior = mean(days_since_prior_order, na.rm = T)  #average interval between orders
  )

users_tmp = head( users, n = 250 )

us <- orders_products %>%
  group_by(user_id) %>%
  summarise(
    user_total_products = n(),                                        #how many products were purchased in total
    user_reorder_ratio = sum(reordered == 1) / sum(order_number > 1), #NB! why divide by sum(order_number > 1)?
    user_reordered_sum = sum(reordered == 1),                         ##-added by BBB
    user_sum_order_number = sum(order_number > 1),                    ##-added by BBB
    user_distinct_products = n_distinct(product_id)                   #how many distict products were purchased
  )

us_tmp              = head( us, n = 250)
orders_products_tmp = head( orders_products, n = 250 )

users <- users %>% inner_join(us)
users$user_average_basket <- users$user_total_products / users$user_orders

#add order_id from train/test
us <- orders %>%
  filter(eval_set != "prior") %>%
  select(user_id, order_id, eval_set,
         time_since_last_order = days_since_prior_order)

users <- users %>% inner_join(us)

rm(us)
gc()


# Database ----------------------------------------------------------------
data <- orders_products %>%
  group_by(user_id, product_id) %>% 
  summarise(
    up_orders = n(),
    up_first_order = min(order_number),
    up_last_order = max(order_number),
    up_average_cart_position = mean(add_to_cart_order))

data_tmp = head( data, n = 250 )

rm(orders_products, orders)

data <- data %>% 
  inner_join(prd, by = "product_id") %>%
  inner_join(users, by = "user_id")

data_tmp = head( data, n = 250 )

data$up_order_rate                   <- data$up_orders / data$user_orders
data$up_orders_since_last_order      <- data$user_orders - data$up_last_order
data$up_order_rate_since_first_order <- data$up_orders / (data$user_orders - data$up_first_order + 1)

data_tmp = head( data, n = 250 )

#add train 
data <- data %>% 
  left_join(ordert %>% select(user_id, product_id, reordered), 
            by = c("user_id", "product_id"))

data_tmp = head( data, n = 250 )

rm(ordert, prd, users)
gc()


# Train / Test datasets ---------------------------------------------------
train <- as.data.frame(data[data$eval_set == "train",])
train$eval_set <- NULL
train$user_id <- NULL
train$product_id <- NULL
train$order_id <- NULL
train$reordered[is.na(train$reordered)] <- 0

train_tmp = head( train, n = 250 )

test <- as.data.frame(data[data$eval_set == "test",])
test$eval_set <- NULL
test$user_id <- NULL
test$reordered <- NULL

test_tmp = head( test, n = 250 )

rm(data)
gc()


# Model -------------------------------------------------------------------
library(xgboost)
library(Ckmeans.1d.dp)

params <- list(
  "objective"           = "reg:logistic",
  "eval_metric"         = "logloss", # rmse
  "eta"                 = 0.3,
  "max_depth"           = 5,
  "min_child_weight"    = 1,  #10
  "gamma"               = 0.70,
  "subsample"           = 0.77,
  "colsample_bytree"    = 0.95,
  "alpha"               = 2e-05,
  "lambda"              = 6 #10
)

subtrain <- train %>% sample_frac(0.20)
#subtrain[1:100 ,"reordered"] = 1 # artificially set some row entries to one

#NB! normalise the data
head(subtrain)

# standardize features in subtrain set
subtrain_mat      = as.matrix(subtrain %>% select(-reordered) )
subtrain_mat_stnd = scale( subtrain_mat ); rm( subtrain_mat )
X                 = xgb.DMatrix(subtrain_mat_stnd, label = subtrain$reordered ); rm( subtrain_mat_stnd )
#model             = xgboost(data = X, params = params, nrounds = 100, nthread = 8)
#model             = xgboost(data = X, params = params, nrounds = 100, nthread = 8, booster = "gblinear")

cv <- xgb.cv(data = X, params = params, nrounds =100, nthread = 8, nfold = 5)


importance <- xgb.importance(colnames(X), model = model)
xgb.ggplot.importance(importance)

rm(X, importance, subtrain)
gc()


# Apply model -------------------------------------------------------------
#scale the test data set
rm( test_mat )
rm( train )
rm( oldDatascaled )

X <- xgb.DMatrix( scale( as.matrix(test %>% select(-order_id, -product_id)) ) )
test$reordered <- predict(model, X)

cutoff = 0.20

hist( test$reordered )
abline( v = cutoff , col = 2, lwd = 2)
mean( test$reordered > cutoff )

test$reordered <- (test$reordered > cutoff) * 1

submission <- test %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  summarise(
    products = paste(product_id, collapse = " ")
  )

submission_tmp = head( submission, n = 250 )

missing <- data.frame(
  order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
  products = "None"
)

submission <- submission %>% bind_rows(missing) %>% arrange(order_id)

submission_tmp = head( submission, n = 250 )

write.csv(submission, file = "submit02.csv", row.names = F)


data(agaricus.train, package='xgboost')
dtrain <- xgb.DMatrix(agaricus.train$data, label = agaricus.train$label)
cv <- xgb.cv(data = dtrain, nrounds = 3, nthread = 2, nfold = 5, metrics = list("rmse","auc"),
             max_depth = 3, eta = 1, objective = "binary:logistic")
print(cv)
print(cv, verbose=TRUE)
