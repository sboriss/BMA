#implement different benchmark models for MBA

cat("\014")  # clear console
rm(list=ls(all=TRUE))



library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)
library( mvoutlier )

setwd('c:/BBB/PROPACAD/CAPSTONE/BASKET/')

#load data 

# orders <- fread('./DATA/orders.csv')
# products <- fread('./DATA/products.csv')
# order_products_train <- fread('./DATA/order_products__train.csv')
# order_products_prior <- fread('./DATA/order_products__prior.csv')
# aisles <- fread('./DATA/aisles.csv')
# departments <- fread('./DATA/departments.csv')

path <- "./DATA/"

aisles      <- fread(file.path(path, "aisles.csv"))
departments <- fread(file.path(path, "departments.csv"))
orderp      <- fread(file.path(path, "order_products__prior.csv"))
ordert      <- fread(file.path(path, "order_products__train.csv"))
orders      <- fread(file.path(path, "orders.csv"))
products    <- fread(file.path(path, "products.csv"))


#get user_id in train
dim( products )

#get user_id in train
user_id_train = orders %>% filter(eval_set == "train") %>% 
                           select( user_id )

#get user_id in train
user_id_test = orders %>% filter(eval_set == "test") %>% 
                          select( user_id )

head( user_id_train )
head( user_id_test  )

head( orderp )

user_id_i = user_id_train$user_id[1]; user_id_i

#get orders in prior for user_id_i
user_id_i_prior = orders %>% filter(eval_set == "prior" & user_id == user_id_i ) 
user_id_i_prior

#get products in prior for user_id_i
products_prior_i <- user_id_i_prior %>% select( order_id,order_number, user_id )  %>% 
                                        left_join(orderp  , by="order_id") %>%
                                        left_join(products, by = 'product_id')

#list all products ever ordered
products_ordered_all = unique( products_prior_i$product_id )
products_reordered   = products_prior_i %>% filter( reordered == 1 ) %>% 
                                            select( product_id ) %>% 
                                            unique
products_ordered_last = products_prior_i %>% filter( order_number == max(order_number) ) %>% 
                                             select( product_id ) 
