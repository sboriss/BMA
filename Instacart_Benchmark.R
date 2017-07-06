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

