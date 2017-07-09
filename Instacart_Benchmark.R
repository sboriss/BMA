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

#setwd('c:/BBB/PROPACAD/CAPSTONE/BASKET/')
setwd('c:/Users/sboriss/BBB/PROPACAD/')
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

user_id_i = user_id_train$user_id[35]; user_id_i

user_seq_beg = 10001
user_seq_end = 30000

users_train = user_id_train$user_id[user_seq_beg:user_seq_end]

ud <- lapply( users_train, function(user_id_i){
  
  cat('\nuser_id_i:', user_id_i)

  #get orders in prior for user_id_i
  user_id_i_prior = orders %>% filter(eval_set == "prior" & user_id == user_id_i ) 
  user_id_i_prior
  
  #get products in prior for user_id_i
  products_prior_i <- user_id_i_prior %>% select( order_id,order_number, user_id )  %>% 
                                          left_join(orderp  , by="order_id") %>%
                                          left_join(products, by = 'product_id')
  
  #list all products ever ordered
  products_ordered_all = unique( products_prior_i$product_id )
  
  #list all products ever reordered
  products_reordered   = products_prior_i %>% filter( reordered == 1 ) %>% 
                                              select( product_id ) %>% 
                                              unique
  
  #list all products in the last order
  products_ordered_last = products_prior_i %>% filter( order_number == max(order_number) ) %>% 
                                               select( product_id ) 
  products_ordered_last
  
  #list all reordered products in the last order
  products_reordered_last = products_prior_i %>% filter( order_number == max(order_number) & reordered == 1 ) %>% 
                                                 select( product_id ) 
  products_reordered_last
  
  #reordering rate
  ## get max number of orders
  order_number_max = products_prior_i %>% select( order_number ) %>% max
  order_number_max
  
  products_reordered_count = products_prior_i %>% filter( reordered == 1 ) %>% 
                                                  group_by(product_id) %>%
                                                  summarise(
                                                    prod_reorders = n()
                                                  )
  products_reordered_count
  
  products_reordered_rate = ( products_reordered_count %>% select( prod_reorders ) ) / (order_number_max - 1 )
  colnames( products_reordered_rate ) = "prod_reordered_rate"
  products_reordered_rate
  
  products_reordered_info = cbind( products_reordered_count, products_reordered_rate)
  products_reordered_info
  
  #get products in train for user_id_i
  user_id_i_train = orders %>% filter(eval_set == "train" & user_id == user_id_i ) 
  user_id_i_train
  
  user_id_i_train = user_id_i_train %>% select( order_id,order_number, user_id ) %>% 
                      left_join(ordert  , by="order_id") %>%
                      left_join(products, by = 'product_id')
  
  user_id_i_train
  #reorder rate per order ( first, last, average: correlate with reorder_rate_test )
  reorder_rate_per_order_prior = products_prior_i %>% group_by(order_id) %>%
                                                      summarise(
                                                        reorder_rate = mean(reordered)
                                                      )
  products_prior_i %>% left_join( reorder_rate_per_order_prior, by="order_id") # %>% sort( order_number )
  
  ###define F1 score
  
  y_actl = user_id_i_train$product_id  
  
  y_pred_ordered_all    = products_ordered_all
  y_pred_ordered_last   = products_ordered_last$product_id
  y_pred_reordered      = products_reordered$product_id
  if( length( products_reordered_last$product_id ) == 0 ){
    y_pred_reordered_last = products_reordered$product_id
  }else{
    y_pred_reordered_last = products_reordered_last$product_id 
  }
  
  precision_ordered_all    = length( Reduce( intersect, list( y_actl, y_pred_ordered_all    ) ) )/ length( y_pred_ordered_all )
  precision_ordered_last   = length( Reduce( intersect, list( y_actl, y_pred_ordered_last   ) ) )/ length( y_pred_ordered_last )
  precision_reordered      = length( Reduce( intersect, list( y_actl, y_pred_reordered      ) ) )/ length( y_pred_reordered )
  precision_reordered_last = length( Reduce( intersect, list( y_actl, y_pred_reordered_last ) ) )/ length( y_pred_reordered_last )
  
  recall_ordered_all    = length( Reduce( intersect, list( y_actl, y_pred_ordered_all    ) ) )/ length( y_actl )
  recall_ordered_last   = length( Reduce( intersect, list( y_actl, y_pred_ordered_last   ) ) )/ length( y_actl )
  recall_reordered      = length( Reduce( intersect, list( y_actl, y_pred_reordered      ) ) )/ length( y_actl )
  recall_reordered_last = length( Reduce( intersect, list( y_actl, y_pred_reordered_last ) ) )/ length( y_actl )
  
  
  precision = c( ordered_all = precision_ordered_all,  ordered_last = precision_ordered_last, 
                   reordered = precision_reordered,  reordered_last = precision_reordered_last)
  
  recall    = c( ordered_all = recall_ordered_all, ordered_last   = recall_ordered_last, 
                 reordered   = recall_reordered  , reordered_last = recall_reordered_last)
  
  f1score   = 2 * ( precision * recall ) / ( precision + recall)
  
  rbind( precision = precision, recall = recall, f1score = f1score)

})

names(ud) = users_train
ud

paste('Ended at: ', Sys.time() )

#convert ud to be stored 
# ud_i = ud[[1]]; ud_i
# ud_i_in_row = c( ud_i )
# names( ud_i_in_row ) = paste( rownames(ud_i), rep( colnames(ud_i), each = 3 ), sep = "-" )
# ud_i_in_row

ud_names <- names( ud ) #[1:20]

ud_mat <- sapply( ud_names, function(i){
  
  ud_i = ud[[i]]; ud_i
  
  #substitute nan with 0
  ud_i[ is.nan( ud_i ) ] = 0
  
  #transform matrix into a row vector
  ud_i_in_row = c( ud_i )
  names( ud_i_in_row ) = paste( rownames(ud_i), rep( colnames(ud_i), each = 3 ), sep = "-" )
  ud_i_in_row

})
ud_mat <- t( ud_mat )
rownames(ud_mat) = ud_names
head( ud_mat )

#write.csv( ud_mat, file = file.path(path,"UD",paste0("ud_mat_",user_seq_beg,"_",user_seq_end,".csv") ) )

#descriptive statistics of columns
summary( ud_mat )

#
precision_indx <- grep( "precision", colnames(ud_mat) )
recall_index   <- grep( "recall"   , colnames(ud_mat) )
f1score_index  <- grep( "f1score"  , colnames(ud_mat) )

boxplot(ud_mat[, precision_indx ], main = "Precision ( 1-10000 )", names= gsub("precision-","", colnames(ud_mat)[precision_indx] ) )
boxplot(ud_mat[, recall_index   ], main = "Recall ( 1-10000 )"   , names= gsub("recall-"   ,"", colnames(ud_mat)[recall_index  ] ) )
boxplot(ud_mat[, f1score_index  ], main = "F1 score ( 1-10000 )" , names= gsub("f1score-"  ,"", colnames(ud_mat)[f1score_index ] ) )

#compare ordered_all with ordered_last
OA_f1s = ud_mat[, "f1score-ordered_all"]
OL_f1s = ud_mat[, "f1score-ordered_last" ]

plot( OL_f1s, OA_f1s)

OA_OL_larger_index = as.vector( which( OA_f1s > OL_f1s ) )

#substitute ordered_all in ordered_last
OAplusOL_f1s = c( OL_f1s[ -OA_OL_larger_index ], OA_f1s[ OA_OL_larger_index ])

summary( OAplusOL_f1s )

sum( OAplusOL_f1s > OL_f1s )

length( OAplusOL_f1s )

#compare ordered_last with reordered
OL_f1s = ud_mat[, "f1score-ordered_last" ]
RO_f1s = ud_mat[, "f1score-reordered" ] 

plot( OL_f1s, RO_f1s)

RO_OL_larger_index = as.vector( which( RO_f1s > OL_f1s ) )
length( RO_OL_larger_index )

#substitute reordered in ordered_last
ROplusOL_f1s = c( OL_f1s[ -RO_OL_larger_index ], RO_f1s[ RO_OL_larger_index ])

summary( cbind( OL_f1s, OAplusOL_f1s, ROplusOL_f1s ) )
boxplot( cbind( OL_f1s, OAplusOL_f1s, ROplusOL_f1s ), names= c("Ordered_Last(OL)","OA+OL","RO+OL"))
