# Importing libraries
library(data.table)
library(tidyverse)            
library(knitr)
library(recommenderlab)


# loading data
retail_data <- readRDS("retail_data.rds")

# Removing duplicates 
retail_data <- retail_data %>%  # create unique column with Invoice number and description
mutate(InNo_Desc = paste(InvoiceNo, Description, sep = ' ')) # filter out duplicates based on new column

retail_data <- retail_data[!duplicated(retail_data$InNo_Desc), ] %>% 
select(-InNo_Desc) # drop unique identifier

# Create the rating matrix 
ratings_matrix <- retail_data %>%                    # Select only needed variables
                  select(InvoiceNo, Description) %>% # Add a column of 1s
                  mutate(value = 1) %>%              # Spread into user-item format
                  spread(Description, value, fill = 0) %>%
                  select(-InvoiceNo) %>%            # Convert to matrix
                  as.matrix() %>%                   # Convert to recommenderlab class 'binaryRatingsMatrix'
                  as("binaryRatingMatrix")

# Create evaluation scheme
scheme <- ratings_matrix %>% 
  evaluationScheme(method = "cross",
                      k      = 5, 
                      train  = 0.8,
                      given  = -1)


# Algorithms to be applied
algorithms <- list(
      "association rules" = list(name  = "AR", param = list(supp = 0.01, conf = 0.01)),
      "random items"      = list(name  = "RANDOM",  param = NULL),
      "popular items"     = list(name  = "POPULAR", param = NULL),
      "item-based CF"     = list(name  = "IBCF", param = list(k = 5)),
      "user-based CF"     = list(name  = "UBCF", param = list(method = "Cosine", nn = 500))
)


# Estimating the Models
results <- recommenderlab::evaluate(scheme, 
                                    algorithms, 
                                    type  = "topNList", 
                                    n     = c(1, 3, 5, 10, 15, 20)
)

# Results for each single model can be easily retrieved and inspected using below equation 
results$'popular' %>% 
  getConfusionMatrix() 

results$'random' %>% 
  getConfusionMatrix() 

# Sorting results
tmp <- results$`user-based CF` %>% 
getConfusionMatrix()  %>%  
as.list() # Pull into a list all confusion matrix information for one model

# printing tmp results
tmp

as.data.frame(Reduce("+",tmp) / length(tmp)) %>% # average value of 5 cross-validation rounds
mutate(n = c(1, 3, 5, 10, 15, 20)) %>% # Add a column for number of recommendations calculated
select('n', 'precision', 'recall', 'TPR', 'FPR') # Select only columns needed and sorting out order 


# function to perform earlier functions
avg_conf_matr <- function(results) {
tmp <- results %>%
getConfusionMatrix()  %>%  
as.list() 
as.data.frame( Reduce("+",tmp) / length(tmp)) %>% 
mutate(n = c(1, 3, 5, 10, 15, 20)) %>%
select('n', 'precision', 'recall', 'TPR', 'FPR') 
}


# use  `map()` to get all results in a tidy format.

results_tbl <- results %>%
            map(avg_conf_matr) %>% # iterate function across all models
            enframe() %>% # Turning into an unnested tibble
            unnest() # Unnesting to have all variables on same level

# ROC curve
results_tbl %>%
  ggplot(aes(FPR, TPR, colour = fct_reorder2(as.factor(name), FPR, TPR))) +
  geom_line() +
  geom_label(aes(label = n))  +
  labs(title = "ROC curves",
  colour = "Model") +
  theme_grey(base_size = 14)

# Precision-Recall curve
results_tbl %>%
  ggplot(aes(recall, precision, 
  colour = fct_reorder2(as.factor(name),  precision, recall))) +
  geom_line() +
  geom_label(aes(label = n))  +
  labs(title = "Precision-Recall curves",
  colour = "Model") +
  theme_grey(base_size = 14)

## Predictions for a new user

# create a made-up order with a string containing 6 products selected at random.
customer_order <- c("GREEN REGENCY TEACUP AND SAUCER",
                    "SET OF 3 BUTTERFLY COOKIE CUTTERS",
                    "JAM MAKING SET WITH JARS",
                    "SET OF TEA COFFEE SUGAR TINS PANTRY",
                    "SET OF 4 PANTRY JELLY MOULDS")


# put string in a format that recommenderlab accepts.
new_order_rat_matrx <- retail_data %>% 
                    select(Description) %>% # Select item descriptions from retail_data dataset
                    unique() %>% 
                    mutate(value = as.numeric(Description %in% customer_order)) %>% # Add a 'value' column
                    spread(key = Description, value = value) %>% # Spread into sparse matrix format
                    as.matrix() %>% # Change to a matrix
                    as("binaryRatingMatrix") # Convert to recommenderlab class 'binaryRatingsMatrix'


# item based is good choice and performed better than others

# creating `Recommender`
recommend <- Recommender(getData(scheme, 'train'), method = "IBCF", param = list(k = 5))

# pass the `Recommender` and the made-up order to the `predict` function to create 
# a top 10 recommendation list for the new customer.
pred <- predict(recommend, newdata = new_order_rat_matrx, n = 10)

# inspect prediction as a list
as(pred, 'list')
