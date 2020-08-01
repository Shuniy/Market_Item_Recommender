
library(shiny)
library(tidyverse)
library(Matrix)

# Loading algorithm implementations and similarity calculations
source("cf_algorithm.R")
source("similarity_measures.R")
past_orders_matrix <- readRDS("past_orders_matrix.rds")
item_list <- readRDS("item_list.rds")

server <- function(input,output) {
    
    output$item_recommend <- renderTable({
        input$submit
        customer_order <- 
            isolate(unique(c(input$input_item1, input$input_item2, input$input_item3, 
                         input$input_item4, input$input_item5))
            )
        # put in a matrix format as done before
        new_order <- item_list %>%
            # Add a 'value' column with 1's for order items
            mutate(value = as.numeric(Description %in% customer_order)) %>%
            # Spread into sparse matrix format
            spread(key = Description, value = value) %>%
            # Change to a matrix
            as.matrix() %>% 
            # Convert to class "dgCMatrix"
            as("dgCMatrix")
        
        # Adding new order to retail matrix
        all_orders_dgc <- t(rbind(new_order, past_orders_matrix))
        
        # Set items to predict range
        items_to_predict <- which(all_orders_dgc[ ,1] == 0)
        users <- c(1)
        
        # Setting prediction indies
        prediction_indices <- as.matrix(expand.grid(items_to_predict, users = users))
        
        # Running IBCF model
        recommend <- predict_cf(all_orders_dgc, prediction_indices, 
                             "ibcf", FALSE, cal_cos, 3, FALSE, 4000, 2000)
        
        # Putting recommended products into a dataframe
        recommend[,users] %>% 
            as.data.frame() %>% 
            rownames_to_column('Recommendations') %>% 
            filter(.>0) %>% 
            select('Recommendations')
        
    })
}