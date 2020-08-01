library(shiny)
library(tidyverse)
library(Matrix)
library(shinythemes)

item_list <- readRDS("item_list.rds")

ui <- fluidPage(theme = shinytheme("sandstone"),
                themeSelector(),
    headerPanel("Product Recommender"),
    fluidRow(
        column(6, 
               h3("Select Items"),    
               wellPanel(
                   selectInput("input_item1", "First Item", choices = c("Choose from the list", item_list)),
                   selectInput("input_item2", "Second Item", choices = c("Choose from the list", item_list)),
                   selectInput("input_item3", "Third Item", choices = c("Choose from the list", item_list)),
                   selectInput("input_item4", "Fourth Item", choices = c("Choose from the list", item_list)),
                   selectInput("input_item5", "Fifth Item", choices = c("Choose from the list", item_list)),
                   actionButton("submit", "Done")
               )
        ),
        column(6,
               h3("Recommendations based on your Choices"),     
               tableOutput("item_recommend")
        )
    ),
)
