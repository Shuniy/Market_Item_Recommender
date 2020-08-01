# Importing libraries
library(data.table)           
library(readxl)               
library(tidyverse)
library(lubridate)
library(skimr)                
library(knitr)                
library(treemap)

# importing data set and trim leading and trailing whitespaces
retail_data <- read_excel("Online Retail.xlsx", trim_ws = TRUE)

# Looking at the data
head(retail_data)
retail_data %>%  skim() # ALternative of summary
summary(retail_data)

# Data Pre-processing

# Total orders with cancellations

# InvoiceNo starts with letter 'C', it indicates a cancellation
retail_data %>% 
  filter(grepl("C", retail_data$InvoiceNo)) %>% 
  summarise(Total = n())

# Cancellations are not needed 
# Removing cancellation data
retail_data  <- retail_data %>% 
  filter(!grepl("C", retail_data$InvoiceNo)) 

# Removing negative values
# filtering by non positive Quantity, Description shows manually entered adjustments codes. 
# Quantity cant be less than 0 and grouping items by price and its description
retail_data %>% filter(Quantity <= 0) %>% group_by(Description, UnitPrice) %>% 
  summarise(count =n()) %>% arrange(desc(count)) %>% ungroup()

# Removing all rows with non-positive Quantity. 
retail_data  <- retail_data %>% filter(Quantity > 0)

# Problem with stock-codes 
# There are non-product related codes

# can be checked using excel and unique function
stc <- c('AMAZONFEE', 'BANK CHARGES', 'C2', 'DCGSSBOY', 'DCGSSGIRL', 'DOT', 'gift_0001_', 'PADS', 'POST')

# Summary of data by stock codes
retail_data %>%  
  filter(grepl(paste(stc, collapse="|"), StockCode))  %>% 
  group_by(StockCode, Description) %>% 
  summarise(count =n()) %>%
  arrange(desc(count)) %>% 
  ungroup()

# all should be removed. 
retail_data <- filter(retail_data, !grepl(paste(stc, collapse="|"), StockCode))

# CHECK: total row count - 529,228

#DESCRIPTION
# Additional adjustment codes to remove
descr <- c( "check", "check?", "?", "??", "damaged", "found", 
"adjustment", "Amazon", "AMAZON", "amazon adjust", 
"Amazon Adjustment", "amazon sales", "Found", "FOUND",
"found box", "Found by jackie ", "Found in w/hse", "dotcom",
"dotcom adjust", "allocate stock for dotcom orders ta", "FBA",
"Dotcomgiftshop Gift Voucher £100.00", "on cargo order",
"wrongly sold (22719) barcode", "wrongly marked 23343",
"dotcomstock", "rcvd be air temp fix for dotcom sit", "Manual",
"John Lewis", "had been put aside", "for online retail_data orders",  
"taig adjust", "amazon", "incorrectly credited C550456 see 47",
"returned", "wrongly coded 20713", "came coded as 20713", 
"add stock to allocate online orders", "Adjust bad debt",
"alan hodge cant mamage this section", "website fixed",
"did  a credit  and did not tick ret", "michel oops",
"incorrectly credited C550456 see 47", "mailout", "test",
"Sale error",  "Lighthouse Trading zero invc incorr", "SAMPLES",
"Marked as 23343", "wrongly coded 23343","Adjustment", 
"rcvd be air temp fix for dotcom sit", "Had been put aside."
)

# Filtering out the unwanted entries.
retail_data <- retail_data %>% 
  filter(!Description %in% descr)

sum(is.na(retail_data$Description))

# Small in quantity therefore we will remove them
retail_data <- retail_data %>% 
  filter(!is.na(Description))

# CUSTOMER ID
# There is still a significant number of NAs in _CustomerID_. 
retail_data$CustomerID %>% skim()


# there are almost 5 times as many Orders as there are Customers so I'm using `InvoiceNo` for orders 
sapply(retail_data[, c('InvoiceNo','CustomerID')], function(x) length(unique(x)))

retail_data <- retail_data %>%
  # Setting 'Description' and 'Country' as factors
  mutate(Description = as.factor(Description)) %>%
  mutate(Country = as.factor(Country)) %>% 
  # Changing 'InvoiceNo' type to numeric
  mutate(InvoiceNo = as.numeric(InvoiceNo)) %>% 
  # Extracting 'Date' and 'Time' from 'InvoiceDate'
  mutate(Date = as.Date(InvoiceDate)) %>% 
  mutate(Time = as.factor(format(InvoiceDate,"%H:%M:%S"))) 

glimpse(retail_data) #dplyr

# Saving clensed data for analysis phase
saveRDS(retail_data, "retail_data.rds")


# EXPLORATORY DATA ANALYSIS

# What items do people buy more often?
retail_data %>% 
  group_by(Description) %>% 
  summarize(count = n()) %>% 
  top_n(10, wt = count) %>%
  arrange(desc(count)) %>% 
  ggplot(aes(x = reorder(Description, count), y = count))+
  geom_bar(stat = "identity", fill = "royalblue", colour = "blue") +
  labs(x = "", y = "Top 10 Best Sellers", title = "Most Ordered Products") +
  coord_flip() +
  theme_grey(base_size = 12)

# Top 10 most sold products represent around 3% of total items sold by the company
retail_data %>% 
  group_by(Description) %>% 
  summarize(count = n()) %>% 
  mutate(pct = (count / sum(count)) * 100) %>% 
  arrange(desc(pct)) %>% 
  ungroup() %>% 
  top_n(10, wt = pct)

# What time of day do people buy more often?
retail_data %>% 
  ggplot(aes(hour(hms(Time)))) + 
  geom_histogram(stat = "count",fill = "#E69F00", colour = "red") +
  labs(x = "Hour of Day", y = "") +
  theme_grey(base_size = 12)

# What day of the week do people buy more often?
retail_data %>% 
  ggplot(aes(wday(Date, 
                  week_start = getOption("lubridate.week.start", 1)))) + 
  geom_histogram(stat = "count" , fill = "forest green", colour = "dark green") +
  labs(x = "Day of Week", y = "") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7),
                     labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  theme_grey(base_size = 14)

# How many items does each customer buy?
retail_data %>% 
  group_by(InvoiceNo) %>% 
  summarise(n = mean(Quantity)) %>%
  ggplot(aes(x=n)) +
  geom_histogram(bins = 100000, fill = "purple", colour = "black") + 
  coord_cartesian(xlim=c(0,100)) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  labs(x = "Average Number of Items per Purchase", y = "") +
  theme_grey(base_size = 14)
 

# What is the average value per order?
retail_data %>% 
  mutate(Value = UnitPrice * Quantity) %>% 
  group_by(InvoiceNo) %>% 
  summarise(n = mean(Value)) %>%
  ggplot(aes(x=n)) +
  geom_histogram(bins = 200000, fill="firebrick3", colour = "sandybrown") + 
  coord_cartesian(xlim=c(0,100)) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  labs(x = "Average Value per Purchase", y = "") + 
  theme_grey(base_size = 14)


# Which countries do they sell their goods to?
treemap(retail_data,
        index      = c("Country"),
        vSize      = "Quantity",
        title      = "",
        palette    = "Set2",
        border.col = "grey40")

