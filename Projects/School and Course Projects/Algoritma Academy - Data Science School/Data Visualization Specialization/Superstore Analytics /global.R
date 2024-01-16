library(shiny) 
library(shinydashboard)
library(DT)
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(glue)
library(stringr)

# Read the data
ecom <- read.csv("superstore.csv", stringsAsFactors = T)


#----Correct wrong data types----
ecom <- mutate(
  ecom,
  Order.Date = mdy(Order.Date),
  Ship.Date = mdy(Ship.Date),
  Order.Year = year(Order.Date),
  Order.Month = month(Order.Date)
)

# I make a loop to automatically turn a factor into character if it has more than 20 levels
for (col in colnames(ecom)) {
  # Check if the column is a factor
  if (is.factor(ecom[[col]])) {
    # Check if the number of levels is more 20
    if (nlevels(ecom[[col]]) > 20) {
      # Convert the factor column to character
      ecom <- mutate(ecom, !!col := as.character(!!sym(col)))
    }
  }
}

# Order data rows based on Order.Date
ecom <- arrange(ecom, Order.Date)

