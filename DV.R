install.packages("readxl")
install.packages("sf")
install.packages("tidyverse")
install.packages("ggalt")


library(ggplot2)
library(sf)
library(readxl)
library(tidyverse)
library(ggalt)

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# Load the dataset
gender_pay_gap_data <- read_csv("gender_pay_gap.csv")

# Remove non-data rows and convert pay gap columns to numeric
cleaned_data <- gender_pay_gap_data %>%
  filter(!is.na(Column3), !is.na(Column4)) %>%
  mutate(Column3 = as.numeric(Column3), Column4 = as.numeric(Column4)) %>%
  drop_na(`Table 6.6a Hourly pay - Excluding overtime`)

# View the cleaned dataset
head(cleaned_data)

library(ggplot2)

# Trend Line Plot for Median Gender Pay Gap
ggplot(cleaned_data, aes(x = Description, y = `Median Pay Gap`, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Trend of Median Gender Pay Gap Across Categories",
       x = "Category", y = "Median Gender Pay Gap")


# Check if 'Description' column exists
if("Description" %in% names(cleaned_data)) {
  # If the column exists, proceed with the plot
  ggplot(cleaned_data, aes(x = Description, y = `Median Pay Gap`, group = 1)) +
    geom_line() +
    geom_point() +
    labs(title = "Trend of Median Gender Pay Gap Across Categories",
         x = "Category", y = "Median Gender Pay Gap")
} else {
  # If the column does not exist, print a message
  print("Column 'Description' not found in the dataframe")
}


ggplot(cleaned_data, aes(x = CategoryDescription, y = `Median Pay Gap`, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Trend of Median Gender Pay Gap Across Categories",
       

# List all column names in the cleaned_data dataframe
 names(cleaned_data)
       
# Display the first few rows of the 'Column2' to 'Column4'
head(cleaned_data$Column2)
head(cleaned_data$Column3)
head(cleaned_data$Column4)
head(cleaned_data$Column5)
head(cleaned_data$Column6)
head(cleaned_data$Column7)
head(cleaned_data$Column8)
head(cleaned_data$Column9)
head(cleaned_data$Column10)
head(cleaned_data$Column11)
# ... and so on for other columns

library(ggplot2)

# Create an index column (if not already present)
cleaned_data$Index <- 1:nrow(cleaned_data)

# Plot using the index as the x-axis and Column3 as the y-axis
ggplot(cleaned_data, aes(x = Index, y = Column3)) +
  geom_line() +
  geom_point() +
  labs(title = "Trend of Median gender pay gap across sectors",
       x = "Category", y = "Median gender pay gap")



library(tidyr)

# Transforming data for a clustered bar chart
cleaned_data_melted <- pivot_longer(cleaned_data, cols = c(`Median Pay Gap`, `Mean Pay Gap`), names_to = "Metric", values_to = "Value")

# Clustered Bar Chart for Median and Mean Gender Pay Gap
ggplot(cleaned_data_melted, aes(x = Description, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Comparison of Median and Mean Gender Pay Gap by Category",
       x = "Category", y = "Pay Gap", fill = "Metric")


# Scatter Plot for Median vs. Mean Gender Pay Gap
ggplot(cleaned_data, aes(x = `Median Pay Gap`, y = `Mean Pay Gap`)) +
  geom_point() +
  labs(title = "Relationship Between Median and Mean Gender Pay Gap",
       x = "Median Gender Pay Gap", y = "Mean Gender Pay Gap")


library(ggalt)  # Make sure to install this package

# Dumbbell Plot for Median vs. Mean Gender Pay Gap
ggplot(cleaned_data_melted, aes(y = Description, x = Value, group = Description, colour = Metric)) +
  geom_dumbbell() +
  labs(title = "Dumbbell Plot Comparing Median and Mean Gender Pay Gap",
       x = "Gender Pay Gap", y = "Category", colour = "Metric")


