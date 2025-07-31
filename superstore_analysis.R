
# Superstore Sales Analysis in R

# Step 3: EDA
# 3.1 Installing packages (uncomment if not installed)
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("summarytools")
# install.packages("tibble")
# install.packages("lubridate")
# install.packages("tidyr")

# Loading required packages
library(readxl)
library(dplyr)
library(ggplot2)
library(summarytools)
library(tibble)
library(lubridate)
library(tidyr)

# 3.2 Importing data
data <- read_excel("C:/Users/HP USER/Downloads/Sample - Superstore (2).xlsx",3)

# 3.3 Structure
str(data)
glimpse(data)

# 3.4 First few rows
head(data,10)

# 3.5 Column names and cleanup
colnames(data)
data <- data[ , -ncol(data)]
colnames(data)

# 3.6 Dataset size
dim(data)

# 3.7 Column summaries
summary(data)

# 3.8 Detailed summary
view(dfSummary(data))

# 3.9 Duplicate check
sum(duplicated(data))

# 3.10 Missing value check
colSums(is.na(data))

# 3.11 Unique values in key columns
unique(data$Category)
unique(data$Region)
unique(data$Segment)

# 3.12 Frequency tables
table(data$Category)
table(data$Region)
table(data$Segment)

# 3.13 Date conversion
data$Date <- as.Date(data$Date)
data$`Ship Date` <- as.Date(data$`Ship Date`)

# 3.14 New time columns
data <- data %>%
  mutate(
    Order_Year = year(Date),
    Order_Month = month(Date, label = TRUE)
  )

# 4.1 Region-wise Profit
region_summary <- data %>%
  group_by(Region) %>%
  summarise(
    Total_Sales = sum(Sales),
    Total_Profit = sum(Profit)
  )

ggplot(region_summary, aes(x = Region, y = Total_Profit, fill = Total_Profit > 0)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("red", "green")) +
  labs(title = "Profit by Region", x = "Region", y = "Total Profit") +
  theme_minimal()

# 4.2 Segment-wise Profit
segment_summary <- data %>%
  group_by(Segment) %>%
  summarise(
    Total_Sales = sum(Sales),
    Total_Profit = sum(Profit)
  )

ggplot(segment_summary, aes(x = Segment, y = Total_Profit, fill = Segment)) +
  geom_col() +
  labs(title = "Profit by Customer Segment", x = "Segment", y = "Total Profit") +
  theme_minimal()

# 4.3 Monthly Sales Trend
monthly_sales <- data %>%
  group_by(Order_Year, Order_Month) %>%
  summarise(Monthly_Sales = sum(Sales))

monthly_sales$Month_Year <- paste(monthly_sales$Order_Year, monthly_sales$Order_Month)
monthly_sales$Month_Year <- factor(monthly_sales$Month_Year, levels = unique(monthly_sales$Month_Year))

ggplot(monthly_sales, aes(x = Month_Year, y = Monthly_Sales, group = 1)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred") +
  labs(title = "Monthly Sales Trend", x = "Month-Year", y = "Sales") +
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(face = "bold"))

# 4.4 Discount vs Profit
ggplot(data, aes(x = Discount, y = Profit)) +
  geom_point(alpha = 0.4, color = "darkorange") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Discount vs Profit", x = "Discount", y = "Profit") +
  theme_minimal()

# 4.5 Quantity vs Sales
ggplot(data, aes(x = Quantity, y = Sales)) +
  geom_point(alpha = 0.4, color = "purple") +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(title = "Quantity vs Sales", x = "Quantity", y = "Sales") +
  theme_minimal()

# 5.1 Top 10 Products by Sales
top_products <- data %>%
  group_by(`Product Name`) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales)) %>%
  head(10)

ggplot(top_products, aes(x = reorder(`Product Name`, Total_Sales), y = Total_Sales)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Products by Sales", x = "Product Name", y = "Total Sales") +
  theme_minimal()

# 5.2 Sub-Category Profit
subcat_profit <- data %>%
  group_by(`Sub-Category`) %>%
  summarise(Total_Profit = sum(Profit, na.rm = TRUE)) %>%
  arrange(desc(Total_Profit))

ggplot(subcat_profit, aes(x = reorder(`Sub-Category`, Total_Profit), y = Total_Profit)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Profit by Sub-Category", x = "Sub-Category", y = "Total Profit") +
  theme_minimal()

# 5.3 Region-wise Sales and Profit
region_stats <- data %>%
  group_by(Region) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE), Total_Profit = sum(Profit, na.rm = TRUE))

region_long <- region_stats %>%
  pivot_longer(cols = c(Total_Sales, Total_Profit), names_to = "Metric", values_to = "Value")

ggplot(region_long, aes(x = Region, y = Value, fill = Metric)) +
  geom_col(position = "dodge") +
  labs(title = "Region-wise Sales vs Profit", x = "Region", y = "Amount") +
  theme_minimal()

# 6. Customer Segmentation
customer_data <- data %>%
  group_by(`Customer ID`) %>%
  summarise(
    Total_Sales = sum(Sales),
    Total_Profit = sum(Profit),
    Total_Quantity = sum(Quantity)
  )

customer_scaled <- scale(customer_data[, -1])
set.seed(123)
kmeans_result <- kmeans(customer_scaled, centers = 3, nstart = 25)
customer_data$Cluster <- as.factor(kmeans_result$cluster)

ggplot(customer_data, aes(x = Total_Sales, y = Total_Profit, color = Cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Customer Segments (K-Means Clustering)", x = "Total Sales", y = "Total Profit") +
  theme_minimal()
