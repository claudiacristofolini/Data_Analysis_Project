# =================================================================
# PROJECT: ANALYSIS OF SALES DATASET
# AUTHORS: Benedetta Di Palma, Claudia Cristofolini, Vittoria Calonghi
# DATE: January 2026
# =================================================================

# 1. SET UP
install.packages("tidyverse")
install.packages("scales")
install.packages("corrplot")
install.packages("cluster")
install.packages("dbscan")

library(tidyverse)
library(scales)


# 2. DATA LOADING & PRE-PROCESSING
# We use raw url from our repository on GitHub to avoid acquisition errors
raw_url <- "https://raw.githubusercontent.com/Vittoria-C/Data_Analysis_Project/refs/heads/main/sales.csv"

# We convert nominal strings to factors to ensure correct domain definition
# and enables frequency-based analysis during the EDA phase.
data <- read.csv(raw_url, stringsAsFactors = TRUE)

View(data)

# 3. EXPLORATORY DATA ANALYSIS (EDA)

# STATISTICAL SUMMARY  (KPIs)
# We display the summary of data 
summary(data)
# We display the main metrics to establish a baseline for our analysis into KPI for better visualization
kpi_summary <- data %>%
  summarise(
    Total_Revenue = sum(total_price),
    Avg_Order_Value = mean(total_price),
    Total_Units_Sold = sum(quantity),
    Avg_Units_per_Sale = mean(quantity),
    Max_Single_Sale = max(total_price),
    Total_Reward_points = sum(reward_points))

print("GLOBAL KEY PERFORMANCE INDICATORS:")
print(kpi_summary)

# We verify data integrity, if there are missing values in the dataset
paste("Missing values:", sum(is.na(data)))

# GEOGRAPHIC PERFORMANCE
# We identify top-performing areas. Note: Each city maps to a single branch.
# Bar plot: Total Revenues by City and Branch
plot_branch <- data %>%
  group_by(city, branch) %>%
  summarise(Total_Revenue = sum(total_price), .groups = 'drop') %>%
  ggplot(aes(x = reorder(city, -Total_Revenue, sum), y = Total_Revenue, fill = branch)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Total_Revenue))), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("A" = "royalblue3", "B" = "red3")) +
  theme_minimal() +
  labs(title = "Total Revenues by City and Branch", x = "City", y = "Total Revenue (€)",fill = "Branch")
plot_branch


# CUSTOMER DEMOGRAPHIC & LOYALTY

# we perform a gender analysis to understand the purchasing power and volume per gender 
# to better tailor future marketing campaigns
# Bar plot: Gender-based Quantity Analysis
plot_gender <- data %>%
  group_by(gender) %>%
  summarise(Total_Qty = sum(quantity)) %>%
  ggplot(aes(x = gender, y = Total_Qty, fill = gender)) +
  geom_bar(stat = "identity", width = 0.6, show.legend = FALSE) +
  # internal labels
  geom_text(aes(label = round(Total_Qty)), vjust = -0.5, fontface = "bold") +
  theme_minimal() +
  scale_fill_manual(values = c("Female" = "plum", "Male" = "royalblue")) +
  labs(title = "Gender-based Quantity Analysis", x = "Gender", y = "Total Quantity")
plot_gender

# We evaluate the ratio of Members vs. Normal customers to see the loyalty program penetration 
# Pie chart: Distribution of Sales by Customer Type (Members or not)
plot_cust <- data %>%
  count(customer_type) %>%
  mutate(perc = n / sum(n)) %>%
  ggplot(aes(x = "", y = n, fill = customer_type)) +
  geom_bar(stat = "identity", width = 1, color = "white", linewidth = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = percent(perc, accuracy = 0.1)), 
            position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size= 5) +
  scale_fill_manual(values = c("Member" = "orange", "Normal" = "sienna4")) +
  theme_void() +
  labs(title = "Sales Distribution by Customer Type", fill = "Type")
plot_cust


# PRODUCT AND CATEGORIES ANALYSIS 

# We identify high-rotation products based on total quantity sold (inventory)
# Bar plot: Top-selling Products based on Quantity
plot_prod <- data %>%
  group_by(product_name) %>%
  summarise(Total_Qty = sum(quantity)) %>%
  ggplot(aes(x = reorder(product_name, Total_Qty), y = Total_Qty)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  # Internal white labels for horizontal bar charts
  geom_text(aes(label = round(Total_Qty)), hjust = 1.1, color = "white", fontface = "bold") +
  coord_flip() +  # swapping coordinates x, y for cleaner labeling
  theme_minimal() +
  labs(title = "Top-selling Products by Quantity", x = "Product", y = "Quantity")
plot_prod

# Analysis of Sales Volume  to identify which categories have the highest inventory turnover.
# (the most frequently purchased, that usually are "essential" or "high-frequency product" 
# Bar plot: Top-selling Categories by Quantity
plot_cat <- data %>%
  group_by(product_category) %>%
  summarise(Total_Qty = sum(quantity)) %>%
  ggplot(aes(x = reorder(product_category, Total_Qty), y = Total_Qty)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  # white labels for horizontal bar charts
  geom_text(aes(label = round(Total_Qty)), hjust = 1.1, color = "white", fontface = "bold") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top-selling Categories by Quantity", x = "Product Category", y = "Quantity")
plot_cat


# Analysis of Revenue Contribution to understand financial impact
# Pie chart: Product Categories Contribution to Revenues
plot_cat_rev <- data %>%
  group_by(product_category) %>%
  summarise(Revenue = sum(total_price)) %>%
  #reorder categories using a color gradient (darker = higher) to highlight top earners
  mutate(perc = Revenue / sum(Revenue), product_category = reorder(product_category, Revenue)) %>%
  ggplot(aes(x = "", y = Revenue, fill = product_category)) +
  geom_bar(stat = "identity", width = 1, color = "white", linewidth = 1) +
  coord_polar("y", start =0) +
  # Internal labels with percentage
  geom_text(aes(label = percent(perc, accuracy = 0.1)), position = position_stack(vjust = 0.5), 
            color = "black", fontface = "bold", size = 4) +
  theme_void() +
  # using a color scale of blues: the darker for the highest % of revenues
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Revenues Contribution by Category", fill = "Category (sorted by Revenue)")
plot_cat_rev



# CORRELATION ANALYSIS 

library(corrplot)

# For correlation analysis we use only numeric variables
# and remove qualitative columns and 'sale_id' (which is just an index).
numeric_data <- data %>%
  select(unit_price, quantity, tax, total_price, reward_points)
# We compute the Pearson correlation coefficient for each pair of variables.
cor_matrix <- cor(numeric_data)
# Rename labels
colnames(cor_matrix) <- c("Unit Price","Quantity", "Tax","Total Price", "Reward Points")
rownames(cor_matrix) <- colnames(cor_matrix)
# We generate the heatmap using a color palette (Red-White-Blue).
# This visualization helps identify strong positive (Blue) or negative (Red) relationships.
corrplot(cor_matrix, method = "color", type = "upper", order = "original",      
         addCoef.col = "black", number.cex = 0.8,  # fonts for coefficients
         tl.col = "black", tl.srt = 45,    # text labels
         col = COL2('RdBu', 10),  # Red-Blu palette
         title = "Pearson Correlation Matrix of Quantitative Sales Variables", mar = c(0,0,2,0))


# =================================================================
# R SCRIPT: CLUSTERING ANALYSIS
# =================================================================

library(cluster)
library(dbscan)

# 1. Setup and Data Loading
data<-read.csv("sales.csv", stringsAsFactors = TRUE)
View(data)
any(is.na(data))
data$customer_type <- as.factor(data$customer_type)
data$product_category <- as.factor(data$product_category)
data$gender <- as.factor(data$gender)
data$city <- as.factor(data$city)
data$branch <- as.factor(data$branch)
data$product_name <- as.factor(data$product_name)


# 2. Preparation
numeric_cols <- data[, c("unit_price", "quantity", "tax", "total_price", "reward_points")]
data_scaled <- scale(numeric_cols)
dist_matrix <- dist(data_scaled, method = "euclidean")


# 3. Hierarchical Clustering

#       A) Models
# -- Single Linkage --
hc_single <- hclust(dist_matrix, method = "single")
plot(hc_single, main = "Dendrogram - Single Linkage", xlab = "", sub = "", labels = FALSE)
# -- Complete Linkage --
hc_complete <- hclust(dist_matrix, method = "complete")
plot(hc_complete, main = "Dendrogram - Complete Linkage", xlab = "", sub = "", labels = FALSE)
# -- Average Linkage --
hc_average <- hclust(dist_matrix, method = "average")
plot(hc_average, main = "Dendrogram - Average Linkage", xlab = "", sub = "", labels = FALSE)
# -- Ward's Method --
hc_ward <- hclust(dist_matrix, method = "ward.D2")
plot(hc_ward, main = "Dendrogram - Ward's Method", xlab = "", sub = "", labels = FALSE)

#       B) Cut Tree and Dendrogram (focus on Average Linkage)
# Cut the Average Linkage tree into 3 clusters
clusters_avg <- cutree(hc_average, k = 3)
clusters_avg
# Visualize clusters on the Dendrogram
plot(hc_average, main = "Dendrogram - Average Linkage (k=3)", labels = FALSE, xlab = "", sub = "")
rect.hclust(hc_average, k = 3, border = "red")

#       C) Cluster Visualization
data$cluster=clusters_avg
View(data)
table(data[,c(3,13)])
table(data[,c(5,13)])
colors=c("red","blue","green")
as.integer(data$cluster)
point=c(3,4,5)
as.integer(data$city)
plot(data$quantity,data$total_price,col=colors[as.integer(data$cluster)],pch=point[as.integer(data$city)])
as.integer(data$gender)
plot(data$quantity,data$total_price,col=colors[as.integer(data$cluster)],pch=point[as.integer(data$gender)])


# 4. K-means Clustering
data_km= kmeans(data_scaled, 3, nstart = 50)
data_km
data_km$cluster
centroids=data_km$centers
centroids
data_km$iter
data_km$ifault
View(data_scaled)

plot(data_scaled[, 2], data_scaled[, 4], col = data_km$cluster, pch = 19,xlab = colnames(data_scaled)[2],  ylab = colnames(data_scaled)[4], main = "K-means clustering (k = 3)")
points(centroids[, 2], centroids[, 4],pch = 1,    cex = 1.5,  col = "black")
text(centroids[, 2], centroids[, 4], labels = paste("C", 1:nrow(centroids)),pos = 3, cex = 0.8)

plot(data_scaled[, 1], data_scaled[, 2], col = data_km$cluster, pch = 19,xlab = colnames(data_scaled)[1],  ylab = colnames(data_scaled)[2], main = "K-means clustering (k = 3)")
points(centroids[, 1], centroids[, 2],pch = 1,    cex = 1.5,  col = "black")
text(centroids[, 1], centroids[, 2], labels = paste("C", 1:nrow(centroids)),pos = 3, cex = 0.8)

hullplot(data_scaled[, c(2, 4)], data_km$cluster,main = "K-means clustering")
points(centroids[, 2], centroids[,4],pch = 1,cex = 1.5,col = "black")
text(centroids[, 2], centroids[, 4],labels = paste("C", 1:nrow(centroids)),pos = 3,cex = 0.8)

hullplot(data_scaled[, c(1, 2)], data_km$cluster,main = "K-means clustering")
points(centroids[, 1], centroids[, 2],pch = 1,cex = 1.5,col = "black")
text(centroids[, 1], centroids[, 2],labels = paste("C", 1:nrow(centroids)),pos = 3,cex = 0.8)


