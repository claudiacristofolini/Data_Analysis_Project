# ====================================================================
# PROJECT: ANALYSIS OF SALES DATASET
# AUTHORS: Benedetta Di Palma, Claudia Cristofolini, Vittoria Calonghi
# DATE: January 2026
# =====================================================================

# 1. SET UP
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

# Library installation (run only once if not installed)
# install.packages("corrplot")
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
# PRINCIPAL COMPONENT ANALYSIS AND DECISION TREE MODELLING
# =================================================================

# 1. DATA PREPARATION FOR PCA
# Selecting only numeric columns for PCA
numeric_cols <- data[, c("unit_price", "quantity", "tax", "total_price", "reward_points")] 

#Scaling the data (normalization)
data_scaled <- scale(numeric_cols)

# 2. IMPLEMENTATION OF PCA
data.pca=prcomp(data_scaled)

#View PCA results (standard deviations, rotations/loadings)
data.pca

# 3. ANALYZING VARIANCE EXPLAINED
# Extract eigenvalues and explained variance from the PCA results
eig.values=get_eigenvalue(data.pca)
eig.values

# Scree plot shows the variance explained by each principal component.
fviz_eig(data.pca, addlabels = TRUE, ylim = c(0, 100))


# 4. PCA PROJECTION AND CLUSTER ANALYSIS
data.pca$x

# Select the first three principal components because they explain approximately 98% of the total variance
data.pca.3=data.pca$x[,c(1,2,3)] 
data.pca.3

# Assign clusters based on hierarchical clustering (3 groups)
data$pca.3=cutree(data.pca.3.hc,3) #add cluster membership to each observation (1–3)
data$pca.3
View(data)

# Summarize numeric and categorical variables for each cluster
summary(data[data$pca.3 == 1, ])
summary(data[data$pca.3 == 2, ])
summary(data[data$pca.3 == 3, ])

# Explore cluster distribution by categorical and numeric variables
table(data$city,data$pca.3)
table(data$gender,data$pca.3)

# 5. ADVANCED PCA USING FactoMineR for a more detailed analysis after prcomp
install.packages("FactoMineR")
library(FactoMineR)

data.pc=PCA(data[,c(8:12)],graph=FALSE)
fviz_pca_var(data.pc, col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE,ggtheme = theme_minimal()) 
summary(data.pc)

# Extract variable information from PCA: contributions of variables to the first five PCs
var=get_pca_var(data.pc)
var
var$contrib
round((var$contrib[,1:5]/100),3)


# 6. DECISION TREE CLASSIFICATION
# Install and load required packages
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

# Split dataset into training (13%) and test set
length(data)
nrow(data)
set.seed(2025)
data.idx=sample(1000,1000*.13)
data.train=data[data.idx,]
data.test=data[-data.idx,]

# Build a decision tree to classify data based on selected features
data.dc=rpart(city~.,data=data.train)
data.dc
rpart.plot(data.dc)

# Predict city for test set and valuate results with confusion matrix
data.dc.pred=predict(data.dc,data.test,type='class')
conf.matrix=table(data.test$city,data.dc.pred)
conf.matrix
   
# Compute overall accuracy
accuracy=sum(diag(conf.matrix)) /sum(conf.matrix)
accuracy

