# Import library
library(readr)
library(ggplot2)
library(outliers)
library(dplyr)

# set workspace 
setwd("~/Documents/assignment/Data Analytics and Machine Learning/Assignment 1")

# Load Datasets
WHR2023 <- read_csv("WHR2023.csv")

# Check Dimesions ( number of row & columns) in dataset
dim(WHR2023)

# Check the variables and their types in WHR2023
str(WHR2023)

# Check and change if this data has outliers values 
z_scores <- apply(WHR2023[,-1], 2, function(x) (x - mean(x)) / sd(x))
threshold <- 3
outliers <- which(abs(z_scores) > threshold, arr.ind = TRUE)
print(outliers)
WHR2023 <- WHR2023[-outliers[, -1], ]
dim(WHR2023)

# Check if this data has missing values
table(is.na(WHR2023))
colSums(is.na(WHR2023))

# Show details of the dataset
imputed_df <- WHR2023
numeric_cols <- sapply(imputed_df, is.numeric) 
for (col in names(imputed_df)) {
  if (is.numeric(imputed_df[[col]]) && any(is.na(imputed_df[[col]]))) {
    imputed_df[is.na(imputed_df[[col]]), col] <- mean(imputed_df[[col]], na.rm = TRUE)
  }
}
table(is.na(imputed_df))

# Point graph
ggplot(imputed_df, aes(x= Generosity, y = `Ladder score`)) + geom_point(size = 2.5, color="navy") 
+ xlab("Generosity") + ylab("Ladder score") + ggtitle("Generosity vs Ladder score")

# Bar graph
ggplot(imputed_df, aes(`Social support`, `Healthy life expectancy`)) + 
  geom_bar(stat = "identity", color = "purple") + 
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + ggtitle("Social support VS  Healthy life expectancy") + theme_bw()

# heatmap
correlation <-  cor(imputed_df[, c("Logged GDP per capita", "Social support", "Healthy life expectancy", "Freedom to make life choices", "Generosity", "Perceptions of corruption")])  
print(correlation)
heatmap(correlation, 
        col = colorRampPalette(c("#D73027", "#FFFFFF", "#4575B4"))(50), 
        main = "Happiness Score Correlation Heatmap"
)

