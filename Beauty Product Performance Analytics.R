# Beauty Product Performance Analytics
# Data Science Portfolio Project

# Load tidyverse
library(tidyverse)

# Import dataset
beauty <- read.csv("beauty_products.csv")

# Preview dataset
head(beauty)

# Examine structure
str(beauty)

# Summary statistics
summary(beauty)

# Check for missing data
colSums(is.na(beauty))

# --------------------------------
# Product Category Analysis
# --------------------------------

# Calculate performance metrics by beauty category
category_summary <- beauty %>%
  group_by(Category) %>%
  summarise(
    Average_Rating = mean(Rating),
    Average_Price = mean(Price),
    Total_Reviews = sum(Reviews),
    Product_Count = n()
  ) %>%
  arrange(desc(Average_Rating))

print(category_summary)

# --------------------------------
# Top Performing Products
# --------------------------------

top_products <- beauty %>%
  arrange(desc(Rating), desc(Reviews)) %>%
  select(Product, Brand, Category, Rating, Reviews)

print(top_products)

+
# --------------------------------
# Visualization 1
# Average Rating by Category
# --------------------------------

ggplot(category_summary,
       aes(x = reorder(Category, Average_Rating),
           y = Average_Rating)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average Consumer Rating by Beauty Category",
    subtitle = "Beauty Product Performance Analysis",
    x = "Product Category",
    y = "Average Rating"
  ) +
  theme_minimal()

# --------------------------------
# Price vs. Consumer Rating
# --------------------------------

# Calculate correlation between price and rating
price_rating_correlation <- cor(
  beauty$Price,
  beauty$Rating,
  use = "complete.obs"
)

print(price_rating_correlation)


# Create price vs. rating visualization
ggplot(beauty, aes(x = Price, y = Rating)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Price vs. Consumer Rating",
    subtitle = "Relationship between product price and customer ratings",
    x = "Price ($)",
    y = "Consumer Rating"
  ) +
  theme_minimal() 
# --------------------------------
# Consumer Engagement Analysis
# --------------------------------

# Find products receiving the most consumer engagement
most_reviewed <- beauty %>%
  arrange(desc(Reviews)) %>%
  select(Product, Brand, Category, Reviews, Rating, Price)

print(most_reviewed)


# Visualize the 10 most reviewed products
top_10_reviewed <- beauty %>%
  arrange(desc(Reviews)) %>%
  slice_head(n = 10)

ggplot(top_10_reviewed,
       aes(x = reorder(Product, Reviews),
           y = Reviews)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Products by Consumer Engagement",
    subtitle = "Number of consumer reviews by product",
    x = "Product",
    y = "Number of Reviews"
  ) +
  theme_minimal()
# --------------------------------
# Save Visualizations
# --------------------------------

# Create folder for charts
if (!dir.exists("charts")) {
  dir.create("charts")
}
# Save Category Rating Chart

category_plot <- ggplot(
  category_summary,
  aes(x = reorder(Category, Average_Rating),
      y = Average_Rating)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average Consumer Rating by Beauty Category",
    subtitle = "Beauty Product Performance Analysis",
    x = "Product Category",
    y = "Average Rating"
  ) +
  theme_minimal()

ggsave(
  "charts/category_ratings.png",
  plot = category_plot,
  width = 8,
  height = 5
)
# Save Price vs Rating Chart

price_plot <- ggplot(
  beauty,
  aes(x = Price, y = Rating)
) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Price vs. Consumer Rating",
    subtitle = "Relationship between product price and customer ratings",
    x = "Price ($)",
    y = "Consumer Rating"
  ) +
  theme_minimal()

ggsave(
  "charts/price_vs_rating.png",
  plot = price_plot,
  width = 8,
  height = 5
) 
# Save Consumer Engagement Chart

engagement_plot <- ggplot(
  top_10_reviewed,
  aes(x = reorder(Product, Reviews),
      y = Reviews)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Products by Consumer Engagement",
    subtitle = "Number of consumer reviews by product",
    x = "Product",
    y = "Number of Reviews"
  ) +
  theme_minimal()

ggsave(
  "charts/consumer_engagement.png",
  plot = engagement_plot,
  width = 8,
  height = 5
)