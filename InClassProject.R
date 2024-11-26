library(readr) # Let's us import data file
library(caret) # Train-Test Split
library(class) # Classifications / K-Nearest neighbors / k means
library(corrplot) # Visualize Correlation Matrix
library(ggplot2) # Graphing

user_data <- read_csv("C:/Users/jalen/OneDrive - University of Mount Union/Data Science/R Notes/DataSets/OA 11.6 - yelp_academic_dataset_user.json.csv")
View(user_data)

business_data <- read_csv("C:/Users/jalen/OneDrive - University of Mount Union/Data Science/R Notes/DataSets/OA 11.7 - yelp_academic_dataset_business.json.csv")
View(business_data)


# Business Data Part

# 1. First 5 rows of data
first_5 <- head(business_data, 5)
View(first_5)

# 2. histogram/bar chart for business location
ggplot(business_data) + geom_bar(aes(x=state), fill="blue") + 
  theme(axis.text.x = element_text(angle = -90))

# 3. Pie Chart of star ratings
cont_table <- table(business_data$stars)
print(cont_table)

pie(cont_table, main="Pie Chart of Star Ratings", col=rainbow(9))

# 4. Review Count vs. Stars Box Plot
business_data$stars = as.factor(business_data$stars)
#bs <- as.factor(business_data$stars)


masked_review_count <- subset(business_data, review_count <= 50)
View(masked_review_count)

ggplot(masked_review_count, aes(x=stars, y=review_count, fill=stars)) +
  geom_boxplot(show.legend=TRUE)


# 5. Chi-square test
View(business_data)

business_data$stars = as.numeric(business_data$stars)
# Converts the factor categorical data back to numerical

masked_stars1 <- subset(business_data, stars == 1)
masked_stars5 <- subset(business_data, stars == 5)

View(masked_stars1)
View(masked_stars5)

cont_table_5 <- table(masked_stars5$review_count)
cont_table_1 <- table(masked_stars1$review_count)

chisq.test(cont_table_5)
chisq.test(cont_table_1)

# When comparing the review counts with 5 stars vs. 1 star. The p-value turns out to
# be the same for both of them. It is alos very low, meaning that the amount of stars
# is correlated to review counts.

# User Data Part

View(user_data)

# 1. column names
colnames(user_data)

# 2. Pearson r correlation
#corr_votes <- cor(user_data$cool_votes, user_data$funny_votes, user_data$useful_votes)

corr_votes <- cor(user_data[, c("cool_votes", "funny_votes", "useful_votes")])
print(corr_votes)

# 3. Linear Regression
votes_linear_model <- lm(user_data$cool_votes ~ user_data$funny_votes)
print(votes_linear_model)

v_coefs <- coef(votes_linear_model)
print(v_coefs)
lm_y_intercept <- v_coefs[1]
lm_slope <- v_coefs[2]

cat("slope:", lm_slope, "Y-intercept", lm_y_intercept)

ggplot(user_data) + geom_point(aes(x = funny_votes, y = cool_votes)) + 
  geom_smooth(aes(x = funny_votes, y = cool_votes), method="lm", se=F)


# 4. Linear Regression of Review and Fans
fan_lin_model <- lm(user_data$fans ~ user_data$review_count)
print(fan_lin_model)

f_coefs <- coef(fan_lin_model)
print(f_coefs)
lm_y_intercept <- f_coefs[1]
lm_slope <- f_coefs[2] 

cat("slope:", lm_slope, "Y-intercept", lm_y_intercept)

ggplot(user_data) + geom_point(aes(x = review_count, y = fans)) + 
  geom_smooth(aes(x = review_count, y = fans), method="lm", se=F)

corr_fans <- cor(user_data$review_count, user_data$fans)
print(corr_fans)

# Based on the correlation score and linear model, review scores are positively 
# correlated to fan amounts. However, the score was only 0.58, so they are not highly
# correlated

# 5. k-means
user_data$user_id <- NULL
user_data$friends <- NULL
user_data$elite <- NULL           # > NULL gets rid of catagorical data
user_data$yelping_since <- NULL
user_data <- na.omit(user_data)

user_cluster <- kmeans(user_data[2:7], 4)   # Clusters data
print(user_cluster)
print(table(user_cluster$cluster, user_data$fans))

View(user_data)

user_data$cluster <- user_cluster$cluster
View(user_data)

ggplot(user_data) + geom_point(aes(x=review_count, y=fans, color=user_data$cluster))

user_x <- user_data[2:7]

wcss <- function(k) {
  kmeans(user_x, centers=k)$tot.withinss
}

k_values <- 1:10

wcss_values <- sapply(k_values, wcss)

elbow_plot <- data.frame(k = k_values, wcss = wcss_values)
View(elbow_plot)


ggplot(elbow_plot, aes(x=k, y = wcss)) + geom_line() + geom_point()

user_cont_table <- table(user_data$review_count, user_data$fans)
print(user_cont_table)
chisq.test(user_cont_table)

# For review count and fans I chose 4 clusters as that is what the elbow plot showed
# to be the best. 

# 5. Part 2
user_cluster <- kmeans(user_data[2:7], 2)   # Clusters data
print(user_cluster)
print(table(user_cluster$cluster, user_data$useful_votes))

View(user_data)

user_data$cluster <- user_cluster2$cluster
View(user_data)

ggplot(user_data) + geom_point(aes(x=average_stars, y=useful_votes, color=user_data$cluster))

user_x <- user_data[2:7]

wcss <- function(k) {
  kmeans(user_x, centers=k)$tot.withinss
}

k_values <- 1:10

wcss_values <- sapply(k_values, wcss)

elbow_plot <- data.frame(k = k_values, wcss = wcss_values)
View(elbow_plot)


ggplot(elbow_plot, aes(x=k, y = wcss)) + geom_line() + geom_point()

user_cont_table <- table(user_data$average_stars, user_data$useful_votes)
print(user_cont_table)
chisq.test(user_cont_table)

# The variables I chose for this is average stars and useful votes. The elbow plot 
# showed that they should be clustered at 4. 
# The p-value from the contingency table also shows a very low number close to zero, 
# which means that they are very closely related.




