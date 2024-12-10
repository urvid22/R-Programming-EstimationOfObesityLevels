install.packages("tidyverse")
install.packages("corrplot")
install.packages("ggpubr")
install.packages("nnet")
install.packages("stats")
install.packages("jtools")
install.packages("epitools")
library(ggpubr)
library(tidyverse)
library(corrplot)
library(nnet)
library(stats)
library(gridExtra)
library(ggplot2)
library(dplyr)
library(magrittr)

data <- read.csv("ObesityDataSet_raw_and_data_sinthetic.csv")
head(data)
Age <- round(data$Age, digits= 0)

num_vars <- data %>% select_if(is.numeric) %>% names()

# Create a grid of density plots
plots <- lapply(num_vars, function(var) {
  ggplot(data, aes(x = get(var))) +
    geom_density(fill = "lightblue", color = "black") +
    labs(x = var, title = var)
})

# Display the grid of plots
grid.arrange(grobs = plots, ncol = 3)

# Select the categorical variables
cat_vars <- data %>% select_if(function(x) !is.numeric(x)) %>% names()

# Create a grid of bar charts
plots <- lapply(cat_vars, function(var) {
  ggplot(data, aes(x = get(var))) +
    geom_bar(fill = "lightblue", color = "black") +
    labs(x = var, title = var)
})

# Display the grid of plots
grid.arrange(grobs = plots, ncol = 3)


pastel_colors <- c("#B5EAD7",  # Soft mint green
                   "#FFD7BA",  # Soft peach
                   "#FFC9DE",  # Soft pink
                   "#C7CEEA")  # Soft blue


boxplot(Age ~ NObeyesdad, data = data,
        main = "Distribution of Age by Obesity Category",
        xlab = "Obesity Category",
        col = pastel_colors,
        ylab = "Age (years)"
        
)  



num_data <- data %>% select_if(is.numeric)


cor_matrix <- cor(num_data)

par(mai = c(1, 1, 1, 1))
corrplot(cor_matrix, method = "circle", type = "lower", 
         tl.col = "black", tl.srt = 45, title = "Correlation Matrix",
         bg = "white", mar = c(0, 0, 2, 0))
#########################################################################
cat_vars <- data %>%
  select_if(function(x) !is.numeric(x)) %>%
  names()



chi_square_results <- data.frame(
  variable1 = character(),
  variable2 = character(),
  chi_square = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Loop through the categorical variables and perform Chi-Square test
for (var1 in cat_vars) {
  for (var2 in cat_vars) {
    if (var1 != var2) {
      # Create a contingency table
      contingency_table <- table(data[[var1]], data[[var2]])
      
      # Perform Chi-Square test
      chi_square_test <- chisq.test(contingency_table)
      
      # Store the results
      chi_square_results <- chi_square_results %>%
        bind_rows(data.frame(
          variable1 = var1,
          variable2 = var2,
          chi_square = chi_square_test$statistic,
          p_value = chi_square_test$p.value
        ))
    }
  }
}

# Sort the results by p-value
chi_square_results <- chi_square_results %>%
  arrange(p_value)


significant_associations <- chi_square_results %>%
  filter(p_value < 0.05)

# Create the graph
ggplot(significant_associations, aes(x = variable1, y = variable2)) +
  geom_tile(aes(fill = -log10(p_value))) +
  scale_fill_gradient(low = "white", high = "darkblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(
    title = "Significant Associations Between Categorical Variables",
    x = "Variable 1",
    y = "Variable 2",
    fill = "-log10(p-value)"
  )



##################################################################################


library(tidyverse)
library(nnet)

# Perform multinomial logistic regression
model <- multinom(NObeyesdad ~ ., data = data)
summary(model)

# Evaluate the model
# Calculate the accuracy of the model
predictions <- predict(model, data, type = "class")
accuracy <- mean(predictions == data$NObeyesdad)
print(paste("Accuracy:", accuracy))

###############################################################################

obese <- data$NObeyesdad %in% c('Overweight_Level_I', 'Overweight_Level_II', 'Obesity_Type_I', 'Obesity_Type_II', 'Obesity_Type_III')
t_test <- t.test(data$MTRANS[obese], data$MTRANS[!obese], var.equal = FALSE)

# Print the t-test results
print(t_test)

# Calculate the confidence interval for the difference in means
conf_int <- t_test$conf.int
print(conf_int)

