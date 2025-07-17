# Capstone Part Two: Music Genre Classification

##########################################################
# Load Libraries
##########################################################
library(tidyverse)
library(data.table)
library(janitor)
library(readr)
library(caret)

##########################################################
# Load Metadata
##########################################################
# Load the CSVs (adjust paths as needed)
features <- fread("fma_metadata/features.csv", data.table = FALSE) %>%
  janitor::clean_names()

# Load tracks.csv with two header rows
tracks_raw <- read_csv("fma_metadata/tracks.csv", col_names = FALSE)
header1 <- tracks_raw[1, ] %>% unlist(use.names = FALSE)
header2 <- tracks_raw[2, ] %>% unlist(use.names = FALSE)
column_names <- make.names(paste0(header1, ".", header2), unique = TRUE)
tracks <- tracks_raw[-c(1, 2), ]
colnames(tracks) <- column_names

library(tidyverse)

# Step 1: Read the raw file with both headers
tracks_raw <- read_csv("fma_metadata/tracks.csv", col_names = FALSE)

# Step 2: Extract the first two rows (they form the column names)
header1 <- tracks_raw[1, ] %>% unlist(use.names = FALSE)
header2 <- tracks_raw[2, ] %>% unlist(use.names = FALSE)

# Step 3: Combine header parts into full column names
column_names <- paste0(header1, ".", header2)
column_names <- make.names(column_names, unique = TRUE)

# Step 4: Remove the first two rows and apply the new column names
tracks <- tracks_raw[-c(1, 2), ]
colnames(tracks) <- column_names

# Step 5: Add a track ID column for merging
tracks$track_id <- as.integer(rownames(tracks))

##########################################################
# Filter for Small Subset and Clean Labels
##########################################################

colnames(tracks)[1:100]

tracks_clean <- tracks %>%
  select(track_id,
         genre_top = `track.genre_top`,
         subset = `set.subset`) %>%
  filter(!is.na(genre_top), subset == "small") %>%
  mutate(genre_top = as.factor(genre_top))


##########################################################
# Prepare Features and Merge
##########################################################
# Clean up features file
colnames(features)[1] <- "track_id"
features <- features[-1, ]
features$track_id <- as.integer(features$track_id)

# Merge on track_id
combined_data <- inner_join(tracks_clean, features, by = "track_id")

##########################################################
# Normalize Features
##########################################################
# Step 1: Remove non-numeric columns
numeric_features <- combined_data %>%
  select(-track_id, -subset, -genre_top) %>%
  select(where(is.numeric))  # Keep only numeric columns

# Step 2: Scale the numeric features
feature_matrix <- scale(numeric_features)

# Step 3: Combine back with genre label for modeling
model_data <- cbind(genre_top = combined_data$genre_top, as.data.frame(feature_matrix))

##########################################################
# Split into Training and Test Sets
##########################################################
set.seed(123)
train_index <- createDataPartition(model_data$genre_top, p = 0.8, list = FALSE)
train_set <- model_data[train_index, ]
test_set  <- model_data[-train_index, ]



install.packages("nnet")
install.packages("caret")
library(nnet)
library(caret)

train_set$genre_top <- as.factor(train_set$genre_top)
test_set$genre_top <- as.factor(test_set$genre_top)


train_set <- model_data[train_index, ]
test_set  <- model_data[-train_index, ]
train_set$genre_top <- as.factor(train_set$genre_top)
test_set$genre_top <- as.factor(test_set$genre_top)

# Double-check model_data is a data frame
model_data <- as.data.frame(model_data)
model_data$genre_top <- as.factor(model_data$genre_top)

# Split again
set.seed(123)
train_index <- createDataPartition(model_data$genre_top, p = 0.8, list = FALSE)

train_set <- model_data[train_index, ]
test_set  <- model_data[-train_index, ]

class(train_set)        
str(train_set)          

# Ensure these libraries are loaded
library(dplyr)
library(caret)

# Step 1: Confirm combined_data exists

# Step 2: Select and normalize numeric features
numeric_features <- combined_data %>%
  select(-track_id, -subset, -genre_top) %>%
  select(where(is.numeric))

feature_matrix <- scale(numeric_features)

# Step 3: Rebuild model_data (genre_top + normalized features)
model_data <- cbind(genre_top = combined_data$genre_top, as.data.frame(feature_matrix))
model_data$genre_top <- as.factor(model_data$genre_top)

# Step 4: Create new train/test split
set.seed(123)
train_index <- createDataPartition(model_data$genre_top, p = 0.8, list = FALSE)

train_set <- model_data[train_index, ]
test_set  <- model_data[-train_index, ]

# Step 5: Confirm it's fixed
class(train_set)        # Should now say "data.frame"
str(train_set)          # Should show genre_top + numeric columns

# Confirm it's a data.frame and inspect columns
str(combined_data)

numeric_features <- combined_data %>%
  select(-track_id, -subset, -genre_top) %>%
  select(where(is.numeric))  # Ensure only numeric columns

feature_matrix <- scale(numeric_features)

model_data <- data.frame(
  genre_top = as.factor(combined_data$genre_top),
  feature_matrix
)
str(model_data)           
class(model_data)         

set.seed(123)
train_index <- createDataPartition(model_data$genre_top, p = 0.8, list = FALSE)
train_set <- model_data[train_index, ]
test_set <- model_data[-train_index, ]

class(train_set)      
str(train_set)        

# 1. Extract just numeric features
numeric_features <- combined_data %>%
  select(-track_id, -subset, -genre_top) %>%
  select(where(is.numeric))

# 2. Normalize
feature_matrix <- scale(numeric_features)

# 3. Create model_data as a true data.frame
model_data <- data.frame(
  genre_top = as.factor(combined_data$genre_top),
  feature_matrix
)

# 4. Confirm it's a data.frame
class(model_data)       # should be "data.frame"
str(model_data)         # should show genre_top + numeric columns

# 5. Split into training/testing
set.seed(123)
train_index <- createDataPartition(model_data$genre_top, p = 0.8, list = FALSE)
train_set <- model_data[train_index, ]
test_set  <- model_data[-train_index, ]

# 6. Double check structure
class(train_set)        # should be "data.frame"
str(train_set$genre_top)  # should be a factor
model_data <- cbind(genre_top = combined_data$genre_top, as.data.frame(feature_matrix))

set.seed(123)
train_index <- createDataPartition(model_data$genre_top, p = 0.8, list = FALSE)
train_set <- model_data[train_index, ]
test_set  <- model_data[-train_index, ]

class(train_set)   # should be "data.frame"
str(train_set)     # should list genre_top and numeric features

set.seed(123)
train_index <- createDataPartition(model_data$genre_top, p = 0.8, list = FALSE)

train_set <- model_data[train_index, , drop = FALSE]
test_set  <- model_data[-train_index, , drop = FALSE]

class(train_set)     # should say "data.frame"
str(train_set)       # should show genre_top + all feature columns


model_data <- data.frame(
  genre_top = combined_data$genre_top,
  feature_matrix  # scaled numeric features
)
set.seed(123)
train_index <- createDataPartition(model_data$genre_top, p = 0.8, list = FALSE)

train_set <- model_data[train_index, , drop = FALSE]
test_set  <- model_data[-train_index, , drop = FALSE]

dim(train_set)         
str(train_set)         

# Step 1: Remove non-numeric columns from combined_data
numeric_features <- combined_data %>%
  select(where(is.numeric)) %>%
  select(-track_id)  # Optional: keep track_id only if needed

# Step 2: Scale the numeric features
feature_matrix <- scale(numeric_features)

# Step 3: Combine scaled features + genre_top
model_data <- data.frame(
  genre_top = combined_data$genre_top,
  feature_matrix
)

# Confirm
str(model_data)  # should show genre_top + hundreds of features
set.seed(123)
train_index <- createDataPartition(model_data$genre_top, p = 0.8, list = FALSE)

train_set <- model_data[train_index, ]
test_set  <- model_data[-train_index, ]

# Confirm again
dim(train_set)  
str(train_set)  #

# Ensure model_data is still a full data frame
str(model_data)     
dim(model_data)    

# Redo the split correctly
set.seed(123)
train_index <- createDataPartition(model_data$genre_top, p = 0.8, list = FALSE)

# Properly subset as data frames
train_set <- model_data[train_index, , drop = FALSE]
test_set <- model_data[-train_index, , drop = FALSE]

# Confirm it worked
class(train_set)    
dim(train_set)      
str(train_set)      


# Normalize numeric features (excluding genre_top and track_id)
numeric_features <- combined_data %>%
  select(-track_id, -subset, -genre_top) %>%
  mutate(across(everything(), as.numeric))  # ensure all are numeric

# Scale the numeric features
feature_matrix <- scale(numeric_features)

# Confirm number of features
cat("Number of features:", ncol(feature_matrix), "\n")

# Final modeling dataset with genre_top + features
model_data <- data.frame(genre_top = combined_data$genre_top, feature_matrix)

# Confirm structure
str(model_data)       
dim(model_data)       

# Split
set.seed(123)
train_index <- createDataPartition(model_data$genre_top, p = 0.8, list = FALSE)
train_set <- model_data[train_index, ]
test_set  <- model_data[-train_index, ]

# Confirm dimensions
dim(train_set)        
dim(test_set)         

##########################################################
# Model Fitting : K-Nearest Neighbors (KNN) 
##########################################################
library(caret)

set.seed(123)
knn_model <- train(
  genre_top ~ .,
  data = train_set,
  method = "knn",
  tuneLength = 5,
  trControl = trainControl(method = "cv", number = 5)
)

# Predict
knn_preds <- predict(knn_model, newdata = test_set)

# Evaluate
confusionMatrix(knn_preds, test_set$genre_top)

##########################################################
# Model Fitting : Random Forest
##########################################################
library(randomForest)

set.seed(123)
rf_model <- randomForest(
  genre_top ~ .,
  data = train_set,
  ntree = 100,
  importance = TRUE
)

# Predict
rf_preds <- predict(rf_model, newdata = test_set)

# Evaluate
confusionMatrix(rf_preds, test_set$genre_top)

varImpPlot(rf_model)

##########################################################
#Test Accuracy : KNN
##########################################################
confusionMatrix(knn_preds, test_set$genre_top)

##########################################################
#Test Accuracy : Random Forest
##########################################################
confusionMatrix(rf_preds, test_set$genre_top)


##########################################################
#Create a Hold Out Set
##########################################################
set.seed(123)
holdout_index <- createDataPartition(model_data$genre_top, p = 0.8, list = FALSE)
training_set <- model_data[holdout_index, ]
holdout_set  <- model_data[-holdout_index, ]

# Confirm structure
dim(training_set)
dim(holdout_set)

# Ensure the label is a factor
training_set$genre_top <- as.factor(training_set$genre_top)
holdout_set$genre_top  <- as.factor(holdout_set$genre_top)

##########################################################
#Hyperparameter Tuning for KNN
##########################################################
set.seed(123)
tuned_knn <- train(
  genre_top ~ ., 
  data = training_set,
  method = "knn",
  tuneLength = 10,  # Try k = 1 to 10
  trControl = trainControl(method = "cv", number = 5)
)

# View best k
tuned_knn$bestTune

# Plot cross-validation results
plot(tuned_knn)

tuned_knn$results        # Accuracy for each k
tuned_knn$bestTune       # Best value of k
plot(tuned_knn)          # Accuracy vs. k

library(caret)

# Predict on final hold-out set
knn_final_preds <- predict(tuned_knn, newdata = holdout_set)

# Evaluate performance
confusionMatrix(knn_final_preds, holdout_set$genre_top)

##########################################################
#Hyperparameter Tuning for Random Forest
##########################################################
library(caret)
set.seed(123)

# Define tuning grid
rf_grid <- expand.grid(mtry = c(10, 20, 30, 40, 50))

# Train the model with cross-validation
rf_tuned <- train(
  genre_top ~ ., 
  data = train_set,
  method = "rf",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = rf_grid,
  importance = TRUE
)

# View Best Parameters and Model Summary
plot(rf_tuned)

# Predict on Hold-Out Set
rf_final_preds <- predict(rf_tuned, newdata = holdout_set)

# Evaluate the Model
library(caret)
cm_rf <- confusionMatrix(rf_final_preds, holdout_set$genre_top)
print(cm_rf)

library(tidyr)
library(dplyr)


##########################################################
#ROC Curve for Random Forest
##########################################################

# Get class probabilities 
rf_probabilities <- predict(rf_tuned, newdata = holdout_set, type = "prob")

library(pROC)
library(ggplot2)
library(tidyr)
library(dplyr)

# Prepare ROC Data for each class

# Create a list to store ROC curves
roc_list <- list()

# Get true labels
true_labels <- holdout_set$genre_top

# For each genre class
classes <- colnames(rf_probabilities)

for (class in classes) {
  # Convert true labels to binary: 1 if class, 0 otherwise
  binary_labels <- ifelse(true_labels == class, 1, 0)
  
  # Extract predicted probs for the current class
  prob <- rf_probabilities[[class]]
  
  # Compute ROC
  roc_obj <- roc(binary_labels, prob, levels = c(0, 1), direction = "<")
  roc_list[[class]] <- roc_obj
}

# Plot ROC Curves with AUC
# Combine all ROC curves into one ggplot
ggroc(roc_list) +
  labs(title = "ROC Curves for Each Genre (Random Forest)",
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme_minimal()

sapply(roc_list, auc)

##########################################################
# Genre-Wise Accuracy Plots
##########################################################
library(caret)
library(dplyr)
library(ggplot2)

# Compute confusion matrix
cm_rf <- confusionMatrix(rf_final_preds, holdout_set$genre_top)

# Extract per-class statistics
genre_accuracy <- as.data.frame(cm_rf$byClass)

# Add genre labels
genre_accuracy$Genre <- rownames(genre_accuracy)

# Keep only relevant columns
genre_accuracy_df <- genre_accuracy %>%
  select(Genre, Sensitivity) %>%   # Sensitivity = Recall = Accuracy per genre
  arrange(desc(Sensitivity))

# Plot Genre-Wise Accuracy
ggplot(genre_accuracy_df, aes(x = reorder(Genre, Sensitivity), y = Sensitivity)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Genre-wise Accuracy (Sensitivity) – Random Forest",
       x = "Genre",
       y = "Accuracy (Recall)") +
  theme_minimal(base_size = 14)

##########################################################
# Genre-Wise Accuracy for KNN and RF
##########################################################

# KNN confusion matrix
cm_knn <- confusionMatrix(knn_final_preds, holdout_set$genre_top)

# Extract and format accuracy (sensitivity)
knn_accuracy <- as.data.frame(cm_knn$byClass) %>%
  mutate(Genre = rownames(.),
         Model = "KNN") %>%
  select(Genre, Sensitivity, Model)

# RF accuracy already exists
rf_accuracy <- as.data.frame(cm_rf$byClass) %>%
  mutate(Genre = rownames(.),
         Model = "Random Forest") %>%
  select(Genre, Sensitivity, Model)

# Combine both
combined_accuracy <- bind_rows(knn_accuracy, rf_accuracy)

# Plot side-by-side Genre Accuracy
library(ggplot2)

ggplot(combined_accuracy, aes(x = reorder(Genre, Sensitivity), y = Sensitivity, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  coord_flip() +
  labs(title = "Genre-wise Accuracy Comparison: KNN vs Random Forest",
       x = "Genre",
       y = "Accuracy (Recall)") +
  theme_minimal(base_size = 14) +
  scale_fill_brewer(palette = "Dark2")

write.csv(combined_accuracy, "genre_accuracy_comparison.csv", row.names = FALSE)

##########################################################
#Exploratory Data Analysis
##########################################################
# Genre distribution
genre_counts <- tracks_clean %>%
  count(genre_top, sort = TRUE)

# Bar plot of genre distribution
ggplot(genre_counts, aes(x = reorder(genre_top, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Track Count by Genre",
       x = "Genre", y = "Number of Tracks") +
  theme_minimal()

# Double-check subset distribution
table(tracks_clean$subset)

# Distribution of Audio Feature Averages

# Identify MFCC, chroma, and spectral columns
mfcc_cols <- grep("^mfcc", names(combined_data), value = TRUE)
chroma_cols <- grep("^chroma", names(combined_data), value = TRUE)
spectral_cols <- grep("^spectral", names(combined_data), value = TRUE)

# Convert relevant columns to numeric safely
combined_data <- combined_data %>%
  mutate(across(all_of(c(mfcc_cols, chroma_cols, spectral_cols)), ~as.numeric(.)))

# Calculate per-track means
audio_means <- combined_data %>%
  mutate(
    mfcc_mean = rowMeans(select(., all_of(mfcc_cols)), na.rm = TRUE),
    chroma_mean = rowMeans(select(., all_of(chroma_cols)), na.rm = TRUE),
    spectral_mean = rowMeans(select(., all_of(spectral_cols)), na.rm = TRUE)
  ) %>%
  select(genre_top, mfcc_mean, chroma_mean, spectral_mean)

# Make sure mfcc_cols is defined
mfcc_cols <- grep("^mfcc", colnames(combined_data), value = TRUE)

# Convert MFCC columns to numeric if needed
combined_data <- combined_data %>%
  mutate(across(all_of(mfcc_cols), ~as.numeric(.)))

# Create mean column
combined_data <- combined_data %>%
  mutate(mfcc_mean = rowMeans(select(., all_of(mfcc_cols)), na.rm = TRUE))

#Distribution of Audio Features
# Boxplot of MFCC feature by genre
ggplot(combined_data, aes(x = genre_top, y = mfcc_mean)) +
  geom_boxplot(outlier.size = 0.5, fill = "lightblue") +
  coord_flip() +
  labs(title = "Distribution of MFCC Mean by Genre",
       x = "Genre", y = "Mean MFCC") +
  theme_minimal()

# Get column names that contain chroma
chroma_cols <- grep("^chroma", colnames(combined_data), value = TRUE)

# Convert to numeric and calculate the mean
combined_data <- combined_data %>%
  mutate(across(all_of(chroma_cols), ~as.numeric(.))) %>%
  mutate(chroma_mean = rowMeans(select(., all_of(chroma_cols)), na.rm = TRUE))

# Get column names that contain spectral
spectral_cols <- grep("^spectral", colnames(combined_data), value = TRUE)

# Convert to numeric and calculate the mean
combined_data <- combined_data %>%
  mutate(across(all_of(spectral_cols), ~as.numeric(.))) %>%
  mutate(spectral_mean = rowMeans(select(., all_of(spectral_cols)), na.rm = TRUE))

# Boxplot of Chroma feature by genre
ggplot(combined_data, aes(x = genre_top, y = chroma_mean)) +
  geom_boxplot(outlier.size = 0.5, fill = "lightgreen") +
  coord_flip() +
  labs(title = "Distribution of Chroma Mean by Genre",
       x = "Genre", y = "Mean Chroma") +
  theme_minimal()

# Boxplot of Spectral feature by genre
ggplot(combined_data, aes(x = genre_top, y = spectral_mean)) +
  geom_boxplot(outlier.size = 0.5, fill = "lightgreen") +
  coord_flip() +
  labs(title = "Distribution of Spectral Mean by Genre",
       x = "Genre", y = "spectral_mean") +
  theme_minimal()

#Correlation Heatmap
library(corrplot)

# Compute correlation matrix on a subset to avoid overload
corr_matrix <- cor(model_data[ , 2:30])  # First 30 features
corrplot(corr_matrix, method = "color", type = "upper", tl.cex = 0.6)


#PCA for Visualization
library(FactoMineR)
library(factoextra)

# Run PCA on scaled features
pca_res <- prcomp(model_data[ , -1], center = TRUE, scale. = TRUE)

# Plot first 2 PCs, colored by genre
fviz_pca_ind(pca_res, 
             geom.ind = "point",
             habillage = model_data$genre_top,
             addEllipses = TRUE,
             palette = "Dark2",
             title = "PCA: Genre Clusters")

# Create pca_df
pca_result <- prcomp(model_data[, -1], center = TRUE, scale. = TRUE)
# Extract the first two principal components
pca_scores <- as.data.frame(pca_result$x[, 1:2])

# Add genre labels back in from model_data
pca_df <- cbind(pca_scores, genre_top = model_data$genre_top)


# PCA of Audio Features by Genre
ggplot(pca_df, aes(x = PC1, y = PC2, color = genre_top)) +
  geom_point(alpha = 0.6) +
  stat_ellipse(type = "norm", level = 0.68, linetype = "dashed") +
  labs(title = "PCA of Audio Features by Genre",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Scree Plot
pca_variance <- pca_result$sdev^2
pca_prop_var <- pca_variance / sum(pca_variance)
qplot(y = pca_prop_var[1:10], x = 1:10, geom = "line") +
  labs(title = "Scree Plot", x = "Principal Component", y = "Proportion of Variance Explained")

# Genre Frequency in Train vs Test vs Holdout sets

library(ggplot2)
library(dplyr)

# Add labels for dataset splits
genre_distribution <- bind_rows(
  train_set %>% mutate(Split = "Train"),
  test_set %>% mutate(Split = "Test"),
  holdout_set %>% mutate(Split = "Holdout")
)

# Plot distribution
ggplot(genre_distribution, aes(x = genre_top, fill = Split)) +
  geom_bar(position = "dodge") +
  labs(title = "Genre Distribution Across Data Splits",
       x = "Genre", y = "Count") +
  coord_flip() +
  theme_minimal(base_size = 14)


# TOP N important features
# Calculate average importance across genres for each feature
rf_importance_overall <- rf_importance$importance %>%
  rowMeans() %>%
  sort(decreasing = TRUE)

# Convert to data frame
rf_top_features <- data.frame(
  Feature = names(rf_importance_overall),
  Importance = rf_importance_overall
)

# View top N
head(rf_top_features, 10)

library(caret)
# Get variable importance from rf_tuned
rf_importance <- varImp(rf_tuned, scale = TRUE)

# Convert to a tidy data frame
importance_df <- rf_importance$importance %>%
  rownames_to_column(var = "Feature") %>%
  arrange(desc(Overall))
# Choose top N
top_n <- 20

# Slice top N features
top_features <- importance_df %>%
  slice_max(order_by = Overall, n = top_n)

# Plot
library(ggplot2)

rf_top_features %>%
  slice_max(order_by = Importance, n = 20) %>%
  ggplot(aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Most Informative Features (Random Forest)",
       x = "Feature", y = "Average Importance") +
  theme_minimal(base_size = 14)
