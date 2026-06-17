sink("output.txt")
pdf("plots.pdf", width=20, height=12)

library(ggplot2)
library(corrplot)
library(car)
library(cluster)
library(caTools)
library(e1071)  
library(rpart)  
library(class)
library(rpart.plot)
blood_data=read.csv("requests.csv")
blood_data


# fulfilled only (time is defined)
blood_fulfilled <- subset(blood_data, days_to_fulfill >= 0)

# quick counts to report
cat("Total rows:", nrow(blood_data), "\n")
cat("Fulfilled:", sum(blood_data$status_Fulfilled == 1), "\n")
cat("Open:", sum(blood_data$status_Open == 1), "\n")
cat("Cancelled:", sum(blood_data$status_Cancelled == 1), "\n")


print("Are there any missing values?")
print(any(is.na(blood_data)))

blood_data$urgency_encoded <- as.factor(blood_data$urgency_encoded)
print("Levels of the target variable:")
levels(blood_data$urgency_encoded)
print("Summary of urgency_encoded:")
cat("\nUrgency Summary (Counts & Percentages):\n")
urg_table <- table(blood_data$urgency_encoded)
urg_pct <- round(prop.table(urg_table) * 100, 2)
urg_summary <- data.frame(
  Urgency_Level = names(urg_table),
  Count = as.vector(urg_table),
  Percentage = as.vector(urg_pct)
)

print(urg_summary)



cat("\nBlood Type Summary (Counts & Percentages):\n")

blood_cols <- grep("^blood_type_", names(blood_data), value = TRUE)
blood_counts <- colSums(blood_data[, blood_cols])
blood_pct <- round((blood_counts / nrow(blood_data)) * 100, 2)

bt <- gsub("^blood_type_", "", names(blood_counts))

# Fix labels for display
bt <- gsub("\\.\\.", "+", bt)      
bt <- gsub("\\+1$", "+", bt)      
bt <- gsub("\\.$", "-", bt)        


blood_summary <- data.frame(
  Blood_Type = bt,
  Count = as.vector(blood_counts),
  Percentage = as.vector(blood_pct),
  stringsAsFactors = FALSE
)

blood_summary <- blood_summary[order(-blood_summary$Count), ]
print(blood_summary)




print("Percentage of Urgency Levels:")
round(prop.table(table(blood_data$urgency_encoded))*100,2)

stats_function <- function(x) {
  c(
    Count = length(x),
    Mean = mean(x),
    Median = median(x),
    Min = min(x),
    Max = max(x),
    SD = sd(x),
    Q1 = as.numeric(quantile(x, 0.25)),
    Q3 = as.numeric(quantile(x, 0.75))
  )
}



# Numeric variables (as-is)
numeric_cols_all <- blood_data[, c("quantity_mL", "request_month", "request_weekday")]

cat("\nSummary Statistics (All requests):\n")
print(round(t(apply(numeric_cols_all, 2, stats_function)), 3))

# days_to_fulfill: fulfilled only
cat("\nSummary Statistics (days_to_fulfill for Fulfilled only):\n")
print(round(stats_function(blood_fulfilled$days_to_fulfill), 3))


# Quantity distribution
hist(blood_data$quantity_mL,
     main="Histogram: Quantity (mL)",
     xlab="Quantity (mL)", col="lightblue")

boxplot(blood_data$quantity_mL,
        main="Boxplot: Quantity (mL)",
        ylab="Quantity (mL)", col="lightblue")

# days_to_fulfill distribution (fulfilled only)
hist(blood_fulfilled$days_to_fulfill,
     main="Histogram: Days to Fulfill (Fulfilled only)",
     xlab="Days", col="pink")

boxplot(blood_fulfilled$days_to_fulfill,
        main="Boxplot: Days to Fulfill (Fulfilled only)",
        ylab="Days", col="pink")


counts <- table(blood_data$urgency_encoded)
pct <- round(counts / sum(counts) * 100, 1)
lbls <- paste(names(counts), "\n", pct, "%", sep="")
pie(counts,
    labels = lbls, 
    main = "Pie Chart of Urgency Levels",
    col = rainbow(length(counts)))

status_vector <- ifelse(blood_data$status_Fulfilled==1,"Fulfilled",
                        ifelse(blood_data$status_Open==1,"Open","Cancelled"))
counts <- table(status_vector)

pct <- round(counts / sum(counts) * 100, 1)
labels_text <- paste(pct, "%", sep="") 

bp <- barplot(counts,
              main="Status Counts", 
              ylab="Count", 
              col=c("gray", "darkred", "orange"), 
              ylim = c(0, max(counts) * 1.2)) 

text(x = bp, y = counts, label = labels_text, pos = 3, cex = 1, col = "black")

q_sorted <- sort(blood_data$quantity_mL)
q_pct <- (1:length(q_sorted)) / length(q_sorted) * 100
plot(q_pct, q_sorted, type="l",
     main="Percentile Plot: Quantity (mL)",
     xlab="Percentile", ylab="Quantity (mL)", col="blue")

ggplot(blood_fulfilled, aes(x = quantity_mL, y = days_to_fulfill)) +
  geom_bin2d() +
  labs(title="2D Histogram: Quantity vs Days to Fulfill (Fulfilled Only)",
       x="Quantity (mL)", y="Days to Fulfill")


blood_fulfilled <- subset(blood_data, days_to_fulfill >= 0)

cor_data <- blood_fulfilled[, c("quantity_mL", "days_to_fulfill")]

cat("Correlation Matrix (Fulfilled only):\n")
mydata.cor <- cor(cor_data, use = "complete.obs", method = "pearson")
print(round(mydata.cor, 3))

cat("Generating Correlation Plot...\n")
corrplot(mydata.cor,
         method = "circle",
         type = "full",
         col = colorRampPalette(c("white","pink","red","darkred"))(200),
         tl.col = "red", tl.srt = 45,
         addCoef.col = "black",
         diag = TRUE,         
         main = "Correlation (Fulfilled Requests Only)")


scatterplotMatrix(
  blood_fulfilled[, c("quantity_mL", "days_to_fulfill", "request_month", "request_weekday")],
  main = "Scatterplot Matrix (Fulfilled Requests Only)"
)





set.seed(123)

cluster_input <- subset(blood_data, days_to_fulfill >= 0)[, c("quantity_mL", "days_to_fulfill")]
cluster_scaled <- scale(cluster_input)
model_kmeans <- kmeans(cluster_scaled, 3)
print("K-Means Clustering Results:")
print(model_kmeans)


clusplot(cluster_scaled, model_kmeans$cluster, color=TRUE, shade=TRUE, labels=0, lines=0,
         main="K-Means Clusters (k=3) - Scaled Features")


centroids_scaled <- model_kmeans$centers
centroids_original <- t( t(centroids_scaled) * attr(cluster_scaled, "scaled:scale") + attr(cluster_scaled, "scaled:center") )
print("Cluster centers (original units):")
print(round(centroids_original, 2))
print("Cluster sizes:")
print(table(model_kmeans$cluster))




# ===== Clean data for ML (Part 3) =====
blood_ml <- blood_data

# Convert -1 (unfulfilled) to NA because it's not a real time value
blood_ml$days_to_fulfill[blood_ml$days_to_fulfill < 0] <- NA

# Remove rows with NA in used features
blood_ml <- na.omit(blood_ml)

blood_ml <- subset(blood_ml, select = -c(status_Cancelled, status_Fulfilled, status_Open))



set.seed(123)
split <- sample.split(blood_ml$urgency_encoded, SplitRatio=0.7)
train_set <- subset(blood_ml, split == TRUE)
test_set  <- subset(blood_ml, split == FALSE)


calc_metrics<- function(cm){
  accuracy<- sum(diag(cm))/sum(cm)
  error<- 1-accuracy
  recall <- mean(diag(cm) / rowSums(cm), na.rm = TRUE)
  precision <- mean(diag(cm) / colSums(cm), na.rm = TRUE)
  f_measure<- 2* ((precision * recall) / (precision + recall))
  return(c(accuracy,error,precision,recall,f_measure))
  
}



# Algorithm 1: Naive Bayes:
nb_model<- naiveBayes(urgency_encoded ~ .,data=train_set)
nb_pred<- predict(nb_model,test_set)
cm_nb<- table(test_set$urgency_encoded,nb_pred)
m_nb<- calc_metrics(cm_nb)

# === Naive Bayes Confusion Matrix Plot (Heatmap) ===

cm_nb <- table(test_set$urgency_encoded, nb_pred)

cm_df <- as.data.frame(cm_nb)
colnames(cm_df) <- c("Actual", "Predicted", "Freq")


ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Naive Bayes - Confusion Matrix", x = "Predicted", y = "Actual") +
  theme_minimal()



# Algorithm 2: Decision Tree:
dt_model <- rpart(urgency_encoded ~ ., data = train_set, method="class")
dt_pred <- predict(dt_model, test_set, type="class")
cm_dt <- table(test_set$urgency_encoded, dt_pred)
m_dt <- calc_metrics(cm_dt)

rpart.plot(dt_model, type=2, extra=104, fallen.leaves=TRUE, tweak=1.2)


# Algorithm 3: KNN:

x_train <- subset(train_set, select = -urgency_encoded)
x_test  <- subset(test_set,  select = -urgency_encoded)

zero_var <- sapply(x_train, function(col) sd(col) == 0)
x_train <- x_train[, !zero_var, drop = FALSE]
x_test  <- x_test[,  !zero_var, drop = FALSE]

train_x <- scale(x_train)
test_x  <- scale(x_test,
                 center = attr(train_x, "scaled:center"),
                 scale  = attr(train_x, "scaled:scale"))

train_y <- train_set$urgency_encoded
test_y  <- test_set$urgency_encoded

knn_pred <- knn(train = train_x, test = test_x, cl = train_y, k = 5)
cm_knn <- table(test_y, knn_pred)
m_knn <- calc_metrics(cm_knn)



# Algorithm 4: Support Vector Machine:

svm_model <- svm(
  x = train_x,
  y = train_y,
  type = "C-classification",
  kernel = "linear",
  scale = FALSE,
  cost = 1
)

svm_pred <- predict(svm_model, test_x)
cm_svm <- table(test_y, svm_pred)
m_svm <- calc_metrics(cm_svm)





results_table <- data.frame(
  Algorithm = c("Naive Bayes", "Decision Tree", "KNN", "SVM"),
  Accuracy = round(c(m_nb[1], m_dt[1], m_knn[1], m_svm[1]), 3),
  Error = round(c(m_nb[2], m_dt[2], m_knn[2], m_svm[2]), 3),
  Precision = round(c(m_nb[3], m_dt[3], m_knn[3], m_svm[3]), 3),
  Recall = round(c(m_nb[4], m_dt[4], m_knn[4], m_svm[4]), 3),
  Measure = round(c(m_nb[5], m_dt[5], m_knn[5], m_svm[5]), 3)
)

print("Comparison of Classification Algorithms:")
print(results_table)




dev.off()


sink()
