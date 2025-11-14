library(ROCR)

# Convert true labels into numeric (1 = setosa, 0 = non_setosa)
true_labels <- ifelse(testData$Species == "setosa", 1, 0)

# Create prediction object
pred_obj <- prediction(log_prob, true_labels)

# Performance object for ROC
perf <- performance(pred_obj, "tpr", "fpr")

# Plot ROC curve
plot(perf, col = "blue", lwd = 2)
abline(a = 0, b = 1, lty = 2)
