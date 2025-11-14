# ============================================================================
# EXPLORATORY DATA ANALYSIS (EDA) PROJECT
# Dataset: Titanic - Machine Learning from Disaster (Kaggle)
# ============================================================================

# --------------------------
# Load Required Libraries
# --------------------------
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(scales)

# ============================================================================
# 1. DATA LOADING
# ============================================================================
url <- "https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv"
titanic <- read.csv(url, stringsAsFactors = FALSE)

cat("Dataset loaded successfully!\n")
cat("Dimensions:", dim(titanic), "\n\n")

# ============================================================================
# 2. INITIAL DATA EXPLORATION
# ============================================================================
cat("=== DATASET OVERVIEW ===\n")
str(titanic)

cat("\n=== FIRST FEW ROWS ===\n")
head(titanic, 10)

cat("\n=== SUMMARY STATISTICS ===\n")
summary(titanic)

cat("\n=== MISSING VALUES ===\n")
missing_data <- data.frame(
  Variable = names(titanic),
  Missing_Count = sapply(titanic, function(x) sum(is.na(x))),
  Missing_Percentage = sapply(titanic, function(x) round(sum(is.na(x))/nrow(titanic)*100, 2))
)
print(missing_data)

# ============================================================================
# 3. DATA CLEANING AND PREPROCESSING
# ============================================================================
df <- titanic

df$Survived <- factor(df$Survived, levels = c(0, 1), labels = c("Died", "Survived"))
df$Pclass <- factor(df$Pclass, levels = c(1, 2, 3), labels = c("1st", "2nd", "3rd"))
df$Sex <- factor(df$Sex)
df$Embarked <- factor(df$Embarked)

df$Age[is.na(df$Age)] <- median(df$Age, na.rm = TRUE)

df$Embarked[df$Embarked == ""] <- names(sort(table(df$Embarked), decreasing = TRUE))[1]
df$Embarked <- droplevels(df$Embarked)

df$AgeGroup <- cut(df$Age, 
                   breaks = c(0, 12, 18, 35, 60, 100),
                   labels = c("Child", "Teen", "Young Adult", "Adult", "Senior"))

df$FamilySize <- df$SibSp + df$Parch + 1

df$Title <- gsub('(.*, )|(\\..*)', '', df$Name)
rare_titles <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
df$Title[df$Title == 'Mlle'] <- 'Miss' 
df$Title[df$Title == 'Ms'] <- 'Miss'
df$Title[df$Title == 'Mme'] <- 'Mrs' 
df$Title[df$Title %in% rare_titles] <- 'Rare Title'
df$Title <- factor(df$Title)

cat("\n=== DATA CLEANING COMPLETED ===\n")

# ============================================================================
# 4. UNIVARIATE ANALYSIS
# ============================================================================
p1 <- ggplot(df, aes(x = Survived, fill = Survived)) +
  geom_bar(stat = "count") +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Survival Distribution", x = "Survival Status", y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Died" = "#E74C3C", "Survived" = "#27AE60"))

p2 <- ggplot(df, aes(x = Age)) +
  geom_histogram(bins = 30, fill = "#3498DB", color = "black", alpha = 0.7) +
  labs(title = "Age Distribution", x = "Age", y = "Frequency") +
  theme_minimal()

p3 <- ggplot(df, aes(x = Pclass, fill = Pclass)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Passenger Class Distribution", x = "Class", y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

p4 <- ggplot(df, aes(x = Sex, fill = Sex)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Gender Distribution", x = "Gender", y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("female" = "#E91E63", "male" = "#2196F3"))

grid.arrange(p1, p2, p3, p4, ncol = 2)

# ============================================================================
# 5. BIVARIATE ANALYSIS
# ============================================================================
p5 <- ggplot(df, aes(x = Sex, fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(title = "Survival by Gender", x = "Gender", y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Died" = "#E74C3C", "Survived" = "#27AE60"))

p6 <- ggplot(df, aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(title = "Survival by Passenger Class", x = "Class", y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Died" = "#E74C3C", "Survived" = "#27AE60"))

p7 <- ggplot(df, aes(x = Age, fill = Survived)) +
  geom_density(alpha = 0.5) +
  labs(title = "Age Distribution by Survival", x = "Age", y = "Density") +
  theme_minimal() +
  scale_fill_manual(values = c("Died" = "#E74C3C", "Survived" = "#27AE60"))

p8 <- ggplot(df, aes(x = Pclass, y = Fare, fill = Pclass)) +
  geom_boxplot() +
  labs(title = "Fare Distribution by Class", x = "Class", y = "Fare") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

grid.arrange(p5, p6, p7, p8, ncol = 2)

# ============================================================================
# 6. MULTIVARIATE ANALYSIS
# ============================================================================
p9 <- ggplot(df, aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "fill") +
  facet_wrap(~Sex) +
  labs(title = "Survival Rate by Class and Gender", x = "Class", y = "Proportion") +
  theme_minimal() +
  scale_fill_manual(values = c("Died" = "#E74C3C", "Survived" = "#27AE60"))

p10 <- ggplot(df, aes(x = Age, y = Fare, color = Survived)) +
  geom_point(alpha = 0.6) +
  labs(title = "Age vs Fare by Survival Status", x = "Age", y = "Fare") +
  theme_minimal() +
  scale_color_manual(values = c("Died" = "#E74C3C", "Survived" = "#27AE60"))

p11 <- ggplot(df, aes(x = factor(FamilySize), fill = Survived)) +
  geom_bar(position = "fill") +
  labs(title = "Survival Rate by Family Size", x = "Family Size", y = "Proportion") +
  theme_minimal() +
  scale_fill_manual(values = c("Died" = "#E74C3C", "Survived" = "#27AE60"))

p12 <- ggplot(df, aes(x = Embarked, fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(title = "Survival by Embarkation Port", x = "Port", y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Died" = "#E74C3C", "Survived" = "#27AE60"))

grid.arrange(p9, p10, p11, p12, ncol = 2)

# ============================================================================
# 7. CORRELATION ANALYSIS
# ============================================================================
numeric_df <- df %>%
  select(Age, Fare, SibSp, Parch, FamilySize) %>%
  mutate(Survived = as.numeric(df$Survived) - 1,
         Pclass = as.numeric(df$Pclass),
         Sex = as.numeric(df$Sex))

cor_matrix <- cor(numeric_df, use = "complete.obs")
print(round(cor_matrix, 2))
corrplot(cor_matrix, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix", mar = c(0,0,1,0))

# ============================================================================
# 8. EXPORT TO HTML REPORT (Auto Open in Browser)
# ============================================================================
if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")
if (!requireNamespace("htmlwidgets", quietly = TRUE)) install.packages("htmlwidgets")
if (!requireNamespace("htmltools", quietly = TRUE)) install.packages("htmltools")

library(DT)
library(htmlwidgets)
library(htmltools)

report_dir <- "titanic_report"
if (!dir.exists(report_dir)) dir.create(report_dir)

# Save all plots
plots <- list(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12)
for (i in seq_along(plots)) {
  fname <- file.path(report_dir, paste0("plot_", i, ".png"))
  tryCatch({
    ggsave(filename = fname, plot = plots[[i]], width = 7, height = 5, dpi = 150)
  }, error = function(e) {
    message("Could not save plot ", i, ":", e$message)
  })
}

# Create interactive table
dt_widget <- datatable(df, options = list(pageLength = 10, scrollX = TRUE))
table_file <- file.path(report_dir, "table.html")
htmlwidgets::saveWidget(dt_widget, file = table_file, selfcontained = TRUE)

# Build plot rows HTML
plot_imgs <- list.files(report_dir, pattern = "^plot_.*\\.png$", full.names = FALSE)
rows_html <- ""
if (length(plot_imgs) > 0) {
  for (rstart in seq(1, length(plot_imgs), by = 3)) {
    row_imgs <- plot_imgs[rstart:min(rstart+2, length(plot_imgs))]
    imgs_html <- paste0(
      sapply(row_imgs, function(img) {
        sprintf('<div class="plotcell"><img src="%s" alt="%s" loading="lazy"></div>', img, img)
      }),
      collapse = "\n"
    )
    rows_html <- paste0(rows_html, sprintf('<div class="plotrow">%s</div>\n', imgs_html))
  }
}

# HTML template (note the %% to escape % symbols)
main_html <- sprintf('
<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>Titanic EDA Report</title>
  <style>
    body { font-family: Arial, Helvetica, sans-serif; margin: 18px; }
    h1 { color: #2c3e50; }
    .section { margin-bottom: 30px; }
    .plotrow { display: flex; gap: 12px; margin-bottom: 12px; flex-wrap: wrap; }
    .plotcell { flex: 1 1 30%%; text-align: center; }
    .plotcell img { max-width: 100%%; height: auto; border: 1px solid #ddd; border-radius: 6px; padding: 6px; background: #fff; }
    iframe { width: 100%%; height: 520px; border: 1px solid #ccc; border-radius: 6px; }
  </style>
</head>
<body>
  <h1>Titanic EDA Report</h1>
  <div class="section">
    <h2>Interactive Data Table</h2>
    <iframe src="table.html" title="Data table"></iframe>
  </div>
  <div class="section">
    <h2>Plots</h2>
    %s
  </div>
  <footer style="margin-top:30px; color:#888; font-size:0.9em;">
    Report created automatically from R EDA script.
  </footer>
</body>
</html>', rows_html)

# Write HTML and open in browser
html_file <- file.path(report_dir, "index.html")
writeLines(main_html, html_file)
browseURL(html_file)

cat("\n✅ Titanic EDA HTML report generated successfully!\n")
cat("📂 Location:", normalizePath(html_file), "\n")
