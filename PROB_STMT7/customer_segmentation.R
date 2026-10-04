# =====================================================================
# Customer Segmentation & Predictive Analytics (UCI Online Retail) - R
# Sections map to the 12 deliverables in the problem statement.
# Run in Google Colab with the R runtime, or in RStudio.
# =====================================================================

# ---- 0. Setup -------------------------------------------------------
pkgs <- c("tidyverse", "readxl", "factoextra", "cluster", "caret",
          "randomForest", "e1071", "pROC", "plotly", "htmlwidgets")
new <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))
set.seed(42)

# ---- 1. Load data ---------------------------------------------------
# If the download fails, get "Online Retail.xlsx" manually from
# https://archive.ics.uci.edu/dataset/352/online+retail and upload it.
if (!file.exists("Online Retail.xlsx")) {
  download.file("https://archive.ics.uci.edu/static/public/352/online+retail.zip",
                "online_retail.zip", mode = "wb")
  unzip("online_retail.zip")
}
raw <- read_excel("Online Retail.xlsx")
glimpse(raw)
cat("Rows:", nrow(raw), "\n")
print(colSums(is.na(raw)))              # CustomerID has ~135k missing

# ---- 2. Cleaning ----------------------------------------------------
# Missing CustomerID -> dropped (cannot build customer-level features)
# Cancellations (InvoiceNo starts with "C") and Quantity <= 0 -> removed
# UnitPrice <= 0 -> removed (free items / adjustments)
clean <- raw %>%
  mutate(InvoiceNo = as.character(InvoiceNo),
         Date = as.Date(InvoiceDate)) %>%
  filter(!is.na(CustomerID),
         !str_starts(InvoiceNo, "C"),
         Quantity > 0, UnitPrice > 0) %>%
  mutate(Revenue = Quantity * UnitPrice)

cat("Rows after cleaning:", nrow(clean), "\n")
cat("Customers:", n_distinct(clean$CustomerID), "\n")

# ---- 3. Customer-level feature engineering (RFM + extras) -----------
snapshot <- max(clean$Date) + 1

cust <- clean %>%
  group_by(CustomerID) %>%
  summarise(
    Recency       = as.numeric(snapshot - max(Date)),
    Frequency     = n_distinct(InvoiceNo),
    Monetary      = sum(Revenue),
    TotalQuantity = sum(Quantity),
    TenureDays    = as.numeric(max(Date) - min(Date)) + 1,
    .groups = "drop"
  ) %>%
  mutate(
    AvgTransactionValue = Monetary / Frequency,
    # invoices per month; tenure floored at 1 month so one-time buyers
    # don't get an inflated rate
    PurchaseFrequency   = Frequency / pmax(TenureDays / 30, 1)
  )

feat_cols <- c("Recency", "Frequency", "Monetary", "AvgTransactionValue",
               "TotalQuantity", "PurchaseFrequency")
summary(cust[, feat_cols])

# ---- 4. Outlier treatment + scaling ---------------------------------
# Features are heavily right-skewed: log1p first, then cap at 1.5*IQR
cap_iqr <- function(x) {
  q <- quantile(x, c(.25, .75)); i <- q[2] - q[1]
  pmin(pmax(x, q[1] - 1.5 * i), q[2] + 1.5 * i)
}
X_log <- cust %>% select(all_of(feat_cols)) %>%
  mutate(across(everything(), ~ cap_iqr(log1p(.x))))
X <- as.data.frame(scale(X_log))        # z-score standardisation

par(mfrow = c(1, 2))
boxplot(cust$Monetary, main = "Monetary (raw)")
boxplot(X_log$Monetary, main = "Monetary (log + capped)")
par(mfrow = c(1, 1))

# ---- 5. Elbow method + silhouette sweep -----------------------------
d <- dist(X)                             # reused for silhouettes / hclust
print(fviz_nbclust(X, kmeans, method = "wss", k.max = 10, nstart = 25) +
        labs(title = "Elbow Method"))
print(fviz_nbclust(X, kmeans, method = "silhouette", k.max = 10, nstart = 25) +
        labs(title = "Average Silhouette by k"))

# ---- 6. K-Means -----------------------------------------------------
K <- 4          # <- set from the elbow / silhouette plots
km <- kmeans(X, centers = K, nstart = 50)
cust$Cluster_KM <- km$cluster
table(cust$Cluster_KM)

# ---- 7. Hierarchical clustering + dendrogram ------------------------
hc <- hclust(d, method = "ward.D2")
plot(hc, labels = FALSE, hang = -1, main = "Dendrogram (Ward.D2)")
rect.hclust(hc, k = K, border = 2:(K + 1))
cust$Cluster_HC <- cutree(hc, k = K)

# ---- 8. Silhouette comparison + agreement ---------------------------
sil_km <- silhouette(cust$Cluster_KM, d)
sil_hc <- silhouette(cust$Cluster_HC, d)
sil_tbl <- data.frame(Method = c("K-Means", "Hierarchical (Ward)"),
                      AvgSilhouette = c(mean(sil_km[, 3]), mean(sil_hc[, 3])))
print(sil_tbl)
print(fviz_silhouette(sil_km))

ari <- function(a, b) {                  # Adjusted Rand Index
  tab <- table(a, b); n <- sum(tab); c2 <- function(x) x * (x - 1) / 2
  s <- sum(c2(tab)); sa <- sum(c2(rowSums(tab))); sb <- sum(c2(colSums(tab)))
  e <- sa * sb / c2(n); (s - e) / ((sa + sb) / 2 - e)
}
print(table(KMeans = cust$Cluster_KM, Hierarchical = cust$Cluster_HC))
cat("Adjusted Rand Index (KM vs HC):", round(ari(cust$Cluster_KM, cust$Cluster_HC), 3), "\n")

# ---- 9. PCA + 2D visualisation --------------------------------------
pca <- prcomp(X, center = FALSE, scale. = FALSE)   # X already scaled
print(summary(pca))
print(fviz_eig(pca, addlabels = TRUE))
print(fviz_pca_var(pca, repel = TRUE))             # loadings

scores <- as.data.frame(pca$x[, 1:3])
scores$Cluster <- factor(cust$Cluster_KM)
ggplot(scores, aes(PC1, PC2, colour = Cluster)) +
  geom_point(alpha = .6, size = 1.4) +
  stat_ellipse(level = .9) +
  labs(title = "Customer Segments - 2D PCA Projection") +
  theme_minimal()

# ---- 10. Cluster profiling ------------------------------------------
profile <- cust %>%
  group_by(Cluster_KM) %>%
  summarise(Customers = n(),
            Pct = round(100 * n() / nrow(cust), 1),
            across(all_of(feat_cols), median),     # median: robust to outliers
            TotalRevenue = sum(Monetary),
            .groups = "drop") %>%
  mutate(RevenueShare = round(100 * TotalRevenue / sum(TotalRevenue), 1))
print(as.data.frame(profile))
# Use this table to NAME your clusters (e.g. Champions, Loyal, At-Risk,
# Lost/One-time). Naming is a judgement call: write it in your notebook.

# ---- 11. High-value target ------------------------------------------
hv_cluster <- profile$Cluster_KM[which.max(profile$Monetary)]
cat("High-value cluster:", hv_cluster, "\n")

model_df <- X
model_df$Target <- factor(ifelse(cust$Cluster_KM == hv_cluster, "HighValue", "Other"),
                          levels = c("Other", "HighValue"))
print(table(model_df$Target))

idx   <- createDataPartition(model_df$Target, p = 0.7, list = FALSE)
train <- model_df[idx, ]
test  <- model_df[-idx, ]

# ---- 12. Models: Random Forest + SVM --------------------------------
rf <- randomForest(Target ~ ., data = train, ntree = 500, importance = TRUE)
rf_pred <- predict(rf, test)
rf_prob <- predict(rf, test, type = "prob")[, "HighValue"]

tuned <- tune.svm(Target ~ ., data = train, kernel = "radial",
                  cost = c(0.5, 1, 5, 10), gamma = c(0.05, 0.1, 0.5),
                  tunecontrol = tune.control(cross = 5))
svm_fit  <- svm(Target ~ ., data = train, kernel = "radial", probability = TRUE,
                cost = tuned$best.parameters$cost,
                gamma = tuned$best.parameters$gamma)
svm_raw  <- predict(svm_fit, test, probability = TRUE)
svm_pred <- svm_raw
svm_prob <- attr(svm_raw, "probabilities")[, "HighValue"]

# ---- 13. Evaluation -------------------------------------------------
evaluate <- function(name, pred, prob, truth) {
  cm  <- confusionMatrix(pred, truth, positive = "HighValue")
  rc  <- roc(truth, prob, levels = c("Other", "HighValue"), direction = "<", quiet = TRUE)
  row <- data.frame(Model = name,
                    Accuracy  = unname(cm$overall["Accuracy"]),
                    Precision = unname(cm$byClass["Precision"]),
                    Recall    = unname(cm$byClass["Recall"]),
                    F1        = unname(cm$byClass["F1"]),
                    ROC_AUC   = as.numeric(auc(rc)))
  list(cm = cm, roc = rc, row = row)
}
ev_rf  <- evaluate("Random Forest", rf_pred, rf_prob, test$Target)
ev_svm <- evaluate("SVM (RBF)", svm_pred, svm_prob, test$Target)

results <- rbind(ev_rf$row, ev_svm$row)
print(results, digits = 3)
print(ev_rf$cm$table);  print(ev_svm$cm$table)    # confusion matrices

plot(ev_rf$roc, col = "forestgreen", main = "ROC Curves")
plot(ev_svm$roc, col = "firebrick", add = TRUE)
legend("bottomright", c("Random Forest", "SVM"), col = c("forestgreen", "firebrick"), lwd = 2)

# ---- 14. Feature importance (Random Forest) -------------------------
imp <- as.data.frame(importance(rf))
imp$Feature <- rownames(imp)
ggplot(imp, aes(reorder(Feature, MeanDecreaseGini), MeanDecreaseGini)) +
  geom_col(fill = "steelblue") + coord_flip() +
  labs(title = "Random Forest Feature Importance", x = NULL) +
  theme_minimal()

# Sanity check for label leakage: the target comes from clusters built on
# these same features, so near-perfect scores are expected. Retrain without
# the most direct value features to see how much behaviour alone predicts.
lite_cols <- setdiff(feat_cols, c("Monetary", "AvgTransactionValue", "TotalQuantity"))
rf_lite <- randomForest(Target ~ ., data = train[, c(lite_cols, "Target")], ntree = 500)
ev_lite <- evaluate("RF (no Monetary/AvgTxn/Qty)",
                    predict(rf_lite, test),
                    predict(rf_lite, test, type = "prob")[, "HighValue"], test$Target)
print(ev_lite$row, digits = 3)

# ---- 15. Interactive 3D (Plotly) ------------------------------------
fig <- plot_ly(scores, x = ~PC1, y = ~PC2, z = ~PC3, color = ~Cluster,
               type = "scatter3d", mode = "markers",
               marker = list(size = 3, opacity = 0.7)) %>%
  layout(title = "Customer Clusters - 3D PCA")
fig
saveWidget(fig, "clusters_3d.html", selfcontained = TRUE)
# For the PDF submission, rotate to a good angle and screenshot the plot;
# keep clusters_3d.html in your Git repo as the interactive version.

# ---- 16. Marketing recommendations (fill in after reading `profile`) -
# Match each cluster to the closest archetype using the medians above:
#   Champions     (low Recency, high F & M)   -> VIP/early access, loyalty tiers,
#                                                referral rewards; no discounts
#   Loyal/Regular (mid-high F, mid M)         -> bundles, cross-sell, upsell to
#                                                higher basket value
#   At-Risk       (high Recency, past decent  -> win-back emails, time-limited
#                  F/M)                          offers, "we miss you" campaigns
#   Lost/One-time (very high R, F ~ 1, low M) -> low-cost reactivation only,
#                                                suppress from paid channels
