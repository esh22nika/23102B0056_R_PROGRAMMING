#####

import_dataset <- function(file_path) {
  df <- tryCatch({
    if (!file.exists(file_path)) {
      stop("File not found at the specified path.")
    }
    data <- read.csv(file_path, stringsAsFactors = FALSE)
    if (ncol(data) == 0) {
      stop("File format is incorrect - no columns detected.")
    }
    data
  },
  error = function(e) {
    message("ERROR while importing dataset: ", conditionMessage(e))
    return(NULL)
  },
  warning = function(w) {
    message("WARNING while importing dataset: ", conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  return(df)
}

file_path <- "PRSA_Data_Aotizhongxin_20130301-20170228.csv"
air_data <- import_dataset(file_path)

if (!is.null(air_data)) {

  cat("\n===== TASK 1: IMPORT AND INSPECT THE DATASET =====\n")

  cat("\n--- First six records ---\n")
  print(head(air_data))

  cat("\n--- Structure of the dataset ---\n")
  str(air_data)

  cat("\n--- Dimensions ---\n")
  cat("Number of rows   :", nrow(air_data), "\n")
  cat("Number of columns:", ncol(air_data), "\n")

  cat("\n--- Missing value check ---\n")
  contains_na <- any(is.na(air_data))
  cat("Dataset contains missing values:", contains_na, "\n")

  total_missing <- sum(is.na(air_data))
  cat("Total number of missing values in dataset:", total_missing, "\n")

} else {
  stop("Dataset could not be loaded. Halting script.")
}

########################################################################
## TASK 2: UNDERSTAND NA, NULL, AND NaN
########################################################################

cat("\n===== TASK 2: UNDERSTAND NA, NULL, AND NaN =====\n")

# NA - unavailable / missing observation
temperature <- c(28, 30, NA, 32)
cat("\nExample of NA:\n")
print(temperature)
cat("is.na()  ->", is.na(temperature), "\n")

# NULL - absent / empty R object
missing_object <- NULL
cat("\nExample of NULL:\n")
print(missing_object)
cat("is.null() ->", is.null(missing_object), "\n")

# NaN - undefined numerical result
undefined_value <- 0 / 0
cat("\nExample of NaN:\n")
print(undefined_value)
cat("is.nan() ->", is.nan(undefined_value), "\n")

cat("\nSummary distinction:\n")
cat(" NA   = value exists conceptually but was not recorded/measured\n")
cat(" NULL = the object itself is empty / has zero length\n")
cat(" NaN  = the value was computed but is mathematically undefined\n")
cat(" Note: is.na(NaN) also returns TRUE, but is.nan(NA) returns FALSE\n")

########################################################################
## TASK 3: MISSING-VALUE SUMMARY FUNCTION
########################################################################

cat("\n===== TASK 3: MISSING-VALUE SUMMARY FUNCTION =====\n")

missing_summary <- function(df, variables) {
  result <- data.frame(
    Variable          = character(),
    Total_Records     = integer(),
    Missing_Values    = integer(),
    Missing_Percentage = numeric(),
    stringsAsFactors = FALSE
  )

  for (var in variables) {
    if (!(var %in% names(df))) {
      message("Variable '", var, "' not found in dataset - skipped.")
      next
    }
    total_records  <- length(df[[var]])
    missing_values <- sum(is.na(df[[var]]))
    missing_pct    <- round((missing_values / total_records) * 100, 2)

    if (missing_pct > 20) {
      warning(paste0("Variable '", var, "' has more than 20% missing values (",
                      missing_pct, "%)."))
    }

    result <- rbind(result, data.frame(
      Variable = var,
      Total_Records = total_records,
      Missing_Values = missing_values,
      Missing_Percentage = missing_pct
    ))
  }
  return(result)
}

selected_vars <- c("PM2.5", "PM10", "SO2", "NO2", "TEMP", "WSPM", "wd")
summary_table <- missing_summary(air_data, selected_vars)

cat("\n--- Missing-Value Summary Table ---\n")
print(summary_table)

write.csv(summary_table, "missing_value_summary.csv", row.names = FALSE)

########################################################################
## TASK 4: IDENTIFY INVALID NUMERICAL RESULTS
########################################################################

cat("\n===== TASK 4: IDENTIFY INVALID NUMERICAL RESULTS =====\n")

air_data$pollution_ratio <- air_data$PM2.5 / air_data$PM10

n_na   <- sum(is.na(air_data$pollution_ratio))
n_nan  <- sum(is.nan(air_data$pollution_ratio))
n_pinf <- sum(is.infinite(air_data$pollution_ratio) & air_data$pollution_ratio > 0)
n_ninf <- sum(is.infinite(air_data$pollution_ratio) & air_data$pollution_ratio < 0)

cat("NA values in pollution_ratio             :", n_na, "\n")
cat("NaN values in pollution_ratio             :", n_nan, "\n")
cat("Positive infinite values in pollution_ratio:", n_pinf, "\n")
cat("Negative infinite values in pollution_ratio:", n_ninf, "\n")

# Replace NaN and Infinite values with NA
air_data$pollution_ratio[is.nan(air_data$pollution_ratio)] <- NA
air_data$pollution_ratio[is.infinite(air_data$pollution_ratio)] <- NA

cat("After replacement, remaining NA count in pollution_ratio:",
    sum(is.na(air_data$pollution_ratio)), "\n")

########################################################################
## TASK 5: HANDLE MISSING NUMERICAL VALUES USING A LOOP
########################################################################

cat("\n===== TASK 5: HANDLE MISSING NUMERICAL VALUES USING A LOOP =====\n")

numeric_variables <- c("PM2.5", "PM10", "SO2", "NO2", "TEMP", "WSPM")

# Store before-treatment counts for later comparison (Task 8)
missing_before <- list()
missing_after  <- list()
medians_used   <- list()

for (var in numeric_variables) {

  if (!(var %in% names(air_data))) {
    message("Column '", var, "' does not exist - skipping.")
    next
  }

  na_count_before <- sum(is.na(air_data[[var]]))
  med_value <- median(air_data[[var]], na.rm = TRUE)

  air_data[[var]][is.na(air_data[[var]])] <- med_value

  na_count_after <- sum(is.na(air_data[[var]]))

  missing_before[[var]] <- na_count_before
  missing_after[[var]]  <- na_count_after
  medians_used[[var]]   <- med_value

  cat("\nVariable          :", var, "\n")
  cat("Missing before     :", na_count_before, "\n")
  cat("Median used        :", round(med_value, 2), "\n")
  cat("Missing after       :", na_count_after, "\n")
}

########################################################################
## TASK 6: HANDLE MISSING CATEGORICAL VALUES
########################################################################

cat("\n===== TASK 6: HANDLE MISSING CATEGORICAL VALUES =====\n")

calculate_mode <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA)
  freq_table <- table(x)
  mode_value <- names(freq_table)[which.max(freq_table)]
  return(mode_value)
}

# Treat empty strings as missing for the categorical variable, if present
air_data$wd[air_data$wd == ""] <- NA

wd_missing_before <- sum(is.na(air_data$wd))
wd_mode <- calculate_mode(air_data$wd)
cat("Mode of 'wd' (wind direction):", wd_mode, "\n")

air_data$wd[is.na(air_data$wd)] <- wd_mode
wd_missing_after <- sum(is.na(air_data$wd))

missing_before[["wd"]] <- wd_missing_before
missing_after[["wd"]]  <- wd_missing_after

cat("Missing values in 'wd' before replacement:", wd_missing_before, "\n")
cat("Missing values in 'wd' after replacement :", wd_missing_after, "\n")

########################################################################
## TASK 7: IMPLEMENT ERROR HANDLING - clean_variable()
########################################################################

cat("\n===== TASK 7: REUSABLE ERROR-HANDLING FUNCTION clean_variable() =====\n")

clean_variable <- function(df, var_name) {
  tryCatch({

    if (!(var_name %in% names(df))) {
      stop(paste0("Variable '", var_name, "' does not exist in the dataset."))
    }

    variable <- df[[var_name]]

    if (!is.numeric(variable)) {
      stop(paste0("Variable '", var_name,
                   "' is categorical, not numerical. Cannot apply median imputation."))
    }

    if (all(is.na(variable))) {
      stop(paste0("Variable '", var_name, "' contains only missing values."))
    }

    med_value <- median(variable, na.rm = TRUE)

    if (is.na(med_value)) {
      stop(paste0("Median could not be calculated for variable '", var_name, "'."))
    }

    variable[is.na(variable)] <- med_value
    message("Variable '", var_name, "' cleaned successfully using median = ",
            round(med_value, 2))
    return(variable)

  }, error = function(e) {
    message("clean_variable() could not process '", var_name, "': ", conditionMessage(e))
    return(NULL)
  })
}

# Demonstration of clean_variable() including deliberate error cases
cat("\n-- Valid case --\n")
cleaned_temp <- clean_variable(air_data, "TEMP")

cat("\n-- Non-existent variable --\n")
invisible(clean_variable(air_data, "HUMIDITY"))

cat("\n-- Categorical variable passed instead of numerical --\n")
invisible(clean_variable(air_data, "wd"))

cat("\n-- All-NA variable --\n")
test_df <- data.frame(dummy = c(NA_real_, NA_real_, NA_real_))
invisible(clean_variable(test_df, "dummy"))

########################################################################
## TASK 8: COMPARE MISSING VALUES BEFORE AND AFTER CLEANING
########################################################################

cat("\n===== TASK 8: COMPARISON TABLE (BEFORE vs AFTER CLEANING) =====\n")

compare_vars <- c(numeric_variables, "wd")

comparison_table <- data.frame(
  Variable = character(),
  Missing_Before = integer(),
  Missing_After = integer(),
  Values_Replaced = integer(),
  stringsAsFactors = FALSE
)

for (var in compare_vars) {
  before <- missing_before[[var]]
  after  <- missing_after[[var]]
  comparison_table <- rbind(comparison_table, data.frame(
    Variable = var,
    Missing_Before = before,
    Missing_After = after,
    Values_Replaced = before - after
  ))
}

print(comparison_table)
write.csv(comparison_table, "missing_value_comparison.csv", row.names = FALSE)

all_clean <- all(comparison_table$Missing_After == 0)
cat("\nAll selected variables fully cleaned (0 missing after treatment):", all_clean, "\n")

########################################################################
## TASK 9: VISUALIZATION - MISSING VALUES BEFORE vs AFTER
########################################################################

cat("\n===== TASK 9: GENERATING VISUALIZATION =====\n")

png("missing_values_before_after.png", width = 900, height = 600)

plot_matrix <- t(as.matrix(comparison_table[, c("Missing_Before", "Missing_After")]))
colnames(plot_matrix) <- comparison_table$Variable

bar_colors <- c("firebrick", "forestgreen")

barplot(plot_matrix,
        beside = TRUE,
        col = bar_colors,
        main = "Missing Values Before vs After Data Cleaning",
        xlab = "Variable",
        ylab = "Number of Missing Values",
        ylim = c(0, max(comparison_table$Missing_Before) * 1.2))

legend("topright",
       legend = c("Before Cleaning", "After Cleaning"),
       fill = bar_colors)

dev.off()
cat("Saved chart to missing_values_before_after.png\n")

########################################################################
## TASK 10: EXPORT THE CLEANED DATASET
########################################################################

cat("\n===== TASK 10: EXPORTING CLEANED DATASET =====\n")

write.csv(air_data, "cleaned_air_quality_data.csv", row.names = FALSE)
cat("Cleaned dataset exported as cleaned_air_quality_data.csv\n")

cat("\n===== SCRIPT EXECUTION COMPLETE =====\n")
