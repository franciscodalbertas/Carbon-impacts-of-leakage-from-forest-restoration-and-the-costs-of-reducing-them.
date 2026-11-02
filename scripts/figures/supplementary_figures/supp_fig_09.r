# Supplementary Figure 09: natural regeneration model importance and ROC curve.
# Run from the repository root.
#

# ---- Packages and paths ------------------------------------------------------
library(ggplot2)
library(dplyr)
library(readr)
library(randomForestSRC)
library(pROC)
library(readr)
data_file <- "input_tables/df_fit_nat_reg_model.csv"
output_dir <- "figures/supp_figures"

if (!file.exists(data_file)) {
  stop("Copy the required dataset to ", data_file, call. = FALSE)
}

# ---- Prepare data (original var_cat = "fito" workflow) -------------------------
df <- as.data.frame(read_csv(data_file, show_col_types = FALSE))

# Fix the seed for repeat runs; use the original seed if reproducing an old fit.
set.seed(123)
train_index <- sample(seq_len(nrow(df)), size = floor(0.7 * nrow(df)))
train_sc <- na.omit(df[train_index, , drop = FALSE])
test_sc <- na.omit(df[-train_index, , drop = FALSE])

for (variable in c("biome", "fitofisionomias_Br")) {
  train_sc[[variable]] <- as.factor(train_sc[[variable]])
  test_sc[[variable]] <- as.factor(test_sc[[variable]])
}

continuous_variables <- names(train_sc)[
  vapply(train_sc, is.numeric, logical(1)) & names(train_sc) != "reg_0_1"
]
# Preserve the supplied script's separate scaling of training and test data.
for (variable in continuous_variables) {
  train_sc[[variable]] <- as.numeric(scale(train_sc[[variable]]))
  test_sc[[variable]] <- as.numeric(scale(test_sc[[variable]]))
}

excluded <- c("x", "y", "agua", "area_urbana", "biome", "reg_0_1")
predictors <- setdiff(names(train_sc), excluded)
model_formula <- reformulate(predictors, response = "reg_0_1")
train_sc <- train_sc %>% mutate(across(where(is.character), as.factor))

# ---- Fit once and predict the held-out 30% ------------------------------------
message("Fitting 400 trees...")
fit_time <- system.time({
  model <- randomForestSRC::rfsrc(
    formula = model_formula, data = train_sc, ntree = 400,
    nodesize = 20, block.size = 1, importance = TRUE
  )
})
message("Model fitting took ", round(fit_time[["elapsed"]], 1), " seconds.")
predicted <- predict(model, newdata = test_sc)$predicted
roc_object <- pROC::roc(test_sc$reg_0_1, predicted, quiet = TRUE)

# ---- Variable importance -----------------------------------------------------
vi <- model$importance
if (!is.numeric(vi) || !is.null(dim(vi)) || is.null(names(vi)) ||
    length(vi) == 0L || any(!is.finite(vi)) || sum(vi) <= 0) {
  stop("Expected a named importance vector with a positive total.")
}
# Permutation importance can be negative; retain these values as in the original.

vi_df <- data.frame(
  variable = names(vi),
  importance = as.numeric(vi)
) %>%
  mutate(
    variable = recode(variable, "fitofisionomias_Br" = "veg_type"),
    relative_importance = importance / sum(importance)
  ) %>%
  arrange(desc(relative_importance))

var_imp <- ggplot(
  vi_df, aes(x = reorder(variable, importance), y = relative_importance)
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(x = NULL, y = "Relative variable importance") +
  theme_classic(base_size = 7)

# ---- ROC curve ---------------------------------------------------------------
roc_df <- data.frame(
  tpr = roc_object$sensitivities,
  fpr = 1 - roc_object$specificities
)

roc_curve <- ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "gray") +
  geom_path(colour = "blue", linewidth = 1) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  coord_equal() +
  labs(x = "False Positive Rate", y = "True Positive Rate") +
  theme_classic(base_size = 7)

# ---- Save both panels --------------------------------------------------------
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(
  file.path(output_dir, "supp_fig_09_A_varimp_pnr.png"), plot = var_imp,
  width = 10, height = 10, units = "cm", dpi = 300, bg = "white"
)
ggsave(
  file.path(output_dir, "supp_fig_09_B_roc_curve_pnr.png"), plot = roc_curve,
  width = 10, height = 10, units = "cm", dpi = 300, bg = "white"
)
