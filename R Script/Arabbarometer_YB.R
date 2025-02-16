
##################################################################
##             Correlation Analysis: Arab Barometer             ##
##################################################################

##---------------------
##  Author: Yuna Baek  
##  Date: Feb 2025     
##---------------------

# Libraries
library(readr)
library(kableExtra)
library(vcd) # for Cramer's V
library(DescTools) # for Cramer's V
library(Hmisc)    # rcorr() for correlation + p-values
library(haven)
library(dplyr)
library(ggthemr)
library(gplots)
library(tidyr)
library(scales)
library(data.table)
library(forcats)

# Load Arab Barometer data
arab_df <- read_csv("/Users/yunabaek/Desktop/2. Policy Lab/Policy-Lab/Data/ArabBarometer_WaveVIII_English_v1.csv")


##------------------
##  Data Wrangling  
##------------------

# Converting 98, 99 to NA
arab_df <- as.data.frame(lapply(arab_df, function(x) {
  x[x %in% c(98, 99)] <- NA
  return(x)
}))

# Grouping variables
dignity_550 <- c("Q550A_1", "Q550A_2", "Q550A_3", "Q550A_4", "Q550A_5", "Q550A_6")

# Subsetting with relevant columns
pillars_df <- arab_df[, c(dignity_550, "Q551A")]

# Removing rows with all NAs
pillars_df <- pillars_df[rowSums(is.na(pillars_df)) < ncol(pillars_df), ]


#################################################################
##              Dependent Variables: Q550A, Q551A              ##
#################################################################

##----------------------------------------------------------------
##                     Spearman Correlation                      -
##----------------------------------------------------------------

# Summary index for 550
pillars_df$summary_index_550A <- rowMeans(
  pillars_df[, c(dignity_550)], 
  na.rm = TRUE
)

# Treating Q550A as ordinal and Q551A as categorical
spearman_results <- lapply(c(dignity_550, "summary_index_550A"), function(i) {
  
  result <- tryCatch({
    # Spearman correlation test
    correl <- cor.test(
      x = as.numeric(as.factor(pillars_df$Q551A)), 
      y = as.numeric(pillars_df[[i]]), 
      method = "spearman", 
      exact = FALSE, 
      use = "pairwise.complete.obs"
    )
    
    # Pearson correlation test (for degrees of freedom)
    pearson_test <- cor.test(
      x = as.numeric(as.factor(pillars_df$Q551A)), 
      y = as.numeric(pillars_df[[i]]), 
      method = "pearson", 
      use = "pairwise.complete.obs"
    )
    
    # Extract values
    rho_value <- round(correl$estimate, 3)  # Spearman correlation coefficient
    p_value <- round(correl$p.value, 4)  # P-value rounded to 4 decimals
    df_value <- pearson_test$parameter  # df approximation
    n_value <- df_value + 2  # Approximate sample size
    
    # Dataframe for results
    cor.res <- data.frame(
      Pillar = i,  
      n = n_value,  
      rho = rho_value,
      df = df_value,  
      p = p_value,  
      Significance = ifelse(p_value < 0.05, "*", "")  
    )
    
    return(cor.res)
    
    # Troubleshooting: return NA in case of error
  }, error = function(e) {
    data.frame(
      Pillar = i,
      n = NA,
      rho = NA,
      df = NA,
      p = NA,
      Significance = NA
    )
  })
  
  return(result)
}) %>% bind_rows()


# Renaming
spearman_results <- spearman_results %>%
  mutate(Pillar = recode(Pillar,
                         "Q550A_1" = "Equality under law",
                         "Q550A_2" = "Absence of corruption",
                         "Q550A_3" = "Basic necessities",
                         "Q550A_4" = "Free election choice",
                         "Q550A_5" = "Safety",
                         "Q550A_6" = "Civil rights",
                         "summary_index_550A" = "Summary Index (All Pillars)"
  ))

# Formatted table
spearman_results %>%
  kable(
    col.names = c("Pillar", "Sample Size (n)", "Spearman Correlation (p)", "df", 
                  "p-value", "Significance"),
    caption = "Spearman Correlation Between Dependent Variables Q550A and Q551A"
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

print(spearman_results)


##----------------------------------------------------------------
##                    Correlation Matrix                         -
##----------------------------------------------------------------

pillars_summary_df <- pillars_df[, c(dignity_550,"summary_index_550A", "Q551A")] %>%
  rename(
    "Q550A: 1. Equality under law" = Q550A_1,
    "Q550A: 2. Absence of corruption" = Q550A_2,
    "Q550A: 3. Basic necessities" = Q550A_3,
    "Q550A: 4. Free election choice" = Q550A_4,
    "Q550A: 5. Feeling safe from physical danger" = Q550A_5,
    "Q550A: 6. Civil rights" = Q550A_6,
    "Q550A: Summary Index" = summary_index_550A,
    "Q551A: Pillars Ranking" = Q551A
  )

# Spearman correlation matrix
spearman_mat <- cor(pillars_summary_df, method = "spearman", use = "pairwise.complete.obs")
spearman_mat

# p-values using rcorr:
pillars_summary_df_mat <- as.matrix(pillars_summary_df)
rcorr_result <- rcorr(pillars_summary_df_mat, type = "spearman")
spearman_r <- rcorr_result$r
spearman_p <- rcorr_result$P

# spearman_r
# spearman_p

# Melt to long format
cor_long <- melt(spearman_r)

# Dataset for the diagonal (Var1 == Var2)
diag_data <- cor_long[cor_long$Var1 == cor_long$Var2, ]

ggplot(cor_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), size = 2.2, color = "black") +
  geom_tile(data = diag_data,
            aes(Var1, Var2),
            fill = "grey90") +
  
  # Correlation 1 in black over grey:
  geom_text(
    data = diag_data,
    aes(Var1, Var2, label = round(value, 2)),
    color = "black", size = 2.2
  ) +
  
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  
  theme_minimal() +
  labs(title = "Spearman Correlation Matrix: Arab Barometer Dependent Variables",
       fill = "Corr") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#################################################################
##                    Independent Variables                    ##
#################################################################
# Select relevant columns from arab_df
independent_vars <- c("Q601_5", "Q211B", "Q303A", "Q303B_1", "Q204A_1", 
                     "Q204A_2", "Q521_1", "Q521_4", "Q521_6", "Q521_2B", "Q521_2A")

# Subsetting with relevant columns
pillars_df <- arab_df[, c(dignity_550, "Q551A", independent_vars)]

# Converting 98, 99 to NA
pillars_df <- as.data.frame(lapply(pillars_df, function(x) {
  x[x %in% c(98, 99)] <- NA
  return(x)
}))

# Removing rows with all NAs
pillars_df <- pillars_df[rowSums(is.na(pillars_df)) < ncol(pillars_df), ]

# Summary Indices for Pillars 4, 6
pillars_df$political_leaders <- rowMeans(
  pillars_df[, c("Q303A", "Q303B_1")], 
  na.rm = TRUE
)

pillars_df$civil_rights <- rowMeans(
  pillars_df[, c("Q204A_1", "Q204A_2", "Q521_1", "Q521_4", 
                 "Q521_6", "Q521_2B", "Q521_2A")], 
  na.rm = TRUE
)

pillars_df <- pillars_df %>% 
  rename(
    "equal_work" = Q601_5,
    "corruption" = Q211B,
    "elections" = Q303A,
    "fair_votes" = Q303B_1,
    "education" = Q204A_1,
    "healthcare" = Q204A_2,
    "freedom_opinion" = Q521_1,
    "freedom_protest" = Q521_4,
    "freedom_religion" = Q521_6,
    "freedom_press" = Q521_2B,
    "freedom_media" = Q521_2A
  )


pillars_summary_df <- pillars_df[, c(dignity_550,"summary_index_550A", "Q551A", "equal_work", "corruption",
                                     "political_leaders", "civil_rights")] %>%
  rename(
    "Dependent: 1. Equality under law" = Q550A_1,
    "Dependent: 2. Absence of corruption" = Q550A_2,
    "Dependent: 4. Free election choice" = Q550A_4,
    "Dependent: 6. Civil rights" = Q550A_6,
    "Dependent: Pillars Ranking" = Q551A
  )

# Spearman correlation matrix
spearman_mat <- cor(pillars_summary_df, method = "spearman", use = "pairwise.complete.obs")
spearman_mat

# p-values using rcorr:
pillars_summary_df_mat <- as.matrix(pillars_summary_df)
rcorr_result <- rcorr(pillars_summary_df_mat, type = "spearman")
spearman_r <- rcorr_result$r
spearman_p <- rcorr_result$P

# spearman_r
# spearman_p

# Melt to long format
cor_long <- melt(spearman_r)

# Dataset for the diagonal (Var1 == Var2)
diag_data <- cor_long[cor_long$Var1 == cor_long$Var2, ]

ggplot(cor_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), size = 2.2, color = "black") +
  geom_tile(data = diag_data,
            aes(Var1, Var2),
            fill = "grey90") +
  
  # Correlation 1 in black over grey:
  geom_text(
    data = diag_data,
    aes(Var1, Var2, label = round(value, 2)),
    color = "black", size = 2.2
  ) +

  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  
  theme_minimal() +
  labs(title = "Spearman Correlation Matrix: Arab Barometer 1, 2, 4, 6",
       fill = "Corr") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







##----------------------------------------------------------------
##                        Discrimination                         -
##----------------------------------------------------------------

# Recode missing values for discrimination-related variables
arab_df[discrimination] <- lapply(arab_df[discrimination], function(x) ifelse(x %in% c(98, 99), NA, x))

# Compute mean score for discrimination variables
arab_df <- arab_df %>% 
  mutate("Discrimination Score" = rowMeans(.[, discrimination], na.rm = TRUE))

# Compute correlations for all dignity and discrimination variable pairs
cor_results <- expand.grid(dignity_550, discrimination) %>%
  rename(Variable1 = Var1, Variable2 = Var2) %>%
  mutate(Correlation = mapply(function(d, dis) cor(arab_df[[d]], arab_df[[dis]], use = "pairwise.complete.obs"), 
                              Variable1, Variable2)) %>%
  filter(!is.na(Correlation))  # Remove NA correlations

"chi 2 test"<-sapply(df[,categ], function( i) 
{chi<-chisq.test(x = df$Q50, i)
cramerv<-DescTools::CramerV(x = df$Q50, y = i)
chi.res<-data.frame(
  name=attr(i, "label"),
  n=sum(chi$observed),
  statistic=chi$statistic,
  df=chi$parameter,
  p=round(
    chi$p.value,4
  ),
  sig=ifelse(chi$p.value<0.05,"*", ""),
  eff.size=cramerv)
return(chi.res)    
}, USE.NAMES = T, simplify = F) %>% 
  bind_rows()

spearman_550 <- cor(df_ord2, method = "spearman", use = "pairwise.complete.obs")


# Display correlation results
print(cor_results)

# Chi-square independence tests with Fisher's Exact Test fallback
chi_sq_results <- expand.grid(dignity, discrimination) %>%
  rename(Variable1 = Var1, Variable2 = Var2) %>%
  rowwise() %>%
  mutate(
    table_data = list(table(arab_df[[Variable1]], arab_df[[Variable2]])),
    expected_counts = list(chisq.test(table_data, simulate.p.value = TRUE)$expected),
    use_fisher = any(expected_counts < 5, na.rm = TRUE),
    test_result = if (use_fisher) fisher.test(table_data) else chisq.test(table_data),
    Chi_Square_Statistic = ifelse(!use_fisher, test_result$statistic, NA),
    Degrees_of_Freedom = ifelse(!use_fisher, test_result$parameter, NA),
    P_Value = test_result$p.value,
    Cramers_V = ifelse(!use_fisher, DescTools::CramerV(table_data), NA)
  ) %>%
  ungroup() %>%
  select(-table_data, -expected_counts, -use_fisher, -test_result) %>%
  filter(!is.na(P_Value))  # Remove invalid tests

# Display chi-square test results
print(chi_sq_results)


##---------------------------------------------------------------
##                      Relevant Questions                      -
##---------------------------------------------------------------

# Grouping Variables
discrimination <- c("Q601_3", "Q601_4", "Q601_5", "Q601_13", "Q601_13B",
                    "Q601_21A", "Q601_21B")





##------------------
##      Attic  
##------------------

##----------------------------------------------------------------
##              Chi-Square Correlation: Q550A, Q551A             -
##----------------------------------------------------------------
# Converting ranking to categorical
for (col in c(dignity_550)) {
  pillars_df[[col]] <- factor(pillars_df[[col]], levels = 1:4, ordered = FALSE)
}

# Chi-square and Cramer's V
chi_results <- lapply(dignity_550, function(var) {
  tbl <- table(pillars_df[[var]], pillars_df[["Q551A"]])
  test <- chisq.test(tbl)
  cramer <- CramerV(tbl)
  data.frame(
    Pillar = var,
    Chi_square = round(test$statistic, 3),
    df = test$parameter,
    p_value = format.pval(test$p.value, digits = 3),
    Cramer_V = round(cramer, 3),
    stringsAsFactors = FALSE
  )
}) %>% bind_rows()

# Renaming
chi_results <- chi_results %>%
  mutate(Pillar = recode(Pillar,
                         "Q550A_1" = "Equality under law",
                         "Q550A_2" = "Absence of corruption",
                         "Q550A_4" = "Free election choice",
                         "Q550A_6" = "Civil rights"
  ))

# Formatted table
chi_results %>%
  kable(
    col.names = c("Pillars", "Chi-square", "df", "p-value", "Cramer's V"),
    caption = "Chi-square Tests and Effect Sizes Between Q550A and Q551A"
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))


# ##---------------------------------------------------------------
# ##                       Summary Index PCA                      -
# ##---------------------------------------------------------------
#
# # PCA
# # Principal Component Analysis (PCA)
# pillars_pca_df <- prcomp(pillars_df[, c("Q550A_1", "Q550A_2", "Q550A_4", "Q550A_6")], center = TRUE, scale. = TRUE)
# 
# # Use the first principal component as the summary index
# pillars_pca_df$summary_index <- pillars_pca_df$x[, 1]
# 
# library(corrr)
# 
# # Compute correlation matrix
# cor_matrix <- cor(pillars_df[, c("Q550A_1", "Q550A_2", "Q550A_4", "Q550A_6", "summary_index")], use = "pairwise.complete.obs")
# 
# # Print correlation matrix
# print(cor_matrix)
# 
# library(ggcorrplot)
# 
# # Generate heatmap
# ggcorrplot(cor_matrix, 
#            method = "square", 
#            type = "lower", 
#            lab = TRUE, 
#            title = "Correlation Heatmap: Q550A Variables & Summary Index")

