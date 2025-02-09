
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

# Setting 98, 99 as NA
arab_df$Q551A <- ifelse(arab_df$Q551A %in% c(98, 99), NA, arab_df$Q551A)

# Grouping variables
dignity_550 <- c("Q550A_1", "Q550A_2", "Q550A_4", "Q550A_6")

# Subsetting with relevant columns
pillars_df <- arab_df[, c(dignity_550, "Q551A")]

# Removing rows with all NAs
pillars_df <- pillars_df[rowSums(is.na(pillars_df)) < ncol(pillars_df), ]

##----------------------------------------------------------------
##                    Spearman: Q550A, Q551A                     -
##----------------------------------------------------------------

# Treating Q550A as ordinal and Q551A as categorical

spearman_results <- lapply(dignity_550, function(i) {
  
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
      spearman_rho = rho_value,  # Now reporting Spearman rho instead of S-statistic
      df = df_value,  
      p = p_value,  
      sig = ifelse(p_value < 0.05, "*", ""),  
      eff.size = abs(rho_value)  
    )
    
    return(cor.res)
    
    # Troubleshooting: return NA in case of error
  }, error = function(e) {
    data.frame(
      Pillar = i,
      n = NA,
      spearman_rho = NA,
      df = NA,
      p = NA,
      sig = NA,
      eff.size = NA
    )
  })
  
  return(result)
}) %>% bind_rows()

# Renaming
spearman_results <- spearman_results %>%
  mutate(Pillar = recode(Pillar,
                         "Q550A_1" = "Equality under law",
                         "Q550A_2" = "Absence of corruption",
                         "Q550A_4" = "Free election choice",
                         "Q550A_6" = "Civil rights"
  ))

# Formatted table
spearman_results %>%
  kable(
    col.names = c("Pillar", "Sample Size (n)", "Spearman Correlation (p)", "df", 
                  "p-value", "Significance", "Effect Size"),
    caption = "Spearman Correlation Between Q550A and Q551A"
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

print(spearman_results)




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
