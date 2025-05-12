# ========================================================================
# Clear Environment and Load Required Libraries
# ========================================================================

# Clear all objects in the environment to start fresh
rm(list = ls())
getwd()

# Load necessary libraries
library(dplyr)       # For data manipulation
library(openxlsx)    # For working with Excel files

library(purrr)
library(stringr)

# ========================================================================
# Define Subgroup for Analysis
# ========================================================================

# Select relevant ICDO codes for analysis
#subgroup <- c("C16", "C25")  # Choose just Stomach and Pancreatic Cancer
subgroup <- unique(substr(d_tnm$ICDO, 1, 3))  # Choose all ICDO Values
     #d_tnm <- d_tnm[substr(d_tnm$ICDO, 1, 3)  %in% subgroup,]    #this will be applied later

# ========================================================================
# Define Function to Calculate Frequency and Percentage
# ========================================================================

calc_freq_percent <- function(column, stratify_by = NULL, filter_condition = NULL, 
                              included_categories = NULL, exclude_na = TRUE) {
  
  # Apply filter condition if provided
  if (!is.null(filter_condition)) {
    column <- column[filter_condition]
    if (!is.null(stratify_by)) stratify_by <- stratify_by[filter_condition]
  }
  
  # Remove NA values if requested
  if (exclude_na) {
    non_na_idx <- !is.na(column)
    column <- column[non_na_idx]
    if (!is.null(stratify_by)) stratify_by <- stratify_by[non_na_idx]
  }
  
  # Convert input to character for safer matching
  column <- as.character(column)
  if (!is.null(stratify_by)) stratify_by <- as.character(stratify_by)
  if (!is.null(included_categories)) included_categories <- as.character(included_categories)
  
  # Unstratified calculation
  if (is.null(stratify_by)) {
    freq_table <- table(column, useNA = "always")
    result <- data.frame(
      Item = names(freq_table),
      Freq = as.numeric(freq_table),
      Percent.Freq = round(100 * freq_table / sum(freq_table), 2),
      stringsAsFactors = FALSE
    )
    
    # Optional column for Percent.Selected
    if (!is.null(included_categories)) {
      result$Percent.Selected <- ifelse(
        result$Item %in% included_categories,
        round(100 * result$Freq / sum(result$Freq[result$Item %in% included_categories]), 2),
        NA
      )
    }
    
  } else {
    freq_table <- as.data.frame(table(column, stratify_by, useNA = "always"))
    colnames(freq_table) <- c("Item", "Stratification", "Freq")
    
    result <- freq_table %>%
      group_by(Stratification) %>%
      mutate(
        Percent.Freq = round(100 * Freq / sum(Freq), 2),
        Percent.Selected = if (!is.null(included_categories)) {
          total_selected <- sum(Freq[Item %in% included_categories])
          ifelse(Item %in% included_categories,
                 round(100 * Freq / total_selected, 2),
                 NA)
        } else {
          NULL
        }
      ) %>%
      ungroup() %>%
      arrange(Stratification, Item)
  }
  
  return(result)
}

# ========================================================================
# Load and Filter TNM Data based on the subgroup selected above (line 17, 18)
# ========================================================================

# Load TNM dataset
d_tnm <- read.delim("input_data/data_tnm_full.csv", header = TRUE, sep = ";", as.is = TRUE)
colnames(d_tnm)

# Filter data based on the chosen subgroup (ICDO codes)
d_tnm <- d_tnm[substr(d_tnm$ICDO, 1, 3) %in% subgroup,]
dim(d_tnm)

# Check 'LOC' and 'ICD10' columns for anomalies
table(d_tnm$LOC)
head(d_tnm$ICD10)

# Check for empty or missing values in key columns
table(d_tnm$SIEWERT)
table(d_tnm$P16)  # Expected to be empty for "C16" and "C25"

# Ensure each ID is unique
table(table(d_tnm$ID))

# Check for missing or invalid values in TNM-related columns
table(is.na(d_tnm$TNM_PT))
table(d_tnm$TNM_PT == "")
                  
# ========================================================================
# Determine Correct TNM Values and Determine if TNM is clinical or pathological
# ========================================================================

library(dplyr)

d_tnm <- d_tnm %>%
  mutate(
    # Assign TNM values: use pathological if valid, else clinical (or when 'y' is present)
    TNM_T = ifelse(is.na(TNM_PT) | TNM_PT %in% c('', 'x', 'X') | TNM_PY == 'y', TNM_CT, TNM_PT),
    TNM_N = ifelse(is.na(TNM_PN) | TNM_PN %in% c('', 'x', 'X') | TNM_PY == 'y', TNM_CN, TNM_PN),
    TNM_M = ifelse(is.na(TNM_PM) | TNM_PM %in% c('', 'x', 'X') | TNM_PY == 'y', TNM_CM, TNM_PM),
    
    # Classify as clinical or pathological
    TNM_T_cp = ifelse(TNM_T == '', '', ifelse(is.na(TNM_PT) | TNM_PT %in% c('', 'x', 'X') | TNM_PY == 'y', "clinical", "pathological")),
    TNM_N_cp = ifelse(TNM_N == '', '', ifelse(is.na(TNM_PN) | TNM_PN %in% c('', 'x', 'X') | TNM_PY == 'y', "clinical", "pathological")),
    TNM_M_cp = ifelse(TNM_M == '', '', ifelse(is.na(TNM_PM) | TNM_PM %in% c('', 'x', 'X') | TNM_PY == 'y', "clinical", "pathological")),
    
    # Handle "y" case: assign 'y_pathological' if pathological values exist
    TNM_T_cp = ifelse(TNM_T == '' & !TNM_PT %in% c('', 'x', 'X') & TNM_PY == 'y', "y_pathological", TNM_T_cp),
    TNM_N_cp = ifelse(TNM_N == '' & !TNM_PN %in% c('', 'x', 'X') & TNM_PY == 'y', "y_pathological", TNM_N_cp),
    TNM_M_cp = ifelse(TNM_M == '' & !TNM_PM %in% c('', 'x', 'X') & TNM_PY == 'y', "y_pathological", TNM_M_cp),
    
    # Replace missing TNM with pathological if "y" is present
    TNM_T = ifelse(TNM_T == '' & !TNM_PT %in% c('', 'x', 'X') & TNM_PY == 'y', TNM_PT, TNM_T),
    TNM_N = ifelse(TNM_N == '' & !TNM_PN %in% c('', 'x', 'X') & TNM_PY == 'y', TNM_PN, TNM_N),
    TNM_M = ifelse(TNM_M == '' & !TNM_PM %in% c('', 'x', 'X') & TNM_PY == 'y', TNM_PM, TNM_M),
    
    # Assign M value from METASTASIS field if missing
    TNM_M_cp = ifelse(TNM_M %in% c('', 'x', 'X'),
                      ifelse(METASTASIS %in% c("yes", "Yes"), "metastasis_yes",
                             ifelse(METASTASIS %in% c("no", "No"), "metastasis_no", TNM_M_cp)),
                      TNM_M_cp),
    TNM_M = ifelse(TNM_M %in% c('', 'x', 'X'),
                   ifelse(METASTASIS %in% c("yes", "Yes"), 1,
                          ifelse(METASTASIS %in% c("no", "No"), 0, TNM_M)),
                   TNM_M),
    
    # Flag cases with completely missing TNM
    TNM_miss = ifelse(TNM_T %in% c('', 'x', 'X') & TNM_N %in% c('', 'x', 'X') & TNM_M %in% c('', 'x', 'X'), "missing", "no")
  )

table(d_tnm$TNM_miss)
table(d_tnm$TNM_T)
table(d_tnm$TNM_N)
table(d_tnm$TNM_M)

# ========================================================================
# Process and Validate TNM Variables with Mapping Table mapping_defined_TNM.csv
# ========================================================================

# Replace '+' with 1 in TNM_N
d_tnm$TNM_N <- ifelse(d_tnm$TNM_N == '+', 1, d_tnm$TNM_N)

# Load mapping table for TNM definitions
d_tnm_map <- read.delim( "mapping_tables/mapping_defined_TNM.csv", fileEncoding = "UTF-8-BOM", header = TRUE, sep = ";")

# Extract and standardize TNM_T
# TNM_T_1: First digit
# TNM_T_2: Two- or three-digit codes for specific cancers like C53

# TNM_T_1 processing
d_tnm$TNM_T_1 <- ifelse(substr(d_tnm$TNM_T, 1, 1) %in% d_tnm_map$TNM_T_1, substr(d_tnm$TNM_T, 1, 1), NA)
d_tnm$TNM_T_1 <- ifelse(d_tnm$TNM_T_1 %in% c('X', 'x', '') | is.na(d_tnm$TNM_T_1), 'X', d_tnm$TNM_T_1)

# TNM_T_2 processing
d_tnm$TNM_T_2 <- ifelse(substr(d_tnm$ICDO, 1, 3) == 'C53' & substr(d_tnm$TNM_T, 1, 3) %in% d_tnm_map$TNM_T_3_C53,
                        substr(d_tnm$TNM_T, 1, 3),
                        ifelse(substr(d_tnm$TNM_T, 1, 2) %in% d_tnm_map$TNM_T_2, substr(d_tnm$TNM_T, 1, 2),
                               ifelse(substr(d_tnm$TNM_T, 1, 3) %in% d_tnm_map$TNM_T_3, substr(d_tnm$TNM_T, 1, 3), NA)))

# Extract and standardize TNM_N, set to 0 v and set _cp to 0 if missing and set _cp to 0
# TNM_N_1 processing
d_tnm$TNM_N_1 <- ifelse(substr(d_tnm$TNM_N, 1, 1) %in% d_tnm_map$TNM_N_1, substr(d_tnm$TNM_N, 1, 1), NA)
d_tnm$TNM_N_cp <- ifelse(d_tnm$TNM_N_1 %in% c('', NA), 0, d_tnm$TNM_N_cp)
d_tnm$TNM_N_1 <- ifelse(d_tnm$TNM_N_1 %in% c('', NA), 0, d_tnm$TNM_N_1)

# TNM_N_2 processing
d_tnm$TNM_N_2 <- ifelse(substr(d_tnm$TNM_N, 1, 2) %in% d_tnm_map$TNM_N_2, substr(d_tnm$TNM_N, 1, 2),
                        ifelse(substr(d_tnm$TNM_N, 1, 3) %in% d_tnm_map$TNM_N_3, substr(d_tnm$TNM_N, 1, 3), NA))

# Extract and standardize TNM_M, set to 0 v and set _cp to 0 if missing and set _cp to 0
# TNM_M_1 processing
d_tnm$TNM_M_1 <-  ifelse(substr(d_tnm$TNM_M, 1, 1) %in% d_tnm_map$TNM_M_1, substr(d_tnm$TNM_M, 1, 1), NA)
d_tnm$TNM_M_cp <- ifelse(d_tnm$TNM_M_1 %in% c('', NA), 0, d_tnm$TNM_M_cp)
d_tnm$TNM_M_1 <-  ifelse(d_tnm$TNM_M_1 %in% c('', NA), 0, d_tnm$TNM_M_1)

# TNM_M_2 processing
d_tnm$TNM_M_2 <- ifelse(substr(d_tnm$TNM_M, 1, 2) %in% d_tnm_map$TNM_M_2, substr(d_tnm$TNM_M, 1, 2), NA)


# Check TNM
table(d_tnm$TNM_T, d_tnm$TNM_T_1, useNA = "always")
table(d_tnm$TNM_T, d_tnm$TNM_T_2, useNA = "always")
table(d_tnm$TNM_N, d_tnm$TNM_N_1, useNA = "always")
table(d_tnm$TNM_N, d_tnm$TNM_N_2, useNA = "always")
table(d_tnm$TNM_M, d_tnm$TNM_M_1, useNA = "always")
table(d_tnm$TNM_M, d_tnm$TNM_M_2, useNA = "always")

# Check completeness of TNM
# TNM_complete = 0 if any TNM_T, TNM_N, or TNM_M are missing ('x', 'X', or '')
d_tnm$TNM_complete <- ifelse(d_tnm$TNM_T %in% c('x', 'X', '') | 
                               d_tnm$TNM_N %in% c('x', 'X', '') | 
                               d_tnm$TNM_M %in% c('x', 'X', ''), 0, 1)
table(d_tnm$TNM_complete)

# Display distributions of clinical/pathological TNM assignments
table(d_tnm$TNM_T_cp, useNA = "always")
table(d_tnm$TNM_N_cp, useNA = "always")
table(d_tnm$TNM_M_cp, useNA = "always")

# ========================================================================
# Export TNM Origin
# ========================================================================

# Function to calculate frequency and percentage summaries
calc_and_combine <- function(type, column) {
  freq <- table(column)
  percent <- round(100 * freq / sum(freq), 2)
  c(
    type,
    freq[c("clinical", "pathological", "y_pathological", "0", "metastasis_yes", "metastasis_no")],
    percent[c("clinical", "pathological", "y_pathological", "0", "metastasis_yes", "metastasis_no")]
  )
}
# Generate TNM origin summary table
pathol <- rbind(
  calc_and_combine("T", d_tnm$TNM_T_cp),
  calc_and_combine("N", d_tnm$TNM_N_cp),
  calc_and_combine("M", d_tnm$TNM_M_cp)
)

# Format and export summary
colnames(pathol) <- c(
  "TNM", "clinical", "pathological", "y_pathological", "imputed_0", "metastasis_yes", "metastasis_no",
  "clinical%", "pathological%", "y_pathological%", "imputed_0%", "metastasis_yes%", "metastasis_no%"
)
pathol <- as.data.frame(pathol)
write.xlsx(pathol,  "results/Result8_FreqOriginTNM.xlsx", sheetName = "Sheet1")

# ========================================================================
# Change language if necessary: positiv -> positive, negativ -> negative 
# ========================================================================

d_tnm$P16 <- ifelse(d_tnm$P16 == "negativ", "negative", ifelse(d_tnm$P16 == "positiv", "positive", d_tnm$P16 ))
d_tnm$EBV <- ifelse(d_tnm$EBV == "negativ", "negative", ifelse(d_tnm$EBV == "positiv", "positive", d_tnm$EBV ))

# ========================================================================
# Assign UICC Version from Year with Mapping Table mapping_version.csv
# ========================================================================

# Load TNM version mapping
d_vers <- read.delim( "mapping_tables/mapping_version.csv", fileEncoding = "UTF-8-BOM", header = TRUE, sep = ";", as.is = TRUE)

# Assign TNM version based on available data
d_tnm$UICC_vers <- NA
for (i in 1:nrow(d_vers)) {       
  d_tnm$UICC_vers <- ifelse(is.na(d_tnm$TNM_VERSION) | (d_tnm$TNM_VERSION == ''),
                            ifelse(is.na(d_tnm$UICC_vers), # Preserve existing values
                                   ifelse(d_tnm$DIAG_YEAR >= d_vers$year[i], d_vers$version[i], d_tnm$UICC_vers),   
                                   d_tnm$UICC_vers),
                            d_tnm$TNM_VERSION)
}                        

d_tnm$UICC_vers <- ifelse(is.na(d_tnm$UICC_vers), 8, d_tnm$UICC_vers)  # Default version 8 if missing

# ========================================================================
# Map Grading Information with Mapping Table mapping_grading.csv
# ========================================================================

# Load grading mapping table
d_grad <- read.delim( "mapping_tables/mapping_grading.csv", fileEncoding = "UTF-8-BOM", header = TRUE, sep = ";", as.is = TRUE)
colnames(d_grad)[1] <- substr(colnames(d_grad)[1], 4, 20)

# Map grading information to TNM data
d_tnm$Grad <- d_grad$Grad[match(d_tnm$GRA, d_grad$Grad)]
d_tnm$Grad <- ifelse(is.na(d_tnm$Grad), "GX", d_tnm$Grad)
table(d_tnm$GRA, d_tnm$Grad, useNA = "always" )

# ========================================================================
# Define Tumor Subtypes, Sarcoma with Mapping Table mapping_sarcoma_hist.csv
# ========================================================================

# Load sarcoma histology codes
d_sarcoma <- read.delim( "mapping_tables/mapping_sarcoma_hist.csv", fileEncoding = "UTF-8-BOM", header = TRUE, sep = ";", as.is = TRUE)
colnames(d_sarcoma)[1] <- substr(colnames(d_sarcoma)[1], 4, 20)
d_tnm$sarcoma <- ifelse(substr(d_tnm$HIS, 2, 10) %in% d_sarcoma$sarcoma_hist, "yes", "no")
table(d_tnm$sarcoma)
table(substr(d_tnm$HIS, 2, 10) %in% d_sarcoma$sarcoma_hist)

# Clean and flag special values
d_tnm$AGE <- ifelse(d_tnm$AGE < 0, NA, d_tnm$AGE)
table(d_tnm$AGE)
table(d_tnm$AGE < 45)

d_tnm$SIEWERT <- ifelse(substr(d_tnm$ICDO, 1, 5) == "C16.0" & is.na(d_tnm$SIEWERT), "noSiewert", d_tnm$SIEWERT)
table(d_tnm$SIEWERT)

d_tnm$P16 <- ifelse(is.na(d_tnm$P16), "no_p16", d_tnm$P16)
table(d_tnm$P16)

# Define tumor types based on histology codes
d_tnm$gist <- ifelse(d_tnm$HIS %in% c("M8936/1", "M8936/3", "M8931/3"), 1, 0)
table(d_tnm$gist)

d_tnm$uterusSarkoma <- ifelse(d_tnm$HIS %in% c("M8890/3", "M8930/3", "M8933/3"), 1, 0)
table(d_tnm$uterusSarkoma)

d_tnm$malignMesothelioma <- ifelse(d_tnm$HIS %in% c("M9050/3", "M9051/3", "M9052/3", "M9053/3"), 1, 0)
table(d_tnm$malignMesothelioma)

d_tnm$MerkelZellkarzinom <- ifelse(d_tnm$HIS %in% c("M8247/3"), 1, 0)
table(d_tnm$MerkelZellkarzinom)

d_tnm$EwingSarkoma <- ifelse(d_tnm$HIS %in% c("M9260/3"), 1, 0)
table(d_tnm$EwingSarkoma)

d_tnm$malignantMelanoma <- ifelse((substr(d_tnm$HIS, 2, 4) >= 872) & (substr(d_tnm$HIS, 2, 4) <= 879), 1, 0)
table(d_tnm$malignantMelanoma)

d_tnm$NET <- ifelse(d_tnm$HIS %in% c("M8240/3", "M8246/3", "M8249/3"), 1, 0)
table(d_tnm$NET)


# ========================================================================
# Define ICDO_loc Based on LOC (if given), or on Localization Mapping Table with Mapping Table mapping_table_R_loc_MUST.csv
# ========================================================================

d_locmap <- read.delim("copyright_catalogs/mapping_table_R_loc_MUST.csv", fileEncoding = "UTF-8-BOM", header = TRUE, sep = ";", as.is = TRUE, na.strings = "")
d_locmap <- replace(d_locmap, is.na(d_locmap), "")
d_locmap <- unique(d_locmap)
dim(d_locmap)

# Initialize ICDO location variable
d_tnm$ICDO_loc <- NA  
d_tnm$source_loc <- NA

# Assign ICDO_loc directly from LOC if defined; otherwise attempt to map using d_locmap
d_tnm <- d_tnm %>%
  mutate(ICDO_loc = ifelse(!is.na(LOC) & LOC != "", LOC, NA)) %>%
  left_join(d_locmap %>% select(LOC) %>% distinct(), by = "LOC") %>%
  mutate(ICDO_loc = coalesce(ICDO_loc, LOC))

# Assign ICDO_loc from conditions
for (i in 1:nrow(d_locmap)) {
  match_cond <- is.na(d_tnm$ICDO_loc) & (
    d_tnm$ICDO == "" |
      (
        (sapply(substr(d_tnm$ICDO, 1, 5), grepl, d_locmap$ICDO5[i]) |
           sapply(substr(d_tnm$ICDO, 1, 3), grepl, d_locmap$ICDO3[i])) &
          (sapply(d_tnm$UICC_vers, grepl, d_locmap$UICC_vers[i]) | d_locmap$UICC_vers[i] == "") &
          (d_locmap$gist[i] == d_tnm$gist | d_locmap$gist[i] == "") &
          (d_locmap$uterusSarkoma[i] == d_tnm$uterusSarkoma | d_locmap$uterusSarkoma[i] == "") &
          (d_locmap$malignMesothelioma[i] == d_tnm$malignMesothelioma | d_locmap$malignMesothelioma[i] == "") &
          (d_locmap$MerkelZellkarzinom[i] == d_tnm$MerkelZellkarzinom | d_locmap$MerkelZellkarzinom[i] == "") &
          (d_locmap$EwingSarkoma[i] == d_tnm$EwingSarkoma | d_locmap$EwingSarkoma[i] == "") &
          (d_locmap$malignantMelanoma[i] == d_tnm$malignantMelanoma | d_locmap$malignantMelanoma[i] == "") &
          (d_locmap$NET[i] == d_tnm$NET | d_locmap$NET[i] == "") &
          ((d_locmap$HIST_not[i] == "not adeno" & !grepl("adeno", d_tnm$HIST, ignore.case = TRUE)) | d_locmap$HIST_not[i] == "") &
          (grepl(d_locmap$HIST_grep[i], d_tnm$HIST, ignore.case = TRUE) | d_locmap$HIST_grep[i] == "") &
          (grepl(d_locmap$ORG_unit[i], d_tnm$ORG_UNIT) | d_locmap$ORG_unit[i] == "") &
          ((is.na(d_tnm$SIEWERT) == FALSE & d_tnm$SIEWERT == d_locmap$siewert[i]) | d_locmap$siewert[i] == "") &
          ((d_locmap$age[i] == "<55" & d_tnm$AGE < 55) | 
             (d_locmap$age[i] == ">=55" & d_tnm$AGE >= 55) | 
             (d_locmap$age[i] == "<45" & d_tnm$AGE < 45) | 
             (d_locmap$age[i] == ">=45" & d_tnm$AGE >= 45) | 
             d_locmap$age[i] == "") &
          (grepl(d_locmap$p16.stad[i], d_tnm$P16) | d_locmap$p16.stad[i] == "") &
          ((d_locmap$sarcoma[i] == "no" & d_tnm$sarcoma == "no") | d_locmap$sarcoma[i] == "") &
          ((is.na(d_tnm$DETAIL) == FALSE & d_tnm$DETAIL == d_locmap$detail[i]) | d_locmap$detail[i] == "") &
          ((is.na(d_tnm$EBV) == FALSE & d_tnm$EBV == d_locmap$EBV[i]) | d_locmap$EBV[i] == "")
      )
  )
  
  d_tnm$ICDO_loc[match_cond] <- ifelse(d_tnm$ICDO[match_cond] == "", "no ICD-O", d_locmap$LOC[i])
  d_tnm$source_loc[match_cond] <- ifelse(d_tnm$ICDO[match_cond] == "", "no ICD-O", d_locmap$source[i])
}

# Display the tables
table(d_tnm$LOC)
table(d_tnm$ICDO_loc)
table(d_tnm$source_loc)

# ========================================================================
# Filter and Export Results for Localization
# ========================================================================

# Save results where ICDO_loc is missing 
write.xlsx(
  d_tnm %>%
    filter(is.na(ICDO_loc)) %>%
    select(ICDO_loc, ICDO, ORG_UNIT, HIST, HIS, UICC_vers, SIEWERT, P16),
  "results/Result5.2_ListLocUnknown.xlsx",
  sheetName = "Sheet1"
)

# Generate and export summarized location table (3-digit ICDO, mapped location, source, freq & percent)
loc_tab0 <- d_tnm %>%
  mutate(original_ICDO = substr(ICDO, 1, 3)) %>%
  count(original_ICDO, mapped_location = ICDO_loc, source_loc, name = "Frequency") %>%
  mutate(
    Percent = round(100 * Frequency / sum(Frequency), 2),
    Freq_percent_str = round(100 * Frequency / ave(Frequency, mapped_location, FUN = sum), 2)
  ) %>%
  filter(Frequency > 0) %>%
  arrange(mapped_location)

# Export to Excel
write.xlsx(loc_tab0, "results/Result5.1_FreqLocAssigned.xlsx", sheetName = "Sheet1")

# Display ICDO_loc statistics
table(d_tnm$ICDO_loc, useNA = "always")

# Check distribution of ICDO and UICC versions
table(d_tnm$ICDO_loc)
table(d_tnm$UICC_vers)
table(is.na(d_tnm$ICDO_loc))

# ========================================================================
# Generate Mapping Table mapping_additional_uicc.csv out of mapping_uicc.csv
# ========================================================================

source("scripts/script_create_additional_RWD_UICC_formatted.R")

# ========================================================================
# Generate UICC TNM Key with Mapping Table mapping_additional_uicc.csv
# ========================================================================

# Load additional mapping table
d_map <- read.delim("copyright_catalogs/mapping_uicc_additional.csv", fileEncoding = "UTF-8-BOM", header = TRUE, sep = ";", as.is = TRUE)
table(d_map$mapping, useNA = "always")

# Initialize
d_tnm$MUST_UICC <- NA
d_tnm$mapping <- NA
d_tnm$T <- NA
d_tnm$N <- NA
d_tnm$M <- NA
d_tnm$grading   <- NA
d_tnm$patho_clin   <- NA
d_tnm$Vers   <- NA
d_tnm$loc   <- NA

d_tnm$TNM_T_cp_map <- ifelse(d_tnm$TNM_T_cp=="clinical", "c", ifelse(d_tnm$TNM_T_cp=="pathological", "p",ifelse(d_tnm$TNM_T_cp=="y_pathological", "p",NA)))

table(d_tnm$TNM_T_cp_map)

# Loop over Mapping-rules
for (i in seq_len(nrow(d_map))) {
  match_cond <- is.na(d_tnm$MUST_UICC) &
    d_tnm$ICDO_loc == d_map$LOC[i] &
    d_tnm$UICC_vers == d_map$UICC_vers[i] &
    (d_map$clin_patho[i]=="" | d_tnm$TNM_T_cp_map == d_map$clin_patho[i]) &
    (d_tnm$TNM_T_1 == d_map$TNM_T[i] | d_tnm$TNM_T_2 == d_map$TNM_T[i] | d_map$TNM_T[i] == "*") &
    (d_tnm$TNM_N_1 == d_map$TNM_N[i] | d_tnm$TNM_N_2 == d_map$TNM_N[i] | d_map$TNM_N[i] == "*") &
    (d_tnm$TNM_M_1 == d_map$TNM_M[i] | d_tnm$TNM_M_2 == d_map$TNM_M[i] | d_map$TNM_M[i] == "*") &
    (d_tnm$Grad == d_map$Grad[i] | d_map$Grad[i] == "")
  
  if (any(match_cond, na.rm = TRUE)) {
    d_tnm$MUST_UICC[match_cond] <- d_map$UICC_stage[i]
    d_tnm$mapping[match_cond] <- d_map$mapping[i]
    d_tnm$T[match_cond] <- d_map$TNM_T[i]
    d_tnm$N[match_cond] <- d_map$TNM_N[i]
    d_tnm$M[match_cond] <- d_map$TNM_M[i]
    d_tnm$grading[match_cond]  <- d_map$Grad[i]
    d_tnm$patho_clin[match_cond]  <- d_map$clin_patho[i]
    d_tnm$Vers[match_cond]  <- d_map$UICC_vers[i]
    d_tnm$loc[match_cond]  <- d_map$LOC[i]
  }
}

table(d_tnm$MUST_UICC)
table(d_tnm$mapping)
table(d_tnm$T)
table(d_tnm$N)
table(d_tnm$M)
table(d_tnm$grading)
table(d_tnm$patho_clin)
table(d_tnm$Vers)
table(d_tnm$loc)

# ========================================================================
# Assign 'no carcinoma' Label
# ========================================================================

d_tnm$MUST_UICC <- ifelse((substr(d_tnm$ICD10, 1, 1) == 'C') | (substr(d_tnm$ICD10, 1, 2) == 'D0'),
                          d_tnm$MUST_UICC,
                          'no carcinoma')

# ========================================================================
# Add Clinical/Pathological TNM Information
# ========================================================================

d_tnm$TNM_T_cp_short <- ifelse(d_tnm$TNM_T_cp == "clinical", "c", ifelse(d_tnm$TNM_T_cp == "pathological", "p", ifelse(d_tnm$TNM_T_cp == "y_pathological", "y_p", d_tnm$TNM_T_cp)))
d_tnm$TNM_N_cp_short <- ifelse(d_tnm$TNM_N_cp == "clinical", "c", ifelse(d_tnm$TNM_N_cp == "pathological", "p", ifelse(d_tnm$TNM_N_cp == "y_pathological", "y_p", d_tnm$TNM_N_cp)))
d_tnm$TNM_M_cp_short <- ifelse(d_tnm$TNM_M_cp == "clinical", "c", ifelse(d_tnm$TNM_M_cp == "pathological", "p", ifelse(d_tnm$TNM_M_cp == "y_pathological", "y_p", ifelse(d_tnm$TNM_M_cp == "metastasis_no", "m_no", ifelse(d_tnm$TNM_M_cp == "metastasis_yes", "m_yes", d_tnm$TNM_M_cp)))))
d_tnm$TNMcpy0 <- paste0(d_tnm$TNM_T_cp_short, "/", d_tnm$TNM_N_cp_short, "/", d_tnm$TNM_M_cp_short)

# ============================================================
# Compare DOCUMENTED_UICC with MUST_UICC and classify match status
# ============================================================

d_tnm$discrepant <- ifelse(
  !is.na(d_tnm$DOCUMENTED_UICC) & !is.na(d_tnm$MUST_UICC),
  ifelse(d_tnm$DOCUMENTED_UICC == d_tnm$MUST_UICC, "UICC both matching", "UICC both discrepant"),
  ""
)
table(d_tnm$discrepant)

# Store only cases where UICC classifications differ
d_tnm_discrepant <- d_tnm[d_tnm$discrepant == "UICC both discrepant", ]

# Derive UICC stage groups (e.g., I, II, III, IV) from full stage labels
d_tnm$MUST_UICC_gr <- gsub("^([IV]+).*", "\\1", d_tnm$MUST_UICC)
d_tnm$doc_UICC_gr <- gsub("^([IV]+).*", "\\1", d_tnm$DOCUMENTED_UICC)

table(d_tnm$MUST_UICC_gr, d_tnm$MUST_UICC)
table(d_tnm$doc_UICC_gr, d_tnm$DOCUMENTED_UICC)

# Compare group-level stages between documented and calculated values
d_tnm$uneq_gr <- ifelse(
  !is.na(d_tnm$doc_UICC_gr) & !is.na(d_tnm$MUST_UICC_gr),
  ifelse(d_tnm$doc_UICC_gr == d_tnm$MUST_UICC_gr, "UICC both matching", "UICC both discrepant"),
  ""
)
table(d_tnm$uneq_gr)

# Save discrepant group-level UICC stages
d_tnm_discrepant <- d_tnm[d_tnm$uneq_gr == "UICC both discrepant", ]

# Check ICD10 values
table(substr(d_tnm$ICD10, 1, 1) == 'C')

# ============================================================
# Derive source category of MUST_UICC: uicc.tnm.from0
# ============================================================

d_tnm$no_loc <- ifelse(d_tnm$ICDO_loc %in% unique(d_map$LOC), NA, "localisation not defined")
table(d_tnm$no_loc, useNA="always")

d_tnm$uicc.tnm.from0 <- with(d_tnm, ifelse(
  substr(ICD10, 1, 1) == 'C' | substr(ICD10, 1, 2) == 'D0',
  ifelse(is.na(MUST_UICC),
         ifelse(is.na(no_loc),
                ifelse(TNM_miss == "missing", "TNM missing", "Combination not defined"),
                "Localisation not defined"),
         "only MUST_UICC"),
  "No carcinoma"
))

table(d_tnm$uicc.tnm.from0)
table(is.na(d_tnm$MUST_UICC))
table(d_tnm$MUST_UICC)
table(d_tnm$uicc.tnm.from0, d_tnm$MUST_UICC, useNA = "always")

table(d_tnm$no_loc)
table(d_tnm$TNM_miss)
table(d_tnm$MUST_UICC)

# Define the desired order of UICC categories only once
category_levels <- c(
  "only DOCUMENTED_UICC", "only MUST_UICC", "UICC both matching", "UICC both discrepant",
  "No carcinoma", "TNM missing", "Localisation not defined",
  "Combination not defined", "Version <6"
)

# Assign uicc.tnm.from based on version and documentation status
d_tnm$uicc.tnm.from <- with(d_tnm, ifelse(
  UICC_vers < 6, "Version <6",
  ifelse(!is.na(DOCUMENTED_UICC) & is.na(MUST_UICC),
         "only DOCUMENTED_UICC",
         ifelse(uneq_gr == "", uicc.tnm.from0, uneq_gr)
  )
))

# Enrich classification with mapping strategy information (optional)
d_tnm$uicc.tnm.from2 <- with(d_tnm, ifelse(
  mapping == "additional_min", paste0(uicc.tnm.from, "_additional_min"),
  ifelse(mapping == "additional_assured", paste0(uicc.tnm.from, "_additional_assured"),
         ifelse(mapping == "additional_grading", paste0(uicc.tnm.from, "_additional_grading"),
                uicc.tnm.from)
  )
))

# Create total frequency and percentage table
tab_MUST_UICC_from <- d_tnm %>%
  mutate(Category = factor(uicc.tnm.from, levels = category_levels)) %>%
  count(Category) %>%
  mutate(Freq_percent = round(100 * n / sum(n), 2)) %>%
  arrange(Category)  # uses factor levels for ordering

# Create stratified frequency and percentage table by ICDO_loc
tab_MUST_UICC_from_ent <- d_tnm %>%
  mutate(Category = factor(uicc.tnm.from, levels = category_levels)) %>%
  count(ICDO_loc, Category) %>%
  group_by(ICDO_loc) %>%
  mutate(Freq_percent = round(100 * n / sum(n), 2)) %>%
  ungroup() %>%
  arrange(ICDO_loc, Category)  # ordered by location, then by defined category order

write.xlsx(tab_MUST_UICC_from[, c("Category", "n", "Freq_percent")],
           "results/Result2.1_FreqTrackUICC.xlsx", rowNames = FALSE)

write.xlsx(tab_MUST_UICC_from_ent,
           "results/Result2.2_FreqTrackUICC.byEntity.xlsx", rowNames = FALSE)

# ============================================================
# Define comparison groups and display order
# ============================================================

group_labels <- list(
  group1 = c("DOCUMENTED_UICC available", "DOCUMENTED_UICC not available"),
  group2 = c("MUST_UICC available", "MUST_UICC not available"),
  group3 = c("RESULTING_UICC available", "RESULTING_UICC not available"),
  group4 = c("Comparison possible", "Comparison not possible"),
  group5 = c("Matching", "Discrepant")
)

group_order <- unlist(group_labels)

# ============================================================
# Generic helper function for group comparisons (no rlang)
# ============================================================

calculate_grouped_frequencies <- function(data, column, group, stratify_by = NULL, group_label = "Group A", other_label = "Group B") {
  data$Group <- ifelse(data[[column]] %in% group, group_label, other_label)
  
  if (!is.null(stratify_by)) {
    result <- data %>%
      count(Stratification = .data[[stratify_by]], Group, name = "Freq") %>%
      group_by(Stratification) %>%
      mutate(Percent = round(100 * Freq / sum(Freq), 2)) %>%
      ungroup()
  } else {
    result <- data %>%
      count(Group, name = "Freq") %>%
      mutate(Percent = round(100 * Freq / sum(Freq), 2))
  }
  
  result$Group <- factor(result$Group, levels = group_order)
  return(result)
}

# ============================================================
# Calculate grouped frequencies across all entities (total)
# ============================================================

# Define category groups for classification comparison
category_groups <- list(
  group1 = c("only DOCUMENTED_UICC", "UICC both matching", "UICC both discrepant"),
  group2 = c("only MUST_UICC", "UICC both matching", "UICC both discrepant"),
  group3 = c("only DOCUMENTED_UICC", "only MUST_UICC", "UICC both matching", "UICC both discrepant"),
  group4 = c("UICC both matching", "UICC both discrepant"),
  group5 = c("UICC both matching")
)

grouped_results <- bind_rows(
  calculate_grouped_frequencies(d_tnm, "uicc.tnm.from", category_groups$group1, group_label = group_labels$group1[1], other_label = group_labels$group1[2]),
  calculate_grouped_frequencies(d_tnm, "uicc.tnm.from", category_groups$group2, group_label = group_labels$group2[1], other_label = group_labels$group2[2]),
  calculate_grouped_frequencies(d_tnm, "uicc.tnm.from", category_groups$group3, group_label = group_labels$group3[1], other_label = group_labels$group3[2]),
  calculate_grouped_frequencies(d_tnm, "uicc.tnm.from", category_groups$group4, group_label = group_labels$group4[1], other_label = group_labels$group4[2]),
  calculate_grouped_frequencies(
    d_tnm[d_tnm$uicc.tnm.from %in% c("UICC both matching", "UICC both discrepant"), ],
    "uicc.tnm.from", category_groups$group5, group_label = group_labels$group5[1], other_label = group_labels$group5[2]
  )
) %>%
  arrange(Group)

write.xlsx(grouped_results, "results/Result2.3_FreqTrackUICC.comb.xlsx", rowNames = FALSE)

# ============================================================
# Calculate grouped frequencies by ICDO entity (stratified)
# ============================================================

grouped_results_e <- bind_rows(
  calculate_grouped_frequencies(d_tnm, "uicc.tnm.from", category_groups$group1, stratify_by = "ICDO_loc", group_labels$group1[1], group_labels$group1[2]),
  calculate_grouped_frequencies(d_tnm, "uicc.tnm.from", category_groups$group2, stratify_by = "ICDO_loc", group_labels$group2[1], group_labels$group2[2]),
  calculate_grouped_frequencies(d_tnm, "uicc.tnm.from", category_groups$group3, stratify_by = "ICDO_loc", group_labels$group3[1], group_labels$group3[2]),
  calculate_grouped_frequencies(d_tnm, "uicc.tnm.from", category_groups$group4, stratify_by = "ICDO_loc", group_labels$group4[1], group_labels$group4[2]),
  calculate_grouped_frequencies(
    d_tnm[d_tnm$uicc.tnm.from %in% c("UICC both matching", "UICC both discrepant"), ],
    "uicc.tnm.from", category_groups$group5, stratify_by = "ICDO_loc", group_labels$group5[1], group_labels$group5[2]
  )
) %>%
  mutate(Group = factor(Group, levels = group_order)) %>%
  arrange(Stratification, Group)

write.xlsx(grouped_results_e, "results/Result2.4_FreqTrackUICC.comb.byEntity.xlsx", rowNames = FALSE)

# ========================================================================
# Calculate Level of Certainty and Frequency of Level of Certainty
# ========================================================================

#Level of Certainty
d_tnm$num_level_of_certainty <- 0  # Initialize with base value

d_tnm$num_level_of_certainty <- ifelse(grepl('0', d_tnm$TNMcpy0) & grepl("0/metastasis_yes", d_tnm$TNMcpy0) == FALSE, d_tnm$num_level_of_certainty + 1, d_tnm$num_level_of_certainty)   # If metastasis, do not lower level

d_tnm$num_level_of_certainty <- ifelse(d_tnm$mapping == "additional_min", d_tnm$num_level_of_certainty + 1, d_tnm$num_level_of_certainty)
d_tnm$num_level_of_certainty <- ifelse(d_tnm$mapping == "additional_grading", d_tnm$num_level_of_certainty + 1, d_tnm$num_level_of_certainty)
d_tnm$num_level_of_certainty <- ifelse(d_tnm$source_loc == "additional", d_tnm$num_level_of_certainty + 1, d_tnm$num_level_of_certainty)

d_tnm$num_level_of_certainty <- ifelse(d_tnm$num_level_of_certainty == 0, "Assured", ifelse(d_tnm$num_level_of_certainty == 1, "Moderate", "Limited"))
##d_tnm$num_level_of_certainty <- ifelse(grepl('y_', d_tnm$TNMcpy0) & !grepl('m_yes', d_tnm$TNMcpy0), paste0("y_", d_tnm$num_level_of_certainty), d_tnm$num_level_of_certainty)

table(d_tnm$num_level_of_certainty)

#Frequency of Level of Certainty
#result_freq_level <- calc_freq_percent(column = d_tnm$num_level_of_certainty, included_categories = c("Assured", "Moderate", "Limited", "y_Assured", "y_Moderate", "y_Limited"))
result_freq_level <- calc_freq_percent(column = d_tnm$num_level_of_certainty, included_categories = c("Assured", "Moderate", "Limited"))
result_freq_level

##result_freq_level_e <- calc_freq_percent(column = d_tnm$num_level_of_certainty, stratify_by = d_tnm$ICDO_loc, included_categories = c("Assured", "Moderate", "Limited", "y_Assured", "y_Moderate", "y_Limited"))
result_freq_level_e <- calc_freq_percent(column = d_tnm$num_level_of_certainty, stratify_by = d_tnm$ICDO_loc, included_categories = c("Assured", "Moderate", "Limited"))
result_freq_level_e

write.xlsx(result_freq_level[, c(1, 2, 5)], "results/Result9.1_FreqLevelCertainty.xlsx", rowNames = FALSE)
write.xlsx(result_freq_level_e[,-4], "results/Result9.2_FreqLevelCertaintyByEntity.xlsx", rowNames = FALSE)

# ========================================================================
# Calculate Additional Location Rules Frequency
# ========================================================================

result_freq_locadd <- calc_freq_percent(column = d_tnm$source_loc, included_categories = c("original", "additional"))
result_freq_locadd

result_freq_locadd_e <- calc_freq_percent(column = d_tnm$source_loc, stratify_by = d_tnm$ICDO_loc, included_categories = c("original", "additional"))
result_freq_locadd_e

write.xlsx(result_freq_locadd[, c(1, 2, 4)], "results/Result5.3_ListLocRWD.xlsx", rowNames = FALSE)
write.xlsx(result_freq_locadd_e, "results/Result5.4_ListLocRWD_ByEntity.xlsx", rowNames = FALSE)

# ========================================================================
# Prepare Final UICC Result Table
# ========================================================================

d_tnm$uicc_result <- ifelse(!is.na(d_tnm$DOCUMENTED_UICC), d_tnm$DOCUMENTED_UICC, d_tnm$MUST_UICC)

table(d_tnm$uicc_result)

result_uicc <- data.frame(
  ID = d_tnm$ID,
  ICDO_LOC = d_tnm$ICDO_loc,
  LOC_SOURCE = d_tnm$source_loc,
  MAPPING = d_tnm$mapping,
  PATHO_CLIN = d_tnm$patho_clin,
  VERSION = d_tnm$Vers,
  T = d_tnm$T,
  N = d_tnm$N,
  M = d_tnm$M,
  GRADING = d_tnm$grading,
  TNMcpy0 = d_tnm$TNMcpy0,
  TRACKING = d_tnm$uicc.tnm.from,
  DOCUMENTED_UICC = d_tnm$DOCUMENTED_UICC,
  MUST_UICC = d_tnm$MUST_UICC,
  LEVEL_CERTAINTY = d_tnm$num_level_of_certainty,
  RESULTING_UICC = d_tnm$uicc_result
)

result_uicc <- result_uicc[order(result_uicc$ICDO_LOC),]

table(result_uicc$RESULTING_UICC, useNA = "always")

write.xlsx(result_uicc, "results/Result1_ListMainResult.xlsx", rowNames = FALSE)


# =======================================================================================
# Compare UICC Classification (DOCUMENTED_UICC) vs. UICC Derived from TNM (MUST_UICC)
# =======================================================================================

# Create frequency table of DOCUMENTED_UICC vs. MUST_UICC, remove zero and NA combinations
compare <- as.data.frame(table(d_tnm$DOCUMENTED_UICC, d_tnm$MUST_UICC, useNA = "always"))
colnames(compare) <- c("DOCUMENTED_UICC", "MUST_UICC", "Freq")
compare <- compare[compare$Freq != 0 & !is.na(compare$DOCUMENTED_UICC), ]
compare$DOCUMENTED_UICC <- as.character(compare$DOCUMENTED_UICC)
compare$MUST_UICC <- as.character(compare$MUST_UICC)

# Subset to valid MUST_UICC stages only
compare_min <- compare[!compare$MUST_UICC %in% c('localisation not defined', 'combination not defined', 'allmiss', 'no carcinoma'), ]
colnames(compare_min) <- c("UICC_from_classif", "UICC_from_TNM", "Freq")

# Define match status
compare$status <- ifelse(compare$DOCUMENTED_UICC == compare$MUST_UICC, "matching", "discrepant")
table(compare$status)

# Extract and sort discrepancies by location
uneq <- d_tnm_discrepant[, c("ID", "loc", "DOCUMENTED_UICC", "MUST_UICC", "mapping", "patho_clin", "Vers", "T", "N", "M", "grading", "TNMcpy0")]
uneq <- uneq[order(uneq$loc), ]
write.xlsx(uneq, "results/Result3_ListDiscrepantUICC.xlsx", rowNames = FALSE)

# =======================================================================================
# Additional Rules and Unknown Location Evaluation (RWD_UICC)
# =======================================================================================

# Count ICDO codes where location is unknown and DOCUMENTED_UICC is missing
result_unkn_loc <- calc_freq_percent(column = d_tnm$ICDO, filter_condition = !is.na(d_tnm$no_loc) & is.na(d_tnm$DOCUMENTED_UICC))
result_unkn_loc

# Frequency table of original vs. additional mapping rules
result_freq_add <- calc_freq_percent(column = d_tnm$mapping)
write.xlsx(result_freq_add[,c(1,2,4)], "results/Result6.1_FreqRWD.UICC.xlsx", rowNames = FALSE)




d_tnm$mapping
# Count and group entries directly from existing columns (ensuring mapping is character)
result_additional <- d_tnm %>%
  filter(startsWith(mapping, "add")) %>%  # ensure character vector
  count(
    mapping,
    patho_clin,
    loc,
    Vers,
    T,
    N,
    M,
    grading,
    MUST_UICC,
    name = "Freq"
  ) %>%
  arrange(desc(Freq))  # sort by frequency

# Export the result
write.xlsx(result_additional, "results/Result6.2_ListRWD.UICC.xlsx", rowNames = FALSE)

# Count and group entries directly from existing columns (no MUST_UICC_key needed)
result_additional <- d_tnm %>%
  filter(startsWith(mapping, "add")) %>%  # optional: filter only additional mapping strategies
  count(
    mapping,
    patho_clin,
    loc,
    Vers ,
    T ,
    N ,
    M ,
    grading ,
    MUST_UICC,
    name = "Freq"
  ) %>%
  arrange(desc(Freq))  # sort by descending frequency

# Export the grouped results to Excel
write.xlsx(result_additional, "results/Result6.2_ListRWD.UICC.xlsx", rowNames = FALSE)
# Count and group entries directly from existing columns (no MUST_UICC_key needed)
result_additional <- d_tnm %>%
  filter(startsWith(mapping, "add")) %>%  # optional: filter only additional mapping strategies
  count(
    mapping,
    patho_clin = TNM_T_cp_map,
    loc = ICDO_loc,
    Vers = UICC_vers,
    T = TNM_T,
    N = TNM_N,
    M = TNM_M,
    grading = Grad,
    MUST_UICC,
    name = "Freq"
  ) %>%
  arrange(desc(Freq))  # sort by descending frequency

# Export the grouped results to Excel
write.xlsx(result_additional, "results/Result6.2_ListRWD.UICC.xlsx", rowNames = FALSE)

# ========================================================================
# Identify and Export Cases with Undefined TNM Combinations
# ========================================================================

table(d_tnm$uicc.tnm.from0[substr(d_tnm$uicc.tnm.from0, 1, 7)  %in% "Combina"])
table(d_tnm$uicc.tnm.from0)
d_tnm_TNM_missing <- d_tnm[substr(d_tnm$uicc.tnm.from0, 1, 7) %in% 'Combina' ,]  # Filter undefined TNM combinations
names(d_tnm_TNM_missing)

# Generate frequency table for undefined TNM combinations
freq_table <- d_tnm_TNM_missing %>%
  count(ICDO_loc, UICC_vers,TNM_T, TNM_N, TNM_M, TNM_T_1, TNM_N_1, TNM_M_1, Grad) %>%
  arrange(desc(n))  # optional: sort by frequency

write.xlsx(freq_table,
           "results/Result7_FreqCombiNotDefined.xlsx",
           quote = FALSE,
           rowNames = FALSE,
           colNames = TRUE)

# ========================================================================
# Check Mapping Consistency and ICDO Location Assignment
# ========================================================================

table(d_tnm$no_loc, useNA="always")

# Identify unknown ICDO locations
table(d_tnm$ICDO_loc[!d_tnm$ICDO_loc %in% d_map$LOC])   # Unknown ICDO_loc
table(d_tnm$ICDO_loc[d_tnm$ICDO_loc %in% d_map$LOC])    # Known ICDO_loc

# ========================================================================
# Calculate and Export Frequency of UICC Stages by Entity
# ========================================================================

uicc_loc1 <- calc_freq_percent(column = d_tnm$MUST_UICC)
uicc_loc <- calc_freq_percent(column = d_tnm$MUST_UICC, stratify_by = d_tnm$ICDO_loc)
colnames(uicc_loc) <- c("UICC_stage", "Tumor_entity", "Freq", "Percent.Freq")

write.xlsx(uicc_loc[uicc_loc$Freq!=0,], "results/Result4_FreqUICC.byEntity.xlsx", quote = FALSE, rowNames = FALSE, colNames = TRUE)

