# ============================================================
# UICC MUST Project – Add RWD_UICC rules to the main mapping table
# ============================================================

# Load required packages
library(openxlsx)
library(dplyr)

getwd()

# ============================================================
# Load and sort original mapping table
# ============================================================
d_map <- read.delim("copyright_catalogs/mapping_uicc_original.csv", header = TRUE, sep = ";", as.is = TRUE)
dim(d_map)
head(d_map)
table(d_map$mapping)

d_map <- d_map %>%
  arrange(
    LOC,
    clin_patho_orig,
    UICC_vers,
    desc(mapping),
    UICC_stage,
    TNM_T,
    TNM_N,
    TNM_M
  )

# ============================================================
# Construct combination keys for later rule-based extension
# ============================================================
d_map$combiT     <- paste(d_map$LOC, d_map$clin_patho, d_map$UICC_vers, d_map$TNM_N, d_map$TNM_M, d_map$Grad, d_map$UICC_stage, sep = "_")
d_map$combiN     <- paste(d_map$LOC, d_map$clin_patho, d_map$UICC_vers, d_map$TNM_T, d_map$TNM_M, d_map$Grad, d_map$UICC_stage, sep = "_")
d_map$combiM     <- paste(d_map$LOC, d_map$clin_patho, d_map$UICC_vers, d_map$TNM_T, d_map$TNM_N, d_map$Grad, d_map$UICC_stage, sep = "_")
d_map$combiGrad  <- paste(d_map$LOC, d_map$clin_patho, d_map$UICC_vers, d_map$TNM_T, d_map$TNM_N, d_map$TNM_M, d_map$UICC_stage, sep = "_")

# Without UICC stage
d_map$combiTU    <- paste(d_map$LOC, d_map$clin_patho, d_map$UICC_vers, d_map$TNM_N, d_map$TNM_M, d_map$Grad, sep = "_")
d_map$combiNU    <- paste(d_map$LOC, d_map$clin_patho, d_map$UICC_vers, d_map$TNM_T, d_map$TNM_M, d_map$Grad, sep = "_")
d_map$combiMU    <- paste(d_map$LOC, d_map$clin_patho, d_map$UICC_vers, d_map$TNM_T, d_map$TNM_N, d_map$Grad, sep = "_")
d_map$combiGradU <- paste(d_map$LOC, d_map$clin_patho, d_map$UICC_vers, d_map$TNM_T, d_map$TNM_N, d_map$TNM_M, sep = "_")

table(d_map$Grad)

# Prepare base for automation

d_map_automated_add <- d_map

d_map_automated_add$Freq.x <- 99
d_map_automated_add$Freq.y <- 99


d_map_automated_add <- d_map_automated_add %>%
  arrange(
    LOC,
    clin_patho,
    UICC_vers,
    desc(mapping),   # funktioniert hier!
    TNM_M,
    TNM_N,
    TNM_T
  )

colnames(d_map_automated_add)

# ============================================================
# Automatically add rules for missing grading information
# ============================================================

# Filter out specific entities where GX is not allowed
# Set grading to GX for eligible entries in order to fill additional UICC for them

d_map_addg <- d_map_automated_add[
  d_map_automated_add$Grad != "" &
    d_map_automated_add$LOC != "GestationalTrophoblasticNeoplasms" &
    d_map_automated_add$LOC != "Testis",
]
d_map_addg$Grad <- "GX"

dim(d_map_addg)
head(d_map_addg)
table(d_map_addg$Grad)

# Keep lowest UICC stage per group (use combiGradU)
d_map_addg <- d_map_addg[order(d_map_addg$combiGradU, d_map_addg$UICC_stage), ]
d_map_addg <- d_map_addg %>% group_by(combiGradU) %>% filter(row_number(UICC_stage) == 1)

# Annotate as "additional_Grading"
d_map_addg$mapping <- "additional_Grading"

# Append new rows to the automated table
d_map_automated_add <- rbind(d_map_automated_add, d_map_addg)

tail(d_map_automated_add)







# ============================================================
# UICC MUST Project – Additional Logic for N/M/T Stage Collapsing
# ============================================================

collapse_stage_group <- function(df, stage_col, combi_key, combi_key_full, stage_prefix, label) {
  stages <- paste0(stage_prefix, letters[1:4])
  base_stage <- paste0(stage_prefix, "a")
  
  # Step 1: Select base mappings (e.g. N1a)
  d_base <- df[df[[stage_col]] == base_stage, !(names(df) %in% c("Freq.x", "Freq.y"))]
  d_base[[stage_col]] <- as.numeric(stage_prefix)
  
  # Step 2: Select all relevant mappings (e.g. N1a–N1d)
  d_full <- df[df[[stage_col]] %in% stages, ]
  
  # Step 3: Frequency tables for identifying uniqueness
  freq_all <- as.data.frame(table(d_full[[combi_key]]))
  d_full_u <- unique(d_full[, c(combi_key_full, combi_key)])
  freq_unique <- as.data.frame(table(d_full_u[[combi_key]]))
  
  # Step 4: Merge back frequencies
  d_result <- merge(d_base, freq_unique, by.x = combi_key, by.y = "Var1", all.x = TRUE)
  d_result <- merge(d_result, freq_all, by.x = combi_key, by.y = "Var1", all.x = TRUE)
  
  # Step 5: Assign mapping label
  d_result$mapping <- ifelse(d_result$Freq.x == 1 & d_result$Freq.y > 1,
                             paste0("additional_assured_", label),
                             paste0("additional_min_", label))
  
  # Step 6: Sort descending
  d_result <- d_result[order(desc(d_result$TNM_M), desc(d_result$TNM_N), desc(d_result$TNM_T)), ]
  
  return(d_result)
}

# Apply logic for T1–T4
for (t_stage in 1:4) {
  result_t <- collapse_stage_group(d_map_automated_add, "TNM_T", "combiTU", "combiT", as.character(t_stage), "T")
  d_map_automated_add <- rbind(d_map_automated_add, result_t)
}

# Apply logic for N1–N3
for (n_stage in 1:3) {
  result_n <- collapse_stage_group(d_map_automated_add, "TNM_N", "combiNU", "combiN", as.character(n_stage), "N")
  d_map_automated_add <- rbind(d_map_automated_add, result_n)
}

# Apply logic for M1
result_m <- collapse_stage_group(d_map_automated_add, "TNM_M", "combiMU", "combiM", "1", "M")
d_map_automated_add <- rbind(d_map_automated_add, result_m)

# Final sorting of result table - additional rules must be below the original rules! 

# Step 1: Initial sort by page and UICC settings

d_map_automated_add_sort <- d_map_automated_add %>%
  arrange(
    LOC,
    clin_patho,
    UICC_vers,
    desc(mapping),
    UICC_stage,
    TNM_T,
    TNM_N,
    TNM_M
  )

#Display results table
table(d_map_automated_add_sort$mapping)

# Step 2: Relabel mappings to reduce granularity

d_map_automated_add_sort$mapping <- ifelse(substr(d_map_automated_add_sort$mapping, 1, 18) == "additional_assured", "additional_assured",
                                           ifelse(substr(d_map_automated_add_sort$mapping, 1, 14) == "additional_min", "additional_min",
                                                  ifelse(substr(d_map_automated_add_sort$mapping, 1, 18) == "additional_Grading", "additional_grading",
                                                         ifelse(substr(d_map_automated_add_sort$mapping, 1, 14) == "original", "original", NA))))

# Final export of results

write.table(d_map_automated_add_sort[, 1:11], "copyright_catalogs/mapping_uicc_additional.csv", col.names = TRUE, row.names = FALSE, sep = ";")

# Summary
dim(d_map_automated_add_sort)
dim(unique(d_map_automated_add_sort))

#Display results table
table(d_map_automated_add_sort$mapping)

d_map_automated_add_sort[1:20, 1:10]
