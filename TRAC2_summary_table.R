setwd("C:/Users/nnb4/OneDrive - University of North Carolina at Chapel Hill/Documents/IDEEL/3 TRAC2/codedata")

# ============================
# TRAC2 Summary Table 
# ============================

library(readxl)
library(readr)
library(dplyr)
library(openxlsx)
library(tidyr)
library(stringr)
library(ggplot2)


# == Load data == #
REDCAP <- read.csv("redactedREDCAPdata.csv")

UNREDACTED <- read.xlsx("TRAC2-042224KY_UNREDACTEDDATA.xlsx")

UNREDACTED_CONMED <- read.xlsx("TRAC2-ConmedsKY_DATA_2024-08-22_1559.xlsx")

trich <- read_csv("Compiled_TRAC&TRAC2_qPCR.csv")

histology <- read_excel(
  "TRAC 1 and 2 Sellers Pathology Consolidated findings 12Jun2024 FromBryce.xlsx",
  sheet = "combined",
  skip = 1
) #note that read_excel has skip argument, read xlsx does not 


#Chlamydia cytobrush (CT CB)
CT_CB_Batch_1_compiled_1 <- read_excel("CT_CB_Batch_1_compiled.xlsx", sheet = "Plate #1")
CT_CB_Batch_1_compiled_2 <- read_excel("CT_CB_Batch_1_compiled.xlsx", sheet = "Plate #2")
CT_CB_Batch_1_compiled_3 <- read_excel("CT_CB_Batch_1_compiled.xlsx", sheet = "Plate #3")

CT_CB_Batch_2_compiled_1 <- read_excel("CT_CB_Batch_2_compiled.xlsx", sheet = "Plate #1")
CT_CB_Batch_2_compiled_2 <- read_excel("CT_CB_Batch_2_compiled.xlsx", sheet = "Plate #2")
CT_CB_Batch_2_compiled_3 <- read_excel("CT_CB_Batch_2_compiled.xlsx", sheet = "Plate #3")

CT_CB_Batch_3_compiled_1 <- read_excel("CT_CB_Batch_3_compiled.xlsx", sheet = "Plate #1")
CT_CB_Batch_3_compiled_2 <- read_excel("CT_CB_Batch_3_compiled.xlsx", sheet = "Plate #2")

CT_CB_Batch_4_compiled_1 <- read_excel("CT_CB_Batch_4_compiled.xlsx", sheet = "Plate #1")

CT_CB_Batch_5_compiled_1 <- read_excel("CT_CB_Batch_5_compiled.xlsx", sheet = "Plate #1")
CT_CB_Batch_5_compiled_2 <- read_excel("CT_CB_Batch_5_compiled.xlsx", sheet = "Plate #2")

#Chlamydia vaginal swab (CT VS)
CT_VS_Batch_1_compiled_1 <- read_excel("CT_VS_Batch_1_compiled.xlsx", sheet = "Plate #1")
CT_VS_Batch_1_compiled_2 <- read_excel("CT_VS_Batch_1_compiled.xlsx", sheet = "Plate #2")
CT_VS_Batch_1_compiled_3 <- read_excel("CT_VS_Batch_1_compiled.xlsx", sheet = "Plate #3")

CT_VS_Batch_2_compiled_1 <- read_excel("CT_VS_Batch_2_compiled.xlsx", sheet = "Plate #1")
CT_VS_Batch_2_compiled_2 <- read_excel("CT_VS_Batch_2_compiled.xlsx", sheet = "Plate #2")
CT_VS_Batch_2_compiled_3 <- read_excel("CT_VS_Batch_2_compiled.xlsx", sheet = "Plate #3")

CT_VS_Batch_3_compiled_1 <- read_excel("CT_VS_Batch_3_compiled.xlsx", sheet = "Plate #1")

CT_VS_Batch_4_compiled_1 <- read_excel("CT_VS_Batch_4_compiled.xlsx", sheet = "Plate #1")

CT_VS_Batch_5_compiled_1 <- read_excel("CT_VS_Batch_5_compiled.xlsx", sheet = "Plate #1")
CT_VS_Batch_5_compiled_2 <- read_excel("CT_VS_Batch_5_compiled.xlsx", sheet = "Plate #2")

#Mycoplasma genitalium (MG CB)
MG_CB_Batch_1_Compiled_1 <- read_excel("MG_CB_Batch_1_Compiled.xlsx", sheet = "Plate #1")
MG_CB_Batch_1_Compiled_2 <- read_excel("MG_CB_Batch_1_Compiled.xlsx", sheet = "Plate #2")
MG_CB_Batch_1_Compiled_3 <- read_excel("MG_CB_Batch_1_Compiled.xlsx", sheet = "Plate #3")

MG_CB_Batch_2_Compiled_1 <- read_excel("MG_CB_Batch_2_Compiled.xlsx", sheet = "Plate #1")
MG_CB_Batch_2_Compiled_2 <- read_excel("MG_CB_Batch_2_Compiled.xlsx", sheet = "Plate #2")
MG_CB_Batch_2_Compiled_3 <- read_excel("MG_CB_Batch_2_Compiled.xlsx", sheet = "Plate #3")

MG_CB_Batch_3_Compiled_1 <- read_excel("MG_CB_Batch_3_Compiled.xlsx", sheet = "Plate #1")
MG_CB_Batch_3_Compiled_2 <- read_excel("MG_CB_Batch_3_Compiled.xlsx", sheet = "Plate #2")

MG_CB_Batch_4_Compiled_1 <- read_excel("MG_CB_Batch_4_Compiled.xlsx", sheet = "Plate#1")

#Mycoplasma genitalium (MG VS)
MG_VS_Batch_1_Compiled_1 <- read_excel("MG_VS_Batch_1_Compiled.xlsx", sheet = "Plate #1")
MG_VS_Batch_1_Compiled_2 <- read_excel("MG_VS_Batch_1_Compiled.xlsx", sheet = "Plate #2")
MG_VS_Batch_1_Compiled_3 <- read_excel("MG_VS_Batch_1_Compiled.xlsx", sheet = "Plate #3")

MG_VS_Batch_2_Compiled_1 <- read_excel("MG_VS_Batch_2_Compiled.xlsx", sheet = "Plate #1")
MG_VS_Batch_2_Compiled_2 <- read_excel("MG_VS_Batch_2_Compiled.xlsx", sheet = "Plate #2")
MG_VS_Batch_2_Compiled_3 <- read_excel("MG_VS_Batch_2_Compiled.xlsx", sheet = "Plate #3")

MG_VS_Batch_3_Compiled_1 <- read_excel("MG_VS_Batch_3_Compiled.xlsx", sheet = "Plate #1")
MG_VS_Batch_3_Compiled_2 <- read_excel("MG_VS_Batch_3_Compiled.xlsx", sheet = "Plate #2")

MG_VS_Batch_4_Compiled_1 <- read_excel("MG_VS_Batch_4_Compiled.xlsx", sheet = "Plate #1")


# == Create df consolidating all compiled CT and MG (CB + VS) == #

CT_CB_list <- list(
  CT_CB_Batch_1_compiled_1, CT_CB_Batch_1_compiled_2, CT_CB_Batch_1_compiled_3,
  CT_CB_Batch_2_compiled_1, CT_CB_Batch_2_compiled_2, CT_CB_Batch_2_compiled_3,
  CT_CB_Batch_3_compiled_1, CT_CB_Batch_3_compiled_2,
  CT_CB_Batch_4_compiled_1,
  CT_CB_Batch_5_compiled_1, CT_CB_Batch_5_compiled_2
)

CT_VS_list <- list(
  CT_VS_Batch_1_compiled_1, CT_VS_Batch_1_compiled_2, CT_VS_Batch_1_compiled_3,
  CT_VS_Batch_2_compiled_1, CT_VS_Batch_2_compiled_2, CT_VS_Batch_2_compiled_3,
  CT_VS_Batch_3_compiled_1,
  CT_VS_Batch_4_compiled_1,
  CT_VS_Batch_5_compiled_1, CT_VS_Batch_5_compiled_2
)

MG_CB_list <- list(
  MG_CB_Batch_1_Compiled_1, MG_CB_Batch_1_Compiled_2, MG_CB_Batch_1_Compiled_3,
  MG_CB_Batch_2_Compiled_1, MG_CB_Batch_2_Compiled_2, MG_CB_Batch_2_Compiled_3,
  MG_CB_Batch_3_Compiled_1, MG_CB_Batch_3_Compiled_2,
  MG_CB_Batch_4_Compiled_1
)

MG_VS_list <- list(
  MG_VS_Batch_1_Compiled_1, MG_VS_Batch_1_Compiled_2, MG_VS_Batch_1_Compiled_3,
  MG_VS_Batch_2_Compiled_1, MG_VS_Batch_2_Compiled_2, MG_VS_Batch_2_Compiled_3,
  MG_VS_Batch_3_Compiled_1, MG_VS_Batch_3_Compiled_2,
  MG_VS_Batch_4_Compiled_1
)


extract_burden <- function(df_list, burden_col) {
  bind_rows(lapply(df_list, function(df) {
    df %>%
      select(PTID, !!sym(burden_col)) %>%
      mutate(PTID = as.character(PTID)) %>%
      filter(!is.na(PTID), !is.na(.data[[burden_col]]))
  })) %>%
    group_by(PTID) %>%
    summarise(!!burden_col := first(.data[[burden_col]]), .groups = "drop")
}


CT_CB_all <- extract_burden(CT_CB_list, "CT_CB_burden")
CT_VS_all <- extract_burden(CT_VS_list, "CT_VS_burden")
MG_CB_all <- extract_burden(MG_CB_list, "MG_CB_burden")
MG_VS_all <- extract_burden(MG_VS_list, "MG_VS_burden")


CT_MG_compiled <- CT_CB_all %>%
  full_join(CT_VS_all, by = "PTID") %>%
  full_join(MG_CB_all, by = "PTID") %>%
  full_join(MG_VS_all, by = "PTID") %>%
  relocate(PTID)

#write_csv(CT_MG_compiled, "CT_MG_compiled.csv")  #exporting df for team QC

######################################
# Create Summary Table
#######################################

#Press ctrl  + alt + e to run all starting here 

#Turn character "NA" into numeric until end for functions
REDCAP[REDCAP == "NA"] <- NA


# Ensure PTID is character everywhere
REDCAP$record_id <- as.character(REDCAP$record_id)

unique_ids <- unique(REDCAP$record_id)
summ <- data.frame(PTID = unique_ids, stringsAsFactors = FALSE)

###############################################
# UNIFIED EXTRACTOR BLOCK — USE FOR ALL MODULES
###############################################

# Safe single‑event extractor
extract_event_var <- function(df, event, source_col, outname, valid_ids) {
  temp <- df[df$redcap_event_name == event, ]
  
  # keep only real participants
  temp <- temp[temp$record_id %in% valid_ids, ]
  
  # if column doesn't exist, create NA
  if (!source_col %in% names(temp)) {
    temp[[source_col]] <- NA
  }
  
  temp <- temp[!duplicated(temp$record_id), ]
  temp[[outname]] <- temp[[source_col]]
  
  temp[, c("record_id", outname)]
}

# Safe follow‑up extractor
extract_followup_var <- function(df, source_col, prefix, valid_ids) {
  
  events <- c(
    "1m"  = "1_month_followup_arm_1",
    "4m"  = "4_month_followup_arm_1",
    "8m"  = "8_month_followup_arm_1",
    "12m" = "12_month_followup_arm_1"
  )
  
  out_list <- list()
  
  for (tp in names(events)) {
    event <- events[[tp]]
    outname <- paste0(prefix, "_", tp)
    out_list[[tp]] <- extract_event_var(df, event, source_col, outname, valid_ids)
  }
  
  out_list
}

# Safe multi‑select extractor (checkbox + free text)
extract_multiselect <- function(df, event, prefix, labels, free_text_col = NULL, valid_ids) {
  
  temp <- df[df$redcap_event_name == event, ]
  temp <- temp[temp$record_id %in% valid_ids, ]
  temp <- temp[!duplicated(temp$record_id), ]
  
  # ensure all checkbox columns exist
  for (col in names(labels)) {
    if (!col %in% names(temp)) temp[[col]] <- NA
  }
  
  # ensure free text exists
  if (!is.null(free_text_col) && !free_text_col %in% names(temp)) {
    temp[[free_text_col]] <- NA
  }
  
  out_list <- vector("list", nrow(temp))
  
  for (i in seq_len(nrow(temp))) {
    selections <- c()
    
    # checkbox selections
    for (col in names(labels)) {
      if (isTRUE(temp[[col]][i] == 1)) {
        selections <- c(selections, labels[[col]])
      }
    }
    
    # free text
    if (!is.null(free_text_col)) {
      ft <- temp[[free_text_col]][i]
      if (!is.na(ft) && ft != "") selections <- c(selections, ft)
    }
    
    out_list[[i]] <- if (length(selections) == 0) NA_character_ else paste(selections, collapse = "; ")
  }
  
  out_df <- data.frame(
    record_id = temp$record_id,
    out = unlist(out_list),
    stringsAsFactors = FALSE
  )
  
  names(out_df)[2] <- prefix
  out_df
}


# ============================
# Demographic helpers
# ============================

make_demo_column <- function(df, event_name, var_name, source_col, transform_fn = identity) {
  temp <- df[df$redcap_event_name == event_name, ]
  temp[[var_name]] <- transform_fn(temp[[source_col]])
  out <- temp[, c("record_id", var_name)]
  out <- out[!duplicated(out$record_id), ]
  out
}

make_gender_column <- function(df, event_name) {
  temp <- df[df$redcap_event_name == event_name, ]
  
  temp$gender <- temp$gender_id
  
  idx <- which(temp$gender_id == 8)
  if (length(idx) > 0 && "oth_genid" %in% names(temp)) {
    oth_vals <- temp$oth_genid[idx]
    temp$gender[idx] <- ifelse(!is.na(oth_vals) & oth_vals != "",
                               oth_vals,
                               temp$gender_id[idx])
  }
  
  out <- temp[, c("record_id", "gender")]
  out <- out[!duplicated(out$record_id), ]
  out
}

make_race_column <- function(df, event_name) {
  temp <- df[df$redcap_event_name == event_name, ]
  temp$race <- NA_character_
  
  if ("race___1" %in% names(temp)) temp$race[temp$race___1 == 1] <- "American Indian or Alaskan Native"
  if ("race___2" %in% names(temp)) temp$race[temp$race___2 == 1] <- "Asian"
  if ("race___3" %in% names(temp)) temp$race[temp$race___3 == 1] <- "Black or African American"
  if ("race___4" %in% names(temp)) temp$race[temp$race___4 == 1] <- "Native Hawaiian or other Pacific Islander"
  if ("race___5" %in% names(temp)) temp$race[temp$race___5 == 1] <- "White"
  
  if ("race___6" %in% names(temp)) {
    idx <- which(temp$race___6 == 1 & !is.na(temp$race___6))
    if (length(idx) > 0 && "race_other" %in% names(temp)) {
      oth_vals <- temp$race_other[idx]
      temp$race[idx] <- ifelse(!is.na(oth_vals) & oth_vals != "", oth_vals, "Other")
    }
  }
  
  out <- temp[, c("record_id", "race")]
  out <- out[!duplicated(out$record_id), ]
  out
}

make_insurance_column <- function(df, event_name) {
  temp <- df[df$redcap_event_name == event_name, ]
  temp$insurance <- NA_character_
  
  idx_regular <- which(!is.na(temp$med_insure) & temp$med_insure %in% 1:5)
  if (length(idx_regular) > 0) {
    temp$insurance[idx_regular] <- as.character(temp$med_insure[idx_regular])
  }
  
  idx_other <- which(!is.na(temp$med_insure) & temp$med_insure == 6)
  if (length(idx_other) > 0 && "other_insure" %in% names(temp)) {
    oth_vals <- temp$other_insure[idx_other]
    temp$insurance[idx_other] <- ifelse(!is.na(oth_vals) & oth_vals != "",
                                        oth_vals,
                                        "6")
  }
  
  out <- temp[, c("record_id", "insurance")]
  out <- out[!duplicated(out$record_id), ]
  out
}

# ============================
# Demographics module
# ============================

make_demographics <- function(summ, REDCAP) {
  
  # Age
  demo_age <- make_demo_column(REDCAP, "enrollment_arm_1", "age", "age", as.numeric)
  summ <- merge(summ, demo_age, by.x = "PTID", by.y = "record_id", all.x = TRUE)
  
  # Gender
  demo_gender <- make_gender_column(REDCAP, "enrollment_arm_1")
  summ <- merge(summ, demo_gender, by.x = "PTID", by.y = "record_id", all.x = TRUE)
  
  # Race
  demo_race <- make_race_column(REDCAP, "enrollment_arm_1")
  summ <- merge(summ, demo_race, by.x = "PTID", by.y = "record_id", all.x = TRUE)
  
  # Ethnicity
  demo_eth <- make_demo_column(REDCAP, "enrollment_arm_1", "hispeth", "hispanic", as.numeric)
  summ <- merge(summ, demo_eth, by.x = "PTID", by.y = "record_id", all.x = TRUE)
  
  # Education
  demo_edu <- make_demo_column(REDCAP, "enrollment_arm_1", "education", "edu_level", as.numeric)
  summ <- merge(summ, demo_edu, by.x = "PTID", by.y = "record_id", all.x = TRUE)
  
  # Marital status
  demo_marital <- make_demo_column(REDCAP, "enrollment_arm_1", "marital", "maritalstat", as.numeric)
  summ <- merge(summ, demo_marital, by.x = "PTID", by.y = "record_id", all.x = TRUE)
  
  # Insurance
  demo_ins <- make_insurance_column(REDCAP, "enrollment_arm_1")
  summ <- merge(summ, demo_ins, by.x = "PTID", by.y = "record_id", all.x = TRUE)
  
  # Sexual preference
  demo_sexpref <- make_demo_column(REDCAP, "enrollment_arm_1", "sex_pref", "sex_pref", as.numeric)
  summ <- merge(summ, demo_sexpref, by.x = "PTID", by.y = "record_id", all.x = TRUE)
  
  summ
}

# Apply demographics
summ <- make_demographics(summ, REDCAP)

########################
# PREGNANCY 
#######################

extract_event_var <- function(df, event, source_col, outname) {
  temp <- df[df$redcap_event_name == event, ]
  temp <- temp[!duplicated(temp$record_id), ]
  temp[[outname]] <- temp[[source_col]]
  temp[, c("record_id", outname)]
}

extract_followup_var <- function(df, source_col, prefix) {
  events <- c(
    "1m"  = "1_month_followup_arm_1",
    "4m"  = "4_month_followup_arm_1",
    "8m"  = "8_month_followup_arm_1",
    "12m" = "12_month_followup_arm_1"
  )
  
  out_list <- list()
  
  for (tp in names(events)) {
    event <- events[[tp]]
    outname <- paste0(prefix, "_", tp)
    out_list[[tp]] <- extract_event_var(df, event, source_col, outname)
  }
  
  out_list
}

make_pregnancy_module <- function(summ, REDCAP) {
  
  # Ensure PTID is character
  summ$PTID <- as.character(summ$PTID)
  
  # ============================
  # Enrollment variables
  # ============================
  
  enroll_vars <- list(
    everpreg     = "everpreg",
    livebirths   = "livebirths",
    miscarriage  = "miscarriage",
    abortions    = "abortions",
    numpregs     = "numpregs",
    tubal_preg   = "tubal_preg",
    blockedtubes = "blockedtubes",
    chronplvpain = "chronplvpain"
  )
  
  for (v in names(enroll_vars)) {
    src <- enroll_vars[[v]]
    df_en <- extract_event_var(REDCAP, "enrollment_arm_1", src, v)
    summ <- merge(summ, df_en, by.x = "PTID", by.y = "record_id", all.x = TRUE)
  }
  
  # ============================
  # Follow‑up variables
  # ============================
  
  followup_vars <- list(
    everpreg_v2      = "everpreg_v2",
    blockedtubes_v2  = "blockedtubes_v2",
    chronplvpain_v2  = "chronplvpain_v2"
  )
  
  for (prefix in names(followup_vars)) {
    src <- followup_vars[[prefix]]
    out_list <- extract_followup_var(REDCAP, src, prefix)
    
    for (tp in names(out_list)) {
      summ <- merge(
        summ,
        out_list[[tp]],
        by.x = "PTID",
        by.y = "record_id",
        all.x = TRUE
      )
    }
  }
  
  summ
}

summ <- make_pregnancy_module(summ, REDCAP)



################
# Pelvic Surgery 
###############

make_plvc_surgery_enrollment <- function(df) {
  
  surgery_labels <- c(
    "plvc_surgery___1" = "C-section",
    "plvc_surgery___2" = "Surgery for pelvic pain",
    "plvc_surgery___3" = "Surgery for ovarian cyst",
    "plvc_surgery___4" = "Appendectomy",
    "plvc_surgery___5" = "Gallbladder",
    "plvc_surgery___6" = "Bariatric surgery",
    "plvc_surgery___7" = "Other abdominal/pelvic surgery",
    "plvc_surgery___8" = "None"
  )
  
  temp <- df[df$redcap_event_name == "enrollment_arm_1", ]
  
  out <- temp %>%
    rowwise() %>%
    mutate(plvc_surgery = {
      selections <- character(0)
      
      for (col in names(surgery_labels)) {
        val <- get(col)
        if (!is.na(val) && val == 1) {
          
          if (col == "plvc_surgery___7") {
            # Handle free text for "Other"
            if (!is.na(oth_surgery) && oth_surgery != "") {
              selections <- c(selections, oth_surgery)
            } else {
              selections <- c(selections, surgery_labels[[col]])
            }
          } else {
            selections <- c(selections, surgery_labels[[col]])
          }
        }
      }
      
      if (length(selections) == 0) NA_character_ else paste(selections, collapse = "; ")
    }) %>%
    ungroup() %>%
    select(record_id, plvc_surgery) %>%
    distinct(record_id, .keep_all = TRUE)
  
  out
}

make_plvc_surgery_followup <- function(df, event, outname) {
  
  temp <- df[df$redcap_event_name == event, ]
  
  out_list <- vector("list", nrow(temp))
  
  for (i in seq_len(nrow(temp))) {
    selections <- c()
    
    if (isTRUE(temp$plvc_surgery_v2___1[i] == 1)) selections <- c(selections, "C-section")
    if (isTRUE(temp$plvc_surgery_v2___2[i] == 1)) selections <- c(selections, "Surgery for pelvic pain")
    if (isTRUE(temp$plvc_surgery_v2___3[i] == 1)) selections <- c(selections, "Surgery for ovarian cyst")
    if (isTRUE(temp$plvc_surgery_v2___4[i] == 1)) selections <- c(selections, "Appendectomy")
    if (isTRUE(temp$plvc_surgery_v2___5[i] == 1)) selections <- c(selections, "Gallbladder")
    if (isTRUE(temp$plvc_surgery_v2___6[i] == 1)) selections <- c(selections, "Bariatric surgery")
    
    # "Other" with free text
    if (isTRUE(temp$plvc_surgery_v2___7[i] == 1)) {
      selections <- c(selections, temp$oth_surgery_v2[i])
    }
    
    # "None"
    if (isTRUE(temp$plvc_surgery_v2___8[i] == 1)) selections <- c(selections, "None")
    
    out_list[[i]] <- if (length(selections) == 0) NA_character_ else paste(selections, collapse = "; ")
  }
  
  out_df <- data.frame(
    record_id = temp$record_id,
    out = unlist(out_list),
    stringsAsFactors = FALSE
  )
  
  names(out_df)[2] <- outname
  out_df <- out_df[!duplicated(out_df$record_id), ]
  
  out_df
}

make_pelvic_surgery_module <- function(summ, REDCAP) {
  
  # Enrollment
  enroll_df <- make_plvc_surgery_enrollment(REDCAP)
  summ <- merge(summ, enroll_df, by.x = "PTID", by.y = "record_id", all.x = TRUE)
  
  # Follow‑ups
  events <- c(
    "1m"  = "1_month_followup_arm_1",
    "4m"  = "4_month_followup_arm_1",
    "8m"  = "8_month_followup_arm_1",
    "12m" = "12_month_followup_arm_1"
  )
  
  for (tp in names(events)) {
    event <- events[[tp]]
    outname <- paste0("plvc_surgery_v2_", tp)
    
    df_fu <- make_plvc_surgery_followup(REDCAP, event, outname)
    
    summ <- merge(
      summ,
      df_fu,
      by.x = "PTID",
      by.y = "record_id",
      all.x = TRUE
    )
  }
  
  summ
}

summ <- make_pelvic_surgery_module(summ, REDCAP)


################
# Substance use 
###############

# ============================
# Helpers for substance use
# ============================

# single event
extract_event_var <- function(df, event, source_col, outname, valid_ids) {
  temp <- df[df$redcap_event_name == event, ]
  temp <- temp[temp$record_id %in% valid_ids, ]
  temp <- temp[!duplicated(temp$record_id), ]
  
  if (!source_col %in% names(temp)) {
    temp[[source_col]] <- NA
  }
  
  temp[[outname]] <- temp[[source_col]]
  temp[, c("record_id", outname)]
}

# follow‑up single variable
extract_followup_var <- function(df, source_col, prefix, valid_ids) {
  
  events <- c(
    "1m"  = "1_month_followup_arm_1",
    "4m"  = "4_month_followup_arm_1",
    "8m"  = "8_month_followup_arm_1",
    "12m" = "12_month_followup_arm_1"
  )
  
  out_list <- vector("list", length(events))
  names(out_list) <- names(events)
  
  for (tp in names(events)) {
    event  <- events[[tp]]
    out_nm <- paste0(prefix, "_", tp)
    out_list[[tp]] <- extract_event_var(df, event, source_col, out_nm, valid_ids)
  }
  
  out_list
}

# generic multi‑select + optional free text
extract_multiselect <- function(df, event, prefix, labels, free_text_col = NULL, valid_ids) {
  
  temp <- df[df$redcap_event_name == event, ]
  temp <- temp[temp$record_id %in% valid_ids, ]
  temp <- temp[!duplicated(temp$record_id), ]
  
  # ensure checkbox cols exist
  for (col in names(labels)) {
    if (!col %in% names(temp)) temp[[col]] <- NA
  }
  
  # ensure free text exists
  if (!is.null(free_text_col) && !free_text_col %in% names(temp)) {
    temp[[free_text_col]] <- NA
  }
  
  out_list <- vector("list", nrow(temp))
  
  for (i in seq_len(nrow(temp))) {
    selections <- character(0)
    
    # checkboxes
    for (col in names(labels)) {
      if (isTRUE(temp[[col]][i] == 1)) {
        selections <- c(selections, labels[[col]])
      }
    }
    
    # free text
    if (!is.null(free_text_col)) {
      ft <- temp[[free_text_col]][i]
      if (!is.na(ft) && ft != "") selections <- c(selections, ft)
    }
    
    out_list[[i]] <-
      if (length(selections) == 0) NA_character_ else paste(selections, collapse = "; ")
  }
  
  out_df <- data.frame(
    record_id = temp$record_id,
    out       = unlist(out_list),
    stringsAsFactors = FALSE
  )
  
  names(out_df)[2] <- prefix
  out_df
}

# labels
recdrugs_labels_baseline <- c(
  "recdrugs_used___1" = "Marijuana (non-prescribed)",
  "recdrugs_used___2" = "Non-prescribed pills by mouth",
  "recdrugs_used___3" = "Non-prescribed intravenous/IV drugs",
  "recdrugs_used___4" = "Other non-prescribed drugs (inhaled/snorted)"
)

recdrugs_labels_v2 <- c(
  "recdrugs_used_v2___1" = "Marijuana (non-prescribed)",
  "recdrugs_used_v2___2" = "Non-prescribed pills by mouth",
  "recdrugs_used_v2___3" = "Non-prescribed intravenous/IV drugs",
  "recdrugs_used_v2___4" = "Other non-prescribed drugs (inhaled/snorted)"
)

# ============================
# MAIN MODULE
# ============================
make_substance_use_module <- function(summ, REDCAP) {
  
  summ$PTID <- as.character(summ$PTID)
  valid_ids <- summ$PTID
  
  # ----------------------------
  # Baseline single‑item vars
  # ----------------------------
  baseline_vars <- list(
    smoker        = "smoker",
    cigs_day      = "cigs_day",
    vape_juul     = "vape_juul",
    med_marij     = "med_marij",
    rec_drugsever = "rec_drugsever",
    druguse       = "druguse"
  )
  
  for (v in names(baseline_vars)) {
    src   <- baseline_vars[[v]]
    df_en <- extract_event_var(REDCAP, "enrollment_arm_1", src, v, valid_ids)
    summ  <- merge(summ, df_en, by.x = "PTID", by.y = "record_id", all.x = TRUE)
  }
  
  # conditional baseline logic
  summ$cigs_day <- ifelse(summ$smoker == 1, summ$cigs_day, NA)
  summ$druguse  <- ifelse(summ$rec_drugsever == 1, summ$druguse, NA)
  
  # ----------------------------
  # Follow‑up single‑item vars
  # ----------------------------
  followup_vars <- list(
    smoker_v2        = "smoker_v2",
    cigs_day_v2      = "cigs_day_v2",
    vape_juul_v2     = "vape_juul_v2",
    med_marij_v2     = "med_marij_v2",
    rec_drugsever_v2 = "rec_drugsever_v2",
    druguse_v2       = "druguse_v2"
  )
  
  for (prefix in names(followup_vars)) {
    src      <- followup_vars[[prefix]]
    out_list <- extract_followup_var(REDCAP, src, prefix, valid_ids)
    
    for (tp in names(out_list)) {
      summ <- merge(
        summ,
        out_list[[tp]],
        by.x = "PTID",
        by.y = "record_id",
        all.x = TRUE
      )
    }
  }
  
  # ----------------------------
  # Baseline multi‑select
  # ----------------------------
  rec_base <- extract_multiselect(
    df           = REDCAP,
    event        = "enrollment_arm_1",
    prefix       = "recdrugs_used",
    labels       = recdrugs_labels_baseline,
    free_text_col = "other_recdrug",
    valid_ids    = valid_ids
  )
  
  summ <- merge(summ, rec_base, by.x = "PTID", by.y = "record_id", all.x = TRUE)
  
  # ----------------------------
  # Follow‑up multi‑select + other_recdrug_v2
  # ----------------------------
  events <- c(
    "1m"  = "1_month_followup_arm_1",
    "4m"  = "4_month_followup_arm_1",
    "8m"  = "8_month_followup_arm_1",
    "12m" = "12_month_followup_arm_1"
  )
  
  for (tp in names(events)) {
    event   <- events[[tp]]
    outname <- paste0("recdrugs_used_v2_", tp)
    
    df_fu <- extract_multiselect(
      df            = REDCAP,
      event         = event,
      prefix        = outname,
      labels        = recdrugs_labels_v2,   # checkbox labels
      free_text_col = "other_recdrug_v2",   # append free text
      valid_ids     = valid_ids
    )
    
    summ <- merge(
      summ,
      df_fu,
      by.x = "PTID",
      by.y = "record_id",
      all.x = TRUE
    )
  }
  
  summ
}

# APPLY MODULE
summ <- make_substance_use_module(summ, REDCAP)


####################
# Alcohol use
###################

extract_event_var <- function(df, event, source_col, outname) {
  temp <- df[df$redcap_event_name == event, ]
  temp <- temp[!duplicated(temp$record_id), ]
  temp[[outname]] <- temp[[source_col]]
  temp[, c("record_id", outname)]
}


extract_followup_var <- function(df, source_col, prefix) {
  
  events <- c(
    "1m"  = "1_month_followup_arm_1",
    "4m"  = "4_month_followup_arm_1",
    "8m"  = "8_month_followup_arm_1",
    "12m" = "12_month_followup_arm_1"
  )
  
  out_list <- list()
  
  for (tp in names(events)) {
    event <- events[[tp]]
    outname <- paste0(prefix, "_", tp)
    out_list[[tp]] <- extract_event_var(df, event, source_col, outname)
  }
  
  out_list
}

make_alcohol_module <- function(summ, REDCAP) {
  
  summ$PTID <- as.character(summ$PTID)
  
  # ============================
  # Baseline variables
  # ============================
  
  # drink (0/1)
  df_drink <- extract_event_var(REDCAP, "enrollment_arm_1", "drink", "drink")
  summ <- merge(summ, df_drink, by.x = "PTID", by.y = "record_id", all.x = TRUE)
  
  # drinks per week (only if drink == 1)
  df_week <- extract_event_var(REDCAP, "enrollment_arm_1", "drinks_week", "drinks_week")
  summ <- merge(summ, df_week, by.x = "PTID", by.y = "record_id", all.x = TRUE)
  summ$drinks_week <- ifelse(summ$drink == 1, summ$drinks_week, NA)
  
  # binge drinking (5+ drinks/day)
  df_binge <- extract_event_var(REDCAP, "enrollment_arm_1", "drink5inaday", "drink5inaday")
  summ <- merge(summ, df_binge, by.x = "PTID", by.y = "record_id", all.x = TRUE)
  summ$drink5inaday <- ifelse(summ$drink == 1, summ$drink5inaday, NA)
  
  # ============================
  # Follow‑up variables
  # ============================
  
  # drink_v2
  drink_fu <- extract_followup_var(REDCAP, "drink_v2", "drink_v2")
  for (tp in names(drink_fu)) {
    summ <- merge(summ, drink_fu[[tp]], by.x = "PTID", by.y = "record_id", all.x = TRUE)
  }
  
  # drinks_week_v2
  week_fu <- extract_followup_var(REDCAP, "drinks_week_v2", "drinks_week_v2")
  for (tp in names(week_fu)) {
    summ <- merge(summ, week_fu[[tp]], by.x = "PTID", by.y = "record_id", all.x = TRUE)
  }
  
  # binge drinking follow‑ups
  binge_fu <- extract_followup_var(REDCAP, "drink5inaday_v2", "drink5inaday_v2")
  for (tp in names(binge_fu)) {
    summ <- merge(summ, binge_fu[[tp]], by.x = "PTID", by.y = "record_id", all.x = TRUE)
  }
  
  # ============================
  # Conditional logic for follow‑ups
  # ============================
  
  # drinks_week_v2 only if drink_v2 == 1
  for (tp in c("1m","4m","8m","12m")) {
    drink_col <- paste0("drink_v2_", tp)
    week_col  <- paste0("drinks_week_v2_", tp)
    if (week_col %in% names(summ)) {
      summ[[week_col]] <- ifelse(summ[[drink_col]] == 1, summ[[week_col]], NA)
    }
  }
  
  # drink5inaday_v2 only if drink_v2 == 1
  for (tp in c("1m","4m","8m","12m")) {
    drink_col <- paste0("drink_v2_", tp)
    binge_col <- paste0("drink5inaday_v2_", tp)
    if (binge_col %in% names(summ)) {
      summ[[binge_col]] <- ifelse(summ[[drink_col]] == 1, summ[[binge_col]], NA)
    }
  }
  
  summ
}

summ <- make_alcohol_module(summ, REDCAP)


#######################################
# Urinary/vaginal symptoms enrollment and f/u
###########################################

collapse_multiselect <- function(df, event, prefix, labels) {
  
  temp <- df[df$redcap_event_name == event, ]
  temp <- temp[!duplicated(temp$record_id), ]
  
  out_list <- vector("list", nrow(temp))
  
  none_col <- names(labels)[labels == "NONE"]
  
  for (i in seq_len(nrow(temp))) {
    selections <- c()
    
    for (col in names(labels)) {
      if (col %in% names(temp) && isTRUE(temp[[col]][i] == 1)) {
        selections <- c(selections, labels[[col]])
      }
    }
    
    # If NONE selected, override everything
    if (length(none_col) == 1 && none_col %in% names(temp) &&
        isTRUE(temp[[none_col]][i] == 1)) {
      selections <- "NONE"
    }
    
    out_list[[i]] <- if (length(selections) == 0) NA_character_ else paste(selections, collapse = "; ")
  }
  
  out_df <- data.frame(
    record_id = temp$record_id,
    out = unlist(out_list),
    stringsAsFactors = FALSE
  )
  
  names(out_df)[2] <- prefix
  out_df
}

urisym_labels <- c(
  "urisymptoms___1" = "pain or burning on urination",
  "urisymptoms___2" = "sudden urge to urinate",
  "urisymptoms___3" = "urinating more often than usual",
  "urisymptoms___4" = "abnormal vaginal discharge",
  "urisymptoms___5" = "unusual vaginal odor",
  "urisymptoms___6" = "vaginal itching/irritation",
  "urisymptoms___7" = "NONE"
)

urisym_v2_labels <- c(
  "urisymptoms_v2___1" = "pain or burning on urination",
  "urisymptoms_v2___2" = "sudden urge to urinate",
  "urisymptoms_v2___3" = "urinating more often than usual",
  "urisymptoms_v2___4" = "abnormal vaginal discharge",
  "urisymptoms_v2___5" = "unusual vaginal odor",
  "urisymptoms_v2___6" = "vaginal itching/irritation",
  "urisymptoms_v2___7" = "NONE"
)


make_urinary_vaginal_symptoms <- function(summ, REDCAP) {
  
  summ$PTID <- as.character(summ$PTID)
  
  # ============================
  # Enrollment
  # ============================
  
  df_en <- collapse_multiselect(
    df = REDCAP,
    event = "enrollment_arm_1",
    prefix = "urisymptoms",
    labels = urisym_labels
  )
  
  summ <- merge(summ, df_en, by.x = "PTID", by.y = "record_id", all.x = TRUE)
  
  # ============================
  # Follow‑ups
  # ============================
  
  events <- c(
    "1m"  = "1_month_followup_arm_1",
    "4m"  = "4_month_followup_arm_1",
    "8m"  = "8_month_followup_arm_1",
    "12m" = "12_month_followup_arm_1"
  )
  
  for (tp in names(events)) {
    event <- events[[tp]]
    outname <- paste0("urisymptoms_v2_", tp)
    
    df_fu <- collapse_multiselect(
      df = REDCAP,
      event = event,
      prefix = outname,
      labels = urisym_v2_labels
    )
    
    summ <- merge(
      summ,
      df_fu,
      by.x = "PTID",
      by.y = "record_id",
      all.x = TRUE
    )
  }
  
  summ
}

summ <- make_urinary_vaginal_symptoms(summ, REDCAP)


##################################################
# ORAL/VAGINAL ANTIBIOTICS IN PAST 30 DAYS
###############################################

extract_event_var <- function(df, event, source_col, outname) {
  temp <- df[df$redcap_event_name == event, ]
  temp <- temp[!duplicated(temp$record_id), ]
  temp[[outname]] <- temp[[source_col]]
  temp[, c("record_id", outname)]
}

extract_followup_var <- function(df, source_col, prefix) {
  
  events <- c(
    "1m"  = "1_month_followup_arm_1",
    "4m"  = "4_month_followup_arm_1",
    "8m"  = "8_month_followup_arm_1",
    "12m" = "12_month_followup_arm_1"
  )
  
  out_list <- list()
  
  for (tp in names(events)) {
    event <- events[[tp]]
    outname <- paste0(prefix, "_", tp)
    out_list[[tp]] <- extract_event_var(df, event, source_col, outname)
  }
  
  out_list
}

make_abx_ov30d_module <- function(summ, REDCAP) {
  
  summ$PTID <- as.character(summ$PTID)
  
  # ============================
  # Enrollment
  # ============================
  
  df_en <- extract_event_var(
    df = REDCAP,
    event = "enrollment_arm_1",
    source_col = "abxov30d",
    outname = "abxov30d"
  )
  
  summ <- merge(
    summ,
    df_en,
    by.x = "PTID",
    by.y = "record_id",
    all.x = TRUE
  )
  
  # ============================
  # Follow‑ups
  # ============================
  
  fu_list <- extract_followup_var(
    df = REDCAP,
    source_col = "abxov30d_v2",
    prefix = "abxov30d_v2"
  )
  
  for (tp in names(fu_list)) {
    summ <- merge(
      summ,
      fu_list[[tp]],
      by.x = "PTID",
      by.y = "record_id",
      all.x = TRUE
    )
  }
  
  summ
}

summ <- make_abx_ov30d_module(summ, REDCAP)


##################################################
# NSAID / PELVIC PAIN MEDICATION USE (PAST 30 DAYS)
###############################################

extract_event_var <- function(df, event, source_col, outname) {
  temp <- df[df$redcap_event_name == event, ]
  temp <- temp[!duplicated(temp$record_id), ]
  temp[[outname]] <- temp[[source_col]]
  temp[, c("record_id", outname)]
}

extract_followup_var <- function(df, source_col, prefix) {
  
  events <- c(
    "1m"  = "1_month_followup_arm_1",
    "4m"  = "4_month_followup_arm_1",
    "8m"  = "8_month_followup_arm_1",
    "12m" = "12_month_followup_arm_1"
  )
  
  out_list <- list()
  
  for (tp in names(events)) {
    event <- events[[tp]]
    outname <- paste0(prefix, "_", tp)
    out_list[[tp]] <- extract_event_var(df, event, source_col, outname)
  }
  
  out_list
}

make_nsaid_pelvicpain_module <- function(summ, REDCAP) {
  
  summ$PTID <- as.character(summ$PTID)
  
  # ============================
  # Enrollment
  # ============================
  
  df_en <- extract_event_var(
    df = REDCAP,
    event = "enrollment_arm_1",
    source_col = "nsaidplvp30d",
    outname = "nsaidplvp30d"
  )
  
  summ <- merge(
    summ,
    df_en,
    by.x = "PTID",
    by.y = "record_id",
    all.x = TRUE
  )
  
  # ============================
  # Follow‑ups
  # ============================
  
  fu_list <- extract_followup_var(
    df = REDCAP,
    source_col = "nsaidplvp30d_v2",
    prefix = "nsaidplvp30d_v2"
  )
  
  for (tp in names(fu_list)) {
    summ <- merge(
      summ,
      fu_list[[tp]],
      by.x = "PTID",
      by.y = "record_id",
      all.x = TRUE
    )
  }
  
  summ
}

summ <- make_nsaid_pelvicpain_module(summ, REDCAP)


########################
# VISIT LOGIC
#####################


visit_cols <- c(
  "visits_present_En",
  "visits_present_1m",
  "visits_present_4m",
  "visits_present_8m",
  "visits_present_12m"
)

# Remove old versions, including .x, .y, .1
summ <- summ[, !grepl("^visits_present", names(summ))]
  
make_enrollment_visit <- function(df) {
  temp <- df[df$redcap_event_name == "enrollment_arm_1", ]
  temp$visits_present_En <- ifelse(is.na(temp$spec_time), 0, 1)
  out <- temp[, c("record_id", "visits_present_En")]
  out <- out[!duplicated(out$record_id), ]
  return(out)
}

make_followup_visit <- function(df, event_name, var_name) {
  temp <- df[df$redcap_event_name == event_name, ]
  temp[[var_name]] <- ifelse(is.na(temp$spec_time_v2), 0, 1)
  out <- temp[, c("record_id", var_name)]
  out <- out[!duplicated(out$record_id), ]
  return(out)
}

# Merge visits
summ <- merge(summ, make_enrollment_visit(REDCAP), by.x = "PTID", by.y = "record_id", all.x = TRUE)
summ <- merge(summ, make_followup_visit(REDCAP, "1_month_followup_arm_1", "visits_present_1m"), by.x = "PTID", by.y = "record_id", all.x = TRUE)
summ <- merge(summ, make_followup_visit(REDCAP, "4_month_followup_arm_1", "visits_present_4m"), by.x = "PTID", by.y = "record_id", all.x = TRUE)
summ <- merge(summ, make_followup_visit(REDCAP, "8_month_followup_arm_1", "visits_present8m"), by.x = "PTID", by.y = "record_id", all.x = TRUE)
summ <- merge(summ, make_followup_visit(REDCAP, "12_month_followup_arm_1", "visits_present12m"), by.x = "PTID", by.y = "record_id", all.x = TRUE)

# == Rectal chalm, ct_rectal columns == #
make_ct_column <- function(df, event_name, var_name) {
  temp <- df[df$redcap_event_name == event_name, ]
  #temp[[var_name]] <- ifelse(is.na(temp$ct_rectal), "NA", temp$ct_rectal) #old way 
  temp[[var_name]] <- temp$ct_rectal  # leave as NA if missing
  out <- temp[, c("record_id", var_name)]
  out <- out[!duplicated(out$record_id), ]
  return(out)
}

summ <- merge(summ, make_ct_column(REDCAP, "enrollment_arm_1", "ct_rectal_en"), by.x = "PTID", by.y = "record_id", all.x = TRUE)
summ <- merge(summ, make_ct_column(REDCAP, "1_month_followup_arm_1", "ct_rectal_1m"), by.x = "PTID", by.y = "record_id", all.x = TRUE)
summ <- merge(summ, make_ct_column(REDCAP, "4_month_followup_arm_1", "ct_rectal_4m"), by.x = "PTID", by.y = "record_id", all.x = TRUE)
summ <- merge(summ, make_ct_column(REDCAP, "8_month_followup_arm_1", "ct_rectal_8m"), by.x = "PTID", by.y = "record_id", all.x = TRUE)
summ <- merge(summ, make_ct_column(REDCAP, "12_month_followup_arm_1", "ct_rectal_12m"), by.x = "PTID", by.y = "record_id", all.x = TRUE)


#Readable CT rectal at enrollment 
summ <- summ %>%
  mutate(
    ct_rectal_en_char = dplyr::case_when(
      ct_rectal_en == 0 ~ "Rectal-",
      ct_rectal_en == 1 ~ "Rectal+",
      ct_rectal_en == 2 ~ "Indeterminate",
      ct_rectal_en == 3 ~ "Not done",
      TRUE ~ NA_character_
    )
  )




# Count follow-up visits completed
summ$followup_visits_completed <- rowSums(summ[, c("visits_present_1m", "visits_present_4m", "visits_present8m", "visits_present12m")] == 1, na.rm = TRUE)


# == ct_vag_bothcenters logic (applies in order) == #

apply_ct_vag_logic <- function(vag, prsby, rectal, event) {
  vag    <- as.character(vag)
  prsby  <- as.character(prsby)
  rectal <- as.character(rectal)
  
  # Missing logic
  if (is.na(vag) && is.na(prsby)) return(NA_character_)
  if (is.na(vag))  return(prsby)
  if (is.na(prsby)) return(vag)
  
  # Indeterminate logic
  if (vag == "2" && prsby %in% c("0","2","3","4")) return("2")
  if (vag %in% c("0","2","3") && prsby == "2") return("2")
  if (vag == "3" && prsby == "3") return("3")
  
  # Enrollment-specific rule
  if (vag == "0" && prsby == "4" && event == "enrollment_arm_1") return("2") #at enrollment, not done because pos value before so not required 
  
  # Follow-up rule
  if (vag == "0" && prsby == "4") return("0") #at f.u 3 is similar to 4
  
  # Negative logic
  if (vag == "0" && !(prsby %in% c("1","4"))) return("0")
  if (!(vag %in% c("1")) && prsby == "0") return("0")
  
  # Positive logic
  if (vag %in% c("1") && prsby %in% c("0","1","2","3","4")) return("1")
  if (vag %in% c("0","1","2","3") && prsby == "1") return("1")
  
  return("NA")
}


for (tp in c("EN", "1m", "4m", "8m", "12m")) {
  
  event_name <- switch(tp,
                       "EN"  = "enrollment_arm_1",
                       "1m"  = "1_month_followup_arm_1",
                       "4m"  = "4_month_followup_arm_1",
                       "8m"  = "8_month_followup_arm_1",
                       "12m" = "12_month_followup_arm_1"
  )
  
  vag_col    <- paste0("ct_vag_", tp)
  prsby_col  <- paste0("ct_vag_prsby_", tp)
  rectal_col <- paste0("ct_rectal_", tp)
  out_col    <- paste0("CT_vag_bothcenters_", tp)
  
  temp <- REDCAP[REDCAP$redcap_event_name == event_name, ]
  
  # Create timepoint-specific columns
  temp[[vag_col]]    <- as.character(temp$ct_vag)
  temp[[prsby_col]]  <- as.character(temp$ct_vag_prsby)
  temp[[rectal_col]] <- as.character(temp$ct_rectal)
  
  # Apply logic with event context
  temp[[out_col]] <- mapply(
    apply_ct_vag_logic,
    temp[[vag_col]],
    temp[[prsby_col]],
    temp[[rectal_col]],
    MoreArgs = list(event = event_name)
  )
  
  # Deduplicate and merge
  lookup <- temp[, c("record_id", out_col)]
  lookup <- lookup[!duplicated(lookup$record_id), ]
  
  summ <- merge(
    summ,
    lookup,
    by.x = "PTID",
    by.y = "record_id",
    all.x = TRUE
  )
}



# == Create endometrial chlamydia col == #

# Function to extract ct_endm at enrollment
make_ct_endm_column <- function(df) {
  temp <- df[df$redcap_event_name == "enrollment_arm_1", ]
  temp$ct_endm <- suppressWarnings(as.numeric(temp$ct_endm))  # numeric, keep NA
  out <- temp[, c("record_id", "ct_endm")]
  out <- out[!duplicated(out$record_id), ]
  return(out)
}

# Merge into summ
summ <- merge(
  summ,
  make_ct_endm_column(REDCAP),
  by.x = "PTID", by.y = "record_id",
  all.x = TRUE
)


# Helper function to extract ct_vag or ct_vag_prsby per event
make_ct_vag_column <- function(df, event_name, var_name, source_col) {
  temp <- df[df$redcap_event_name == event_name, ]
  #temp[[var_name]] <- ifelse(is.na(temp[[source_col]]), "NA", as.character(temp[[source_col]]))
  temp[[var_name]] <- as.character(temp[[source_col]])
  out <- temp[, c("record_id", var_name)]
  out <- out[!duplicated(out$record_id), ]
  return(out)
}

# Add ct_vag columns
summ <- merge(summ, make_ct_vag_column(REDCAP, "enrollment_arm_1", "ct_vag_EN", "ct_vag"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)
summ <- merge(summ, make_ct_vag_column(REDCAP, "1_month_followup_arm_1", "ct_vag_1m", "ct_vag"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)
summ <- merge(summ, make_ct_vag_column(REDCAP, "4_month_followup_arm_1", "ct_vag_4m", "ct_vag"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)
summ <- merge(summ, make_ct_vag_column(REDCAP, "8_month_followup_arm_1", "ct_vag_8m", "ct_vag"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)
summ <- merge(summ, make_ct_vag_column(REDCAP, "12_month_followup_arm_1", "ct_vag_12m", "ct_vag"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)

# Add ct_vag_prsby columns
summ <- merge(summ, make_ct_vag_column(REDCAP, "enrollment_arm_1", "ct_vag_prsby_EN", "ct_vag_prsby"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)
summ <- merge(summ, make_ct_vag_column(REDCAP, "1_month_followup_arm_1", "ct_vag_prsby_1m", "ct_vag_prsby"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)
summ <- merge(summ, make_ct_vag_column(REDCAP, "4_month_followup_arm_1", "ct_vag_prsby_4m", "ct_vag_prsby"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)
summ <- merge(summ, make_ct_vag_column(REDCAP, "8_month_followup_arm_1", "ct_vag_prsby_8m", "ct_vag_prsby"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)
summ <- merge(summ, make_ct_vag_column(REDCAP, "12_month_followup_arm_1", "ct_vag_prsby_12m", "ct_vag_prsby"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)


# ============================================================
# 1. Remove old CT_vag_bothcenters_* columns BEFORE rebuilding
# ============================================================
summ <- summ[, !grepl("^CT_vag_bothcenters_", names(summ))]

# ============================================================
# 2. Build CT_vag_bothcenters_* WITHOUT .x/.y suffixes
# ============================================================
for (tp in c("EN", "1m", "4m", "8m", "12m")) {
  
  event_name <- switch(tp,
                       "EN"  = "enrollment_arm_1",
                       "1m"  = "1_month_followup_arm_1",
                       "4m"  = "4_month_followup_arm_1",
                       "8m"  = "8_month_followup_arm_1",
                       "12m" = "12_month_followup_arm_1"
  )
  
  vag_col    <- paste0("ct_vag_", tp)
  prsby_col  <- paste0("ct_vag_prsby_", tp)
  rectal_col <- paste0("ct_rectal_", tp)
  out_col    <- paste0("CT_vag_bothcenters_", tp)
  
  temp <- REDCAP[REDCAP$redcap_event_name == event_name, ]
  
  temp[[vag_col]]    <- as.character(temp$ct_vag)
  temp[[prsby_col]]  <- as.character(temp$ct_vag_prsby)
  temp[[rectal_col]] <- as.character(temp$ct_rectal)
  
  temp[[out_col]] <- mapply(
    apply_ct_vag_logic,
    temp[[vag_col]],
    temp[[prsby_col]],
    temp[[rectal_col]],
    MoreArgs = list(event = event_name)
  )
  
  lookup <- temp[, c("record_id", out_col)]
  lookup <- lookup[!duplicated(lookup$record_id), ]
  
  summ <- merge(
    summ,
    lookup,
    by.x = "PTID",
    by.y = "record_id",
    all.x = TRUE
  )
}


# ============================================================
# 3. Merge CT_CB_burden and CT_VS_burden
# ============================================================
summ <- merge(
  summ,
  CT_MG_compiled[, c("PTID", "CT_CB_burden", "CT_VS_burden")],
  by = "PTID",
  all.x = TRUE
)


# ============================================================
# 4. Compute CT_CB_result
# ============================================================
summ$CT_CB_result <- ifelse(
  is.na(summ$CT_CB_burden), NA,
  ifelse(summ$CT_CB_burden > 500, 1, 0)
)

# ============================================================
# 5. Compute CT_VS_result
# ============================================================
summ$CT_VS_result <- ifelse(
  is.na(summ$CT_VS_burden), NA,
  ifelse(summ$CT_VS_burden > 500, 1, 0)
)

# ============================================================
# 6. Ensure CT burden columns are numeric
# ============================================================
summ$CT_CB_burden <- as.numeric(summ$CT_CB_burden)
summ$CT_VS_burden <- as.numeric(summ$CT_VS_burden)

# ============================================================
# 7. Ensure CT result columns are numeric
# ============================================================
summ$CT_CB_result <- as.numeric(summ$CT_CB_result)
summ$CT_VS_result <- as.numeric(summ$CT_VS_result)

# ============================================================
# 8. Compute CT_inferred_diagnostic
# ============================================================

cb <- summ$CT_CB_result
vs <- summ$CT_VS_result
en <- as.numeric(summ$CT_vag_bothcenters_EN)

summ <- summ %>%
  mutate(
    CT_any_1 = rowSums(cbind(
      CT_CB_result == 1,
      CT_VS_result == 1,
      CT_vag_bothcenters_EN == 1
    ), na.rm = TRUE),
    
    CT_any_0 = rowSums(cbind(
      CT_CB_result == 0,
      CT_VS_result == 0,
      CT_vag_bothcenters_EN == 0
    ), na.rm = TRUE),
    
    CT_all_na = is.na(CT_CB_result) & is.na(CT_VS_result) & is.na(CT_vag_bothcenters_EN),
    
    CT_inferred_diagnostic_num = case_when(
      CT_all_na ~ NA_real_,
      CT_any_1  > 0 ~ 1,
      CT_any_1 == 0 & CT_any_0 > 0 ~ 0
    )
  )

#summ$CT_inferred_diagnostic_num <- ifelse(
  # 1) Assign 1 if ANY is 1
#  cb == 1 | vs == 1 | en == 1, 1,
  
#  ifelse(
    # 2) Assign 0 if CB and EN are 0, regardless of VS missing (account for 613)
#    cb == 0 & en == 0 & is.na(vs), 0,
    
    # 2a) assign 0 is CB, EN, and VS are 0
#    cb == 0 & en == 0 & vs == 0, 0,
    
#   ifelse(
      # 3) Assign NA only if ALL are NA
#      is.na(cb) & is.na(vs) & is.na(en), NA,
      
      # 4) Otherwise assign 2 (indeterminate)
#      2
#    )
#  )
#)


summ$CT_inferred_diagnostic_num <- as.numeric(summ$CT_inferred_diagnostic_num)

summ$CT_inferred_diagnostic_char <- dplyr::case_when(
  summ$CT_inferred_diagnostic_num == 0 ~ "CT-",
  summ$CT_inferred_diagnostic_num == 1 ~ "CT+",
  summ$CT_inferred_diagnostic_num == 2 ~ "Indeterminate",
  TRUE ~ NA_character_
)

# == Merge MG burden values from CT_MG_compiled == #
summ <- merge(summ, CT_MG_compiled[, c("PTID", "MG_CB_burden", "MG_VS_burden")], 
              by = "PTID", all.x = TRUE)

# Compute MG_CB_result
summ$MG_CB_result <- ifelse(
  is.na(summ$MG_CB_burden), NA_real_,
  ifelse(summ$MG_CB_burden > 500, 1, 0)
)

summ$MG_VS_result <- ifelse(
  is.na(summ$MG_VS_burden), NA_real_,
  ifelse(summ$MG_VS_burden > 500, 1, 0)
)

# ---------------------------------------------------------
# EXCEPTION: Force MG_CB_result = 1 for PTID 538 and 550
# from email Re: Question for Monday - MG inferred diagnostic;consider #538 and #550 as positive with burden at the limit of detection because detection in at least 1 well
# ---------------------------------------------------------
summ$MG_CB_result[summ$PTID %in% c("538", "550")] <- 1

# Recompute MG_inferred_diagnostic using updated MG_CB_result
summ$MG_inferred_diagnostic <- ifelse(
  is.na(summ$MG_CB_result) | is.na(summ$MG_VS_result), NA_real_,
  ifelse(summ$MG_CB_result == 0 & summ$MG_VS_result == 0, 0,
         ifelse(summ$MG_CB_result == 1 | summ$MG_VS_result == 1, 1, 2))
)

# Ensure numeric type and preserve NA
summ$MG_CB_burden <- as.numeric(summ$MG_CB_burden)
summ$MG_VS_burden <- as.numeric(summ$MG_VS_burden)


##################
# Histology data
##################

#summ <- summ |>
#  dplyr::left_join(
#    histology |> dplyr::select(PTID, Chronicity),
#    by = "PTID"
#  )

#this way ensures the 2 NA (619 and 620) are lumped with NP 
#note that PTID in summ df is character, so needs to be character in histology df 

histology <- histology |>
  dplyr::mutate(PTID = as.character(PTID))

summ <- summ |>
  dplyr::mutate(PTID = as.character(PTID)) |>
  dplyr::left_join(
    histology |> dplyr::select(PTID, Chronicity),
    by = "PTID"
  ) |>
  dplyr::mutate(
    Chronicity = dplyr::if_else(
      is.na(Chronicity),
      "NA",
      Chronicity
    )
  )



# == Combined ct and rectal == #

summ <- summ %>%
  mutate(
    ct_rectal_combined_en = dplyr::case_when(
      CT_inferred_diagnostic_num == 1 & ct_rectal_en == 1 ~ "Rectal+ CT+",
      CT_inferred_diagnostic_num == 0 & ct_rectal_en == 1 ~ "Rectal+ CT-",
      CT_inferred_diagnostic_num == 1 & ct_rectal_en == 0 ~ "Rectal- CT+",
      CT_inferred_diagnostic_num == 0 & ct_rectal_en == 0 ~ "Rectal- CT-",
      TRUE ~ NA_character_
    )
  )



# == CT interim columns (1m, 4m, 8m, 12m) == # 

make_chlam_column <- function(df, event_name, var_name) {
  temp <- df[df$redcap_event_name == event_name, ]
  temp[[var_name]] <- temp[["chlam_v2"]]
  out <- temp[, c("record_id", var_name)]
  out <- out[!duplicated(out$record_id), ]
  return(out)
}

# Add chlam_v2 columns by event
summ <- merge(summ, make_chlam_column(REDCAP, "1_month_followup_arm_1", "chlam_v2_1m"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)
summ <- merge(summ, make_chlam_column(REDCAP, "4_month_followup_arm_1", "chlam_v2_4m"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)
summ <- merge(summ, make_chlam_column(REDCAP, "8_month_followup_arm_1", "chlam_v2_8m"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)
summ <- merge(summ, make_chlam_column(REDCAP, "12_month_followup_arm_1", "chlam_v2_12m"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)

# CT_vag_interim logic
summ <- summ %>%
  mutate(
    CT_vag_interim_1m  = case_when(
      is.na(CT_vag_bothcenters_1m) | is.na(chlam_v2_1m) ~ NA_real_,
      CT_vag_bothcenters_1m == 0 & chlam_v2_1m == 0 ~ 0, #neg diagnostic at both sites & no report 
      CT_vag_bothcenters_1m == 1 | chlam_v2_1m == 1 ~ 1, #pos diagnostic either site or a report 
      TRUE ~ 2
    ),
    CT_vag_interim_4m  = case_when(
      is.na(CT_vag_bothcenters_4m) | is.na(chlam_v2_4m) ~ NA_real_,
      CT_vag_bothcenters_4m == 0 & chlam_v2_4m == 0 ~ 0,
      CT_vag_bothcenters_4m == 1 | chlam_v2_4m == 1 ~ 1,
      TRUE ~ 2
    ),
    CT_vag_interim_8m  = case_when(
      is.na(CT_vag_bothcenters_8m) | is.na(chlam_v2_8m) ~ NA_real_,
      CT_vag_bothcenters_8m == 0 & chlam_v2_8m == 0 ~ 0,
      CT_vag_bothcenters_8m == 1 | chlam_v2_8m == 1 ~ 1,
      TRUE ~ 2
    ),
    CT_vag_interim_12m = case_when(
      is.na(CT_vag_bothcenters_12m) | is.na(chlam_v2_12m) ~ NA_real_,
      CT_vag_bothcenters_12m == 0 & chlam_v2_12m == 0 ~ 0,
      CT_vag_bothcenters_12m == 1 | chlam_v2_12m == 1 ~ 1,
      TRUE ~ 2
    )
  )

# == report of [chlam] diagnosed before enrollment or since last visit == #

# function for chlam diagnostic per visit
make_chlam_column <- function(df, event_name, var_name, source_var) {
  temp <- df[df$redcap_event_name == event_name, ]
  temp[[var_name]] <- suppressWarnings(as.numeric(temp[[source_var]]))
  
  out <- temp[, c("record_id", var_name)]
  out <- out[!duplicated(out$record_id), ]
  return(out)
}

# Enrollment (uses chlam)
summ <- merge(
  summ,
  make_chlam_column(REDCAP, "enrollment_arm_1", "chlam_en", "chlam"),
  by.x = "PTID", by.y = "record_id", all.x = TRUE
)

# 1‑month (uses chlam_v2)
summ <- merge(
  summ,
  make_chlam_column(REDCAP, "1_month_followup_arm_1", "chlam_v2_1m", "chlam_v2"),
  by.x = "PTID", by.y = "record_id", all.x = TRUE
)

# 4‑month
summ <- merge(
  summ,
  make_chlam_column(REDCAP, "4_month_followup_arm_1", "chlam_v2_4m", "chlam_v2"),
  by.x = "PTID", by.y = "record_id", all.x = TRUE
)

# 8‑month
summ <- merge(
  summ,
  make_chlam_column(REDCAP, "8_month_followup_arm_1", "chlam_v2_8m", "chlam_v2"),
  by.x = "PTID", by.y = "record_id", all.x = TRUE
)

# 12‑month
summ <- merge(
  summ,
  make_chlam_column(REDCAP, "12_month_followup_arm_1", "chlam_v2_12m", "chlam_v2"),
  by.x = "PTID", by.y = "record_id", all.x = TRUE
)


# == Pre-enrollment chlamydia == #

#Function
make_ct_column <- function(df, event_name, var_name, source_col) {
  temp <- df[df$redcap_event_name == event_name, ]
  temp[[var_name]] <- suppressWarnings(as.numeric(temp[[source_col]]))
  out <- temp[, c("record_id", var_name)]
  out <- out[!duplicated(out$record_id), ]
  return(out)
}


#numtimes chlam before enrollment
summ <- merge(
  summ,
  make_ct_column(REDCAP, "enrollment_arm_1", "numtimes_ct", "numtimes_ct"),
  by.x = "PTID", by.y = "record_id", all.x = TRUE
)



#date of most recent chlamydia infection
# clean up dates - in df some are mm/dd/yyyy some are yyyy some blank/NA 

parse_one_date <- function(x) {
  x <- trimws(x)
  x <- sub("\\.0$", "", x)
  
  if (is.na(x) || x == "") return(as.Date(NA))
  
  # Excel serial number (pure digits, length 5+ typically)
  if (grepl("^\\d+$", x) && nchar(x) >= 5) {
    return(as.Date(as.numeric(x), origin = "1899-12-30"))
  }
  
  #  Full date mm/dd/yyyy
  if (grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", x)) {
    return(as.Date(x, format = "%m/%d/%Y"))
  }
  
  #  Month/year (mm/yyyy)
  if (grepl("^\\d{1,2}/\\d{4}$", x)) {
    return(as.Date(paste0("01/", x), format = "%d/%m/%Y"))
  }
  
  #  Year only
  if (grepl("^\\d{4}$", x)) {
    return(as.Date(paste0(x, "-01-01")))
  }
  
  return(as.Date(NA))
}


fix_mixed_dates <- function(x) {
  x <- as.character(x)
  
  result <- sapply(x, function(cell) {
    
    if (is.na(cell) || cell == "") return(NA)
    
    parts <- unlist(strsplit(cell, ","))
    parts <- trimws(parts)
    
    parsed <- sapply(parts, parse_one_date)
    
    if (all(is.na(parsed))) return(NA)
    
    max(parsed, na.rm = TRUE)
  })
  
  as.Date(result, origin = "1970-01-01")
}




#keep raw columns and parse from them
# note in UNREDACTED data file, 4 participants (504, 608, 609, 618) had male_lastsex dates that occurred after enrollment in the unredacted data and Nefer/Kacy determined that the years were incorrect and were manually adjusted

UNREDACTED <- UNREDACTED |>
  dplyr::mutate(
    enroll_date_fix      = as.Date(as.numeric(enroll_date), origin = "1899-12-30"),
    daterecent_ct_fix_2  = fix_mixed_dates(daterecent_ct_fix),
    daterecent_gc_fix_2  = fix_mixed_dates(daterecent_gc_fix),
    daterecent_pid_fix   = fix_mixed_dates(daterecent_pid),
    male_lastsex_fix_2     = fix_mixed_dates(male_lastsex_fix)
  )

#compute date diff of diagnosis date days before enrollment 
# align variable types and convert record_id to PTID for merging to summ df (UNREDACTED df still uses record_id)

make_date_diff_var <- function(df, diag_var, enroll_var, out_var) {
  df |>
    dplyr::transmute(
      PTID = as.character(record_id),
      enroll_date = .data[[enroll_var]],
      diag_date   = .data[[diag_var]],
      !!out_var := as.numeric(enroll_date - diag_date)
    ) |>
    dplyr::select(PTID, !!out_var)
}

#generate days since for chlamydia, gonorrhea, vag sex w male, pelvic inflamm disease at enrollment (excluding any dx at enrollment)

ct_dates <- make_date_diff_var(
  df        = UNREDACTED,
  diag_var  = "daterecent_ct_fix_2",
  enroll_var = "enroll_date_fix",
  out_var   = "daterecent_ct_days"
)

gc_dates <- make_date_diff_var(
  df        = UNREDACTED,
  diag_var  = "daterecent_gc_fix_2",
  enroll_var = "enroll_date_fix",
  out_var   = "daterecent_gc_days"
)

pid_dates <- make_date_diff_var(
  df        = UNREDACTED,
  diag_var  = "daterecent_pid_fix",
  enroll_var = "enroll_date_fix",
  out_var   = "daterecent_pid_days"
)

male_lastsex_dates <- make_date_diff_var(
  df        = UNREDACTED,
  diag_var  = "male_lastsex_fix_2",
  enroll_var = "enroll_date_fix",
  out_var   = "male_lastsex_days"
)

#merge into summ
summ <- summ |>
  dplyr::select(
    -dplyr::any_of(c(
      "daterecent_ct_days",
      "daterecent_gc_days",
      "daterecent_pid_days",
      "male_lastsex_days"
    ))
  ) |>
  dplyr::left_join(ct_dates,          by = "PTID") |>
  dplyr::left_join(gc_dates,          by = "PTID") |>
  dplyr::left_join(pid_dates,         by = "PTID") |>
  dplyr::left_join(male_lastsex_dates, by = "PTID")


#create years since and merge into summ
summ <- summ |>
  dplyr::mutate(
    daterecent_ct_years      = round(daterecent_ct_days      / 365, 1),
    daterecent_gc_years      = round(daterecent_gc_days      / 365, 1),
    daterecent_pid_years     = round(daterecent_pid_days     / 365, 1),
    male_lastsex_years       = round(male_lastsex_days       / 365, 1)
  )

#make sure numeric for output 
summ <- summ |>
  dplyr::mutate(
    daterecent_ct_days      = as.numeric(daterecent_ct_days),
    daterecent_ct_years     = as.numeric(daterecent_ct_years),
    
    daterecent_gc_days      = as.numeric(daterecent_gc_days),
    daterecent_gc_years     = as.numeric(daterecent_gc_years),
    
    daterecent_pid_days     = as.numeric(daterecent_pid_days),
    daterecent_pid_years    = as.numeric(daterecent_pid_years),
    
    male_lastsex_days       = as.numeric(male_lastsex_days),
    male_lastsex_years      = as.numeric(male_lastsex_years)
  )

#see distribution to decide groupings

summ |>
  dplyr::mutate(
    ct_years_cat = dplyr::case_when(
      daterecent_ct_years >= 2   ~ "≥ 2 years",
      daterecent_ct_years >= 1   ~ "≥1 year to <2 years",
      daterecent_ct_years >= 0.5 ~ "≥0.5 years to <1 year",
      daterecent_ct_years >= 0   ~ "<0.5 years",
      TRUE ~ NA_character_
    ),
    ct_years_cat = factor(
      ct_years_cat,
      levels = c(
        "<0.5 years",
        "≥0.5 years to <1 year",
        "≥1 year to <2 years",
        "≥ 2 years"
      ),
      ordered = TRUE
    )
  ) |>
  dplyr::count(ct_years_cat)

#participant did not report prior NA 
summ$numtimes_ct[ summ$chlam != 1 ] <- NA

###############################################
# Create endo/cervix classification 
##############################################

summ$endo <- dplyr::case_when(
  summ$CT_vag_bothcenters_EN == 1 & summ$ct_endm == 0 ~ "Endo-",
  summ$CT_vag_bothcenters_EN == 1 & summ$ct_endm == 1 ~ "Endo+",
  TRUE ~ "NA"
)


#########################################################
# === Create genital chlamydia follow up pos/neg col === # 
#########################################################

#rowSums collapses the logical checks into counts, ignoring NA. So if count is 1 or more, its fu+ rather than checking for any 1s in cols


summ <- summ %>%
  mutate(
    fu = case_when(
      followup_visits_completed >= 3 &
        rowSums(cbind(
          CT_vag_interim_1m == 1,
          CT_vag_interim_4m == 1,
          CT_vag_interim_8m == 1,
          CT_vag_interim_12m == 1
        ), na.rm = TRUE) == 0 ~ "FU-",
      
      rowSums(cbind(
        CT_vag_interim_1m == 1,
        CT_vag_interim_4m == 1,
        CT_vag_interim_8m == 1,
        CT_vag_interim_12m == 1
      ), na.rm = TRUE) > 0 ~ "FU+",
      
      TRUE ~ NA_character_
    ),
    
    fu_num = case_when(
      fu == "FU-" ~ 0,
      fu == "FU+" ~ 1,
      TRUE ~ NA_real_
    )
  )

# ============================
# genital/cervix + rectal combined classification
# ============================

# helper
combine_genital_rectal <- function(vag, rect) {
  if (is.na(vag) | is.na(rect)) return(NA_real_)
  if (vag == 0 && rect == 0) return(0)
  if (vag == 1 && rect == 0) return(1)
  if (vag == 0 && rect == 1) return(2)
  if (vag == 1 && rect == 1) return(3)
  return(NA_real_)
}

# timepoints
tps <- c("1m", "4m", "8m", "12m")

for (tp in tps) {
  
  vag_col  <- paste0("CT_vag_interim_", tp)
  rect_col <- paste0("ct_rectal_", tp)
  out_col  <- paste0("genital_rectal_", tp)
  
  # ---- SAFETY: create missing columns as NA ----
  if (!vag_col %in% names(summ)) summ[[vag_col]] <- NA_real_
  if (!rect_col %in% names(summ)) summ[[rect_col]] <- NA_real_
  
  # ---- always returns a vector of length nrow(summ) ----
  summ[[out_col]] <- mapply(
    combine_genital_rectal,
    summ[[vag_col]],
    summ[[rect_col]]
  )
}

#### Rectal follow-up positive or negative #####
################################################

## Rectal chlamydia diagnosis at follow-up

summ <- summ %>%
  mutate(
    fu_rectal = case_when(
      followup_visits_completed >= 3 &
        rowSums(cbind(
          ct_rectal_1m == 1,
          ct_rectal_4m == 1,
          ct_rectal_8m == 1,
          ct_rectal_12m == 1
        ), na.rm = TRUE) == 0 ~ "FU-",
      
      rowSums(cbind(
        ct_rectal_1m == 1,
        ct_rectal_4m == 1,
        ct_rectal_8m == 1,
        ct_rectal_12m == 1
      ), na.rm = TRUE) > 0 ~ "FU+",
      
      TRUE ~ NA_character_
    ),
    
    fu_rectal_num = case_when(
      fu_rectal == "FU-" ~ 0,
      fu_rectal == "FU+" ~ 1,
      TRUE ~ NA_real_
    )
  )

## Rectal chlamydia+ but CT- at any point during f/u
###################################################3

#checks whether any of the 4 genital–rectal timepoints equals 2 (rectal+ only), Yes if so, else No/NA


summ <- summ %>%
  mutate(
    rectal_reservoir = dplyr::case_when(
      genital_rectal_1m == 2 |
        genital_rectal_4m == 2 |
        genital_rectal_8m == 2 |
        genital_rectal_12m == 2 ~ "Yes",
      TRUE ~ "No/NA"
    )
  )




# == CT status of sex partner(s) at enrollment and f/u == #

#Function
make_gc_partner_column <- function(df, event_name, var_name, source_col) {
  temp <- df[df$redcap_event_name == event_name, ]
  temp[[var_name]] <- suppressWarnings(as.numeric(temp[[source_col]]))
  out <- temp[, c("record_id", var_name)]
  out <- out[!duplicated(out$record_id), ]
  return(out)
}

#Partner ct at enrollment - yes/no
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "enrollment_arm_1", "ct", "ct"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

#Partner ct at enrollment - partner treatment 
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "enrollment_arm_1", "ct_tx", "ct_tx"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

#Partner ct at enrollment - self treatment 
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "enrollment_arm_1", "ct_self", "ct_self"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

# NOTE: ONLY 604, 609 were yes at 1 mo
#Partner ct at 1mo - yes/no
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "1_month_followup_arm_1", "ct_v2_1m", "ct_v2"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

#Partner ct at 1mo - partner treatment 
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "1_month_followup_arm_1", "ct_tx_v2_1m", "ct_tx_v2"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

#Partner ct at 1mo - self treatment 
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "1_month_followup_arm_1", "ct_self_v2_1m", "ct_self_v2"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

# Partner CT at 4m – yes/no
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "4_month_followup_arm_1", "ct_v2_4m", "ct_v2"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

# Partner CT at 4m – partner treatment
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "4_month_followup_arm_1", "ct_tx_v2_4m", "ct_tx_v2"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

# Partner CT at 4m – self treatment
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "4_month_followup_arm_1", "ct_self_v2_4m", "ct_self_v2"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

# Partner CT at 8m – yes/no
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "8_month_followup_arm_1", "ct_v2_8m", "ct_v2"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

# Partner CT at 8m – partner treatment
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "8_month_followup_arm_1", "ct_tx_v2_8m", "ct_tx_v2"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

# Partner CT at 8m – self treatment
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "8_month_followup_arm_1", "ct_self_v2_8m", "ct_self_v2"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

# Partner CT at 12m – yes/no
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "12_month_followup_arm_1", "ct_v2_12m", "ct_v2"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

# Partner CT at 12m – partner treatment
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "12_month_followup_arm_1", "ct_tx_v2_12m", "ct_tx_v2"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

# Partner CT at 12m – self treatment
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "12_month_followup_arm_1", "ct_self_v2_12m", "ct_self_v2"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)


# == Add trich data == #

# function for trich diagnosis before enrollment
make_trich_column <- function(df, event_name, var_name) {
  temp <- df[df$redcap_event_name == event_name, ]
  temp[[var_name]] <- suppressWarnings(as.numeric(temp$trich))
  out <- temp[, c("record_id", var_name)]
  out <- out[!duplicated(out$record_id), ]
  return(out)
}

# function for vaginal trichomonas diagnostic per visit
make_tx_vag_column <- function(df, event_name, var_name) {
  temp <- df[df$redcap_event_name == event_name, ]
  temp[[var_name]] <- suppressWarnings(as.numeric(temp$tx_vag))
  out <- temp[, c("record_id", var_name)]
  out <- out[!duplicated(out$record_id), ]
  return(out)
}

# Extract reported trich diagnosis since last visit
make_trich_v2_column <- function(df, event_name, var_name) {
  temp <- df[df$redcap_event_name == event_name, ]
  temp[[var_name]] <- suppressWarnings(as.numeric(temp$trich_v2))
  out <- temp[, c("record_id", var_name)]
  out <- out[!duplicated(out$record_id), ]
  return(out)
}

# Ever had trich diagnosis before enrollment
summ <- merge(summ, make_trich_column(REDCAP, "enrollment_arm_1", "trich_En"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)

# Vaginal trichomonas diagnostic (enrollment + 4 f/u visits)
summ <- merge(summ, make_tx_vag_column(REDCAP, "enrollment_arm_1", "tx_vag_En"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)
summ <- merge(summ, make_tx_vag_column(REDCAP, "1_month_followup_arm_1", "tx_vag_1m"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)
summ <- merge(summ, make_tx_vag_column(REDCAP, "4_month_followup_arm_1", "tx_vag_4m"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)
summ <- merge(summ, make_tx_vag_column(REDCAP, "8_month_followup_arm_1", "tx_vag_8m"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)
summ <- merge(summ, make_tx_vag_column(REDCAP, "12_month_followup_arm_1", "tx_vag_12m"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)

# Reported trich diagnosis since last visit (4 f/u viists)
summ <- merge(summ, make_trich_v2_column(REDCAP, "1_month_followup_arm_1", "trich_v2_1m"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)
summ <- merge(summ, make_trich_v2_column(REDCAP, "4_month_followup_arm_1", "trich_v2_4m"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)
summ <- merge(summ, make_trich_v2_column(REDCAP, "8_month_followup_arm_1", "trich_v2_8m"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)
summ <- merge(summ, make_trich_v2_column(REDCAP, "12_month_followup_arm_1", "trich_v2_12m"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)

# Interim trich ; Participant reported or was diagnosed with trich at f/u (0=no/1=yes/2=indeterminate/NA=missing)
# Remove .y versions
summ <- summ[, !grepl("\\.y$", names(summ))]

# Rename .x → clean name
names(summ) <- sub("\\.x$", "", names(summ))


summ <- summ %>%
  mutate(
    tv_interim_1m = case_when(
      is.na(tx_vag_1m) | is.na(trich_v2_1m) ~ NA_real_,
      tx_vag_1m == 0 & trich_v2_1m == 0 ~ 0,
      tx_vag_1m == 1 | trich_v2_1m == 1 ~ 1,
      TRUE ~ 2
    ),
    tv_interim_4m = case_when(
      is.na(tx_vag_4m) | is.na(trich_v2_4m) ~ NA_real_,
      tx_vag_4m == 0 & trich_v2_4m == 0 ~ 0,
      tx_vag_4m == 1 | trich_v2_4m == 1 ~ 1,
      TRUE ~ 2
    ),
    tv_interim_8m = case_when(
      is.na(tx_vag_8m) | is.na(trich_v2_8m) ~ NA_real_,
      tx_vag_8m == 0 & trich_v2_8m == 0 ~ 0,
      tx_vag_8m == 1 | trich_v2_8m == 1 ~ 1,
      TRUE ~ 2
    ),
    tv_interim_12m = case_when(
      is.na(tx_vag_12m) | is.na(trich_v2_12m) ~ NA_real_,
      tx_vag_12m == 0 & trich_v2_12m == 0 ~ 0,
      tx_vag_12m == 1 | trich_v2_12m == 1 ~ 1,
      TRUE ~ 2
    )
  )

# Trich f/u negative or positive

summ <- summ %>%
  mutate(
    tv_fu = case_when(
      followup_visits_completed >= 3 &
        rowSums(cbind(
          tv_interim_1m == 1,
          tv_interim_4m == 1,
          tv_interim_8m == 1,
          tv_interim_12m == 1
        ), na.rm = TRUE) == 0 ~ 0,
      
      rowSums(cbind(
        tv_interim_1m == 1,
        tv_interim_4m == 1,
        tv_interim_8m == 1,
        tv_interim_12m == 1
      ), na.rm = TRUE) > 0 ~ 1,
      
      TRUE ~ NA_real_
    )
  )

# ==  Trich burden (from separate df with 11 PTIDs) == # 
## EDIT: consider putting empty cells at LOD (500) and put key in title for 500 being neg 
summ <- merge(summ,
              trich[, c("PTID", "trich_burden")],
              by = "PTID",    # same column name in both data frames
              all.x = TRUE)

# ===========================================
# Reported or diagnosed co-infections 
# ===========================================

# == Gonorrhea == #
# -----------------

#Gonorrhea pre-enrollment

# Gonorrhea prior to enrollment
make_gc_enrollment_column <- function(df, event_name, var_name, source_col) {
  temp <- df[df$redcap_event_name == event_name, ]
  temp[[var_name]] <- temp[[source_col]]
  out <- temp[, c("record_id", var_name)]
  out <- out[!duplicated(out$record_id), ]
  return(out)
}

# numtimes_gc
summ <- merge(
  summ,
  make_gc_enrollment_column(REDCAP, "enrollment_arm_1", "numtimes_gc", "numtimes_gc"),
  by.x = "PTID", by.y = "record_id", all.x = TRUE
)


# == Gonorrhea at enrollment == #
make_gonn_column <- function(df, event_name, var_name) {
  temp <- df[df$redcap_event_name == event_name, ]
  temp[[var_name]] <- suppressWarnings(as.numeric(temp$gonn))
  out <- temp[, c("record_id", var_name)]
  out <- out[!duplicated(out$record_id), ]
  return(out)
}

# Reported gonorrhea dx since last visit
make_gonn_v2_column <- function(df, event_name, var_name) {
  temp <- df[df$redcap_event_name == event_name, ]
  temp[[var_name]] <- suppressWarnings(as.numeric(temp$gonn_v2))
  out <- temp[, c("record_id", var_name)]
  out <- out[!duplicated(out$record_id), ]
  return(out)
}

# Enrollment gonorrhea diagnosis
summ <- merge(summ,
              make_gonn_column(REDCAP, "enrollment_arm_1", "gonn"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)

# Reported gonorrhea dx since last visit
summ <- merge(summ,
              make_gonn_v2_column(REDCAP, "1_month_followup_arm_1", "gonn_v2_1m"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)

summ <- merge(summ,
              make_gonn_v2_column(REDCAP, "4_month_followup_arm_1", "gonn_v2_4m"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)

summ <- merge(summ,
              make_gonn_v2_column(REDCAP, "8_month_followup_arm_1", "gonn_v2_8m"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)

summ <- merge(summ,
              make_gonn_v2_column(REDCAP, "12_month_followup_arm_1", "gonn_v2_12m"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)


# Combined gonorrhea genital results at centers == #

# MWRI vaginal gonorrhea result (0=negative/1=positive/2=indeterminate/3=not done)

make_gc_vag_column <- function(df, event_name, var_name) {
  temp <- df[df$redcap_event_name == event_name, ]
  temp[[var_name]] <- suppressWarnings(as.numeric(temp$gc_vag))
  out <- temp[, c("record_id", var_name)]
  out <- out[!duplicated(out$record_id), ]
  return(out)
}

summ <- merge(summ, make_gc_vag_column(REDCAP, "enrollment_arm_1", "gc_vag_en"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)

summ <- merge(summ, make_gc_vag_column(REDCAP, "1_month_followup_arm_1", "gc_vag_1m"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)

summ <- merge(summ, make_gc_vag_column(REDCAP, "4_month_followup_arm_1", "gc_vag_4m"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)

summ <- merge(summ, make_gc_vag_column(REDCAP, "8_month_followup_arm_1", "gc_vag_8m"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)

summ <- merge(summ, make_gc_vag_column(REDCAP, "12_month_followup_arm_1", "gc_vag_12m"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)

# Presby vaginal gonorrhea result (0=negative/1=positive/2=indeterminate/3=not done/4=not required)"

make_gc_vag_prsby_column <- function(df, event_name, var_name) {
  temp <- df[df$redcap_event_name == event_name, ]
  temp[[var_name]] <- suppressWarnings(as.numeric(temp$gc_vag_prsby))
  out <- temp[, c("record_id", var_name)]
  out <- out[!duplicated(out$record_id), ]
  return(out)
}

summ <- merge(summ, make_gc_vag_prsby_column(REDCAP, "enrollment_arm_1", "gc_vag_prsby_en"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)

summ <- merge(summ, make_gc_vag_prsby_column(REDCAP, "1_month_followup_arm_1", "gc_vag_prsby_1m"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)

summ <- merge(summ, make_gc_vag_prsby_column(REDCAP, "4_month_followup_arm_1", "gc_vag_prsby_4m"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)

summ <- merge(summ, make_gc_vag_prsby_column(REDCAP, "8_month_followup_arm_1", "gc_vag_prsby_8m"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)

summ <- merge(summ, make_gc_vag_prsby_column(REDCAP, "12_month_followup_arm_1", "gc_vag_prsby_12m"),
              by.x = "PTID", by.y = "record_id", all.x = TRUE)

# Gonorrhea (vaginal) combined diagnostic (both centers) (0=negative/1=positive/2=indeterminate/3=not done)" 

# Remove all .y versions across the entire data frame
summ <- summ[, !grepl("\\.y$", names(summ))]

# Rename all .x versions to clean names
names(summ) <- sub("\\.x$", "", names(summ))

#names(summ)[grepl("gc_vag", names(summ))]

apply_gc_vag_logic <- function(vag, prsby, redcap_event_name) {
  vag   <- suppressWarnings(as.numeric(vag))
  prsby <- suppressWarnings(as.numeric(prsby))
  
  # 9 & 10: missingness rules
  if (is.na(vag) && is.na(prsby)) return(NA_real_)
  if (is.na(vag) && !is.na(prsby)) return(prsby)
  if (!is.na(vag) && is.na(prsby)) return(vag)
  
  # 1: MWRI=1 & prsby in 0–4 → pos
  if (vag == 1 && prsby %in% c(0,1,2,3,4)) return(1)
  
  # 2: MWRI in 0–3 & prsby=1 → pos
  if (vag %in% c(0,1,2,3) && prsby == 1) return(1)
  
  # 3: MWRI=0 & prsby=4 → indeterminate ONLY at enrollment
 # if (vag == 0 && prsby == 4 && redcap_event_name == "enrollment_arm_1") return(2) #corrected 2/25/26 based on Re: Variable adjustment email
  
  # 3a: otherwise → negative
  if (vag == 0 && prsby == 4) return(0)
  
  # 4: vag=0 & prsby in 0,2,3 → neg
  if (vag == 0 && prsby %in% c(0,2,3)) return(0)
  
  # 5: vag !=1 & prsby=0 → neg
  if (vag != 1 && prsby == 0) return(0)
  
  # 6: vag=2 & prsby !=1 → indeterminate
  if (vag == 2 && prsby != 1) return(2)
  
  # 7: vag !=1 & prsby=2 → indeterminate
  if (vag != 1 && prsby == 2) return(2)
  
  # 8: vag=3 & prsby=3 → not done
  if (vag == 3 && prsby == 3) return(3)
  
  return(NA_real_)
}

for (tp in c("en", "1m", "4m", "8m", "12m")) {
  
  vag_col   <- paste0("gc_vag_", tp)
  prsby_col <- paste0("gc_vag_prsby_", tp)
  out_col   <- paste0("gc_vag_bothcenters_", tp)
  
  summ[[out_col]] <- mapply(
    apply_gc_vag_logic,
    summ[[vag_col]],
    summ[[prsby_col]],
    redcap_event_name = ifelse(tp == "en", "enrollment_arm_1", "followup")
  )
}

# Rectal gonorrhea result (0=negative/1=positive/2=indeterminate/3=not done)

make_gc_rectal_column <- function(df, event_name, var_name) {
  temp <- df[df$redcap_event_name == event_name, ]
  temp[[var_name]] <- suppressWarnings(as.numeric(temp$gc_rectal))
  out <- temp[, c("record_id", var_name)]
  out <- out[!duplicated(out$record_id), ]
  return(out)
}

summ <- merge(summ, make_gc_rectal_column(REDCAP, "enrollment_arm_1", "gc_rectal_en"),
              by.x="PTID", by.y="record_id", all.x=TRUE)

summ <- merge(summ, make_gc_rectal_column(REDCAP, "1_month_followup_arm_1", "gc_rectal_1m"),
              by.x="PTID", by.y="record_id", all.x=TRUE)

summ <- merge(summ, make_gc_rectal_column(REDCAP, "4_month_followup_arm_1", "gc_rectal_4m"),
              by.x="PTID", by.y="record_id", all.x=TRUE)

summ <- merge(summ, make_gc_rectal_column(REDCAP, "8_month_followup_arm_1", "gc_rectal_8m"),
              by.x="PTID", by.y="record_id", all.x=TRUE)

summ <- merge(summ, make_gc_rectal_column(REDCAP, "12_month_followup_arm_1", "gc_rectal_12m"),
              by.x="PTID", by.y="record_id", all.x=TRUE)


# Endometrial gonorrhea result  (0=negative/1=positive/2=indeterminate/3=not done)

make_gc_endm_column <- function(df, event_name, var_name) {
  temp <- df[df$redcap_event_name == event_name, ]
  temp[[var_name]] <- suppressWarnings(as.numeric(temp$gc_endm))
  out <- temp[, c("record_id", var_name)]
  out <- out[!duplicated(out$record_id), ]
  return(out)
}

summ <- merge(summ, make_gc_endm_column(REDCAP, "enrollment_arm_1", "gc_endm"),
              by.x="PTID", by.y="record_id", all.x=TRUE)


# Interim gonn ; Participant reported or was diagnosed with gonorrhea at f/u (0=no/1=yes/2=indeterminate/NA=missing)

summ <- summ %>%
  mutate(
    gc_vag_interim_1m = case_when(
      is.na(gc_vag_bothcenters_1m) | is.na(gonn_v2_1m) ~ NA_real_,
      gc_vag_bothcenters_1m == 0 & gonn_v2_1m == 0 ~ 0,
      gc_vag_bothcenters_1m == 1 | gonn_v2_1m == 1 ~ 1,
      TRUE ~ 2
    ),
    gc_vag_interim_4m = case_when(
      is.na(gc_vag_bothcenters_4m) | is.na(gonn_v2_4m) ~ NA_real_,
      gc_vag_bothcenters_4m == 0 & gonn_v2_4m == 0 ~ 0,
      gc_vag_bothcenters_4m == 1 | gonn_v2_4m == 1 ~ 1,
      TRUE ~ 2
    ),
    gc_vag_interim_8m = case_when(
      is.na(gc_vag_bothcenters_8m) | is.na(gonn_v2_8m) ~ NA_real_,
      gc_vag_bothcenters_8m == 0 & gonn_v2_8m == 0 ~ 0,
      gc_vag_bothcenters_8m == 1 | gonn_v2_8m == 1 ~ 1,
      TRUE ~ 2
    ),
    gc_vag_interim_12m = case_when(
      is.na(gc_vag_bothcenters_12m) | is.na(gonn_v2_12m) ~ NA_real_,
      gc_vag_bothcenters_12m == 0 & gonn_v2_12m == 0 ~ 0,
      gc_vag_bothcenters_12m == 1 | gonn_v2_12m == 1 ~ 1,
      TRUE ~ 2
    )
  )

# Gonorrhea f/u negative or positive

summ <- summ %>%
  mutate(
    gonn_fu = case_when(
      followup_visits_completed >= 3 &
        rowSums(cbind(
          gc_vag_interim_1m == 1,
          gc_vag_interim_4m == 1,
          gc_vag_interim_8m == 1,
          gc_vag_interim_12m == 1
        ), na.rm = TRUE) == 0 ~ 0,
      
      rowSums(cbind(
        gc_vag_interim_1m == 1,
        gc_vag_interim_4m == 1,
        gc_vag_interim_8m == 1,
        gc_vag_interim_12m == 1
      ), na.rm = TRUE) > 0 ~ 1,
      
      TRUE ~ NA_real_
    )
  )

# Gonorrhea status of sex partner(s) at enrollment and f/u

make_gc_partner_column <- function(df, event_name, var_name, source_col) {
  temp <- df[df$redcap_event_name == event_name, ]
  temp[[var_name]] <- suppressWarnings(as.numeric(temp[[source_col]]))
  out <- temp[, c("record_id", var_name)]
  out <- out[!duplicated(out$record_id), ]
  return(out)
}

# Partner diagnosed with GC at enrollment
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "enrollment_arm_1", "gc", "gc"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

# Partner treated (only meaningful if gc == 1)
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "enrollment_arm_1", "gc_tx", "gc_tx"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

# Participant treated (only meaningful if gc == 1)
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "enrollment_arm_1", "gc_self", "gc_self"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

# Partner diagnosed since last visit

summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "1_month_followup_arm_1", "gc_v2_1m", "gc_v2"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "4_month_followup_arm_1", "gc_v2_4m", "gc_v2"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "8_month_followup_arm_1", "gc_v2_8m", "gc_v2"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "12_month_followup_arm_1", "gc_v2_12m", "gc_v2"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

# Partner treated since last visit
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "1_month_followup_arm_1", "gc_tx_v2_1m", "gc_tx_v2"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "4_month_followup_arm_1", "gc_tx_v2_4m", "gc_tx_v2"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "8_month_followup_arm_1", "gc_tx_v2_8m", "gc_tx_v2"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "12_month_followup_arm_1", "gc_tx_v2_12m", "gc_tx_v2"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

#Participant treated since last visit 
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "1_month_followup_arm_1", "gc_self_v2_1m", "gc_self_v2"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "4_month_followup_arm_1", "gc_self_v2_4m", "gc_self_v2"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "8_month_followup_arm_1", "gc_self_v2_8m", "gc_self_v2"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "12_month_followup_arm_1", "gc_self_v2_12m", "gc_self_v2"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)


# == Pelvic Inflammatory Disease == #
# ------------------------------------
# PID ever diagnosed
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "enrollment_arm_1", "pelv", "pelv"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)

# Number of PID episodes
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "enrollment_arm_1", "numtimes_pid", "numtimes_pid"),
  by.x="PTID", by.y="record_id", all.x=TRUE
)



# == Bacterial vaginosis == #
# ---------------------------

# Enrollment [bv]
summ <- merge(summ, make_gc_partner_column(REDCAP, "enrollment_arm_1", "bv", "bv"),
              by.x="PTID", by.y="record_id", all.x=TRUE)

# Follow-up bv
summ <- merge(summ, make_gc_partner_column(REDCAP, "1_month_followup_arm_1", "bv_v2_1m", "bv_v2"),
              by.x="PTID", by.y="record_id", all.x=TRUE)
summ <- merge(summ, make_gc_partner_column(REDCAP, "4_month_followup_arm_1", "bv_v2_4m", "bv_v2"),
              by.x="PTID", by.y="record_id", all.x=TRUE)
summ <- merge(summ, make_gc_partner_column(REDCAP, "8_month_followup_arm_1", "bv_v2_8m", "bv_v2"),
              by.x="PTID", by.y="record_id", all.x=TRUE)
summ <- merge(summ, make_gc_partner_column(REDCAP, "12_month_followup_arm_1", "bv_v2_12m", "bv_v2"),
              by.x="PTID", by.y="record_id", all.x=TRUE)

# BV gram stain [gs_vag]

summ <- merge(summ, make_gc_partner_column(REDCAP, "enrollment_arm_1", "gs_vag_en", "gs_vag"),
              by.x="PTID", by.y="record_id", all.x=TRUE)
summ <- merge(summ, make_gc_partner_column(REDCAP, "1_month_followup_arm_1", "gs_vag_1m", "gs_vag"),
              by.x="PTID", by.y="record_id", all.x=TRUE)
summ <- merge(summ, make_gc_partner_column(REDCAP, "4_month_followup_arm_1", "gs_vag_4m", "gs_vag"),
              by.x="PTID", by.y="record_id", all.x=TRUE)
summ <- merge(summ, make_gc_partner_column(REDCAP, "8_month_followup_arm_1", "gs_vag_8m", "gs_vag"),
              by.x="PTID", by.y="record_id", all.x=TRUE)
summ <- merge(summ, make_gc_partner_column(REDCAP, "12_month_followup_arm_1", "gs_vag_12m", "gs_vag"),
              by.x="PTID", by.y="record_id", all.x=TRUE)


# == Yeast infection == #
# -----------------------

summ <- merge(summ, make_gc_partner_column(REDCAP, "enrollment_arm_1", "yeast", "yeast"),
              by.x="PTID", by.y="record_id", all.x=TRUE)

summ <- merge(summ, make_gc_partner_column(REDCAP, "1_month_followup_arm_1", "yeast_v2_1m", "yeast_v2"),
              by.x="PTID", by.y="record_id", all.x=TRUE)
summ <- merge(summ, make_gc_partner_column(REDCAP, "4_month_followup_arm_1", "yeast_v2_4m", "yeast_v2"),
              by.x="PTID", by.y="record_id", all.x=TRUE)
summ <- merge(summ, make_gc_partner_column(REDCAP, "8_month_followup_arm_1", "yeast_v2_8m", "yeast_v2"),
              by.x="PTID", by.y="record_id", all.x=TRUE)
summ <- merge(summ, make_gc_partner_column(REDCAP, "12_month_followup_arm_1", "yeast_v2_12m", "yeast_v2"),
              by.x="PTID", by.y="record_id", all.x=TRUE)

# == Genital herpes == #
# ----------------------

summ <- merge(summ, make_gc_partner_column(REDCAP, "enrollment_arm_1", "ghsv", "ghsv"),
              by.x="PTID", by.y="record_id", all.x=TRUE)

summ <- merge(summ, make_gc_partner_column(REDCAP, "1_month_followup_arm_1", "ghsv_v2_1m", "ghsv_v2"),
              by.x="PTID", by.y="record_id", all.x=TRUE)
summ <- merge(summ, make_gc_partner_column(REDCAP, "4_month_followup_arm_1", "ghsv_v2_4m", "ghsv_v2"),
              by.x="PTID", by.y="record_id", all.x=TRUE)
summ <- merge(summ, make_gc_partner_column(REDCAP, "8_month_followup_arm_1", "ghsv_v2_8m", "ghsv_v2"),
              by.x="PTID", by.y="record_id", all.x=TRUE)
summ <- merge(summ, make_gc_partner_column(REDCAP, "12_month_followup_arm_1", "ghsv_v2_12m", "ghsv_v2"),
              by.x="PTID", by.y="record_id", all.x=TRUE)


# == Genital warts == #
# ---------------------

summ <- merge(summ, make_gc_partner_column(REDCAP, "enrollment_arm_1", "ghv", "ghv"),
              by.x="PTID", by.y="record_id", all.x=TRUE)


# == Abnormal pap smear == #
# -----------------------

summ <- merge(summ, make_gc_partner_column(REDCAP, "enrollment_arm_1", "pap", "pap"),
              by.x="PTID", by.y="record_id", all.x=TRUE)

summ <- merge(summ, make_gc_partner_column(REDCAP, "1_month_followup_arm_1", "pap_v2_1m", "pap_v2"),
              by.x="PTID", by.y="record_id", all.x=TRUE)
summ <- merge(summ, make_gc_partner_column(REDCAP, "4_month_followup_arm_1", "pap_v2_4m", "pap_v2"),
              by.x="PTID", by.y="record_id", all.x=TRUE)
summ <- merge(summ, make_gc_partner_column(REDCAP, "8_month_followup_arm_1", "pap_v2_8m", "pap_v2"),
              by.x="PTID", by.y="record_id", all.x=TRUE)
summ <- merge(summ, make_gc_partner_column(REDCAP, "12_month_followup_arm_1", "pap_v2_12m", "pap_v2"),
              by.x="PTID", by.y="record_id", all.x=TRUE)



# == Nongonococcal Urethritis == #
# --------------------------------

#No sex partners were diagnosed with NGU at enrollment nor follow-up, EXCEPT for participant 533 with a partner diagnosed at 4 months follow-up.


# NGU at enrollment – yes/no
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "enrollment_arm_1", "ngu", "ngu"),
  by.x = "PTID", by.y = "record_id", all.x = TRUE
)

# NGU partner treatment at enrollment
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "enrollment_arm_1", "ngu_prt_tx", "ngu_prt_tx"),
  by.x = "PTID", by.y = "record_id", all.x = TRUE
)

# NGU at 1m – yes/no
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "1_month_followup_arm_1", "ngu_v2_1m", "ngu_v2"),
  by.x = "PTID", by.y = "record_id", all.x = TRUE
)

# NGU at 1m – partner treatment
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "1_month_followup_arm_1", "ngu_prt_tx_v2_1m", "ngu_prt_tx_v2"),
  by.x = "PTID", by.y = "record_id", all.x = TRUE
)

# NGU at 4m – yes/no
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "4_month_followup_arm_1", "ngu_v2_4m", "ngu_v2"),
  by.x = "PTID", by.y = "record_id", all.x = TRUE
)

# NGU at 4m – partner treatment
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "4_month_followup_arm_1", "ngu_prt_tx_v2_4m", "ngu_prt_tx_v2"),
  by.x = "PTID", by.y = "record_id", all.x = TRUE
)

# NGU at 8m – yes/no
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "8_month_followup_arm_1", "ngu_v2_8m", "ngu_v2"),
  by.x = "PTID", by.y = "record_id", all.x = TRUE
)

# NGU at 8m – partner treatment
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "8_month_followup_arm_1", "ngu_prt_tx_v2_8m", "ngu_prt_tx_v2"),
  by.x = "PTID", by.y = "record_id", all.x = TRUE
)

# NGU at 12m – yes/no
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "12_month_followup_arm_1", "ngu_v2_12m", "ngu_v2"),
  by.x = "PTID", by.y = "record_id", all.x = TRUE
)

# NGU at 12m – partner treatment
summ <- merge(
  summ,
  make_gc_partner_column(REDCAP, "12_month_followup_arm_1", "ngu_prt_tx_v2_12m", "ngu_prt_tx_v2"),
  by.x = "PTID", by.y = "record_id", all.x = TRUE
)


#==========================================================#
# == Contraception / pregnancy prevention == # 
#==========================================================#

#hormonal multi select at enrollment
make_bc_hormonal <- function(df) {
  temp <- df[df$redcap_event_name == "enrollment_arm_1", ]
  
  out_list <- vector("list", nrow(temp))
  
  for (i in seq_len(nrow(temp))) {
    row <- temp[i, ]
    methods <- c()
    
    if (isTRUE(row$bc_hormone___1 == 1)) methods <- c(methods, "IUD (Mirena/Skyla)")
    if (isTRUE(row$bc_hormone___2 == 1)) methods <- c(methods, "Oral contraceptive pill (OCP) or Nuvaring")
    if (isTRUE(row$bc_hormone___3 == 1)) methods <- c(methods, "Depo or Nexplanon/implant")
    if (isTRUE(row$bc_hormone___4 == 1)) methods <- c(methods, "Depo or Nexplanon/implant")
    if (isTRUE(row$bc_hormone___5 == 1)) methods <- c(methods, "Oral contraceptive pill (OCP) or Nuvaring")
    if (isTRUE(row$bc_hormone___6 == 1)) methods <- c(methods, "Plan B/emergency contraception")
    
    # Special bc_hormone___7 logic
    if (isTRUE(row$bc_hormone___7 == 1) && row$bc_other == "Medical abortion")
      methods <- c(methods, "Plan B/emergency contraception") #PTID 569 had "medical abortion" written for other, team agreed to group into "Plan B.."
    
    if (isTRUE(row$bc_hormone___7 == 1) && row$bc_other == "Xulane patch")
      methods <- c(methods, "Depo or Nexplanon/implant") #PTID 611 had "xulane patch" written for other, team agreed to group into "Depo.."
    
    
    # bc_hormone___8 + bc_pregprev___14 is selected, then they are truly none 
    if (isTRUE(row$bc_hormone___8 == 1) && isTRUE(row$bc_pregprev___14 == 1))
      methods <- c(methods, "None")
    
    #return NA instead of blank 
    if (length(methods) == 0) {
      out_list[[i]] <- NA_character_
    } else {
      out_list[[i]] <- paste(methods, collapse = ", ")
    }
  }
  
  out <- data.frame(
    record_id = temp$record_id,
    bc_hormone_en = unlist(out_list), #variable for enrollment created here 
    stringsAsFactors = FALSE
  )
  
  out <- out[!duplicated(out$record_id), ]
  return(out)
}

summ <- merge(
  summ,
  make_bc_hormonal(REDCAP),
  by.x = "PTID", by.y = "record_id", all.x = TRUE
)


# hormonal multi select af f/u


make_bc_hormonal <- function(df, event_name, out_var) {
  temp <- df[df$redcap_event_name == event_name, ]
  
  if (nrow(temp) == 0) {
    return(data.frame(record_id = character(0), 
                      dummy = character(0),
                      stringsAsFactors = FALSE)[, 0])
  }
  
  selections <- lapply(seq_len(nrow(temp)), function(i) {
    row <- temp[i, ]
    
    out <- character(0)
    
    if (isTRUE(row$bc_hormone_v2___1 == 1)) out <- c(out, "IUD (Mirena/Skyla)")
    if (isTRUE(row$bc_hormone_v2___2 == 1)) out <- c(out, "Oral contraceptive pill (OCP) or Nuvaring")
    if (isTRUE(row$bc_hormone_v2___3 == 1)) out <- c(out, "Depo or Nexplanon/implant")
    if (isTRUE(row$bc_hormone_v2___4 == 1)) out <- c(out, "Depo or Nexplanon/implant")
    if (isTRUE(row$bc_hormone_v2___5 == 1)) out <- c(out, "Oral contraceptive pill (OCP) or Nuvaring")
    if (isTRUE(row$bc_hormone_v2___6 == 1)) out <- c(out, "Plan B/emergency contraception")
    
    # Special "None" rule
    if (isTRUE(row$bc_hormone_v2___8 == 1) && isTRUE(row$bc_pregprev_v2___14 == 1)) {
      out <- "None"
    }
    
    #explicitly set missing to NA 
    if (length(out) == 0) {
      NA_character_
    } else if (length(out) == 1) {
      out
    } else {
      paste(out, collapse = ", ") # collapse into comma-separated string
    }
  })
  
  out_vec <- unlist(selections)
  
  out_df <- data.frame(
    record_id = temp$record_id,
    value = out_vec,
    stringsAsFactors = FALSE
  )
  
  # rename "value" to desired output var
  names(out_df)[names(out_df) == "value"] <- out_var
  
  out_df <- out_df[!duplicated(out_df$record_id), ]
  return(out_df)
}

summ <- merge(
  summ,
  make_bc_hormonal(REDCAP, "1_month_followup_arm_1", "bc_hormone_1m"),
  by.x = "PTID", by.y = "record_id",
  all.x = TRUE
)

summ <- merge(
  summ,
  make_bc_hormonal(REDCAP, "4_month_followup_arm_1", "bc_hormone_4m"),
  by.x = "PTID", by.y = "record_id",
  all.x = TRUE
)

summ <- merge(
  summ,
  make_bc_hormonal(REDCAP, "8_month_followup_arm_1", "bc_hormone_8m"),
  by.x = "PTID", by.y = "record_id",
  all.x = TRUE
)

summ <- merge(
  summ,
  make_bc_hormonal(REDCAP, "12_month_followup_arm_1", "bc_hormone_12m"),
  by.x = "PTID", by.y = "record_id",
  all.x = TRUE)

#  pregnancy prevention multi select at enrollment
# ==================================================



make_bc_nonhormonal <- function(df) {
  temp <- df[df$redcap_event_name == "enrollment_arm_1", ]
  
  out_list <- vector("list", nrow(temp))
  
  for (i in seq_len(nrow(temp))) {
    row <- temp[i, ]
    methods <- c()
    
    if (isTRUE(row$bc_pregprev___1 == 1))
      methods <- c(methods, "Copper IUD (Paragard)")
    if (isTRUE(row$bc_pregprev___2 == 1))
      methods <- c(methods, "Male condoms or female condoms")
    if (isTRUE(row$bc_pregprev___3 == 1))
      methods <- c(methods, "Abstinence")
    if (isTRUE(row$bc_pregprev___4 == 1))
      methods <- c(methods, "Same sex partner")
    if (isTRUE(row$bc_pregprev___5 == 1))
      methods <- c(methods, "Spermicide alone")
    if (isTRUE(row$bc_pregprev___6 == 1))
      methods <- c(methods, "Diaphragm")
    if (isTRUE(row$bc_pregprev___7 == 1))
      methods <- c(methods, "Male condoms or female condoms")
    if (isTRUE(row$bc_pregprev___8 == 1))
      methods <- c(methods, "Withdrawal or rhythm method")
    if (isTRUE(row$bc_pregprev___9 == 1))
      methods <- c(methods, "BTL, Essure/sterilization")
    if (isTRUE(row$bc_pregprev___10 == 1))
      methods <- c(methods, "Sex partner who has vasectomy")
    if (isTRUE(row$bc_pregprev___11 == 1))
      methods <- c(methods, "Menopausal")
    if (isTRUE(row$bc_pregprev___12 == 1))
      methods <- c(methods, "Hysterectomy")
    if (isTRUE(row$bc_pregprev___13 == 1)) {
      # Only PTID 602 selected "Other"and stated "Anal sex" in redacted data
      # Anyone else who selects "Other" in theory gets the generic label "Other" so they're never misattributed anal sex they didn't report.
      if (identical(as.character(row$record_id), "602")) {
        methods <- c(methods, "Anal sex")   # PTID 602 specifically
      } else {
        methods <- c(methods, "Other")
      }
    }
    if (isTRUE(row$bc_pregprev___14 == 1))
      methods <- c(methods, "None")   # explicit "none of the above", distinct from NA (no response)
    
    methods <- unique(methods)   # de-dupe (e.g. Male + Female condoms both selected)
    
    #missing responses (nothing selected at all) should be marked NA
    if (length(methods) == 0) {
      out_list[[i]] <- NA_character_
    } else {
      out_list[[i]] <- paste(methods, collapse = ", ") # collapse into comma-separated string
    }
  }
  
  out <- data.frame(
    record_id = temp$record_id,
    bc_pregprev_en = unlist(out_list), #created variable here
    stringsAsFactors = FALSE
  )
  
  out <- out[!duplicated(out$record_id), ]
  return(out)
}

summ <- merge(
  summ,
  make_bc_nonhormonal(REDCAP),
  by.x = "PTID", by.y = "record_id",
  all.x = TRUE
)

# ---- FOLLOW-UP (Q460, bc_pregprev_v2) ----

make_bc_nonhormonal <- function(df, event_name, out_var) {
  temp <- df[df$redcap_event_name == event_name, ]
  
  if (nrow(temp) == 0) {
    return(data.frame(record_id = character(0), stringsAsFactors = FALSE))
  }
  
  selections <- lapply(seq_len(nrow(temp)), function(i) {
    row <- temp[i, ]
    out <- character(0)
    
    if (isTRUE(row$bc_pregprev_v2___1 == 1))  out <- c(out, "Copper IUD (Paragard)")
    if (isTRUE(row$bc_pregprev_v2___2 == 1))  out <- c(out, "Male condoms or female condoms")
    if (isTRUE(row$bc_pregprev_v2___3 == 1))  out <- c(out, "Abstinence")
    if (isTRUE(row$bc_pregprev_v2___4 == 1))  out <- c(out, "Same sex partner")
    if (isTRUE(row$bc_pregprev_v2___5 == 1))  out <- c(out, "Spermicide alone")
    if (isTRUE(row$bc_pregprev_v2___6 == 1))  out <- c(out, "Diaphragm")
    if (isTRUE(row$bc_pregprev_v2___7 == 1))  out <- c(out, "Male condoms or female condoms")
    if (isTRUE(row$bc_pregprev_v2___8 == 1))  out <- c(out, "Withdrawal or rhythm method")
    if (isTRUE(row$bc_pregprev_v2___9 == 1))  out <- c(out, "BTL, Essure/sterilization")
    if (isTRUE(row$bc_pregprev_v2___10 == 1)) out <- c(out, "Sex partner who has vasectomy")
    if (isTRUE(row$bc_pregprev_v2___11 == 1)) out <- c(out, "Menopausal")
    if (isTRUE(row$bc_pregprev_v2___12 == 1)) out <- c(out, "Hysterectomy")
    if (isTRUE(row$bc_pregprev_v2___13 == 1)) out <- c(out, "Other")   # nobody selected this at f/u 
    if (isTRUE(row$bc_pregprev_v2___14 == 1)) out <- c(out, "None")    # explicit "none of the above", distinct from NA (no response)
    
    out <- unique(out)   # de-dupe (e.g. Male + Female condoms both selected)
    
    if (length(out) == 0) {
      NA_character_
    } else if (length(out) == 1) {
      out
    } else {
      paste(out, collapse = ", ")
    }
  })
  
  out_vec <- unlist(selections)
  
  out_df <- data.frame(
    record_id = temp$record_id,
    value = out_vec,
    stringsAsFactors = FALSE
  )
  
  names(out_df)[names(out_df) == "value"] <- out_var
  
  out_df <- out_df[!duplicated(out_df$record_id), ]
  return(out_df)
}
  
# 1-month
summ <- merge(
  summ,
  make_bc_nonhormonal(REDCAP, "1_month_followup_arm_1", "bc_pregprev_1m"),
  by.x = "PTID", by.y = "record_id",
  all.x = TRUE
)

# 4-month
summ <- merge(
  summ,
  make_bc_nonhormonal(REDCAP, "4_month_followup_arm_1", "bc_pregprev_4m"),
  by.x = "PTID", by.y = "record_id",
  all.x = TRUE
)

# 8-month
summ <- merge(
  summ,
  make_bc_nonhormonal(REDCAP, "8_month_followup_arm_1", "bc_pregprev_8m"),
  by.x = "PTID", by.y = "record_id",
  all.x = TRUE
)

# 12-month
summ <- merge(
  summ,
  make_bc_nonhormonal(REDCAP, "12_month_followup_arm_1", "bc_pregprev_12m"),
  by.x = "PTID", by.y = "record_id",
  all.x = TRUE
)
  
  #condom frequency at f/u (CORRECT)

summ <- summ[ , !grepl("^condom_freq", names(summ)) ]
  
make_condom_freq <- function(df, event_name, redcap_var, out_var) {
  temp <- df[df$redcap_event_name == event_name, ]
  
  if (nrow(temp) == 0) {
    return(data.frame(record_id = character(0), stringsAsFactors = FALSE))
  }
  
  # If the variable does NOT exist, create an all-NA vector
  if (!redcap_var %in% names(temp)) {
    out_df <- data.frame(
      record_id = temp$record_id,
      value = NA,
      stringsAsFactors = FALSE
    )
  } else {
    out_df <- data.frame(
      record_id = temp$record_id,
      value = temp[[redcap_var]],
      stringsAsFactors = FALSE
    )
  }
  
  names(out_df)[names(out_df) == "value"] <- out_var
  out_df <- out_df[!duplicated(out_df$record_id), ]
  return(out_df)
}

# Enrollment
summ <- merge(
  summ,
  make_condom_freq(REDCAP, "enrollment_arm_1", "condom_freq", "condom_freq"),
  by.x = "PTID", by.y = "record_id",
  all.x = TRUE
)
  
# 1-month
summ <- merge(
  summ,
  make_condom_freq(REDCAP, "1_month_followup_arm_1", "condom_freq_v2", "condom_freq_v2_1m"),
  by.x = "PTID", by.y = "record_id",
  all.x = TRUE
)

# 4-month
summ <- merge(
  summ,
  make_condom_freq(REDCAP, "4_month_followup_arm_1", "condom_freq_v2", "condom_freq_v2_4m"),
  by.x = "PTID", by.y = "record_id",
  all.x = TRUE
)

# 8-month
summ <- merge(
  summ,
  make_condom_freq(REDCAP, "8_month_followup_arm_1", "condom_freq_v2", "condom_freq_v2_8m"),
  by.x = "PTID", by.y = "record_id",
  all.x = TRUE
)

# 12-month
summ <- merge(
  summ,
  make_condom_freq(REDCAP, "12_month_followup_arm_1", "condom_freq_v2", "condom_freq_v2_12m"),
  by.x = "PTID", by.y = "record_id",
  all.x = TRUE
)
  
  

# == Sexual history == # 
  
# Do you or have you had sex with men, women, or both + since your last visit 
  
  # Remove ANY existing sexmenwmb columns before merging
  summ <- summ[ , !grepl("^sexmenwmb", names(summ)) ]
  

  make_single_var <- function(df, event_name, redcap_var, out_var) {
    temp <- df[df$redcap_event_name == event_name, ]
    
    if (nrow(temp) == 0) {
      return(data.frame(record_id = character(0), stringsAsFactors = FALSE))
    }
    
    out_df <- data.frame(
      record_id = temp$record_id,
      value = temp[[redcap_var]],
      stringsAsFactors = FALSE
    )
    
    names(out_df)[names(out_df) == "value"] <- out_var
    out_df <- out_df[!duplicated(out_df$record_id), ]
    return(out_df)
  }
  
  summ <- merge(
    summ,
    make_single_var(REDCAP, "enrollment_arm_1", "sexmenwmb", "sexmenwmb"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  summ <- merge(
    summ,
    make_single_var(REDCAP, "1_month_followup_arm_1", "sexmenwmb_v2", "sexmenwmb_v2_1m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  summ <- merge(
    summ,
    make_single_var(REDCAP, "4_month_followup_arm_1", "sexmenwmb_v2", "sexmenwmb_v2_4m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  summ <- merge(
    summ,
    make_single_var(REDCAP, "8_month_followup_arm_1", "sexmenwmb_v2", "sexmenwmb_v2_8m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  summ <- merge(
    summ,
    make_single_var(REDCAP, "12_month_followup_arm_1", "sexmenwmb_v2", "sexmenwmb_v2_12m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # How many male partners have you had since ...at enrollment
  
  summ <- summ[ , !grepl("^ma_", names(summ)) ]
  
   # ma_30
  summ <- merge(
    summ,
    make_single_var(REDCAP, "enrollment_arm_1", "ma_30", "ma_30"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # ma_3
  summ <- merge(
    summ,
    make_single_var(REDCAP, "enrollment_arm_1", "ma_3", "ma_3"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # ma_12
  summ <- merge(
    summ,
    make_single_var(REDCAP, "enrollment_arm_1", "ma_12", "ma_12"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # ma_lifet
  summ <- merge(
    summ,
    make_single_var(REDCAP, "enrollment_arm_1", "ma_lifet", "ma_lifet"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # How many male partners have you had since your last visit
  # Clean out any old ma_slv_v2_* columns
 
  summ <- summ[ , !grepl("^ma_slv_v2_", names(summ)) ]
  
  
  # 1-month follow-up
  summ <- merge(
    summ,
    make_single_var(REDCAP, "1_month_followup_arm_1", "ma_slv_v2", "ma_slv_v2_1m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 4-month follow-up
  summ <- merge(
    summ,
    make_single_var(REDCAP, "4_month_followup_arm_1", "ma_slv_v2", "ma_slv_v2_4m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 8-month follow-up
  summ <- merge(
    summ,
    make_single_var(REDCAP, "8_month_followup_arm_1", "ma_slv_v2", "ma_slv_v2_8m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 12-month follow-up
  summ <- merge(
    summ,
    make_single_var(REDCAP, "12_month_followup_arm_1", "ma_slv_v2", "ma_slv_v2_12m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
# condom use last vs (enrollment & fu)
  
  summ <- summ[ , !grepl("^conlastsex", names(summ)) ]
  
  summ <- merge(
    summ,
    make_single_var(REDCAP, "enrollment_arm_1", "conlastsex", "conlastsex"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  

  # 1-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "1_month_followup_arm_1", "conlastsex_v2", "conlastsex_v2_1m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 4-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "4_month_followup_arm_1", "conlastsex_v2", "conlastsex_v2_4m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 8-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "8_month_followup_arm_1", "conlastsex_v2", "conlastsex_v2_8m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 12-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "12_month_followup_arm_1", "conlastsex_v2", "conlastsex_v2_12m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # How many NEW male partners have you had sex with (en) ...and since last visit (fu)
  
  summ <- summ[ , !grepl("^newmen", names(summ)) ]
  
   # newmen30d
  summ <- merge(
    summ,
    make_single_var(REDCAP, "enrollment_arm_1", "newmen30d", "newmen30d"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # newmen12m
  summ <- merge(
    summ,
    make_single_var(REDCAP, "enrollment_arm_1", "newmen12m", "newmen12m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 1-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "1_month_followup_arm_1", "newmen30d_v2", "newmen30d_v2_1m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 4-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "4_month_followup_arm_1", "newmen30d_v2", "newmen30d_v2_4m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 8-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "8_month_followup_arm_1", "newmen30d_v2", "newmen30d_v2_8m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 12-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "12_month_followup_arm_1", "newmen30d_v2", "newmen30d_v2_12m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # How many female partners have you had since.. (en) .. and since last visit (fu)
  
  summ <- summ[ , !grepl("^fsx", names(summ)) ]
  

  # fsx30d
  summ <- merge(
    summ,
    make_single_var(REDCAP, "enrollment_arm_1", "fsx30d", "fsx30d"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # fsx3mo
  summ <- merge(
    summ,
    make_single_var(REDCAP, "enrollment_arm_1", "fsx3mo", "fsx3mo"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # fsx12m
  summ <- merge(
    summ,
    make_single_var(REDCAP, "enrollment_arm_1", "fsx12m", "fsx12m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # fsxlife
  summ <- merge(
    summ,
    make_single_var(REDCAP, "enrollment_arm_1", "fsxlife", "fsxlife"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
    # 1-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "1_month_followup_arm_1", "fsx_slv_v2", "fsx_slv_v2_1m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 4-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "4_month_followup_arm_1", "fsx_slv_v2", "fsx_slv_v2_4m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 8-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "8_month_followup_arm_1", "fsx_slv_v2", "fsx_slv_v2_8m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 12-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "12_month_followup_arm_1", "fsx_slv_v2", "fsx_slv_v2_12m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # How many NEW female partners have you had sex with since... (en) ... and since last visist (fu)
  
  summ <- summ[ , !grepl("^newwomen", names(summ)) ]
  
   # newwomen30d
  summ <- merge(
    summ,
    make_single_var(REDCAP, "enrollment_arm_1", "newwomen30d", "newwomen30d"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # newwomen12m
  summ <- merge(
    summ,
    make_single_var(REDCAP, "enrollment_arm_1", "newwomen12m", "newwomen12m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )

  # 1-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "1_month_followup_arm_1", "newwomen30d_v2", "newwomen30d_v2_1m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 4-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "4_month_followup_arm_1", "newwomen30d_v2", "newwomen30d_v2_4m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 8-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "8_month_followup_arm_1", "newwomen30d_v2", "newwomen30d_v2_8m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 12-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "12_month_followup_arm_1", "newwomen30d_v2", "newwomen30d_v2_12m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  #received oral sex
  
  summ <- summ[ , !grepl("^oral_sex", names(summ)) ]
  
  summ <- merge(
    summ,
    make_single_var(REDCAP, "enrollment_arm_1", "oral_sex", "oral_sex"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 1-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "1_month_followup_arm_1", "oral_sex_v2", "oral_sex_v2_1m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 4-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "4_month_followup_arm_1", "oral_sex_v2", "oral_sex_v2_4m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 8-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "8_month_followup_arm_1", "oral_sex_v2", "oral_sex_v2_8m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 12-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "12_month_followup_arm_1", "oral_sex_v2", "oral_sex_v2_12m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  #uncircumcised partner
  
  summ <- summ[ , !grepl("^ma_uncirc", names(summ)) ]
  
  summ <- merge(
    summ,
    make_single_var(REDCAP, "enrollment_arm_1", "ma_uncirc", "ma_uncirc"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 1-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "1_month_followup_arm_1", "ma_uncirc_v2", "ma_uncirc_v2_1m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 4-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "4_month_followup_arm_1", "ma_uncirc_v2", "ma_uncirc_v2_4m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 8-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "8_month_followup_arm_1", "ma_uncirc_v2", "ma_uncirc_v2_8m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 12-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "12_month_followup_arm_1", "ma_uncirc_v2", "ma_uncirc_v2_12m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  
#anal sex
  summ <- summ[ , !grepl("^anal_sex", names(summ)) ]
  
  summ <- merge(
    summ,
    make_single_var(REDCAP, "enrollment_arm_1", "anal_sex", "anal_sex"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  
  # 1-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "1_month_followup_arm_1", "anal_sex_v2", "anal_sex_v2_1m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 4-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "4_month_followup_arm_1", "anal_sex_v2", "anal_sex_v2_4m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 8-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "8_month_followup_arm_1", "anal_sex_v2", "anal_sex_v2_8m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 12-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "12_month_followup_arm_1", "anal_sex_v2", "anal_sex_v2_12m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # vaginal sex after rectal sex ? did you use condom? ---------
  
  # Clean out any old vagafrec* or conrecvag* columns
  
  summ <- summ[ , !grepl("^vagafrec", names(summ)) ]
  summ <- summ[ , !grepl("^conrecvag", names(summ)) ]
  

  # Enrollment: vagafrec

    summ <- merge(
    summ,
    make_single_var(REDCAP, "enrollment_arm_1", "vagafrec", "vagafrec"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
   # Follow-up: vagafrec_v2_* (leave NA as NA)

  # 1-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "1_month_followup_arm_1", "vagafrec_v2", "vagafrec_v2_1m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 4-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "4_month_followup_arm_1", "vagafrec_v2", "vagafrec_v2_4m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 8-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "8_month_followup_arm_1", "vagafrec_v2", "vagafrec_v2_8m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 12-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "12_month_followup_arm_1", "vagafrec_v2", "vagafrec_v2_12m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
 
  #  conrecvag
  
  
  summ <- merge(
    summ,
    make_single_var(REDCAP, "enrollment_arm_1", "conrecvag", "conrecvag"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # Follow-up: conrecvag_v2_* (leave NA as NA) -----
  
  # 1-month
  summ <- merge(
    summ,
    make_single_var(REDCAP, "1_month_followup_arm_1", "conrecvag_v2", "conrecvag_v2_1m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 4-month  (correct event name)
  summ <- merge(
    summ,
    make_single_var(REDCAP, "4_month_followup_arm_1", "conrecvag_v2", "conrecvag_v2_4m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 8-month  (correct event name)
  summ <- merge(
    summ,
    make_single_var(REDCAP, "8_month_followup_arm_1", "conrecvag_v2", "conrecvag_v2_8m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # 12-month  (correct event name)
  summ <- merge(
    summ,
    make_single_var(REDCAP, "12_month_followup_arm_1", "conrecvag_v2", "conrecvag_v2_12m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # sex30mens - had sex during menstruation 
  # bleed30sex - had bleeding with sex / intercourse
  # mid30spot - had bleeding while you were not on your period
  # pain30sex - had pelvic pain while you were having sex
  # plv30reg - had pelvic pain unrelated to sex 
  
  
  ###############################################
  # Clean out any old columns for these 
  ###############################################
  
  summ <- summ[ , !grepl("^sex30mens", names(summ)) ]
  summ <- summ[ , !grepl("^bleed30sex", names(summ)) ]
  summ <- summ[ , !grepl("^mid30spot", names(summ)) ]
  summ <- summ[ , !grepl("^pain30sex", names(summ)) ]
  summ <- summ[ , !grepl("^plv30reg", names(summ)) ]
  
  ###############################################
  # Enrollment variables
  ###############################################
  
  # sex30mens
  summ <- merge(
    summ,
    make_single_var(REDCAP, "enrollment_arm_1", "sex30mens", "sex30mens"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # bleed30sex
  summ <- merge(
    summ,
    make_single_var(REDCAP, "enrollment_arm_1", "bleed30sex", "bleed30sex"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # mid30spot
  summ <- merge(
    summ,
    make_single_var(REDCAP, "enrollment_arm_1", "mid30spot", "mid30spot"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # pain30sex
  summ <- merge(
    summ,
    make_single_var(REDCAP, "enrollment_arm_1", "pain30sex", "pain30sex"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  # plv30reg
  summ <- merge(
    summ,
    make_single_var(REDCAP, "enrollment_arm_1", "plv30reg", "plv30reg"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  ###############################################
  # Follow-up variables (v2)
  ###############################################
  
  ### sex30mens_v2
  summ <- merge(
    summ,
    make_single_var(REDCAP, "1_month_followup_arm_1", "sex30mens_v2", "sex30mens_v2_1m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  summ <- merge(
    summ,
    make_single_var(REDCAP, "4_month_followup_arm_1", "sex30mens_v2", "sex30mens_v2_4m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  summ <- merge(
    summ,
    make_single_var(REDCAP, "8_month_followup_arm_1", "sex30mens_v2", "sex30mens_v2_8m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  summ <- merge(
    summ,
    make_single_var(REDCAP, "12_month_followup_arm_1", "sex30mens_v2", "sex30mens_v2_12m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  ### bleed30sex_v2
  summ <- merge(
    summ,
    make_single_var(REDCAP, "1_month_followup_arm_1", "bleed30sex_v2", "bleed30sex_v2_1m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  summ <- merge(
    summ,
    make_single_var(REDCAP, "4_month_followup_arm_1", "bleed30sex_v2", "bleed30sex_v2_4m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  summ <- merge(
    summ,
    make_single_var(REDCAP, "8_month_followup_arm_1", "bleed30sex_v2", "bleed30sex_v2_8m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  summ <- merge(
    summ,
    make_single_var(REDCAP, "12_month_followup_arm_1", "bleed30sex_v2", "bleed30sex_v2_12m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  ### mid30spot_v2
  summ <- merge(
    summ,
    make_single_var(REDCAP, "1_month_followup_arm_1", "mid30spot_v2", "mid30spot_v2_1m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  summ <- merge(
    summ,
    make_single_var(REDCAP, "4_month_followup_arm_1", "mid30spot_v2", "mid30spot_v2_4m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  summ <- merge(
    summ,
    make_single_var(REDCAP, "8_month_followup_arm_1", "mid30spot_v2", "mid30spot_v2_8m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  summ <- merge(
    summ,
    make_single_var(REDCAP, "12_month_followup_arm_1", "mid30spot_v2", "mid30spot_v2_12m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  ### pain30sex_v2
  summ <- merge(
    summ,
    make_single_var(REDCAP, "1_month_followup_arm_1", "pain30sex_v2", "pain30sex_v2_1m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  summ <- merge(
    summ,
    make_single_var(REDCAP, "4_month_followup_arm_1", "pain30sex_v2", "pain30sex_v2_4m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  summ <- merge(
    summ,
    make_single_var(REDCAP, "8_month_followup_arm_1", "pain30sex_v2", "pain30sex_v2_8m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  summ <- merge(
    summ,
    make_single_var(REDCAP, "12_month_followup_arm_1", "pain30sex_v2", "pain30sex_v2_12m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  ### plv30reg_v2
  summ <- merge(
    summ,
    make_single_var(REDCAP, "1_month_followup_arm_1", "plv30reg_v2", "plv30reg_v2_1m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  summ <- merge(
    summ,
    make_single_var(REDCAP, "4_month_followup_arm_1", "plv30reg_v2", "plv30reg_v2_4m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  summ <- merge(
    summ,
    make_single_var(REDCAP, "8_month_followup_arm_1", "plv30reg_v2", "plv30reg_v2_8m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  summ <- merge(
    summ,
    make_single_var(REDCAP, "12_month_followup_arm_1", "plv30reg_v2", "plv30reg_v2_12m"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )


#douching questions - enrollment 
  

  summ <- summ[ , !grepl("^douch$", names(summ)) ]
  
  # Enrollment: douch

  summ <- merge(
    summ,
    make_single_var(REDCAP, "enrollment_arm_1", "douch", "douch"),
    by.x = "PTID", by.y = "record_id",
    all.x = TRUE
  )
  
  #remove pre merged douch vars
  summ <- summ[ , !grepl("^freqdche", names(summ)) ]

#how often do you douche
make_freqdche <- function(df) {
  temp <- df[df$redcap_event_name == "enrollment_arm_1", ]
  
  out <- rep(NA_character_, nrow(temp))
  
  out[temp$freqdche___1 == 1] <- "1"
  out[temp$freqdche___2 == 1] <- "2"
  out[temp$freqdche___3 == 1] <- "3"
  out[temp$freqdche___4 == 1] <- "4"
  
  out_df <- data.frame(
    record_id = temp$record_id,
    freqdche = out,
    stringsAsFactors = FALSE
  )
  
  out_df <- out_df[!duplicated(out_df$record_id), ]
  return(out_df)
}

summ <- merge(
  summ,
  make_freqdche(REDCAP),
  by.x = "PTID", by.y = "record_id",
  all.x = TRUE
)

#why do you douche

make_whydche <- function(df) {
  temp <- df[df$redcap_event_name == "enrollment_arm_1", ]
  
  out <- rep(NA_character_, nrow(temp))
  
  out[temp$whydche___1 == 1] <- "1"
  out[temp$whydche___2 == 1] <- "2"
  out[temp$whydche___3 == 1] <- "3"
  out[temp$whydche___4 == 1] <- "4"
  out[temp$whydche___5 == 1] <- "5"
  
  out_df <- data.frame(
    record_id = temp$record_id,
    whydche = out,
    stringsAsFactors = FALSE
  )
  
  out_df <- out_df[!duplicated(out_df$record_id), ]
  return(out_df)
}

summ <- summ[ , !grepl("^whydche$", names(summ)) ]

summ <- merge(
  summ,
  make_whydche(REDCAP),
  by.x = "PTID", by.y = "record_id",
  all.x = TRUE
)

summ$freqdche[ !(summ$douch %in% c(1,2)) ] <- NA
summ$whydche[ !(summ$douch %in% c(1,2)) ] <- NA



# ----------------------------------------------------------------
# STEP 9 Define column groups 
# ----------------------------------------------------------------
demo_cols       <- c("age","gender","race","hispeth","education","marital","insurance", "sex_pref")
preg_cols       <- c("everpreg","everpreg_v2_1m","everpreg_v2_4m","everpreg_v2_8m","everpreg_v2_12m", "livebirths","miscarriage","abortions",
               "numpregs","tubal_preg","blockedtubes", "blockedtubes_v2_1m", "blockedtubes_v2_4m","blockedtubes_v2_8m","blockedtubes_v2_12m",
               "chronplvpain", "chronplvpain_v2_1m", "chronplvpain_v2_4m","chronplvpain_v2_8m","chronplvpain_v2_12m",
               "plvc_surgery", "plvc_surgery_v2_1m", "plvc_surgery_v2_4m", "plvc_surgery_v2_8m", "plvc_surgery_v2_12m")
substance_cols <- c("smoker", "smoker_v2_1m", "smoker_v2_4m", "smoker_v2_8m", "smoker_v2_12m",
                    "cigs_day", "cigs_day_v2_1m", "cigs_day_v2_4m", "cigs_day_v2_8m", "cigs_day_v2_12m",
                    "vape_juul", "vape_juul_v2_1m", "vape_juul_v2_4m", "vape_juul_v2_8m", "vape_juul_v2_12m",
                    "med_marij", "med_marij_v2_1m", "med_marij_v2_4m", "med_marij_v2_8m", "med_marij_v2_12m",
                    "rec_drugsever", "rec_drugsever_v2_1m", "rec_drugsever_v2_4m", "rec_drugsever_v2_8m", "rec_drugsever_v2_12m",
                    "druguse", "druguse_v2_1m", "druguse_v2_4m", "druguse_v2_8m", "druguse_v2_12m",
                    "recdrugs_used", "recdrugs_used_v2_1m", "recdrugs_used_v2_4m", "recdrugs_used_v2_8m", "recdrugs_used_v2_12m")
                    #"other_recdrug", "other_recdrug_v2_1m", "other_recdrug_v2_4m", "other_recdrug_v2_8m", "other_recdrug_v2_12m")
alcohol_cols <-c("drink", "drink_v2_1m", "drink_v2_4m", "drink_v2_8m", "drink_v2_12m",
                 "drinks_week", "drinks_week_v2_1m", "drinks_week_v2_4m", "drinks_week_v2_8m", "drinks_week_v2_12m",
                 "drink5inaday", "drink5inaday_v2_1m", "drink5inaday_v2_4m", "drink5inaday_v2_8m", "drink5inaday_v2_12m"
                  )

urisymp_cols  <-c( "urisymptoms", "urisymptoms_v2_1m", "urisymptoms_v2_4m", "urisymptoms_v2_8m", "urisymptoms_v2_12m") 
antibiotics_30d_cols <-c("abxov30d", "abxov30d_v2_1m", "abxov30d_v2_4m", "abxov30d_v2_8m", "abxov30d_v2_12m" )
                     
pelvic_cols  <-c("nsaidplvp30d", "nsaidplvp30d_v2_1m", "nsaidplvp30d_v2_4m", "nsaidplvp30d_v2_8m", "nsaidplvp30d_v2_12m")

visit_cols      <- c("visits_present_En", "visits_present_1m", "visits_present_4m", "visits_present8m", "visits_present12m")
mwri_vag_cols   <- c("ct_vag_EN","ct_vag_1m","ct_vag_4m","ct_vag_8m","ct_vag_12m")
prsby_vag_cols  <- c("ct_vag_prsby_EN","ct_vag_prsby_1m","ct_vag_prsby_4m","ct_vag_prsby_8m","ct_vag_prsby_12m")
rectal_cols     <- c("ct_rectal_en", "ct_rectal_1m", "ct_rectal_4m", "ct_rectal_8m", "ct_rectal_12m")
genital_rectal_cols <- c("genital_rectal_1m", "genital_rectal_4m", "genital_rectal_8m", "genital_rectal_12m")
vaginal_cols    <- c("CT_vag_bothcenters_EN", "CT_vag_bothcenters_1m", "CT_vag_bothcenters_4m", "CT_vag_bothcenters_8m", "CT_vag_bothcenters_12m")
diagnostic_cols <- c("CT_CB_result", "CT_CB_burden", "CT_VS_result", "CT_VS_burden")
inferred_col_num <- c("CT_inferred_diagnostic_num")
inferred_col_char <- c("CT_inferred_diagnostic_char")
mg_cols         <- c("MG_CB_result", "MG_CB_burden", "MG_VS_result", "MG_VS_burden", "MG_inferred_diagnostic")
endm_cols       <- c("ct_endm")
chlam_report_cols <-c("chlam_en", "chlam_v2_1m", "chlam_v2_4m","chlam_v2_8m","chlam_v2_12m")
pre_chlam_cols  <- c("numtimes_ct", "daterecent_ct_days", "daterecent_ct_years")
interim_vag_cols<- c("CT_vag_interim_1m", "CT_vag_interim_4m", "CT_vag_interim_8m", "CT_vag_interim_12m")
endo_col      <- c("endo")
ct_partner_cols <- c(
  # Enrollment
  "ct", "ct_tx", "ct_self",
    # 1-month
  "ct_v2_1m", "ct_tx_v2_1m", "ct_self_v2_1m",
    # 4-month
  "ct_v2_4m", "ct_tx_v2_4m", "ct_self_v2_4m",
    # 8-month
  "ct_v2_8m", "ct_tx_v2_8m", "ct_self_v2_8m",
    # 12-month
  "ct_v2_12m", "ct_tx_v2_12m", "ct_self_v2_12m")
trich_cols      <- c("trich_En")
tx_vag_cols     <- c("tx_vag_En", "tx_vag_1m", "tx_vag_4m", "tx_vag_8m", "tx_vag_12m")
trich_v2_cols   <- c("trich_v2_1m", "trich_v2_4m", "trich_v2_8m", "trich_v2_12m")
tv_interim_cols <-c("tv_interim_1m", "tv_interim_4m","tv_interim_8m","tv_interim_12m")
tv_fu           <-c("tv_fu")
gonn_prior_cols <- c("gonn", "numtimes_gc", "daterecent_gc_days", "daterecent_gc_years")
gonn_v2_cols  <- c("gonn_v2_1m", "gonn_v2_4m", "gonn_v2_8m", "gonn_v2_12m")
gc_vag_cols <- c("gc_vag_en", "gc_vag_1m", "gc_vag_4m", "gc_vag_8m", "gc_vag_12m")
gc_vag_prsby_cols <- c("gc_vag_prsby_en", "gc_vag_prsby_1m", "gc_vag_prsby_4m",
                       "gc_vag_prsby_8m", "gc_vag_prsby_12m")
gc_vag_bothcenters_cols <- c("gc_vag_bothcenters_en", "gc_vag_bothcenters_1m",
                             "gc_vag_bothcenters_4m", "gc_vag_bothcenters_8m",
                             "gc_vag_bothcenters_12m")
gc_rectal_cols <- c("gc_rectal_en", "gc_rectal_1m", "gc_rectal_4m",
                    "gc_rectal_8m", "gc_rectal_12m")
gc_endm_col <- c("gc_endm")
gc_interim_cols <- c("gc_vag_interim_1m", "gc_vag_interim_4m",
                   "gc_vag_interim_8m", "gc_vag_interim_12m")
gc_fu_col <- c("gonn_fu")
gc_partner_cols <- c("gc", "gc_tx", "gc_self")
gc_partner_v2_cols <- c(
  "gc_v2_1m", "gc_tx_v2_1m", "gc_self_v2_1m",
  "gc_v2_4m", "gc_tx_v2_4m", "gc_self_v2_4m",
  "gc_v2_8m", "gc_tx_v2_8m", "gc_self_v2_8m",
  "gc_v2_12m", "gc_tx_v2_12m", "gc_self_v2_12m")
pid_cols <- c("pelv", "numtimes_pid", "daterecent_pid_days", "daterecent_pid_years")
bv_cols     <- c("bv")
bv_v2_cols  <- c("bv_v2_1m", "bv_v2_4m", "bv_v2_8m", "bv_v2_12m")
gs_vag_cols <- c("gs_vag_en", "gs_vag_1m", "gs_vag_4m", "gs_vag_8m", "gs_vag_12m")
yeast_cols    <- c("yeast")
yeast_v2_cols <- c("yeast_v2_1m", "yeast_v2_4m", "yeast_v2_8m", "yeast_v2_12m")
ghsv_cols    <- c("ghsv")
ghsv_v2_cols <- c("ghsv_v2_1m", "ghsv_v2_4m", "ghsv_v2_8m", "ghsv_v2_12m")
ghv_cols <- c("ghv")
pap_cols    <- c("pap")
pap_v2_cols <- c("pap_v2_1m", "pap_v2_4m", "pap_v2_8m", "pap_v2_12m")
ngu_cols <- c(
  # Enrollment
  "ngu",
  "ngu_prt_tx",
    # 1-month
  "ngu_v2_1m",
  "ngu_prt_tx_v2_1m",
    # 4-month
  "ngu_v2_4m",
  "ngu_prt_tx_v2_4m",
    # 8-month
  "ngu_v2_8m",
  "ngu_prt_tx_v2_8m",
    # 12-month
  "ngu_v2_12m",
  "ngu_prt_tx_v2_12m")
bc_cols <- c("bc_hormone_en", "bc_hormone_1m", "bc_hormone_4m", "bc_hormone_8m", "bc_hormone_12m", "bc_pregprev_en", "bc_pregprev_1m", "bc_pregprev_4m", "bc_pregprev_8m", "bc_pregprev_12m","condom_freq", "condom_freq_v2_1m", "condom_freq_v2_4m", "condom_freq_v2_8m", "condom_freq_v2_12m")
sexhist_cols <- c(
  "sexmenwmb","sexmenwmb_v2_1m", "sexmenwmb_v2_4m","sexmenwmb_v2_8m","sexmenwmb_v2_12m",
  "ma_30","ma_3","ma_12","ma_lifet", "male_lastsex_days", "male_lastsex_years", "ma_slv_v2_1m", "ma_slv_v2_4m","ma_slv_v2_8m","ma_slv_v2_12m",
  "conlastsex", "conlastsex_v2_1m", "conlastsex_v2_4m","conlastsex_v2_8m","conlastsex_v2_12m",
  "newmen30d","newmen12m", "newmen30d_v2_1m", "newmen30d_v2_4m","newmen30d_v2_8m","newmen30d_v2_12m",
  "fsx30d","fsx3mo","fsx12m","fsxlife", "fsx_slv_v2_1m", "fsx_slv_v2_4m", "fsx_slv_v2_8m", "fsx_slv_v2_12m",
  "newwomen30d","newwomen12m", "newwomen30d_v2_1m", "newwomen30d_v2_4m", "newwomen30d_v2_8m", "newwomen30d_v2_12m",
  "oral_sex", "oral_sex_v2_1m", "oral_sex_v2_4m", "oral_sex_v2_8m", "oral_sex_v2_12m",
  "ma_uncirc", "ma_uncirc_v2_1m", "ma_uncirc_v2_4m","ma_uncirc_v2_8m","ma_uncirc_v2_12m",
  "anal_sex", "anal_sex_v2_1m", "anal_sex_v2_4m","anal_sex_v2_8m","anal_sex_v2_12m",
  "vagafrec", "vagafrec_v2_1m", "vagafrec_v2_4m","vagafrec_v2_8m","vagafrec_v2_12m",
  "conrecvag", "conrecvag_v2_1m", "conrecvag_v2_4m","conrecvag_v2_8m","conrecvag_v2_12m",
  "sex30mens", "sex30mens_v2_1m", "sex30mens_v2_4m","sex30mens_v2_8m","sex30mens_v2_12m",
  "bleed30sex", "bleed30sex_v2_1m", "bleed30sex_v2_4m","bleed30sex_v2_8m","bleed30sex_v2_12m",
  "mid30spot", "mid30spot_v2_1m", "mid30spot_v2_4m","mid30spot_v2_8m","mid30spot_v2_12m",
  "pain30sex", "pain30sex_v2_1m", "pain30sex_v2_4m","pain30sex_v2_8m","pain30sex_v2_12m",
  "plv30reg", "plv30reg_v2_1m", "plv30reg_v2_4m","plv30reg_v2_8m","plv30reg_v2_12m",
  "douch","freqdche","whydche"
  )



# Visits: replace NA with 0 ----------------------------------------------------------------
for (col in visit_cols) {
  if (col %in% names(summ)) {
    summ[[col]] <- ifelse(is.na(summ[[col]]), 0, summ[[col]])
  }
}



# Full column order.......................................
all_cols <- c(
  "PTID",
  inferred_col_char,
  endo_col,
  "fu",
  "ct_rectal_en_char",
  "ct_rectal_combined_en",
  "fu_rectal", 
  "rectal_reservoir",
  demo_cols,
  preg_cols,
  substance_cols,
  alcohol_cols,
  urisymp_cols,
  antibiotics_30d_cols,
  pelvic_cols, 
  visit_cols,
  "followup_visits_completed",
  mwri_vag_cols,
  prsby_vag_cols,
  vaginal_cols,
  diagnostic_cols,
  inferred_col_num, 
   rectal_cols,
  "fu_rectal_num", 
  pre_chlam_cols,
  chlam_report_cols,
  interim_vag_cols,
  "fu_num", 
  genital_rectal_cols, 
  endm_cols,
  ct_partner_cols,
  "Chronicity",
  mg_cols,
  trich_cols,
  tx_vag_cols,
  trich_v2_cols,
  tv_interim_cols,
  tv_fu,
  "trich_burden",
  gonn_prior_cols,
  gonn_v2_cols,
  gc_vag_cols,
  gc_vag_prsby_cols,
  gc_vag_bothcenters_cols,
  gc_rectal_cols,
  gc_endm_col,
  gc_interim_cols,
  gc_fu_col,
  gc_partner_cols,
  gc_partner_v2_cols,
  pid_cols,
  bv_cols,
  bv_v2_cols,
  gs_vag_cols,
  yeast_cols,
  yeast_v2_cols,
  ghsv_cols,
  ghsv_v2_cols,
  ghv_cols,
  pap_cols,
  pap_v2_cols,
  ngu_cols,
  bc_cols,
  sexhist_cols
  )

# Reorder summ
summ <- summ[, all_cols]


# === Create wb === #
wb <- createWorkbook()
addWorksheet(wb, "Summary")


# === Create grouped header map === #

header_map <- list(
  # Demographics

  age                   = "Demographics",
  gender                = "Demographics",
  race                  = "Demographics",
  hispeth               = "Demographics",
  education             = "Demographics",
  marital               = "Demographics",
  insurance             = "Demographics",
  sex_pref              = "Demographics",
  
  # Pregnancy
  everpreg     = "Pregnancy",
  everpreg_v2_1m = "Pregnancy",
  everpreg_v2_4m = "Pregnancy",
  everpreg_v2_8m = "Pregnancy",
  everpreg_v2_12m = "Pregnancy",
  livebirths   = "Pregnancy",
  miscarriage  = "Pregnancy",
  abortions    = "Pregnancy",
  numpregs     = "Pregnancy",
  tubal_preg   = "Pregnancy",
  blockedtubes = "Pregnancy",
  blockedtubes_v2_1m = "Pregnancy",
  blockedtubes_v2_4m = "Pregnancy",
  blockedtubes_v2_8m = "Pregnancy",
  blockedtubes_v2_12m = "Pregnancy",
  chronplvpain = "Pregnancy",
  chronplvpain_v2_1m = "Pregnancy",
  chronplvpain_v2_4m = "Pregnancy",
  chronplvpain_v2_8m = "Pregnancy",
  chronplvpain_v2_12m = "Pregnancy",
  plvc_surgery = "Pregnancy",
  plvc_surgery_v2_1m = "Pregnancy",
  plvc_surgery_v2_4m = "Pregnancy",
  plvc_surgery_v2_8m = "Pregnancy",
  plvc_surgery_v2_12m = "Pregnancy",
 
 
  #substance use 
  smoker        = "Substance use",
  cigs_day      = "Substance use",
  vape_juul     = "Substance use",
  med_marij     = "Substance use",
  rec_drugsever = "Substance use",
  druguse       = "Substance use",
  recdrugs_used = "Substance use",
  other_recdrug = "Substance use",
  
    # Visits
  visits_present_En     = "Present at visit (0=no / 1=yes)",
  visits_present_1m     = "Present at visit (0=no / 1=yes)",
  visits_present_4m     = "Present at visit (0=no / 1=yes)",
  visits_present8m      = "Present at visit (0=no / 1=yes)",
  visits_present12m     = "Present at visit (0=no / 1=yes)",
  followup_visits_completed = "Count of follow-up visits completed (not enrollment)",
  
  # Vaginal CT raw
  ct_vag_EN             = "MWRI vaginal chlamydia results (0=neg/1=pos/2=indeterminate/3=not done)",
  ct_vag_1m             = "MWRI vaginal chlamydia results (0=neg/1=pos/2=indeterminate/3=not done)",
  ct_vag_4m             = "MWRI vaginal chlamydia results (0=neg/1=pos/2=indeterminate/3=not done)",
  ct_vag_8m             = "MWRI vaginal chlamydia results (0=neg/1=pos/2=indeterminate/3=not done)",
  ct_vag_12m            = "MWRI vaginal chlamydia results (0=neg/1=pos/2=indeterminate/3=not done)",
  
  ct_vag_prsby_EN       = "Presby vaginal chlamydia results (0=neg/1=pos/2=indeterminate/3=not done/4=not required)",
  ct_vag_prsby_1m       = "Presby vaginal chlamydia results (0=neg/1=pos/2=indeterminate/3=not done/4=not required)",
  ct_vag_prsby_4m       = "Presby vaginal chlamydia results (0=neg/1=pos/2=indeterminate/3=not done/4=not required)",
  ct_vag_prsby_8m       = "Presby vaginal chlamydia results (0=neg/1=pos/2=indeterminate/3=not done/4=not required)",
  ct_vag_prsby_12m      = "Presby vaginal chlamydia results (0=neg/1=pos/2=indeterminate/3=not done/4=not required)",
  
  # Vaginal CT combined
  CT_vag_bothcenters_EN = "Vaginal chlamydia concatenated diagnostic (both centers) (0=negative/1=positive/2=indeterminate)",
  CT_vag_bothcenters_1m = "Vaginal chlamydia concatenated diagnostic (both centers) (0=negative/1=positive/2=indeterminate)",
  CT_vag_bothcenters_4m = "Vaginal chlamydia concatenated diagnostic (both centers) (0=negative/1=positive/2=indeterminate)",
  CT_vag_bothcenters_8m = "Vaginal chlamydia concatenated diagnostic (both centers) (0=negative/1=positive/2=indeterminate)",
  CT_vag_bothcenters_12m= "Vaginal chlamydia concatenated diagnostic (both centers) (0=negative/1=positive/2=indeterminate)",
  
  # CT qPCR
  CT_CB_result          = "Chlamydia qPCR",
  CT_CB_burden          = "Chlamydia qPCR",
  CT_VS_result          = "Chlamydia qPCR",
  CT_VS_burden          = "Chlamydia qPCR",
  
  #Inferred diagnostic 
#  CT_inferred_diagnostic_num= "Inferred result using diagnostic and qPCR (enrollment) (0=negative/1=positive/2=indeterminate)", 
#  CT_inferred_diagnostic_char= "Inferred result using diagnostic and qPCR (enrollment)", 
  
  # Endometrial CT
  #ct_endm            = "Endometrial chlamydia",
  
  # Cervix/endo classification
 # endo         = "Cervix/endo classification (0=neg at both/1=cervix only/2=cervix and endometrium)",

  # Rectal CT
  ct_rectal_en          = "Rectal chlamydia (0=negative/1=positive/2=indeterminate/3=not done)",
  ct_rectal_1m          = "Rectal chlamydia (0=negative/1=positive/2=indeterminate/3=not done)",
  ct_rectal_4m          = "Rectal chlamydia (0=negative/1=positive/2=indeterminate/3=not done)",
  ct_rectal_8m          = "Rectal chlamydia (0=negative/1=positive/2=indeterminate/3=not done)",
  ct_rectal_12m         = "Rectal chlamydia (0=negative/1=positive/2=indeterminate/3=not done)",
  
  #cervix rectal cols
  genital_rectal_1m = "Genital and rectal chlamydia combined",
  genital_rectal_4m = "Genital and rectal chlamydia combined",
  genital_rectal_8m = "Genital and rectal chlamydia combined",
  genital_rectal_12m = "Genital and rectal chlamydia combined",
  
  # Pre-enrollment CT
  numtimes_ct           = "Chlamydia prior to enrollment",
  daterecent_ct_days         = "Chlamydia prior to enrollment",
  daterecent_ct_years   = "Chlamydia prior to enrollment",

  #Chlam report of diagnosis (intermediate var for interim)
  chlam_en  = "Participant's report of chlamydia diagnosis",
  chlam_v2_1m = "Participant's report of chlamydia diagnosis",
  chlam_v2_4m = "Participant's report of chlamydia diagnosis",
  chlam_v2_8m = "Participant's report of chlamydia diagnosis",
  chlam_v2_12m = "Participant's report of chlamydia diagnosis",
  
  # Interim CT
  CT_vag_interim_1m     = "Genital chlamydia at any point during f/u (0=no/1=yes/2=indeterminate/NA=missing)",
  CT_vag_interim_4m     = "Genital chlamydia at any point during f/u (0=no/1=yes/2=indeterminate/NA=missing)",
  CT_vag_interim_8m     = "Genital chlamydia at any point during f/u (0=no/1=yes/2=indeterminate/NA=missing)",
  CT_vag_interim_12m    = "Genital chlamydia at any point during f/u (0=no/1=yes/2=indeterminate/NA=missing)",
  
  
  
  
 # fu                    = "Genital chlamydia follow-up positive/negative (0=neg/1=pos)",
  
  # Chlamydia status of sex partner(s) at enrollment and f/u
  ct             = "Chlamydia status of sex partner(s) at enrollment and f/u",
  ct_tx          = "Chlamydia status of sex partner(s) at enrollment and f/u",
  ct_self        = "Chlamydia status of sex partner(s) at enrollment and f/u",
  ct_v2_1m       = "Chlamydia status of sex partner(s) at enrollment and f/u",
  ct_tx_v2_1m    = "Chlamydia status of sex partner(s) at enrollment and f/u",
  ct_self_v2_1m  = "Chlamydia status of sex partner(s) at enrollment and f/u",
  
  # MG
  MG_CB_result          = "Mgen cytobrush result (0=neg/1=pos)",
  MG_CB_burden          = "Mgen cytobrush burden",
  MG_VS_result          = "Mgen vaginal swab result (0=neg/1=pos)",
  MG_VS_burden          = "Mgen vaginal swab burden",
  MG_inferred_diagnostic= "Mgen inferred diagnostic (0=neg/1=pos/2=indeterminate)",
  
  # Trich
  trich_En              = "Ever had trich diagnosis before enrollment (0=no/1=yes)",
  tx_vag_En             = "Genital trichomonas diagnosis",
  tx_vag_1m             = "Genital trichomonas diagnosis",
  tx_vag_4m             = "Genital trichomonas diagnosis",
  tx_vag_8m             = "Genital trichomonas diagnosis",
  tx_vag_12m            = "Genital trichomonas diagnosis",
  trich_v2_1m           = "Reported trich diagnosis at f/u",
  trich_v2_4m           = "Reported trich diagnosis at f/u",
  trich_v2_8m           = "Reported trich diagnosis at f/u",
  trich_v2_12m          = "Reported trich diagnosis at f/u",
  
  tv_interim_1m         = "Participant reported or was diagnosed with trich at f/u (0=no/1=yes/2=indeterminate/NA=missing)",
  tv_interim_4m         = "Participant reported or was diagnosed with trich at f/u (0=no/1=yes/2=indeterminate/NA=missing)",
  tv_interim_8m         = "Participant reported or was diagnosed with trich at f/u (0=no/1=yes/2=indeterminate/NA=missing)",
  tv_interim_12m        = "Participant reported or was diagnosed with trich at f/u (0=no/1=yes/2=indeterminate/NA=missing)",
  
  tv_fu                 = "Trich f/u positive or negative (0=negative/1=positive)",
  
  trich_burden          = "Trichomonas burden (qPCR)",
  
  
  # Gonorrhea - prior to enrollment
  gonn          = "Gonorrhea prior to enrollment",
  numtimes_gc   = "Gonorrhea prior to enrollment",
  daterecent_gc_days = "Gonorrhea prior to enrollment",
  daterecent_gc_years = "Gonorrhea prior to enrollment",
  
  # Gonorrhea - reports of a diagnosis at En and f/u
  gonn_v2_1m    = "Reported diagnosis of Gonorrhea at f/u",
  gonn_v2_4m    = "Reported diagnosis of Gonorrhea at f/u",
  gonn_v2_8m    = "Reported diagnosis of Gonorrhea at f/u",
  gonn_v2_12m   = "Reported diagnosis of Gonorrhea at f/u",
  
  # Gonorrhea site diagnostics and combined
  # MWRI vaginal gonorrhea
  gc_vag_en   = "MWRI vaginal gonorrhea result (0=negative/1=positive/2=indeterminate/3=not done)",
  gc_vag_1m   = "MWRI vaginal gonorrhea result (0=negative/1=positive/2=indeterminate/3=not done)",
  gc_vag_4m   = "MWRI vaginal gonorrhea result (0=negative/1=positive/2=indeterminate/3=not done)",
  gc_vag_8m   = "MWRI vaginal gonorrhea result (0=negative/1=positive/2=indeterminate/3=not done)",
  gc_vag_12m  = "MWRI vaginal gonorrhea result (0=negative/1=positive/2=indeterminate/3=not done)",
  
  # Presby vaginal gonorrhea
  gc_vag_prsby_en   = "Presby vaginal gonorrhea result (0=negative/1=positive/2=indeterminate/3=not done/4=not required)",
  gc_vag_prsby_1m   = "Presby vaginal gonorrhea result (0=negative/1=positive/2=indeterminate/3=not done/4=not required)",
  gc_vag_prsby_4m   = "Presby vaginal gonorrhea result (0=negative/1=positive/2=indeterminate/3=not done/4=not required)",
  gc_vag_prsby_8m   = "Presby vaginal gonorrhea result (0=negative/1=positive/2=indeterminate/3=not done/4=not required)",
  gc_vag_prsby_12m  = "Presby vaginal gonorrhea result (0=negative/1=positive/2=indeterminate/3=not done/4=not required)",
  
  # Combined diagnostic
  gc_vag_bothcenters_en   = "Gonorrhea (vaginal) combined diagnostic (both centers) (0=negative/1=positive/2=indeterminate/3=not done)",
  gc_vag_bothcenters_1m   = "Gonorrhea (vaginal) combined diagnostic (both centers) (0=negative/1=positive/2=indeterminate/3=not done)",
  gc_vag_bothcenters_4m   = "Gonorrhea (vaginal) combined diagnostic (both centers) (0=negative/1=positive/2=indeterminate/3=not done)",
  gc_vag_bothcenters_8m   = "Gonorrhea (vaginal) combined diagnostic (both centers) (0=negative/1=positive/2=indeterminate/3=not done)",
  gc_vag_bothcenters_12m  = "Gonorrhea (vaginal) combined diagnostic (both centers) (0=negative/1=positive/2=indeterminate/3=not done)",
  
  # Rectal gonn
  gc_rectal_en   = "Rectal gonorrhea result (0=negative/1=positive/2=indeterminate/3=not done)",
  gc_rectal_1m   = "Rectal gonorrhea result (0=negative/1=positive/2=indeterminate/3=not done)",
  gc_rectal_4m   = "Rectal gonorrhea result (0=negative/1=positive/2=indeterminate/3=not done)",
  gc_rectal_8m   = "Rectal gonorrhea result (0=negative/1=positive/2=indeterminate/3=not done)",
  gc_rectal_12m  = "Rectal gonorrhea result (0=negative/1=positive/2=indeterminate/3=not done)",
  
  #Endm gonn
  gc_endm   = "Endometrial gonorrhea result (0=negative/1=positive/2=indeterminate/3=not done)",
  
  #Gonn interim dx
  gc_vag_interim_1m  = "Participant reported or was diagnosed with gonorrhea at f/u (0=no/1=yes/2=indeterminate/NA=missing)",
  gc_vag_interim_4m  = "Participant reported or was diagnosed with gonorrhea at f/u (0=no/1=yes/2=indeterminate/NA=missing)",
  gc_vag_interim_8m  = "Participant reported or was diagnosed with gonorrhea at f/u (0=no/1=yes/2=indeterminate/NA=missing)",
  gc_vag_interim_12m = "Participant reported or was diagnosed with gonorrhea at f/u (0=no/1=yes/2=indeterminate/NA=missing)",
  
  #Gonn f/u pos or neg 
  gonn_fu = "Gonorrhea f/u negative or positive (0=negative/1=positive)",
  
  #Gonorrhea status of sex partner(s) at enrollment and f/u
  # Enrollment partner GC status
  gc      = "Gonorrhea status of sex partner(s) at enrollment and f/u",
  gc_tx   = "Gonorrhea status of sex partner(s) at enrollment and f/u",
  gc_self = "Gonorrhea status of sex partner(s) at enrollment and f/u",
  
  # Follow-up partner GC status
  gc_v2_1m    = "Gonorrhea status of sex partner(s) at enrollment and f/u",
  gc_tx_v2_1m = "Gonorrhea status of sex partner(s) at enrollment and f/u",
  gc_self_v2_1m = "Gonorrhea status of sex partner(s) at enrollment and f/u",
  
  gc_v2_4m    = "Gonorrhea status of sex partner(s) at enrollment and f/u",
  gc_tx_v2_4m = "Gonorrhea status of sex partner(s) at enrollment and f/u",
  gc_self_v2_4m = "Gonorrhea status of sex partner(s) at enrollment and f/u",
  
  gc_v2_8m    = "Gonorrhea status of sex partner(s) at enrollment and f/u",
  gc_tx_v2_8m = "Gonorrhea status of sex partner(s) at enrollment and f/u",
  gc_self_v2_8m = "Gonorrhea status of sex partner(s) at enrollment and f/u",
  
  gc_v2_12m    = "Gonorrhea status of sex partner(s) at enrollment and f/u",
  gc_tx_v2_12m = "Gonorrhea status of sex partner(s) at enrollment and f/u",
  gc_self_v2_12m = "Gonorrhea status of sex partner(s) at enrollment and f/u",
  
  #PID 
  pelv            = "Pelvic Inflammatory Disease",
  numtimes_pid    = "Pelvic Inflammatory Disease",
  daterecent_pid_days  = "Pelvic Inflammatory Disease",
  daterecent_pid_years  = "Pelvic Inflammatory Disease",
  
  #BV
  bv          = "Bacterial vaginosis",
  bv_v2_1m    = "Bacterial vaginosis",
  bv_v2_4m    = "Bacterial vaginosis",
  bv_v2_8m    = "Bacterial vaginosis",
  bv_v2_12m   = "Bacterial vaginosis",
  #BV gram stain
  gs_vag_en   = "Vaginal gram stain",
  gs_vag_1m   = "Vaginal gram stain",
  gs_vag_4m   = "Vaginal gram stain",
  gs_vag_8m   = "Vaginal gram stain",
  gs_vag_12m  = "Vaginal gram stain",
  
  #yeast infection
  yeast        = "Yeast infection",
  yeast_v2_1m  = "Yeast infection",
  yeast_v2_4m  = "Yeast infection",
  yeast_v2_8m  = "Yeast infection",
  yeast_v2_12m = "Yeast infection",
  
  #Gen herpes
  ghsv        = "Genital herpes",
  ghsv_v2_1m  = "Genital herpes",
  ghsv_v2_4m  = "Genital herpes",
  ghsv_v2_8m  = "Genital herpes",
  ghsv_v2_12m = "Genital herpes",
  
  #Gen warts
  ghv = "Genital warts",
  
  #Abnormal pap smear
  pap        = "Abnormal pap smear",
  pap_v2_1m  = "Abnormal pap smear",
  pap_v2_4m  = "Abnormal pap smear",
  pap_v2_8m  = "Abnormal pap smear",
  pap_v2_12m = "Abnormal pap smear",
  
  #Nongonococcal Urethritis
  ngu_v2_4m        = "Nongonococcal Urethritis",
  ngu_prt_tx_v2_4m = "Nongonococcal Urethritis",
  
  #Contraception and pregnancy prevention 
  bc_hormone_en    = "Contraception/birth control",
  bc_hormone_1m = "Contraception/birth control",
  bc_hormone_4m  = "Contraception/birth control",
  bc_hormone_8m  = "Contraception/birth control",
  bc_hormone_12m = "Contraception/birth control",
  
  bc_pregprev_en = "Contraception/birth control",
  bc_pregprev_1m = "Contraception/birth control",
  bc_pregprev_4m = "Contraception/birth control",
  bc_pregprev_8m = "Contraception/birth control",
  bc_pregprev_12m = "Contraception/birth control",
  
  condom_freq        = "Contraception/birth control",
  condom_freq_v2_1m = "Contraception/birth control",
  condom_freq_v2_4m = "Contraception/birth control",
  condom_freq_v2_8m = "Contraception/birth control",
  condom_freq_v2_12m = "Contraception/birth control",
  
  
  #Sexual history at enrollment
    sexmenwmb = "Sexual history at enrollment",
    sexmenwmb_v2_1m = "Sexual history at enrollment",
  sexmenwmb_v2_4m = "Sexual history at enrollment",
  sexmenwmb_v2_8m = "Sexual history at enrollment",
  sexmenwmb_v2_12m = "Sexual history at enrollment",
    ma_30 = "Sexual history at enrollment",
    ma_3 = "Sexual history at enrollment",
    ma_12 = "Sexual history at enrollment",
    ma_lifet = "Sexual history at enrollment",
  male_lastsex_days = "Sexual history at enrollment", 
   male_lastsex_years = "Sexual history at enrollment",
  ma_slv_v2_1m = "Sexual history at enrollment",
  ma_slv_v2_4m = "Sexual history at enrollment",
  ma_slv_v2_8m= "Sexual history at enrollment",
  ma_slv_v2_12m = "Sexual history at enrollment",
    conlastsex = "Sexual history at enrollment",
    newmen30d = "Sexual history at enrollment",
    newmen12m = "Sexual history at enrollment",
    fsx30d = "Sexual history at enrollment",
    fsx3mo = "Sexual history at enrollment",
    fsx12m = "Sexual history at enrollment",
    fsxlife = "Sexual history at enrollment",
    newwomen30d = "Sexual history at enrollment",
    newwomen12m = "Sexual history at enrollment",
    oral_sex = "Sexual history at enrollment",
    ma_uncirc = "Sexual history at enrollment",
    anal_sex = "Sexual history at enrollment",
    vagafrec = "Sexual history at enrollment",
    conrecvag = "Sexual history at enrollment",
    douch = "Sexual history at enrollment",
    freqdche = "Sexual history at enrollment",
    whydche = "Sexual history at enrollment",
    sex30mens = "Sexual history at enrollment",
    bleed30sex = "Sexual history at enrollment",
    mid30spot = "Sexual history at enrollment",
    pain30sex = "Sexual history at enrollment",
    plv30reg = "Sexual history at enrollment"
    )


# Function for dynamic header row 
grouped_header <- sapply(all_cols, function(col) {
  if (col %in% names(header_map)) header_map[[col]] else ""
})

# Write to wb 

writeData(wb, sheet = "Summary", x = as.data.frame(t(grouped_header)), startRow = 1, colNames = FALSE)


# === Group mapping to merge cells with same header (so header is not just above 1 col) === # 

groups <- list(
  Demographics = demo_cols,
  Pregnancy   = preg_cols,
  Substances  = substance_cols,
  Visits      = visit_cols,
  MWRI        = mwri_vag_cols,
  PRSBY       = prsby_vag_cols,
  Rectal      = rectal_cols,
  Vaginal     = vaginal_cols,
  Diagnostic  = diagnostic_cols,
  MG          = mg_cols,
  Endometrial = endm_cols,
  Endocol     = endo_col,
  CR = genital_rectal_cols,
  CT_Partner  = ct_partner_cols,
  PreEnrollCT = pre_chlam_cols,
  InterimCT   = interim_vag_cols,
  Trich       = trich_cols,
  TxVag       = tx_vag_cols,
  Trich_v2    = trich_v2_cols,
  TV_interim  = tv_interim_cols,
  TV_fu_stat  =tv_fu, 
  TrichBurden = c("trich_burden"),
  GonnPrior = gonn_prior_cols,
  GonnReport  = gonn_v2_cols,
  GC_MWRI        = gc_vag_cols,
  GC_PRSBY       = gc_vag_prsby_cols,
  GC_BothCenters = gc_vag_bothcenters_cols,
  GC_Rectal   = gc_rectal_cols,
  GC_Endometrial = gc_endm_col,
  GC_Interim  = gc_interim_cols,
  GC_FU       = c("gonn_fu"),
  GC_Partner = c(gc_partner_cols, gc_partner_v2_cols),
  PID = pid_cols,
  bv_all = c(bv_cols, bv_v2_cols, gs_vag_cols),
  yeast_all = c(yeast_cols, yeast_v2_cols), 
  gen_herpes_all = c(ghsv_cols, ghsv_v2_cols),
  gen_warts = ghv_cols,
  abnormal_pap = c(pap_cols, pap_v2_cols),
  NGU = ngu_cols,
  BirthControl = bc_cols,
  SexualHistory = sexhist_cols
)

# merge loop: merges only sequential blocks in actual column order
for (grp in names(groups)) {
  grp_cols <- groups[[grp]]
  
  # Find actual column indices in the sheet
  col_idx <- which(all_cols %in% grp_cols)
  
  # If group columns are not sequential, merge each contiguous block separately
  if (length(col_idx) > 1) {
    runs <- split(col_idx, cumsum(c(1, diff(col_idx) != 1)))
    
    for (r in runs) {
      if (length(r) > 1) {
        mergeCells(wb, sheet = "Summary",
                   cols = min(r):max(r),
                   rows = 1)
      }
    }
  }
}

# ---------------------------------------------
# Make more intuitive labels (2nd row of sheet)
#----------------------------------------------

label_map <- list(
  
 #Key summary variables 
  PTID                  = "Patient ID",
  
  #Inferred diagnostic 
  CT_inferred_diagnostic_char = "Inferred genital chlamydia result using combined MWRI/Prsby diagnostics and qPCR at enrollment",
  
  # Cervix/endo classification
  endo = "Endometrial chlamydia (endo+ = endo+ CT+ /endo- = endo- CT+ /NA = CT- OR endo indeterminate or not done OR discrepant diagnostic and qPCR results)",
  
  # Follow-up summary
  fu  = "Genital chlamydia diagnosed and/or reported at follow-up (positive/negative)",
  
  #rectal chlam at enrollment from redcap but readable
  ct_rectal_en_char = "Rectal chlamydia diagnostic at enrollment", 
  
  #rectal 
  ct_rectal_combined_en = "Rectal and genital chlamydia at enrollment", 
  
  #Rectal chlam diagnosis at f/u
  fu_rectal = "Rectal chlamydia diagnosis at follow-up",
  
  #Rectal chlam combined with CT result at f/u
  rectal_reservoir = "Rectal-positive but genital-negative at any point during follow-up",
  
  # Demographics 
  age                   = "Age",
  gender                = "Gender (1=female/8=other(1 unspecified)",
  race                  = "Race",
  hispeth               = "Latina or hispanic origin (0=no/1=yes)",
  education             = "Education level (1=<HS/2=in HS/3=HS grad or GED/4=Some college/5=College grad/6=Post-grad work/7=Vocational)",
  marital               = "Partner status (1=single/2=living w partner >=4mo/3=married4=separated/5=divorced/6=widowed)",
  insurance             = "Type of insurance (1=none/2=private/3=public/4=Medicaid HMO/5=unsure/6=other)",
  sex_pref              = "Sexual preference (1=heterosexual/2=homosexual/3=bi-sexual/4=asexual/5=other (none specified))",
  
  
  # Pregnancy
  everpreg     = "Have you ever been pregnant (0=no/1=yes)",
  everpreg_v2_1m = "Since your last visit, have you been pregnant? (0=no, 1=yes)",
  everpreg_v2_4m = "Since your last visit, have you been pregnant? (0=no, 1=yes)",
  everpreg_v2_8m = "Since your last visit, have you been pregnant? (0=no, 1=yes)",
  everpreg_v2_12m = "Since your last visit, have you been pregnant? (0=no, 1=yes)",
  livebirths   = "Number of live births",
  miscarriage  = "Number of miscarriages",
  abortions    = "Number of abortions",
  numpregs     = "Total number of pregnancies (auto-calculated)",
  tubal_preg   = "Ever had an ectopic (tubal) pregnancy? (0=no/1=yes/2=unsure)",
  blockedtubes = "Ever found to have a problem with your fallopian tubes (eg blocked tubes)? (0=no/1=yes/2=unsure)",
  blockedtubes_v2_1m = "Since your last visit, were you found to have a problem with your fallopian tubes? (eg blocked tubes)? (0=no, 1=yes, 2=unsure)",
  blockedtubes_v2_4m = "Since your last visit, were you found to have a problem with your fallopian tubes? (eg blocked tubes)? (0=no, 1=yes, 2=unsure)",
  blockedtubes_v2_8m = "Since your last visit, were you found to have a problem with your fallopian tubes? (eg blocked tubes)? (0=no, 1=yes, 2=unsure)", 
  blockedtubes_v2_12m = "Since your last visit, were you found to have a problem with your fallopian tubes? (eg blocked tubes)? (0=no, 1=yes, 2=unsure)",
  chronplvpain = "History of chronic pelvic pain (defined as consistent pain ≥ 30 days)? (0=no/1=yes)",
  chronplvpain_v2_1m = "Since your last visit, have you had chronic pelvic pain (defined as consistent pain =>30 days)? (0=no, 1=yes)",
  chronplvpain_v2_4m = "Since your last visit, have you had chronic pelvic pain (defined as consistent pain =>30 days)? (0=no, 1=yes)",
  chronplvpain_v2_8m = "Since your last visit, have you had chronic pelvic pain (defined as consistent pain =>30 days)? (0=no, 1=yes)",
  chronplvpain_v2_12m = "Since your last visit, have you had chronic pelvic pain (defined as consistent pain =>30 days)? (0=no, 1=yes)",
  plvc_surgery = "Have you had any of the following surgeries?",
  plvc_surgery_v2_1m = "Since your last visit, have you had any of the following surgeries or other abdominal/pelvic surgeries?",
  plvc_surgery_v2_4m = "Since your last visit, have you had any of the following surgeries or other abdominal/pelvic surgeries?",
  plvc_surgery_v2_8m = "Since your last visit, have you had any of the following surgeries or other abdominal/pelvic surgeries?",
  plvc_surgery_v2_12m = "Since your last visit, have you had any of the following surgeries or other abdominal/pelvic surgeries?",

 
  # substance use
  smoker              = "Cigarette smoking at enrollment (0=no/1=yes-daily/2=yes-socially)",
  smoker_v2_1m        = "Since your last visit, have you smoked cigarettes? (0=no/1=yes-daily/2=yes-socially)",
  smoker_v2_4m        = "Since your last visit, have you smoked cigarettes? (0=no/1=yes-daily/2=yes-socially)",
  smoker_v2_8m        = "Since your last visit, have you smoked cigarettes? (0=no/1=yes-daily/2=yes-socially)",
  smoker_v2_12m       = "Since your last visit, have you smoked cigarettes? (0=no/1=yes-daily/2=yes-socially)",
  
  cigs_day            = "Cigarettes/day (enrollment)",
  cigs_day_v2_1m      = "Cigarettes/day",
  cigs_day_v2_4m      = "Cigarettes/day",
  cigs_day_v2_8m      = "Cigarettes/day",
  cigs_day_v2_12m     = "Cigarettes/day",
  
  vape_juul           = "Smokeless tobacco (i.e. vape) use (0=no/1=yes)",
  vape_juul_v2_1m     = "Since your last visit, have you used smokeless tobacco (i.e. vape)? (0=no/1=yes)",
  vape_juul_v2_4m     = "Since your last visit, have you used smokeless tobacco (i.e. vape)? (0=no/1=yes)",
  vape_juul_v2_8m     = "Since your last visit, have you used smokeless tobacco (i.e. vape)? (0=no/1=yes)",
  vape_juul_v2_12m    = "Since your last visit, have you used smokeless tobacco (i.e. vape)? (0=no/1=yes)",
  
  med_marij           = "Has medical marijuana card (0=no/1=yes)",
  med_marij_v2_1m     = "Since your last visit, have you gotten a medical marijuana card? (0=no/1=yes)",
  med_marij_v2_4m     = "Since your last visit, have you gotten a medical marijuana card? (0=no/1=yes)",
  med_marij_v2_8m     = "Since your last visit, have you gotten a medical marijuana card? (0=no/1=yes)",
  med_marij_v2_12m    = "Since your last visit, have you gotten a medical marijuana card? (0=no/1=yes)",
  
  rec_drugsever       = "Ever used recreational drugs (0=no/1=yes)",
  rec_drugsever_v2_1m = "Since your last visit, have you used recreational drugs? (0=no/1=yes)",
  rec_drugsever_v2_4m = "Since your last visit, have you used recreational drugs? (0=no/1=yes)",
  rec_drugsever_v2_8m = "Since your last visit, have you used recreational drugs? (0=no/1=yes)",
  rec_drugsever_v2_12m = "Since your last visit, have you used recreational drugs? (0=no/1=yes)",
  
  druguse             = "Used any recreational drugs in the past 30 days (0=no/1=yes/2=unsure)",
  druguse_v2_1m       = "Used any recreational drugs in the past 30 days (0=no/1=yes/2=unsure)",
  druguse_v2_4m       = "Used any recreational drugs in the past 30 days (0=no/1=yes/2=unsure)",
  druguse_v2_8m       = "Used any recreational drugs in the past 30 days (0=no/1=yes/2=unsure)",
  druguse_v2_12m      = "Used any recreational drugs in the past 30 days (0=no/1=yes/2=unsure)",
  
  recdrugs_used       = "Recreational drugs used",
  recdrugs_used_v2_1m = "Recreational drugs used in the past 30 days",
  recdrugs_used_v2_4m = "Recreational drugs used in the past 30 days",
  recdrugs_used_v2_8m = "Recreational drugs used in the past 30 days",
  recdrugs_used_v2_12m = "Recreational drugs used in the past 30 days",
  
  other_recdrug       = "Other recreational drug(s) used",
  other_recdrug_v2_1m = "Other recreational drug(s) used in the past 30 days",
  other_recdrug_v2_4m = "Other recreational drug(s) used in the past 30 days",
  other_recdrug_v2_8m = "Other recreational drug(s) used in the past 30 days",
  other_recdrug_v2_12m = "Other recreational drug(s) used in the past 30 days", 
  
  # alcohol use
  drink               = "Do you drink alcohol? (0=no/1=yes)",
  drink_v2_1m         = "Since your last visit, have you had any alcohol? (0=no/1=yes)",
  drink_v2_4m         = "Since your last visit, have you had any alcohol? (0=no/1=yes)",
  drink_v2_8m         = "Since your last visit, have you had any alcohol? (0=no/1=yes)",
  drink_v2_12m        = "Since your last visit, have you had any alcohol? (0=no/1=yes)",
  
  drinks_week         = "How many alcoholic drinks do you have in an average week? (enrollment)",
  drinks_week_v2_1m   = "Since your last visit, how many alcoholic drinks have you had in an average week?",
  drinks_week_v2_4m   = "Since your last visit, how many alcoholic drinks have you had in an average week?",
  drinks_week_v2_8m   = "Since your last visit, how many alcoholic drinks have you had in an average week?",
  drinks_week_v2_12m  = "Since your last visit, how many alcoholic drinks have you had in an average week?",
  
  drink5inaday        = "How many days have you had 5 or more drinks of alcohol in one day in the last 30 days? (enrollment)",
  drink5inaday_v2_1m  = "Since your last visit, how many days have you had 5 or more drinks of alcohol in one day?",
  drink5inaday_v2_4m  = "Since your last visit, how many days have you had 5 or more drinks of alcohol in one day?",
  drink5inaday_v2_8m  = "Since your last visit, how many days have you had 5 or more drinks of alcohol in one day?",
  drink5inaday_v2_12m = "Since your last visit, how many days have you had 5 or more drinks of alcohol in one day?",
  
  # urinary / vaginal symptoms
  urisymptoms            = "Do you currently have any of the following symptoms? (Urinary or vaginal symptoms) (multi-select)",
  urisymptoms_v2_1m      = "Since your last visit, have you had any of the following symptoms? (Urinary or vaginal symptoms) (multi-select)",
  urisymptoms_v2_4m      = "Since your last visit, have you had any of the following symptoms? (Urinary or vaginal symptoms) (multi-select)",
  urisymptoms_v2_8m      = "Since your last visit, have you had any of the following symptoms? (Urinary or vaginal symptoms) (multi-select)",
  urisymptoms_v2_12m     = "Since your last visit, have you had any of the following symptoms? (Urinary or vaginal symptoms) (multi-select)",
  
  # oral or vaginal antibiotics in past 30 days
  abxov30d               = "Have you used any other antibiotics (oral or vaginal) in the past 30 days besides for Gonorrhea or Chlamydia as listed above? (0=no/1=yes)",
  abxov30d_v2_1m         = "Since your last visit, have you used any antibiotics (oral or vaginal) besides for Gonorrhea or Chlamydia? (0=no/1=yes)",
  abxov30d_v2_4m         = "Since your last visit, have you used any antibiotics (oral or vaginal) besides for Gonorrhea or Chlamydia? (0=no/1=yes)",
  abxov30d_v2_8m         = "Since your last visit, have you used any antibiotics (oral or vaginal) besides for Gonorrhea or Chlamydia? (0=no/1=yes)",
  abxov30d_v2_12m        = "Since your last visit, have you used any antibiotics (oral or vaginal) besides for Gonorrhea or Chlamydia? (0=no/1=yes)",
  
  # NSAID or pelvic pain medication use in past 30 days
  nsaidplvp30d           = "Have you used any medication for pelvic pain (eg Tylenol, Ibuprofen) in the past 30days? (0=no/1=yes)",
  nsaidplvp30d_v2_1m     = "Since your last visit, have you used any medication for pelvic pain (eg Tylenol,Ibuprofen)? (0=no/1=yes)",
  nsaidplvp30d_v2_4m     = "Since your last visit, have you used any medication for pelvic pain (eg Tylenol,Ibuprofen)? (0=no/1=yes)",
  nsaidplvp30d_v2_8m     = "Since your last visit, have you used any medication for pelvic pain (eg Tylenol,Ibuprofen)? (0=no/1=yes)",
  nsaidplvp30d_v2_12m    = "Since your last visit, have you used any medication for pelvic pain (eg Tylenol,Ibuprofen)? (0=no/1=yes)",

  
  # Visits
  visits_present_En  = "Enrollment visit present",
  visits_present_1m  = "1 mo visit present",
  visits_present_4m  = "4 mo visit present",
  visits_present8m   = "8 mo visit present",
  visits_present12m  = "12 mo visit present",
  followup_visits_completed = "Count of follow-up visits completed (not enrollment)",
  
  # Vaginal CT raw (MWRI + Presby)
  ct_vag_EN          = "MWRI vag CT result enrollment",
  ct_vag_1m          = "MWRI vag CT result 1m",
  ct_vag_4m          = "MWRI vag CT result 4m",
  ct_vag_8m          = "MWRI vag CT result 8m",
  ct_vag_12m         = "MWRI vag CT result 12m",
  ct_vag_prsby_EN    = "Presby vag CT result enrollment",
  ct_vag_prsby_1m    = "Presby vag CT result 1m",
  ct_vag_prsby_4m    = "Presby vag CT result 4m",
  ct_vag_prsby_8m    = "Presby vag CT result 8m",
  ct_vag_prsby_12m   = "Presby vag CT result 12m",
  
  # Vaginal CT combined
  CT_vag_bothcenters_EN  = "Vag CT result enrollment (both centers)",
  CT_vag_bothcenters_1m  = "Vag CT result 1m (both centers)",
  CT_vag_bothcenters_4m  = "Vag CT result 4m (both centers)",
  CT_vag_bothcenters_8m  = "Vag CT result 8m (both centers)",
  CT_vag_bothcenters_12m = "Vag CT result 12m (both centers)",
  
  # Chlamydia qPCR
  CT_CB_result        = "CT cytobrush result (0=neg/1=pos)",
  CT_CB_burden        = "CT cytobrush burden",
  CT_VS_result        = "CT vaginal swab result (0=neg/1=pos)",
  CT_VS_burden        = "CT vaginal swab burden",
 
  #Inferred diagnostic 
  CT_inferred_diagnostic_num= "Inferred genital chlamydia result using combined MWRI/Prsby diagnostics and qPCR at enrollment (0=neg/1=pos/2=indeterminate) ", 
  
  # Endometrial CT
  ct_endm          = "MWRI CT endometrial resuls at enrollment (as is in REDCap) (0=negative/1=positive/2=indeterminate/3=not done)",

  
  # Rectal CT
  ct_rectal_en        = "Rectal CT at enrollment (0=negative/1=positive/2=indeterminate/3=not done)",
  ct_rectal_1m        = "Rectal CT at 1m (0=negative/1=positive/2=indeterminate/3=not done)",
  ct_rectal_4m        = "Rectal CT at 4m (0=negative/1=positive/2=indeterminate/3=not done)",
  ct_rectal_8m        = "Rectal CT at 8m (0=negative/1=positive/2=indeterminate/3=not done)",
  ct_rectal_12m       = "Rectal CT at 12m (0=negative/1=positive/2=indeterminate/3=not done)",
  fu_rectal_num       = "Rectal CT at any point during follow-up (0=no never positive /1=yes had positive result", 
  
  #cervix rectal cols
  genital_rectal_1m = "Genital & rectal CT (0=cervix & rectal neg, 1=cervix pos only / 2=rectal pos only / 3=cervix & rectal pos)",
  genital_rectal_4m = "Genital & rectal CT (0=cervix & rectal neg, 1=cervix pos only / 2=rectal pos only / 3=cervix & rectal pos)",
  genital_rectal_8m = "Genital & rectal CT (0=cervix & rectal neg, 1=cervix pos only / 2=rectal pos only / 3=cervix & rectal pos)",
  genital_rectal_12m = "Genital & rectal CT (0=cervix & rectal neg, 1=cervix pos only / 2=rectal pos only / 3=cervix & rectal pos)",
  
  # Pre-enrollment CT
  numtimes_ct   = "Participant’s report of number of times they had chlamydia before enrollment (excluding any diagnosis at enrollment)",
  daterecent_ct_days = "Days since last chlamydia infection (prior to enrollment)",
  daterecent_ct_years = "Years since last chlamydia infection (prior to enrollment)",
  
  #Chlam report of diagnosis (intermediate var for interim)
  chlam_en  = "Have you ever been diagnosed with chlamydia prior to enrollment? (excluding any diagnosis at enrollment) (0=no/1=yes)",
  chlam_v2_1m = "Since your last visit, have you been diagnosed with chlamydia? (0=no/1=yes)",
  chlam_v2_4m = "Since your last visit, have you been diagnosed with chlamydia? (0=no/1=yes)",
  chlam_v2_8m = "Since your last visit, have you been diagnosed with chlamydia? (0=no/1=yes)",
  chlam_v2_12m = "Since your last visit, have you been diagnosed with chlamydia? (0=no/1=yes)",
  
  # Interim vaginal diagnostics
  CT_vag_interim_1m   = "1 mo interim vaginal chlamydia (0=negative at fu/1=positive at fu)",
  CT_vag_interim_4m   = "4 mo interim vaginal chlamydia (0=negative at fu/1=positive at fu)",
  CT_vag_interim_8m   = "8 mo interim vaginal chlamydia (0=negative at fu/1=positive at fu)",
  CT_vag_interim_12m  = "12 mo interim vaginal chlamydia (0=negative at fu/1=positive at fu)",
  
  # genital chlam at f/u, but numeric levels if needed for analysis 
  fu_num = "Genital chlamydia diagnosed and/or reported at follow-up numeric levels (0 = fu negative /1 = fu positive)", 
  

  # Chlamydia status of sex partner(s) at enrollment and f/u
  ct      = "Sex partner(s) diagnosed with chlamydia in the past 3 months at enrollment (0=no/1=yes)",
  ct_tx   = "Infected partner was treated for chlamydia (pre-enrollment) (0=no/1=yes/2=unknown)",
  ct_self = "Participant was treated for chlamydia (pre-enrollment) (0=no/1=yes/2=got medication but did not take it or finish it)",
  
  ct_v2_1m      = "Sex partner(s) diagnosed with chlamydia since last visit (0=no/1=yes)",
  ct_tx_v2_1m   = "Infected partner was treated for chlamydia (0=no/1=yes/2=unknown)",
  ct_self_v2_1m = "Participant was treated for chlamydia (0=no/1=yes/2=got medication but did not take it or finish it)",
  ct_v2_4m      = "Sex partner(s) diagnosed with chlamydia since last visit (0=no/1=yes)",
  ct_tx_v2_4m   = "Infected partner was treated for chlamydia (0=no/1=yes/2=unknown)",
  ct_self_v2_4m = "Participant was treated for chlamydia (0=no/1=yes/2=got medication but did not take it or finish it)",
  
  ct_v2_8m      = "Sex partner(s) diagnosed with chlamydia since last visit (0=no/1=yes)",
  ct_tx_v2_8m   = "Infected partner was treated for chlamydia (0=no/1=yes/2=unknown)",
  ct_self_v2_8m = "Participant was treated for chlamydia (0=no/1=yes/2=got medication but did not take it or finish it)",
  
  ct_v2_12m      = "Sex partner(s) diagnosed with chlamydia since last visit (0=no/1=yes)",
  ct_tx_v2_12m   = "Infected partner was treated for chlamydia (0=no/1=yes/2=unknown)",
  ct_self_v2_12m = "Participant was treated for chlamydia (0=no/1=yes/2=got medication but did not take it or finish it)",
  
  # Mycoplasma genitalium
  MG_CB_result        = "Mgen cytobrush result ",
  MG_CB_burden        = "Mgen cytobrush burden",
  MG_VS_result        = "Mgen vaginal swab result",
  MG_VS_burden        = "Mgen vaginal swab burden",
  MG_inferred_diagnostic = "Mgen inferred diagnostic (0=neg/1=pos/2=indeterminate)",
  
  #Histology 
  Chronicity = "Endometritis chronicity (histology data) (NA=sample not adequate or no slide; Chronic=had at least one plasma cell; Chronic/acute=there was at least one plasma cell with neutrophils; Acute/premensis = samples with neutrophils without plasma cells", 
  
  # Trichomonas
  trich_En            = "Reported having been ever diagnosed with trich before enrollment (excluding dx at enrollment) (0=no/1=yes)",
  tx_vag_En           = "Vaginal trich diagnosis enrollment",
  tx_vag_1m           = "Vaginal trich diagnosis 1m",
  tx_vag_4m           = "Vaginal trich diagnosis 4m",
  tx_vag_8m           = "Vaginal trich diagnosis 8m",
  tx_vag_12m          = "Vaginal trich diagnosis 12m",
  trich_v2_1m         = "Reported trich diagnosis since last visit 1m",
  trich_v2_4m         = "Reported trich diagnosis since last visit 4m",
  trich_v2_8m         = "Reported trich diagnosis since last visit 8m",
  trich_v2_12m        = "Reported trich diagnosis since last visit 12m",
  tv_interim_1m       = "Interim trich status 1m",
  tv_interim_4m       = "Interim trich status 4m",
  tv_interim_8m       = "Interim trich status 8m",
  tv_interim_12m      = "Interim trich status 12m",
  trich_burden        = "Trichomonas burden (qPCR)",
  
  # Gonorrhea - prior to enrollment
  gonn        = "Reported having been ever diagnosed with gonorrhea before enrollment (excluding dx at enrollment) (0=no/1=yes)",
  numtimes_gc   = "Participant’s report at enrollment of the number of times they had gonorrhea (excluding any diagnosis at enrollment)",
  daterecent_gc_days = "Days since most recent Gonorrhea infection at enrollment",
  daterecent_gc_years = "Years since most recent Gonorrhea infection at enrollment",
  
  
  # Gonorrhea - reports of a diagnosis at En and f/u
  gonn_v2_1m  = "Reported gonorrhea dx since last visit (0=no/1=yes)",
  gonn_v2_4m  = "Reported gonorrhea dx since last visit (0=no/1=yes)",
  gonn_v2_8m  = "Reported gonorrhea dx since last visit (0=no/1=yes)",
  gonn_v2_12m = "Reported gonorrhea dx since last visit (0=no/1=yes)",
  
  # Gonorrhea site diagnostics and combined
  # MWRI vaginal gonorrhea
  gc_vag_en   = "MWRI vag gonn result enrollment",
  gc_vag_1m   = "MWRI vag gonn result 1m",
  gc_vag_4m   = "MWRI vag gonn result 4m",
  gc_vag_8m   = "MWRI vag gonn result 8m",
  gc_vag_12m  = "MWRI vag gonn result 12m",
  
  # Presby vaginal gonorrhea
  gc_vag_prsby_en   = "Presby vag gonn result enrollment",
  gc_vag_prsby_1m   = "Presby vag gonn result 1m",
  gc_vag_prsby_4m   = "Presby vag gonn result 4m",
  gc_vag_prsby_8m   = "Presby vag gonn result 8m",
  gc_vag_prsby_12m  = "Presby vag gonn result 12m",
  
  # Combined diagnostic
  gc_vag_bothcenters_en   = "Vaginal gonorrhea combined diagnostic enrollment",
  gc_vag_bothcenters_1m   = "Vaginal gonorrhea combined diagnostic 1m",
  gc_vag_bothcenters_4m   = "Vaginal gonorrhea combined diagnostic 4m",
  gc_vag_bothcenters_8m   = "Vaginal gonorrhea combined diagnostic 8m",
  gc_vag_bothcenters_12m  = "Vaginal gonorrhea combined diagnostic 12m",
  
  #Rectal gonn
  gc_rectal_en   = "Rectal gonn at enrollment",
  gc_rectal_1m   = "Rectal gonn at 1 mo",
  gc_rectal_4m   = "Rectal gonn at 4 mo",
  gc_rectal_8m   = "Rectal gonn at 8 mo",
  gc_rectal_12m  = "Rectal gonn at 12 mo",
  
  #Endm gonn
  gc_endm   = "Endometrial gonn at enrollment",
 
  #Gonn inferred dx
  gc_vag_interim_1m  = "Interim gonorrhea status 1m",
  gc_vag_interim_4m  = "Interim gonorrhea status 4m",
  gc_vag_interim_8m  = "Interim gonorrhea status 8m",
  gc_vag_interim_12m = "Interim gonorrhea status 12m",
  
  #Gonn f/u pos or neg 
  gonn_fu = "Gonorrhea f/u negative or positive",
  
  #Gonorrhea status of sex partner(s) at enrollment and f/u
  # Enrollment
  gc      = "Sex partner diagnosed with gonorrhea in the past 3 months at enrollment (0=no/1=yes)",
  gc_tx   = "Infected partner was treated for gonorrhea (enrollment) (0=no/1=yes/2=unknown)",
  gc_self = "Participant was treated for gonorrhea (enrollment) (0=no/1=yes/2=got medication but did not take it or finish it)",
  
  # Follow-up 1m
  gc_v2_1m      = "Sex partner(s) diagnosed with gonorrhea since last visit (0=no/1=yes)",
  gc_tx_v2_1m   = "Partner was treated for gonorrhea (0=no/1=yes/2=unknown)",
  gc_self_v2_1m = "Participant was treated for gonorrhea (0=no/1=yes/2=got medication but did not take it or finish it)",
  
  # Follow-up 4m
  gc_v2_4m      = "Sex partner(s) diagnosed with gonorrhea since last visit (0=no/1=yes)",
  gc_tx_v2_4m   = "Partner was treated for gonorrhea (0=no/1=yes/2=unknown)",
  gc_self_v2_4m = "Participant was treated for gonorrhea (0=no/1=yes/2=got medication but did not take it or finish it)",
  
  # Follow-up 8m
  gc_v2_8m      = "Sex partner(s) diagnosed with gonorrhea since last visit (0=no/1=yes)",
  gc_tx_v2_8m   = "Partner was treated for gonorrhea (0=no/1=yes/2=unknown)",
  gc_self_v2_8m = "Participant was treated for gonorrhea (0=no/1=yes/2=got medication but did not take it or finish it)",
  
  # Follow-up 12m
  gc_v2_12m      = "Sex partner(s) diagnosed with gonorrhea since last visit (0=no/1=yes)",
  gc_tx_v2_12m   = "Partner was treated for gonorrhea (0=no/1=yes/2=unknown)",
  gc_self_v2_12m = "Participant was treated for gonorrhea (0=no/1=yes/2=got medication but did not take it or finish it)",
  
  #PID 
  pelv            = "Reported having been diagnosed with PID before enrollment (0=no/1=yes)",
  numtimes_pid    = "Participant’s report of number of PID episodes before enrollment",
  daterecent_pid_days  = "Days since most recent PID infection at enrollment",
  daterecent_pid_years  = "Years since most recent PID infection at enrollment",
  
  #BV
  bv          = "Ever diagnosed with BV before enrollment (0=no/1=yes)",
  bv_v2_1m    = "Reported BV dx since last visit (0=no/1=yes)",
  bv_v2_4m    = "Reported BV dx since last visit (0=no/1=yes)",
  bv_v2_8m    = "Reported BV dx since last visit (0=no/1=yes)",
  bv_v2_12m   = "Reported BV dx since last visit (0=no/1=yes)",
  #bv gram stain
  gs_vag_en   = "Vaginal gram stain at enrollment",
  gs_vag_1m   = "Vaginal gram stain at 1 mo",
  gs_vag_4m   = "Vaginal gram stain at 4 mo",
  gs_vag_8m   = "Vaginal gram stain at 8 mo",
  gs_vag_12m  = "Vaginal gram stain at 12 mo",
  
  #yeast infection
  yeast        = "Ever diagnosed with yeast infection before enrollment (0=no/1=yes)",
  yeast_v2_1m  = "Reported yeast dx since last visit (0=no/1=yes)",
  yeast_v2_4m  = "Reported yeast dx since last visit (0=no/1=yes)",
  yeast_v2_8m  = "Reported yeast dx since last visit (0=no/1=yes)",
  yeast_v2_12m = "Reported yeast dx since last visit (0=no/1=yes)",
  
  #Gen herpes
  ghsv        = "Ever diagnosed with genital herpes before enrollment (0=no/1=yes)",
  ghsv_v2_1m  = "Reported genital herpes dx since last visit (0=no/1=yes)",
  ghsv_v2_4m  = "Reported genital herpes dx since last visit (0=no/1=yes)",
  ghsv_v2_8m  = "Reported genital herpes dx since last visit (0=no/1=yes)",
  ghsv_v2_12m = "Reported genital herpes dx since last visit (0=no/1=yes)",
  
  #Gen warts
  ghv = "Genital warts prior to enrollment (0=no/1=yes)",
  
  #Abnormal pap smear
  pap        = "Ever had an abnormal pap smear before enrollment (0=no/1=yes)",
  pap_v2_1m  = "Reported abnormal pap smear since last visit (0=no/1=yes)",
  pap_v2_4m  = "Reported abnormal pap smear since last visit (0=no/1=yes)",
  pap_v2_8m  = "Reported abnormal pap smear since last visit (0=no/1=yes)",
  pap_v2_12m = "Reported abnormal pap smear since last visit (0=no/1=yes)",
  
  #Nongonococcal Urethritis
  ngu         = "Sex partner(s) diagnosed with nongonococcal urethritis in the past 3 months at enrollment (0=no/1=yes)",
  ngu_prt_tx  = "Partner was treated for nongonococcal urethritis (0=no/1=yes/2=unknown)",
  
  ngu_v2_1m        = "Sex partner(s) diagnosed with nongonococcal urethritis since last visit (0=no/1=yes)",
  ngu_prt_tx_v2_1m = "Partner was treated for nongonococcal urethritis (0=no/1=yes/2=unknown)",
  
  ngu_v2_4m        = "Sex partner(s) diagnosed with nongonococcal urethritis since last visit (0=no/1=yes)",
  ngu_prt_tx_v2_4m = "Partner was treated for nongonococcal urethritis (0=no/1=yes/2=unknown)",
  
  ngu_v2_8m        = "Sex partner(s) diagnosed with nongonococcal urethritis since last visit (0=no/1=yes)",
  ngu_prt_tx_v2_8m = "Partner was treated for nongonococcal urethritis (0=no/1=yes/2=unknown)",
  
  ngu_v2_12m        = "Sex partner(s) diagnosed with nongonococcal urethritis since last visit (0=no/1=yes)",
  ngu_prt_tx_v2_12m = "Partner was treated for nongonococcal urethritis (0=no/1=yes/2=unknown)",
  
  #Contraception and pregnancy prevention
  bc_hormone_en    = "Are you currently using any of the following hormonal methods of birth control? (at enrollment)",
  bc_hormone_1m = "Since last visit, have you used or are currently using any of the following hormonal methods of birth control?",
  bc_hormone_4m  = "Since last visit, have you used or are currently using any of the following hormonal methods of birth control?",
  bc_hormone_8m  = "Since last visit, have you used or are currently using any of the following hormonal methods of birth control?",
  bc_hormone_12m = "Since last visit, have you used or are currently using any of the following hormonal methods of birth control?",
  
  bc_pregprev_en = "Are you currently using any of the following to prevent pregnancy? (at enrollment)",
  bc_pregprev_1m = "Since last visit, have you used or are currently using any of the following to prevent pregnancy?" ,
  bc_pregprev_4m = "Since last visit, have you used or are currently using any of the following to prevent pregnancy?", 
  bc_pregprev_8m = "Since last visit, have you used or are currently using any of the following to prevent pregnancy?" ,
  bc_pregprev_12m = "Since last visit, have you used or are currently using any of the following to prevent pregnancy?" ,
  
  condom_freq       = "Out of the last 10 times you had sex, how often did you use condoms?",
  condom_freq_v2_1m = "Out of the last 10 times you had sex, how often did you use condoms?",
  condom_freq_v2_4m = "Out of the last 10 times you had sex, how often did you use condoms?",
  condom_freq_v2_8m = "Out of the last 10 times you had sex, how often did you use condoms?",
  condom_freq_v2_12m = "Out of the last 10 times you had sex, how often did you use condoms?",

  #Sexual history 
  sexmenwmb   = "Do you or have you had sex with men, women, or both? (1=men/2=women/3=both)",
  sexmenwmb_v2_1m = "Since your last visit, have you had sex with men, women, or both? (1=men/2=women/3=both/4=NA)",
  sexmenwmb_v2_4m = "Since your last visit, have you had sex with men, women, or both? (1=men/2=women/3=both/4=NA)",
  sexmenwmb_v2_8m = "Since your last visit, have you had sex with men, women, or both? (1=men/2=women/3=both/4=NA)",
  sexmenwmb_v2_12m = "Since your last visit, have you had sex with men, women, or both? (1=men/2=women/3=both/4=NA)",
  
  ma_30       = "How many male partners have you had in the past 30 days (0=0/1=1/2=2-4/3=5-10/4=11+/5=prefer not to answer)",
  ma_3        = "How many male partners have you had in the past 3 months (0=0/1=1/2=2-4/3=5-10/4=11+/5=prefer not to answer)",
  ma_12       = "How many male partners have you had in the past 12 months (0=0/1=1/2=2-4/3=5-10/4=11+/5=prefer not to answer)",
  ma_lifet    = "How many male partners have you had in your lifetime (0=0/1=1/2=2-4/3=5-10/4=11+/5=prefer not to answer)",
  male_lastsex_days = "Days since last time participant had vaginal sex with a male (enrollment)", 
  male_lastsex_years = "Years since last time participant had vaginal sex with a male (enrollment)",
  ma_slv_v2_1m = "How many male partners have you had since your last visit (1=0/2=1/3=2-4/4=5-10/5=11+/6=prefer not to answer)",
  ma_slv_v2_4m = "How many male partners have you had since your last visit (1=0/2=1/3=2-4/4=5-10/5=11+/6=prefer not to answer)",
  ma_slv_v2_8m = "How many male partners have you had since your last visit (1=0/2=1/3=2-4/4=5-10/5=11+/6=prefer not to answer)",
  ma_slv_v2_12m = "How many male partners have you had since your last visit (1=0/2=1/3=2-4/4=5-10/5=11+/6=prefer not to answer)",
  
  conlastsex  = "Did you use a condom the last time you had vaginal sex? (0=no/1=yes)",
  conlastsex_v2_1m = "Did you use a condom the last time you had vaginal sex? (0=no/1=yes)",
  conlastsex_v2_4m = "Did you use a condom the last time you had vaginal sex? (0=no/1=yes)",
  conlastsex_v2_8m = "Did you use a condom the last time you had vaginal sex? (0=no/1=yes)",
  conlastsex_v2_12m = "Did you use a condom the last time you had vaginal sex? (0=no/1=yes)",
  
  newmen30d   = "How many NEW male partners have you had sex with in the past 30 days?",
  newmen12m   = "How many NEW male partners have you had sex with in the past 12 months?",
  newmen30d_v2_1m = "Since your last visit, how many NEW male partners have you had sex with?",
  newmen30d_v2_4m  = "Since your last visit, how many NEW male partners have you had sex with?",
  newmen30d_v2_8m  = "Since your last visit, how many NEW male partners have you had sex with?",
  newmen30d_v2_12m  = "Since your last visit, how many NEW male partners have you had sex with?",
  
  fsx30d      = "How many female partners have you had in the past 30 days (0=0/1=1/2=2-4/3=5-10/4=11+/5=prefer not to answer)",
  fsx3mo      = "How many female partners have you had in the past 3 months (0=0/1=1/2=2-4/3=5-10/4=11+/5=prefer not to answer)",
  fsx12m      = "How many female partners have you had in the past 12 months (0=0/1=1/2=2-4/3=5-10/4=11+/5=prefer not to answer)",
  fsxlife     = "How many female partners have you had in your lifetime (0=0/1=1/2=2-4/3=5-10/4=11+/5=prefer not to answer)",
  fsx_slv_v2_1m = "How many female partners have you had since your last visit (0=0/1=1/2=2-4/3=5-10/4=11+/5=prefer not to answer)",
  fsx_slv_v2_4m = "How many female partners have you had since your last visit (0=0/1=1/2=2-4/3=5-10/4=11+/5=prefer not to answer)",
  fsx_slv_v2_8m = "How many female partners have you had since your last visit (0=0/1=1/2=2-4/3=5-10/4=11+/5=prefer not to answer)",
  fsx_slv_v2_12m = "How many female partners have you had since your last visit (0=0/1=1/2=2-4/3=5-10/4=11+/5=prefer not to answer)",
  
  newwomen30d = "How many NEW female partners have you had sex with in the past 30 days?",
  newwomen12m = "How many NEW female partners have you had sex with in the past 12 months?",
  newwomen30d_v2_1m = "Since your last visit, how many NEW female partners have you had sex with?",
  newwomen30d_v2_4m = "Since your last visit, how many NEW female partners have you had sex with?",
  newwomen30d_v2_8m = "Since your last visit, how many NEW female partners have you had sex with?",
  newwomen30d_v2_12m = "Since your last visit, how many NEW female partners have you had sex with?",
  
  oral_sex    = "Have you ever received oral sex? (0=no/1=yes, in the past 30 days/2=yes, more than 30 days ago/3=once or twice in lifetime, long ago)",
  ma_uncirc   = "Have you ever had sex with an uncircumcised partner? (0=no/1=yes, in the past 30 days/2=yes, more than 30 days ago/3=once or twice in lifetime, long ago)",
  anal_sex    = "Have you ever had anal (rectal) sex? (0=no/1=yes, in the past 30 days/2=yes, more than 30 days ago/3=once or twice in lifetime, long ago)",
  vagafrec    = "Did you have vaginal sex after rectal sex? (0=no/1=yes)",
  conrecvag   = "Did you use a condom when you had vaginal sex after rectal sex? (0=no/1=yes)",
  
  # oral sex
  oral_sex        = "Have you ever received oral sex? (0=no/1=yes, in the past 30 days/2=yes, more than 30 days ago/3=once or twice in lifetime, long ago)",
  oral_sex_v2_1m  = "Since your last visit, have you received oral sex? (0=no/1=yes, in the past 30 days/2=yes, more than 30 days ago/3=unsure)",
  oral_sex_v2_4m  = "Since your last visit, have you received oral sex? (0=no/1=yes, in the past 30 days/2=yes, more than 30 days ago/3=unsure)",
  oral_sex_v2_8m  = "Since your last visit, have you received oral sex? (0=no/1=yes, in the past 30 days/2=yes, more than 30 days ago/3=unsure)",
  oral_sex_v2_12m = "Since your last visit, have you received oral sex? (0=no/1=yes, in the past 30 days/2=yes, more than 30 days ago/3=unsure)",
  
  # uncircumcised male partner
  ma_uncirc        = "Have you ever had sex with an uncircumcised partner? (0=no/1=yes, in the past 30 days/2=yes, more than 30 days ago/3=once or twice in lifetime, long ago)",
  ma_uncirc_v2_1m  = "Since your last visit, have you had sex with an uncircumcised partner? (0=no/1=yes, in the past 30 days/2=yes, more than 30 days ago/3=unsure)",
  ma_uncirc_v2_4m  = "Since your last visit, have you had sex with an uncircumcised partner? (0=no/1=yes, in the past 30 days/2=yes, more than 30 days ago/3=unsure)",
  ma_uncirc_v2_8m  = "Since your last visit, have you had sex with an uncircumcised partner? (0=no/1=yes, in the past 30 days/2=yes, more than 30 days ago/3=unsure)",
  ma_uncirc_v2_12m = "Since your last visit, have you had sex with an uncircumcised partner? (0=no/1=yes, in the past 30 days/2=yes, more than 30 days ago/3=unsure)",
  
  # anal sex
  anal_sex        = "Have you ever had anal (rectal) sex? (0=no/1=yes, in the past 30 days/2=yes, more than 30 days ago/3=once or twice in lifetime, long ago)",
  anal_sex_v2_1m  = "Since your last visit, have you had anal (rectal) sex? (0=no/1=yes, in the past 30 days/2=yes, more than 30 days ago/3=unsure)",
  anal_sex_v2_4m  = "Since your last visit, have you had anal (rectal) sex? (0=no/1=yes, in the past 30 days/2=yes, more than 30 days ago/3=unsure)",
  anal_sex_v2_8m  = "Since your last visit, have you had anal (rectal) sex? (0=no/1=yes, in the past 30 days/2=yes, more than 30 days ago/3=unsure)",
  anal_sex_v2_12m = "Since your last visit, have you had anal (rectal) sex? (0=no/1=yes, in the past 30 days/2=yes, more than 30 days ago/3=unsure)",
  
  # vaginal-after-rectal sex
  vagafrec        = "Did you have vaginal sex after rectal sex? (0=no/1=yes)",
  vagafrec_v2_1m  = "Since your last visit, did you have vaginal sex after rectal sex? (0=no/1=yes)",
  vagafrec_v2_4m  = "Since your last visit, did you have vaginal sex after rectal sex? (0=no/1=yes)",
  vagafrec_v2_8m  = "Since your last visit, did you have vaginal sex after rectal sex? (0=no/1=yes)",
  vagafrec_v2_12m = "Since your last visit, did you have vaginal sex after rectal sex? (0=no/1=yes)",
  
  # condom use during vaginal-after-rectal sex
  conrecvag        = "Did you use a condom when you had vaginal sex after rectal sex? (0=no/1=yes)",
  conrecvag_v2_1m  = "Since your last visit, did you use a condom when you had vaginal sex after rectal sex? (0=no/1=yes)",
  conrecvag_v2_4m  = "Since your last visit, did you use a condom when you had vaginal sex after rectal sex? (0=no/1=yes)",
  conrecvag_v2_8m  = "Since your last visit, did you use a condom when you had vaginal sex after rectal sex? (0=no/1=yes)",
  conrecvag_v2_12m = "Since your last visit, did you use a condom when you had vaginal sex after rectal sex? (0=no/1=yes)",
  
  douch       = "Have you ever douched? (0=no/1=yes, in the past 30 days/2=yes, more than 30 days ago/3=once or twice in lifetime, long ago)",
  freqdche    = "How often do you douche? (1=more than monthly/2=monthly/3=less than monthly/4=as needed)",
  whydche     = "Why do you douche? (1=general hygiene/2=after menses or bleeding/3=after sex/4=when I notice vaginal discharge or color/5=other)",
  
  # sex during period (30 days)
  sex30mens        = "Within the past 30 days, have you had sex during your period? (0=no/1=yes)",
  sex30mens_v2_1m  = "Since your last visit, have you had sex during your period? (0=no/1=yes)",
  sex30mens_v2_4m  = "Since your last visit, have you had sex during your period? (0=no/1=yes)",
  sex30mens_v2_8m  = "Since your last visit, have you had sex during your period? (0=no/1=yes)",
  sex30mens_v2_12m = "Since your last visit, have you had sex during your period? (0=no/1=yes)",
  
  # bleeding with sex (30 days)
  bleed30sex        = "Within the past 30 days, have you had bleeding with sex/intercourse? (0=no/1=yes)",
  bleed30sex_v2_1m  = "Since your last visit, have you had bleeding with sex/intercourse? (0=no/1=yes)",
  bleed30sex_v2_4m  = "Since your last visit, have you had bleeding with sex/intercourse? (0=no/1=yes)",
  bleed30sex_v2_8m  = "Since your last visit, have you had bleeding with sex/intercourse? (0=no/1=yes)",
  bleed30sex_v2_12m = "Since your last visit, have you had bleeding with sex/intercourse? (0=no/1=yes)",
  
  # mid‑cycle spotting (30 days)
  mid30spot        = "Within the past 30 days, have you had bleeding while you were not on your period? (0=no/1=yes)",
  mid30spot_v2_1m  = "Since your last visit, have you had bleeding while you were not on your period? (0=no/1=yes)",
  mid30spot_v2_4m  = "Since your last visit, have you had bleeding while you were not on your period? (0=no/1=yes)",
  mid30spot_v2_8m  = "Since your last visit, have you had bleeding while you were not on your period? (0=no/1=yes)",
  mid30spot_v2_12m = "Since your last visit, have you had bleeding while you were not on your period? (0=no/1=yes)",
  
  # pelvic pain during sex (30 days)
  pain30sex        = "Within the past 30 days, have you had pelvic pain while you were having sex? (0=no/1=yes)",
  pain30sex_v2_1m  = "Since your last visit, have you had pelvic pain while you were having sex? (0=no/1=yes)",
  pain30sex_v2_4m  = "Since your last visit, have you had pelvic pain while you were having sex? (0=no/1=yes)",
  pain30sex_v2_8m  = "Since your last visit, have you had pelvic pain while you were having sex? (0=no/1=yes)",
  pain30sex_v2_12m = "Since your last visit, have you had pelvic pain while you were having sex? (0=no/1=yes)",
  
  # pelvic pain unrelated to sex (30 days)
  plv30reg        = "Within the past 30 days, have you had pelvic pain unrelated to sex? (0=no/1=yes)",
  plv30reg_v2_1m  = "Since your last visit, have you had pelvic pain unrelated to sex? (0=no/1=yes)",
  plv30reg_v2_4m  = "Since your last visit, have you had pelvic pain unrelated to sex? (0=no/1=yes)",
  plv30reg_v2_8m  = "Since your last visit, have you had pelvic pain unrelated to sex? (0=no/1=yes)",
  plv30reg_v2_12m = "Since your last visit, have you had pelvic pain unrelated to sex? (0=no/1=yes)"

  )

label_row <- sapply(all_cols, function(col) {
  if (col %in% names(label_map)) label_map[[col]] else col
})
writeData(wb, sheet = "Summary", x = as.data.frame(t(label_row)), startRow = 2, colNames = FALSE)

writeData(wb, sheet = "Summary", x = as.data.frame(t(all_cols)), startRow = 3, colNames = FALSE)


summ_export <- summ
summ_export[] <- lapply(summ_export, function(x) {
  if (is.numeric(x)) {
    return(ifelse(is.na(x), "NA", as.character(x)))
  } else {
    return(ifelse(is.na(x), "NA", x))
  }
})

writeData(wb, sheet = "Summary", x = summ_export, startRow = 4, colNames = FALSE)

# Apply scientific formatting to MG burden columns
addStyle(wb, sheet = "Summary",
         style = createStyle(numFmt = "0.00E+00"),
         cols = which(names(summ) %in% c("CT_CB_burden", "CT_VS_burden", "MG_CB_burden", "MG_VS_burden", "trich_burden")),
         rows = 4:(nrow(summ) + 3),
         gridExpand = TRUE)

#STEP 15
saveWorkbook(wb, "TRAC2 summary sheet.xlsx", overwrite = TRUE)

