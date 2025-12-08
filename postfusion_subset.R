
############################ LOAD PACKAGES ##########################
library(tidyverse) 
library(readxl) 
library(writexl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(purrr)
library(hms)
########################### IMPORT FILES ###############################

# folder path
folder <- "C:/Users/snmum/OneDrive/PhD/NVBP/NVBP_Rproj/Copy_NVBP_postfusion/"

# list all .xlsx files
files <- list.files(path = folder, pattern = "\\.xlsx$", full.names = TRUE)
files

# grooming and scan data sheets from all files
may1groom <- readxl::read_excel("C:/Users/snmum/OneDrive/PhD/NVBP/NVBP_Rproj/Copy_NVBP_postfusion/NV_Focal_1-7_May_2025_DE_GGM_DC_GER.xlsx", 
                           sheet = "NVT_TMF_Focal_Grooming_GGM")
may1scans <- readxl::read_excel("C:/Users/snmum/OneDrive/PhD/NVBP/NVBP_Rproj/Copy_NVBP_postfusion/NV_Focal_1-7_May_2025_DE_GGM_DC_GER.xlsx", 
                           sheet = "NVT_TMF_Behavior_Scans_GGM")
june2groom <- readxl::read_excel("C:/Users/snmum/OneDrive/PhD/NVBP/NVBP_Rproj/Copy_NVBP_postfusion/NV_Focal_02_June_2025_DE_IBA_DC_SYZ.xlsx", 
                            sheet = "NVT_TMF_Focal_Grooming_INITIALS")
june2scans <- readxl::read_excel("C:/Users/snmum/OneDrive/PhD/NVBP/NVBP_Rproj/Copy_NVBP_postfusion/NV_Focal_02_June_2025_DE_IBA_DC_SYZ.xlsx", 
                            sheet = "NVT_TMF_Behavior_Scans_INITIALS")
may8groom <- readxl::read_excel("C:/Users/snmum/OneDrive/PhD/NVBP/NVBP_Rproj/Copy_NVBP_postfusion/NV_Focal_8-9_May_2025_DE_GGM_DC_GER.xlsx", 
                          sheet = "NVT_TMF_Focal_Grooming_GGM")
may8scans <- readxl::read_excel("C:/Users/snmum/OneDrive/PhD/NVBP/NVBP_Rproj/Copy_NVBP_postfusion/NV_Focal_8-9_May_2025_DE_GGM_DC_GER.xlsx",
                          sheet = "NVT_TMF_Behavior_Scans_GGM")
may12groom <- readxl::read_excel("C:/Users/snmum/OneDrive/PhD/NVBP/NVBP_Rproj/Copy_NVBP_postfusion/NV_Focal_12-16_May_2025_DE_GER_DC_GGM.xlsx", 
                           sheet = "GR_NVT_TMF_Focal_Grooming_GER")
may12scans <- readxl::read_excel("C:/Users/snmum/OneDrive/PhD/NVBP/NVBP_Rproj/Copy_NVBP_postfusion/NV_Focal_12-16_May_2025_DE_GER_DC_GGM.xlsx", 
                           sheet = "NVT_TMF_Behavior_Scans_GER")
june13groom <- readxl::read_excel("C:/Users/snmum/OneDrive/PhD/NVBP/NVBP_Rproj/Copy_NVBP_postfusion/NV_Focal_13_June_2025_DE_GGM_DC_GER.xlsx", 
                            sheet = "NVT_TMF_Focal_Grooming_GGM")
june13scans <- readxl::read_excel("C:/Users/snmum/OneDrive/PhD/NVBP/NVBP_Rproj/Copy_NVBP_postfusion/NV_Focal_13_June_2025_DE_GGM_DC_GER.xlsx", 
                                  sheet = "NVT_TMF_Behavior_Scans_GGM")
june1316groom <- readxl::read_excel("C:/Users/snmum/OneDrive/PhD/NVBP/NVBP_Rproj/Copy_NVBP_postfusion/NV_Focal_13-16_June_2025_DE_SYZ_DC_IBA.xlsx", 
                              sheet = "NVT_TMF_Focal_Grooming_INITIALS")
june1316scans <- readxl::read_excel("C:/Users/snmum/OneDrive/PhD/NVBP/NVBP_Rproj/Copy_NVBP_postfusion/NV_Focal_13-16_June_2025_DE_SYZ_DC_IBA.xlsx",
                              sheet = "NVT_TMF_Behavior_Scans_INITIALS")
june16groom <- readxl::read_excel("C:/Users/snmum/OneDrive/PhD/NVBP/NVBP_Rproj/Copy_NVBP_postfusion/NV_Focal_16Jun-19Jun_2025_DE_GER_DC_GGM.xlsx", 
                           sheet = "GR_NVT_TMF_Focal_Grooming_GER")
june16scans <- readxl::read_excel("C:/Users/snmum/OneDrive/PhD/NVBP/NVBP_Rproj/Copy_NVBP_postfusion/NV_Focal_16Jun-19Jun_2025_DE_GER_DC_GGM.xlsx", 
                           sheet = "NVT_TMF_Behavior_Scans_GER")
april17groom <- readxl::read_excel("C:/Users/snmum/OneDrive/PhD/NVBP/NVBP_Rproj/Copy_NVBP_postfusion/NV_Focal_17-18_April_2025_DE_GGM_DC_GER.xlsx", 
                            sheet = "NVT_TMF_Focal_Grooming_GGM")
april17scans <- readxl::read_excel("C:/Users/snmum/OneDrive/PhD/NVBP/NVBP_Rproj/Copy_NVBP_postfusion/NV_Focal_17-18_April_2025_DE_GGM_DC_GER.xlsx", 
                                   sheet = "NVT_TMF_Behavior_Scans_GGM")
may19groom <- readxl::read_excel("C:/Users/snmum/OneDrive/PhD/NVBP/NVBP_Rproj/Copy_NVBP_postfusion/NV_Focal_19-22_May_2025_DE_GGM_DC_GER.xlsx",
                           sheet = "NVT_TMF_Focal_Grooming_GGM")
may19scans <- readxl::read_excel("C:/Users/snmum/OneDrive/PhD/NVBP/NVBP_Rproj/Copy_NVBP_postfusion/NV_Focal_19-22_May_2025_DE_GGM_DC_GER.xlsx",
                            sheet = "NVT_TMF_Behavior_Scans_GGM")
april22groom <- readxl::read_excel("C:/Users/snmum/OneDrive/PhD/NVBP/NVBP_Rproj/Copy_NVBP_postfusion/NV_Focal_22-30_April_2025_DE_GER_DC_GGM.xlsx",
                             sheet = "GR_NVT_TMF_Focal_Grooming_GER")
april22scans <- readxl::read_excel("C:/Users/snmum/OneDrive/PhD/NVBP/NVBP_Rproj/Copy_NVBP_postfusion/NV_Focal_22-30_April_2025_DE_GER_DC_GGM.xlsx",
                             sheet = "NVT_TMF_Behavior_Scans_GER")
march27groom <- readxl::read_excel("C:/Users/snmum/OneDrive/PhD/NVBP/NVBP_Rproj/Copy_NVBP_postfusion/NV_Focal_27March 27-15April_2025_DE_GER_DC_GGM.xlsx",
                            sheet = "GR_NVT_TMF_Focal_Grooming_GER")
march27scans <- readxl::read_excel("C:/Users/snmum/OneDrive/PhD/NVBP/NVBP_Rproj/Copy_NVBP_postfusion/NV_Focal_27March 27-15April_2025_DE_GER_DC_GGM.xlsx",
                                   sheet = "NVT_TMF_Behavior_Scans_GER")
june213groom <- readxl::read_excel("C:/Users/snmum/OneDrive/PhD/NVBP/NVBP_Rproj/Copy_NVBP_postfusion/NV_Focal_Data_02-13_June_DE_IBA_DC_SYZ.xlsx",
                            sheet = "NVT_TMF_Focal_Grooming_INITIALS")
june213scans <- readxl::read_excel("C:/Users/snmum/OneDrive/PhD/NVBP/NVBP_Rproj/Copy_NVBP_postfusion/NV_Focal_Data_02-13_June_DE_IBA_DC_SYZ.xlsx",
                            sheet = "NVT_TMF_Behavior_Scans_INITIALS")

########################## groom clean before bind ##############################

all_groom_list <- list(may1groom, june2groom, may8groom, may12groom, june13groom, june1316groom,
                   june16groom, april17groom, may19groom, april22groom, march27groom, june213groom)

# getting rid of rows of NA
all_groom_list<- lapply(all_groom_list, function(df) {
  df %>% filter(!if_all(everything(), is.na))
})


# Function to replace "in prog"/"In prog" with TMF times
clean_inprog_times <- function(df) {
  df %>%
    mutate(
      Start_time_clean = case_when(
        Start_time %in% c("in prog", "In prog") ~ as.character(TMF_start_time),
        TRUE ~ as.character(Start_time)
      ),
      End_time_clean = case_when(
        End_time %in% c("in prog", "In prog") ~ as.character(TMF_end_time),
        TRUE ~ as.character(End_time)
      )
    )
}
all_groom_list <- map(all_groom_list, clean_inprog_times)

# Function to convert mixed times
convert_mixed_time <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  
  #Try lubridate::hms (HH:MM:SS)
  try_hms <- suppressWarnings(as.numeric(lubridate::hms(x)))
  if (!is.na(try_hms)) return(try_hms)
  
  #Try numeric fraction of day
  num <- suppressWarnings(as.numeric(x))
  if (!is.na(num)) return(num * 86400)
  
  #Try Excel date-time string (like "1899-12-31 10:15:07")
  if (grepl("^1899-12-31", x)) {
    # Extract HH:MM:SS part
    time_part <- sub("1899-12-31\\s+", "", x)
    try_excel_hms <- suppressWarnings(as.numeric(lubridate::hms(time_part)))
    if (!is.na(try_excel_hms)) return(try_excel_hms)
  }
  
  #Anything else → NA
  return(NA_real_)
}

# function to process one data frame
process_groom_df <- function(df) {
  df %>%
    mutate(
      Start_time_sec = sapply(Start_time_clean, convert_mixed_time),
      End_time_sec   = sapply(End_time_clean, convert_mixed_time),
      Start_time_hms = hms(seconds = Start_time_sec),
      End_time_hms   = hms(seconds = End_time_sec),
      Duration_sec   = End_time_sec - Start_time_sec
    )
}

all_groom_list <- lapply(all_groom_list, process_groom_df)

# groomer and recipient across all dfs
compute_groomer_recipient <- function(df) {
  df %>%
    rowwise() %>%
    mutate(
      Groomer = case_when(
        Direction == "GG" ~ Focal_ID,
        Direction == "BG" ~ Partner_ID,
        Direction == "MG" ~ NA_character_,
        TRUE ~ NA_character_
      ),
      Recipient = case_when(
        Direction == "GG" ~ Partner_ID,
        Direction == "BG" ~ Focal_ID,
        Direction == "MG" ~ NA_character_,
        TRUE ~ NA_character_
      )
    ) %>%
    ungroup()
}

all_groom_list <- map(all_groom_list, compute_groomer_recipient)

# fix date issues
all_groom_list <- map(all_groom_list, ~ .x %>%
                        mutate(Date = as.character(Date))
)
standardize_dates <- function(df) {
  df %>%
    mutate(
      Date = parse_date_time(Date, orders = c("ymd", "d-b-Y")) %>% as.Date()
    )
}
all_groom_list <- map(all_groom_list, standardize_dates)

# clean columns 
cleaned_columns <- c(
  "Date",
  "Focal_ID",
  "Partner_ID",
  "Direction",
  "Groomer",
  "Recipient",
  "Start_time_hms",
  "End_time_hms",
  "Duration_sec"
)
all_groom_list = map(all_groom_list, ~ .x %>%
                         select(all_of(cleaned_columns))
)

# getting rid of questionable data (for now)
all_groom_list = lapply(all_groom_list, function(df) {
  df %>% filter(!is.na(Duration_sec),
                Duration_sec >= 0,
                Duration_sec <= 10000)
})

########################## Finally Bind! ################################
cleaned_postfusion_grooming <- bind_rows(all_groom_list)
postfusion_subset_groom = cleaned_postfusion_grooming %>% arrange(Date)

############################ Scan Clean #################################
all_scan_list = list(may1scans, june2scans, may8scans, may12scans, june13scans, june1316scans,
                     june16scans, april17scans, may19scans, april22scans, march27scans, june213scans)

# fix date issues
all_scan_list <- map(all_scan_list, ~ .x %>%
                        mutate(Date = as.character(Date))
)
all_scan_list <- map(all_scan_list, standardize_dates)

# clean columns 
cleaned_columns_scans <- c(
  "Date",
  "Focal_ID",
  "Focal_behavior"
)
all_scan_list = map(all_scan_list, ~ .x %>%
                       select(all_of(cleaned_columns_scans))
)

# getting rid of rows of NA
all_scan_list<- lapply(all_scan_list, function(df) {
  df %>% filter(!if_all(everything(), is.na))
})


############################ Bind Scans #################################
cleaned_postfusion_scans = bind_rows(all_scan_list)
postfusion_subset_scans = cleaned_postfusion_scans %>% arrange(Date)

########################### Analysis #####################################

# Postfusion Grooming Rate
postfusion_subset_groom_rate_df = postfusion_subset_scans %>%
  mutate(is_groom = Focal_behavior == "groom") %>%
  group_by(Focal_ID) %>%
  summarize(
    total_scans = n(),
    groom_scans = sum(is_groom, na.rm = TRUE),
    postfusion_grooming_rate = groom_scans / total_scans
  )

##### Dyad Rates ####

# total grooming duration per dyad during the prefusion period
dyad_summary2 = postfusion_subset_groom %>%
  filter(!is.na(Groomer) & !is.na(Recipient)) %>%
  group_by(Groomer, Recipient) %>%
  summarise(
    dyads_total_groom_secs = sum(Duration_sec, na.rm = TRUE),
    n_bouts = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(dyads_total_groom_secs))

# how much the Focal was observed grooming in total during the prefusion period
ID_groom_observed = postfusion_subset_groom %>%
  group_by(Groomer) %>%
  summarise(
    groomer_total_secs = sum(Duration_sec, na.rm = TRUE),
    .groups = "drop",
  )

# merge to get dyadic grooming rates
dyad_summary2 = dyad_summary2 %>%
  left_join(ID_groom_observed, by = "Groomer") %>%
  mutate(
    dyad_rates = dyads_total_groom_secs / groomer_total_secs
  )

# How much the dyad groomer is Giving Grooming (GG)
groomer_totals <- dyad_summary2 %>%
  group_by(Groomer) %>%
  summarise(dyad_total_GG = sum(dyads_total_groom_secs), .groups = "drop")

# How uch the dyad recipient is Being Groomed (BG)
recipient_totals <- dyad_summary2 %>%
  group_by(Recipient) %>%
  summarise(dyad_total_BG = sum(dyads_total_groom_secs), .groups = "drop")

# Calculate expected amount Groomer would be grooming Recipient based on totals
dyad_summary2 <- dyad_summary2 %>%
  left_join(groomer_totals, by = "Groomer") %>%
  left_join(recipient_totals, by = "Recipient") %>%
  mutate(
    expected_duration = (dyad_total_GG * dyad_total_BG) / sum(dyads_total_groom_secs),
    deviation = dyads_total_groom_secs - expected_duration
  )

postfusion_subset_dyadsummary = dyad_summary2 %>%
  select(
    Groomer,
    Recipient,
    dyads_total_groom_secs,
    dyad_total_GG, 
    dyad_total_BG,
    dyad_rates,
    expected_duration,
    deviation
  )

# plotting dyadic grooming relationships
postfusion_dyad_plot = dyad_summary2 %>%
  ggplot(aes(x = Groomer, y = Recipient, size = dyads_total_groom_secs, color = deviation)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient2(low = "blue", mid = "orchid", high = "red", midpoint = 0) +
  labs(title = "Dyadic Grooming Relationships Post Fusion (March - June 2025)",
       x = "Groomer",
       y = "Recipient",
       size = "Total Duration (seconds)",
       color = "Deviation from Expected") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(postfusion_dyad_plot)

