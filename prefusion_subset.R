
############################ LOAD PACKAGES ##########################
library(tidyverse) 
library(readxl) 
library(writexl)
library(ggplot2)
library(dplyr)
library(lubridate)
########################### IMPORT FILES ###############################
# plug in correct file path
file = "C:/Users/snmum/OneDrive/PhD/NVBP/NVBP_Rproj/Copy_NVBP_Prefusion.xlsx"
Grooming = readxl::read_xlsx(file, sheet="NVT_TMF_Focal_Grooming")

############################# Build Grooming DF #################################
grooming_df <- Grooming %>%
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

# replace "in prog/In prog" in Start_time with TMF_start_time
grooming_df <- grooming_df %>%
  mutate(
    Start_time_clean = case_when(
      Start_time %in% c("in prog", "In prog") ~ as.character(TMF_start_time),  
      TRUE ~ as.character(Start_time)                        
    )
  ) %>%
  
# replace "in prog/In prog" in End_time with TMF_end_time
  mutate(
    End_time_clean = case_when(
      End_time %in% c("in prog", "In prog") ~ as.character(TMF_end_time), 
      TRUE ~ as.character(End_time)                             
    )
  )
# convert Start_time and End_time to hms and calculate Duration in seconds
grooming_df <- grooming_df %>%
  mutate(
    Start_time_num = suppressWarnings(as.numeric(Start_time_clean)),
    End_time_num   = suppressWarnings(as.numeric(End_time_clean))
  ) %>%
  
  mutate(
    Start_time_hms = hms::hms(seconds = Start_time_num * 86400),
    End_time_hms   = hms::hms(seconds = End_time_num * 86400),
    Duration_sec   = as.numeric(End_time_hms - Start_time_hms)
  )

# new df with cleaned times and removing old columns
cleaned_prefusion_grooming = grooming_df %>%
  select(
    Date,
    Day,
    Month,
    Year,
    Focal_ID,
    Partner_ID,
    Direction,
    Groomer,
    Recipient,
    Start_time_hms,
    End_time_hms,
    Duration_sec
  )

prefusion_subset_groom = cleaned_prefusion_grooming %>%
  filter(Date >= as.Date("2024-06-01") & Date <= as.Date("2024-09-30")) %>%
  filter(Duration_sec >= 0 | is.na(Duration_sec))


############ Pulling in ScanSummary data for context ###############
ScanSummary<- read_xlsx(file, sheet="NVT_TMF_Behavior_Scans")

# convert ScanTime to hms
ScanSummary <- ScanSummary %>%
  mutate(
    ScanTime_hms = hms::as_hms(ScanTime)
  )
# clean behavior category to standardize
ScanSummary <- ScanSummary %>%
  mutate(Focal_behavior_clean = tolower(Focal_behavior))

cleaned_prefusion_scans <- ScanSummary %>%
  select(
    Date,
    Day,
    Month,
    Year,
    ScanTime_hms,
    Focal_ID,
    Focal_behavior_clean, 
    ID_2_if_social, 
    NN_ID)

# plot of monthly obs efforts: number of scans per month
ggplot(cleaned_prefusion_scans, aes(x = Date)) +
  geom_bar(fill = "orange") +
  labs(title = "Monthly Observation Effort (Number of Scans)",
       x = "Month",
       y = "Number of Scans") +
  theme_minimal()

#we see a substanial amount collected from June 24 - September 24
#this is a good amount of scans for pre-fusion period rates due to high n

# Filter to June–September 2024
prefusion_subset_scans = cleaned_prefusion_scans %>%
  filter(Date >= as.Date("2024-06-01") & Date <= as.Date("2024-09-30"))

# Prefusion Grooming Rate
prefusion_subset_groom_rate_df = prefusion_subset_scans %>%
  mutate(is_groom = Focal_behavior_clean == "groom") %>%
  group_by(Focal_ID) %>%
  summarize(
    total_scans = n(),
    groom_scans = sum(is_groom, na.rm = TRUE),
    prefusion_grooming_rate = groom_scans / total_scans
  )

# removing "SD" outlier (might be ES)
prefusion_subset_groom_rate_df <- prefusion_subset_groom_rate_df %>%
  filter(Focal_ID != "SD")


################### Analysis of Prefusion Subset #######################

######Dyad Rates#####
# total grooming duration per dyad during the prefusion period
dyad_summary = prefusion_subset_groom %>%
  filter(!is.na(Groomer) & !is.na(Recipient)) %>%
  group_by(Groomer, Recipient) %>%
  summarise(
    dyads_total_groom_secs = sum(Duration_sec, na.rm = TRUE),
    n_bouts = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(dyads_total_groom_secs))

# how much the Focal was observed grooming in total during the prefusion period
ID_groom_observed = prefusion_subset_groom %>%
  group_by(Groomer) %>%
  summarise(
    groomer_total_secs = sum(Duration_sec, na.rm = TRUE),
    .groups = "drop",
  )

# merge to get dyadic grooming rates
dyad_summary = dyad_summary %>%
  left_join(ID_groom_observed, by = "Groomer") %>%
  mutate(
    dyad_rates = dyads_total_groom_secs / groomer_total_secs
  )

# removing "SD" outlier (might be ES)
dyad_summary <- dyad_summary %>%
  filter(Groomer != "SD")


# How much the dyad groomer is Giving Grooming (GG)
groomer_totals <- dyad_summary %>%
  group_by(Groomer) %>%
  summarise(dyad_total_GG = sum(dyads_total_groom_secs), .groups = "drop")

# How uch the dyad recipient is Being Groomed (BG)
recipient_totals <- dyad_summary %>%
  group_by(Recipient) %>%
  summarise(dyad_total_BG = sum(dyads_total_groom_secs), .groups = "drop")


# Calculate expected amount Groomer would be grooming Recipient based on totals
dyad_summary = dyad_summary %>%
  left_join(groomer_totals, by = "Groomer") %>%
  left_join(recipient_totals, by = "Recipient") %>%
  mutate(
    expected_duration = (dyad_total_GG * dyad_total_BG) / sum(dyads_total_groom_secs),
    deviation = dyads_total_groom_secs - expected_duration
  )

prefusion_subset_dyadsummary = dyad_summary %>%
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

#some unclean data removed
remove_ids = c("SD", "GB")
prefusion_dyad_plot = dyad_summary %>% 
  filter(
    !Groomer %in% remove_ids, 
    !Recipient %in% remove_ids
  )

# plotting dyadic grooming relationships
prefusion_dyad_plot = prefusion_dyad_plot %>%
  ggplot(aes(x = Groomer, y = Recipient, size = dyads_total_groom_secs, color = deviation)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient2(
    low = "blue",
    mid = "orchid",
    high = "red",
    midpoint = 0,
    name = "Deviation from Expected") +
  scale_size_continuous(name = "Total Duration (seconds)") +
  guides(
    color = guide_colorbar(order = 1),
    size  = guide_legend(order = 2)) +
  labs(
    title = "Dyadic Grooming Relationships Pre Fusion (June–Sep 2024)",
    x = "Groomer",
    y = "Recipient") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.box = "vertical")

print(prefusion_dyad_plot)


