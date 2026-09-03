# ============================================================
# LIBRARIES
# ============================================================

library(dplyr)
library(lubridate)
library(tidyr)
library(readxl)


# ============================================================
# 1. PREPARE BED DATA
# ============================================================
# Convert bed snapshot date to month.
# Keep one observation per provider/month.
#
# If there are multiple bed observations within the same month,
# use the first available value for that provider/month.
# ============================================================

df_beds_monthly <- df_beds %>%
  
  mutate(
    Effective_Snapshot_Date = as.Date(
      Effective_Snapshot_Date
    ),
    
    join_month = floor_date(
      Effective_Snapshot_Date,
      "month"
    )
  ) %>%
  
  group_by(
    Organisation_Code,
    join_month
  ) %>%
  
  summarise(
    
    Total_Available_Beds = first(
      Total_Available_Beds[!is.na(Total_Available_Beds)],
      default = NA_real_
    ),
    
    Total_Occupied_Beds = first(
      Total_Occupied_Beds[!is.na(Total_Occupied_Beds)],
      default = NA_real_
    ),
    
    Pct_Occupied = first(
      Pct_Occupied[!is.na(Pct_Occupied)],
      default = NA_real_
    ),
    
    .groups = "drop"
  )


# ============================================================
# 2. JOIN BED DATA TO A&E DATA
# ============================================================

df_combined <- df_join %>%
  
  mutate(
    Month = floor_date(
      as.Date(Month),
      "month"
    )
  ) %>%
  
  left_join(
    
    df_beds_monthly,
    
    by = c(
      "Provider_Code" = "Organisation_Code",
      "Month" = "join_month"
    )
  ) %>%
  
  arrange(
    Provider_Code,
    Month
  ) %>%
  
  # Carry latest bed figures forward when a month
  # has no bed data.
  group_by(
    Provider_Code
  ) %>%
  
  fill(
    Total_Available_Beds,
    Total_Occupied_Beds,
    Pct_Occupied,
    .direction = "down"
  ) %>%
  
  ungroup()


# ============================================================
# 3. LOAD RTT PROVIDER DATA FROM EXCEL
# ============================================================

RTT_incomplete_provider_combined <- read_excel(
  "RTT_incomplete_provider_combined.xlsx"
)


# ============================================================
# 4. STANDARDISE RTT EXCEL DATE
# ============================================================

RTT_incomplete_provider_combined <- RTT_incomplete_provider_combined %>%
  
  mutate(
    
    Effective_Snapshot_Date = as.Date(
      paste0(
        "01/",
        Effective_Snapshot_Date
      ),
      format = "%d/%m/%Y"
    ),
    
    Effective_Snapshot_Date = floor_date(
      Effective_Snapshot_Date,
      "month"
    )
  )


# ============================================================
# 5. PREPARE RTT SQL DATA
# ============================================================

df_rtt_clean <- df_rtt %>%
  
  mutate(
    
    Effective_Snapshot_Date = floor_date(
      as.Date(Effective_Snapshot_Date),
      "month"
    )
  )


# ============================================================
# 6. COMBINE RTT SQL + RTT EXCEL
# ============================================================

df_rtt_combined <- df_rtt_clean %>%
  
  full_join(
    
    RTT_incomplete_provider_combined,
    
    by = c(
      "Provider_Org_Code",
      "Effective_Snapshot_Date"
    ),
    
    suffix = c(
      ".x",
      ".y"
    )
  ) %>%
  
  mutate(
    
    Total_Incomplete_Pathways = coalesce(
      Total_Incomplete_Pathways.x,
      Total_Incomplete_Pathways.y
    ),
    
    Pct_Within_18_Weeks = coalesce(
      Pct_Within_18_Weeks.x,
      Pct_Within_18_Weeks.y
    ),
    
    Sum_Within_18_Weeks = coalesce(
      Sum_Within_18_Weeks.x,
      Sum_Within_18_Weeks.y
    ),
    
    Pct_Over_52_Weeks = coalesce(
      Pct_Over_52_Weeks.x,
      Pct_Over_52_Weeks.y
    ),
    
    Sum_Over_52_Weeks = coalesce(
      Sum_Over_52_Weeks.x,
      Sum_Over_52_Weeks.y
    )
  ) %>%
  
  select(
    -ends_with(".x"),
    -ends_with(".y")
  ) %>%
  
  arrange(
    Provider_Org_Code,
    Effective_Snapshot_Date
  ) %>%
  
  # Carry latest RTT observation forward when
  # a provider/month is missing.
  group_by(
    Provider_Org_Code
  ) %>%
  
  fill(
    Total_Incomplete_Pathways,
    Pct_Within_18_Weeks,
    Sum_Within_18_Weeks,
    Pct_Over_52_Weeks,
    Sum_Over_52_Weeks,
    .direction = "down"
  ) %>%
  
  ungroup()


# ============================================================
# 7. JOIN RTT DATA TO MAIN DATASET
# ============================================================

df_final <- df_combined %>%
  
  mutate(
    Month = floor_date(
      as.Date(Month),
      "month"
    )
  ) %>%
  
  left_join(
    
    df_rtt_combined,
    
    by = c(
      "Provider_Code" = "Provider_Org_Code",
      "Month" = "Effective_Snapshot_Date"
    )
  ) %>%
  
  arrange(
    Provider_Code,
    Month
  )


# ============================================================
# 8. CHECK WHAT COLUMNS WE HAVE
# ============================================================

cat("\n========================================\n")
cat("COLUMNS IN DATASET BEFORE FINAL FILTER\n")
cat("========================================\n")

print(
  names(df_final)
)


# ============================================================
# 9. FILTER TO REQUIRED PROVIDERS
# ============================================================

provider_codes <- c(
  "RTK",
  "RF4",
  "RMC",
  "RAE",
  "RWY",
  "RGT",
  "RXP",
  "RJN",
  "RXR",
  "RDE",
  "RVR",
  "RDU",
  "RJ1",
  "RJZ",
  "R1K",
  "RWF",
  "R0A",
  "RBT",
  "RD8",
  "RNS",
  "RA2",
  "RHQ",
  "R0B",
  "RAS",
  "RQW",
  "RRV",
  "RKB",
  "RBK",
  "RGR",
  "REY",
  "RLQ",
  "RK9",
  "RWG",
  "RJR",
  "RNN",
  "R1H",
  "RQM",
  "RFS",
  "RTE",
  "RQX",
  "RJ2",
  "RPA",
  "RVW",
  "RX1",
  "RTH",
  "RHW",
  "RH8",
  "RAL",
  "RXK",
  "RTR",
  "RTP",
  "RNA",
  "RTD",
  "RFR",
  "RL4",
  "RTG",
  "RWE",
  "RTX",
  "RJE",
  "RWP",
  "RXW",
  "RKE",
  "RJ6",
  "RH5",
  "RCF",
  "RXL",
  "RP5",
  "RXC",
  "RR7",
  "RLT",
  "RGP",
  "REM",
  "RAJ",
  "RM1",
  "RTF",
  "REF",
  "RK5",
  "RJC",
  "RWJ",
  "RCX",
  "RA9",
  "RWD",
  "RYR",
  "RCB",
  "R8D",
  "RW1",
  "RXF",
  "RM3",
  "RHU",
  "RD1",
  "RMP",
  "RFF",
  "RC9",
  "RXQ",
  "RN7",
  "RWH",
  "RVV",
  "RN3",
  "RCD",
  "RWA",
  "RYJ",
  "RNQ",
  "RAX",
  "RXN",
  "RR8",
  "RBN",
  "RGN",
  "RJL",
  "RNZ",
  "RJ7",
  "RHM",
  "RRK",
  "RA7",
  "R0D",
  "RWW",
  "RBL",
  "R1F",
  "RGQ",
  "RQ6",
  "RW3",
  "RM2",
  "RVY",
  "RNL",
  "RQJ",
  "RW6",
  "RBA",
  "RA4",
  "RE9",
  "RLN"
)


# Remove any accidental duplicates
provider_codes <- unique(
  provider_codes
)


# ============================================================
# 10. LOAD TRUST CONTACT / EPR DATA
# ============================================================

Trust_Contact_List_Validated <- read_excel(
  "Trust_Contact_List_Validated.XLSX"
)


# ============================================================
# 11. CHECK WHAT GOLIVEDATE LOOKS LIKE
# ============================================================

cat("\n========================================\n")
cat("EXAMPLE GOLIVEDATE VALUES\n")
cat("========================================\n")

print(
  head(
    Trust_Contact_List_Validated$GoLiveDate,
    20
  )
)

cat("\nGoLiveDate class:\n")

print(
  class(
    Trust_Contact_List_Validated$GoLiveDate
  )
)


# ============================================================
# 12. PREPARE EPR DATA
# ============================================================

epr_selected <- Trust_Contact_List_Validated %>%
  
  select(
    `Trust Code`,
    GoLiveDate,
    GoLiveType
  ) %>%
  
  mutate(
    
    GoLiveDate = as.character(
      GoLiveDate
    ),
    
    # Convert common UK date formats:
    #
    # 1. dd/mm/yyyy
    # 2. yyyy-mm-dd
    # 3. Excel serial dates
    #
    # Anything that cannot be interpreted becomes NA.
    
    GoLiveDate = case_when(
      
      grepl(
        "^\\d{1,2}/\\d{1,2}/\\d{4}$",
        GoLiveDate
      ) ~ as.Date(
        GoLiveDate,
        format = "%d/%m/%Y"
      ),
      
      grepl(
        "^\\d{4}-\\d{1,2}-\\d{1,2}$",
        GoLiveDate
      ) ~ as.Date(
        GoLiveDate,
        format = "%Y-%m-%d"
      ),
      
      grepl(
        "^\\d+(\\.\\d+)?$",
        GoLiveDate
      ) ~ as.Date(
        as.numeric(GoLiveDate),
        origin = "1899-12-30"
      ),
      
      TRUE ~ as.Date(NA)
    )
  ) %>%
  
  select(
    `Trust Code`,
    GoLiveDate,
    GoLiveType
  ) %>%
  
  distinct(
    `Trust Code`,
    .keep_all = TRUE
  )


# ============================================================
# 13. CHECK CONVERTED EPR DATES
# ============================================================

cat("\n========================================\n")
cat("CONVERTED GOLIVE DATES\n")
cat("========================================\n")

print(
  epr_selected %>%
    select(
      `Trust Code`,
      GoLiveDate,
      GoLiveType
    ) %>%
    head(20)
)


# ============================================================
# 14. JOIN EPR DATA TO DATASET
# ============================================================
# IMPORTANT:
# Do this BEFORE applying the merger mapping.
#
# This means every original Provider_Code retains its own
# EPR information.
# ============================================================

df_final <- df_final %>%
  
  left_join(
    epr_selected,
    
    by = c(
      "Provider_Code" = "Trust Code"
    )
  ) %>%
  
  arrange(
    Provider_Code,
    Month
  )


# ============================================================
# 15. TRUST MERGER / CONSOLIDATION MAPPING
# ============================================================
#
# Main Trust Code = the final/canonical trust code.
#
# All Merge 1 and Merge 2 organisations are moved into the
# Main Trust Code.
#
# The MAIN TRUST'S EPR information will be retained later.
# ============================================================

trust_merge_map <- tibble(
  
  Main_Trust_Code = c(
    "RC9",
    "RA7",
    "RDE",
    "REM",
    "R0A",
    "RBN",
    "RNN",
    "RGN",
    "RM3",
    "RH5",
    "RH5",
    "R0B"
  ),
  
  Merge_1 = c(
    "RC1",
    "RVJ",
    "RDE",
    "RQ6",
    "RW3",
    "RVY",
    "RNL",
    "RGN",
    "RM3",
    "RBA",
    "RA4",
    "RE9"
  ),
  
  Merge_2 = c(
    "RC9",
    "RA7",
    "RGQ",
    "REM",
    "RM2",
    "RBN",
    "RNN",
    "RQJ",
    "RW6",
    "RH5",
    NA,
    "RLN"
  )
)


# ============================================================
# 16. CREATE CODE -> MAIN TRUST LOOKUP
# ============================================================
#
# Every merger code points to its Main Trust Code.
#
# Main trust codes themselves also point to themselves.
# ============================================================

trust_code_lookup <- bind_rows(
  
  # Main trusts
  trust_merge_map %>%
    select(
      Provider_Code = Main_Trust_Code,
      Main_Trust_Code
    ),
  
  # Merge 1
  trust_merge_map %>%
    select(
      Provider_Code = Merge_1,
      Main_Trust_Code
    ) %>%
    filter(
      !is.na(Provider_Code)
    ),
  
  # Merge 2
  trust_merge_map %>%
    select(
      Provider_Code = Merge_2,
      Main_Trust_Code
    ) %>%
    filter(
      !is.na(Provider_Code)
    )
  
) %>%
  
  distinct(
    Provider_Code,
    .keep_all = TRUE
  )


# ============================================================
# 17. SAVE THE MAIN TRUST'S EPR VALUES
# ============================================================
#
# This is critical.
#
# We explicitly extract EPR information from the ORIGINAL
# main trust rows BEFORE we change Provider_Code.
#
# Therefore:
#
# RC9 -> keeps RC9's GoLiveDate / GoLiveType
# RA7 -> keeps RA7's GoLiveDate / GoLiveType
# etc.
#
# A merged organisation's EPR values cannot overwrite these.
# ============================================================

main_trust_epr <- df_final %>%
  
  filter(
    Provider_Code %in% unique(
      trust_merge_map$Main_Trust_Code
    )
  ) %>%
  
  select(
    Provider_Code,
    GoLiveDate,
    GoLiveType
  ) %>%
  
  # Prefer rows where EPR information actually exists.
  arrange(
    Provider_Code,
    is.na(GoLiveDate),
    is.na(GoLiveType)
  ) %>%
  
  distinct(
    Provider_Code,
    .keep_all = TRUE
  )


# ============================================================
# 18. CHECK MAIN TRUST EPR VALUES
# ============================================================

cat("\n========================================\n")
cat("MAIN TRUST EPR VALUES TO BE RETAINED\n")
cat("========================================\n")

print(
  main_trust_epr
)


# ============================================================
# 19. APPLY TRUST MERGER MAPPING
# ============================================================
#
# Example:
#
# RC1 -> RC9
# RC9 -> RC9
#
# RVJ -> RA7
# RA7 -> RA7
#
# RQ6 -> REM
# REM -> REM
#
# RW3 -> R0A
# RM2 -> R0A
# R0A -> R0A
#
# etc.
# ============================================================

df_final <- df_final %>%
  
  left_join(
    trust_code_lookup,
    by = "Provider_Code"
  ) %>%
  
  mutate(
    
    # Keep original code temporarily for checking
    Original_Provider_Code = Provider_Code,
    
    # Replace with Main Trust Code
    Provider_Code = coalesce(
      Main_Trust_Code,
      Provider_Code
    )
  ) %>%
  
  select(
    -Main_Trust_Code
  )


# ============================================================
# 20. AGGREGATE MERGED TRUSTS BY MONTH
# ============================================================
#
# Counts are SUMMED.
#
# Percentages are NOT summed.
# They are recalculated after aggregation.
#
# EPR columns are deliberately excluded here because they
# will be restored from main_trust_epr afterwards.
# ============================================================

df_final <- df_final %>%
  
  group_by(
    Provider_Code,
    Month
  ) %>%
  
  summarise(
    
    # --------------------------------------------------------
    # A&E
    # --------------------------------------------------------
    
    total_type1_attends = sum(
      total_type1_attends,
      na.rm = TRUE
    ),
    
    total_over_4hrs = sum(
      total_over_4hrs,
      na.rm = TRUE
    ),
    
    DTAover12 = sum(
      DTAover12,
      na.rm = TRUE
    ),
    
    DTAover4 = sum(
      DTAover4,
      na.rm = TRUE
    ),
    
    
    # --------------------------------------------------------
    # BEDS
    # --------------------------------------------------------
    
    Total_Available_Beds = sum(
      Total_Available_Beds,
      na.rm = TRUE
    ),
    
    Total_Occupied_Beds = sum(
      Total_Occupied_Beds,
      na.rm = TRUE
    ),
    
    
    # --------------------------------------------------------
    # RTT
    # --------------------------------------------------------
    
    Total_Incomplete_Pathways = sum(
      Total_Incomplete_Pathways,
      na.rm = TRUE
    ),
    
    Sum_Within_18_Weeks = sum(
      Sum_Within_18_Weeks,
      na.rm = TRUE
    ),
    
    Sum_Over_52_Weeks = sum(
      Sum_Over_52_Weeks,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


# ============================================================
# 21. RECALCULATE PERCENTAGES
# ============================================================

df_final <- df_final %>%
  
  mutate(
    
    # --------------------------------------------------------
    # A&E: % under 4 hours
    # --------------------------------------------------------
    
    pct_under_4hrs = if_else(
      total_type1_attends > 0,
      (
        total_type1_attends -
          total_over_4hrs
      ) /
        total_type1_attends * 100,
      NA_real_
    ),
    
    
    # --------------------------------------------------------
    # Beds: % occupied
    # --------------------------------------------------------
    
    Pct_Occupied = if_else(
      Total_Available_Beds > 0,
      Total_Occupied_Beds /
        Total_Available_Beds * 100,
      NA_real_
    ),
    
    
    # --------------------------------------------------------
    # RTT: % within 18 weeks
    # --------------------------------------------------------
    
    Pct_Within_18_Weeks = if_else(
      Total_Incomplete_Pathways > 0,
      Sum_Within_18_Weeks /
        Total_Incomplete_Pathways * 100,
      NA_real_
    ),
    
    
    # --------------------------------------------------------
    # RTT: % over 52 weeks
    # --------------------------------------------------------
    
    Pct_Over_52_Weeks = if_else(
      Total_Incomplete_Pathways > 0,
      Sum_Over_52_Weeks /
        Total_Incomplete_Pathways * 100,
      NA_real_
    )
  )


# ============================================================
# 22. RESTORE MAIN TRUST EPR VALUES
# ============================================================
#
# This joins the ORIGINAL EPR values for the Main Trust Code.
#
# Therefore, after consolidation:
#
# RC9 receives RC9's EPR values
# RA7 receives RA7's EPR values
# REM receives REM's EPR values
# etc.
# ============================================================

df_final <- df_final %>%
  
  left_join(
    main_trust_epr,
    by = "Provider_Code"
  ) %>%
  
  arrange(
    Provider_Code,
    Month
  )


# ============================================================
# 23. FINAL COLUMN SELECTION
# ============================================================

df_final <- df_final %>%filter(
  Provider_Code %in% provider_codes
) %>%
  
  select(
    
    Provider_Code,
    Month,
    
    # --------------------------------------------------------
    # EPR
    # --------------------------------------------------------
    
    GoLiveDate,
    GoLiveType,
    
    # --------------------------------------------------------
    # A&E
    # --------------------------------------------------------
    
    total_type1_attends,
    total_over_4hrs,
    DTAover12,
    DTAover4,
    pct_under_4hrs,
    
    # --------------------------------------------------------
    # Beds
    # --------------------------------------------------------
    
    Total_Available_Beds,
    Total_Occupied_Beds,
    Pct_Occupied,
    
    # --------------------------------------------------------
    # RTT
    # --------------------------------------------------------
    
    Total_Incomplete_Pathways,
    Pct_Within_18_Weeks,
    Sum_Within_18_Weeks,
    Pct_Over_52_Weeks,
    Sum_Over_52_Weeks
  ) %>%
  
  arrange(
    Provider_Code,
    Month
  )


# ============================================================
# 24. PROVIDERS WITH NO EPR MATCH
# ============================================================

providers_without_epr <- df_final %>%
  
  filter(
    is.na(GoLiveType)
  ) %>%
  
  distinct(
    Provider_Code
  ) %>%
  
  arrange(
    Provider_Code
  )


cat("\n========================================\n")
cat("PROVIDERS WITH NO EPR MATCH\n")
cat("========================================\n")

print(
  providers_without_epr
)


# ============================================================
# 25. CHECK MERGED TRUSTS
# ============================================================
# This lets you verify that the merger codes have been
# successfully consolidated into the Main Trust Code.
# ============================================================

cat("\n========================================\n")
cat("MERGER CODE CHECK\n")
cat("========================================\n")

merger_check <- trust_code_lookup %>%
  
  arrange(
    Main_Trust_Code,
    Provider_Code
  )

print(
  merger_check
)


# ============================================================
# 26. FINAL DATASET CHECK
# ============================================================

cat("\n========================================\n")
cat("FINAL DATASET SUMMARY\n")
cat("========================================\n")

cat(
  "Rows: ",
  nrow(df_final),
  "\n"
)

cat(
  "Providers: ",
  n_distinct(df_final$Provider_Code),
  "\n"
)

cat(
  "Months: ",
  n_distinct(df_final$Month),
  "\n"
)

cat("\nFinal columns:\n")

print(
  names(df_final)
)


# ============================================================
# 27. FINAL DATASET
# ============================================================
#
# df_final is now the final consolidated dataset.
# ============================================================

df_final
