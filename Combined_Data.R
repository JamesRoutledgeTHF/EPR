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
  
  # Carry the latest bed figures forward when a month
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
  
  # Carry the most recent RTT observation forward
  # when a provider/month is missing.
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
# 7. JOIN RTT DATA TO THE MAIN DATASET
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
# 8. FINAL CLEAN-UP
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
# 9. CHECK WHAT COLUMNS WE HAVE
# ============================================================

cat("\n========================================\n")
cat("COLUMNS IN FINAL DATASET\n")
cat("========================================\n")

print(names(df_final))


# ============================================================
# 10. FINAL DATASET
# ============================================================
# Explicitly select the variables we actually want.
#
# EPR variables such as GoLiveDate and GoLiveType are retained
# if they already exist in df_combined.

# ============================================================
# FILTER TO REQUIRED PROVIDERS
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
  "RVJ",
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
  "RC1",
  "RVJ",
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


# ============================================================
# FILTER AND FINALISE DATASET
# ============================================================

df_final <- df_final %>%
  
  filter(
    Provider_Code %in% provider_codes
  ) %>%
  
  select(
    Provider_Code,
    Month,
    
    # -------------------------
    # A&E
    # -------------------------
    total_type1_attends,
    total_over_4hrs,
    DTAover12,
    DTAover4,
    pct_under_4hrs,
    
    # -------------------------
    # Beds
    # -------------------------
    Total_Available_Beds,
    Total_Occupied_Beds,
    Pct_Occupied,
    
    # -------------------------
    # RTT
    # -------------------------
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
# READ IN TRUST CONTACT / EPR DATA
# ============================================================

Trust_Contact_List_Validated <- read_excel(
  "Trust_Contact_List_Validated.XLSX"
)


# ============================================================
# CHECK WHAT GOLIVEDATE LOOKS LIKE
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
# PREPARE EPR DATA
# ============================================================

epr_selected <- Trust_Contact_List_Validated %>%
  
  select(
    `Trust Code`,
    GoLiveDate,
    GoLiveType
  ) %>%
  
  mutate(
    
    GoLiveDate = as.character(GoLiveDate),
    
    # Convert common UK date formats.
    #
    # 1. dd/mm/yyyy
    # 2. yyyy-mm-dd
    # 3. Excel serial dates
    #
    # Anything that cannot be interpreted becomes NA.
    
    GoLiveDate = case_when(
      
      # UK format: 31/03/2026
      grepl(
        "^\\d{1,2}/\\d{1,2}/\\d{4}$",
        GoLiveDate
      ) ~ as.Date(
        GoLiveDate,
        format = "%d/%m/%Y"
      ),
      
      # ISO format: 2026-03-31
      grepl(
        "^\\d{4}-\\d{1,2}-\\d{1,2}$",
        GoLiveDate
      ) ~ as.Date(
        GoLiveDate,
        format = "%Y-%m-%d"
      ),
      
      # Excel serial number
      grepl(
        "^\\d+(\\.\\d+)?$",
        GoLiveDate
      ) ~ as.Date(
        as.numeric(GoLiveDate),
        origin = "1899-12-30"
      ),
      
      # Anything else
      TRUE ~ as.Date(NA)
    )
  ) %>%
  
  # Keep only the columns we need
  select(
    `Trust Code`,
    GoLiveDate,
    GoLiveType
  ) %>%
  
  # Make sure each Trust Code appears only once
  distinct(
    `Trust Code`,
    .keep_all = TRUE
  )


# ============================================================
# CHECK THE CONVERTED DATES
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
# JOIN EPR DATA TO FINAL DATASET
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
# FINAL COLUMN ORDER
# ============================================================

df_final <- df_final %>%
  
  select(
    Provider_Code,
    Month,
    
    # -------------------------
    # EPR
    # -------------------------
    GoLiveDate,
    GoLiveType,
    
    # -------------------------
    # A&E
    # -------------------------
    total_type1_attends,
    total_over_4hrs,
    DTAover12,
    DTAover4,
    pct_under_4hrs,
    
    # -------------------------
    # Beds
    # -------------------------
    Total_Available_Beds,
    Total_Occupied_Beds,
    Pct_Occupied,
    
    # -------------------------
    # RTT
    # -------------------------
    Total_Incomplete_Pathways,
    Pct_Within_18_Weeks,
    Sum_Within_18_Weeks,
    Pct_Over_52_Weeks,
    Sum_Over_52_Weeks
    
  
  )




# ============================================================
# PROVIDERS WITH NO EPR MATCH
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

print(providers_without_epr)




