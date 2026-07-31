library(dplyr)
library(lubridate)

df_combined <- df_join %>%
  mutate(
    join_month = floor_date(Month, "month")
  ) %>%
  left_join(
    df_beds %>%
      mutate(
        Effective_Snapshot_Date = as.Date(Effective_Snapshot_Date),
        join_month = floor_date(Effective_Snapshot_Date, "month")
      ),
    by = c(
      "Provider_Code" = "Organisation_Code",
      "join_month"
    )
  ) %>%
  select(-join_month)

library(readxl)

RTT_incomplete_provider_combined <- read_excel("RTT_incomplete_provider_combined.xlsx")

RTT_incomplete_provider_combined <- RTT_incomplete_provider_combined %>%
  mutate(
    Effective_Snapshot_Date = as.Date(
      paste0("01/", Effective_Snapshot_Date),
      format = "%d/%m/%Y"
    )
  )

df_rtt_combined <- df_rtt %>%
  mutate(
    Effective_Snapshot_Date = floor_date(
      as.Date(Effective_Snapshot_Date),
      "month"
    )
  ) %>%
  left_join(
    RTT_incomplete_provider_combined,
    by = c("Provider_Org_Code", "Effective_Snapshot_Date"),
    suffix = c(".x", ".y")
  ) %>%
  mutate(
    Total_Incomplete_Pathways = coalesce(Total_Incomplete_Pathways.x, Total_Incomplete_Pathways.y),
    Pct_Within_18_Weeks = coalesce(Pct_Within_18_Weeks.x, Pct_Within_18_Weeks.y),
    Sum_Within_18_Weeks = coalesce(Sum_Within_18_Weeks.x, Sum_Within_18_Weeks.y),
    Pct_Over_52_Weeks = coalesce(Pct_Over_52_Weeks.x, Pct_Over_52_Weeks.y),
    Sum_Over_52_Weeks = coalesce(Sum_Over_52_Weeks.x, Sum_Over_52_Weeks.y)
  ) %>%
  select(-ends_with(".x"), -ends_with(".y"))

df_final <- df_combined %>%
  mutate(
    join_month = floor_date(as.Date(Month), "month")
  ) %>%
  left_join(
    df_rtt_combined,
    by = c(
      "Provider_Code" = "Provider_Org_Code",
      "join_month" = "Effective_Snapshot_Date"
    )
  ) %>%
  select(-join_month)