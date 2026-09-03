# ============================================================
# 1. LIBRARIES
# ============================================================

library(DBI)
library(odbc)
library(readxl)
library(writexl)
library(dplyr)
library(lubridate)
library(ggplot2)


# ============================================================
# 2. MONTHLY A&E DATA
# ============================================================

query_monthly <- "
SELECT 
    Provider_Code,
    Effective_Snapshot_Date,

    SUM(Attends_Over_4Hrs_Arr_To_Adm_Tfr_Disch_Type1) AS total_over_4hrs,
    SUM(AandE_Attends_Type1) AS total_type1_attends,

    SUM(Dec_To_Adm_Over_12Hrs) AS DTAover12,

    SUM(Dec_To_Adm_4_to_12Hrs + Dec_To_Adm_Over_12Hrs) AS DTAover4,

    (
        1.0 - (
            SUM(Attends_Over_4Hrs_Arr_To_Adm_Tfr_Disch_Type1) * 1.0
            / NULLIF(SUM(AandE_Attends_Type1), 0)
        )
    ) * 100 AS pct_under_4hrs

FROM [Sitreps].[AandE_Attendances_And_Emergency_Admissions_Mthly1]

GROUP BY 
    Provider_Code,
    Effective_Snapshot_Date

"

df_monthly <- dbGetQuery(con, query_monthly)


# ============================================================
# 3. WEEKLY A&E DATA
# ============================================================
# The -3 days shifts the Sunday week-ending date back to
# Thursday, so the month containing the majority of the
# week's days is used.

query_weekly <- "
SELECT 
    Provider_Code,

    DATEFROMPARTS(
        YEAR(DATEADD(DAY, -3, Effective_Snapshot_Date)),
        MONTH(DATEADD(DAY, -3, Effective_Snapshot_Date)),
        1
    ) AS Reporting_Month,

    SUM(AandE_Attends_Type1) AS total_type1_attends,

    SUM(Attends_Over_4Hrs_Arr_To_Adm_Tfr_Disch_Type1) AS total_over_4hrs,

    SUM(Dec_To_Adm_Over_12Hrs) AS DTAover12,

    SUM(Dec_To_Adm_4_to_12Hrs) +
    SUM(Dec_To_Adm_Over_12Hrs) AS DTAover4,

    (
        1.0 - (
            SUM(Attends_Over_4Hrs_Arr_To_Adm_Tfr_Disch_Type1) * 1.0
            / NULLIF(SUM(AandE_Attends_Type1), 0)
        )
    ) * 100 AS pct_under_4hrs

FROM [Sitreps].[AandE_Attendances_And_Emergency_Admissions1]

GROUP BY 
    Provider_Code,

    DATEFROMPARTS(
        YEAR(DATEADD(DAY, -3, Effective_Snapshot_Date)),
        MONTH(DATEADD(DAY, -3, Effective_Snapshot_Date)),
        1
    )

"

df_weekly <- dbGetQuery(con, query_weekly)



# ============================================================
# 4. STANDARDISE DATES
# ============================================================

df_weekly <- df_weekly %>%
  mutate(
    Month = floor_date(as.Date(Reporting_Month), "month")
  )

df_monthly <- df_monthly %>%
  mutate(
    Month = floor_date(as.Date(Effective_Snapshot_Date), "month")
  )


# ============================================================
# 5. RENAME MONTHLY VARIABLES
# ============================================================

df_monthly <- df_monthly %>%
  rename(
    pct_under_4hrs_monthly = pct_under_4hrs,
    DTAover12_monthly = DTAover12,
    DTAover4_monthly = DTAover4
  )


# ============================================================
# 6. COMBINE WEEKLY AND MONTHLY DATA
# ============================================================
# full_join keeps:
# - historical weekly data (2010-2015)
# - monthly data (2015 onwards)
#
# Where both sources have data for the same provider/month,
# the weekly value is used first and the monthly value is
# used as a fallback.

df_join <- full_join(
  
  df_weekly,
  
  df_monthly %>%
    select(
      Provider_Code,
      Month,
      total_type1_attends,
      total_over_4hrs,
      pct_under_4hrs_monthly,
      DTAover12_monthly,
      DTAover4_monthly
    ),
  
  by = c("Provider_Code", "Month")
  
) %>%
  
  mutate(
    
    total_type1_attends = coalesce(
      total_type1_attends.x,
      total_type1_attends.y
    ),
    
    total_over_4hrs = coalesce(
      total_over_4hrs.x,
      total_over_4hrs.y
    ),
    
    pct_under_4hrs = coalesce(
      pct_under_4hrs,
      pct_under_4hrs_monthly
    ),
    
    DTAover12 = coalesce(
      DTAover12,
      DTAover12_monthly
    ),
    
    DTAover4 = coalesce(
      DTAover4,
      DTAover4_monthly
    )
    
  ) %>%
  
  select(
    Provider_Code,
    Month,
    total_type1_attends,
    total_over_4hrs,
    DTAover12,
    DTAover4,
    pct_under_4hrs
  ) %>%
  
  arrange(
    Provider_Code,
    Month
  )


# ============================================================
# 7. CHECK COMBINED DATE RANGE
# ============================================================

cat(
  "Combined data range:",
  as.character(min(df_join$Month, na.rm = TRUE)),
  "to",
  as.character(max(df_join$Month, na.rm = TRUE)),
  "\n"
)


# ============================================================
# 8. EPR GO-LIVE DATA
# ============================================================

epr_go_live <- read_xlsx("EPR_Go_Live_230326.xlsx") %>%
  mutate(
    GoLiveDate = as.Date(
      as.numeric(GoLiveDate),
      origin = "1899-12-30"
    )
  )

epr_selected <- epr_go_live %>%
  select(
    `Trust Code`,
    GoLiveDate,
    GoLiveType
  )


# ============================================================
# 9. STANDARDISE PROVIDER CODES
# ============================================================

df_final <- df_join %>%
  
  mutate(
    Provider_Code = case_when(
      
      Provider_Code %in% c("RM2", "RW3") ~ "R0A",
      
      Provider_Code %in% c("RLN", "RE9") ~ "R0B",
      
      Provider_Code %in% c("RV8", "RC3") ~ "R1K",
      
      TRUE ~ Provider_Code
    )
  ) %>%
  
  left_join(
    epr_selected,
    by = c("Provider_Code" = "Trust Code")
  ) %>%
  
  arrange(
    Provider_Code,
    Month
  )


# ============================================================
# 10. CHECK FINAL DATE RANGE
# ============================================================

cat(
  "Final data range:",
  as.character(min(df_final$Month, na.rm = TRUE)),
  "to",
  as.character(max(df_final$Month, na.rm = TRUE)),
  "\n"
)


# ============================================================
# 11. CHECK HOW MUCH DATA HAS EPR INFORMATION
# ============================================================

df_final %>%
  summarise(
    total_rows = n(),
    rows_with_golive_type = sum(!is.na(GoLiveType)),
    rows_without_golive_type = sum(is.na(GoLiveType)),
    earliest_month = min(Month, na.rm = TRUE),
    latest_month = max(Month, na.rm = TRUE)
  )


# ============================================================
# 12. PLOT: BIG BANG VS NONE
# ============================================================
# NOTE:
# This plot intentionally only uses trusts classified as
# Big Bang or None.
#
# It does NOT represent the full historical dataset because
# trusts with missing GoLiveType cannot be classified here.

plot_df <- df_final %>%
  
  filter(
    GoLiveType %in% c("Big Bang", "None")
  ) %>%
  
  group_by(
    Month,
    GoLiveType
  ) %>%
  
  summarise(
    avg_pct_under_4hrs = mean(
      pct_under_4hrs,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


ggplot(
  plot_df,
  aes(
    x = Month,
    y = avg_pct_under_4hrs,
    colour = GoLiveType
  )
) +
  
  geom_line(
    linewidth = 1.2
  ) +
  
  geom_point(
    size = 2
  ) +
  
  labs(
    title = "Average % Under 4 Hours: Big Bang vs None",
    x = "Reporting Month",
    y = "% Under 4 Hours",
    colour = "Go Live Type"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )


# ============================================================
# 13. EVENT STUDY DATA
# ============================================================
# Big Bang trusts only

event_df <- df_final %>%
  
  filter(
    GoLiveType == "Big Bang"
  ) %>%
  
  mutate(
    
    GoLive_Month = floor_date(
      GoLiveDate,
      "month"
    ),
    
    months_from_golive =
      (year(Month) - year(GoLive_Month)) * 12 +
      (month(Month) - month(GoLive_Month))
  )


# ============================================================
# 14. EVENT STUDY - AVERAGE % UNDER 4 HOURS
# ============================================================

plot_df <- event_df %>%
  
  group_by(
    months_from_golive
  ) %>%
  
  summarise(
    
    avg_pct_under_4hrs = mean(
      pct_under_4hrs,
      na.rm = TRUE
    ),
    
    sum_over_4hrs = mean(
      total_over_4hrs,
      na.rm = TRUE
    ),
    
    sum_over_4hrs_a2d = sum(
      DTAover4,
      na.rm = TRUE
    ),
    
    avg_over_4hrs_a2d = mean(
      DTAover4,
      na.rm = TRUE
    ),
    
    sum_over_12hrs_a2d = sum(
      DTAover12,
      na.rm = TRUE
    ),
    
    avg_over_12hrs_a2d = mean(
      DTAover12,
      na.rm = TRUE
    ),
    
    n_trusts = n_distinct(
      Provider_Code
    ),
    
    .groups = "drop"
  ) %>%
  
  filter(
    between(
      months_from_golive,
      -24,
      24
    )
  )


# Every 3 months

breaks_df <- plot_df %>%
  filter(
    months_from_golive %% 3 == 0
  )


# ============================================================
# 15. PLOT - % UNDER 4 HOURS
# ============================================================

ggplot(
  plot_df,
  aes(
    x = months_from_golive,
    y = avg_pct_under_4hrs
  )
) +
  
  geom_line(
    linewidth = 1.2,
    colour = "#0072B2"
  ) +
  
  geom_point(
    size = 2,
    colour = "#0072B2"
  ) +
  
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "red",
    linewidth = 1
  ) +
  
  scale_x_continuous(
    breaks = breaks_df$months_from_golive,
    labels = paste0(
      breaks_df$months_from_golive,
      "\n(n=",
      breaks_df$n_trusts,
      ")"
    )
  ) +
  
  labs(
    title = "Big Bang Trusts: % Under 4 Hours Relative to Go-Live",
    subtitle = "Month 0 = Go-Live Month",
    x = "Months Relative to Go-Live",
    y = "Average % Under 4 Hours"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )


# ============================================================
# 16. PLOT - >4 HOURS DTA TO ADMISSION
# ============================================================

ggplot(
  plot_df,
  aes(
    x = months_from_golive,
    y = avg_over_4hrs_a2d
  )
) +
  
  geom_line(
    linewidth = 1.2,
    colour = "#0072B2"
  ) +
  
  geom_point(
    size = 2,
    colour = "#0072B2"
  ) +
  
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "red",
    linewidth = 1
  ) +
  
  scale_x_continuous(
    breaks = breaks_df$months_from_golive,
    labels = paste0(
      breaks_df$months_from_golive,
      "\n(n=",
      breaks_df$n_trusts,
      ")"
    )
  ) +
  
  labs(
    title = "Big Bang Trusts: Patients Spending >4 Hours from DTA to Admission",
    subtitle = "Month 0 = Go-Live Month",
    x = "Months Relative to Go-Live",
    y = "Average Total over 4 Hours"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )


# ============================================================
# 17. PLOT - >12 HOURS DTA TO ADMISSION
# ============================================================

ggplot(
  plot_df,
  aes(
    x = months_from_golive,
    y = avg_over_12hrs_a2d
  )
) +
  
  geom_line(
    linewidth = 1.2,
    colour = "#0072B2"
  ) +
  
  geom_point(
    size = 2,
    colour = "#0072B2"
  ) +
  
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "red",
    linewidth = 1
  ) +
  
  scale_x_continuous(
    breaks = breaks_df$months_from_golive,
    labels = paste0(
      breaks_df$months_from_golive,
      "\n(n=",
      breaks_df$n_trusts,
      ")"
    )
  ) +
  
  labs(
    title = "Big Bang Trusts: Patients Spending >12 Hours from DTA to Admission",
    subtitle = "Month 0 = Go-Live Month",
    x = "Months Relative to Go-Live",
    y = "Average Total over 12 Hours"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )


# ============================================================
# 18. PROVIDER-LEVEL EVENT STUDY
# ============================================================

plot_prov_df <- event_df %>%
  
  group_by(
    months_from_golive,
    Provider_Code
  ) %>%
  
  summarise(
    
    avg_pct_under_4hrs = mean(
      pct_under_4hrs,
      na.rm = TRUE
    ),
    
    sum_over_4hrs_a2d = sum(
      DTAover4,
      na.rm = TRUE
    ),
    
    avg_over_4hrs_a2d = mean(
      DTAover4,
      na.rm = TRUE
    ),
    
    sum_over_12hrs_a2d = sum(
      DTAover12,
      na.rm = TRUE
    ),
    
    n_trusts = n_distinct(
      Provider_Code
    ),
    
    .groups = "drop"
  ) %>%
  
  filter(
    between(
      months_from_golive,
      -24,
      24
    )
  )


# ============================================================
# 19. PROVIDER-LEVEL PLOT
# ============================================================

ggplot(
  plot_prov_df,
  aes(
    x = months_from_golive,
    y = sum_over_4hrs_a2d,
    colour = Provider_Code,
    group = Provider_Code
  )
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    size = 1.5
  ) +
  
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "red"
  ) +
  
  scale_x_continuous(
    breaks = breaks_df$months_from_golive,
    labels = paste0(
      breaks_df$months_from_golive,
      "\n(n=",
      breaks_df$n_trusts,
      ")"
    )
  ) +
  
  labs(
    title = "Big Bang Trusts: Patients Spending >4 Hours from DTA to Admission",
    subtitle = "One line per trust",
    x = "Months Relative to Go-Live",
    y = "Average Total over 4 Hours",
    colour = "Provider Code"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )


# ============================================================
# 20. OPTIONAL: SAVE FINAL DATA
# ============================================================

# write_xlsx(
#   df_final,
#   "AandE_Output.xlsx"
# )
