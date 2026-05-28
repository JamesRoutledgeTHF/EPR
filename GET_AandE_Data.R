library(DBI)
library(odbc)

query <- "
SELECT *
FROM [AandE_Attendance].[ECDS_Monthly_Performance1]
WHERE Breakdown = '4 Hour Performance'
  AND Organisation = 'Total'
  AND Measure_Category = 'Percentage of Type 1 & 2 Attendances Admitted, Transferred or Discharged within 4 Hours'
  AND Measure = 'Total'
"

df <- dbGetQuery(con, query)

sort(unique(df$Effective_Snapshot_Date))


query <- "
SELECT DISTINCT Effective_Snapshot_Date
FROM [Sitreps].[AandE_Attendances_And_Emergency_Admissions_Mthly1]
ORDER BY Effective_Snapshot_Date
"

df <- dbGetQuery(con, query)

query_monthly <- "
SELECT 
    Provider_Code,
    Effective_Snapshot_Date,

    SUM(Attends_Over_4Hrs_Arr_To_Adm_Tfr_Disch_Type1) AS over_4hrs,
    SUM(AandE_Attends_Type1) AS total_attends,

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

ORDER BY 
    Provider_Code,
    Effective_Snapshot_Date
"

df <- dbGetQuery(con, query_monthly)


query_weekly <- "
SELECT 
    Provider_Code,

    -- Assign week to the month containing the majority of days
    -- (week-ending dates assumed to be Sundays)
    DATEFROMPARTS(
        YEAR(DATEADD(DAY, -3, Effective_Snapshot_Date)),
        MONTH(DATEADD(DAY, -3, Effective_Snapshot_Date)),
        1
    ) AS Reporting_Month,

    SUM(AandE_Attends_Type1) AS total_type1_attends,
    SUM(Attends_Over_4Hrs_Arr_To_Adm_Tfr_Disch_Type1) AS total_over_4hrs,

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

ORDER BY 
    Provider_Code,
    Reporting_Month
"

df_weekly <- dbGetQuery(con, query_weekly)

library(readxl)
library(writexl)
library(dplyr)

df_weekly <- df_weekly %>% mutate(Reporting_Month = floor_date(Reporting_Month, "month"))
df <- df %>% mutate(Reporting_Month = floor_date(Effective_Snapshot_Date, "month"))

df <- df %>% rename(pct_under_4hrs_monthly = pct_under_4hrs)

df_join <- full_join(
  df_weekly,
  df %>% select(Provider_Code, Reporting_Month, pct_under_4hrs_monthly),
  by = c("Provider_Code", "Reporting_Month")
) %>%
  # Keep only one percentage column
  mutate(
    pct_under_4hrs = coalesce(pct_under_4hrs, pct_under_4hrs_monthly)
  ) %>%
  # Drop the redundant monthly column
  select(
    Provider_Code,
    Reporting_Month,
    total_type1_attends,
    total_over_4hrs,
    pct_under_4hrs
  )

epr_go_live <- read_xlsx("EPR_Go_Live_230326.xlsx") %>%
  mutate(
    # Convert numeric Excel serials to Date
    GoLiveDate = as.Date(as.numeric(GoLiveDate), origin = "1899-12-30")
  )

# Select only the columns we need from the Excel sheet
epr_selected <- epr_go_live %>%
  select(`Trust Code`, GoLiveDate, GoLiveType)

df_final <- df_join %>%
  group_by(Provider_Code) %>%
  filter(
    !any(is.na(pct_under_4hrs)) &
      n_distinct(Reporting_Month) == n_distinct(df_join$Reporting_Month)
  ) %>%
  ungroup() %>%
    left_join(
    epr_selected,
    by = c("Provider_Code" = "Trust Code")
  )

write_xlsx(df_final, "AandE_Output.xlsx")


library(dplyr)
library(ggplot2)

# Filter to only Big Bang and None
plot_df <- df_final %>%
  filter(GoLiveType %in% c("Big Bang", "None")) %>%
  group_by(Reporting_Month, GoLiveType) %>%
  summarise(
    avg_pct_under_4hrs = mean(pct_under_4hrs, na.rm = TRUE),
    .groups = "drop"
  )

# Plot
ggplot(plot_df, aes(x = Reporting_Month,
                    y = avg_pct_under_4hrs,
                    colour = GoLiveType)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
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


library(dplyr)
library(ggplot2)
library(lubridate)

# Keep only Big Bang trusts
event_df <- df_final %>%
  filter(GoLiveType == "Big Bang") %>%
  
  # Convert both dates to monthly periods
  mutate(
    Reporting_Month = floor_date(Reporting_Month, "month"),
    GoLive_Month = floor_date(GoLiveDate, "month"),
    
    # Event time: months relative to go-live
    months_from_golive =
      (year(Reporting_Month) - year(GoLive_Month)) * 12 +
      (month(Reporting_Month) - month(GoLive_Month))
  )

# Average across trusts at each relative month
plot_df <- event_df %>%
  group_by(months_from_golive) %>%
  summarise(
    avg_pct_under_4hrs = mean(pct_under_4hrs, na.rm = TRUE),
    n_trusts = n_distinct(Provider_Code),
    .groups = "drop"
  )

# Plot
ggplot(plot_df,
       aes(x = months_from_golive,
           y = avg_pct_under_4hrs)) +
  geom_line(size = 1.2, colour = "#0072B2") +
  geom_point(size = 2, colour = "#0072B2") +
  
  # Vertical line at go-live
  geom_vline(xintercept = 0,
             linetype = "dashed",
             colour = "red",
             linewidth = 1) +
  
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
