
query_outpatient <- "
SELECT DISTINCT *
FROM [HES].[Monthly_Activity_Report1]
ORDER BY Effective_Snapshot_Date
"

df_outpatient <- dbGetQuery(con, query_outpatient)

query_outpatient <- "
SELECT
    Org_Code,
    Effective_Snapshot_Date,

    SUM(
        CASE 
            WHEN Metric = 'All_specialties:_First_attendances_seen_total'
            THEN Metric_Value
            ELSE 0
        END
    ) AS first_attendances,

    SUM(
        CASE 
            WHEN Metric = 'All_specialties:_Subsequent_attendances_seen'
            THEN Metric_Value
            ELSE 0
        END
    ) AS subsequent_attendances,
    
    -- Total attendances
    SUM(
        CASE 
            WHEN Metric IN (
                'All_specialties:_First_attendances_seen_total',
                'All_specialties:_Subsequent_attendances_seen'
            )
            THEN Metric_Value
            ELSE 0
        END
    ) AS Total_attendances,

    -- Percentage of first attendances
    CASE 
        WHEN SUM(
                CASE 
                    WHEN Metric IN (
                        'All_specialties:_First_attendances_seen_total',
                        'All_specialties:_Subsequent_attendances_seen'
                    )
                    THEN Metric_Value
                    ELSE 0
                END
            ) = 0 THEN NULL
        ELSE
            SUM(
                CASE 
                    WHEN Metric = 'All_specialties:_First_attendances_seen_total'
                    THEN Metric_Value
                    ELSE 0
                END
            ) * 1.0
            /
            SUM(
                CASE 
                    WHEN Metric IN (
                        'All_specialties:_First_attendances_seen_total',
                        'All_specialties:_Subsequent_attendances_seen'
                    )
                    THEN Metric_Value
                    ELSE 0
                END
            ) * 100
    END AS pct_first_attendances

FROM [HES].[Monthly_Activity_Report1]

WHERE Metric IN (
    'All_specialties:_First_attendances_seen_total',
    'All_specialties:_Subsequent_attendances_seen'
)

GROUP BY
    Org_Code,
    Effective_Snapshot_Date

ORDER BY
    Org_Code,
    Effective_Snapshot_Date
"

df_outpatient <- dbGetQuery(con, query_outpatient)


library(dplyr)
library(readxl)
library(lubridate)

epr_go_live <- read_xlsx("EPR_Go_Live_230326.xlsx") %>%
  mutate(
    GoLiveDate = as.Date(as.numeric(GoLiveDate), origin = "1899-12-30")
  )

epr_selected <- epr_go_live %>%
  select(`Trust Code`, GoLiveDate, GoLiveType)

df_final <- df_outpatient %>%
  mutate(Reporting_Month = floor_date(Effective_Snapshot_Date, "month")) %>%
  left_join(
    epr_selected,
    by = c("Org_Code" = "Trust Code")
  )

library(ggplot2)

plot_df <- df_final %>%
  filter(GoLiveType %in% c("Big Bang", "None")) %>%
  group_by(Reporting_Month, GoLiveType) %>%
  summarise(
    avg_pct_first_attendances = mean(pct_first_attendances, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(plot_df,
       aes(x = Reporting_Month,
           y = avg_pct_first_attendances,
           colour = GoLiveType)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Outpatient: First Attendance Rate (Big Bang vs None)",
    x = "Month",
    y = "% First Attendances",
    colour = "Go Live Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

library(lubridate)

event_df <- df_final %>%
  filter(GoLiveType == "Big Bang") %>%
  mutate(
    Reporting_Month = floor_date(Reporting_Month, "month"),
    GoLive_Month = floor_date(GoLiveDate, "month"),
    
    months_from_golive =
      (year(Reporting_Month) - year(GoLive_Month)) * 12 +
      (month(Reporting_Month) - month(GoLive_Month))
  )

plot_event <- event_df %>%
  group_by(months_from_golive) %>%
  summarise(
    avg_pct_first_attendances = mean(pct_first_attendances, na.rm = TRUE),
    n_trusts = n_distinct(Org_Code),
    .groups = "drop"
  )

ggplot(plot_event,
       aes(x = months_from_golive,
           y = avg_pct_first_attendances)) +
  geom_line(linewidth = 1.2, colour = "#0072B2") +
  geom_point(size = 2, colour = "#0072B2") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red") +
  labs(
    title = "Outpatient First Attendance Rate: Event Study (Big Bang)",
    subtitle = "Month 0 = Go-Live",
    x = "Months Relative to Go-Live",
    y = "% First Attendances"
  ) +
  theme_minimal()