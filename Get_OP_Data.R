
query_outpatient <- "
SELECT DISTINCT *
FROM [HES].[Monthly_Activity_Report1]
ORDER BY Effective_Snapshot_Date
"

df_outpatient <- dbGetQuery(con, query_outpatient)

query_metrics <- "
SELECT DISTINCT
    Metric
FROM [HES].[Monthly_Activity_Report1]
ORDER BY Metric
"

metric_values <- dbGetQuery(con, query_metrics)


query_outpatient <- "
SELECT
    Org_Code,
    Effective_Snapshot_Date,

    -- First attendances seen
    SUM(
        CASE 
            WHEN Metric = 'All_specialties:_First_attendances_seen_total'
            THEN Metric_Value
            ELSE 0
        END
    ) AS first_attendances,

    -- Subsequent attendances seen
    SUM(
        CASE 
            WHEN Metric = 'All_specialties:_Subsequent_attendances_seen'
            THEN Metric_Value
            ELSE 0
        END
    ) AS subsequent_attendances,

    -- Total appointments (seen + DNA)
    SUM(
        CASE 
            WHEN Metric IN (
                'All_specialties:_First_attendances_seen_total',
                'All_specialties:_Subsequent_attendances_seen',
                'All_specialties:_First_attendances_DNA',
                'All_specialties:_Subsequent_attendances_DNA'
            )
            THEN Metric_Value
            ELSE 0
        END
    ) AS Total_Appointments,

    -- Total seen appointments
    SUM(
        CASE 
            WHEN Metric IN (
                'All_specialties:_First_attendances_seen_total',
                'All_specialties:_Subsequent_attendances_seen'
            )
            THEN Metric_Value
            ELSE 0
        END
    ) AS Total_Seen,

    -- Total DNA appointments
    SUM(
        CASE 
            WHEN Metric IN (
                'All_specialties:_First_attendances_DNA',
                'All_specialties:_Subsequent_attendances_DNA'
            )
            THEN Metric_Value
            ELSE 0
        END
    ) AS Total_DNA,

    -- DNA rate
    CASE 
        WHEN SUM(
                CASE 
                    WHEN Metric IN (
                        'All_specialties:_First_attendances_seen_total',
                        'All_specialties:_Subsequent_attendances_seen',
                        'All_specialties:_First_attendances_DNA',
                        'All_specialties:_Subsequent_attendances_DNA'
                    )
                    THEN Metric_Value
                    ELSE 0
                END
            ) = 0 THEN NULL
        ELSE
            SUM(
                CASE 
                    WHEN Metric IN (
                        'All_specialties:_First_attendances_DNA',
                        'All_specialties:_Subsequent_attendances_DNA'
                    )
                    THEN Metric_Value
                    ELSE 0
                END
            ) * 100.0
            /
            SUM(
                CASE 
                    WHEN Metric IN (
                        'All_specialties:_First_attendances_seen_total',
                        'All_specialties:_Subsequent_attendances_seen',
                        'All_specialties:_First_attendances_DNA',
                        'All_specialties:_Subsequent_attendances_DNA'
                    )
                    THEN Metric_Value
                    ELSE 0
                END
            )
    END AS DNA_rate,

    -- Percentage of first attendances (of seen appointments only)
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
            ) * 100.0
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
            )
    END AS pct_first_attendances

FROM [HES].[Monthly_Activity_Report1]

WHERE Metric IN (
    'All_specialties:_First_attendances_seen_total',
    'All_specialties:_Subsequent_attendances_seen',
    'All_specialties:_First_attendances_DNA',
    'All_specialties:_Subsequent_attendances_DNA'
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
  mutate(
    Org_Code = case_when(
      Org_Code %in% c("RM2", "RW3") ~ "R0A",
      Org_Code %in% c("RLN", "RE9") ~ "R0B",
      Org_Code %in% c("RV8", "RC3") ~ "R1K",
      TRUE ~ Org_Code
    )
  ) %>%
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
    avg_DNA_pct = mean(DNA_rate, na.rm = TRUE),
    avg_apps = mean(Total_Appointments, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(plot_df,
       aes(x = Reporting_Month,
           y = avg_DNA_pct,
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


ggplot(plot_df,
       aes(x = Reporting_Month,
           y = avg_apps,
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

event_df <- df_final %>%
  filter(GoLiveType == "Big Bang") %>%
  mutate(
    Reporting_Month = floor_date(Reporting_Month, "month"),
    GoLive_Month = floor_date(GoLiveDate, "month"),
    
    months_from_golive =
      (year(Reporting_Month) - year(GoLive_Month)) * 12 +
      (month(Reporting_Month) - month(GoLive_Month))
  )

required_months <- -24:24

eligible_trusts <- event_df %>%
  filter(months_from_golive %in% required_months) %>%
  group_by(Org_Code) %>%
  summarise(
    n_months = n_distinct(months_from_golive),
    .groups = "drop"
  ) %>%
  filter(n_months == length(required_months)) %>%
  pull(Org_Code)

# Keep only eligible trusts and create event study data
plot_event <- event_df %>%
  filter(
    Org_Code %in% eligible_trusts,
    !Org_Code %in% c("RXR"),
    between(months_from_golive, -24, 24)
  ) %>%
  group_by(months_from_golive) %>%
  summarise(
    avg_pct_first_attendances = mean(pct_first_attendances, na.rm = TRUE),
    avg_DNA_pct = mean(DNA_rate, na.rm = TRUE),
    avg_Apps = mean(Total_Appointments, na.rm = TRUE),
    n_trusts = n_distinct(Org_Code),
    .groups = "drop"
  )

breaks_df <- plot_event %>%
  filter(months_from_golive %% 3 == 0)

ggplot(plot_event,
       aes(x = months_from_golive,
           y = avg_pct_first_attendances)) +
  geom_line(size = 1.2, colour = "#0072B2") +
  geom_point(size = 2, colour = "#0072B2") +
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             colour = "red") +
  scale_x_continuous(
    breaks = breaks_df$months_from_golive,
    labels = paste0(
      breaks_df$months_from_golive,
      "\n(n=", breaks_df$n_trusts, ")"
    )
  ) +
  labs(
    title = "Outpatient Percent First Attendances",
    subtitle = "Month 0 = Go-Live",
    x = "Months Relative to Go-Live",
    y = "% First Attendances"
  ) +
  theme_minimal()

ggplot(plot_event,
       aes(x = months_from_golive,
           y = avg_DNA_pct)) +
  geom_line(size = 1.2, colour = "#0072B2") +
  geom_point(size = 2, colour = "#0072B2") +
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             colour = "red") +
  scale_x_continuous(
    breaks = breaks_df$months_from_golive,
    labels = paste0(
      breaks_df$months_from_golive,
      "\n(n=", breaks_df$n_trusts, ")"
    )
  ) +
  labs(
    title = "Outpatient DNA Rate",
    subtitle = "Month 0 = Go-Live",
    x = "Months Relative to Go-Live",
    y = "DNA %",
    colour = "Trust"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right"
  )


ggplot(plot_event,
       aes(x = months_from_golive,
           y = avg_Apps)) +
  geom_line(size = 1.2, colour = "#0072B2") +
  geom_point(size = 2, colour = "#0072B2") +
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             colour = "red") +
  scale_x_continuous(
    breaks = breaks_df$months_from_golive,
    labels = paste0(
      breaks_df$months_from_golive,
      "\n(n=", breaks_df$n_trusts, ")"
    )
  ) +
  labs(
    title = "Outpatient Avg Num of attended Apps",
    subtitle = "Month 0 = Go-Live",
    x = "Months Relative to Go-Live",
    y = "Avg Num of Apps",
    colour = "Trust"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right"
  )