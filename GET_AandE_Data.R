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
SELECT 
    Provider_Code,
    Effective_Snapshot_Date,
    SUM(Attends_Under_4Hrs_Arr_To_Adm_Tfr_Disch_Type1) * 100.0 
        / NULLIF(SUM(AandE_Attends_Type1), 0) AS pct_under_4hrs
FROM [Sitreps].[AandE_Attendances_And_Emergency_Adm_Mthly_Footprint1]
GROUP BY 
    Provider_Code,
    Effective_Snapshot_Date
ORDER BY 
    Provider_Code,
    Effective_Snapshot_Date
"

df <- dbGetQuery(con, query)
