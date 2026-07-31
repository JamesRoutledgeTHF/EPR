query_surplus <- "
SELECT
    Organisation_Name,
    Effective_Snapshot_Date,

    -- Total incomplete pathways
    SUM(Value) AS SurplusOrDeficit

FROM Trust_Accounts_Consolidation.Trusts_Data1

WHERE Work_Sheet_Name = 'TAC02 SoCI'
  AND Table_ID = 2 
  AND Sub_Code = 'SOC0220'
  

GROUP BY
    Organisation_Name,
    Effective_Snapshot_Date

ORDER BY
    Organisation_Name,
    Effective_Snapshot_Date
"

df_surplus <- dbGetQuery(con, query_surplus)


query_surplus_FT <- "
SELECT
    Organisation_Name,
    Effective_Snapshot_Date,

    -- Total incomplete pathways
    SUM(Value) AS SurplusOrDeficit

FROM Trust_Accounts_Consolidation.Foundation_Trusts_Data1

WHERE Work_Sheet_Name = 'TAC02 SoCI'
  AND Table_ID = 2 
  AND Sub_Code = 'SOC0220'
  

GROUP BY
    Organisation_Name,
    Effective_Snapshot_Date

ORDER BY
    Organisation_Name,
    Effective_Snapshot_Date
"

df_surplus_FT <- dbGetQuery(con, query_surplus_FT)


