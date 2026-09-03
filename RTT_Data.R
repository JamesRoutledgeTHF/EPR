query_rtt <- "
SELECT
    Provider_Org_Code,
    Effective_Snapshot_Date,

    -- Total incomplete pathways
    ROUND(SUM(Total_All), 1) AS Total_Incomplete_Pathways,

    -- Percentage waiting within 18 weeks
    CASE
        WHEN SUM(Total_All) = 0 THEN NULL
        ELSE ROUND(
            SUM(
                Gt_00_To_01_Weeks +
                Gt_01_To_02_Weeks +
                Gt_02_To_03_Weeks +
                Gt_03_To_04_Weeks +
                Gt_04_To_05_Weeks +
                Gt_05_To_06_Weeks +
                Gt_06_To_07_Weeks +
                Gt_07_To_08_Weeks +
                Gt_08_To_09_Weeks +
                Gt_09_To_10_Weeks +
                Gt_10_To_11_Weeks +
                Gt_11_To_12_Weeks +
                Gt_12_To_13_Weeks +
                Gt_13_To_14_Weeks +
                Gt_14_To_15_Weeks +
                Gt_15_To_16_Weeks +
                Gt_16_To_17_Weeks +
                Gt_17_To_18_Weeks
            ) * 100.0 / SUM(Total_All),
            1
        )
    END AS Pct_Within_18_Weeks,

    -- Sum waiting within 18 weeks
    CASE
        WHEN SUM(Total_All) = 0 THEN NULL
        ELSE ROUND(
            SUM(
                Gt_00_To_01_Weeks +
                Gt_01_To_02_Weeks +
                Gt_02_To_03_Weeks +
                Gt_03_To_04_Weeks +
                Gt_04_To_05_Weeks +
                Gt_05_To_06_Weeks +
                Gt_06_To_07_Weeks +
                Gt_07_To_08_Weeks +
                Gt_08_To_09_Weeks +
                Gt_09_To_10_Weeks +
                Gt_10_To_11_Weeks +
                Gt_11_To_12_Weeks +
                Gt_12_To_13_Weeks +
                Gt_13_To_14_Weeks +
                Gt_14_To_15_Weeks +
                Gt_15_To_16_Weeks +
                Gt_16_To_17_Weeks +
                Gt_17_To_18_Weeks
            ),
            1
        )
    END AS Sum_Within_18_Weeks,

    -- Percentage waiting over 52 weeks
    CASE
        WHEN SUM(Total_All) = 0 THEN NULL
        ELSE ROUND(
            SUM(Gt_52_Weeks) * 100.0 / SUM(Total_All),
            1
        )
    END AS Pct_Over_52_Weeks,

    -- Sum waiting over 52 weeks
    CASE
        WHEN SUM(Total_All) = 0 THEN NULL
        ELSE ROUND(
            SUM(Gt_52_Weeks),
            1
        )
    END AS Sum_Over_52_Weeks

FROM RTT.Full_Dataset1

WHERE RTT_Part_Description = 'Incomplete Pathways'
  AND Treatment_Function_Code = 'C_999'

GROUP BY
    Provider_Org_Code,
    Effective_Snapshot_Date

ORDER BY
    Provider_Org_Code,
    Effective_Snapshot_Date
"

df_rtt <- dbGetQuery(con, query_rtt)


query_rtt_2 <- "SELECT *
FROM RTT.Full_Dataset1
WHERE Provider_Org_Code = 'R0A'
  AND Effective_Snapshot_Date = '2020-02-29';"


df_rtt_R0A_raw <- dbGetQuery(con, query_rtt_2)
