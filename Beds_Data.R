query_beds <- "
WITH Occupied AS (
    SELECT
        Organisation_Code,
        Effective_Snapshot_Date,
        SUM(Number_Of_Beds) AS Total_Occupied_Beds
    FROM Bed_Availability.Provider_By_Sector_Occupied_Overnight_Beds1
    GROUP BY
        Organisation_Code,
        Effective_Snapshot_Date
),

Available AS (
    SELECT
        Organisation_Code,
        Effective_Snapshot_Date,
        SUM(Number_Of_Beds) AS Total_Available_Beds
    FROM Bed_Availability.Provider_By_Sector_Available_Overnight_Beds1
    GROUP BY
        Organisation_Code,
        Effective_Snapshot_Date
)

SELECT
    COALESCE(o.Organisation_Code, a.Organisation_Code) AS Organisation_Code,
    COALESCE(o.Effective_Snapshot_Date, a.Effective_Snapshot_Date) AS Effective_Snapshot_Date,

    ROUND(a.Total_Available_Beds, 1) AS Total_Available_Beds,
    ROUND(o.Total_Occupied_Beds, 1) AS Total_Occupied_Beds,

    CASE
        WHEN a.Total_Available_Beds = 0 THEN NULL
        ELSE ROUND(
            o.Total_Occupied_Beds * 100.0 / a.Total_Available_Beds,
            1
        )
    END AS Pct_Occupied

FROM Occupied o

FULL OUTER JOIN Available a
    ON o.Organisation_Code = a.Organisation_Code
    AND o.Effective_Snapshot_Date = a.Effective_Snapshot_Date

ORDER BY
    Organisation_Code,
    Effective_Snapshot_Date
"

df_beds <- dbGetQuery(con, query_beds)