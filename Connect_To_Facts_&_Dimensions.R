library(odbc)
library(DBI)

con <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "ODBC Driver 17 for SQL Server",
  Server = "factsanddimensions.database.windows.net",
  Database = "Health_Foundation_UserDB",
  UID = "Health_Foundation",
  PWD = "2933Werthers91334Orion", 
  Encrypt = "yes"
  
)