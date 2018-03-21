library("RSQLite")

## connect to db
db <- dbConnect(drv=RSQLite::SQLite(), dbname="data/database.sqlite")

## list all tables
tables <- dbListTables(db)

Country <- dbReadTable(db, "Country")
League <- dbReadTable(db, "League")
Match <- dbReadTable(db, "Match")
Players <- dbReadTable(db, "Player")
Player_Attributes <- dbReadTable(db, "Player_Attributes")
Team <- dbReadTable(db, "Team")
Team_Attributes <- dbReadTable(db, "Team_Attributes")
sqlite_sequence <- dbReadTable(db, "sqlite_sequence")

