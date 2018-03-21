
library(RSQLite)

##Connection a la base de donnees SQLite

filename <- "database.sqlite"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver, dbname = filename)

##Importation des donnees

dbListTables(db)
Country <- dbReadTable(db,"Country")
League <- dbReadTable(db,"League")
Match <- dbReadTable(db,"Match")
Player <- dbReadTable(db,"Player")
Player_Attributes <- dbReadTable(db,"Player_Attributes")
Team <- dbReadTable(db,"Team")
Team_Attributes <- dbReadTable(db,"Team_Attributes")
sqlite_sequence <- dbReadTable(db,"sqlite_sequence")

dbDisconnect(db)