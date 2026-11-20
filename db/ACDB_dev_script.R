library(DBI)
library(RSQLite)
library(dplyr)
library(readr)

#open database connection----
ACDB_dev <- dbConnect(RSQLite::SQLite(), "/Users/kiranbasava/DDL/ACDB_git/db/ACDB_dev.sql")

dbListTables(ACDB_dev)

#order of table creation is as follows: 1. sources, 2. species, 3. groups, 4. behaviors

dbExecute(ACDB_dev, "DROP TABLE sources;")

#sources table----
dbExecute(ACDB_dev, "CREATE TABLE sources (
  source_id TEXT PRIMARY KEY,
  title TEXT,
  year INTEGER(4),
  authors TEXT,
  doi TEXT
);")

sources <- read.csv("/Users/kiranbasava/DDL/ACDB/dev_Kiran/table_csvs/sources_feb17.25.csv", stringsAsFactors = FALSE)
dbWriteTable(ACDB_dev, "sources", sources, append=TRUE)
View(dbGetQuery(ACDB_dev, "SELECT * FROM sources ;")) #all records as a dataframe

dbGetQuery(ACDB_dev, "PRAGMA table_info([sources]);")  #checking primary key acknowledged

#species table----
dbExecute(ACDB_dev, "DROP TABLE species;")

species <- read.csv("/Users/kiranbasava/DDL/ACDB/dev_Kiran/table_csvs/species_jan23.25.csv", stringsAsFactors = FALSE)
dbExecute(ACDB_dev,"CREATE TABLE species (
    species_id TEXT PRIMARY KEY,
    common_name TEXT,
    GBIF INTEGER,
    canonicalName TEXT,
    Genus TEXT,
    Family TEXT,
    Ordr TEXT,
    Class TEXT,
    Phylum TEXT,
    primary_social_unit TEXT,
    unit_evidence TEXT,
    unit_source TEXT,
    IUCN TEXT,
    FOREIGN KEY (
        unit_source
    )
    REFERENCES sources (source_id) 
);")
dbWriteTable(ACDB_dev, "species", species, append=TRUE)

dbGetQuery(ACDB_dev, "PRAGMA table_info([species]);")  #make sure species_id is primary key
View(dbGetQuery(ACDB_dev, "SELECT * FROM species ;"))

#groups table----
groups <- read.csv("/Users/kiranbasava/DDL/ACDB/dev_Kiran/table_csvs/groups_jan24.25.csv", stringsAsFactors = FALSE)

dbExecute(ACDB_dev, "DROP TABLE groups;")

dbExecute(ACDB_dev,"CREATE TABLE groups (
    group_id INTEGER PRIMARY KEY AUTOINCREMENT,
    species_id TEXT REFERENCES species (species_id),
    group_name TEXT,
    group_level INTEGER,
    group_above TEXT,
    size INTEGER,
    size_evidence TEXT,
    size_date INTEGER,
    size_source TEXT REFERENCES sources (source_id),
    location_evidence TEXT,
    location_source TEXT REFERENCES sources (source_id),
    lat REAL,
    long REAL,
    FOREIGN KEY (
        species_id
    )
    REFERENCES species (species_id),
    FOREIGN KEY (
        size_source
    )
    REFERENCES sources (source_id),
    FOREIGN KEY (
        location_source
    )
    REFERENCES sources (source_id) 
);")
dbWriteTable(ACDB_dev, "groups", groups, append = TRUE)
dbGetQuery(ACDB_dev, "PRAGMA table_info([groups]);")  #make sure species_id is primary key

View(dbGetQuery(ACDB_dev, "SELECT * FROM groups ;"))

#behaviors table----
dbExecute(ACDB_dev, "DROP TABLE behaviors;")

behaviors <- read.csv("/Users/kiranbasava/DDL/ACDB/dev_Kiran/table_csvs/behaviors_feb18.25.csv", stringsAsFactors = FALSE)
dbExecute(ACDB_dev, 
          "CREATE TABLE behaviors (
    behavior_id          INTEGER PRIMARY KEY AUTOINCREMENT,
    behavior             TEXT,
    group_id             INTEGER,
    behavior_description TEXT,
    behavior_source      TEXT,
    start_date           INTEGER,
    end_date             INTEGER,
    vertical             TEXT,
    vertical_evidence    TEXT,
    vertical_source      TEXT,
    horizontal           TEXT,
    horizontal_evidence  TEXT,
    horizontal_source    TEXT,
    oblique              TEXT,
    oblique_evidence     TEXT,
    oblique_source       TEXT,
    domains              TEXT,
    domains_evidence     TEXT,
    domains_source       TEXT,
    anth_effects         TEXT,
    anth_source          TEXT,
    FOREIGN KEY (
        group_id
    )
    REFERENCES groups (group_id),
    FOREIGN KEY (
        behavior_source
    )
    REFERENCES sources (source_id),
    FOREIGN KEY (
        vertical_source
    )
    REFERENCES sources (source_id),
    FOREIGN KEY (
        horizontal_source
    )
    REFERENCES sources (source_id),
    FOREIGN KEY (
        oblique_source
    )
    REFERENCES sources (source_id),
    FOREIGN KEY (
        domains_source
    )
    REFERENCES sources (source_id),
    FOREIGN KEY (
        anth_source
    )
    REFERENCES sources (source_id) 
);
")
dbWriteTable(ACDB_dev, "behaviors", behaviors, append = TRUE)
View(dbGetQuery(ACDB_dev, "SELECT * FROM behaviors ;"))
dbGetQuery(ACDB_dev, "PRAGMA table_info([behaviors]);")  #make sure species_id is primary key

#write to csvs----
library(readr)

behaviors <- as.data.frame(dbGetQuery(ACDB_dev, "SELECT * FROM behaviors ;"))
write_excel_csv(behaviors, "/Users/kiranbasava/DDL/ACDB_git/db/behaviors.csv")

sources <- as.data.frame(dbGetQuery(ACDB_dev, "SELECT * FROM sources ;"))
write_excel_csv(sources, "/Users/kiranbasava/DDL/ACDB_git/db/table_csvs/sources.csv")

species <- as.data.frame(dbGetQuery(ACDB_dev, "SELECT * FROM species ;"))
write_excel_csv(species, "/Users/kiranbasava/DDL/ACDB_git/db/table_csvs/species.csv")

groups <- as.data.frame(dbGetQuery(ACDB_dev, "SELECT * FROM groups ;"))
write_excel_csv(groups, "/Users/kiranbasava/DDL/ACDB_git/db/groups.csv")




#disconnect----
dbDisconnect(ACDB_dev)
