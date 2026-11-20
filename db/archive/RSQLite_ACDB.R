library(ggmap)
register_google(key = "AIzaSyACiSeLxGlJhJN2ZabXQeg5soyGmulPE5I")
groups_nov2824 <- read.csv(file="/Users/kiranbasava/DDL/ACDB/oct18_testtables/nov2824_groups.csv")
locs <- as.data.frame(unique(groups_nov2824$location))
View(locs)
class(locs)
colnames(locs)[1] <- "location"
geocode <- mutate_geocode(locs, location) #hey that worked!

library(DBI)
library(RSQLite)

# groups_nov2824 <- merge(groups_nov2824, geocode, by="location", all.x=T, all.y=F)
# write.csv(groups_nov2824, file="/Users/kiranbasava/DDL/ACDB/oct18_testtables/nov2824_groups.csv")
# 
# mydb <- dbConnect(RSQLite::SQLite(), "ACDB_v0.db")
# mydb <- dbConnect("/Users/kiranbasava/DDL/ACDB/oct18_testtables/ACDB_v0.sql")
# 
# dbGetQuery(mydb, "SELECT * FROM species;")
# src_dbi(mydb)

#ok this isn't working
#I'll try to import them as csvs directly into R
setwd("/Users/kiranbasava/DDL/ACDB/oct18_testtables/")
sources01 <- read.csv("nov2624_sources.csv")
dbWriteTable(mydb, name="sources", value=sources01)
src_dbi(mydb)
dbGetQuery(mydb, "SELECT * FROM sources WHERE year > 2010 ;")

#open database connection----
ACDB_v01 <- dbConnect(RSQLite::SQLite(), "/Users/kiranbasava/DDL/ACDB/oct18_testtables/ACDB_v01.sql")
src_dbi(ACDB_v01)

dbListTables(ACDB_v01)

#disconnect----
dbDisconnect(ACDB_v01)

#sources table----
dbExecute(ACDB_v01, 
"CREATE TABLE sources (
  source_id TEXT PRIMARY KEY,
  title TEXT,
  year INTEGER,
  authors TEXT,
  doi TEXT
);")

sources <- read.csv("/Users/kiranbasava/DDL/ACDB/oct18_testtables/dec924_sources.csv", stringsAsFactors = FALSE)
dbWriteTable(ACDB_v01, "sources", sources, overwrite=, append = )
dbGetQuery(ACDB_v01, "SELECT * FROM sources WHERE year > 2010 ;")
View(dbGetQuery(ACDB_v01, "SELECT * FROM sources ;"))

#species table----
species <- read.csv("/Users/kiranbasava/DDL/ACDB/oct18_testtables/dec924_species.csv", stringsAsFactors = FALSE)
dbExecute(ACDB_v01,
    "CREATE TABLE species (
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
dbWriteTable(ACDB_v01, "species", species, overwrite=, append = )
dbGetQuery(ACDB_v01, "SELECT * FROM species WHERE Class LIKE '%Aves%' ;")
View(dbGetQuery(ACDB_v01, "SELECT * FROM species ;"))


#groups table----
groups <- read.csv("/Users/kiranbasava/DDL/ACDB/oct18_testtables/dec924_groups.csv", stringsAsFactors = FALSE)

#fuck I forgot to reappend the location data
groupsold <- read.csv("/Users/kiranbasava/DDL/ACDB/oct18_testtables/nov2824_groups.csv", stringsAsFactors = FALSE)
groups <- merge(groups[c(1:9)], groupsold[c(1,10:13)], by="group_id", all.x=T)
write.csv(groups, "/Users/kiranbasava/DDL/ACDB/oct18_testtables/dec1324_groups.csv")

dbExecute(ACDB_v01,
"CREATE TABLE groups (
    group_id TEXT PRIMARY KEY,
    species_id TEXT REFERENCES species (species_id),
    group_name TEXT,
    group_level INTEGER,
    group_above TEXT,
    size INTEGER,
    size_evidence TEXT,
    size_date INTEGER,
    size_source TEXT REFERENCES sources (source_id),
    location_evidence TEXT,
    lat REAL,
    long REAL,
    location_source TEXT REFERENCES sources (source_id),
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
dbWriteTable(ACDB_v01, "groups", groups, overwrite=, append = )

View(dbGetQuery(ACDB_v01, "SELECT * FROM groups ;"))
View(dbGetQuery(ACDB_v01, "SELECT * FROM species ;"))


#behaviors table----
behaviors <- read.csv("/Users/kiranbasava/DDL/ACDB/oct18_testtables/dec924_behaviors.csv", stringsAsFactors = FALSE)
dbExecute(ACDB_v01, 
    "CREATE TABLE behaviors (
    behavior_id          TEXT    PRIMARY KEY,
    [beh.key]            TEXT,
    behavior             TEXT,
    group_id             TEXT,
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
dbWriteTable(ACDB_v01, "behaviors", behaviors, overwrite=, append = )
View(dbGetQuery(ACDB_v01, "SELECT * FROM behaviors ;"))

View(dbGetQuery(ACDB_v01, 
    "SELECT * FROM groups LEFT JOIN behaviors ON groups.group_id = behaviors.group_id;"))

#update data entry through R, make sure changes update across tables
#write queries through multiple tables
#push initial release to github



#ok let's try updating some of the records here in R----
#hopefully simple: add 'Avacha Gulf resident killer whales' as 'group above' for Avacha, K20, and K19 acoustic clans in groups table
#I guess if I had a 'group above' in the groups table and it's not already a group it should be added as a record automatically?
dbExecute(ACDB_v01, 
          "UPDATE groups
          SET group_above = 'Avacha Gulf resident killer whales'
          WHERE group_id IN('g7','g8','g9') ;"
)
#it worked!

#ok let's try something else
#I'm adding two orang groups, and two new corresponding species 
View(dbGetQuery(ACDB_v01, 
                "SELECT * FROM groups ;"))

dbExecute(ACDB_v01,
  "INSERT INTO groups (group_id, species_id, group_name, size, size_evidence, size_source, location_evidence, location_source)
 VALUES ('g36', 'Pongo_abelii', 'Suaq Balimbing orangutans', 200, 'In the last 20 years we have identified more than 200 orangutans at Suaq', 'suaq.org/research', 'Suaq Balimbing monitoring station, Sumatra, Indonesia', 'schupplivanschaik2019') ;"
)

dbExecute(ACDB_v01, 
          "UPDATE groups
          SET lat = 3.066667, long = 97.433333
          WHERE group_id IN('g36') ;"
)

View(dbGetQuery(ACDB_v01, 
                "SELECT * FROM sources ;"))

#and now I need to add Pongo abelii to the species table, which is probably another thing that is done automatically
dbExecute(ACDB_v01,
          "INSERT INTO species (species_id, common_name, GBIF, canonicalName, Genus, Family, Ordr, Class, Phylum, IUCN)
 VALUES ('Pongo_abelii', 'Sumatran orangutans', 5707420, 'Pongo_abelii', 'Pongo', 'Hominidae', 'Primates', 'Mammalia', 'Chordata', 'CR') ;"
)

#adding three behaviors for Suaq orangs----
View(dbGetQuery(ACDB_v01, 
                "SELECT * FROM behaviors ;"))

dbListFields(ACDB_v01, "behaviors")

dbExecute(ACDB_v01, 
          "INSERT INTO behaviors (behavior_id, [beh.key], behavior, group_id, group_name, behavior_description, behavior_source)
          VALUES ('b26g36','b26','foraging, Neesia tool use','g36','Suaq Balimbing orangutans','using stick tools to extract seeds from Neesia fruits','vanschaikknott2001'),
          ('b27g36','b27','architecture, bunk nesting','g36','Suaq Balimbing orangutans','during rain, building a nest above usual resting nest','schupplivanschaik2019'),
          ('b28g36','b28','foraging, slow loris hunting','g36','Suaq Balimbing orangutans','finding and eating slow lorises hiding in vegetation','schupplivanschaik2019');"
          )
          
View(dbGetQuery(ACDB_v01, 
                "SELECT * FROM sources ;"))

dbExecute(ACDB_v01, 
          "INSERT INTO sources (source_id, title, year, authors, doi)
          VALUES ('vanschaikknott2001','Geographic variation in tool use on Neesia fruits in orangutans',	'2001','Van Schaik, Carel and Knott, Cheryl','10.1002/ajpa.1045'),
          ('schupplivanschaik2019','Animal cultures how weve only seen the tip of the iceberg','2019','Schuppli, Caroline and Van Schaik, Carel','10.1017/ehs.2019.1');"
          )

#technically I want it to check for the next unique id in the group_id sequence and add it automatically if a new group record is added
#and if a new source is added a new  record should be created in the sources table?
#also latitude and longitude


#alphanumeric ids for primary keys----
paste0("g",seq(1:20))
paste0("b",seq(1:20))


#possibly best to just have a character column with the letter (b or g) and an incrementing column for the number, then composite primary key


library(rgbif)
transient_gbif <- name_backbone_checklist("Pongo_abelii")
transient_gbif <- transient_gbif[,c("verbatim_name", "usageKey", "canonicalName", "genus", "family", "order", "class", "phylum")]
View(transient_gbif)

#and add that to the species table


#IUCN data----
#add to species table
library(taxize)
install.packages('ropensci')
library(rredlist)
remotes::install_github("ropensci/rredlist")
IUCN_REDLIST_KEY ='CYcdGadq4XE3ypKLzcLkNRyxiG76YawjYC3j'
rl_species_latest(genus="Orcinus", species="orca", key='CYcdGadq4XE3ypKLzcLkNRyxiG76YawjYC3j')$conservation_actions
rl_species_latest(genus="Physeter", species="macrocephalus", key='CYcdGadq4XE3ypKLzcLkNRyxiG76YawjYC3j')$conservation_actions

#string manipulations----
#convert authors list to key in formats 'coauthor1coauthor2year' or 'firstauthoryear'

#
