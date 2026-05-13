####################
### Pulling data ###
####################

# Verifies all necessary environment variables are set
if (Sys.getenv("AIRTABLE_API_KEY") == "")
  print("API key not found") else print("API key found")

if (Sys.getenv("AIRTABLE_BASE_NAME") == "")
  print("Base name not found") else print("Base name found")

if (Sys.getenv("AIRTABLE_L3DB_NAME") == "")
  print("Lesson 3 table name not found") else print("Lesson 3 table name found")

if (Sys.getenv("AIRTABLE_SITEDB_NAME") == "")
  print("Site table name not found") else print("Site table name found")

if (Sys.getenv("AIRTABLE_ORGANIZATIONDB_NAME") == "")
  print("Organization table name not found") else print("Organization table name found")

if (Sys.getenv("AIRTABLE_FISHDB_NAME") == "")
  print("Fish table name not found") else print("Fish table name found")

key                   <- Sys.getenv("AIRTABLE_API_KEY")
base                  <- Sys.getenv("AIRTABLE_BASE_NAME")
l3_table_name         <- Sys.getenv("AIRTABLE_L3DB_NAME")
site_table_name       <- Sys.getenv("AIRTABLE_SITEDB_NAME")
organization_table_name <- Sys.getenv("AIRTABLE_ORGANIZATIONDB_NAME")
fish_table_name       <- Sys.getenv("AIRTABLE_FISHDB_NAME")

ldat <- get_airtable_records(base, l3_table_name, key)
sdat <- get_airtable_records(base, site_table_name, key)
odat <- get_airtable_records(base, organization_table_name, key)
fdat <- get_airtable_records(base, fish_table_name, key)
