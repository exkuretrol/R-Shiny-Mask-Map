# Sys_getenv("varname")
dotenv::load_dot_env(file = ".env")

library(pool)
library(dplyr)
library(readr)
library(RMariaDB)

db_user <- Sys.getenv("db_user")
db_pass <- Sys.getenv("db_password")
db_host <- Sys.getenv("db_host")
db_port <- as.numeric(Sys.getenv("db_port"))
db_name <- "mask"

maskdata <- read_csv(
  file = "https://data.nhi.gov.tw/resource/mask/maskdata.csv", 
  col_names = c(
    "M_id",
    "M_name",
    "M_addr",
    "M_num",
    "adult_mask",
    "child_mask",
    "m_datetime"
  ),
  col_types = cols(
    M_id = col_character(),
    M_name = col_character(),
    M_addr = col_character(),
    M_num = col_character(),
    adult_mask = col_integer(),
    child_mask = col_integer(),
    m_datetime = col_character()
  ), 
  skip = 1
)

masklog <- maskdata %>%
  select(-c(M_name, M_addr, M_num)) %>%
  rename(MedicalInstitute_M_id = M_id) %>%
  mutate(m_datetime = as.POSIXct(m_datetime))

# MySQL connection ----
con <- dbConnect(
  drv = RMariaDB::MariaDB(),
  host = db_host,
  user = db_user,
  password = db_pass,
  port = db_port,
  dbname = db_name
)

dbWriteTable(
  conn = con, 
  name = "masklog", 
  masklog, 
  append = TRUE, 
  overwrite = FALSE, 
  row.names = FALSE
)

dbDisconnect(con)
