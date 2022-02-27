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

con <- dbConnect(
  drv = RMySQL::MySQL(),
  host = db_host,
  user = db_user,
  password = db_pass,
  port = db_port,
  dbname = db_name
)


# sql_statement <- "show tables"
# dbGetQuery(pool, sql_statement)

# dbReadTable(con, "MedicalInstitute")

#從網路上下載資料 ----
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
    M_id = col_character(), # 留下
    M_name = col_character(), # 留下
    M_addr = col_character(), # 留下
    M_num = col_character(), # 留下
    adult_mask = col_integer(), # 留下
    child_mask = col_integer(), # 留下
    m_datetime = col_character() # 留下
  ), 
  skip = 1
)

masklog <- maskdata %>%
  mutate(m_datetime = as.POSIXct(m_datetime))


# 因為網址是中文要編譯成URLCode
url <- URLencode("https://data.nhi.gov.tw/resource/Opendata/全民健康保險特約院所固定服務時段.csv")

Medical_Institute_opening_time <- read_csv(
  file = url,
  col_types = cols(
    醫事機構代碼 = col_character(), # 留下
    醫事機構名稱 = col_character(), 
    業務組別 = col_character(), 
    特約類別 = col_character(),
    看診年度 = col_integer(),
    看診星期 = col_character(), # 留下
    看診備註 = col_character(), # 留下
    開業狀況 = col_integer(), # 留下
    資料集更新時間 = col_character() # 留下
  )
)


Medical_Institute_opening_time <- Medical_Institute_opening_time %>%
  select(
    "M_id" = 醫事機構代碼, 
    "M_opening_hour" = 看診星期,
    "M_comment" = 看診備註,
    "M_isclosed" = 開業狀況,
    "m_datetime" = 資料集更新時間
  ) %>%
  mutate(m_datetime = as.POSIXct(m_datetime, format = "%Y%m%d%H%M%S"))

MedicalInstitute <- left_join(masklog, Medical_Institute_opening_time, by = "M_id")
MedicalInstitute <- MedicalInstitute %>%
  select(-c("adult_mask", "child_mask", "m_datetime.x")) %>%
  rename(m_datetime = m_datetime.y)


dbSendQuery(con, "SET GLOBAL local_infile = true;")

dbSendQuery(con, "TRUNCATE `mask`.`MedicalInstitute`")
dbWriteTable(
  conn = con,
  name = "MedicalInstitute",
  value = MedicalInstitute, 
  append = TRUE,
  temporary= FALSE, 
  row.names = FALSE
)

dbDisconnect(con)