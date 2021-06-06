library(readxl)
library(dplyr)

# url = 'https://www.post.gov.tw/post/download/county_h_10706.xls'
# curl::curl_download(url = url, destfile = "./country.xls")
# country_xls <- read_xls("./country.xls", col_names = c("ZipCode", "CityDist", "CityDist_en"))
# CityDist <- country_xls %>%
#   select(CityDist) %>%
#   mutate(
#     # normal expression好難 :C
#     # Dist = gsub(".*[市縣]", "", CityDist)
#     City = substr(CityDist, 1, 3),
#     Dist = substr(CityDist, 4, length(CityDist))
#   ) %>% 
#   select(-CityDist)
# 
# saveRDS(CityDist, "./data/CityDist.rds")
CityDist <- readRDS("./data/CityDist.rds")

drawer1_level <- CityDist %>% 
  select(City) %>%
  unique() %>% 
  add_row(City = "請選擇縣市", .before = 1)

Institute_location <- read_csv(
  "./data/Institute_location.csv", 
    col_types = cols(
    醫事機構代碼 = col_character(),
    醫事機構名稱 = col_skip(),
    醫事機構地址 = col_skip(),
    醫事機構電話 = col_skip(),
    成人口罩剩餘數 = col_skip(),
    兒童口罩剩餘數 = col_skip(),
    來源資料時間 = col_skip(),
    x = col_character(),
    y = col_character()
  )
)
