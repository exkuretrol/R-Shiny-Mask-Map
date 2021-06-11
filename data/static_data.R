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
        醫事機構代碼  = col_character(),
        醫事機構名稱  = col_skip(),
        醫事機構地址  = col_skip(),
        醫事機構電話  = col_skip(),
        成人口罩剩餘數  = col_skip(),
        兒童口罩剩餘數  = col_skip(),
        來源資料時間  = col_skip(),
        x = col_character(),
        y = col_character()
    )
)

maskdata_url <- "https://data.nhi.gov.tw/resource/mask/maskdata.csv"
opening_time_data_url <- "https://data.nhi.gov.tw/resource/Opendata/全民健康保險特約院所固定服務時段.csv"
opening_time_data_url <- URLencode(opening_time_data_url)

maskdata <- read_csv(
    maskdata_url,
    col_types = cols(
        醫事機構代碼  = col_character(),
        醫事機構名稱  = col_character(),
        醫事機構地址  = col_character(),
        醫事機構電話  = col_character(),
        成人口罩剩餘數  = col_integer(),
        兒童口罩剩餘數  = col_integer(),
        來源資料時間  = col_character()
    )
)

opening_time_data <- read_csv(
    opening_time_data_url,
    col_types = cols(
        醫事機構代碼  = col_character(),
        醫事機構名稱  = col_skip(),
        業務組別  = col_skip(),
        特約類別  = col_skip(),
        看診年度  = col_skip(),
        看診星期  = col_character(),
        看診備註  = col_character(),
        開業狀況  = col_integer(),
        資料集更新時間  = col_character()
    )
)

Institute_data <- left_join(maskdata, opening_time_data, by = "醫事機構代碼")
Institute_data <- left_join(Institute_data, Institute_location)
Institute_data <- Institute_data %>%
    mutate(City = substr(醫事機構地址, 1, 3)) %>% 
    filter(!is.na(x) | !is.na(y))

rm(
    Institute_location,
    maskdata,
    opening_time_data,
    maskdata_url,
    opening_time_data_url
)

markerIcons <- awesomeIcons(
    icon = "plus",
    iconColor = "white",
    markerColor = sapply(Institute_data$成人口罩剩餘數, function(x) {
        if (x >= 200) { "green" }
        else if (x < 200 & x >= 100) { "yellow" }
        else if (x < 100 & x >= 50) { "orange" }
        else if (x < 50) { "red" }
    })
)

m <- Institute_data %>%
    leaflet() %>%
    addControlGPS(
        options = gpsOptions(
            position = "bottomleft",
            activate = TRUE,
            autoCenter = TRUE,
            maxZoom = 16,
            setView = TRUE
        )
    ) %>%
    addTiles() %>%
    addAwesomeMarkers(
        lng = ~as.double(x),
        lat = ~as.double(y),
        clusterOptions = markerClusterOptions(),
        icon = markerIcons,
        layerId = ~as.character(醫事機構代碼)
    ) %>%
    addLegend(
        position = "topright",
        colors = c("green", "yellow", "orange", "red"),
        labels = c("> 200", ">= 100", ">= 50", "< 50"),
        opacity = 1,
        title = "成人口罩數目"
    )

genHTMLTable <- function(OpeningHourVector) {
    str <- OpeningHourVector
    s <- sapply(
        seq(
            from=1, 
            to=nchar(str), 
            by=7
        ), 
        function(i) substr(str, i, i+6)
    )
    tags$table(
        style = "width: 100%;",
        tags$tr(
            tags$th("營業時間"),
            tags$th("一"),
            tags$th("二"),
            tags$th("三"),
            tags$th("四"),
            tags$th("五"),
            tags$th("六"),
            tags$th("日")
        ),
        lapply(X = 1:3, FUN = function(i) {
            tags$tr(
                lapply(X = 0:7, FUN = function(j) {
                    ss <- strsplit(s[i], "")
                    
                    tags$td(
                        if (j == 0) {
                            if (i == 1 & j == 0) { "上午" }
                            else if (i == 2 & j == 0) { "下午" }
                            else if (i == 3 & j == 0) { "晚上" }
                        }  else {
                            if (ss[[1]][j] == "N") { tags$i(class = "fas fa-check-circle") } 
                            else { tags$i(class = "fas fa-times-circle") }
                        }
                        
                    )
                })
            )
        })
    )
}

