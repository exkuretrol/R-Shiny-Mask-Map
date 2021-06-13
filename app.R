library(shiny)
library(dplyr)
library(bs4Dash)
library(waiter)
library(leaflet)
library(leaflet.extras)
library(readr)
library(plotly)
library(pool)
library(RMariaDB)
library(dotenv)
library(dbplyr)
library(rintrojs)

# comment when publish to shinyapps.io
# setwd("~/workspace/R-Shiny/mask/")

# MySQL connection ----
dotenv::load_dot_env(file = "./.env")
db_user <- Sys.getenv("db_user")
db_password <- Sys.getenv("db_password")
db_host <- Sys.getenv("db_host")
db_port <- as.numeric(Sys.getenv("db_port"))
db_name <- if(Sys.getenv("db_name") == "") "mask"

pool <- dbPool(
    drv = MariaDB(),
    host = db_host,
    username = db_user,
    password = db_password,
    dbname = db_name
)
onStop(function() {
    poolClose(pool)
})

# prepare data ----
source("./data/static_data.R")

# make preloader ----
preloader <- list(html = tagList(waiter::spin_google(), "讀取中..."), color = "#343a40")

# UI section ----
ui <- dashboardPage(
    ## dashboardHeader ----
    header = dashboardHeader(
        title = dashboardBrand(
            title = "藥局 & 口罩資訊地圖",
            color = "white",
            image = "./assets/android-chrome-512x512.png"
        ),
        controlbarIcon = icon("filter"),
        tags$head(
            tags$style(
                type = "text/css", 
                "#map {height: calc(100vh - 90px) !important;}",
                "#shiny-modal-wrapper {overflow: hidden;}",
                "#helpBtn {background-color: rgba(0, 0, 0, 0); border: none;}",
                "body > div.wrapper > div.content-wrapper > section {padding: 0px !important;}"
            ),
            includeHTML("./Intropage/favicon.html")
            # includeCSS("./www/css/style.css")
        ),
        
        # custom help buttom
        tags$ul(
            class = "navbar-nav",
            tags$li(
                class = "nav-item",
                actionButton(
                    inputId = "helpBtn", label = "幫助", icon = icon("life-ring")
                )
            )
        )
    ),
    ## dashboardSidebar ----
    sidebar = dashboardSidebar(
        sidebarMenu(
            id = "test",
            sidebarHeader("資料視覺化期末報告"),
            menuItem(
                text = "口罩地圖",
                tabName = "tab1",
                icon = icon("map-marked")
            ),
            menuItem(
                text = "研究項目",
                icon = icon("pen"),
                startExpanded = FALSE,
                menuSubItem(
                    text = "研究項目A",
                    tabName = "tab2_1",
                    icon = icon("pen")
                ),
                menuSubItem(
                    text = "研究項目B",
                    tabName = "tab2_2",
                    icon = icon("pen")
                ),
                menuSubItem(
                    text = "研究項目C",
                    tabName = "tab2_3",
                    icon = icon("pen")
                )
            ),
            menuItem(
                text = "分工表",
                tabName = "tab98",
                icon = icon("tasks")
            ),
            menuItem(
                text = "參考資料",
                tabName = "tab99",
                icon = icon("book")
            )
        ),
        # customArea = fluidPage(
        #     "Custom Area Here!"
        # ),
        skin = "light",
        collapsed = TRUE,
        expandOnHover = FALSE
    ),
    ## dashboardBody ----
    body = dashboardBody(
        tabItems(
            
            ### tab1 Page content ----
            tabItem(
                tabName = "tab1",
                leafletOutput(
                    outputId = "map"
                )
            ),
            
            ### tab2_1 Page content ----
            tabItem(
                tabName = "tab2_1",
                includeHTML("./Intropage/pageA.html")
            ),
            
            ### tab2_2 Page content ----
            tabItem(
                tabName = "tab2_2",
                includeHTML("./Intropage/pageB.html")
            ),
            
            ### tab2_3 Page content ----
            tabItem(
                tabName = "tab2_3",
                includeHTML("./Intropage/pageC.html")
            ),
            
            ### tab98 Page content ----
            tabItem(
                tabName = "tab98",
                fluidRow(
                    lapply(1:3, FUN = function(i) {
                        sortable(
                            width = 4,
                            p(class = "text-center", paste("Column", i)),
                            lapply(1:2, FUN = function(j) {
                                box(
                                    title = paste0("I am the ", j, "-th card of the ", i, "-th column"),
                                    width = 12,
                                    "Click on my header"
                                )
                            })
                        )
                    })
                )
            )
        ),
        use_waitress()
    ),
    
    ## dashboardControlbar ----
    controlbar = dashboardControlbar(
        introjsUI(),
        div(
            selectInput(
                inputId = "drawer1",
                label = "縣市:",
                choices = drawer1_level
            ),
            conditionalPanel(
                "input.drawer1 != '請選擇縣市'",
                selectInput(
                    inputId = "drawer2",
                    label = "鄉/鎮/市/區:",
                    choices = NULL
                )
            ),
            class = "p-3"
        ),
        id = "controlbar",
        collapsed = FALSE,
        skin = "light",
        pinned = TRUE
    ),
    title = "藥局 & 口罩資訊地圖",
    preloader = preloader
)

# Server Section ----
server <- function(input, output, session) {
    
    ## Introjs ----
    steps <- reactive(
        data.frame(
            title = c("導覽"),
            element = c(
                NA, 
                
                # filter
                "body > div.wrapper > nav > ul.navbar-nav.ml-auto.navbar-right > li",
                
                # drawer1
                "#controlbarTitle > div.os-padding > div > div > div > div.form-group.shiny-input-container", 
                
                # control bar pin
                "#controlbarPin",
                
                # whole map
                "#map", 
                
                # map pin(not work :c)
                "#map", 
                
                # gps button
                "#map > div.leaflet-control-container > div.leaflet-bottom.leaflet-left > div > a",
                
                NA
            ),
            intro = c(
                "你好~ 你剛剛點擊的按鈕是導覽按鈕😋",
                "這邊你可以控制整個篩選介面的開關👀",
                "然後，這裡有一個下拉式選單，可以讓你篩選縣市、鄉鎮市區，然後自動切換地圖到該區域🤔",
                "這個圖釘📌可以控制整個篩選介面要不要固定",
                "中間這塊是用 leaflet.js 做的地圖，右上角有簡單的圖例，會根據口罩剩餘數量把地圖圖釘上色🟢",
                "地圖上的圖標📍其實是可以點的，待會可以試試。",
                "最後，使用這個定位按鈕，當你點開地圖上的圖釘時，會多出一個按鈕可以自動開啟 Google Map 規劃路線🎯。",
                "以上 👍"
            ),
            position = c(
                NA, "bottom", "left", "left", NA, "left", "top", NA
            )
        )
    )
    
    observeEvent(input$helpBtn,{
        introjs(
            session,
            options = list(
                steps = steps(),
                nextLabel = "好👌",
                prevLabel = "等等"
            )
        )
        
    })
    
        
    ## Failed Message Model ----
    msgModel <- function(failed_msg = "") {
        modalDialog(
            title = "錯誤訊息",
            size = "s",
            div(tags$b(failed_msg, style = "color: red;")),
            easyClose = TRUE,
            footer = tagList(
                modalButton(label = "確定")
            )
        )
    }
    
    ## Initial reactive values ----
    rv <- reactiveValues(
        M_id = NULL,
        M_name = NULL,
        map_data = Institute_data,
        plot_data = NULL,
        last_City = NULL,
        last_Dist = NULL,
        isLocated = FALSE,
        gpsLng = NULL,
        gpsLat = NULL
    )
    
    ## observe Event ----
    observeEvent(input$drawer1, {
        
        if (input$drawer1 == "請選擇縣市") {
            
            x <- NULL
            
        } else {
            
            x <- CityDist %>%
                filter(City %in% input$drawer1) %>%
                select(Dist) %>%
                add_row(Dist = "請選擇鄉/鎮/市/區", .before = 1)
            
            updateSelectInput(
                session = session,
                inputId = "drawer2",
                label = "鄉/鎮/市/區:",
                choices = x
            )
            
            if (rv$last_City != input$drawer1 & rv$last_Dist == "請選擇鄉/鎮/市/區") {
                
                rv$map_data <- Institute_data %>%
                    filter(City == input$drawer1) 
                
            }
            
        }    
        rv$last_City <- input$drawer1
    })
    
    observeEvent(input$drawer2, {
        
        if (input$drawer1 != "請選擇縣市" & input$drawer2 == "請選擇鄉/鎮/市/區") {
            
            rv$map_data <- Institute_data %>%
                filter(City == input$drawer1)
            
        }  else if (input$drawer1 != "請選擇縣市" & input$drawer2 != "請選擇鄉/鎮/市/區") {
            
            
            data <- Institute_data %>%
                filter(City == input$drawer1)
            
            data <- data[grep(pattern = input$drawer2, x = data$醫事機構地址), ]
            
            if (nrow(data) == 0) {
                
                showModal(msgModel("找不到該地區的藥局，或是沒有資料"))
                return()
                
            } else {
                
                rv$map_data <- data
                
            }
            
        } else {
            
            rv$map_data <- Institute_data
            
        }
        rv$last_Dist <- input$drawer2
    })
    
    # draw map ----
    output$map <- renderLeaflet({
        
        if (!is.null(input$drawer2)) {
            map_data <- rv$map_data
            
            minLng <- min(as.double(map_data$x))
            minLat <- min(as.double(map_data$y))
            maxLng <- max(as.double(map_data$x))
            maxLat <- max(as.double(map_data$y))
            
            # when we only have one observation, use set View Instead.
            if (minLng == maxLng & minLat == maxLat) {
                
                m <- m %>% setView(lng = minLng, lat = maxLat, zoom = 17)
                
            } else {
                
                m <- m %>%
                    fitBounds(
                        lng1 = minLng,
                        lat1 = minLat,
                        lng2 = maxLng,
                        lat2 = maxLat,
                        options = list(maxZoom = 17)
                    )
                
            }
            
        } else {
            
            m <- m %>% 
                # center of Taiwan
                setView(
                    lng = 120.97388194444444,
                    lat = 23.97565,
                    zoom = 7
                )
            
        }
        
        m
    })
    
    # observe when user is Located, save coordinates.
    observe({
        x <- input$map_gps_located
        if (!is.null(x)) { rv$isLocated <- TRUE }
        rv$gpsLat <- x$coordinates[1]
        rv$gpsLng <- x$coordinates[2]
    })
    
    ## pop-up Dialog ----
    observe({
        click <- input$map_marker_click
        if (is.null(click)) {
            return()
        }
        
        # pop up message at least once.
        if (exists("API_NO_DATA")) {
            if (API_NO_DATA == 1) {
                showModal(
                    msgModel(
                        "目前的資料為歷史資料，因為API目前抓不到口罩剩餘數量；此錯誤通常是禮拜日才會發生。"
                    )
                )
            # remove global variable API_NO_DATA.
            rm(API_NO_DATA, pos = ".GlobalEnv")
            return()
            }
        }
        
        selected_Institute <- Institute_data %>%
            filter(醫事機構代碼 == click$id) 
        
        rv$plot_data <- NULL
        rv$M_id <- click$id
        rv$M_name <- selected_Institute$醫事機構名稱
        
        if (rv$isLocated) {
            url <- paste0(
                "https://www.google.com/maps/dir/?api=1&origin=", 
                rv$gpsLat, ",", rv$gpsLng, 
                "&destination=",selected_Institute$y, ",", selected_Institute$x,
                collapse = ''
            )
            url <- URLencode(url)
        }
        
        Popup <- paste0(
            tags$strong("醫事機構地址: "), selected_Institute$醫事機構地址, tags$br(),
            tags$strong("醫事機構電話: "), selected_Institute$醫事機構電話, tags$br(),
            tags$strong("成人口罩剩餘數: "), selected_Institute$成人口罩剩餘數, tags$br(),
            tags$strong("兒童口罩剩餘數: "), selected_Institute$兒童口罩剩餘數, tags$br(),
            tags$strong("開業狀況: "), if (!is.na(selected_Institute$開業狀況)) {if (selected_Institute$開業狀況 == 0) "正常營業" else "暫停營業"} else { "" }, tags$br(),
            tags$strong("看診備註: "), selected_Institute$看診備註,
            tags$br(),
            genHTMLTable(selected_Institute$看診星期),
            p("來源資料時間: ", selected_Institute$來源資料時間, style = "text-align: right; color: lightslategray;"),
            tags$br(),
            collapse = ''
        ) %>% HTML()
        
        
        showModal(
            modalDialog(
                title = selected_Institute$醫事機構名稱,
                use_waitress(),
                Popup,
                easyClose = TRUE,
                footer = tagList(
                    if (rv$isLocated) tags$a(tags$button(icon("directions"), "規劃路線", class = "btn btn-default"), href = url),
                    actionButton(inputId = "plotMaskToggle", "顯示歷史資料"),
                    modalButton(label = "確定")
                )
            )
        )
        
    })
    
    observeEvent(input$plotMaskToggle, {
        removeModal()
        
        rv$plot_data <- pool %>% 
            tbl("masklog") %>%
            filter(MedicalInstitute_M_id == local(rv$M_id))
        
        waitress$start()
        
        if (!is.null(rv$plot_data)) {  
            showModal(
                modalDialog(
                    title = rv$M_name,
                    size = "l",
                    plotlyOutput("plotMask"),
                    easyClose = TRUE,
                    footer = tagList(
                        modalButton(label = "確定")
                    )
                )
            )
        }
        
    })
    
    # css selector not work, dunno why
    waitress <- Waitress$new(
        # selector = "#shiny-modal > div > div",
        theme = "overlay-percent"
    )
    
    ## plot Mask history ----
    output$plotMask <- renderPlotly({
        
        for(i in 1:10){
            waitress$inc(10)
            Sys.sleep(.3)
        }
        
        fig <- rv$plot_data %>%
            as_tibble() %>%
            plot_ly() %>%
            add_lines(
                x = ~m_datetime,
                y = ~adult_mask,
                name = "成人口罩",
                mode = 'lines',
                fill='tozeroy'
            ) %>%
            add_lines(
                x = ~m_datetime,
                y = ~child_mask,
                name = "兒童口罩",
                fill='tozeroy'
            ) %>%
            layout(
                title = list(
                    text = "30天內口罩數量圖",
                    x = 1,
                    y = "auto"
                ),
                xaxis = list(
                    title = "時間",
                    rangeselector = list(
                        buttons = list(
                            list(
                                count = 1,
                                label = "過去24小時",
                                step = "day",
                                stepmode = "backward"
                            ),
                            list(
                                count = 3,
                                label = "過去3天",
                                step = "day",
                                stepmode = "backward"
                            ),
                            list(
                                count = 7,
                                label = "過去1週",
                                step = "day",
                                stepmode = "backward"
                            ),
                            list(
                                count = 14,
                                label = "過去2週",
                                step = "day",
                                stepmode = "todate"
                            ),
                            list(step = "all")
                        )
                    ),
                    rangeslider = list(
                        thickness = "0.05"
                    )
                ),
                yaxis = list(title = "口罩數量"),
                legend = list(
                    orientation = "v",
                    x = 0
                ),
                hovermode = "x"
            ) %>%
            config(displayModeBar = FALSE)
        
        waitress$close()
        fig
    })
    
    
}

shinyApp(ui = ui, server = server)