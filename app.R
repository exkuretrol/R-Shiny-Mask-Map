library(shiny)
library(dplyr)
library(bs4Dash)
library(waiter)
library(leaflet)
library(leaflet.extras)
library(readr)

# comment when publish to shinynapps.io
# setwd("~/workspace/R-Shiny/mask/")

# prepare data ----
source("./data/tidydata.R")

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
            tags$style(type = "text/css", "#map {height: calc(100vh - 90px) !important;}"),
            includeHTML("./Intropage/favicon.html")
            # includeCSS("./www/css/style.css")
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
        )
    ),
    
    ## dashboardControlbar ----
    controlbar = dashboardControlbar(
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
        pinned = FALSE
    ),
    title = "藥局 & 口罩資訊地圖",
    preloader = preloader
)

# Server Section ----
server <- function(input, output, session) {
    observeEvent(input$drawer1, {
        
        if (input$drawer1 == "請選擇縣市") {
            
            x <- NULL
            
        } else {
            x <- CityDist %>%
                filter(City %in% input$drawer1) %>%
                select(Dist)
        }
        
        updateSelectInput(
            session = session,
            inputId = "drawer2",
            label = "鄉/鎮/市/區:",
            choices = x
        )
        
    })
    
    # oberveEvent when tab99 clicked, a pop-up dialog appear
    observeEvent(input$test, {
        if (input$test == "tab99") {
            showModal(
                modalDialog(
                    title = "Thank you so much",
                    "
                    You clicked me! This event is the result of
                    an input bound to the menu. By adding an id to the
                    bs4SidebarMenu, input$id will give the currently selected
                    tab. This is useful to trigger some events.
                    ",
                    easyClose = TRUE,
                    footer = NULL
                )
            )
        }
    })
    
    ## output Map ----
    
    rv <- reactiveValues(
        time = Sys.time(),
        map_data = Institute_data
    )
    
    observeEvent(input$drawer2, {
        
        if (input$drawer1 != "請選擇縣市" & !is.null(input$drawer2)) {
            
            rv$map_data <- Institute_data %>%
                filter(substr(醫事機構地址, 1, 3) == input$drawer1)
            
        } else {
            
            rv$map_data <- Institute_data
            
        }
        
    })
    
    # TODO: improve efficiency
    output$map <- renderLeaflet({
        map_data <- rv$map_data %>%
            filter(!is.na(x) | !is.na(y)) 
        
        popup_items <- paste0(
                tags$strong("醫事機構名稱: "), map_data$醫事機構名稱, tags$br(),
                tags$strong("醫事機構地址: "), map_data$醫事機構地址, tags$br(),
                tags$strong("醫事機構電話: "), map_data$醫事機構電話, tags$br(),
                tags$strong("成人口罩剩餘數: "), map_data$成人口罩剩餘數, tags$br(),
                tags$strong("兒童口罩剩餘數: "), map_data$兒童口罩剩餘數, tags$br(),
                tags$strong("開業狀況: "), map_data$開業狀況, tags$br(),
                tags$strong("來源資料時間: "), map_data$來源資料時間
            ) %>% lapply(., HTML)
        
        markerIcons <- awesomeIcons(
            icon = "plus",
            iconColor = "white",
            markerColor = sapply(map_data$成人口罩剩餘數, function(x) {
                    if (x >= 200) { "green" }
                    else if (x < 200 & x >= 100) { "yellow" }
                    else if (x < 100 & x >= 50) { "orange" }
                    else if (x < 50) { "red" } 
                }
            )
        )
        
        m <- Institute_data %>%
            filter(!is.na(x) | !is.na(y)) %>% 
            leaflet() %>%
            addControlGPS(
                options = gpsOptions(
                    position = "topleft",
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
                group = ~醫事機構地址,
                popup = popup_items,
                icon = markerIcons
            )
        
        if (!is.null(input$drawer2)) {
            
            minLng <- min(as.double(map_data$x))
            minLat <- min(as.double(map_data$y))
            maxLng <- max(as.double(map_data$x))
            maxLat <- max(as.double(map_data$y))
            
            m <- m %>%
                fitBounds(
                    lng1 = minLng,
                    lat1 = minLat,
                    lng2 = maxLng,
                    lat2 = maxLat
                )
            
        } else {
            
            m <- m %>% 
                setView(
                lng = 120.97388194444444,
                lat = 23.97565,
                zoom = 7
            )
            
        }
        
        m
    })
}

shinyApp(ui = ui, server = server)