library(shiny)
# library(emojifont)

# 說明頁面 ----
pageIntro <- fluidPage(
  includeHTML("./www/pageMain.html")
)

# 陳家瑋的研究項目頁面 ----
pageA <- fluidPage(
  includeHTML("./www/pageA.html")
)

# 許紫涵的研究項目頁面 ----
pageB <- fluidPage(
  includeHTML("./www/pageB.html")
)

# 葉彥妤的研究項目頁面 ----
pageC <- fluidPage(
  includeHTML("./www/pageC.html")
)

# ui ----
ui <- fluidPage(
  tags$head(
    tags$script(src="https://kit.fontawesome.com/5c941e5954.js", crossorigin="anonymous"),
    tags$title("藥局 & 口罩資訊地圖"),
    includeHTML("./www/favicon.html")
  ),
  includeCSS("./www/css/style.css"),
  navbarPage(
    title = span(icon("map-marked-alt"), "藥局 & 口罩資訊地圖"),
    tabPanel(title = "說明頁面", pageIntro),
    tabPanel(title = "陳家瑋的研究項目", pageA), 
    tabPanel(title = "許紫涵的研究項目", pageB),
    tabPanel(title = "葉彥妤的研究項目", pageC)
  )
)


# server ----
server <- function(input, output) {
  # insert server code here
}

shinyApp(ui = ui, server = server)