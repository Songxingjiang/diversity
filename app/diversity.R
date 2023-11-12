#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# 定义需要检查和安装的包列表
required_packages <- c("shiny", "vegan", "magrittr","tibble")

# 检查并安装缺失的包
missing_packages <- setdiff(required_packages, installed.packages()[,1])
if (length(missing_packages) > 0) {
  install.packages(missing_packages,repos = "https://cran.r-project.org")
}

library(shiny)
library(vegan)
library(magrittr)
library(tibble)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("阿尔法多样性计算"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "上传数据", accept = ".csv"),
      selectInput("div_type", "多样性指数指标",
                  c("Shannon" = "shannon","Simpson" = "simpson", "Pielou" = "pielou")
      ),
      downloadButton("download", "下载结果")
    ),
    mainPanel(
      tableOutput("div_table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # 读取上传的数据
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, header = TRUE, row.names = 1, check.names = FALSE) %>%
      rownames_to_column(var = "Sample")  # 将样方名作为一列
  })
  
  # 计算多样性指数
  div <- reactive({
    S <- rowSums(data()[,-1] > 0)  # 去掉第一列样方名
    H <- diversity(data()[,-1])
    D <- diversity(data()[,-1], index = "simpson")
    J <- H/log(S)
    data.frame(Sample = data()[,1], Species = S, Shannon = H, Simpson = D, Pielou = J)  # 将样方名添加到结果中
  })
  
  # 生成结果表格
  output$div_table <- renderTable({
    div()
  })
  
  # 下载结果文件
  output$download <- downloadHandler(
    filename = function() {
      "diversity.csv"
    },
    content = function(file) {
      write.csv(div(), file, row.names = FALSE)
    }
  )
}


# Run the application 
shinyApp(ui = ui, server = server)
