# app_simple.R - 使用基础图形，无需svglite
library(shiny)
ui <- fluidPage(
  # 只显示图片
  plotOutput("plot", width = "800px", height = "600px")
)
server <- function(input, output, session) {
  # 解析参数并生成图表
  output$plot <- renderPlot({
    # 获取查询参数
    query <- parseQueryString(session$clientData$url_search)
    # 提取参数
    if (!is.null(query$data)) {
      # 解析数据
      points <- strsplit(query$data, ";")[[1]]
      x <- numeric()
      y <- numeric()
      for (i in seq_along(points)) {
        coords <- strsplit(points[i], ",")[[1]]
        if (length(coords) >= 2) {
          x <- c(x, as.numeric(coords[1]))
          y <- c(y, as.numeric(coords[2]))
        }
      }
    } else {
      # 默认数据
      x <- 1:10
      y <- runif(10, 1, 10)
    }
    # 创建图形
    plot(x, y,
         type = if (!is.null(query$type) && query$type == "line") "b" else "p",
         col = if (!is.null(query$color)) query$color else "steelblue",
         pch = 19, lwd = 2,
         main = if (!is.null(query$title)) URLdecode(query$title) else "Quick Chart",
         xlab = if (!is.null(query$xlab)) URLdecode(query$xlab) else "X",
         ylab = if (!is.null(query$ylab)) URLdecode(query$ylab) else "Y",
         cex.main = 1.5, cex.lab = 1.2, cex.axis = 1)
    # 如果是折线图，添加连接线
    if (!is.null(query$type) && query$type == "line") {
      lines(x, y, col = if (!is.null(query$color)) query$color else "steelblue", lwd = 2)
    }
  })
  # 返回PNG图片而不是HTML
  output$plot_image <- renderImage({
    # 生成临时PNG文件
    tmpfile <- tempfile(fileext = ".png")
    # 调用上面的绘图代码
    png(tmpfile, width = 800, height = 600)
    # 获取查询参数并绘图（重复上面的绘图代码）
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$data)) {
      points <- strsplit(query$data, ";")[[1]]
      x <- numeric()
      y <- numeric()
      for (i in seq_along(points)) {
        coords <- strsplit(points[i], ",")[[1]]
        if (length(coords) >= 2) {
          x <- c(x, as.numeric(coords[1]))
          y <- c(y, as.numeric(coords[2]))
        }
      }
    } else {
      x <- 1:10
      y <- runif(10, 1, 10)
    }
    plot(x, y,
         type = if (!is.null(query$type) && query$type == "line") "b" else "p",
         col = if (!is.null(query$color)) query$color else "steelblue",
         pch = 19, lwd = 2,
         main = if (!is.null(query$title)) URLdecode(query$title) else "Quick Chart",
         xlab = if (!is.null(query$xlab)) URLdecode(query$xlab) else "X",
         ylab = if (!is.null(query$ylab)) URLdecode(query$ylab) else "Y",
         cex.main = 1.5, cex.lab = 1.2, cex.axis = 1)
    if (!is.null(query$type) && query$type == "line") {
      lines(x, y, col = if (!is.null(query$color)) query$color else "steelblue", lwd = 2)
    }
    dev.off()
    # 返回图片
    list(src = tmpfile,
         contentType = "image/png",
         width = 800,
         height = 600,
         alt = "Dynamic Chart")
  }, deleteFile = TRUE)
}
shinyApp(ui, server)