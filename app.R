# app_simple.R - 使用基础图形，无需svglite
library(shiny)
ui <- fluidPage(
  # 这个UI几乎为空，因为我们不打算通过浏览器看网页，只通过API拿图片
  # 但Shiny需要一个输出元素
  imageOutput("plot_image")
)
server <- function(input, output, session) {
  # 监听请求，检查是否为直接的图片API调用
  observe({
    query <- parseQueryString(session$clientData$url_search)
    # 判断条件：当URL中包含 'api=png' 参数时，触发直接图片响应
    if (!is.null(query$api) && query$api == 'png') {
      # 1. 设置响应的内容类型为PNG
      session$response$setHeader("Content-Type", "image/png")
      # 可选：设置缓存控制，避免Teams缓存旧图片
      session$response$setHeader("Cache-Control", "no-cache, no-store")
      # 2. 根据查询参数生成数据（复用你之前的逻辑）
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
      # 3. 创建一个临时文件来保存PNG图片
      tmpfile <- tempfile(fileext = ".png")
      # 4. 将图形输出到这个文件
      png(tmpfile, width = 800, height = 600, res = 150)
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
      dev.off() # 保存文件
      # 5. 关键步骤：读取临时文件的二进制内容，并直接写入HTTP响应体
      img_data <- readBin(tmpfile, "raw", file.info(tmpfile)$size)
      session$response$write(img_data)
      # 6. 至关重要的步骤：立即结束会话，阻止Shiny继续处理并返回常规HTML
      session$close()
      # 7. 清理临时文件
      unlink(tmpfile)
    }
  })
  # 以下是你原有用于网页预览的输出（可选保留，不影响API）
  output$plot_image <- renderImage({
    # ... 你原有的用于网页显示的renderImage逻辑 ...
  }, deleteFile = TRUE)
}
shinyApp(ui, server)