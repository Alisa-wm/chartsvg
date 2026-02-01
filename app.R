server <- function(input, output, session) {
  # 定义一个直接生成PNG二进制流的响应函数
  output$plot_image <- renderImage({
    # 1. 解析查询参数（与之前相同）
    query <- parseQueryString(session$clientData$url_search)
    # 2. 根据参数生成数据（与之前相同）
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
    # 3. 将图表保存为临时PNG文件
    temp_plot <- tempfile(fileext = ".png")
    png(temp_plot, width = 800, height = 600, res = 150) # 设置分辨率
    # 4. 绘制图表（绘图代码）
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
    dev.off() # 重要：关闭图形设备，保存文件
    # 5. 以图片格式返回这个临时文件
    list(src = temp_plot,
         contentType = "image/png",
         width = 800,
         height = 600,
         alt = "Dynamic Chart")
  }, deleteFile = TRUE) # deleteFile=TRUE 表示请求结束后删除临时文件
}