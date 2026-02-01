# app.R - 通过 URL 返回 SVG 图表

library(shiny)
library(ggplot2)

# 解析 URL 参数
get_param <- function(req, key, default = NULL) {
  qs <- shiny::parseQueryString(req$QUERY_STRING %||% "")
  val <- qs[[key]]
  if (is.null(val)) default else val
}

ui <- function(req) {
  # -------- 读取参数 --------
  title <- get_param(req, "title", "SVG Chart")
  data_str <- get_param(req, "data", "1,3,2,5,4")
  type <- get_param(req, "type", "line")
  width <- as.integer(get_param(req, "w", "800"))
  height <- as.integer(get_param(req, "h", "500"))
  
  # -------- 数据解析 --------
  y <- suppressWarnings(as.numeric(strsplit(data_str, ",")[[1]]))
  if (any(is.na(y))) {
    return(shiny::httpResponse(
      status = 400,
      content_type = "text/plain",
      content = "Invalid data parameter, use ?data=1,2,3"
    ))
  }
  df <- data.frame(x = seq_along(y), y = y)
  
  # -------- 生成图形 --------
  p <- ggplot(df, aes(x, y)) + ggtitle(title)
  if (type == "bar") p <- p + geom_col(fill="#2C7BE5")
  else if (type == "point") p <- p + geom_point(color="#2C7BE5") + geom_line(color="#2C7BE5")
  else p <- p + geom_line(color="#2C7BE5", linewidth=1.2)
  
  p <- p + theme_minimal(base_size = 14)
  
  # -------- 输出 SVG --------
  shiny::httpResponse(
    status = 200,
    content_type = "image/svg+xml",
    content = function() {
      tf <- tempfile(fileext = ".svg")
      svg(tf, width = width/96, height = height/96)  # SVG 物理尺寸
      print(p)
      dev.off()
      readChar(tf, nchars = file.info(tf)$size, useBytes = TRUE)
    }
  )
}

server <- function(...) {}

shinyApp(ui, server)