

line_list <- list()

alart_line_1 <- list(type = "line",
                     x0 = 0, #obs_runoff$time[1],
                     x1 = 1, #tail(obs_runoff$time, n = 1),
                     y0 = 0.2,
                     y1 = 0.2,
                     xref = "paper",
                     yref = "y",
                     line  = list(color = "yellow"))

# Line indicating when forecast was issued

verticle_line <- list(type = "line",
                      x0 = 0.5,
                      x1 = 0.5,
                      y0 = 0,
                      y1 = 1,
                      xref = "x",
                      yref = "paper")

line_list[[1]] <- alart_line_1
line_list[[2]] <- verticle_line
  
lines_list <- list(verticle_line, alart_line_1)


p <- plot_ly()
p <- add_lines(p, x = c(0,1), y = c(0,0.4), type = 'scatter', mode = 'lines')
p <- layout(p, shapes = line_list)
p

