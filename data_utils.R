

# Read simulations --------------------------------------------------------

read_sim <- function(path, df_meta) {
  
  ldf <- list()
  
  for (i in 1:nrow(df_meta)) {
    
    file <- file.path(path, paste(gsub("\\.", "_", df_meta$regine_main[i]), "_data.txt", sep = ""))
    
    tmp <- read.table(file, header = TRUE)
    
    ldf[[i]] <- tmp
    
  }
  
  time <- ymd_hms(ldf[[1]]$date)
  
  df <- data.frame(ldf[[1]]$q_sim)
  
  for (i in 2:length(ldf)) {
    df <- cbind(df, ldf[[i]]$q_sim)
  }
  
  colnames(df) <- df_meta$stat_id
  
  df <- round(df, digits = 1)
  
  return(list(time = time, runoff = df))
  
}


# Read observations -------------------------------------------------------

read_obs <- function(path, variable, df_meta, read_all = FALSE) {

  tmp <- list()

  for (i in 1:nrow(df_meta)) {
    
    file <- file.path(path, paste(gsub("\\.", "_", df_meta$regine_main[i]), "_data", sep = ""), variable)
    
    if (read_all) {
      skip_rows <- 1
    } else {
      skip_rows <- countLines(file)[1]-100
    }
    
    df_tmp <- read.table(file, sep = ";", skip = skip_rows, header = FALSE)

    if (ncol(df_tmp)>2) {
      tmp[[i]] <- rowMeans(df_tmp[, 2:ncol(df_tmp)])
    } else {
      tmp[[i]] <- df_tmp[, 2]
    }

  }

  data <- do.call(cbind, tmp)

  data <- as.data.frame(data)

  colnames(data) <- df_meta$stat_id #sapply(strsplit(files, "_"), function(x) (paste(x[1], x[2], sep = ".")))
  
  data[data == -999] = NA
  
  if (variable == "prec.txt") {
    data[data < 0] = 0
    data[data > 300] = 300
  }
  
  if (variable == "tair.txt") {
    data[data < -40] = -40
    data[data > 40] = 40
  }
  
  data <- round(data, digits = 1)
  
  time <- ymd_hm(df_tmp[, 1])

  return(list(time = time, data = data))

}




