# FogPredictor_functions.r

# returns 1 if fog between split_hourZ-split_hourZ, 0 otherwise
FG_or_BR_f <- function(date) {
  metar <- as.data.frame(metar)
  date_f <- date(as.POSIXct(date, tz = "UTC", origin = "1970-01-01"))
  presentwx_date <- metar[(metar$date == date & as.integer(metar$hour) >= split_hour) | (metar$date == as.character(date_f + 1) & as.integer(metar$hour) < split_hour), ]$presentwx
  require(stringr)
  n_fg_br <- length(na.omit(c(as.vector(str_match(pattern = ".*FG.*", string = presentwx_date)), as.vector(str_match(pattern = ".*BR.*", string = presentwx_date)))))
  if(n_fg_br == 0){
    return(0)
  } else {
    return(1)
  }
}

# gives time fog first occured in range split_hourZ-split_hourZ
# if no fog gives time of min dewpoint depression
date <- "2010-01-01"
first_fog_time_f <- function(date) {
  date_f <- date(as.POSIXct(date, tz = "UTC", origin = "1970-01-01"))
  metar_date <- metar[(metar$date == date & as.integer(metar$hour) >= split_hour) | (metar$date == as.character(date_f + 1) & as.integer(metar$hour) < split_hour), ]
  metar_date$valid <- as.POSIXct(metar_date$valid, tz = "UTC", origin = "1970-01-01")
  require(stringr)
  FG_or_BR_date <- na.omit(c(as.vector(str_match(pattern = ".*FG.*", string = metar_date$presentwx)), as.vector(str_match(pattern = ".*BR.*", string = metar_date$presentwx))))
  if(length(FG_or_BR_date) == 0) {
    valid <- metar_date[which.min(as.numeric(metar_date$tmpf) - as.numeric(metar_date$dwpf)), ]$valid[1]
    valid <- as.POSIXct(valid, tz = "UTC", origin = "1970-01-01")
    hour <- metar_date[which.min(as.numeric(metar_date$tmpf) - as.numeric(metar_date$dwpf)), ]$hour[1]
    minute <- metar_date[which.min(as.numeric(metar_date$tmpf) - as.numeric(metar_date$dwpf)), ]$minute[1]
  } else {
    valid <- min(metar_date[metar_date$presentwx %in% FG_or_BR_date, ]$valid)
    valid <- as.POSIXct(valid, tz = "UTC")
    hour <- formatC(as.integer(hour(valid)), width = 2, flag = "0")
    minute <- formatC(as.integer(minute(valid)), width = 2, flag = "0")
  }
  return(valid)
}

# # no fog, YPPH?
# date <- "2010-01-01"
# # fog
# date <- "2010-02-09"

# # no fog
# first_fog_time_f("2010-01-01")
# FG_or_BR_f("2010-01-01")

# # fog
# first_fog_time_f("2010-02-09")
# FG_or_BR_f("2010-02-09")

collapse <- function(Outputs, Outputs_f, result, P_var = NA, P_val = 0.5) {

  if(is.na(P_val) == TRUE) {
    P_val = 0.5
  }

  result_collapse <- matrix(NA, ncol = length(Outputs), nrow = nrow(result))

  o <- 1
  for(o in seq(1, length(Outputs), 1)) {

    Output <- Outputs[o]
    Output

    Output_f_o <- as.vector(na.omit(str_match(Outputs_f, paste(Output, ".+", sep = ""))))
    Output_f_o

    result_o <- result[, Outputs_f %in% Output_f_o]
    result_o <- as.data.frame(result_o)
    result_o

    result_o_max.idx <- which(result_o == apply(result_o, MARGIN = 1, FUN = max), arr.in=TRUE)
    result_o_max.idx <- result_o_max.idx[order(result_o_max.idx[, 1]), ]
    result_o_max.idx <- as.data.frame(result_o_max.idx)

    require(plyr)
    result_o_max.idx <- ddply(.data = result_o_max.idx, .variables = .(row),
          .fun = summarize,
          fcol = col[1]
    )

    result_o_max <- Output_f_o[result_o_max.idx[, 2]]
    result_o_max <- as.vector(result_o_max)
    result_o_max

    if(is.na(P_var) == TRUE) {

      result_collapse[, o] <- result_o_max

    } else {

      result_collapse[, o] <- ifelse(result_o[, paste(Output, P_var, sep = "")] >= P_val, 
        paste(Output, P_var, sep = ""),
        result_o_max
        )

    }

  }

  result_collapse <- as.data.frame(result_collapse)
  names(result_collapse) <- Outputs
  return(result_collapse)

}

# Output = Outputs[o]
# results = results_nn
# var2 = "arnn_result"
# var1 = "data_result"

confusion <- function(i, Output, results, var1 = "data_result", var2) {

  Outputs_f_o <- na.omit(str_match(Outputs_f, paste(Output, ".+", sep = "")))
  Outputs_f_o

  Var1 <- results[[i]][[var1]][Output]
  Var2 <- results[[i]][[var2]][Output]

  tab <- table(results[[i]][[var1]][, Output], results[[i]][[var2]][, Output])
  tab <- as.data.frame(tab)
  tab <- rbind(tab, expand.grid(Outputs_f_o, Outputs_f_o, Freq = 0))
  tab <- xtabs(Freq ~ Var1 + Var2, data = tab)

  df <- matrix(NA, ncol = ncol(tab), nrow = nrow(tab))
  df <- tab[,]
  df <- as.data.frame(df)

  df <- df[order(row.names(df)),]
  if(length(df) > 1){
    df <- df[, order(names(df))]
  }

  require(reshape2)
  dcast(df, Var1 ~ Var2, value.var = "Freq")[, Outputs_f_o]


  return(dcast(df, Var1 ~ Var2, value.var = "Freq")[, Outputs_f_o])
 
}

per_correct <- function(confusion) {
  confusion <- as.matrix(confusion)
  return( sum(diag(confusion)) / sum(confusion) )
}

