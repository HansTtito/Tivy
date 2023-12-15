plot_mapa_peru = function(lineas_costa = 1, lineas_paralelas_costa = NULL, add_linea_5_millas = FALSE, x_lim = c(-84,-69), y_lim = c(-20,0), col_linea_costa = "black", lwd_linea_costa = 1, lty_linea_costa = 1, col_paralelas_costa = "black", lwd_paralelas_costa = 1, lty_paralelas_costa = 1, col_5_millas = "black", lwd_5_millas = 1, lty_5_millas = 1, grados_lat_at = seq(-20,0, 5), grados_lat_label = paste0(seq(20,0,-5),"°S"), ylab = "Latitud", cex_lab_y = 1) {

  if(any(y_lim > 0 | y_lim < -20 )){
    stop("Solo permite valores entre 0 y -20")
  }else if(y_lim[1] > y_lim[2]){
    stop("El segundo valor debe ser mayor al primero")
  }

  if(any(x_lim > -69 | x_lim < -84 )){
    stop("Solo permite valores entre 0 y -20")
  }else if(x_lim[1] > x_lim[2]){
    stop("El segundo valor debe ser mayor al primero")
  }

  if(length(lineas_costa) != 1){
    stop("Solo acepta un valor")
  } else {
    if(!is.numeric(lineas_costa) | lineas_costa > 9 | lineas_costa < 1 | as.integer(lineas_costa) != lineas_costa){
      stop("Solo acepta números enteros y menores a 10, por defecto es 1")
    }
  }


  x_lim[1] = x_lim[1] - (lineas_costa - 1)*5

  valores_permitidos_by_dist_costa = seq(10,120,10)

  if(!is.null(lineas_paralelas_costa)){

    if (!all(lineas_paralelas_costa %in% valores_permitidos_by_dist_costa)) {
      stop("Error: El valor proporcionado para 'by_dist_costa' no es válido. Debe ser 10, 20, 30, ..., 120.")
    }

    if(length(lineas_paralelas_costa) == 1){
      lineas_paralelas_costa = paste0("l", seq(lineas_paralelas_costa,120, lineas_paralelas_costa))
    } else {
      lineas_paralelas_costa = paste0("l", lineas_paralelas_costa)
    }

  }

  step_interno = 5
  # paralelas = paraleas_costa_peru[by_dist_costa]
  plot(NA, xlim = x_lim, ylim = y_lim, xlab = "", ylab= ylab, axes = FALSE, cex.lab = cex_lab_y)
  axis(2, at = grados_lat_at, labels = grados_lat_label, las = 2)
  box()


  for(i in 1:lineas_costa){

    resta_x <- ifelse(i == 1, 0, 5 * (i - 1))

    lines(x = Shoreline_Peru$Long - resta_x, y = Shoreline_Peru$Lat, col = col_linea_costa, lwd = lwd_linea_costa, lty = lty_linea_costa)

    if(!is.null(lineas_paralelas_costa)){
      for(i in 1:length(lineas_paralelas_costa)){

        lines(x = paraleas_costa_peru[[lineas_paralelas_costa[i]]]$lon - resta_x, y = paraleas_costa_peru[[lineas_paralelas_costa[i]]]$lat, col = col_paralelas_costa, lwd = lwd_paralelas_costa, lty = lty_paralelas_costa)

      }
    }

  }

  if(is.logical(add_linea_5_millas)){

    if(add_linea_5_millas){

      for(i in 1:lineas_costa){

        resta_x <- ifelse(i == 1, 0, 5 * (i - 1))

        lines(x = linea_5_millas$X  - resta_x, y = linea_5_millas$Y, col = col_5_millas, lwd = lwd_5_millas, lty = lty_5_millas)

      }

    }

  } else {
    stop("Solo acepta TRUE o FALSE")
  }

}


agrega_etiquetas = function(bx_labels, pos_x = -81.5, pos_y = -1.5, padding = c(0.3, 0.4, 0.3, 0.4), col = "black", border = "red", fill = "pink", lwd = 2, ...){

  for(i in seq_along(bx_labels)){

    resta_x <- ifelse(i == 1, 0, 5 * (i - 1))
    text_width <- strwidth(bx_labels[i], cex = 0.7)
    textBox(x = pos_x - resta_x - text_width/2, y = pos_y, labels = bx_labels[i], cex = 0.7, padding = padding, col = col, border = border, fill = fill, lwd = lwd,...)

  }

}
