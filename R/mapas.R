plot_mapa_peru = function(lineas_costa = 1, lineas_paralelas_costa = NULL, x_lim = c(-84,-69), y_lim = c(-20,0)) {

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
  plot(NA, xlim = x_lim, ylim = y_lim, xlab = "", ylab= "Latitud", axes = FALSE)
  axis(2, at = seq(-20,0, 5), labels = paste0(seq(20,0,-5),"°S"), las = 2)
  box()


  for(i in 1:lineas_costa){

    resta_x <- ifelse(i == 1, 0, 5 * (i - 1))
    lines(x = Shoreline_Peru$Long - resta_x, y = Shoreline_Peru$Lat)
    if(!is.null(lineas_paralelas_costa)){
      for(i in 1:length(lineas_paralelas_costa)){
        lines(x = paraleas_costa_peru[[lineas_paralelas_costa[i]]]$lon - resta_x, y = paraleas_costa_peru[[lineas_paralelas_costa[i]]]$lat)
      }
    }

  }

}


agrega_etiquetas = function(bx_labels, pos_x = -81.5, pos_y = -1.5, padding = c(0.3, 0.4, 0.3, 0.4), col = "black", border = "red", fill = "pink", lwd = 2, ...){

  for(i in seq_along(bx_labels)){

    resta_x <- ifelse(i == 1, 0, 5 * (i - 1))
    text_width <- strwidth(bx_labels[i], cex = 0.7)
    textBox(x = pos_x - resta_x - text_width/2, y = pos_y, labels = bx_labels[i], cex = 0.7, padding = padding, col = col, border = border, fill = fill, lwd = lwd,...)

  }

}
