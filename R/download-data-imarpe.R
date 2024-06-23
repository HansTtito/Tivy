data_juv_imarpe = function(link){

  webpage = read_html(link) %>%
    html_elements("#dashboard-container") %>%
    html_table() %>%
    simplify() %>%
    first()

  tabla = webpage[1:(grep(pattern = "Total", x = webpage$Fecha)-1),]
  names(tabla) = c("fecha", "juv_ton","adu_ton","total_ton")

  tabla = tabla %>%
    mutate(fecha = convertir_a_fecha(fecha),
           juv_ton = as.numeric(str_remove_all(juv_ton, pattern = " ")),
           adu_ton = as.numeric(str_remove_all(adu_ton, pattern = " ")),
           total_ton = as.numeric(str_remove_all(total_ton, pattern = " ")))

  return(tabla)

}
