# Tivy  
## Herramientas para el Análisis de Datos Pesqueros  

Este paquete proporciona funciones para cargar, procesar y analizar datos pesqueros, incluyendo información de bitácoras de faenas, tallas y calas del Ministerio de Producción de Perú, así también los comunicados emitidos para cierres preventivos. También incluye herramientas para visualización y modelado estadístico.  

### 📦 Instalación  
Puedes instalar el paquete desde GitHub con:  

```r
# install.packages("devtools")
devtools::install_github("HansTtito/Tivy")
```

🚀 Uso

Carga el paquete y usa sus funciones principales:

```r

library(Tivy)

# Cargar de Archivos
data_calas <- procesar_calas(data_calas = calas_bitacora)
data_faenas <- procesar_faenas(data_faenas = faenas_bitacora)
calas_tallas <- procesar_tallas(data_tallas = tallas_bitacora)

# Merge de calas, tallas y faenas
data_tallasfaenas <- merge(x = data_faenas, y = calas_tallas, by = 'codigo_faena')
data_total <- merge_tallas_faenas_calas(data_calas = data_calas, data_tallas_faenas = data_tallasfaenas)

# Aplicación de la función
datos_final <- agregar_variables(data_total)

head(datos_final)


# Lectura de comunicados

# Suponiendo que tengas una lista de archivos PDF

pdf_urls <- c("https://consultasenlinea.produce.gob.pe/produce/descarga/comunicados/dgsfs/1542_comunicado1.pdf",
              "https://consultasenlinea.produce.gob.pe/produce/descarga/comunicados/dgsfs/1478_comunicado1.pdf",
              "https://consultasenlinea.produce.gob.pe/produce/descarga/comunicados/dgsfs/1468_comunicado1.pdf")

resultados <- extrae_data_comunicados(vector_pdf_names = pdf_urls)
print(resultados)

# Si es necesario, formatear los datos antes de ingresarlo a la función de gráficos

resultados <- formatear_datos_comunicados(resultados)

# Visualización de las áreas cerradas

graficar_poligonos_ggplot(datos = resultados)

graficar_poligonos_leaflet(datos = resultados)

```
