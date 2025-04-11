# Tivy  
## Herramientas para el Análisis de Datos Pesqueros  

Este paquete proporciona funciones para cargar, procesar y analizar datos pesqueros, incluyendo bitácoras de faenas, tallas y calas. También incluye herramientas para visualización y modelado estadístico.  

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
data_calas <- procesar_calas(data_calas = calas)
data_faenas <- procesar_faenas(data_faenas = faenas)
calas_tallas <- procesar_tallas(data_tallas = tallas)

# Merge de calas, tallas y faenas
data_tallasfaenas <- merge(x = data_faenas, y = calas_tallas, by = 'codigo_faena')
data_total <- merge_tallas_faenas_calas(data_calas = data_calas, data_tallas_faenas = data_tallasfaenas)

# Aplicación de la función
resultados <- agregar_variables(data_total)

```
