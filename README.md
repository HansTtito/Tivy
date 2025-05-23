## Tools for Fisheries Data Analysis in Peru

`Tivy` is an R package specialized in processing and analyzing fisheries data from Peru. It facilitates the handling of information from fishing trip logbooks, size records, hauls, and official announcements issued by the Ministry of Production for preventive closures.

## 📋 Main Features

- Loading and processing fishing logbook data
- Integration of haul, size, and fishing trip data
- Estimation of juvenile proportions
- Analysis of preventive closures
- Interactive visualization of fishing areas and results
- Tools for fisheries statistical modeling

## 📦 Installation

You can install the development version of `Tivy` from GitHub:

```r

# install.packages("devtools")
devtools::install_github("HansTtito/Tivy")

```

## 🚀 Usage Examples

### Basic Loading and Processing

```r

library(Tivy)

# Load and process logbook files
data_hauls <- process_hauls(
  data_hauls          = calas_bitacora,
  format              = "xlsx",
  correct_coordinates = TRUE
  )

data_fishing_trips <- process_fishing_trips(
  data_fishing_trips = faenas_bitacora, 
  format             = "xlsx"
  )

hauls_length <- process_length(
  data_length = tallas_bitacora, 
  format      = "xlsx"
  )
  
```

### Data Integration

```r

# Combination of size and fishing trip data
data_length_fishing_trips <- merge(
  x  = data_fishing_trips,
  y  = hauls_length, 
  by = 'fishing_trip_code'
  )

# Complete integration with haul data
data_total <- merge_length_fishing_trips_hauls(
  data_hauls                = data_hauls, 
  data_length_fishing_trips = data_length_fishing_trips
  )

# Add derived variables
final_data <- add_variables(
  data = data_total
  )
  
```

### Juvenile Analysis

```r

# Define size columns
length_cols <- as.character(seq(from = 8, to = 15, by = 0.5))
a <- 0.0001
b <- 2.983

# Weight length according to catch
final_data_weighted <- weight_length_df(
  df          = final_data, 
  length_cols = length_cols, 
  catch_col   = 'catch_ANCHOVETA', 
  a           = a,  # Parameter a of length-weight relationship
  b           = b   # Parameter b of length-weight relationship
  )

# Convert dates for temporal grouping
final_data_weighted$unique_date <- convert_to_date(
  date_vector = final_data_weighted$start_date_haul, 
  type        = "date"
  )

# Calculate juvenile proportion by date
juvenile_results <- juveniles_by_group(
  data        = final_data_weighted, 
  group_cols  = c("dc_cat"), 
  cols_length = paste0("pond_", length_cols)
  )

print(juvenile_results)

```

### Visualization of Results

```r

# Basic plot of juveniles by date
plot_juveniles(
  df             = final_data_weighted, 
  var_x          = "unique_date",
  fill_var       = "dc_cat",
  cols_length    = paste0("pond_", length_cols),
  a              = a,
  b              = b,
  step_x_date    = "1 day",
  plot_type      = "bars",
  use_facet_wrap = TRUE, 
  reference_line = 10
  )

```

![Example of juvenile plot](man/figures/ejemplo_juveniles.png)

```r

# Complete dashboard of juvenile analysis
final_data_weighted$catch_t = final_data_weighted$catch_ANCHOVETA/1000

dashboard <- juveniles_dashboard(
  data_total     = final_data_weighted,
  col_date       = "unique_date", 
  cols_length    = paste0("pond_", length_cols), 
  a              = a, 
  b              = b, 
  col_latitude   = "lat_final",
  col_longitude  = "lon_final", 
  col_catch      = "catch_t",
  col_juveniles  = "juv",
  step_x_date    = "1 day",
  )

# View individual dashboard components
dashboard$comparison  # Juvenile comparison
dashboard$relation  # Relationship juveniles ~ Time
dashboard$cumulative_catch  # Cumulative catch
dashboard$juveniles_map # Juvenile map

dashboard$dashboard    # Complete panel with all plots

```

![Juvenile analysis dashboard](man/figures/dashboard_juveniles.png)

### Analysis of Official Announcements

```r

# Example with URLs of announcements from the Ministry of Production
pdf_urls <- get_produce_announcements(
  start_date = "01/03/2025", 
  end_date   = "31/03/2025",  
  download   = FALSE # If you want to donwload the files, change TRUE
  )

print(pdf_urls)

# Extract information from announcements
results <- extract_announcement_data(
  vector_pdf_names = pdf_urls$DownloadURL
  )

# Format data for visualization
formatted_results <- format_announcement_data(
  data = results
  )

# Visualize closed areas with ggplot
plot_polygons_ggplot(
  data = formatted_results, 
  show_legend = TRUE
  )
  
```

![Visualization of closed areas with ggplot](man/figures/poligonos_ggplot.png)

```r

# Interactive visualization with leaflet
interactive_map <- plot_polygons_leaflet(
  data = formatted_results, 
  show_legend = TRUE
  )

interactive_map

```

![Interactive visualization of closed areas](man/figures/poligonos_leaflet.png)

## 📊 Recommended Workflow

1. Load and process haul, fishing trip, and size data
2. Integrate the data using the merge functions
3. Calculate derived variables and perform weightings
4. Analyze the proportion of juveniles by zones or seasons
5. Visualize results through plots or dashboards
6. Integrate with preventive closure information

## 📄 Supported Data Structure

`Tivy` is designed to work with the data structure of Peru's Ministry of Production. Typical input files include:

- **Haul logbooks**: Records of fishing operations.
- **Fishing trip logbooks**: Information on trips and vessels.
- **Size records**: Biometric measurements of captured species.
- **Official announcements**: PDF documents with information on preventive closures.

## 🔧 Main Functions

| Category | Functions | Description |
|-----------|-----------|-------------|
| Processing | `process_hauls()`, `process_fishing_trips()`, `process_length()` | Data loading and cleaning |
| Integration | `merge_length_fishing_trips_hauls()`, `add_variables()` | Data combination and enrichment |
| Juveniles | `weight_length_df()`, `juveniles_by_group()` | Analysis of juvenile proportions |
| Visualization | `plot_juveniles()`, `juveniles_dashboard()` | Creation of plots and dashboards |
| Announcements | `get_produce_announcements()`, `extract_announcement_data()`, `format_announcement_data()` | Processing of official announcements |
| Maps | `plot_polygons_ggplot()`, `plot_polygons_leaflet()` | Geospatial visualization |

## 👩‍💻 Contributions

Contributions are welcome. Please consider:

1. Opening an issue to discuss important changes
2. Following the project's code style
3. Including tests for new features
4. Updating the corresponding documentation

## 📚 Citation

If you use `Tivy` in your research, please cite it as:

```
Ttito, H. (2025). Tivy: Tools for Fisheries Data Analysis in Peru. R package version 0.2.1.
https://github.com/HansTtito/Tivy
```
