% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{juveniles_dashboard}
\alias{juveniles_dashboard}
\title{Create visual dashboard for juvenile analysis}
\usage{
juveniles_dashboard(
  data_total,
  col_date = NULL,
  cols_length = NULL,
  a = 1e-04,
  b = 2.984,
  col_latitude = NULL,
  col_longitude = NULL,
  col_catch = NULL,
  col_juveniles = NULL,
  xlim = c(-85, -70),
  ylim = c(-20, 0),
  palette = NULL,
  step_x_date = NULL,
  sort_comparison = FALSE,
  title_comp = NULL,
  title_catch = NULL,
  title_map = NULL,
  title_relation = NULL
)
}
\arguments{
\item{data_total}{Data frame with catch, size, and location data.
It must contain information about dates, length, coordinates, and catches.}

\item{col_date}{Name of the column containing dates. If NULL,
the function will try to detect it automatically.}

\item{cols_length}{Vector with names of columns containing size
frequencies. If NULL, the function will try to detect them automatically.}

\item{a}{Parameter 'a' of the length-weight relationship (W = a*L^b). The default value
is 0.0001.}

\item{b}{Parameter 'b' of the length-weight relationship (W = a*L^b). The default value
is 2.984.}

\item{col_latitude}{Name of the column containing latitude. If NULL,
the function will try to detect it automatically.}

\item{col_longitude}{Name of the column containing longitude. If NULL,
the function will try to detect it automatically.}

\item{col_catch}{Name of the column containing catches. If NULL,
the function will try to detect it automatically.}

\item{col_juveniles}{Name of the column containing the percentage of juveniles.
If NULL, the function will try to detect it automatically.}

\item{xlim}{Numeric vector of length 2 defining the limits of the x-axis for the map.
Default c(-85, -70) to cover the Peruvian coast.}

\item{ylim}{Numeric vector of length 2 defining the limits of the y-axis for the map.
Default c(-20, 0) to cover the Peruvian coast.}

\item{palette}{Custom color vector for the graphs. If NULL,
a default palette will be used.}

\item{step_x_date}{Optional. Interval for date breaks (e.g., "1 day", "1 month").}

\item{sort_comparison}{Logical. If TRUE, the percentage comparison chart
will be sorted from highest to lowest. The default value is FALSE.}

\item{title_comp}{Title for the juvenile comparison graph.}

\item{title_catch}{Title for the cumulative catch graph.}

\item{title_map}{Title for the spatial distribution map.}

\item{title_relation}{Title for the smoothed juvenile percentage graph.}
}
\value{
A list with the following components:
\item{comparison}{Bar chart comparing juveniles by number and weight}
\item{cumulative_catch}{Area chart showing cumulative catch over time}
\item{juveniles_map}{Map showing the spatial distribution of juveniles}
\item{relation}{Chart of smoothed juvenile percentage trends}
\item{dashboard}{If the patchwork package is installed, a combined dashboard of all charts}
}
\description{
Generates a set of visualizations in dashboard format to analyze the proportion
of juveniles in fish catches. The dashboard includes:
\itemize{
\item A comparison of juveniles by number and weight
\item A cumulative catch graph
\item A map of spatial distribution of juveniles
\item A visualization of juvenile percentage trends
}
}
\examples{
\dontrun{

data_hauls <- process_hauls(data_hauls = calas_bitacora)
data_fishing_trips <- process_fishing_trips(data_fishing_trips = faenas_bitacora)
hauls_length <- process_length(data_length = tallas_bitacora)

# Integrate data
data_length_fishing_trips <- merge(
   x = data_fishing_trips,
   y = hauls_length,
   by = 'fishing_trip_code'
)

data_total <- merge_length_fishing_trips_hauls(
   data_hauls = data_hauls,
   data_length_fishing_trips = data_length_fishing_trips
)

final_data <- add_variables(data_total)

# Prepare size data
length_cols <- c("8", "8.5", "9", "9.5", "10", "10.5", "11", "11.5",
                "12", "12.5", "13", "13.5", "14", "14.5", "15")

# Weight length by catch
results <- weight_length_df(
  df = final_data,
  length_cols = length_cols,
  catch_col = "catch_ANCHOVETA",
  a = 0.0001,
  b = 2.984
)

# Prepare data for analysis
results$unique_date <- convert_to_date(results$start_date_haul, type = "date")
results$catch_t <- results$catch_ANCHOVETA/1000  # Convert to tons

# Create dashboard
dashboard <- juveniles_dashboard(
  data_total = results,
  col_date = "unique_date",
  cols_length = paste0("pond_", seq(8, 15, 0.5)),
  a = 0.0001,
  b = 2.984,
  col_latitude = "lat_final",
  col_longitude = "lon_final",
  col_catch = "catch_t",
  col_juveniles = "juv",
  show_limit_juv = TRUE
)

# View individual components
dashboard$comparison
dashboard$cumulative_catch
dashboard$juveniles_map
dashboard$relation

# View complete dashboard
dashboard$dashboard
}
}
