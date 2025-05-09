% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{extract_points_between_latitudes}
\alias{extract_points_between_latitudes}
\title{Extract all points of a parallel line between two latitudes}
\usage{
extract_points_between_latitudes(line, lat_min, lat_max, lat_name, lon_name)
}
\arguments{
\item{line}{Data frame representing a line parallel to the coast.}

\item{lat_min}{Numeric value of the minimum latitude (southernmost).}

\item{lat_max}{Numeric value of the maximum latitude (northernmost).}

\item{lat_name}{Name of the column containing latitudes in the data frame.}

\item{lon_name}{Name of the column containing longitudes in the data frame.}
}
\value{
A matrix of points with columns corresponding to longitude and latitude.
}
\description{
This function obtains all points of a line parallel to the coast that are between
two given latitudes, including interpolated points at the exact limits.
}
\details{
The function first filters the points that are within the latitude range.
If necessary, it interpolates additional points for the exact limits of
lat_min and lat_max. If there are not enough points in the range, it creates a straight
segment between the interpolated points at the limits.
}
\keyword{internal}
