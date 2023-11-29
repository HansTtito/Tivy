\name{lat_long}
\alias{lat_long}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{

Función para convertir la longitud y/o la latitud

}
\description{

Funciones que permitirán apoyar el procesamiento de los datos

}
\usage{
lat_long(lat_lon)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat_lon}{

Ingresar el vector de longitud y/o latitud

}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{


lat_corregida = lat_long(calas$Latitud.Inicio)

print(lat_corregida)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
