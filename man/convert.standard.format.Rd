% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert.standard.format.R
\name{convert.standard.format}
\alias{convert.standard.format}
\title{convert.standard.format}
\usage{
convert.standard.format(chicane.results, file.name = "")
}
\arguments{
\item{chicane.results}{Path to CHiCANE interaction calls file}

\item{file.name}{Path to output file}
}
\value{
TRUE if output files are created successfully
}
\description{
Create a file in standard format for cross compatability including with 
WashU Epigenome Browser.
}
\examples{
chicane.results <- system.file(
   'extdata', 'T47D_2q35_filtered_chicane_calls.txt', 
   package = 'chicane'
   );
 output.file = file.path(tempdir(), 'temp_standard_format.txt');
	convert.standard.format(chicane.results, file.name = output.file);

}
\author{
Andrea Gillespie, Syed Haider
}
