\name{run_metabocensor}
\alias{run_metabocensor}
\title{Run the MetaboCensoR Application}
\usage{
run_metabocensor(max_size_gb = 5, ...)
}
\arguments{
\item{max_size_gb}{Numeric. The maximum allowed file upload size in GBs. Defaults to 5.}

\item{\dots}{Additional arguments passed to \code{\link[shiny]{shinyApp}}.}
}
\description{
Run the MetaboCensoR Application locally. Shiny deployment
\href{https://plyush1993.shinyapps.io/metabocensor/}{here}.
}
\references{
To be updated.
}
\examples{
\dontrun{
# Launch the MetaboCensoR application with default settings (5GB limit)
run_metabocensor()

# Launch the application allowing up to 10GB for file uploads
run_metabocensor(max_size_gb = 10)
}
}
