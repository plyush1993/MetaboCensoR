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
Plyushchenko I.V. & Luzzatto-Knaan T. "MetaboCensoR: A Shiny Application for Data Filtering in Untargeted LC-MS Metabolomics to Enhance Interpretability" bioRxiv 2026.07.02.735197
\href{https://doi.org/10.64898/2026.07.02.735197}{DOI}.
}
\examples{
\dontrun{
# Launch the MetaboCensoR application with default settings (5GB limit)
run_metabocensor()

# Launch the application allowing up to 10GB for file uploads
run_metabocensor(max_size_gb = 10)
}
}
