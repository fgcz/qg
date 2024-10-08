% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/configProteomics.R
\name{qconfigProteomicsM_CLASS48_48VialXCalibur}
\alias{qconfigProteomicsM_CLASS48_48VialXCalibur}
\title{queue confiug for Proteomics}
\usage{
qconfigProteomicsM_CLASS48_48VialXCalibur(
  x,
  howOften = 4,
  lssystem = "M_CLASS48_48",
  ...
)
}
\arguments{
\item{x}{a data.frame}

\item{howOften}{how frequently the sample should be inserted}

\item{...}{parameters to pass to \code{\link[qg]{.insertSample}}}
}
\description{
queue confiug for Proteomics
}
\details{
increments clean and qc positions 
\itemize{
\item position mapping f: 1-48 -> (1:8; A:F) starting: A,1; A,2; ...; A,8; B,1; ...; F,8
}
}
\author{
Christian Panse 2024-09-03

qconfig metabolomics for plates
}
