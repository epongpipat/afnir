% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/afni_read_affine.R
\name{afni_read_affine}
\alias{afni_read_affine}
\title{Read AFNI Affine Matrix}
\usage{
afni_read_affine(x)
}
\arguments{
\item{x}{Filename of Affine matrix}
}
\value{
A matrix (most likely) or list of matrices (not likely)
}
\description{
Reads in an AFNI Affine matrix from an ASCII file using
\code{\link{readLines}}
}
