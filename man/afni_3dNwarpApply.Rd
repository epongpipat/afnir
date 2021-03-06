% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/afni_3dNwarpApply.R
\name{afni_3dNwarpApply}
\alias{afni_3dNwarpApply}
\alias{afni_warp_apply}
\alias{NwarpApply}
\title{AFNI 3dNwarpApply function}
\usage{
afni_3dNwarpApply(file, warp, fixed = NULL, interpolator = c("wsinc5",
  "cubic", "linear", "NN", "quintic"), invert_warp = FALSE,
  outfile = NULL, retimg = TRUE, opts = "", ...)

afni_warp_apply(...)

NwarpApply(...)
}
\arguments{
\item{file}{nifti object or NIfTI filename to register to \code{fixed} image}

\item{warp}{list of nifti objects or NIfTI filenames of warps to be applied}

\item{fixed}{nifti object or NIfTI filename of the target space, used
to determine the output grid/resolution.  If not specified, then it will use
that of \code{file}.  You can also specify this as \code{"WARP"} to use the
warps as the destination resolutione/etc.}

\item{interpolator}{interpolation method to use during matching process
NN - nearest neighbor.}

\item{invert_warp}{After the warp specified in \code{warp} is computed,
invert it.  If the input warp would take a dataset
from space A to B, then the inverted warp will do
the reverse.}

\item{outfile}{Output filename (should not have an extension).  This
should be the same length as \code{file}}

\item{retimg}{Should an image be returned (\code{TRUE}) or a filename?}

\item{opts}{Additional options passed to \code{3dNwarpApply}}

\item{...}{additional arguments to \code{\link{afni_3dAFNItoNIFTI}}}
}
\value{
Output filename of the brik
}
\description{
Wrapper for AFNI \code{3dNwarpApply} function
}
