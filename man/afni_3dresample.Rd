% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/afni_3dresample.R
\name{afni_3dresample}
\alias{afni_3dresample}
\alias{resample}
\title{AFNI 3dresample function}
\usage{
afni_3dresample(file, bound_type = NULL, dxyz = NULL, orient = NULL,
  resample_mode = c("NN", "Li", "Cu", "Bk"), master = NULL,
  outfile = NULL, retimg = TRUE, opts = "", ...)

resample(...)
}
\arguments{
\item{file}{nifti object or NIfTI filename.}

\item{bound_type}{specify which boundary is preserved, options are
"FOV" (default if \code{NULL}) or "SLAB"}

\item{dxyz}{resample to new dimensions for x, y, and z-directions}

\item{orient}{new axis order.  The orientation code is a 3 character string,
where the characters come from the respective sets {A,P}, {I,S}, {L,R}.
For example LPI makes the x-axis is
Left-to-Right, the y-axis is Posterior-to-Anterior,
and the z-axis is Inferior-to-Superior.}

\item{resample_mode}{Resampling method.
The options are for 'Nearest Neighbor', 'Linear', 'Cubic'
and 'Blocky' interpolation, respectively.}

\item{master}{a filename of an image, which the input file is aligned to
(not doing registration, but takes orientation and pixel dimension information)}

\item{outfile}{Output filename (should not have an extension)}

\item{retimg}{Should an image be returned (\code{TRUE}) or a filename?}

\item{opts}{Additional options passed to \code{3dresample}}

\item{...}{additional arguments to \code{\link{afni_3dAFNItoNIFTI}}}
}
\value{
Output filename of the brik
}
\description{
Wrapper for AFNI \code{3dresample} function
}
