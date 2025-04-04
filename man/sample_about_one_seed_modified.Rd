% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootstrap-snowboot.R
\name{sample_about_one_seed_modified}
\alias{sample_about_one_seed_modified}
\title{Sample about one seed from \code{snowboot} package}
\usage{
sample_about_one_seed_modified(net, seed, n.wave = 1)
}
\arguments{
\item{net}{A network object as described in the \link[snowboot:sample_about_one_seed]{snowboot} package. package}

\item{seed}{Character string of the selected seed to start the LSMI (name of the node).}

\item{n.wave}{Number of waves to be included each the snowball sample}
}
\value{
A list of length \code{n.wave} where each element is the edge list of edges included on the corresponding wave of sampling.
}
\description{
This is a slightly modified version of the \link[snowboot:sample_about_one_seed]{sample_about_one_seed} from the \pkg{snowboot} package.
}
\details{
This is a slightly modified version of the \link[snowboot:sample_about_one_seed]{sample_about_one_seed} from the \pkg{snowboot} package.
This function uses the node names (not node IDs),
and it returns the edge list of the bootstrap sample (not the nodes included in the sample).

This is primarily used as an internal function for \link[JaB:bootstrap_snowboot]{bootstrap_snowboot}.
Most code in this function is directly from \link[snowboot:sample_about_one_seed]{sample_about_one_seed}
from the \pkg{snowboot} package. This function is designed to be a wrapper function
to format the \link[snowboot:sample_about_one_seed]{sample_about_one_seed} results
into the JaB package syntax.
}
\references{
\insertRef{snowboot}{JaB}

\insertRef{gel-lyubchich-ramirez-2017}{JaB}
}
\seealso{
\link[snowboot:lsmi]{lsmi} \link[snowboot:sample_about_one_seed]{sample_about_one_seed}
}
