% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io.R
\name{io.snp_unphased}
\alias{io.snp_unphased}
\title{IO handler for SNP unphased matrix.}
\usage{
io.snp_unphased(filename, read_mode = "file")
}
\arguments{
\item{filename}{File name.}

\item{read_mode}{Reading mode.}
}
\value{
IO handler for SNP unphased data.
}
\description{
IO handler for SNP unphased matrix.
}
\examples{
n <- 123
s <- 423
filename <- paste(tempdir(), "snp_unphased_dummy.snpdat", sep="/")
handle <- io.snp_unphased(filename)
mat <- matrix(
    as.integer(sample.int(
        3, n * s, 
        replace=TRUE, 
        prob=c(0.7, 0.2, 0.1)
    ) - 1),
    n, s
)
impute <- double(s)
handle$write(mat, "mean", impute, 1)
handle$read()
file.remove(filename)
}
