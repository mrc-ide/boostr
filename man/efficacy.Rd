% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/efficacy.R
\name{efficacy}
\alias{efficacy}
\title{Estimate vaccine efficacy}
\usage{
efficacy(titre, max_efficacy, alpha, beta, cpp = TRUE)
}
\arguments{
\item{titre}{Vector of antibody titres}

\item{max_efficacy}{Maximum vaccine efficacy}

\item{alpha}{Shape parameter}

\item{beta}{Scale parameter}

\item{cpp}{Use cpp efficacy model}
}
\value{
Vector of antibody efficacies for each titre
}
\description{
Estimate vaccine efficacy
}
