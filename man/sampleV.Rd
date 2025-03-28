% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SampleV.R
\name{sampleV}
\alias{sampleV}
\title{Sample Next Health State Based on Transition Probabilities}
\usage{
sampleV(m_trans_probs, v_states_names)
}
\arguments{
\item{m_trans_probs}{A numeric matrix of transition probabilities where each row represents an individual's transition probabilities across states.}

\item{v_states_names}{A character vector of state names corresponding to the columns of \code{m_trans_probs}.}
}
\value{
A character vector indicating the health state each individual transitions to in the next cycle.
}
\description{
This function determines the next health state for individuals based on a matrix of transition probabilities.
It samples a random value for each individual and assigns them to a new state using cumulative transition probabilities.
}
\examples{
# Define a transition probability matrix (3 individuals, 3 states)
m_trans_probs <- matrix(c(0.7, 0.2, 0.1,
                          0.6, 0.3, 0.1,
                          0.5, 0.4, 0.1),
                        nrow = 3, byrow = TRUE)
v_states_names <- c("Healthy", "Diseased", "Dead")

# Sample next states
sampleV(m_trans_probs, v_states_names)

}
