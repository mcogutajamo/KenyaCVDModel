#' Sample Next Health State Based on Transition Probabilities
#'
#' This function determines the next health state for individuals based on a matrix of transition probabilities.
#' It samples a random value for each individual and assigns them to a new state using cumulative transition probabilities.
#'
#' @param m_trans_probs A numeric matrix of transition probabilities where each row represents an individual's transition probabilities across states.
#' @param v_states_names A character vector of state names corresponding to the columns of `m_trans_probs`.
#'
#' @return A character vector indicating the health state each individual transitions to in the next cycle.
#'
#' @examples
#' # Define a transition probability matrix (3 individuals, 3 states)
#' m_trans_probs <- matrix(c(0.7, 0.2, 0.1,
#'                           0.6, 0.3, 0.1,
#'                           0.5, 0.4, 0.1),
#'                         nrow = 3, byrow = TRUE)
#' v_states_names <- c("Healthy", "Diseased", "Dead")
#'
#' # Sample next states
#' sampleV(m_trans_probs, v_states_names)
#'
#' @export
sampleV <- function(
    m_trans_probs,
    v_states_names) {

  # create an upper triangular matrix of ones
  m_upper_tri <- upper.tri(
    x = diag(ncol(m_trans_probs)),
    diag = TRUE
  )

  # create matrix with row-wise cumulative transition probabilities
  m_cum_probs <- m_trans_probs %*% m_upper_tri
  colnames(m_cum_probs) <- v_states_names

  # ensure that the maximum cumulative probabilities are equal to 1
  if (any(m_cum_probs[, ncol(m_cum_probs)] > 1.000000)) {
    stop("Error in multinomial sampling: probabilities do not sum to 1")
  }

  # sample random values from Uniform standard distribution for each individual
  v_rand_values <- runif(n = nrow(m_trans_probs))

  # repeat each sampled value to have as many copies as the number of states
  m_rand_values <- matrix(
    data  = rep(
      x = v_rand_values,
      each = length(v_states_names)
    ),
    nrow  = nrow(m_trans_probs),
    ncol  = length(v_states_names),
    byrow = TRUE
  )

  # identify transitions, compare random samples to cumulative probabilities
  m_transitions <- m_rand_values > m_cum_probs # transitions from first state

  # sum transitions to identify health state in next cycle
  v_transitions <- rowSums(m_transitions)

  # identify health state to which each individual is transitioning
  v_health_states <- v_states_names[1 + v_transitions]

  return(v_health_states)
}
