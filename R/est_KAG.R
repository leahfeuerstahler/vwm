#' Estimate Visual Working Memory Capacity, Attention, and Guessing Parameters
#'
#' This function estimates visual working memory capactity (K), attentional
#' engagement (A), and guessing (G) from data gathered under the canonical change
#' detection task (Luck & Vogel, 1997) or the multiple change detection task
#' (Gibson, Wasserman, & Luck, 2011).
#'
#' @param S Number of displayed objects for each trial type
#' @param C Number of changed objects for each trial type
#' @param N Number of times each trial type is administered
#' @param data For one examinee, the number of observed "change" respones for
#' each trial type. For multiple examinees, a matrix of observed "change"
#' responses, one column for each trial type.
#' @param nrep Number of estimation replications. Because parameter estimation
#' is highly sensitive to local maxima, higher values of nrep will help avoid
#' suboptimal solutions.
#'
#' @return Matrix with the following columns:
#' \item{Khat}{Estimate of working memory capacity (K)}
#' \item{Ahat}{Estimate of attentional engagement (A)}
#' \item{Ghat}{Estimate of guessing (G)}
#' \item{Obs_C_S}{Observed proportion of "change" responses for each trial type
#' (defined by C and S)}
#' \item{Pred_C_S}{Model-predicted proportion of "change" responses for each
#' trial type (defined by C and S)}
#' \item{logLik}{Model negative log likelihood}
#' \item{rmseFit}{Root mean squared difference between observed and predicted
#' probabilities, a measure of model-data fit}
#' \item{convergence}{Convergence flag. If equal to a value other than '0',
#' score estimates are not reliable.}
#'
#' @details
#'
#' This function estimates visual working memory capacity (\eqn{K}), attentional
#' engagement (\eqn{A}), and guessing (\eqn{G}) as defined in the model
#' described below. In this model, the probability of a "change" response
#' depends on \eqn{K}, \eqn{A}, and \eqn{G}, as well as the number of objects
#' on the screen \eqn{S} and the number of changed objects \eqn{C}. If no
#' objects change on a given trial (\eqn{C = 0}), then the probability of a
#' "change" response, also known as the false-alarm rate, equals \eqn{G}. If
#' instead \eqn{C > 0} objects change, the probability of a "change" response,
#' also known as the hit rate, equals
#'
#' \deqn{P_{C,S} = G + (1 - G)AD_{C,S},}
#'
#' where
#'
#' \deqn{D_{C,S} = min[1, 1 - \prod_{i = S-C+1}^{S} (i - K) / i] if C \leq S - K}
#'
#' and
#'
#' \deqn{D_{C,S} = 1 if C > S - K}.
#'
#' @references
#'
#' Feuerstahler, L. M., Luck, S. J., MacDonald III., A., & Waller, N. G. (in
#' press). A note on the identification of change detection task models to
#' measure storage capacity and attention in visual working memory. \emph{
#' Behavior Research Methods}.
#'
#' Gibson, B., Wasserman, E., & Luck, S. J. (2011). Qualitative similarities in
#' the visual short-term memory of pigeons and people. \emph{Psychonomic
#' Bulletin & Review}, \emph{18}, 979--984.
#'
#' Luck, S. J., & Vogel, E. K. (1997). The capacity of visual working memory for
#' features and conjunctions. \emph{Nature}, \emph{390}, 279--281.
#'
#'
#' @examples
#'
#' set.seed(4321)
#'
#' # multiple change detection task with 5 displayed objects
#' #   and 0, 1, 2, and 5 changed objects, 60 reps of each trial type
#' S <- c(5, 5, 5, 5)
#' C <- c(0, 1, 2, 5)
#' N <- c(60, 60, 60, 60)
#'
#' # examinee 1 gave a "change" response to 10, 40, 50, and 58 of the
#' #   0, 1, 2, and 5-change trials
#'
#' data1 <- c(10, 40, 50, 58)
#'
#' out1 <- est_KAG(S = S, C = C, N = N, data = data1)
#' out1
#'
#' #          Khat      Ahat      Ghat   Obs_0_5   Obs_1_5   Obs_2_5   Obs_5_5  Pred_0_5  Pred_1_5  Pred_2_5  Pred_5_5   logLik    rmseFit convergence
#' # [1,] 2.819044 0.9555343 0.1748655 0.1666667 0.6666667 0.8333333 0.9666667 0.1748655 0.6193973 0.8617735 0.9633099 7.990804 0.02793617           0
#'
#' # examinee 2 gave a "change" response to 5, 20, 40, and 50 of the
#' #   0, 1, 2, and 5-change trials
#'
#' data2 <- c(5, 20, 40, 50)
#' out2 <- est_KAG(S = S, C = C, N = N, data = data2)
#' out2
#'
#' #          Khat      Ahat       Ghat    Obs_0_5   Obs_1_5   Obs_2_5   Obs_5_5   Pred_0_5  Pred_1_5 Pred_2_5  Pred_5_5   logLik   rmseFit convergence
#' # [1,] 2.054223 0.8254206 0.07727247 0.08333333 0.3333333 0.6666667 0.8333333 0.07727247 0.3901875 0.620632 0.8389108 8.835067 0.0368084           0
#'
#' # estimate parameters for both examinees at the same time
#'
#' out3 <- est_KAG(S = S, C = C, N = N, data = rbind(data1, data2))
#' out3
#'
#' #          Khat      Ahat       Ghat    Obs_0_5   Obs_1_5   Obs_2_5   Obs_5_5   Pred_0_5  Pred_1_5  Pred_2_5  Pred_5_5   logLik    rmseFit convergence
#' # [1,] 2.818856 0.9555488 0.17483180 0.16666667 0.6666667 0.8333333 0.9666667 0.17483180 0.6193589 0.8617533 0.9633203 7.990804 0.02794453           0
#' # [2,] 2.054328 0.8254129 0.07729917 0.08333333 0.3333333 0.6666667 0.8333333 0.07729917 0.3902182 0.6206573 0.8389083 8.835067 0.03681112           0
#'
#'
#' @importFrom dfoptim nmkb
#' @export


est_KAG <- function(S, C, N, data, nrep = 20){

  # if data is provided for only one person, format as a matrix
  if(length(data) == length(S)) data <- matrix(data, nrow = 1)

  # number of trial types
  ntt <- length(S)

  # ensure that S, C, N, and data all reference the same number of trials
  stopifnot(ntt == length(C))
  stopifnot(ntt == length(N))
  stopifnot(ntt == ncol(data))

  # ensure that the number of changed objects is less than the total number of objects
  stopifnot(all(C <= S))

  # ensure that the number of "change" responses is less than the number of trials
  for(i in 1:nrow(data)) stopifnot(all(data[i, ] <= N))

  # log likelihood function
  logLik <- function(KAG, S, C, N, data_i){

    # kag: vector of length 3 containing the k, a, and g parameters
    # data_i: vector of observed number of "change" responses for one examinee

    # pull out K, A, and G parameters
    K <- KAG[1]
    A <- KAG[2]
    G <- KAG[3]

    # get predicted response probabilities
    pred <- numeric(length(S))

    for(t in 1:ntt){ # for each trial type t

      if(C[t] == 0) D <- 0

      if(C[t] > 0){
        idx <- (S[t] - C[t] + 1) : S[t] ## indices to take the product over
        D <- min(1, 1 - prod((idx - K) / idx))
        if(C[t] > S[t] - K) D <- 1 ## if the number of changed objects is bigger than the number of objects not in working memory
      }
      pred[t] <- A * D * (1 - G) + G
    }

    ## get negative model log likelihood
    lik <- numeric(ntt)

    for(t in 1:ntt){

      lik[t] <- stats::dbinom(data_i[t], N[t], pred[t])
    }

    ## avoid problems if probabilities are exactly = 0
    lik[lik == 0] <- 10^-10

    # output negative log likelihood
    -sum(log(lik))
  }

  # prepare output matrix
  OUT <- matrix(nrow = nrow(data), ncol = 2 * ntt + 6)

  for(i in 1:nrow(data)){

    # randomly generate algorithm start values for the K, A, and G parameters
    startValMat <- cbind(stats::runif(nrep, 0, max(S)), # 0 <= K <= max(S)
                         stats::runif(nrep, 0, 1), # 0 <= A <= 1
                         stats::runif(nrep, 0, 1)) # 0 <= G <= 1

    # optimize starting at each set of start values
    res_tmp <- lapply(1:nrep, function(x){
      out <- try(dfoptim::nmkb(par = startValMat[x, ], fn = logLik,
                               lower = c(0, 0, 0), upper = c(max(S), 1, 1),
                               S = S, C = C, N = N, data_i = data[i, ]))
      if(class(out) == "try-error") out <- list(value = NA, convergence = 1)
      out
    })

    # keep the solution with the highest log likelihood (min negative log likelihood)
    #    and penalize nonconvergenced solutions
    best <- which.min(sapply(res_tmp, function(x) x$value + 100000 * x$convergence))
    res <- res_tmp[[best]]

    ## extract parameters
    KAG <- res$par
    K <- KAG[1]
    A <- KAG[2]
    G <- KAG[3]

    ## get predicted response probabilities
    pred <- numeric(ntt)

    for(t in 1:ntt){ # for each trial type t

      if(C[t] == 0) D <- 0

      if(C[t] > 0){
        idx <- (S[t] - C[t] + 1) : S[t] ## indices to take the product over
        D <- min(1, 1 - prod((idx - K) / idx))
        if(C[t] > S[t] - K) D <- 1 ## if the number of changed objects is bigger than the number of objects not in working memory
      }
      pred[t] <- A * D * (1 - G) + G
    }

    ## root mean squared difference between observed proportions and predicted probabilities
    rmseFit <- sqrt(mean((data[i, ] / N - pred)^2))

    OUT[i,] <- c(res$par, # length 3 - Khat, Ahat, Ghat
                 data[i, ] / N, # length ntt - observed proportions of "change" responses
                 pred, # length ntt - predicted proportions of "change" responses
                 res$value, # length 1 - model negative log likelihood
                 rmseFit, # length 1 -
                 res$convergence) # length 1 - convergence flag, re-run results if not equal to 0

  }

  colnames(OUT) <- c("Khat", "Ahat", "Ghat",
                     paste("Obs_", apply(cbind(C, S), 1, paste, collapse = "_"), sep = ""),
                     paste("Pred_", apply(cbind(C, S), 1, paste, collapse = "_"), sep = ""),
                     "logLik",
                     "rmseFit",
                     "convergence")
  OUT
}

