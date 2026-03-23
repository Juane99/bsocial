#' Classify Strains as Cooperators, Cheaters, or Neutrals
#'
#' Uses pairwise t-tests and median comparisons to classify each strain
#' based on its effect on consortium fitness.
#'
#' @param .Object A \linkS4class{bsocial} object after \code{analyze_social_behavior()} has been called.
#' @return The modified \linkS4class{bsocial} object with \code{resultados_analisis$summary_gen} and \code{$summary_gr}.
#'
#' @export
setMethod("summarize_social_behavior", "bsocial", function(.Object) {
  bsocial_log("INFO", "summarize_social_behavior(): iniciando")

  sb <- .Object@resultados_analisis$social_behavior

  if (is.null(sb) || !isTRUE(sb$success)) {
    .Object@resultados_analisis$summary_gen <- list(
      positives = character(0),
      negatives = character(0),
      neutrals  = character(0)
    )
    .Object@resultados_analisis$summary_gr <- .Object@resultados_analisis$summary_gen
    return(.Object)
  }

  cepas <- .Object@cepas_seleccionadas
  nstrains <- length(cepas)

  summarize_one_metric <- function(data_for_boxplot) {
    if (is.null(data_for_boxplot) || nrow(data_for_boxplot) == 0) {
      return(list(
        positives = character(0),
        negatives = character(0),
        neutrals  = character(0)
      ))
    }

    nr <- nrow(data_for_boxplot)

    list_anova <- list()

    for (i in seq_len(nstrains)) {
      all_col <- data_for_boxplot[, (i - 1) * 3 + 1]
      not_col <- data_for_boxplot[, (i - 1) * 3 + 2]
      pres_col <- data_for_boxplot[, (i - 1) * 3 + 3]

      dataOneWayComparisons <- rbind(
        cbind(Treatment = paste0(cepas[i], "_ALL"),        Fitness = all_col),
        cbind(Treatment = paste0(cepas[i], "_NotPresent"), Fitness = not_col),
        cbind(Treatment = paste0(cepas[i], "_Present"),    Fitness = pres_col)
      )

      dfw <- as.data.frame(dataOneWayComparisons, stringsAsFactors = FALSE)
      dfw$Fitness <- suppressWarnings(as.numeric(dfw$Fitness))
      dfw <- stats::na.omit(dfw)

      list_anova[[cepas[i]]] <- dfw
    }

    signif <- logical(length(list_anova))
    j <- 1
    for (nm in names(list_anova)) {
      dataOneWayComparisons <- list_anova[[nm]]

      if (nrow(dataOneWayComparisons) == 0 ||
          length(unique(dataOneWayComparisons$Treatment)) < 2) {
        signif[j] <- FALSE
      } else {
        tt <- try(
          stats::pairwise.t.test(
            dataOneWayComparisons$Fitness,
            dataOneWayComparisons$Treatment,
            p.adjust.method = "none"
          ),
          silent = TRUE
        )

        if (inherits(tt, "try-error") || is.null(tt$p.value)) {
          signif[j] <- FALSE
        } else {
          pmat <- tt$p.value
          min_p <- suppressWarnings(min(pmat, na.rm = TRUE))
          p22 <- if (nrow(pmat) >= 2 && ncol(pmat) >= 2) pmat[2, 2] else NA
          if (!is.finite(min_p)) {
            signif[j] <- FALSE
          } else {
            cond <- FALSE
            if (min_p <= 0.05) {
              if (nrow(pmat) >= 2 && ncol(pmat) >= 2 && !is.na(pmat[2, 2])) {
                cond <- (pmat[2, 2] <= 0.05)
              }
            }
            signif[j] <- isTRUE(cond)
          }
        }
      }
      j <- j + 1
    }

    def_hay <- !signif

    medianas <- apply(data_for_boxplot, 2, stats::median, na.rm = TRUE)

    cooperators <- logical(nstrains)
    cheaters    <- logical(nstrains)
    absolute_cheaters <- logical(nstrains)

    for (i in seq_len(nstrains)) {
      m_all  <- medianas[(i - 1) * 3 + 1]
      m_not  <- medianas[(i - 1) * 3 + 2]
      m_pres <- medianas[(i - 1) * 3 + 3]

      if (!is.na(m_not) && !is.na(m_pres)) {
        cooperators[i] <- (m_not < m_pres)
        cheaters[i]    <- (m_not > m_pres)
      } else {
        cooperators[i] <- FALSE
        cheaters[i]    <- FALSE
      }

      if (!is.na(m_not) && !is.na(m_pres) && !is.na(m_all)) {
        absolute_cheaters[i] <- (m_not > 1 && m_pres > 1 && m_all > 1)
      } else {
        absolute_cheaters[i] <- FALSE
      }
    }

    cooperators[is.na(cooperators)]       <- FALSE
    cheaters[is.na(cheaters)]             <- FALSE
    absolute_cheaters[is.na(absolute_cheaters)] <- FALSE

    aux <- logical(nstrains)
    for (i in seq_len(nstrains)) {
      aux[i] <- (cooperators[i] && signif[i])
      if (isTRUE(aux[i])) def_hay[i] <- FALSE
    }
    pos_names <- cepas[aux]

    aux <- logical(nstrains)
    for (i in seq_len(nstrains)) {
      aux[i] <- (absolute_cheaters[i] || (cheaters[i] && signif[i]))
      if (isTRUE(aux[i])) def_hay[i] <- FALSE
    }
    neg_names <- cepas[aux]

    neu_names <- cepas[def_hay]

    list(
      positives = pos_names,
      negatives = neg_names,
      neutrals  = neu_names
    )
  }

  summary_gen <- summarize_one_metric(sb$data_gen)
  summary_gr  <- summarize_one_metric(sb$data_gr)

  .Object@resultados_analisis$summary_gen <- summary_gen
  .Object@resultados_analisis$summary_gr  <- summary_gr

  .Object
})
