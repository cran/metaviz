#'Thick forest plots for meta-analyses
#'
#'Creates a thick forest plot, a novel variant of the forest plot.
#'
#'The thick forest plot was proposed by Schild and Voracek (2015) as a variant and
#'enhancement of classic forest plots. Thick forest plots use rectangular error bars
#'instead of traditional lines to display confidence intervals (width of the error bar), as well as the relative
#'meta-analytic weight (height of the error bar) of each study. In addition, study and summary level
#'point estimates are depicted clearly by a specific symbol.
#'
#'Thick forest plots have the following advantages, as compared to classic forest plots:
#'
#'\enumerate{
#'\item Using the height of bars proportional to the (relative) meta-analytic weight
#'causes small studies (with wide confidence intervals and less weight in the meta-analysis) to
#'be visually less dominant.
#'
#'\item In classic forest plots, it is often hard to depict the magnitude of
#'point estimates to a reasonable degree of accuracy, especially for studies
#'with large meta-analytic weights and correspondingly large plotting symbols
#'(commonly squares). Specific symbols within the thick forest plot improve the
#'visualization of study point estimates.}
#'
#'Note that for subgroup analysis the height of each error bar is scaled by the weight of each study within the subgroup divided by
#'the sum of the weights of all studies irrespective of subgroup. Therefore, with subgroups present, the overall impression of error
#'bar heights within a given subgroup compared to other subgroups conveys information about the relative precision of the meta-analytic
#'estimate within the subgroup.
#'
#'@param x data.frame or matrix with the effect sizes of all studies (e.g.,
#'  correlations, log odds ratios, or Cohen \emph{d}) in the first column and their
#'  respective standard errors in the second column. Alternatively, x can be the
#'  output object of function \code{\link[metafor]{rma.uni}} from package
#'  \pkg{metafor}; then effect sizes and standard errors are extracted from \code{x}.
#'@param group factor indicating the subgroup of each study to plot a subgroup forest plot. Has to be in the same order than \code{x}.
#'@param type character string indicating the type of forest plot to be plotted. Can be "standard" (default), "study_only",
#'  "summary_only", "cumulative", or "sensitivity". See 'Details'.
#'@param method character string indicating which method should be used to compute the study weights and summary effect(s).
#'  Can be any method argument from \code{\link[metafor]{rma.uni}}
#'  (e.g., "FE" for the fixed effect model, or "DL" for the random effects model using the
#'  DerSimonian-Laird method to estimate \eqn{\tau^2}{tau squared}).
#'  If input \code{x} is an output object of function \code{\link[metafor]{rma.uni}} from package \pkg{metafor}, then the method is extracted from \code{x}.
#'@param study_labels a character vector with names/identifiers to annotate each study in the forest plot.
#'  Has to be in the same order than \code{x}. Ignored if \code{study_table} and/or \code{summary_table} is supplied.
#'@param summary_label a character string specifying the name to annotate the summary effect. If a subgroup
#'  analysis is plotted, \code{summary_label} should be a character vector with a name for each
#'  subgroup summary effect, arranged in the order of the levels of \code{group}. Ignored if \code{study_table} and/or
#'  \code{summary_table} is supplied.
#'@param confidence_level numeric value. The confidence level for the plotted confidence bars.
#'@param col character string specifying the color used for the study-level error bars. Can be a vector of length \code{nrow(x)}
#'  with colors for each study-level result individually.
#'@param tick_col character string specifying the color used for the ticks indicating the point estimates.
#'@param summary_col character string specifying the main color for plotting the summary effect(s). Can be a vector
#'  with colors for each subgroup summary effect individually.
#'@param text_size numeric value. Size of text in the forest plot. Default is 3.
#'@param xlab character string specifying the label of the x axis. By default also used for the header of the aligned table if \code{annotate_CI} is \code{TRUE}.
#'@param x_limit numeric vector of length 2 with the limits (minimum, maximum) of the x axis.
#'@param x_trans_function function to transform the labels of the x axis. Common uses are to transform
#'  log-odds-ratios or log-risk-ratios with \code{exp} to their original scale (odds ratios and risk ratios), or Fisher's z values
#'  back to correlation coefficients using \code{tanh}.
#'@param x_breaks numeric vector of values for the breaks on the x-axis. When used in tandem with \code{x_trans_function}
#'  the supplied values should be not yet transformed.
#'@param annotate_CI logical scalar. Should the effect size and confidence interval values be shown as text in an aligned table on the right-hand side of the forest plot?
#'@param study_table a data.frame with additional study-level variables which should be shown in an aligned table.
#'  Has to be in the same order than \code{x}.
#'@param summary_table a data.frame with additional summary-level information shown in an aligned table.
#'  If \code{group} is supplied, \code{summary_table} must have a row for each subgroup
#'  summary effect, arranged in the order of the levels of \code{group}.
#'@param table_headers character vector. Headers for each column of aligned tables via \code{study_table}, \code{summary_table}, or \code{annotate_CI}.
#'@param table_layout numeric layout matrix passed to \code{layout_matrx} of \code{\link[gridExtra]{arrangeGrob}}. Can be used to overwrite the default spacing
#'  of the forest plot and aligned tables via \code{study_table}, \code{summary_table}, and \code{annotate_CI}.
#'@references Schild, A. H., & Voracek, M. (2015). Finding your way out of the
#'  forest without a trail of bread crumbs: Development and evaluation of two
#'  novel displays of forest plots. \emph{Research Synthesis Methods}, \emph{6},
#'  74-86.
#'@return A thick forest plot is created using ggplot2.
#'@author Michael Kossmeier* <michael.kossmeier@univie.ac.at>
#'@author Ulrich S. Tran* <ulrich.tran@univie.ac.at>
#'@author Martin Voracek* <martin.voracek@univie.ac.at>
#'@author *Department of Basic Psychological Research and Research Methods, School of Psychology, University of Vienna
#'@examples
#' library(metaviz)
#' # Plotting a thick forest plot using the mozart data
#' viz_thickforest(x = mozart[, c("d", "se")],
#' study_labels = mozart[, "study_name"], xlab = "Cohen d")
#'
#' # Visualizing a subgroup analysis of published and unpublished studies
#' viz_thickforest(x = mozart[, c("d", "se")], group = mozart[, "rr_lab"],
#' study_labels = mozart[, "study_name"], method = "REML",
#' summary_label = c("Summary (rr_lab = no)", "Summary (rr_lab = yes)"),
#' xlab = "Cohen d")
#'
#' # Showing additional information in aligned tables. Log risk ratios are labeled
#' # in their original metric (risk ratios) on the x axis.
#' viz_thickforest(x = exrehab[, c("logrr", "logrr_se")],
#' annotate_CI = TRUE, xlab = "RR", x_trans_function = exp,
#' study_table = data.frame(
#' Name = exrehab[, "study_name"],
#' eventsT = paste(exrehab$ai, "/", exrehab$ai + exrehab$bi, sep = ""),
#' eventsC = paste(exrehab$ci, "/", exrehab$ci + exrehab$di, sep = "")),
#' summary_table = data.frame(
#' Name = "Summary",
#' eventsT = paste(sum(exrehab$ai), "/", sum(exrehab$ai + exrehab$bi), sep = ""),
#' eventsC = paste(sum(exrehab$ci), "/", sum(exrehab$ci + exrehab$di), sep = "")),
#' table_layout = matrix(c(1, 1, 2, 2, 3), nrow = 1))
#'@export
viz_thickforest <- function(x, group = NULL, type = "standard", method = "FE",
                           study_labels = NULL, summary_label = NULL,
                           confidence_level = 0.95, col = "Blues", summary_col = col, tick_col = "firebrick",
                           text_size = 3, xlab = "Effect", x_limit = NULL,
                           x_trans_function = NULL, x_breaks = NULL, annotate_CI = FALSE,
                           study_table = NULL, summary_table = NULL, table_headers = NULL,
                           table_layout = NULL) {

  viz_forest(x, group = group, type = type, variant = "thick", method = method,
             study_labels = study_labels, summary_label = summary_label,
             confidence_level = confidence_level, col = col,  summary_col = summary_col, tick_col = tick_col,
             text_size = text_size, xlab = xlab, x_limit = x_limit,
             x_trans_function = x_trans_function, x_breaks = NULL,
             annotate_CI = annotate_CI, study_table = study_table, summary_table = summary_table,
             table_headers = table_headers, table_layout = table_layout)
}
