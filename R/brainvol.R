#' Example data for a meta-analysis of correlations: Human brain volume and intelligence
#'
#' A dataset consisting of 83 empirical studies used in the meta-analysis of
#' Pietschnig, Penke, Wicherts, Zeiler, and Voracek (2015) on the associations between human brain volume and
#' intelligence (measured full-scale IQ) in healthy participant samples.
#'
#' @format A data frame with 83 rows and 8 variables:
#' \describe{
#'   \item{study_name}{short name of each study}
#'   \item{year}{publication year of each study}
#'   \item{r}{observed correlations between intelligence and brain size}
#'   \item{z}{Fisher's z transform of the observed correlations for meta-analysis}
#'   \item{z_se}{Standard error of Fisher's z transformed observed correlations for meta-analysis}
#'   \item{n}{sample size of each study}
#'   \item{sex}{categorical moderator variable: gender of the participants in each study}
#'   \item{mean_age}{continuous moderator variable: mean age of all participants in each study}
#'   }
#' @references Pietschnig, J., Penke, L., Wicherts, J. M., Zeiler, M., & Voracek, M. (2015). Meta-analysis of
#' associations between human brain volume and intelligence differences: How strong are they and what do they mean?.
#' \emph{Neuroscience & Biobehavioral Reviews}, \emph{57}, 411-432.
"brainvol"
