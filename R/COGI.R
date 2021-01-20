#' Calculate percentage below targeted values
#'
#' #' @description
#' The function below_percent produces a tibble object with values equal to
#' the percentage of glucose measurements below target values. The output columns
#' correspond to the subject id followed by the target values and the output rows
#' correspond to the subjects. The values will be between 0 (no measurements)
#' and 100 (all measurements).
#'
#' @usage
#' below_percent(data, targets_below = c(50, 80))
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or numeric vector of glucose values.
#'
#' @param targets_below Numeric vector of glucose thresholds. Glucose values from
#' data argument will be compared to each value in the targets_below vector.
#' Default list is (50, 80).
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' column for each target value is returned. NA's will be omitted from the glucose
#' values in calculation of percent.
#'
#'
#' @return If a data.frame object is passed, then a tibble object with
#' a column for subject id and then a column for each target value is returned. If a vector of glucose
#' values is passed, then a tibble object without the subject id is returned.
#' as.numeric() can be wrapped around the latter to output a numeric vector.
#'
#' @export
#'
#' @references
#' Rodbard (2009) Interpretation of continuous glucose monitoring data:
#' glycemic variability and quality of glycemic control,
#' \emph{Diabetes Technology and Therapeutics} \strong{11} .55-67,
#' \doi{10.1089/dia.2008.0132}.
#'
#' @examples
#'
#' data(example_data_1_subject)
#'
#' below_percent(example_data_1_subject)
#' below_percent(example_data_1_subject, targets_below = c(50, 100, 180))
#'
#' data(example_data_5_subject)
#'
#' below_percent(example_data_5_subject)
#' below_percent(example_data_5_subject, targets_below = c(80, 180))
#'


COGI <- function(data, targets = c(70,180)){

  weight_features = function(feature, scale_range, weight = 1, increasing = TRUE) {
    if (increasing) {
      out = pmin(as.numeric(feature>min(scale_range))*(feature-min(scale_range))/(max(scale_range)-min(scale_range)), 1)
      return(out*weight)
    } else {
      out = pmin(as.numeric(feature<max(scale_range))*(feature-max(scale_range))/(min(scale_range)-max(scale_range)), 1)
      return(out*weight)
    }
  }



  x = target_val = id = NULL
  rm(list = c("id", "target_val", "x"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")
  targets = sort(as.double(targets))
  ir = in_range_percent(data, list(targets))[,2]
  br = below_percent(data, targets_below = targets[1])[,2]
  stddev = sd_glu(data)[,2]
  subjects = sd_glu(data)[,1]

  w_f = weight_features(ir,c(0,100),weight = .5)+weight_features(br,c(0,15),weight = .35, increasing = FALSE)+weight_features(stddev,c(18,108),weight = .15)
  out = data.frame(subjects, w_f)

  if (is_vector) {
    out$id = NULL
  }
  return(out)
}




