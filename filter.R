filter_data = function(data, filter_parameters) {
  
}

# general functions ------------------------------------------------------------
# TODO find a way to filter certain ages
# TODO create a general filtering function

filter <- function(data, experiment_type, cohorts = NULL, subjects = NULL,
                   age = NULL, replicate = NULL) {
  exp_type <- list(
    "bodyweight" = list("exp_type" = "weight", "exp_sub_1" = "body"),
    "mri" = list("exp_type" = "mri", "exp_sub_1" = c("fat", "lean"), "exp_type_2" = "percentage"),
    "gtt" = list("exp_type" = "gtt"),
    "muscle_weights" = list("exp_type" = "weight", "exp_sub_1" = c("SOL", "EDL", "TA", "GA"))
  )
  # deparse(sys.calls())
  # deparse(sys.calls()[[sys.nframe()-1]])
  as.character(match.call()[[1]])
}

filter_experiment_type <- function(data, experiment_type) {

}

filter_subject <- function(data, cohorts = NULL, subjects = NULL) {

}

filter_age <- function(data, age = NULL) {

}