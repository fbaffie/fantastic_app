


# Model options -----------------------------------------------------------

# Deterministic model

det_model <- function(time, runoff, name, color) {
  structure(list(time = time,
                 runoff = runoff,
                 name = name,
                 color = color),
            class = "det_model")
}


# Ensemble model

ens_model <- function(time, runoff_min, runoff_max, name, color) {
  structure(list(time = time,
                 runoff_min = runoff_min,
                 runoff_max = runoff_max,
                 name = name,
                 color = color),
            class = "ens_model")
}

