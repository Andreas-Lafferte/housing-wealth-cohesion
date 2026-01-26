remove_value_labels <- function(x, codes_to_remove) {
    if (!is.null(get_labels(x, attr.only = TRUE))) {
        labs <- get_labels(x, values = TRUE, attr.only = TRUE)
        labs <- labs[!names(labs) %in% as.character(codes_to_remove)]
        set_labels(x, labels = labs)
    } else {
        x
    }
}

# Create function to invert the scale of 5 categories likert variables
invert_scale <- function(x, cats = 5) {
    plus <- cats + 1
    ((x * (-1)) + plus)
}

impute_waves <- function(data, base_var, wave_to_impute = "w01", waves_source = c("w02", "w03", "w04", "w05", "w06")) {
    # Create variable names
    vars_to_impute <- paste0(base_var, "_", waves_source)
    base_var <- paste0(base_var, "_", wave_to_impute)

    # Check if variables are in the data
    vars_exist <- vars_to_impute %in% names(data)
    if (!any(vars_exist)) {
        stop("Ninguna variable para imputar existe en el dataset.")
    }

    # Keep only existing variables
    vars_to_impute <- vars_to_impute[vars_exist]

    # Keep the first non-NA value!
    data %>%
        mutate(
            !!base_var := coalesce(!!sym(base_var), !!!syms(vars_to_impute))
        )
}

impute_wave_long <- function(data, var, target_wave, donor_waves) {
  stopifnot(all(c("idencuesta", "ola") %in% names(data)))
  
  var_sym  <- rlang::ensym(var)
  var_name <- rlang::as_string(var_sym)
  
  # Create a type-consistent NA to use as reduce initializer
  ptype <- vctrs::vec_ptype(data[[var_name]])
  na0   <- vctrs::vec_cast(NA, ptype)
  
  donors <- data %>%
    group_by(idencuesta) %>%
    summarise(
      .donor_value = purrr::reduce(
        donor_waves,
        function(acc, w) {
          v <- dplyr::first((!!var_sym)[ola == w])
          dplyr::coalesce(acc, v)
        },
        .init = na0
      ),
      .groups = "drop"
    )
  
  data %>%
    left_join(donors, by = "idencuesta") %>%
    mutate(
      !!var_sym := dplyr::if_else(
        ola == target_wave & is.na(!!var_sym),
        .donor_value,
        !!var_sym
      )
    ) %>%
    select(-.donor_value)
}

