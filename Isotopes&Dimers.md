```r
# Define parameters
z_max <- 3      # Maximum charge state
n_iso <- 3      # Number of isotopes
n_d   <- 3      # Dimer-series order
delta <- 1.00336 # Mass difference for 13C

# Generate Isotope Shifts
shifts_iso <- tidyr::expand_grid(z = 1:z_max, k = 1:n_iso) %>%
  dplyr::mutate(
    name = paste0("C13*", k, " (z=", z, ")"),
    delta_mz = delta * k / z
  ) %>% 
  dplyr::select(name, delta_mz)

# Generate Dimer Shifts
shifts_dimer <- tibble::tibble(
  name = paste0("C13*(", 1:n_d, "+0.5)"),
  delta_mz = delta * ((1:n_d) + 0.5)
)

# Combine into final shifts table
shifts <- dplyr::bind_rows(shifts_iso, shifts_dimer)
print(shifts)
```

| name  | delta_mz |
| ------------- | ------------- |
C13*1 (z=1)|1.003360
C13*2 (z=1)|2.006720
C13*3 (z=1)|3.010080
C13*1 (z=2)|0.501680
C13*2 (z=2)|1.003360
C13*3 (z=2)|1.505040
C13*1 (z=3)|0.334453
C13*2 (z=3)|0.668907
C13*3 (z=3)|1.003360
C13*(1+0.5)|1.505040
C13*(2+0.5)|2.508400
C13*(3+0.5)|3.511760
