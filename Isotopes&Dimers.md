# Adducts & Dimers Table
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

# Script
```r
packages <- c("tibble", "tidyr", "dplyr")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

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

# Example of Dimer and Isotope Series
**Assume:** <br>
Monoisotopic neutral mass of glucose: ~180.063388 Da <br>
Proton: 1.007276 Da <br>
Isotope step for z=1: ~1.00336 Da <br>
Isotope step for z=2: ~0.50168 Da (= 1.00336 / 2) <br>
The two observed series (same RT) <br>
 <br>
**Monomer z = 1: M+H⁺** <br>
m₀ ≈ 181.07066 <br>
m₁ ≈ 182.07402 (m₀ + 1.00336) <br>
m₂ ≈ 183.07738 (m₁ + 1.00336) <br>
<br>
**Dimer/cluster z = 2: 2M+2H²⁺**<br>
d₀ ≈ 181.07066<br>
d₁ ≈ 181.57234 (d₀ + 0.50168)<br>
d₂ ≈ 182.07402 (d₁ + 0.50168)<br>
d₃ ≈ 182.57570 (d₂ + 0.50168)<br>
d₄ ≈ 183.07738 (d₃ + 0.50168)<br>
<br>
**Compute between-feature Δm/z for some pairs:** <br>
Δ = 0.5 (0.5 × 1.00336 ≈ 0.50168)<br>
m₁ − d₁ ≈ 182.07402 − 181.57234 = 0.50168<br>
m₂ − d₃ ≈ 183.07738 − 182.57570 = 0.50168<br>
d₁ − m₀ ≈ 181.57234 − 181.07066 = 0.50168<br>
d₃ − m₁ ≈ 182.57570 − 182.07402 = 0.50168<br>
<br>
Δ = 1.5 (1.5 × 1.00336 ≈ 1.50504)<br>
m₂ − d₁ ≈ 183.07738 − 181.57234 = 1.50504<br>
d₃ − m₀ ≈ 182.57570 − 181.07066 = 1.50504<br>

…and similarly for Δ = 2.5 (~2.5084 Da), etc.<br>
<br>

> [!IMPORTANT]
>**Since Isotopes and Dimers are determined by graph construction, we recommend to keep the default value of `n` is 1 and `n_d` 3.** <br>
> For isotopes, `n` = 1 forces the algorithm to find the $1^{st}$ $^{13}C$ isotope before it can group an $M+2$ or $M+3$ peak. If `n` is set higher, the app might incorrectly cluster two independent molecules that are simply separated by exactly $2.0067$ Da, even if the intermediate $M+1$ peak is missing.<br>
> For dimers, an `n_d` = 3 provides robust 'bridging' between monomer and dimer clusters. Because these half-integer shifts ($1.5, 2.5, 3.5$ Da) are unique and do not overlap with standard isotope patterns, a higher `n_d` value significantly improves the recovery of co-eluting dimer complexes without risking over-connection of the feature graph.
