# Installation
Package can be installed using `devtools`.
```
if (!require('devtools')) install.packages('devtools')
devtools::install_github('benfasoli/uataq')
```

# Included Functions

1. `archive` splits data_frame by time and saves to csv files.
2. `breakstr` splits strings by the given delimiter and returns a data_frame. Lines that do not fit the correct number of columns are omitted.
3. `calibrate` applies an algorithm for calibrating atmospheric trace gas data by linearly interpolating several reference gas tanks.
4. `find_neighbor` finds the closest value index in y for each x, using findInterval.
5. `iggplot` generates an interactive gadget for a given ggplot object.
6. `keeling` performs keeling isotope analysis, generating a linear fit and d13C keeling plot.
7. `gps_move` calculates the speeds, distances, and directions given vectors of GPS coordinates.
8. `na_interp` linearly interpolates NA values found in vector y with respect to dimension x (e.g. timestamp).
9. `rds2csv` converts .rds files to ASCII tabular representation.
10. `run_smooth` generates smoothed representation of x.
