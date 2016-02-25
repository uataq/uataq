# Installation
Package can be installed using `devtools`.
```
if (!require('devtools')) install.packages('devtools')
devtools::install_github('benfasoli/uataq')
```

# Included Functions

1. `breakstr` splits strings by the given delimiter and returns a data_frame. Lines that do not fit the correct number of columns are omitted.
2. `calibrate` applies an algorithm for calibrating atmospheric trace gas data by linearly interpolating several reference gas tanks.
3. `find_neighbor` finds the closest value index in y for each x, using findInterval.
4. `gps_dir` calculates the direction between GPS coordinates.
5. `gps_distance` calculates the distances between GPS coordinates.
6. `gps_speed` calculates the speed in m/s between GPS coordinates.
7. `na_interp` linearly interpolates NA values found in vector y with respect to dimension x (e.g. timestamp).
8. `rds2csv` converts .rds files to ASCII tabular representation.
9. `run_smooth` generates smoothed representation of x.
