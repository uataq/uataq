# Installation
Package can be downloaded using `devtools`.
```
if (!require('devtools')) install.packages('devtools')
devtools::install_github(‘benfasoli/uataq’)

```

# Documentation
**rds2csv(rds, file, sep=',', quote=F, ...)**  
Creates a .csv representation of the given .rds file.

```
rds   <- 'file.rds'
file  <- 'output.csv'
rds2csv(rds, file)
```
