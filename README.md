# rndop

**rndop** is an R package for accessing and downloading data from [The Species 
Occurrence Database of NCA CR](https://portal.nature.cz/nd/) (NDOP AOPK).

**NDOP** is Czech national species occurrence database of NCA CR (Nature 
Conservation Agency of the Czech Republic). 
Database includes occurrence records both from professionals (monitorings, 
inventarisations) and citizen scientists. Use of the database is free and 
requires free [registration](https://idm.nature.cz/idm/#/registration). 
Data itself are published under CC BY-SA 4.0 licence and should be referenced 
according to 
[AOPK citation
 rules](https://portal23.nature.cz/publik_syst/ctihtmlpage.php?what=4910).

## Installation

**Development version:**

```r
# install.packages("remotes")
remotes::install_github("kalab-oto/rndop")
```

**Specific version**

Use `install_github` `ref` argument with desired version number or commit (see `?install_github`):

```r
remotes::install_github("kalab-oto/rndop", ref = "v0.2.0")
```

> [!WARNING]  
> This package is under development, and older versions may not work preperly.

## Usage

```r
library(rndop)

mr <- ndop_download("mantis religiosa")
head(mr)
```

