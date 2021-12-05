# Replication of "Environmental Regulations, Air and Water Pollution, and Infant Mortality in India"

This is the final project repository for **PUBPOL 870K: Statistics & Program Evaluation** at Duke Kunshan University, Fall 2021

Group Member: Yixin Fang, Yiming Li, Xinkai Wang, Weichen Xu, Zhijie Zhou

*Instructor: Prof. Jiahua Yue*

*Teaching Assistant: Xudong Ren* 

Full-text + Author-released data & STATA programs available at: https://www.aeaweb.org/articles?id=10.1257/aer.104.10.3038

Scripts written in R with reference to the original scripts in STATA posted by the authors.

---

Basic Environment:

*R version: 4.0.2* 

Libraries used (difference among previous or newer versions of packages may exist):

``` R
library(foreign) # ver 0.8.81
library(ggplot2) # ver 3.3.5
library(dplyr) # ver 1.0.7
library(biostat3) # ver 0.1.6
library(lubridate) # ver 1.8.0
library(gridExtra) # ver 2.3
```

<small>Hint: You can check your package verison in R by `packageVersion("Name_of_the_package")`.</small>	

## Abstract

Inspections of environmental regulation effects in developing countries are important. Greenstone & Hanna (2014) took a deep insight into the performance of India’s air and water regulations, including the Supreme Court Action Plans (SCAP), the Catalytic Converter Policies (CAT), and the National River Conservation Plan (NRCP). The authors applied a difference-in-difference (DID) design using a two-stage econometric approach based on their organized city-level panel data for the years 1986-2007.  Results have shown the association between the catalytic converter policies and substantial improvement in air quality, while no measurable benefits have been found for water regulations. In addition, the authors determined a modest relationship between the decline in infant mortality rate and successful air regulations, though the relationships were not statistically significant. The parallel-trend assumption for DID was tested by Quandt Likelihood Ratio (QLR). We managed to use the R language to replicate their main findings and apply a preliminary DID approach to study the effects of the 2008 Economic Crisis on the effects of air policies. Only CAT was still likely to sustain their influence albeit disturbed by the crisis. Further research may take spatial insights and systematic homogeneity among cities in response to policies or events into consideration.

## Program Outline

`main.R` --> Run all codes in this file.

> --> 0.ENVIRONMENT SETUP & DATA IMPORT
> --> 1.MAIN PROGRAM
> --> 2.VISUALIZATION

-------> `main-functions.R` : all functions needed to replicate the data analysis/regression tasks for the main replication program

-------> `plot-functions.R` : all functions needed to visualize the results for the main replication program

-------> `extension-functions.R`: all functions needed to go over the preliminary extension task (of the 2008 Economic Crisis as an event)

<small> Note: we corrected an error in the code about two-way fix effects. The older version of `extension-functions.R` used to prepare our manuscript wrongly coded the `year` variable as dummies rather than the actual year. We were supposed to control the effects of each numerical year using `as.factor(year)`. This may result in a discrepancy between the result shown in the extension section (Table 10) of our original manuscript and the output of our updated code but should not affect the preliminary conclusion since the statistical significance of our results did not vary (only slight shift in regression estimates). (Dec 5, 2021, by Zhijie) </small>

## Reference
Greenstone, Michael, and Rema Hanna. 2014. "Environmental Regulations, Air and Water Pollution, and Infant Mortality in India." *American Economic Review*, 104 (10): 3038-72. DOI: 10.1257/aer.104.10.3038