# Optimal-Triage-COVID-19
Study of optimal triage for severe COVID-19 patients to minimize mortality rate.

This repository contains a source code for optimal triage of severe COVID-19 patients.

## Setup
Data processing and modeling were implemented in R.

### Dependencies

library(ggplot2)<br />
library(magrittr)<br />
library(caret)<br />
library(tidyverse)<br />
library(dplyr)<br />
library(xgboost)<br />
library(pROC)<br />
library(SHAPforxgboost)<br />
library(gridExtra)<br />
library(grid)<br />
library(EpiDynamics)<br />
library(reshape2)<br />
library(readxl)<br />
library(scales)<br />
library(simmer)

Function conflicts can be resolved by specifying the necessary package name before a function depending on its purpose <br />
(i.e. dplyr::select, simmer::select).

## Data Availability
Data supporting the main findings of this study are available from the corresponding author upon reasonable request. A portion of data is available within the Supplementary Information. The original dataset is not publicly available due to the confidential policy of the Korea Disease Control and Prevention Agency (KDCA).

## Authors
†Jeong Min Kim, †Hwa Kyung Lim, Jae-Hyeon Ahn, Kyung Hwa Lee, *Kwang Suk Lee, *Kyo Chul Koo

†Jeong Min Kim and Hwa Kyung Lim equally contributed as first authors<br />
*Kwang Suk Lee and Kyo Chul Koo equally contributed as corresponding authors

## Acknowledgments
We thank Korea Disease Control & Prevention Agency, National Medical Center, and the Health Information Manager in hospitals for their effort in collecting the medical records. This study was supported through the Infection Prevention Strategy Development Program of Korea (HW20C2103).
