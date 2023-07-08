# AnalyticCodebase
ReportingSystem

![Build Status Badge main](https://github.com/metrc/AnalyticCodebase/actions/workflows/build-main-releases.yml/badge.svg)

## Installation

```R
Sys.setenv(GITHUB_PAT = "TODO_ADD_YOUR_GITHUB_PAT");
devtools::install_github("metrc/AnalyticCodebase", upgrade="never", dependencies="Depends");
```

## Type
Package

## Title
METRC Redcap Analytic Codebase

## Version
0.3.1

## Author
Elias Weston-Farber

## Maintainer
Elias Weston-Farber <eweston4@jhu.edu>

## Description

### Data Analysis Skill Test

In this task, your goal is to create a function that adds a column named complications_number to the analytic dataset.

1. Clone this repository and set your working directory to the repository root.

2. Run the script weight_bearing_analytic_env.R. This will install the necessary packages and set up the Weight Bearing project.

3. Copy the 'weight_bearing_data.feather' file into the 'skills_test/weight bearing-cached_data' folder. Do NOT commit this file to the repository.

4. Create an R function pasteur_complications_number() that calculates the number of complications for each study_id. This function should be written from scratch and integrated at the suitable spot within a .R file in the R folder.

5. Save your visualization code in the 'skills test' directory. Generate one static and one interactive visualization using any subset of data you prefer.

6. Once your function is complete, ensure it runs smoothly when the 'weight_bearing_analytic_env.R' file is sourced.

7. Update this README with a description of your process and decisions made.

8. Draft an email to a hypothetical boss, summarizing your accomplishments and put that at the bottom of the README.

While you work on this task, make sure to refer to the documentation for the AnalyticCodebase package and the Analytic System. It's good practice to seek advice or clarifications when the documentation does not cover your questions. Remember, the purpose of this test is not just to assess your coding skills, but also your problem-solving ability and how you leverage available resources.

## Depends:
  AnalyticSystem,
  tidyverse,
  httr,
  feather,
  googlesheets4,
  igraph,
  writexl

## Encoding
UTF-8

## LazyData
true

## RoxygenNote
7.1.1
