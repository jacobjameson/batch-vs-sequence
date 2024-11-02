# Variation in batch ordering of imaging tests in the emergency department and the impact on care delivery

[![DOI](https://zenodo.org/badge/661395359.svg)](https://doi.org/10.5281/zenodo.14029337)


## Overview
This study examines heterogeneity in physician batch ordering practices and their impact on patient outcomes and resource utilization in emergency departments (ED). We analyze how different diagnostic test ordering strategies - batch ordering (multiple tests at once) versus sequential ordering - affect key healthcare metrics using comprehensive EMR data from Mayo Clinic of Arizona ED.

## Data
- **Source**: Mayo Clinic of Arizona Emergency Department
- **Period**: October 6, 2018 - December 31, 2019
- **Sample**: 43,299 patient encounters from 50,836 total visits
- **Assignment**: Random physician allocation to patients

## Key Outcomes
1. **Length of Stay (LOS)**
   - Physicians with batch tendency 1SD above average associated with 4.5% increase in ED LOS (p < 0.001)

2. **72-Hour Returns**
   - 14.8% decrease (0.2 percentage points) in 72-hour return with admission for high-batch physicians
   - Suggests more comprehensive initial evaluations

3. **Resource Utilization**
   - Additional 8 imaging tests per 100 patient encounters for high-batch physicians
   - Indicates potential over-ordering in batch approach

## Methods
- Retrospective analysis using comprehensive EMR data
- Multivariable linear regression controlling for various covariates
- Balance testing for random assignment
- Subgroup analysis across different clinical conditions

## Conclusions
The study demonstrates significant associations between physician ordering strategies and ED outcomes:
- Trade-off between longer initial visits and reduced return rates
- Potential over-utilization of imaging resources with batch ordering
- Need for optimized guidelines for ED test ordering practices

## Replication
The analysis code in this repository allows for:
- Data processing and cleaning
- Statistical analysis and model estimation
- Generation of figures and tables
- Robustness checks and sensitivity analyses

## Contact
For questions about this research or repository:
- **Author**: Jacob Jameson
- **Email**: jacobjameson@g.harvard.edu

## Citation
If you use this code in your research, please cite:
[Citation information to be added upon publication]
