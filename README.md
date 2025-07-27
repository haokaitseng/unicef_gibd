# unicef_gibd
An exploratory analysis on UNICEF Global Immunization Budget Database
- URL: https://gibd.unicef.org/dashboards
- Date: 2025-07-27
- Author: Hao-Kai Tseng

## Rationale:
This project aims to analyse the links from GIBD general immunisation budget changes to WUENIC immunisation coverages changes for 5 antigens (MCV, DTP, BCG, PCV, polio). LMICs in 2021-2024 are fitted to predict the coverage for 2025.

## Method: 
Bayesian temporal models are fitted using the Integrated Nested Laplace Approximation (INLA) framework, assuming a Gaussian distribution and a random walk of order 1 for the year as a random effect. Penalized complexity (PC) priors are implemented to control model complexity.

## Main results/conclusion:  
- A significant association between budget increase to coverage improvement is identified only for DTPCV1 (posterior mean: 0.42%, 95% CrI: 0.02% to 0.82%). 
- The 2025 outlook for DTPCV1 coverage in Kenya, Madagascar, Mali, Togo, and Zambia is an average increase of 0.5% (95% CrI: -4.6% to 5.5%), corresponding to an expected average coverage of 88.6% (95% CrI: 84.2% to 93.0%).
- Unlike lagging expenditure data, budget data are typically available prior to programme implementation, making GIBD a valuable tool for early coverage outlooks

## Dissemination: 
This project is set to be submitted to the [2025 Taiwan Public Health Joint Annual Conference](https://www.publichealth.org.tw/news_detail.asp?CateName=%E6%B4%BB%E5%8B%95%E8%88%87%E7%A0%94%E8%A8%8E%E6%9C%83&CateID=9&NewsID=1035) for oral presentation in the subject of Global Health, titled: 
> A Budget Impact Analysis on Immunisation Coverage: Evidence from UNICEF Global Immunization Budget Database in LMICs (2021–2025)