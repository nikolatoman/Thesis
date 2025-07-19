 # Thesis
Master Thesis - Application of Extreme Value Theory to Estimating Selected Risk Measures in Financial Time Series.

## Abstract of the thesis
The master thesis focuses on the application of Extreme Value Theory (EVT) to the estimation of Value at Risk (VaR) and Expected Shortfall (ES) of a conservative and dynamic investment portfolio. The block maxima method and the peak over threshold method were used to estimate the risk measures. The main objective of the thesis was to compare the resulting estimates with the results obtained by the historical simulation method and the variance-covariance method with the assumption of normality of the data. The individual estimates were evaluated and compared in backtesting. The paper applied the Kupiec's coverage test for VaR, the Costanzino-Curran test for ES and the Fissler-Ziegel loss function, which considers both risk measures together, to the estimates obtained. The main focus was on the resulting values of the Fissler-Ziegel loss function. By examining the values of the Fissler-Ziegel loss function, it was found that the non-declustered General Pareto models with an appropriately chosen threshold provide approximately as good or better estimates than the other methods examined. Therefore, non-declustered General Pareto models with an appropriately chosen threshold were evaluated as the most appropriate method for estimating the risk measures examined.

## Keywords
Expected Shortfall, Peaks Over Threshold Method, Extreme Value Theory, Block-Maxima Method, Fissler-Ziegel Loss Function, Value at Risk

# Content of this repository
This repository contains my thesis written in Czech language on the topic of application of extreme value theory to estimating selected risk measures in financial time series, see Tomancova_thesis.pdf. The thesis contains a theoretical part with the necessary theoretical background and an application part, the content of which is briefly described in the abstract above. In addition to the thesis, the repository also contains a several-page text summarizing the content and main conclusions of the thesis, see Tomancova_autoreferat.pdf. In the RScripts subfolder there are my own 7 scripts in R language that were used during the writing of the thesis.

## Brief content of the script files:
Tomancova_theoretical_examples.R with theoretical examples of VaR, ES and EVT, <br/> 
Tomancova_eda.R with getting the data, its preparation and exploratory data analysis, <br/> 
Tomancova_bmm.R with application of BMM method, <br/> 
Tomancova_thresholds.R with choosing correct thresholds fot POT method, <br/> 
Tomancova_pot.R with application of POT method, <br/> 
Tomancova_results_comparison.R with comparison of results with other VaR estimation methods, <br/> 
Tomancova_backtesting.R with backtesting of the results.
