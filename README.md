priceT

**Introduction:**
Temperature derivatives are financial contracts whose payoffs depend on the temperatures at specific stations over certain periods of time. Currently, there are three main types of temperature derivatives, comprising Cooling Degree Days (CDD), Heating Degree Days (HDD) and Cumulative Average Temperature (CAT) futures contracts, traded at the Chicago Mercantile Exchange (CME) and a total of $13$ measuring stations in the United States. The three temperature indices are defined as follow: 
$$\text{CDD}(\tau_1,\tau_2)=\sum_{i=\tau_1}^{\tau_2}\max(T_i-c,0),\text{HDD}(\tau_1,\tau_2)=\sum_{i=\tau_1}^{\tau_2}\max(c-T_i,0),\text{and}\text{CAT}(\tau_1,\tau_2)=\sum_{i=\tau_1}^{\tau_2}T_i,$$
where $T_i$ denotes the mean of the maximum and minimum temperatures on day $i$, and $\tau_1$ and $\tau_2$ denote the start and end dates of the measurement period respectively. The CDD over the measurement period is the accumulated temperatures above the threshold $c$, which is usually taken as $c=65^{\circ}\text{F}$, serving as an indicator of the need for cooling, whereas the HDD indicates the need for heating. For the stations in the US, CME mainly organizes trades for monthly temperature futures. CDD contracts are only traded for the summer season from April to October whereas HDD contracts are only traded for the winter season from October to April. Existing approaches on the pricing of temperature derivatives focus on the use of continuous-time ARMA (CARMA) processes and Brownian-driven Ornstein-Uhlenbeck (OU) dynamics with univariate seasonal volatility for modeling the deseasonalized temperatures at a particular site. In contrast with these two existing methods that focus on estimating univariate seasonal volatility and isolating Gaussian risk factors, we develop multivariate non-Gaussian pricing methods where the evolutions of the temperatures at different stations over time are described by spatio-temporal models.

**Functionality:**
The main purpose of the package is to compute the price of a temperature futures using different methods based on the station (location), measurement period (time), which will be a particular month, and the type of contract (CDD, HDD or CAT) inputted. In the package priceT, the following functions are implemented: 
1. seasonal: returns the estimated coefficients of the seasonality function. 
2. Fourier: returns the price of a temperature futures using the truncated Fourier series method. 
3. adaptBW: returns the price of a temperature futures using the adaptive bandwidth method. 
4. MLSS_spacetime: returns the price of a temperature futures using multivariate non-Gaussian spatio-temporal models. 
5. calib: returns the calibrated risk-neutral parameters and in-sample pricing errors based on the market prices. 
6. diagnostics: for simple preliminary tests on the temperatures data, returns test statistics and $p$-values from tests such as the Kolmogorov-Smirnov test, and visual outputs such as Q-Q plots and kernel density estimates. 
7. temp_forecast: forecasting the temperatures in the future after fitting the historical data to the spatio-temporal models, both point forecasts and confidence intervals will be considered, graphs will be plotted and returned. 
8. temp_hedge: computes the hedge ratio, which is the number of contracts needed in order to create a perfect hedge for the temperature risk at a particular station over a specified measurement period. \\
9. loc_hedge: creates an optimal portfolio for hedging the temperature risk at a new location using the contracts traded based on other locations and returns the optimal weights. 
In addition, datasets for the past temperatures and deseasonalized temperatures at different stations are provided. 

**Installation instructions:**

**Tasks:**
The completed tasks include adding and documenting datasets, and the function seasonal has been completed with proper documentation. 
I have started to work on several functions, including their codes and documentations, but still have not completed them. These functions include Fourier, adaptBW, calib and diagnostics. 
For the remainder of the term, I need to complete the following:
1. Add the functions MLSS_spacetime, temp_forecast, temp_hedge and loc_hedge, including their documentations. 
2. Move some of the codes to C++
3. Make vignette
4. Extensively test all the functions and create testing directory with code coverage
