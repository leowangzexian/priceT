**Package priceT**

**Introduction:**
Temperature derivatives are financial contracts whose payoffs depend on the temperatures at specific stations over certain periods of time. Currently, there are three main types of temperature derivatives, comprising Cooling Degree Days (CDD), Heating Degree Days (HDD) and Cumulative Average Temperature (CAT) futures contracts, traded at the Chicago Mercantile Exchange (CME) and a total of $13$ measuring stations in the United States. The three temperature indices are defined as follow: 
$$\text{CDD}(\tau_1,\tau_2)=\sum_{i=\tau_1}^{\tau_2}\max(T_i-c,0)$$, $$\text{HDD}(\tau_1,\tau_2)=\sum_{i=\tau_1}^{\tau_2}\max(c-T_i,0)$$, and $$\text{CAT}(\tau_1,\tau_2)=\sum_{i=\tau_1}^{\tau_2}T_i$$, 
where $T_i$ denotes the mean of the maximum and minimum temperatures on day $i$, and $\tau_1$ and $\tau_2$ denote the start and end dates of the measurement period respectively. The CDD over the measurement period is the accumulated temperatures above the threshold $c$, which is usually taken as $c=65^{\circ}\text{F}$, serving as an indicator of the need for cooling, whereas the HDD indicates the need for heating. For the stations in the US, CME mainly organizes trades for monthly temperature futures. CDD contracts are only traded for the summer season from April to October whereas HDD contracts are only traded for the winter season from October to April. Existing approaches on the pricing of temperature derivatives focus on the use of continuous-time ARMA (CARMA) processes and Brownian-driven Ornstein-Uhlenbeck (OU) dynamics with univariate seasonal volatility for modeling the deseasonalized temperatures at a particular site. In contrast with existing methods that focus on estimating univariate seasonal volatility and isolating Gaussian risk factors, we develop efficient pricing methods with analytical pricing formulae based on multivariate seasonal volatility, which can be modeled and estimated by three methods, truncated Fourier series, adaptive bandwidth selection and non-Gaussian spatio-temporal random fields describing the evolutions of the temperatures at different stations over time. 
The package focuses on the calibration and computation of the futures prices as well as other relevant techniques such as temperature modeling and forecasting, and the design of hedging strategies for managing temperature risk.

**Functionality:**
The main purpose of the package is to compute the price of a temperature futures using different methods based on the station (location), measurement period (time), which will be a particular month, and the type of contract (CDD, HDD or CAT) inputted. In the package priceT, the following functions are implemented: 
1. seasonal: returns the estimated coefficients of the seasonality function. 
2. Fourier: returns the price of a temperature futures using the truncated Fourier series method. 
3. adaptBW: returns the price of a temperature futures using the adaptive bandwidth method. 
4. MLSS_spacetime: returns the price of a temperature futures using multivariate non-Gaussian spatio-temporal models. 
5. calib: returns the calibrated risk-neutral parameters and in-sample pricing errors based on the market prices. 
6. diagnostics: for simple preliminary tests on the deseasonalized temperatures data, returns the test statistic and $p$-value from the Kolmogorov-Smirnov normality test, and visual outputs including the Q-Q plot against normal distribution, Q-Q plot against the generalised hyperbolic distribution and the kernel density estimate. 
7. temp_forecast: forecasting the temperatures in the future after fitting the historical data to the spatio-temporal models, both point forecasts and confidence intervals are returned. 
8. temp_hedge: computes the hedge ratio, which is the number of contracts needed in order to create a perfect hedge for the temperature risk at a particular station over a specified measurement period. 
9. loc_hedge: creates an optimal portfolio for hedging the temperature risk at a new location using the contracts traded based on other locations and returns the optimal weights. 
10. seasonal_c: returns the estimated coefficients of the seasonality function using C++.
11. Fourier_c: returns the price of a temperature futures using the truncated Fourier series method via C++.
12. calib_c: returns the calibrated risk-neutral parameters and in-sample pricing errors based on the market prices using C++.

In addition, five datasets are provided:
1. tempstations: containing the information of the 225 stations, including their IDs, names of their locations, countries (either US or Canada), states located in, latitudes, longitudes and altitudes. 
2. temp0: containing the daily mean temperatures of the 225 stations from 2017 to 2021.
3. seasonal_coefs: containing the 4 coefficients of the fitted seasonal functions for each of the 225 stations. 
4. residuals: containing the daily deseasonalized temperatures of the 225 stations from 2020 to 2021.
5. sresids: containing the daily standardized residuals of the 225 stations from 2020 to 2021 after fitting vector AR models to the deseasonalized temperatures.

Vignette and testing directory with code coverage are also provided. 

**Installation instructions:**
The package priceT can be installed from GitHub directly. To install the package, run `devtools::install_github("leowangzexian/priceT")` in R. 
To install the package with the vignette, run `devtools::install_github("leowangzexian/priceT", build_vignette = TRUE)` in R.

**Examples:**
We demonstrate the detailed usage of some functions with relevant examples. 

1. seasonal  

The input is:  
temp = A n by 1 vector containing the past temperatures data at one station over (n / 365) years  

The outputs are:  
a = a scalar that is the value of the coefficient a in the seasonal function  
b = a scalar that is the value of the coefficient b in the seasonal function  
c = a scalar that is the value of the coefficient c in the seasonal function  
d = a scalar that is the value of the coefficient d in the seasonal function  
seasonality = n by 1 vector containing the values of the seasonal function  
plt = plot of the temperatures and the seasonal function over the time indices  

For the illustrative example, we study the temperatures data and seasonality function of Atlanta.
```{r}
# load the temp0 data from temp0.rda in the data folder
# example 1
temp = as.numeric(temp0[, 3]) # historical temperatures at one station
seasonal1 = seasonal(temp)

# examine results
c(seasonal1$a, seasonal1$b, seasonal1$c, seasonal1$d) # coefficients
head(seasonal1$seasonality) # fitted values of the seasonal function

# > c(seasonal1$a, seasonal1$b, seasonal1$c, seasonal1$d) # coefficients
# [1]  6.524117e+01 -3.922853e-04 -1.708097e+01  1.755952e+01
# > head(seasonal1$seasonality) # fitted values of the seasonal function
# [1] 48.84910 48.76846 48.69269 48.62183 48.55589 48.49490
```

2. Fourier  

The inputs are:  
residuals = A n by p matrix containing the past deseasonalized temperatures data at p stations over (n / 365) years (n >= 365 * 2) (p >= 3)  
station = A numeric index denoting the specific station that the price of the temperature futures depends on  
start_ind = A numeric index for the start of the measurement period  
end_ind = A numeric index for the end of the measurement period  
type = A string denoting the type of the temperature futures, either CDD, HDD or CAT  
seasonal_coefs = A 1 by 4 vector containing the coefficients of the seasonality function at the station to be priced  

The outputs are:  
price = a scalar that is the price of the derivative computed  
sresids = n by p matrix containing the standardized residuals computed  
plt = plot of the empirical and fitted seasonal variances at the station priced  

For the illustrative example, we use the deseasonalized temperatures from three stations, Atlanta, Boston and Burbank, for multivariate modeling.  
```{r}
# load the residuals data from residuals.rda in the data folder
# load the seasonal coefficients data from seasonal_coefs.rda in the data folder

# example 1
residuals1 = matrix(as.numeric(residuals[, 3:5]), 730, 3)
seasonal_coefs1 = as.numeric(seasonal_coefs[1, 2:5])
station11 = 1
start_ind11 = 1
end_ind11 = 31
type11 = "HDD"
Fourier11 = Fourier(residuals1, station11, start_ind11, end_ind11, type11, seasonal_coefs1)
station12 = 1
start_ind12 = 182
end_ind12 = 212
type12 = "CDD"
Fourier12 = Fourier(residuals1, station12, start_ind12, end_ind12, type12, seasonal_coefs1)

# examine results
Fourier11$price # HDD price
Fourier12$price # CDD price
head(Fourier11$sresids) # standardised residuals
head(Fourier12$sresids) # standardised residuals

# > Fourier11$price # HDD price
# [1] 575.247
# > Fourier12$price # CDD price
# [1] 631.0655
# > head(Fourier11$sresids) # standardised residuals
#             [,1]        [,2]        [,3]
# [1,]  0.30134456 -0.34755029  0.26944183
# [2,]  0.05274071  0.23208378 -0.23645609
# [3,] -0.28476533 -0.25788300 -0.09051341
# [4,] -0.18606411  0.08222787 -0.06924198
# [5,] -0.18412655 -0.15213328  0.10192403
# [6,]  0.23271495 -0.02893035  0.13130861
# > head(Fourier12$sresids) # standardised residuals
#             [,1]        [,2]        [,3]
# [1,]  0.30134456 -0.34755029  0.26944183
# [2,]  0.05274071  0.23208378 -0.23645609
# [3,] -0.28476533 -0.25788300 -0.09051341
# [4,] -0.18606411  0.08222787 -0.06924198
# [5,] -0.18412655 -0.15213328  0.10192403
# [6,]  0.23271495 -0.02893035  0.13130861
```

From the outputs, we can see that the computed price of a January HDD futures based on Atlanta is $575.247$ and the computed price of a July CDD futures based on Atlanta is $631.0655$.  


4. MLSS_spacetime  

The inputs are:  
residuals = A n by p matrix containing the past deseasonalized temperatures data at p stations over (n / 365) years (n >= 365 * 2) (p >= 3)  
station = A numeric index denoting the specific station that the price of the temperature futures depends on  
start_ind = A numeric index for the start of the measurement period  
end_ind = A numeric index for the end of the measurement period  
type = A string denoting the type of the temperature futures, either CDD, HDD or CAT  
seasonal_coefs = A 1 by 4 vector containing the coefficients of the seasonality function at the station to be priced  

The outputs are:  
price = a scalar that is the price of the derivative computed  
mu = 1 by p vector containing the parameter mu of the fitted NIG distribution for the standardized residuals  

For the illustrative example, we use the deseasonalized temperatures from three stations, Atlanta, Boston and Burbank, for multivariate modeling.
```{r}
# load the residuals data from residuals.rda in the data folder
# load the seasonal coefficients data from seasonal_coefs.rda in the data folder

# example 1
residuals1 = matrix(as.numeric(residuals[, 3:5]), 730, 3)
seasonal_coefs1 = as.numeric(seasonal_coefs[1, 2:5])
station11 = 1
start_ind11 = 1
end_ind11 = 31
type11 = "HDD"
MLSS_spacetime11 = MLSS_spacetime(residuals1, station11, start_ind11,
                                  end_ind11, type11, seasonal_coefs1)
station12 = 1
start_ind12 = 182
end_ind12 = 212
type12 = "CDD"
MLSS_spacetime12 = MLSS_spacetime(residuals1, station12, start_ind12,
                                  end_ind12, type12, seasonal_coefs1)

# examine results
MLSS_spacetime11$price # HDD price
MLSS_spacetime12$price # CDD price
MLSS_spacetime11$mu # parameter mu of NIG random field
MLSS_spacetime12$mu # parameter mu of NIG random field

# > MLSS_spacetime11$price # HDD price
# [1] 573.9523
# > MLSS_spacetime12$price # CDD price
# [1] 581.2839
# > MLSS_spacetime11$mu # parameter mu of NIG random field
# [1]  0.015877004 -0.008273250 -0.009076429
# > MLSS_spacetime12$mu # parameter mu of NIG random field
# [1]  0.015877004 -0.008273250 -0.009076429
```

From the outputs, we can see that the computed price of a January HDD futures based on Atlanta is $573.9523$ and the computed price of a July CDD futures based on Atlanta is $581.2839$.  


5. calib  

The inputs are:  
market_price = A scalar that is the actual futures prices traded in the market  
residuals = A n by p matrix containing the past deseasonalized temperatures data at p stations over (n / 365) years (n >= 365 * 2) (p >= 3)  
station = A numeric index denoting the specific station that the price of the temperature futures depends on  
start_ind = A numeric index for the start of the measurement period  
end_ind = A numeric index for the end of the measurement period  
type = A string denoting the type of the temperature futures, either CDD, HDD or CAT  
seasonal_coefs = A 1 by 4 vector containing the coefficients of the seasonality function at the station to be priced  
func = A function to compute the futures price, either Fourier, adaptBW or MLSS_spacetime  

The outputs are:  
theta = a scalar that is the calibrated risk-neutral parameter   
error = a scalar that is the in-sample pricing error  

Previously, we have computed the prices of July CDD futures based on Atlanta. Now, suppose we know that the price of July CDD futures traded in the market is $600$. We can then calibrate the risk-neutral parameter.  

```{r}
# load the residuals data from residuals.rda in the data folder
# load the seasonal coefficients data from seasonal_coefs.rda in the data folder

# example 1
residuals1 = matrix(as.numeric(residuals[, 3:5]), 730, 3)
seasonal_coefs1 = as.numeric(seasonal_coefs[1, 2:5])
station11 = 1
start_ind11 = 182
end_ind11 = 212
type11 = "CDD"
market_price1 = 600
calib11 = calib(market_price1, residuals1, station11, start_ind11,
                end_ind11, type11, seasonal_coefs1, Fourier)
calib12 = calib(market_price1, residuals1, station11, start_ind11,
                end_ind11, type11, seasonal_coefs1, adaptBW)
calib13 = calib(market_price1, residuals1, station11, start_ind11,
                end_ind11, type11, seasonal_coefs1, MLSS_spacetime)

# examine results
calib11$theta # calibrated risk-neutral parameter when using Fourier
calib12$theta # calibrated risk-neutral parameter when using adaptBW
calib13$theta # calibrated risk-neutral parameter when using MLSS_sapcetime
calib11$error # in-sample pricing error when using Fourier
calib12$error # in-sample pricing error when using adaptBW
calib13$error # in-sample pricing error when using MLSS_sapcetime

# > calib11$theta # calibrated risk-neutral parameter when using Fourier
# [1] 0.9507729
# > calib12$theta # calibrated risk-neutral parameter when using adaptBW
# [1] 1.000805
# > calib13$theta # calibrated risk-neutral parameter when using MLSS_sapcetime
# [1] 1.032198
# > calib11$error # in-sample pricing error when using Fourier
# [1] 0
# > calib12$error # in-sample pricing error when using adaptBW
# [1] 0
# > calib13$error # in-sample pricing error when using MLSS_sapcetime
# [1] 9.701277e-12
```

From the results, we can see that the risk-neutral parameter calibrated is $0.9507729$ when using the truncated Fourier series method to estimate seasonal volatility with in-sample pricing error of $0$ and the risk-neutral parameter calibrated is $1.032198$ when using non-Gaussian spatio-temporal random fields to estimate seasonal volatility with in-sample pricing error of close to $0$.  

For a wider variety of illustrative examples including visual outputs and the details regarding other functions in the package, you may refer to the vignette provided.  
