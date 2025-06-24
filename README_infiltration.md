A decay rate fits the recession limb of ponded water depth in a structural BMP after a storm event is used to calculate infiltration rate. Depth data, collected using a water level sensor or piezometer, is processed in the following steps:

1\. Data smoothing:

To reduce noise and enhance the clarity of trends in the depth data, a median filter is applied to depth data using Equation 1:

$$
y_i = \text{median}(y_{i-j}, \ldots, y_i, \ldots, y_{i+j})
$$

<div align="right"> 
Equation 1
</div>

where $j$ is the smoothing window size, set at 15 min by default; $y_i$ is the smoothed depth data. 

**Note**: the smoothing window is based on a fixed duration of 15 min, rather than a fixed number of data points, e.g., when the depth data are in 3-minute increments (instead of 1-minute), $y_i$ is the median of 5 points (instead of 15).

2\. Data fitting:

A moving window with a default duration of 720 min (12 h) is applied to the smoothed depth data. Within this moving window, the SciPy CurveFit package is used to fit an exponential decay model (Equation 2), with the coefficient of determination ($R^2$) used as the goodness-of-fit metric for the regression model to the depth data. 

$$
y(t) = y_0 e^{-kt}
$$

<div align="right"> 
Equation 2
</div>

where $y(t)$ is the depth at time t, $y_0$ is the initial depth, and $k$ is the rate constant ($time^{-1}$).

The best fit is selected as the maximum $R^2$ achieved by fitting the exponential decay model to the smoothed depth data. This way, the best fit regression is guaranteed to be centered on a time window of the depth data that maximizes the regression limb. 

**Note**: A high goodness-of-fit threshold is enforced ($R^2$ > 0.999) to ensure that only the regression limb is considered. If the whole timeseries has been scanned with the 12 h moving window and a suitable model cannot be found, the window size is reduced by 1 h until a suitable fit is found. The minimum window size for a regression limb is 1 h.


<p align="center">
  <img src="https://github.com/user-attachments/assets/86a6f365-ed3a-4f40-a553-d76a8a77e64b"
       style="max-width: 900px; max-height: 600px; height: auto; width: auto;">
</p>


3\. Infiltration rate calculation: 
Once the best-fitting time window is identified, the **infiltration rate** (length/time) is calculated using Equation 3, based on the rate constant $k$ and the average depth within the selected window (from step 2).

$$Infiltration \ Rate = k \cdot y_{ave}$$  			

<div align="right"> 
Equation 3
</div>

The **average depth** is computed as:

$$
y_{ave} = \frac{1}{n} \sum_{i=1}^{n} y_i
$$	

<div align="right"> 
Equation 4
</div>

where $y_{ave}$ is the average depth within the selected time window, $y_i$ represents individual smoothed depth values within the window, and $n$ is the number of data points in the window.

SCCWRP is grateful to Matthew McGauley from Villanova University for providing
foundational logic for the infiltration calculator.
