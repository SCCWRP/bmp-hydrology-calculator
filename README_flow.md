Urban BMP monitoring studies typically generate runoff hydrographs where flow is measured at 1-15 min intervals.

The runoff duration (time) is the elapsed time between the start and end of the runoff hydrograph. The start and end of the hydrograph can be user-specified from the uploaded timestamps.

Runoff volume is determined by integrating the hydrograph using a numerical approximation to calculate the area under the curve. In this application, the area between two consecutive measurements (i.e., the runoff volume for that interval) is computed using a trapezoid; The cumulative runoff volume is then determined by summing the areas of all trapezoids across the hydrograph.

$$ V=\int_0^t Q dt \approx \sum_{n=1}^i \frac{(Q_i+Q_{i-1})}{2}(t_i-t_{i-1}) $$

<div align="right"> 
Equation 1
</div>

where $V$ is the runoff volume ($length^3$); $Q_i$ is the flow rate ($length^3$/time) recorded at time $t_i$; $t_i$ represents the timestamps, with $i$ (ranging from $1$ to $n$) denoting the index of each recorded values within the dataset or the user defined period of interest.

Peak flow is determined as the maximum flow rate observed over a 5-min period. If data are reported at intervals shorter than 5 min, a 5 min moving average is applied to smooth the flow data; if the data are reported at intervals longer than 5 min, interpolation is used to estimate flow rates at 5 min intervals for consistent peak flow determination.

$$ Q_p=max(Q_i )$$
<div align="right"> 
Equation 2
</div>

where $𝑄_{𝑝}$ represents the peak flow rate during the rain event ($length^3$/time), $𝑄_j$ ($length^3$/time) represents the flow rate calculated over 5 min intervals ($𝑡_{𝑗+1}$ − $𝑡_𝑗$=5 min).

Hydrologic mitigation or alteration provided by BMPs is often reported as the percent change in a characteristic (flow rate or volume) between the inflow and the outflow. This is illustrated in figures below for the simplest data collection scenario:

<p align="center">
  <img src="https://user-images.githubusercontent.com/55409702/228071181-d4008432-2b9e-42f7-a9a6-4744d9239f1b.png" />
</p>

$$\text{Figure 1. Typical, simple BMP hydrologic measurement scenario.}$$
<div align="right"> 
Equation 3
</div>

If runoff bypass or overflow occurs and the relevant hydrograph is provided (Figure 2):

<p align="center">
  <img src="https://user-images.githubusercontent.com/55409702/228077002-427ef5b0-dc90-4b0e-9f92-644f408d2a77.png" />
</p>

$$\text{Figure 2. Typical BMP hydrologic measurement scenario with bypass.}$$

The terms “bypass” and “overflow” are used interchangeably to refer to flow that is not managed by the BMP. Physically, this may include flow that exceeds the inlet capacity and is routed around the BMP, or flow that enters the BMP but is conveyed directly to the outlet structure because the system’s capture capacity is exceeded (Figure 2).

$$ \text{percent change} = \frac{(V_{in}+V_{byp}-V_{out})}{(V_{in} + V_{byp})}  \times 100 \% $$
<div align="right"> 
Equation 4
</div>

If there is more than one inflow to the BMP, including bypass (Figure 3):

<p align="center">
  <img src="https://user-images.githubusercontent.com/55409702/229169339-5514e028-4ab6-46d8-b36e-2c6da329d17f.png" />
</p>

$$\text{Figure 3. BMP hydrology with multiple inlets and bypass}$$


$$ \text{percent change} = \frac{(V_{in1}+V_{in2}+V_{byp} - V_{out})}{(V_{in1}+V_{in2}+V_{byp})} \times 100\% $$ 
<div align="right"> 
Equation 5
</div>

