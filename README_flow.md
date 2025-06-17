### Flow Statistics

Urban BMP monitoring studies typically generate runoff hydrographs where flow is measured at 1-15 min intervals. Runoff volume is determined from the integral of the hydrograph; this accomplished by using a numerical approximation to calculate the area under the hydrograph curve. In this application, area under the curve between two known flow measurements (i.e., the runoff volume for the interval) is computed using a trapezoid. The cumulative runoff volume is determined from the sum of the areas of each trapezoid.

$$ V=\int_0^t Q dt \approx \sum_{n=1}^i \frac{(Q_i+Q_{i-1})}{2}(t_i-t_{i-1}) $$

<div align="right"> 
Equation 1
</div>

Where V = runoff volume $(length^3)$, $Q_i$ = flow rate $(length^3/time)$ recorded at time $t_i$, and t = time.

The runoff duration is the elapsed time between the beginning and end of the runoff hydrograph. The beginning and end of the hydrograph are currently user-specified timestamps. 

The peak flow is the maximum flow rate reported over a 5-min period. If data are reported in less than 5-min intervals, the peak flow is determined as the maximum 5-min moving average .

$$ Q_p=max(Q_i )$$
<div align="right"> 
Equation 2
</div>

Where $Q_p$ = peak flow $(length^3/time)$. 

Hydrologic mitigation or alteration provided by BMPs is often reported as a percent change in characteristic (flow rate or volume) between the inflow and the outflow, as illustrated for the simplest data collection scenario in

<p align="center">
  <img src="https://user-images.githubusercontent.com/55409702/228071181-d4008432-2b9e-42f7-a9a6-4744d9239f1b.png" />
</p>

$$\text{Figure 1. Typical, simple BMP hydrologic measurement scenario.}$$
<div align="right"> 
Equation 3
</div>

The associated calculation is:

$$ \text{percent change} = \frac{\text{(Inflow value-Outflow value)}}{\text{(Inflow value)}} × 100% $$		


Where the “value” is either $Q_p$ or V determined by the calculator. Inflow and outflow hydrographs must be provided by the user in order to calculate % change.

The web app can calculate a % change in volume during events where runoff bypass or overflow occur, if the relevant hydrograph is provided. The terms “bypass” and “overflow” are used interchangeably here, indicating flow that is not managed by the BMP. Physically this may be flow that exceeds the inlet capacity and is routed around the BMP, or it may enter the BMP but flow directly to the outlet structure because the BMP capture capacity is exceeded (Figure 1). Either physical case is treated the same way for calculating the volume change statistic:

<p align="center">
  <img src="https://user-images.githubusercontent.com/55409702/228077002-427ef5b0-dc90-4b0e-9f92-644f408d2a77.png" />
</p>

$$\text{Figure 2. Typical BMP hydrologic measurement scenario with bypass.}$$

The associated calculation is:

$$ \text{percent change} = \frac{(V_{in}+V_{byp}-V_{out})}{(V_{in} + V_{byp})}  \times 100 \% $$
<div align="right"> 
Equation 4
</div>

The web app can also calculate a % change in volume if there is more than one inflow to the BMP,  including bypass (Equation 5), as illustrated in Figure 2. 

<p align="center">
  <img src="https://user-images.githubusercontent.com/55409702/229169339-5514e028-4ab6-46d8-b36e-2c6da329d17f.png" />
</p>

$$\text{Figure 3. BMP hydrology with multiple inlets and bypass}$$

The associated calculation is:

$$ \text{percent change} = \frac{(V_{in1}+V_{in2}+V_{byp} - V_{out})}{(V_{in1}+V_{in2}+V_{byp})} \times 100\% $$ 
<div align="right"> 
Equation 5
</div>







