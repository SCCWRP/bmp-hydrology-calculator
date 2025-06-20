The total rainfall is the cumulative precipitation depth measured for each event. The average rainfall intensity is the ratio of the total rainfall to the total rainfall duration. The peak 5-min, 10-min, or 60 min rainfall intensity is given by the maximum depth of rainfall over any 5-min, 10-min, or 60 min interval, respectively.

Precipitation statistics are calculated as: <br>

$$ P= \sum_{j=1}^{n}   P_j  $$
<div align="right"> 
Equation 1
</div>

where $P$ represents the **total rainfall** during an event (length, in or mm), and $P_j$ is each recorded rain depth (length), $j$ (ranging from 1 to $n$) denotes the index of each recorded values by the rain gauge within the rain event.

The first tip for an event is defined as the first non-zero value either at the beginning of the uploaded dataset or following a 12-hour period with no rainfall (i.e., rain depth values are zero for 12 hours). The last tip is defined as the final non-zero value before the next 12-hour dry period or the last non-zero value at the end of the dataset. 

$$i_{ave}=\frac{P}{t_P}=\frac{P}{t_n - t_1} $$ 
<div align="right"> 
Equation 2
</div>

where $i_{ave}$ represents the **average rainfall intensity** over the rainfall duration (length/time); $t_P$ (time) is the rainfall duration, determined as the elapsed time between the first tip ($t_1$) and the last tip ($t_n$) of the rain event. 

$$peak \ i_{∆t}=\max \left( \frac{\sum\limits_{t_j}^{(t_j + ∆t)} P_i}{∆t} \right)$$
<div align="right"> 
Equation 3
</div>

where $peak \ i⁡_{∆t}$ represents the **peak ∆t rainfall intensity** (length/time), calculated as the maximum rainfall intensity value of over a moving average over a window of duration ∆t (time), e.g., **5 min**, **10 min**, or **60 min**, for the rainfall event. For example, $peak \ i_{5min}$ is the maximum 5 min average rainfall intensity averaged over any 5 min interval observed during the entire rainfall duration.

The **antecedent dry period** (time) is the elapsed time between the last rain gauge tip of an event and the first rain gauge tip of the subsequent event. This value is not calculated for the first rainfall event in the uploaded dataset.
