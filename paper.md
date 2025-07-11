---
# title: "BMP Hydrology Calculator: A Web Tool for Analyzing Rainfall, Runoff, and Infiltration in Stormwater BMPs"
tags:
  - hydrology
  - stormwater
  - BMP monitoring
  - infiltration
  - web application
  - water resources
authors:
  - name: Edward Tiernan
    orcid: 0000-0001-7981-1495
    affiliation: 1
  - name: Danhui Xin
    orcid: 0000-0002-5267-9727
    affiliation: 1
  - name: Duy Nguyen
    orcid: 0009-0006-6597-9039
    affiliation: 1  
  - name: Nicholas Lombardo
    orcid: 0009-0000-2898-2577
    affiliation: 1 
  - name: Addison Torres Grant
    orcid:
    affiliation: 1
  - name: Elizabeth Fassman-Beck
    orcid: 0000-0001-7594-8881
    affiliation: 1 
affiliations:
  - name: Southern California Coastal Water Research Project
    index: 1
date: 2025-06-19
---

# BMP Hydrology Calculator: A Web Tool for Analyzing Rainfall, Runoff, and Infiltration in Stormwater BMPs

Edward Tiernan¹, Duy Nguyen¹, Danhui Xin¹, Nicholas Lombardo¹, Addison Torres Grant¹, Elizabeth Fassman-Beck¹

¹ Southern California Coastal Water Research Project (SCCWRP)

---

## Summary

Field-based performance studies of stormwater best managment practices (BMPs) generate large volumes of time series data from rain gauges, flow meters, and water level sensors [@davis_green_2022]. Practitioners often rely on ad hoc spreadsheets or inconsistent workflows for data reduction [@erickson_optimizing_2013]. This results in methodological variation across studies and limits reproducibility. 

**Statement of Need:** The BMP Hydrology Calculator fills this gap by providing a centralized, open-source platform for applying standard hydrologic post-processing techniques in a consistent and automated manner. This web application has been developed to enable consistent, transparent, and easily applied calculations of rainfall, flow, and infiltration for stormwater BMP monitoring studies.

The BMP Hydrology Calculator provides three types of analyses:

1.	**Rainfall Analysis** - Generates the cumulative rainfall depth, average rainfall intensity, rainfall duration, and the maximum rainfall intensity over multiple storm events based on a user-uploaded rainfall data (a.k.a. hyetograph).

2.	**Flow Analysis** - Calculates statistics from user-uploaded flowrate data (a.k.a. hydrographs) including total runoff volume, runoff duration, and the peak (maximum) flow rate. Multiple BMP configurations are supported, including when the BMP has monitored flow data from multiple inflows and/or bypass (a.k.a. "overflow"). 

3.	**Infiltration Analysis** - Calculates the infiltration rate of ponded water through a BMP based on water level depth measured over time. The calculation applies to water that moves across a soil-water interface, for example ponded runoff moving from the surface into a filtration media, or runoff stored in a subsurface BMP exfiltrating into the surrounding soil (e.g. the vadose zone). 

Each method applies a consistent set of calculations designed to mirror typical field monitoring workflows [@cook_npdess_1992]. This approach ensures that essential stormwater BMP metrics (e.g., rainfall intensity, runoff volume, infiltration rates) are derived using transparent and reproducible methods, all within a unified web-based environment. The underlying equations and methodological assumptions used in each analysis are documented in detail in the project's [README](https://github.com/SCCWRP/rainfall_flow_calculator_shiny) on GitHub. 

This web application is intended for technical users. The calculators process data obtained from sensors to generate summary statistics commonly used to provide context for and/or interpret stormwater BMP performance; this web application itself does not provide any interpretation. While the motivation for creating the calculator was intended to support field monitoring of stormwater BMPs, it may be applied for other hydrologic monitoring applications where rainfall, hydrograph, or infiltration analysis may be of interest. The user is responsible for raw data quality assurance. There are no checks of data quality built into the calculators, other than for missing data and formatting. The documentation herein does not provide guidance on how to install sensors or collect field data. Users interested to learn more about stormwater BMP field monitoring are encouraged to review guidance from the International Stormwater BMP Database at https://bmpdatabase.org/monitoring [@pitt_national_2018].

## Functionality

The BMP Hydrology Calculator is built as an RShiny (version 1.10.0 [@chang2024shiny]) web application with an optional API to run the calculator locally. Three analysis tools are featured: Rainfall Analysis, Flow Analysis, and Infiltration Analysis. The Rainfall and Flow Analysis modules were programmed in R (version 4.4.1 [@r_core_2024]), while the Infiltration Analysis calculator was programmed in Python (version 3.10.11 [@python_software_foundation_2023]). 

On the RShiny web application, each calculator tab contains an *"Instruction"* page with guidance on how to format user data into the beskope, downloadable data templates, as well as a *"Method"* page detailing the underlying equations and methodological assumptions. Step-by-step instructions for uploading, labeling, identifying units, and filtering the user data are included on the left-hand side of each calculator tab. 

The BMP Hydrology Calculator outputs include downloadable tables and figures. Herein, example calculator outputs are generated using the downloadable "demo data"; each calculator has its own demo dataset and timeseries template. 

<img src="BMPHC_Header.png" alt="Figure 1: BMP Hydrology Calculator Landing Page Header." width="600">

*Figure 1: BMP Hydrology Calculator Landing Page Header.*

**Rainfall Analysis**: User-uploaded rainfall data are visualized and analyzed. Output statistics include the number of unique events in the hyetograph record, average and peak rainfall intensity statistics, as well as the antecedent dry periods. 

<img src="Hyetograph.png" alt="Figure 2: Example rainfall hyetograph visualization generated by the Rainfall Analysis calculator." width="600">

*Figure 2: Example cumulative rainfall visualization generated by the Rainfall Analysis calculator with unique Event IDs annotated.*

*Table 1: Example rainfall analysis results showing unique rainfall events with intensity and dry period statistics.*

| Event ID | Storm DateTime  | Total Rainfall (in) | Avg Intensity (in/hr) | Peak 5-min Intensity (in/hr) | Peak 60-min Intensity (in/hr) | Antecedent Dry Period (hours) |
|:--------:|:------------:|:------------:|:------------:|:------------:|:------------:|:------------:|
| 1        | 2/14/1950 16:59 | 3.6 | 8.1 | 12.2 | 3.6 | NA |
| 2        | 2/23/1950 4:21  | 54.6 | 0.8 | 15.2 | 6.6 | 1.3 |
| 3        | 2/27/1950 17:46 | 3.1 | 0.3  | 6.1  | 1.8  | 1.7  |
| 4        | 2/28/1950 21:56 | 26.2 | 1.6    | 12.2     | 5.3    | 0.7   |

**Flow Analysis**: User-uploaded hydrograph data are visualized and analyzed. Output statistics include the peak flowrate, the runoff duration, and the total runoff volume. Comparisons of inflow/outflow are available for BMP performance assessments. Multiple BMP configurations are digestable by the Flow Analysis calculator; the example highlighted in Figure 3 is a BMP that has two inputs, outflow, and bypass all measured.

<img src="BMPschema.png" alt="Figure 3: BMP schematic with two inflows, outflow, and bypass data." width="800">

*Figure 3: Example BMP Schematic with inflow from two drainage areas ($Q_{in1}$, $Q_{in2}$), one outflow ($Q_{out}$), and one bypass ($Q_{by}$) flowrate timeseries.*

<img src="Hydrograph1.png" alt="Figure 4: Example flow hydrograph visualization generated by the Flow Analysis calculator." width="600">

*Figure 4: Example flow hydrograph visualization generated by the Flow Analysis calculator.*

*Table 2: Example flow analysis results showing unique storm events with flow statistics.*

| Event ID | Storm DateTime | Total Runoff Volume (ft³) | Peak Flowrate (cfs) | Runoff Duration (hr) | Inflow (ft³) | Outflow (ft³) | Bypass (ft³) |
|:--------:|:------------:|:------------:|:------------:|:------------:|:------------:|:------------:|:------------:|
| 1        | 2/14/1950 16:59 | 2,350                     | 1.8                 | 2.1                  | 2,350        | 2,100         | 250          |
| 2        | 2/23/1950 4:21  | 5,800                     | 3.2                 | 4.5                  | 5,800        | 5,200         | 600          |
| 3        | 2/27/1950 17:46 | 1,200                     | 0.9                 | 1.3                  | 1,200        | 1,100         | 100          |
| 4        | 2/28/1950 21:56 | 4,100                     | 2.5                 | 3.2                  | 4,100        | 3,700         | 400          |

**Infiltration Analysis**: User-uploaded depth data are visualized and analyzed to determine a characteristic infiltration rate on the regression limb (i.e., when the BMP is draining after a storm). An exponential decay model, expressed as y(t) = y<sub>0</sub> e<sup>-kt</sup>, where y<sub>0</sub> is the initial depth, k is the decay constant, and t is time, is fit to the depth data using the Scipy curvefit library [@scipy_curve_fit]. The calculator identifies the best-fit window (i.e. the duration of the regression limb) and reports the infiltration rate (cm/hr)

<img src="Infiltration.png" alt="Figure 5: Example infiltration rate visualized with depth data (from Infiltration Demo Data)." width="600">

*Figure 5: Example infiltration data visualization generated by the Infiltration Analysis calculator. Data from three separate piezometers installed in the same BMP.*

*Table 3: Example infiltration analysis results showing unique storm events with infiltration statistics.*

| Piezometer | Infiltration Rate (cm/hr) | Duration (hrs) | Average Depth (cm) |
|:--------:|:------------:|:------------:|:------------:|
| Piez A | 0.90 | 14.8 | 43.5 |
| Piez B | 0.90 | 14.8 | 42.6 |
| Piez C | 0.89 | 14.8 | 38.0 |


## Acknowledgements

This tool was developed by the Southern California Coastal Water Research Project (SCCWRP) with support from the Southern California Stormwater Monitoring Coalition (SMC). We additionally thank Matt McGauley from Villanova University for providing foundational infiltration calculator logic, and Adrian Montoya from Riverside County Flood Control District for sharing piezometer data. 

## References

<!-- References will be automatically generated from bmphydrologycalculator.bib based on citations used in the text -->
