### Unemployment and Public Transport Job Accessibility (PTJA)
Urban Transport MSc at Uni of Glasgow. Dissertation project on relationship between unemployment and public transport job accessibility in Greater Manchester. Subsequently extended the study to other combined authorities for comparison.

### Public Transport Job Accessibility (Demand-adjusted)
Public transport job accessibility distribution for each area calculated by summing the numbers of jobs decayed according to travel time by public transport to their location. Travel time matrix calculated using [R5R, Rapid Realistic Routing algorithm for multi-model transit journeys](https://github.com/ipeaGIT/r5r).

PTJA distributions for Greater Manchester Combined Authority, Liverpool City Region Combined Authority and West Yorkshire Combined Authority shown below.
<div style="display: flex; flex-direction: row; gap: 20px;">
  <img src="Greater_Manchester_Combined_Authority/Images/PTJA_D_GMCA.jpeg" title="GMCA" width="400">
  <figcaption>Greater Manchester Combined Authority</figcaption>
  <img src="Liverpool_City_Region/Images/PTJA_D.jpeg" title = "LCRCA" alt="PTJDA-D" width="400">
</div>
<div style="display: flex; flex-direction: row; gap: 20px;">
  <img src="WYCA/Images/PTJA_D.jpeg" title="WYCA" width="400">
</div>

### Interactive Mapping
An [interactive map of Liverpool and Manchester](https://samallwood.github.io/Unemployment_Public_Transport_Access/) showing job distributions 
from the Business Register and Employment Survey and Public Transport Job Accessibility (Demand Adjusted). There is a similar map for West Yorkshire available in the docs folder but displaying all three combined authorities on one map made the file too large for github.

### Population Distribution in GMCA and WYCA
<div style="display: flex; flex-direction: row; gap: 20px;">
  <img src="Greater_Manchester_Combined_Authority/Images/Population_GMCA.png" alt="Pop_dens" width="400">
  <img src="WYCA/Images/WYCA_Pop.png" alt="PTJDA-D" width="400">
</div>


Created using Niloy Biswas's [population density map](https://github.com/niloy-biswas/Population-Density-Map/) method.

### Project Dataset Schematic:
<img src="Greater_Manchester_Combined_Authority/Images/Dataset_diagram.jpg" alt="Datasets" width="800">
All datasets are publicly available from Bus Open Data Service (BODS), Rail Delivery Group, UK Data Service and ONSGeoportal. 
For full details and citations, see the final report.
