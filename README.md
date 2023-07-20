# Accumulation of immunity in heavy-tailed sexual contact networks shapes mpox outbreak sizes
Source code accompanying Murayama et al. "Accumulation of immunity in heavy-tailed sexual contact networks shapes mpox outbreak sizes". Published in *The Journal of Infectious Diseases* doi: [10.1093/infdis/jiad254](https://doi.org/10.1093/infdis/jiad254)

## Main file
- countries_analysis.ipynb: Jupyter Notebook to identify where the local growth rate for each country reaches zero.
- US_states_analysis.ipynb: Jupyter Notebook to identify where the growth rate for each US-state reaches zero.
- SAR_estim.ipynb: Jupyter Notebook to reproduce the simulation outputs based on our network model.
- growth_estim.stan: Stan code for fitting observed cumulative incidence data to Gompertz curve with its local growth rate jointly computed.
- inference.R: R code to reproduce the transmission trajectories over time and the relationship between secondary attack risk and herd immunity threshold without accounting for the effect of any interventions or behavioural changes.
- finalsize.R: R code to compute the final sizes.

## Licence

[MIT](https://github.com/hiroaki-murayama/MPX_depletion_susceptibles/blob/master/LICENSE)

## Authors

[Hiroaki Murayama](https://github.com/hiroaki-murayama),
[Carl A. B. Pearson](https://github.com/pearsonca),
[Sam Abbott](https://github.com/seabbs),
[Fuminari Miura](https://github.com/fmiura),
[Sung-mok Jung](https://github.com/SungmokJung),
Elizabeth Fearon,
[Sebastian Funk](https://github.com/sbfnk),
[Akira Endo](https://github.com/akira-endo)
