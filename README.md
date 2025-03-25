# pledge_rdd

This repository houses replication data for the paper "Renegers or Zealots?
The Causal Effect of Radical Right Opposition Parties on Government Anti-Immigration
Pledge-Fulfillment"

## Replicating 

To replicate the results of the paper: 

- clone this repository 
- unzip `data.zip`
- follow the instruction in `scripts/rdd.R`

To construct the data used for analysis from scratch:
- clone this repository
- unzip `data.zip`
- run `scripts/prepare_data.R`

Please consult `sessionInfo.txt` for R + package version requirements. 

## Note of Caution

This is really just a first stab at adding some causal identification to the pledge fulfillment literature (and also just meant to be a bit of fun). The analysis could definitely benefit from revision and extension (something I may find the time to do eventually). Also please excuse the poor documentation and code quality in `scripts/prepare_data.R`. 

## Data Sources

1. Lehmann, Pola, Franzmann, Simon, Al-Gaddooa, Denise, Burst, Tobias, Ivanusch, Christoph, Regel, Sven, Riethmüller, Felicia, Volkens, Andrea, Weßels, Bernhard, & Zehnter, Lisa (2024). *The Manifesto Data Collection. Manifesto Project (MRG/CMP/MARPOR). Version 2024a*. Wissenschaftszentrum Berlin für Sozialforschung / Göttinger Institut für Demokratieforschung. [DOI: 10.25522/manifesto.mpds.2024a](https://doi.org/10.25522/manifesto.mpds.2024a).

2. Soto Nishimura, A., & Mooyaart, J. (2021). *Migration Data Inventory Records: Data Inventory*. Online resource.

3. Döring, Holger, & Manow, Philip (2024). *ParlGov 2024 Release*. Harvard Dataverse. [DOI: 10.7910/DVN/2VZ5ZC](https://doi.org/10.7910/DVN/2VZ5ZC).

4. Helbling, Marc, Bjerre, Liv, Römer, Friederike, & Zobel, Malisa (2017). *Measuring immigration policies: The IMPIC database*. *European Political Science*, 16(1), 79–98. Springer.

5. Pemstein, Daniel, Marquardt, Kyle L., Tzelgov, Eitan, Wang, Yi-ting, Medzihorsky, Juraj, Krusell, Joshua, Miri, Farhad, & von Römer, Johannes (2025). *The V-Dem Measurement Model: Latent Variable Analysis for Cross-National and Cross-Temporal Expert-Coded Data*. *Varieties of Democracy Institute Working Paper*, 21(10th Ed). [URL](https://v-dem.net/wp.html).

6. Czaika, Mathias (2023). *DEMIG-QuantMig Migration Policy Database (August 2021, released July 2023)*. Zenodo. [DOI: 10.5281/zenodo.8169110](https://doi.org/10.5281/zenodo.8169110).

7. The World Bank (n.d.). *World Development Indicators*. Washington, D.C.: The World Bank. [URL](https://databank.worldbank.org/source/world-development-indicators). Accessed: 2025-03-04.

8. Rovny, Jan, Bakker, Ryan, Hooghe, Liesbet, Jolly, Seth, Marks, Gary, Polk, Jonathan, Steenbergen, Marco, & Vachudova, Milada (2024). *25 Years of Political Party Positions in Europe: The Chapel Hill Expert Survey, 1999-2024*. Working paper.

