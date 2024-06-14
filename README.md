# Master Thesis
## The dependence of the implied cost of capital as an expected return proxy on the performance of the underlying earnings forecasts.

I show that the performance of the implied cost of capital (ICC) as an expected return proxy directly depends on the performance of the underlying earnings forecasts. I show that the established model-based earnings forecasts of Hou et al. (2012) for implied cost of capital estimation do not perform well among small caps, value firms and low profitability firms. By accounting for size and BM-ratio-related heterogeneity in terms of earnings persistence, I demonstrate that the performance of the underlying earnings forecasts significantly improves. This improvement in the earnings forecasts spills over into the implied cost of capital performance in terms of their alignment with realized returns and their reliability as the expected return proxy in asset pricing tests. Finally, asset pricing tests demonstrate that the size effect is sensitive to the expected return proxy.

Keywords: Implied Cost of Capital; Earnings forecasts; Expected return proxies

JEL-Classification: tbd.

[Paper] (tbd.)

---

### How to replicate the analysis

**Step 1**: Download the data (compressed Stata file) from [Dropbox](https://www.dropbox.com/scl/fi/zw5xxioftbs9is2bllqgi/All_Data.dta.zip?rlkey=gvy60hyavlhtzimmw6dz3957r&st=7swykeyw&dl=0).

**Step 2**: Open `thesis.Rproj` in RStudio.

**Step 3**:
- By double-clicking on the project file, the working directory should be automatically set to the directory where all files are located. If this is not the case, you can set the working directory manually (in the `main.R` file) by changing the path in the setwd() function. 
  ```r
  setwd("path/to/your/project/directory")
- The *here function* in the `data.R` file (line 2) constructs file paths relative to the root of your project directory. If this is not the case, you must replace the full path inside the read_dta function with the location of the downloaded dataset file.
  ```r
  All_data <- read_dta("path/to/your/downloaded/All_Data.dta")
  
**Step 4**: Install and load the packages inside the `main.R` file.

**Step 5**: Run all the R scripts by sourcing the code chunks inside the `main.R` file to replicate the analysis (i.e. plots and results).

---
### Output of the Analysis (tbd.)

After executing these steps, your project folder (more specifically your **plots and results folders**) will contain the following files:

#### Results

- `earnings.csv`: earnings forecast from HVZ (2012) and modified LM (2014) models in the US
- `icc_gls.csv`: ICC's from the GLS (2001) model in the US

#### Plots

- `earnings.csv`: earnings forecast from HVZ (2012) and modified LM (2014) models in the US

- `icc_gls.csv`: ICC's from the GLS (2001) model in the US

---
### Description of R `scripts` folder:

- `main`: sets the working directory, loads packages, and can run all code chunks.
- `data`: creates various data subsets, and calculates variables of interest used in further analysis.
- `functions`: provides detailed descriptions of the unique functions created for this analysis.
- `tests`: tbd.
- `01_Earnings`: computes average cross-sectional earnings forecast regression summary statistics (Table 1)
- `02_FB`: computes average cross-sectional returns and unexpected earnings (forecast bias) sorted by unexpected earnings deciles (Table 2)
- `03_FF`: computes average cross-sectional earnings, forecast bias, and returns sorted by Fama and French (2015) characteristics (Table 3)
- `04_ICC`: computes average cross-sectional ICC decile summary statistics, i.e. GLS model ICCs implied by HVZ- and LM-model-based earnings forecasts respectively (Table 4)
- `05_MeV`: computes average cross-sectional MeV across the full sample, size, and BM-ratio subgroups (Table 5)
- `06_Risk`: computes average cross-sectional risk factor, ICC, and realized return effects (Table 6)

---

### Main References:

The earnings forecasts and ICC models used in this project are based on the methodologies proposed in the following seminal papers:

```bibtex
@article{gebhardt2001toward,
  title={Toward an implied cost of capital},
  author={Gebhardt, William R and Lee, Charles MC and Swaminathan, Bhaskaran},
  journal={Journal of accounting research},
  volume={39},
  number={1},
  pages={135--176},
  year={2001},
  publisher={Wiley Online Library}
}

@article{hou2012implied,
  title={The implied cost of capital: A new approach},
  author={Hou, Kewei and Van Dijk, Mathijs A and Zhang, Yinglei},
  journal={Journal of Accounting and Economics},
  volume={53},
  number={3},
  pages={504--526},
  year={2012},
  publisher={Elsevier}
}

@article{li2014evaluating,
  title={Evaluating cross-sectional forecasting models for implied cost of capital},
  author={Li, Kevin K and Mohanram, Partha},
  journal={Review of Accounting Studies},
  volume={19},
  pages={1152--1185},
  year={2014},
  publisher={Springer}
}
```
![Maastricht University](UM_Logo.png)
