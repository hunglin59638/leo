Leo is freely available at https://leo.hlin.tw/

## Introduction
Leo is a web-based toolkit for Laboratory of Animal Proteomics in the department of Animal Science, National Chung Hsing University  

### Analysis items  
+ Descriptive statistics
+ Normality tests
+ Heteroscedasticity test 
+ Mulitiple comparasion
+ Non-parametric tests
+ Gene ID conversion
+ GO classification
+ GO over-representation (enrichment) analysis
+ KEGG over-representation analysis
## Requirement
+ linux (dev os is Ubuntu 20.04)
+ R 

## Installation

### Source code
Installing all dependencies using conda.
```
git clone https://github.com/hunglin59638/leo.git
cd leo
conda env create -f environment.yml or conda.lock.yml
Rscript run.R [port_num] # default is 6523
```  

### Docker (Recommended)
```
docker pull hunglin59638/leo:latest
docker run -d -p 6523:6523 hunglin59638/leo:latest
```


