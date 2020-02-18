# TInGa
This is a repository hosting the implementation of TinGa, a trajectory inference method based on growing neural gas.

### 1) Download datasets from the dynbenchmark repository
The 110 real and 240 synthetic datasets that were used to test TInGa against other trajectory inference methods can be downloaded using the script "0_download_datasets".


### 2) Load a specific dataset 
Once all the datasets have been downloaded locally, we can load one specific dataset as follows:

```{r}
dataset <- dynbenchmark::load_dataset("synthetic/dyntoy/converging_1")
```
