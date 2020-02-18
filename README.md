# TInGa
This is a repository hosting the implementation of TinGa, a trajectory inference method based on growing neural gas [1].

The scripts and functions in this package can be used to:

* Run TInGa on any dataset (TInGa will also perform dimensionality reduction as a first step)
* Run top performing trajectory inference methods such as Slingshot [2], PAGA [3], Monocle3 [4], RaceID/StemID [5]
* Generate figures to compare the performance of TInGa to other methods

[1]: Fritzke, B. (1995) A growing neural gas network learns topologies, _Advances in Neural Information Processing Systems_ 7, 625-632.
[2]: Street, _et al._ (2018) Slingshot: Cell lineage and pseudotime inference for single-cell transcriptomics, _BMC Genomics_, 19, 1-16.
[3]: Wolf, _et al._ (2019) PAGA: graph abstraction reconciles clustering with trajectory inference through a topology preserving map of single cells, _Genome Biology_, 20, 1-9.
[4]: Cao, _et al._ (2019) The single-cell transcriptional landscape of mammalian organogenesis, _Nature_, 566, 496-502.
[5]: Grün, _et al._ (2016) De Novo Prediction of Stem Cell Identity using Single-Cell Transcriptome Data, _Cell Stem Cell_, 19, 266-277.
