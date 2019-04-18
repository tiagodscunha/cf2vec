# cf2vec: Collaborative Filtering dataset embeddings for algorithm selection

The code presented is composed by a series of R scripts, all pertaining to the metalevel procedure. Thus, we do not include in this repository the source code required to train the Collaborative Filtering algorithms nor to use graph2vec. Instead, we provide already the results of such experiments through several CSV files in appropriately named folders. If you wish to obtain more information regarding this topic, please refer to the official repositories of MyMediaLite (https://github.com/zenogantner/MyMediaLite) and graph2vec (https://github.com/MLDroid/graph2vec_tf).

The contents provided can be organized in folders and files. The first refer to the metadatasets, algorithms and results obtained, while the latter refer to an R script responsible for an individual task. Each task is usually perceptible the the file name selected. However, for clarification purposes, we list the meaning of each folder/file below.

Folders:
- labelrankingforests-master: source code with Label Ranking algorithms implementation and evaluation and tuning procedures (source code adapted from https://github.com/rebelosa/labelrankingforests)
- embeddings_grid_search: metafeatures generated by graph2vec (original source code available in https://github.com/MLDroid/graph2vec_tf)
- embeddings_grid_search_results: pre-computed metalevel evaluations obtained by all ariations of cf2vec studied
- metafeatures_graph: graph-based CF metafeatures (proposed in https://arxiv.org/abs/1807.09097)
- metafeatures_landmarkers: subsampling landmarkers metafeatures (proposed in https://link.springer.com/chapter/10.1007/978-3-319-67786-6_14)
- metafeatures_statistical: statistical and information theoretical metafeatures (proposed in https://link.springer.com/chapter/10.1007/978-3-319-46227-1_25)
- results: metalevel evaluation results for all metamodels used
- targets: multicriteria metatargets (proposed in https://arxiv.org/abs/1807.09097)

Files:
- auxiliary.R: auxiliary functions to process metadata and metatargets
- auxiliary_CF4CF_META: auxiliary file which contains the source code for CF4CF-META framework 
- baselevel_impact_graphic.R: script to create the graphics to assess the impact on the baselevel performance for all competing approaches
- CF4CF-META_baselevel_impact.R: script to calculate CF4CF-META's impact on the baselevel performance
- CF4CF-META_meta_experiments.R: script to calculate CF4CF-META's metalevel evaluation performance
- LR_baselevel_impact.R: script to calculate LR's impact on the baselevel performance
- LR_evaluation.R: script that contains auxiliary functions for Label Ranking evaluation
- LR_grid_search_visualization.R: script used to create boxplot performance visualization for all configurations used in the hyperparameter tuning procedure
- LR_meta_experiments.R: script to calculate LR's metalevel evaluation performance
- LR_tuning.R: auxiliary functions to perform grid-search tuning for Label Ranking algorithms
- metalevel_visualization.R: script to create graphics to assess the metalevel evaluation performance for all competing approaches
- PCA_metadataset_visualizations.R: script to create the metadataset visualizations using PCA
- tuningCF.R: auxiliary functions used to perform hyperparameter tuning in CF4CF-META

Notice that in addition to provide all metadatasets and algorithm implementations used for each step in the procedure and which guarantee the reproducibility of the presented research, we also include all metalevel experiment results. This allows to obtain the figures used in the paper without the necessity of running all scripts presented. However, if you wish to re-run the experiments, the outcoming results replace the ones provided in the repository, thus making the analysis easily reproducible. Lastly, in order to obtain the figures, you must run each script individually and assess the graphics output. 

