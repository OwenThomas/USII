The code is structured as follows:

The models for factor analysis for CIDI are run here: ./fa_models_CIDI.R

The models for factor analysis for PHQ9 are run here: ./fa_models_PHQ9.R

The post-processing for factor analysis for CIDI are here: ./fa_models_analyse_CIDI.qmd

The post-processing for factor analysis for PHQ9 are here: ./fa_PHQ9_models_analyse.qmd

Gaussian likelihood clustering for CIDI is performed here: ./mclust_models_run_CIDI.R

Multinomial likelihood clustering for CIDI is performed here: ./fpc_models_run_CIDI.R

Ordinal likelihood clustering for CIDI is performed here: ./clustMD_2022_CIDI.R

The CIDI post-processing is done here: cluster_combined_postprocess_CIDI.qmd

The execution of clustering for the PHQ9 data set is run here: cluster_PHQ9_models_run.R

The post-processing for clustering for the PHQ9 data set is done here: cluster_PHQ9_combined_postprocess.qmd

The Network analysis for the CIDI data set is here: network_CIDI.qmd

The Network analysis for the PHQ9 data set is here: network_PHQ9.qmd

The timing for the clustering algorithms for CIDI is here: CIDI_cluster_timing.R

The timing for the clustering algorithms for PHQ9 is here: PHQ9_cluster_timing.R

Synthetic data for mimicking the PHQ9 data set is found here: synthesise_PHQ9.R
The generated data is located at: synthetic_phq9.rds
The data is generated based on summary data of count numbers and latent polychoric correlation found here: phq9.counts.rds and mixed.phq9.cor.all.rds

Synthetic data for mimicking the CIDI data set is found here: synthesise_cidi.R
The generated data is located at: synthetic_cidi.rds
The data is generated based on summary data of count numbers and latent polychoric correlation found here: cidi.counts.rds and mixed.cidi.cor.all.rds

