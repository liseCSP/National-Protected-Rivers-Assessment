# National-Protected-Rivers-Assessment
Repository supporting the analyses presented in the manuscript "The urgent need for expanded protections to safeguarding America’s rivers"

The repository includes five R scripts to reproduce the analyses and figures presented in the manuscript. Data are available in the repository (https://doi.org/10.5281/zenodo.17279334) and otherwise sourced in the scripts. All the data should be added to the 'data' folder before running the scripts.

1.MechanismsProtection.R
    --> Summarize the extent of river protection per class of the Protected River Index (PRI), and includes code for Figure 1b-c
    --> Summarize extent of river protection per category of mechanisms presented in Table 1
    --> Estimate co-occurences among mechanisms of protection and includes code for Extended Data Figure 3
    --> Estimate mean number of individual protection mechanism per segment and PRI class and includes code for Extended Data Figure 4

2.Representativity.R
    --> Estimate extent of protection per PRI class for ecoregions and includes code for Extended Data Figure 5
    --> Estimate extent of protection per PRI class for hydrologic regions (HUC02) and includes code for Extended Data Figure 6
    --> Estimate extent of protection per PRI class for elevation and includes code for Extended Data Figure 7
    --> Estimate extent of protection per PRI class for (a) major habitat types, (b) stream type, (c) hydrologic regime, and (d) water temperature regime and includes code for Figure 2 and tests for unbalances

3.LocalVsUpstreamProtection.R
    --> Estimate extent of in-state (local) and out-of-state (upstream) protection within shared river basins (HUC06) per PRI class
    --> Estimate types of in-state (local) and out-of-state (upstream) protection within shared river basins (HUC06) (federal, state, local, private, other)
    --> Code to reproduce Figure 3, Extended Data Table 1, Extended Data Figure 9

4.ProtectionPrioritization.R
    --> Estimate indicators of river values and overlap among them at the watershed scale (HUC12)
    --> Identify priority watersheds for future protection
    --> Estimate progress towards 30 x 30 goals by considering both local (watershed) and upstream (within upstream watersheds) protection across all and high priority watersheds
    --> Code to reproduce Figure 4 (Venn diagramm) and Figure 5

5.MatrixOfWeights.R
    --> Code to reproduce Figure 1a and Figure S1 showing the matrix of weights used to compute the PRI
