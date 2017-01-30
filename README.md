
<!-- README.md is generated from README.Rmd. Please edit that file -->
FusionClust - Cluster fusions
=============================

Collects multiple fusion points into fusion point ranges. Can be one or two ended. Clustering is based on a window, with each fusion point initially its own group. Groups are merged if they contain fusions that are less than or equal to the window distance apart. For two-ended grouping, ends are clustered separately and then groups are merged when they contain opposite sides of the same fusion.
