Clifford Street missing segment
================
Peter Li
2024-01-23

With ‘cholera’ v.0.8.0.9017, I’ve made an amendment to Clifford Street.
I believe that the far Eastern segment between Old Marlborough Street
and Saville Row is missing from Dodson and Tobler’s digitization.

The graphic below shows Clifford Street from the map Snow used in his
work (Cheffin’s):

![](clifford.png)

This next graph shows the original Dodson and Tobler version:

<img src="clifford_files/figure-gfm/dodson_tobler-1.png" width="50%" />

To add the missing segment, I simply extend Clifford Street (street =
407; id = “407-1”) in a straight line to Saville Row. The details can be
found in cliffordStreet().

The graph below shows the amended segment (street” = 529; id = “529-1”):

<img src="clifford_files/figure-gfm/dodson_tobler_amended-1.png" width="50%" />

Georeferencing in [QGIS](https://qgis.org/) using ground control points
(v.02) shows this to be an adequate if not good approximation of the
actual road.
