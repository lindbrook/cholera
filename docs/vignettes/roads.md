Roads
================
lindbrook
2019-01-05

Overview
--------

In 1992, Rusty Dodson and Waldo Tobler digitized John Snow's cholera map. Unfortunately, they did not include the names of roads (e.g., Broad Street) in their data set. While not strictly necessary for analysis or visualization, having the names will be useful to some. To that end, I append the actual street names, taken from the map itself, to the data. This appended data set is called `roads`.

Roads data
----------

Before discussing names, some discussion of the structure of `roads` is warranted. The data contain 1241 pairs of x-y coordinates. These coordinates define the endpoints of the straight line segments used to describe 528 numerically identified "streets".

``` r
head(roads)
>   street n        x        y id      name
> 1      1 2 16.73800 18.69600  1 Map Frame
> 2      1 2 17.66000 18.71200  2 Map Frame
> 3      2 2 14.46200 18.65500  3 Map Frame
> 4      2 2 16.73800 18.69600  4 Map Frame
> 5      3 2 12.79388 18.61613  5 Map Frame
> 6      3 2 14.46200 18.65500  6 Map Frame
nrow(roads)
> [1] 1241
length(unique(roads$street))
> [1] 528
```

These 528 "streets" do not correspond to real-world streets. Excluding the 50 "streets" used to describe the map's frame, the remaining 478 "streets" describe 206 "real-world" roads (e.g., Oxford Street, Regent Street). This discrepancy emerges because 40% of "real-world" roads are composed of multiple "street" segments. For example, Oxford Street consists of 26 line segments and Broad Street consists of 6.

``` r
# Map Border "Streets" #

top <- c(1:12, 14)
right <- c(37, 62, 74, 142, 147, 205, 240, 248, 280, 360, 405, 419, 465)
bottom <- c(483, seq(487, 495, 2), 498, 500, seq(503, 519, 2))
left <- c(31, 79, 114, 285, 348, 397, 469)
border <- sort(c(bottom, left, top, right))

length(border)
> [1] 50
```

Road names
----------

The primary source for road names is Snow's map. A high resolution version is available [here](http://www.ph.ucla.edu/epi/snow/highressnowmap.html). While great effort was made to correctly record and cross-reference names, there may still be errors. Error reports and suggestions for amendments are welcome.

Some roads on the map do not have a name. In those cases, I attach unique labels like "Unknown-C".

Some names appear multiple times even though they lie at different locations. For these, I use Roman numerals to distinguish them (e.g., "King Street (I)" and "King Street (II)").[1]

Queen Street (I) and Marlborough Mews
-------------------------------------

There is one apparent coding error in Dodson and Tobler's road data. Queen Street (I) extends too far: the water pump (\#5) that is clearly located on Marlborough Mews (see [map](http://www.ph.ucla.edu/epi/snow/highressnowmap.html), cited above), ends up on Queen Street (I).

I amend this by moving the end point of Queen Street (I) westward so that the street only runs north-south. I do so by reassigning the segment that runs east-west to be part of Marlborough Mews.

``` r
snow.streets <- HistData::Snow.streets
snow.streets$id <- seq_len(nrow(snow.streets))

# Data frame of road names
road.data <- read.csv("~/Documents/Data IV/Snow/road3b.csv",
  stringsAsFactors = FALSE)

roads <- merge(snow.streets, road.data, by = "street", all.x = TRUE)
roads[is.na(roads$name), "name"] <- "Map Frame"

roads[roads$id == 277, "street"] <- 116
roads[roads$id == 277, "name"] <- "Marlborough Mews"
roads[roads$id == 277, c("x", "y")] <- roads[roads$id == 276, c("x", "y")]
roads[roads$name == "Queen Street (I)", "n"] <- 4
roads[roads$name == "Marlborough Mews", "n"] <- 3
roads <- roads[order(roads$id), ]
```

Finding roads by name, "street" number, or segment ID.
------------------------------------------------------

To help locate and visualize streets and road segments (including map frame segments), you can use the `streetNameLocator()`, `streetNumberLocator()`, or `segmentLocator()`.

Note that `streetNameLocator()` uses the names in the `roads` data set. However, the function tries to corrects for case and removes extraneous spaces: `streetNameLocator("Oxford Street")` and `streetNameLocator("oxford street")`.

`segmentLocator()` is for those interested in more granular analysis. It uses individual road segments as the unit of observation.

List of road names
------------------

There are 206 "valid" road names; 207, if we include "Map Frame":

                                                                        
    1   Adam and Eve Court                 Maidenhead Court             
    2   Air Street                         Margaret Court               
    3   Albany Street                      Margaret Street              
    4   Albemarle Street                   Market Court                 
    5   Allens Court                       Market Place                 
    6   Angel Court                        Market Row                   
    7   Archer Street                      Market Street (I)            
    8   Argyll Place                       Market Street (II)           
    9   Argyll Street                      Marlborough Court            
    10  Arundel Place                      Marlborough Mews             
    11  Arundel Street                     Marlborough Row              
    12  Batemans Buildings                 Marshall Street              
    13  Beak Street                        Marylebone Street            
    14  Bentinck Street                    Masons Arms Yard             
    15  Berners Street                     Meards Court/Street          
    16  Berwick Street                     Mill Street                  
    17  Black Lion Court                   Nags Head Yard               
    18  Blenheim Mews                      Naylors Yard                 
    19  Blenheim Street                    New Bond Street              
    20  Boyle Street                       New Burlington Mews          
    21  Brewer Street                      New Burlington Street        
    22  Bridle Street                      New Street                   
    23  Broad Street                       New Street/Husband Street    
    24  Bruton Street                      Newman Street                
    25  Bull Yard                          Noel Street                  
    26  Burlington Arcade                  Norris's Place               
    27  Burlington Gardens                 North Coventry Street        
    28  Cambridge Street                   Old Burlington Mews          
    29  Carlisle Street                    Old Burlington Street        
    30  Carnaby Court                      Old Compton Street           
    31  Carnaby Street                     Orchard Place                
    32  Castle Street East                 Oxendon Street               
    33  Catherine Wheel Yard               Oxford Street                
    34  Chapel Place                       Panton Street                
    35  Charles Street                     Perrys Place                 
    36  Church Street                      Peter Street                 
    37  Clifford Street                    Pews Place                   
    38  Coach & Horses Yard                Phoenix Yard                 
    39  Cock Court                         Picadilly                    
    40  Conduit Street                     Plough Yard                  
    41  Cork Mews                          Poland Street                
    42  Cork Street                        Pollen Street                
    43  Coventry Street                    Portland Mews                
    44  Cross Street                       Portland Street              
    45  Crown Court                        Princes Street (I)           
    46  Dean Street                        Princes Street (II)          
    47  Dover Street                       Princes Street (III)         
    48  Duck Lane/Ham Square               Princes Street/Hanover Square
    49  Dufours Place                      Pulteney Court (I)           
    50  Edward Street                      Pulteney Court (II)          
    51  Falconberg Court                   Queen Street (I)             
    52  Falconberg Mews                    Queen Street (II)            
    53  Fouberts Place                     Queen Street (III)           
    54  Francis Street                     Queens Head Court            
    55  Frith Street                       Rathbone Place               
    56  George Court (I)                   Regent Street                
    57  George Court (II)                  Regents Quadrant             
    58  George Place (?)                   Richmond Buildings/Mews      
    59  George Street                      Richmond Street              
    60  George Yard                        Rupert Street                
    61  Glasshouse Street                  Sackville Street             
    62  Golden Place                       Saville Row                  
    63  Golden Square                      Sherrard Street              
    64  Grafton Street                     Ship Yard                    
    65  Great Castle Street                Sidney Alley                 
    66  Great Chapel Street                Silver Street                
    67  Great Crown Court                  Smiths Court/Yard            
    68  Great Marlborough Street           Soho Square                  
    69  Great Pulteney Street              South Row                    
    70  Great Windmill Street              St Anns Court                
    71  Greek Street                       St Anns Place                
    72  Green Dragon Yard                  St James Workhouse           
    73  Greens Court                       St James's Market            
    74  Ham Yard                           Sutton Street                
    75  Hanover Street                     Swallow Place                
    76  Hanway Street                      Swallow Street               
    77  Haymarket                          Tent Court                   
    78  Heddon Court                       Tichborne Street             
    79  Heddon Street                      Titchfield Street            
    80  Hollen Street                      Turks Head Yard              
    81  Hopkins Street                     Tyler Court                  
    82  Jermyn Street                      Tyler Street                 
    83  John Street                        Tylers Court                 
    84  Kemps Court                        Unknown-A1                   
    85  King Street (I)                    Unknown-A2                   
    86  King Street (II)                   Unknown-B                    
    87  Kings Arms Yard                    Unknown-C                    
    88  Leicester Street (I)               Upper James Street           
    89  Leicester Street (II)              Upper John Street            
    90  Lisle Street                       Upper Rupert Street          
    91  Little Argyll Street               Vigo Street                  
    92  Little Chapel Street               Vine Street                  
    93  Little Crown Court                 Walkers Court                
    94  Little Dean Street                 Wardour Mews                 
    95  Little Marlborough Street          Wardour Street               
    96  Little Pulteney Street             Warwick Street               
    97  Little Windmill Street             Wellington Mews              
    98  Lower James Street                 Wells Street                 
    99  Lower John Street                  West Street                  
    100 Lowndes Court                      Whitcomb Court               
    101 Macclesfield Street                White Bear Yard              
    102 Macclesfield Street/Gerrard Street William and Mary Yard        
    103 Maddox Street                      Winsley Street               

Coordinate unit
---------------

The original map is 14.5 x 15.5 inches with a stated nominal scale of 30 inches per mile.

Dodson and Tobler write that "The scale of the source map is approx. 1:2000. Coordinate units are meters." By my calculation one unit on the map is approximately 177 feet or 54 meters per unit.[2]

Notes
-----

[1] Streets with the same name were not an unusual occurrence. See Judith Flanders. 2012. *The Victorian City: everyday life in Dickens' London*. New York: St. Martin's Press, 57-58.

[2] According to Dodson and Tobler's street data, the length of Carnaby Street from its intersection with Great Marlborough to its intersection with Silver Street is 2.61 units. According to Google Maps, the approximate analog of that segment is the distance along parts of Great Marlborough Street and Carnaby Street between 19-21 Great Marlborough Street and 43 Carnaby Street (at Ganton Street): 463 ft. This translates into approximately 177 feet/unit or 54 meters/unit.
