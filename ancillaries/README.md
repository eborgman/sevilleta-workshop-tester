## Webinars
_SDrawNPS: A Graphical User Interface for GRTS Sampling and Analysis_ by Leigh
Ann Starcevich  
https://attendee.gotowebinar.com/recording/8005103632974168833

_Trend Analysis for Complex Survey Designs_
https://attendee.gotowebinar.com/recording/6666593365100927234

## GRTS design file attributes
`mdcaty` - multi-density category (a category used for unequal probability sampling)  
`wgt` - the design weight  
`EvalStatus` - needed to adjust design weights

## When do we adjust the design weights?
__If your sample is _larger_ or _smaller_ than expected__, e.g., when you don't
visit every single site and collect data.  
__You use a revisit design__ that doesn't involve visiting every site every year.
That sample is assuming that all of the sites are visited at the same time. If
you're only visiting a subset of those sites in a given year, you have to weight
a little bit more to account for the sites that were _not_ included in that
given year.  
__If non-sampling error is encountered__
