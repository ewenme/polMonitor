polMonitor
================

![polMonitor](https://github.com/ewenme/polMonitor/blob/master/example.gif)

<br>

polMonitor is an interactive [Shiny](https://shiny.rstudio.com/) application for people to explore, and engage with, *known* police violence data. It is currently limited to US data on police killings. Future ambitions for the project include the incorporation of other nations' police killings data, and combining other data sources e.g. [The Stanford Open Policing Project](https://openpolicing.stanford.edu/).

**Data Sources**

police violence data is taken from [Mapping Police Violence](https://mappingpoliceviolence.org), a police killings database which draws on several large, independent crowdsourcing efforts (e.g. [KilledbyPolice.net](http://killedbypolice.net/)). More information about the data is available [here](https://mappingpoliceviolence.org/aboutthedata/).

[US census](https://www.census.gov/2010census/data/) data (2010) was used predominantly in the calculation of police killing rates amongst various populations.

**Credits**

The [tidycensus](https://cran.r-project.org/web/packages/tidycensus/index.html), [leaflet](https://cran.r-project.org/web/packages/leaflet/leaflet.pdf) and [shinyjs](https://cran.r-project.org/web/packages/shinyjs/index.html) R packages were invaluable resources in realising this idea.

The basemap is *CartoDB.DarkMatter*, designed by [Stamen](http://stamen.com/), while the ui was hugely inspired by Joe Cheng's [SuperZIP](https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example).

The beautiful typeface you see in the app is [Work Sans (Regular)](https://github.com/weiweihuanghuang/Work-Sans), developed by [wei](https://twitter.com/w__h_).
