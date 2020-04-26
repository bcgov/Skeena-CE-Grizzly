# grizzly-bear-IUCN-threats

This repository contains R code that summarizes Data/Maps to assess Grizzly Bear IUCN GBPU threat

# 
Data/Maps to assess Grizzly Bear IUCN GBPU threat

<div id="devex-badge"><a rel="Exploration" href="https://github.com/BCDevExchange/assets/blob/master/README.md"><img alt="Being designed and built, but in the lab. May change, disappear, or be buggy." style="border-width:0" src="https://assets.bcdevexchange.org/images/badges/exploration.svg" title="Being designed and built, but in the lab. May change, disappear, or be buggy." /></a></div>

### Data

### Usage

#### Scenario Analysis


There are four core scripts that are required for the threat analysis, they need to be run in order:

-   01\_load.R
-   02\_clean.R
-   03\_analysis.R
-   04\_output.R

```r
install.packages("devtools") # if you don't already have it installed

library(devtools)
install_github("bcgov/envreportutils")
install_github("thomasp85/patchwork")
```

## Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov-c/intact-landcover/issues/).

## How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.

## Licence

    Copyright 2017 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.


This repository is maintained by [ENVEcosystems](https://github.com/orgs/bcgov/teams/envecosystems/members).