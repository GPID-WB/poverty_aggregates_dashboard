
net install github, from("https://haghish.github.io/github/")
github install worldbank/pip, version(0.10.15.9000)

* pip_survey_* 
pip, clear server(qa) 

* pip_fillgaps_* 
pip, clear server(qa) fillgaps

* pip_aggregates_*
pip wb, clear server(qa) fillgaps

* pip_country_coverage
pip tables, table(country_coverage) clear server(qa)