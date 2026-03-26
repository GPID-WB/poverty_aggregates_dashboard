*ado uninstall pip
net install github, from("https://haghish.github.io/github/")
github install worldbank/pip, version(0.11.0.9000) replace


* pip_survey_* 
pip, clear server(qa) povline(3.0,4.2,8.3)

* pip_fillgaps_* 
pip, clear server(qa) fillgaps povline(3.0,4.2,8.3)

* pip_aggregates_*
pip wb, clear server(qa) fillgaps povline(3.0,4.2,8.3)

* pip_country_coverage
pip tables, table(country_coverage) clear server(qa)