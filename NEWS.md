# SurveySolutionsAPIv2 0.1.0

* added new class UserClass
* to have consistency across all function arguments, all functions now have server, apiUser, apiPass, this may break some
code from the old SurveySolutionAPI package.
* extended suso_getINT, such that if no sv_id is provided, it will return all interviewers in the workspace.
* suso_PwCheck now prints a message and the url to the console when in interactive mode
* suso_getINT_info now covers both interviewer details and retrieval of log files (log = T)
* workspace has been added to suso_set_key
* translation can be applied to export data for categorical variables
* export allows to add weights file, so the data is analysis ready
