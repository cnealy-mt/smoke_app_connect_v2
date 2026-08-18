This folder contains old scheduler scripts. 

Currently, the original scheduler script is here, which ran hourly updates for a singular workflow. Conditional logic in the R file determined whether just
the monitoring data needed updated or both the modeling data and monitoring data.

This workflow caused frequent overlapping jobs (usually once a day) because the full model + monitoring download would take more than an hour.
The workflows were separated 6/16/26 to ensure that GitHub Actions did not deprioritize the hourly monitoring updates.
Additionally, the new model-only update workflow is now scheduled to run a maximum of 3 times with 90 minute windows long enough to ensure no double triggering.
