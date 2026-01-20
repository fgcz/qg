# new csv_to_paramsjson.py

## goal

I have a set of about 400 queue files generated with the legacy queue generator,
in test_data/qg_20260119_dump

They represent files which are loaded in the the xcalibur chronos or hystar software.

I would like to use them as a testset, to this task we need to infer the params_model json used to created them. 

that is we need date, instrument, output_format, sample ids, queue_pattern, as well as the sampler. 

we have already a similar script for doing so from the sld file generated csv files. However, I do not think that this script is usefull.

plase use the config-model output format to infer the output format by matching the column names in the qg files.

use the position_format and from sampler.toml and the qc_layouts to determine the sampler...

etc...

Generate the parms_json for all csv files -> if you can't infer set empty string. but cleary mark the json files with PARTIAL 
also create a .log file for each json where you clearly log which parmeter could not be inferred.




