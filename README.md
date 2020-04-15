# IMO Anderson Lab COVID19 Working Group

## Instructions for running the model
Inputs of these models are all within the ```data/``` directory. If you change anything within this folder it is likely we will need to rerun the model.

Outputs for these models are held within ```ShinyApp/data/SEIR/``` and ```ShinyApp/data/SEIcIscR/``` for each model type, respectively.

To execute everything, you will have to run everything like so within an R session:
1. Open the file ```0_master_simulator.R```
2. Each line of code here will load the necessary components of the model and functions with the appropriate data.

The last step is to prepare the inputs for visualizations (```codes/5_PrepModelOutputs.R```), by executing ```0_master_simulator.R``` you should get everything you need.

# More details
The part of this that houses the actual models is ```codes/model_functions.R``` with the execution of these functions being done in ```codes/3_simOutbreak_ncov_SEIR.R``` and ```codes/4_simOutbreak_ncov_SEIcIscR.R``` where the data is saved and output. Please don't change the output format or where things are saved to.
