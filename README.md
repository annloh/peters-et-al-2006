# peters-et-al-2006
Replication of the simulation study by Peters et al 2006

## Project structure
The project structure can be obtained from the following tree.
All R-code is contained in the src directory.
The working directory has to be set to the parent directory (i.e. the one the README.md is located in).
 
```
.
├── README.md
├── shiny
│   └── peters-et-al-2006
│       ├── server.R
│       └── ui.R
└── src
    ├── analysis.R
    ├── data_generation_functions.R
    ├── dependencies.R
    ├── main.R
    ├── nested_loop_plot.R
    ├── plot_error_rate.R
    ├── plot_i_squared.R
    ├── plotting.R
    ├── run_sim.R
    ├── scenarios.R
    └── test_functions.R
```

## src

### dependencies.R
Specifies and loads dependencies.
There is not auto-installer. 
So please check this script before running the code for any packages you might need to install.

### main.R
Will run the simulation study sourcing everything necessary to do so.
Results are saved as `sim_data.Rds` in the parent directory.
Without changing the input it will run 1000 iterations which takes a considerate amount of time
You can change this by resetting `n_iter` to your prefered number of iterations. 

### analysis.R
Loads the results and applies the egger test as well as the Peters test to the data.
Computes type 1 error rate and power and saves these in long format for plotting.

### plotting.R
Loads data produced by `analysis.R` and plots type 1 error rate and power for variable grouping options.
Saves the plot as `.pdf` file in the parent directory.

### plot_i_squared.R 
Loads data produced by `main.R` and produces violing plots of the $I^2$. 
Does not require `analysis.R` to run first.

### run_sim
Wrapper function to loop over the simulation scenarios and generate data. Does not need to be run.
Is called by `main.R`.

### data_generation_functions.R
Helper functions for data generation. Does not need to be run. Is sourced by `main.R`.

### test_functions.R
Functions for egger regression test and Peters regression test.
Does not need to be run, is sourced in `analysis.R` 

### scenarios.R
Compiles scenarios for simulation. Does not need to be run, is sourced in `main.R`.

### nested_loop_plot.R
(Does not work) Work in progress to plot the results as a nested loop plot.

### plot_error_rate.R
Plotting function. Does not need to be run is sourced by `analysis.R`.

## shiny
(Does not work)
