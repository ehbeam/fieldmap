#!/usr/bin/python

# Created by Elizabeth Beam on 7/9/17
# Execute mapping of coordinates using all combinations of parameters
# Exclude anatomical networks, which take longer and should be run on Sherlock
# Ignore BrainMap since those networks were previously generated
# Run different levels in parallel

import map_all

# Load lists of parameters
strategies = ["winner-takes-all", "probabilistic-winner-takes-all", "probabilistic"]
sigmas = [5, 10, 15, 20, 25, 30, 35, 40, 45, 50]
behav_inputs = ["results"]#["abstracts", "results", "texts"] # "brainmap"
lemmas = [True]#[True, None]
lev = "studies"

# Execute mapping of functional networks of coordinates
for strat in strategies:
	for inp in behav_inputs:
		for lem in lemmas:
			map_all.map("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro", "coordinates", sigma = None, func = True, behav_input = inp, lemmas = lem, neighbors = 100, level = lev, strategy = strat, smooth = False, scrambled = True)
			#map_all.map("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro", "coordinates", sigma = None, anat = True, level = lev, strategy = strat, smooth = False, scrambled = True)
			for sig in sigmas:
			 	map_all.map("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro", "coordinates", sigma = sig, func = True, behav_input = inp, lemmas = lem, neighbors = 100, level = lev, strategy = strat, smooth = True, scrambled = True)
			 	#map_all.map("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro", "coordinates", sigma = sig, anat = True, level = lev, strategy = strat, smooth = True, scrambled = True)

