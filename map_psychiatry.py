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
behav_inputs = ["abstracts", "texts", "results"]
lemmas = [True]
lev = "studies"
windows = ["sentences"]

# Instantiate path
path = "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/psychiatry"

# Execute mapping of networks from texts
map_all.map(path, "brainmap", behav = True, level = "studies")
for win in windows:
	for lem in lemmas:
		for inp in behav_inputs:
			map_all.map(path, inp, anat = True, level = "studies", window = win, lemmas = lem, neighbors = 100)
			map_all.map(path, inp, behav = True, level = "studies", window = win, lemmas = lem, neighbors = 100)
			map_all.map(path, inp, func = True, level = "studies", window = win, lemmas = lem, neighbors = 100)

# Execute mapping of functional networks of coordinates
for strat in strategies:
	for inp in behav_inputs:
		for lem in lemmas:
			map_all.map(path, "coordinates", sigma = None, func = True, behav_input = inp, lemmas = lem, neighbors = 100, level = lev, strategy = strat, smooth = False)
			# for sig in sigmas:
			# 	map_all.map(path, "coordinates", sigma = sig, func = True, behav_input = inp, lemmas = lem, neighbors = 100, level = lev, strategy = strat, smooth = True, force = True)
