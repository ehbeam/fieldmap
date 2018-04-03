#!/usr/bin/python

# Created by Elizabeth Beam on 7/9/17
# Execute mapping of coordinates using all combinations of parameters
# Exclude anatomical networks, which take longer and should be run on Sherlock
# Ignore BrainMap since those networks were previously generated
# Run different levels in parallel

import map_all

# Load lists of parameters
inputs = ["results", "abstracts", "texts"]
lemmas = [True, None]
windows = ["sentences", None]

# Execute mapping of functional networks of coordinates
for inp in inputs:
	for lem in lemmas:
		for win in windows:
			map_all.map("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro", inp, anat = True, lemmas = lem, neighbors = 100, level = "studies", window = win)
			map_all.map("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro", inp, behav = True, lemmas = lem, neighbors = 100, level = "studies", window = win)
			map_all.map("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro", inp, func = True, lemmas = lem, neighbors = 100, level = "studies", window = win)