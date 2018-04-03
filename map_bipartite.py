#!/usr/bin/python

# Created by Elizabeth Beam on 7/9/17
# Create projections of anatomical and behavioral networks from functional networks
# Example function call: project("/path/to/psychiatlas/directory", level = "studies", behav_input = "abstracts", sigma = 0, strategy = "probabilistic", neighbors = 100)

import itertools
import map_all
import networkx
from networkx import bipartite
import os

# Function to execute network projection
def bipartite(path, level = None, input = None, behav_input = None, sigma = None, strategy = None, lemmas = None, neighbors = None, windows = None, scrambled = None):

	# Load term labels by class
	anat_nodes = [map_all.presentable(node) for node in map_all.init_labs("{}/labels/harvard-oxford_anatomical_labels.txt".format(path))]
	behav_nodes = [map_all.presentable(node) for node in map_all.init_labs("{}/labels/brainmap_behavioral-domain_labels.txt".format(path))]

	# Load network
	filename = ""
	lemma_label = ""
	if lemmas:
			lemma_label = "lemmas_{}neighbors_".format(neighbors)
	if input == "coordinates":
		smooth_label = "raw"
		if sigma > 0:
			smooth_label = "gaussian_{}mm".format(sigma)
		filename = "coordinates_{}_{}_{}_{}func_{}".format(level, smooth_label, strategy, lemma_label, behav_input)
	if input in ["abstracts", "results", "texts"]:
		window_label = ""
		if windows == "sentences":
			window_label = "sentences_"
		filename = "{}_{}_{}{}func".format(input, level, window_label, lemma_label)
	net = networkx.read_graphml("{}/networks/{}.xml".format(path, filename))

	# Instantiate output file
	if not scrambled:
		outdir = "{}/networks/_bipartite".format(path)
		if not os.path.exists(outdir):
			os.mkdir(outdir)
	if scrambled:
		outdir = "{}/networks/_bipartite/_scrambled".format(path)
		if not os.path.exists(outdir):
			os.mkdir(outdir)
	outfile = open("{}/{}_bipartite.csv".format(outdir, filename), "w+")
	outfile.close()
	outfile = open("{}/{}_bipartite.csv".format(outdir, filename), "a+")

	# Write bipartite graph
	for orig_edge in list(net.edges_iter(data = "weight")):
		lab1, lab2, weight = orig_edge
		if lab1 in anat_nodes:
			anat_node = lab1
			behav_node = lab2
		else:
			anat_node = lab2
			behav_node = lab1
		outfile.write(anat_node + "," + behav_node + "," + str(weight) + "\n")

# Load lists of parameters
inputs = ["abstracts", "results", "texts"]
levels = ["studies", "experiments"]
scrambles = [None, True]
lemmas = [True, None]
windows = ["sentences", None]
sigmas = [0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50]
strategies = ["probabilistic", "winner-takes-all", "probabilistic-winner-takes-all"]

# Execute mapping of bipartite functional networks of texts
for scram in scrambles:
	for inp in inputs:
		for win in windows:
			for lem in lemmas:
				bipartite("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro", level = "studies", input = inp, lemmas = lem, neighbors = 100, windows = win, scrambled = scram)

# Execute mapping of bipartite functional networks of coordinates
for lev in levels:
	for scram in scrambles:
		for strat in strategies:
			for inp in inputs:
				for lem in lemmas:
					for sig in sigmas:
						bipartite("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro", input = "coordinates", level = lev, behav_input = inp, sigma = sig, strategy = strat, lemmas = lem, neighbors = 100, scrambled = scram)

