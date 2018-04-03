#!/usr/bin/python

# Created by Elizabeth Beam on 11/17/17
# Create control networks which are weighted by volume of coordinates and frequency of terms

import collections
import csv
import itertools
import os
import map_all
import networkx
import string

# Function to load word frequencies
def load_freq(file, labels, lemmas):
	freq = {label: 0 for label in labels}
	words = [word for line in open("{}/{}".format(textdir, file), "r") for word in line.split() if (word in lemmas.keys()) and (lemmas[word]['LEMMA'] in labels)]
	for word in words:
		freq[lemmas[word]['LEMMA']] += 1
	return freq

# Function to load matrix of terms from one node class
def load_unipartite(dict, nodes):
	matrix = map_all.init_mat(nodes, nodes)
	for edge in list(itertools.product(nodes, nodes)):
		node_1, node_2 = edge
		edge_labels = (map_all.gen_coord(node_1), map_all.gen_coord(node_2))
		if edge_labels in matrix.keys() and edge_labels[0] != edge_labels[1]:
			if node_1 in dict.keys() and node_2 in dict.keys():
				matrix[edge_labels] += dict[node_1] + dict[node_2]
				matrix[edge_labels[::-1]] += dict[node_1] + dict[node_2]
	return(matrix)

# Function to load matrix of terms from two node classes
def load_bipartite(dict_1, nodes_1, dict_2, nodes_2):
	matrix = map_all.init_mat(nodes_1 + nodes_2, nodes_1 + nodes_2)
	for edge in list(itertools.product(nodes_1, nodes_2)):
		node_1, node_2 = edge
		edge_labels = (map_all.gen_coord(node_1), map_all.gen_coord(node_2))
		if edge_labels in matrix.keys() and edge_labels[0] != edge_labels[1]:
			if node_1 in dict_1.keys() and node_2 in dict_2.keys():
				matrix[edge_labels] += dict_1[node_1] + dict_2[node_2]
				matrix[edge_labels[::-1]] += dict_1[node_1] + dict_2[node_2]
				continue
			elif node_1 in dict_2.keys() and node_2 in dict_1.keys():
				matrix[edge_labels] += dict_2[node_1] + dict_1[node_2]
				matrix[edge_labels[::-1]] += dict_2[node_1] + dict_1[node_2]
	return(matrix)

# Function to export network
def export(network, file):
	graph = map_all.build_graph(network)
	networkx.write_weighted_edgelist(graph, file, delimiter = ",")

# Instantiate path
path = "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro"

# Create directories for control networks
outdir = "{}/networks/_control/_intermediary".format(path)
if not os.path.exists(outdir):
    os.makedirs(outdir)

# Load anatomical and behavioral labels
anat = map_all.init_labs("{}/labels/harvard-oxford_anatomical_labels.txt".format(path))
behav = map_all.init_labs("{}/labels/brainmap_behavioral-domain_labels.txt".format(path))

# Load lemma dictionary
lems = {}
for dict in csv.DictReader(open("{}/vecs/lemmas_texts_100neighbors_dictionary.csv".format(path), "rU")):
    lems.update({dict['TERM']: dict})

# Load parameters for mapping
texts = ["abstracts", "results", "texts"]

# Instantiate directory for output
outdir = "{}/networks/_counts/".format(path)
if not os.path.exists(outdir):
    os.makedirs(outdir)

# Compute text-based networks for each document
for input in texts:

	# Load directory to input
	textdir = "{}/texts/{}/preproc".format(path, input)
	anatdir = "{}/{}_studies_lemmas_100neighbors_{}".format(outdir, input, "anat")
	behavdir = "{}/{}_studies_lemmas_100neighbors_{}".format(outdir, input, "behav")
	funcdir = "{}/{}_studies_lemmas_100neighbors_{}".format(outdir, input, "func")
	if not os.path.exists(anatdir):
	    os.makedirs(anatdir)
	if not os.path.exists(behavdir):
	    os.makedirs(behavdir)
	if not os.path.exists(funcdir):
	    os.makedirs(funcdir)

	# Output networks by document
	for file in filter(lambda f: not f.startswith("."), os.listdir(textdir)):
		export(load_unipartite(load_freq(file, anat, lems), anat), "{}/{}.csv".format(anatdir, os.path.basename(file)))
		export(load_unipartite(load_freq(file, behav, lems), behav), "{}/{}.csv".format(behavdir, os.path.basename(file)))
		export(load_bipartite(load_freq(file, anat + behav, lems), anat, load_freq(file, anat + behav, lems), behav), "{}/{}.csv".format(funcdir, os.path.basename(file)))

