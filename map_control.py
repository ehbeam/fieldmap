#!/usr/bin/python

# Created by Elizabeth Beam on 11/17/17
# Create control networks which are weighted by volume of coordinates and frequency of terms
# Example function call: control("/path/to/psychiatlas/directory")

import collections
import csv
import itertools
import os
import map_all
import networkx
import string

# Instantiate path
path = "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro"

# Function to load normalized word frequencies
def load_freq(file, labels, lemmas):
	words = []
	for line in open(file, "r").readlines():
		for word in line.split():
			if word in lemmas.keys():
				words.append(lemmas[word]['LEMMA'])
	counts = collections.Counter(words)
	return {key: float(val) / max(counts.values()) for key, val in counts.items()}

# Function to load matrix of terms from one node class
def load_unipartite(dict, nodes):
	matrix = map_all.init_mat(nodes, nodes)
	for edge in list(itertools.product(nodes, nodes)):
		node_1, node_2 = edge
		edge_labels = (map_all.gen_coord(node_1), map_all.gen_coord(node_2))
		if edge_labels in matrix.keys() and edge_labels[0] != edge_labels[1]:
			if node_1 in dict.keys() and node_2 in dict.keys():
				matrix[edge_labels] += dict[node_1] * dict[node_2]
				matrix[edge_labels[::-1]] += dict[node_1] * dict[node_2]
	return(matrix)

# Function to load matrix of terms from two node classes
def load_bipartite(dict_1, nodes_1, dict_2, nodes_2):
	matrix = map_all.init_mat(nodes_1 + nodes_2, nodes_1 + nodes_2)
	for edge in list(itertools.product(nodes_1, nodes_2)):
		node_1, node_2 = edge
		edge_labels = (map_all.gen_coord(node_1), map_all.gen_coord(node_2))
		if edge_labels in matrix.keys() and edge_labels[0] != edge_labels[1]:
			if node_1 in dict_1.keys() and node_2 in dict_2.keys():
				matrix[edge_labels] += dict_1[node_1] * dict_2[node_2]
				matrix[edge_labels[::-1]] += dict_1[node_1] * dict_2[node_2]
				continue
			elif node_1 in dict_2.keys() and node_2 in dict_1.keys():
				matrix[edge_labels] += dict_2[node_1] * dict_1[node_2]
				matrix[edge_labels[::-1]] += dict_2[node_1] * dict_1[node_2]
	return(matrix)

# Load anatomical and behavioral labels
anat = map_all.init_labs("{}/labels/harvard-oxford_anatomical_labels.txt".format(path))
behav = map_all.init_labs("{}/labels/brainmap_behavioral-domain_labels.txt".format(path))

# Load and normalize volumes from Harvard-Oxford atlas
vols = {}
for dict in csv.DictReader(open("{}/data/harvard-oxford_volumes_labels.csv".format(path), "rU")):
    vols.update({dict['LABEL']: float(dict['VOLUME'])})
vols = {key: float(val) / max(vols.values()) for key, val in vols.items()}

# Load lemma dictionary
lems = {}
for dict in csv.DictReader(open("{}/vecs/lemmas_texts_100neighbors_dictionary.csv".format(path), "rU")):
    lems.update({dict['TERM']: dict})

# Load and normalize word frequencies for abstracts, results, and full texts
abs = load_freq("{}/texts/abstracts/preproc_abstracts.txt".format(path), anat + behav, lems)
res = load_freq("{}/texts/results/preproc_results.txt".format(path), anat + behav, lems)
tex = load_freq("{}/texts/texts/preproc_texts.txt".format(path), anat + behav, lems)

# Load list of all networks
networks = [load_unipartite(vols, anat),
			load_bipartite(vols, anat, abs, behav),
			load_bipartite(vols, anat, res, behav),
			load_bipartite(vols, anat, tex, behav),
			load_unipartite(abs, anat),
			load_unipartite(res, anat),
			load_unipartite(tex, anat),
			load_unipartite(abs, behav),
			load_unipartite(res, behav),
			load_unipartite(tex, behav),
			load_bipartite(abs, anat, abs, behav),
			load_bipartite(res, anat, res, behav),
			load_bipartite(tex, anat, tex, behav)]

# Load list of filenames
filenames = ["coordinates_anat",
			"coordinates_func_abstracts",
			"coordinates_func_results",
			"coordinates_func_texts",
			"abstracts_anat",
			"results_anat",
			"texts_anat",
			"abstracts_behav",
			"results_behav",
			"texts_behav",
			"abstracts_func",
			"results_func",
			"texts_func"]

# Create directories for control networks
outdir = "{}/networks/_control/".format(path)
if not os.path.exists(outdir):
    os.makedirs(outdir)

# Export networks to graphml and csv files
for i, network in enumerate(networks):
	graph = map_all.build_graph(network)
	networkx.write_graphml(graph, outdir + filenames[i] + ".xml")
	networkx.write_weighted_edgelist(graph, outdir + filenames[i] + ".csv", delimiter = ",")

