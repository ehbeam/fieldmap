#!/usr/bin/python

# Created by Elizabeth Beam on 11/17/17
# Create control networks which are weighted by volume of coordinates and frequency of terms
# Example function call: control("/path/to/psychiatlas/directory")

import ast
import collections
import csv
import itertools
import os
import map_all
import networkx
import string

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

# Load and normalize volumes from Harvard-Oxford atlas
vols = {}
for dict in csv.DictReader(open("{}/data/harvard-oxford_volumes_labels.csv".format(path), "rU")):
	vols.update({dict['LABEL']: float(dict['VOLUME'])})
vols = {key: float(val) / max(vols.values()) for key, val in vols.items()}

# Load lemma dictionary
lems = {}
for dict in csv.DictReader(open("{}/vecs/lemmas_texts_100neighbors_dictionary.csv".format(path), "rU")):
	lems.update({dict['TERM']: dict})

# Load parameters for mapping
texts = ["texts"]#["abstracts", "results", "texts"]

# Instantiate path for output networks
outdir = "{}/networks/_control/".format(path)
if not os.path.exists(outdir):
	os.makedirs(outdir)

# Compute text-based networks for each document
for i, input in enumerate(texts):

	# Load term frequencies normalized across all documents
	freq = load_freq("{}/texts/{}/preproc_{}.txt".format(path, input, input), anat + behav, lems)

	# Load directories to input
	textdir = "{}/texts/{}/preproc".format(path, input)
	textanatdir = "{}/_intermediary/{}_studies_lemmas_100neighbors_{}".format(outdir, input, "anat")
	textbehavdir = "{}/_intermediary/{}_studies_lemmas_100neighbors_{}".format(outdir, input, "behav")
	textfuncdir = "{}/_intermediary/{}_studies_lemmas_100neighbors_{}".format(outdir, input, "func")
	if not os.path.exists(textanatdir):
		os.makedirs(textanatdir)
	if not os.path.exists(textbehavdir):
		os.makedirs(textbehavdir)
	if not os.path.exists(textfuncdir):
		os.makedirs(textfuncdir)

	# Output networks by document
	for file in filter(lambda f: not f.startswith("."), os.listdir(textdir)):

		# Send output
		print("Processing {}".format(file))

		# Text-based networks
		words = set([word for line in open("{}/{}".format(textdir, file), "r") for word in line.split()])
		docfreq = {label: 0 for label in anat + behav}
		for word in words:
			if word in lems.keys():
				lem = lems[word]['LEMMA']
				docfreq.update({lem: freq[lem]})
		if not os.path.isfile("{}/{}".format(textanatdir, os.path.basename(file).replace(".txt", ".csv"))):
			export(load_unipartite(docfreq, anat), "{}/{}".format(textanatdir, os.path.basename(file).replace(".txt", ".csv")))
		if not os.path.isfile("{}/{}".format(textbehavdir, os.path.basename(file).replace(".txt", ".csv"))):
			export(load_unipartite(docfreq, behav), "{}/{}".format(textbehavdir, os.path.basename(file).replace(".txt", ".csv")))
		if not os.path.isfile("{}/{}".format(textfuncdir, os.path.basename(file).replace(".txt", ".csv"))):
			export(load_bipartite(docfreq, anat, docfreq, behav), "{}/{}".format(textfuncdir, os.path.basename(file).replace(".txt", ".csv")))

		# Coordinate-based networks
		behavfreq = {key: val for key, val in docfreq.iteritems() if key in behav}
		for sigma in range(0, 55, 5):
			if sigma == 0:
				sigma_lab = "raw"
			if sigma > 0:
				sigma_lab = "gaussian_{}mm".format(sigma)
			coordfuncdir = "{}/_intermediary/coordinates_studies_{}_lemmas_100neighbors_func_{}".format(outdir, sigma_lab, input)
			if not os.path.exists(coordfuncdir):
				os.makedirs(coordfuncdir)
			if not os.path.isfile("{}/{}".format(coordfuncdir, os.path.basename(file).replace(".txt", ".csv"))):
				infile = "{}/queries/studies/{}/{}".format(path, sigma_lab, file)
				if os.path.isfile(infile):
					raw_coords = [line.split(",") for line in open(infile, "r").readlines()]
					coords = [map_all.gen_coord(coord[0][0]) for coord in ast.literal_eval(open(infile, "r").read()) if coord]
					docvol = {label: 0 for label in anat}
					for coord in coords:
						if coord in vols.keys():
							docvol.update({coord: vols[coord]})
					export(load_bipartite(docvol, anat, behavfreq, behav), "{}/{}".format(coordfuncdir, os.path.basename(file).replace(".txt", ".csv")))
			

# Compute coordinate-based anatomical network for each document
for sigma in range(0, 55, 5):
	if sigma == 0:
		sigma_lab = "raw"
	if sigma > 0:
		sigma_lab = "gaussian_{}mm".format(sigma)
	coordanatdir = "{}/_intermediary/coordinates_studies_{}_anat".format(outdir, sigma_lab)
	if not os.path.exists(coordanatdir):
		os.makedirs(coordanatdir)
	for file in filter(lambda f: not f.startswith("."), os.listdir("{}/queries/studies/{}".format(path, sigma_lab))):
		if not os.path.isfile("{}/{}".format(coordanatdir, os.path.basename(file).replace(".txt", ".csv"))):
			infile = "{}/queries/studies/{}/{}".format(path, sigma_lab, file)
			raw_coords = [line.split(",") for line in open(infile, "r").readlines()]
			coords = [map_all.gen_coord(coord[0][0]) for coord in ast.literal_eval(open(infile, "r").read()) if coord]
			docvol = {label: 0 for label in anat}
			for coord in coords:
				if coord in vols.keys():
					docvol.update({coord: vols[coord]})
			export(load_unipartite(docvol, anat), "{}/{}".format(coordanatdir, os.path.basename(file).replace(".txt", ".csv")))

