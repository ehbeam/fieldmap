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
def project(path, level = None, behav_input = None, sigma = None, strategy = None, neighbors = None):

	# Load term labels by class
	anat_nodes = [map_all.presentable(node) for node in map_all.init_labs("{}/labels/harvard-oxford_anatomical_labels.txt".format(path))]
	behav_nodes = [map_all.presentable(node) for node in map_all.init_labs("{}/labels/brainmap_behavioral-domain_labels.txt".format(path))]

	# Load network
	smooth_label = "raw"
	if sigma > 0:
		smooth_label = "gaussian_{}".format(sigma)
	net_file = "{}/networks/coordinates_{}_{}_{}_func_{}.xml".format(path, level, smooth_label, strategy, behav_input)
	net = networkx.read_graphml(net_file)

	# Convert to bipartite graph
	bp_net = networkx.Graph()
	bp_net.add_nodes_from(anat_nodes, bipartite = 0)
	bp_net.add_nodes_from(behav_nodes, bipartite = 1)
	for orig_edge in list(net.edges_iter(data = "weight")):
		lab1, lab2, weight = orig_edge
		if lab1 in anat_nodes:
			anat_node = lab1
			behav_node = lab2
		else:
			anat_node = lab2
			behav_node = lab1
		bp_net.add_edge(anat_node, behav_node, weight = weight, bipartite = 1)

	# Project bipartite graph
	anat_projection = networkx.preferential_attachment(bp_net, ebunch = itertools.combinations(anat_nodes, 2))
	#behav_projection = bipartite.spectral_bipartivity(bp_net, weight = "weight", nodes = behav_nodes)
	for edge in anat_projection:
		print(edge)

	# Instantiate output file
	# if not os.path.exists("{}/networks/_projected".format(path)):
	# 	os.mkdir("{}/networks/_projected".format(path))
	# anat_outfile = "{}/networks/_projected/coordinates_{}_{}_{}_func_{}_projected_anat".format(path, level, smooth_label, strategy, behav_input)
	# behav_outfile = "{}/networks/_projected/coordinates_{}_{}_{}_func_{}_projected_behav".format(path, level, smooth_label, strategy, behav_input)
	# networkx.write_graphml(anat_projection, anat_outfile + ".xml")
	# networkx.write_weighted_edgelist(anat_projection, anat_outfile + ".csv", delimiter = ",")
	# networkx.write_graphml(behav_projection, behav_outfile + ".xml")
	# networkx.write_weighted_edgelist(behav_projection, behav_outfile + ".csv", delimiter = ",")


project("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro", level = "studies", behav_input = "abstracts", sigma = 0, strategy = "probabilistic", neighbors = 100)