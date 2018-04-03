#!/usr/bin/python

# Created by Elizabeth Beam on 7/18/17
# Compute node- and network-level stats and compare between networks of abstracts and coordinates
# Example function call: comp("/path/to/psychiatlas/directory")

import glob
import csv
import os
import ast
import re
import string
import itertools
import pandas
import nltk
import networkx
import numpy
from scipy import stats

# Function to add punctuation to select node labels
def presentable(node):
	node = node.replace(" ", "_")
	if node == "heschls_gyrus":
		node = "heschl's gyrus"
	if node == "parahippocampal_gyrus":
		node = "para- hippocampal gyrus"
	if node == "antisaccades":
		node = "anti-saccades"
	if node == "gonogo":
		node = "go/no-go"
	if node == "nback":
		node = "n-back"
	if node == "selfreflection":
		node = "self-reflection"
	node = node.replace("_", " ")
	return node

# Function to convert label back to original form
def unpresentable(node):
	node = node.replace("_", " ")
	if node == "heschl's gyrus":
		node = "heschls_gyrus"
	if node =="para- hippocampal gyrus":
		node = "parahippocampal_gyrus"
	if node == "anti-saccades":
		node = "antisaccades"
	if node == "go/no-go":
		node = "gonogo"
	if node == "n-back":
		node = "nback"
	if node == "self-reflection":
		node = "selfreflection"
	node = node.replace(" ", "_")
	return node

# Function for loading a list of labels
def init_labs(label_file):
	label_list = []
	with open(label_file) as flab:
		lines = flab.readlines()
		for label in lines:
			label = label.translate(None, string.punctuation).lower().replace("\n", "")
			label = presentable(label)
			label_list.append(label)
	return label_list

# Function to extract BrainMap behavioral domains
def get_behav(key, data):
	
	# Initiate set for tracking behavioral domains in study
	study_behav = set()
		
	# Load list of behavioral domains from data dictionary
	study_behav_list = []
	if data[key]['BEHAVIORAL_DOMAIN']:
		study_behav_list = ast.literal_eval(data[key]['BEHAVIORAL_DOMAIN'])
	for experiment in study_behav_list:
		behav = re.findall('[A-Z][a-z]*', experiment)
		for label in behav:
			study_behav.add(label.lower())
	
	# Convert phrases to n-grams
	if 'memory' in study_behav:
		if 'explicit' in study_behav:
			study_behav.add('explicit_memory')
			study_behav.remove('explicit')
		if 'implicit' in study_behav:
			study_behav.add('implicit_memory')
			study_behav.remove('implicit')
		if 'working' in study_behav:
			study_behav.add('working_memory')
			study_behav.remove('working')
	if 'cognition' in study_behav:
		if 'social' in study_behav:
			study_behav.add('social_cognition')
			study_behav.remove('social')
		if 'temporal' in study_behav:
			study_behav.add('temporal_cognition')
			study_behav.remove('temporal')

	# Lowercase and convert set to list
	study_behav = list(study_behav)
	return study_behav

# Function to compute node-level centrality stats
def get_node_stats(path, network, node_stats, node_stats_dict, grouped_node_stats, grouped_node_stats_dict):
	
	# Load network
	network_file = "{}/networks/{}.xml".format(path, network.replace(".", "-"))
	G_raw = networkx.read_graphml(network_file)
	G = networkx.Graph()
	fedges = []
	for (u, v) in G_raw.edges():
		if G_raw[u][v]["weight"] > 0:
			G.add_edge(u, v, weight = G_raw[u][v]["weight"])
	
	# Compute node-level stats
	node_edges = networkx.degree(G, weight = "weight")
	degree = networkx.degree_centrality(G)
	betweenness = networkx.betweenness_centrality(G, weight = "weight")
	betweenness_flow = networkx.current_flow_betweenness_centrality(G, weight = "weight")
	closeness = networkx.closeness_centrality(G)
	closeness_flow = networkx.current_flow_closeness_centrality(G, weight = "weight")
	closeness_vitality = networkx.closeness_vitality(G, weight = "weight")
	load = networkx.load_centrality(G, weight = "weight")
	clustering = networkx.clustering(G, weight = "weight")
	eccentricity = networkx.eccentricity(G)
	pagerank = networkx.pagerank(G, weight = "weight")
	
	# Update list of node-level stats
	node_stats_names = [(node_edges, 'number of edges'), (degree, 'degree'), (betweenness, 'betweenness'), (betweenness_flow, 'betweenness current flow'), (closeness, 'closeness'), (closeness_flow, 'closeness current flow'), (closeness_vitality, 'closeness vitality'), (load, 'load'), (clustering, 'clustering'), (eccentricity, 'eccentricity'), (pagerank, 'pagerank')]
	node_stats += [stat_name[1] + " in " + network for stat_name in node_stats_names]

	# Load node-level stats into dictionary
	for stat_name in node_stats_names:
		stat, name = stat_name
		network_name = stat_name[1] + " in " + network
		stat_list = []
		for label in node_stats_dict.keys():
			try:
				node_stats_dict[label][network_name] = float(stat[label])
				stat_list.append(float(stat[label]))
			except:
				stat_list.append(0)
		grouped_node_stats_dict[network][name] = stat_list

# Function to compute network-level centrality stats
def get_net_stats(path, network, net_stats, net_stats_dict):
		
	# Load network file
	network_file = "{}/networks/{}.xml".format(path, network.replace(".", "-"))
	G = networkx.read_graphml(network_file)

	# Compute network-level stats
	net_nodes = networkx.number_of_nodes(G)
	net_edges = networkx.number_of_edges(G)
	density = networkx.density(G)
	clustering = networkx.average_clustering(G)
	cliques = networkx.graph_number_of_cliques(G)
	largest_clique = networkx.graph_clique_number(G)
	diameter = networkx.diameter(G)
	radius = networkx.radius(G)
	
	# Associate stats with labels
	net_stats_names = [(net_nodes, 'number of nodes'), (net_edges, 'number of edges'), (density, 'density'), (clustering, 'clustering'), (cliques, 'number of cliques'), (largest_clique, 'largest clique'), (diameter, 'diameter'), (radius, 'radius')]
	
	# Load network stats into dictionary
	for stat_name in net_stats_names:
		stat, name = stat_name
		net_stats_dict[network][name] = float(stat)

# Function for significance stars
def get_stars(p, stats_list, corrected = None):
	stars = ""
	if corrected:
		if p < 0.001 / len(stats_list):
			stars = "***"
		elif p < 0.01 / len(stats_list):
			stars = "**"
		elif p < 0.05 / len(stats_list):
			stars = "*"
	elif not corrected:
		if p < 0.001:
			stars = "***"
		elif p < 0.01:
			stars = "**"
		elif p < 0.05:
			stars = "*"
	return stars

# Function for writing stats to CSV file
def write_stats(outfile, stats_dict, column_list):
	with open(outfile, "w+") as fout:
		output = pandas.DataFrame(stats_dict)
		output_trans = pandas.DataFrame.transpose(output)
		output_trans.to_csv(outfile, index = False, quoting = 1, columns = column_list)

# Function for computing network statistics
def comp(path, smooth = None, sigma = None):

	# Instantiate infiles
	data_file = "{}/data/data_studies.csv".format(path)
	anat_labels = "{}/labels/harvard-oxford_anatomical_labels.txt".format(path)
	behav_labels = "{}/labels/brainmap_behavioral-domain_labels.txt".format(path)
	infiles = glob.glob("{}/networks/*.xml".format(path))
	networks = [file.replace(".xml", "").replace("{}/networks/".format(path), "").replace("-", ".") for file in infiles if "studies" in file]
	
	# Instantiate outfiles
	node_file = "{}/stats/node_stats.csv".format(path)
	net_file = "{}/stats/network_stats.csv".format(path)
	t_file = "{}/stats/t-tests.csv".format(path)
	
	# Initialize dictionaries
	data = {}
	t_dict = {}
	node_stats_dict = {}
	net_stats_dict = {}
	grouped_node_stats_dict = {}
	
	# List headers for outfiles
	node_stats = ['node', 'class', 'frequency in abstracts', 'frequency in results'] # Will be updated with stats for each network
	grouped_node_stats = ['number of edges', 'degree', 'betweenness', 'betweenness current flow', 'closeness', 'closeness current flow', 'closeness vitality', 'load', 'clustering', 'eccentricity', 'pagerank']
	net_stats = ['network', 'input', 'class', 'strategy', 'sigma', 'threshold', 'number of nodes', 'number of edges', 'density', 'clustering', 'number of cliques', 'largest clique', 'diameter', 'radius']
	t_stats = ['class', 'statistic', 'network 1', 'network 2', 't', 'p', 'significance', 'p (corrected)', 'significance (corrected)', 'wilcoxon t', 'wilcoxon p', 'wilcoxon significance', 'wilcoxon p (corrected)', 'wilcoxon significance (corrected)', 'pearson r', 'pearson p', 'pearson significance', 'pearson p (corrected)', 'pearson significance (corrected)', 'spearman r', 'spearman p', 'spearman significance', 'spearman p (corrected)', 'spearman significance (corrected)']
	
	# Load data from studies
	fdat = open(data_file, "rU")
	dat_reader = csv.DictReader(fdat)
	for study_dict in dat_reader:
		study_key = study_dict['KEY']
		data.update({study_key: study_dict})
	
	# Load lists of anatomical and combined labels
	anat_list = init_labs(anat_labels)
	behav_list = init_labs(behav_labels)
	labels = anat_list + behav_list

	# Initialize node-level stats dictionary with labels representing nodes
	for label in labels:
		node_stats_dict[label] = {}
		node_stats_dict[label]['node'] = presentable(label)
		if label in anat_list:
			node_stats_dict[label]['class'] = 'anat'
		elif label in behav_list:
			node_stats_dict[label]['class'] = 'behav'

	# Initialize network stats dictionary with networks
	for network in networks:
		# net_stats_dict[network] = {}
		# net_stats_dict[network]['network'] = network
		# for i, s in enumerate(network.replace("thres", "").split("_")):
		# 	if s.isdigit():
		# 		net_stats_dict[network]['threshold'] = network.replace("thres", "").split("_").pop(i)
		# if 'threshold' not in net_stats_dict[network].keys():
		# 	net_stats_dict[network]['threshold'] = 0
		# net_stats_dict[network]['input'] = [s for s in network.split("_") if s in ["abstracts", "coords", "texts"]].pop()
		# net_stats_dict[network]['class'] = [s for s in network.split("_") if s in ["anat", "behav", "func"]].pop()
		# net_stats_dict[network]['sigma'] = "".join([s for s in network.split("_") if "mm" in s])
		# if net_stats_dict[network]['input'] == "coords" and net_stats_dict[network]['class'] != "behav":
		# 	net_stats_dict[network]['strategy'] = [s for s in network.split("_") if s in ["probabilistic", "probabilistic.winner.takes.all", "winner.takes.all"]].pop().replace(".", " ")
		grouped_node_stats_dict[network] = {}
		grouped_node_stats_dict[network]['network'] = network

	# Compute label frequencies in abstracts
	abstracts = [data[key]['ABSTRACT_PREPROC'].split() for key, value in data.iteritems()]
	abstract_words = list(itertools.chain(*abstracts))
	abstract_freqs = nltk.FreqDist(abstract_words)

	# Load abstract frequencies into dictionary
	for label in node_stats_dict.keys():
		if abstract_freqs[label.replace(" ", "_")]:
			node_stats_dict[label]['frequency in abstracts'] = float(abstract_freqs[label.replace(" ", "_")])

	# Compute label frequencies in coordinates
	coord_words = []
	for key, value in data.iteritems():
		
		# Load anatomical labels from atlasquery output files
		if smooth:
			query = "{}/queries/studies/gaussian_{}mm/{}.txt".format(path, sigma, key)
		if not smooth:
			query = "{}/queries/studies/raw/{}.txt".format(path, key)
		if not os.path.isfile(query):
			continue
		fquer = open(query, "r")
		study_coords = ast.literal_eval(fquer.read())
		for coord in study_coords:
			for label in coord:
				coord_words.append(label[0])
		coord_words += get_behav(key, data)
	coord_freqs = nltk.FreqDist(coord_words)

	# Load coordinate frequencies into dictionary
	for label in node_stats_dict.keys():
		if coord_freqs[unpresentable(label)]:
			node_stats_dict[label]['frequency in results'] = float(coord_freqs[unpresentable(label)])

	# Get stats for raw and normalized networks
	for network in networks:
		print(network)
		get_node_stats(path, network, node_stats, node_stats_dict, grouped_node_stats, grouped_node_stats_dict)
		#get_net_stats(path, network, net_stats, net_stats_dict)

	# # Compare node-level stats between networks of abstracts and coordinates
	# for stat in grouped_node_stats:
	#	 for i in range(len(networks)-1):
	#		 for j in range(i+1, len(networks)):
				
	#			 # Restrict comparison to networks mapping the same node class (anat, behav, or func)
	#			 class_i = [s for s in networks[i].split("_") if s in ["anat", "behav", "func"]]
	#			 class_j = [s for s in networks[j].split("_") if s in ["anat", "behav", "func"]]
	#			 if class_i == class_j:
	#				 t_dict[(stat, i, j)] = {}
	#				 t_dict[(stat, i, j)]['class'] = class_i.pop()
	#			 else:
	#				 continue
				
	#			 if (stat, i, j) in t_dict.keys():
					
	#				 # Load dictionary of t-test stats
	#				 t_dict[(stat, i, j)]['statistic'] = stat
	#				 t_dict[(stat, i, j)]['network 1'] = networks[i]
	#				 t_dict[(stat, i, j)]['network 2'] = networks[j]
				
	#				 with numpy.errstate(invalid = "ignore"):
						
	#					 # Perform parametric t-tests
	#					 t = stats.ttest_rel(grouped_node_stats_dict[networks[i]][stat], grouped_node_stats_dict[networks[j]][stat], nan_policy = "propagate")
	#					 t_dict[(stat, i, j)]['t'] = t.statistic
	#					 t_dict[(stat, i, j)]['p'] = t.pvalue
	#					 t_dict[(stat, i, j)]['significance'] = get_stars(t.pvalue, grouped_node_stats)
	#					 t_dict[(stat, i, j)]['p (corrected)'] = t.pvalue * len(grouped_node_stats)
	#					 t_dict[(stat, i, j)]['significance (corrected)'] = get_stars(t.pvalue, grouped_node_stats, corrected = True)
						
	#					 # Perform Wilcoxon t-tests
	#					 nonpar_t, nonpar_p = stats.wilcoxon(grouped_node_stats_dict[networks[i]][stat], grouped_node_stats_dict[networks[j]][stat])
	#					 t_dict[(stat, i, j)]['wilcoxon t'] = nonpar_t
	#					 t_dict[(stat, i, j)]['wilcoxon p'] = nonpar_p
	#					 t_dict[(stat, i, j)]['wilcoxon significance'] = get_stars(nonpar_p, grouped_node_stats)
	#					 t_dict[(stat, i, j)]['wilcoxon p (corrected)'] = nonpar_t * len(grouped_node_stats)
	#					 t_dict[(stat, i, j)]['wilcoxon signifiance (corrected)'] = get_stars(nonpar_p, grouped_node_stats, corrected = True)
						
	#					 # Compute Pearson's r
	#					 pearson_r, pearson_p = stats.pearsonr(grouped_node_stats_dict[networks[i]][stat], grouped_node_stats_dict[networks[j]][stat])
	#					 t_dict[(stat, i, j)]['pearson r'] = pearson_r
	#					 t_dict[(stat, i, j)]['pearson p'] = pearson_p
	#					 t_dict[(stat, i, j)]['pearson significance'] = get_stars(pearson_p, grouped_node_stats)
	#					 t_dict[(stat, i, j)]['pearson p (corrected)'] = pearson_p * len(grouped_node_stats)
	#					 t_dict[(stat, i, j)]['pearson significance (corrected)'] = get_stars(pearson_p, grouped_node_stats, corrected = True)
						
	#					 # Compute Spearman's r
	#					 spearman_r, spearman_p = stats.spearmanr(grouped_node_stats_dict[networks[i]][stat], grouped_node_stats_dict[networks[j]][stat])
	#					 t_dict[(stat, i, j)]['spearman r'] = spearman_r
	#					 t_dict[(stat, i, j)]['spearman p'] = spearman_p
	#					 t_dict[(stat, i, j)]['spearman significance'] = get_stars(spearman_p, grouped_node_stats)
	#					 t_dict[(stat, i, j)]['spearman p (corrected)'] = spearman_p * len(grouped_node_stats)
	#					 t_dict[(stat, i, j)]['spearman significance (corrected)'] = get_stars(spearman_p, grouped_node_stats, corrected = True)

	# Write stats to CSV files
	write_stats(node_file, node_stats_dict, node_stats)
	# write_stats(net_file, net_stats_dict, net_stats)
	# write_stats(t_file, t_dict, t_stats)

comp("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro")