#!/usr/bin/python

# Created by Elizabeth Beam on 11/19/17
# Compute matrices for each study for each network to be used in bootstrap sampling studies
# Example function call: map("/path/to/psychiatlas/directory", "")

import map_all
import networkx
import os

# Function to get base of network file names
def get_dirname(input, anat = None, behav = None, func = None, behav_input = None, smooth = None, sigma = None, strategy = None, 
	threshold = None, window = None, lemmas = None, neighbors = None, level = None, scrambled = None, raw_only = None):
	base = "{}_{}_".format(input, level)
	if scrambled:
		base = "_scrambled/{}_{}_".format(path, input, level)
	if input == 'coordinates':
		if not smooth:
			base += "raw_{}_".format(strategy)
		if smooth:
			base += "gaussian_{}mm_{}_".format(sigma, strategy)
	if window:
		if window == 'sentences':
			base += "sentences_"
		elif int(window).isdigit():
			base += "window{}_".format(window)
	if lemmas:
		base += "lemmas_{}neighbors_".format(neighbors)
	if anat:
		base += "anat"
	if behav:
		base += "behav"
	if func:
		if input == "coordinates":
			base += "func_" + behav_input
		else:
			base += "func"
	return base

# Function to build graph for individual study and write to file
def export_intermed(path, intermed_dir, key, anat_mat, behav_mat, func_mat, anat = None, behav = None, func = None):
	if anat == True:
		graph = map_all.build_graph(anat_mat)
	elif behav == True:
		graph = map_all.build_graph(behav_mat)
	elif func == True:
		graph = map_all.build_graph(func_mat)
	filename = "{}/networks/_intermediary/{}/{}".format(path, intermed_dir, key)
	networkx.write_weighted_edgelist(graph, filename + ".csv", delimiter = ",")

# Function to execute mapping
def map_intermeds(path, input, anat = None, behav = None, func = None, level = None, behav_input = None, strategy = None, smooth = None,
	sigma = None, window = None, lemmas = None, neighbors = None, force = None, scrambled = None, dual_key = None):

	# Set smoothing parameter
	if sigma > 0:
		smooth = True

	# Load filename for user-specified network
	intermed_dir = get_dirname(input, anat = anat, behav = behav, func = func, behav_input = behav_input, smooth = smooth, sigma = sigma, 
		strategy = strategy, window = window, lemmas = lemmas, neighbors = neighbors, level = level)

	# Check if raw networks exist
	if force or not os.path.exists(intermed_dir):

		# Create root directory for intermediary networks
		if not os.path.exists("{}/networks/_intermediary".format(path)):
			os.mkdir("{}/networks/_intermediary".format(path))

		# Create directory for intermediaries of user-specified network
		if not os.path.exists("{}/networks/_intermediary/{}".format(path, intermed_dir)):
			os.mkdir("{}/networks/_intermediary/{}".format(path, intermed_dir))

		# Load labels and data
		anat_list = map_all.init_labs("{}/labels/harvard-oxford_anatomical_labels.txt".format(path))
		behav_list = map_all.init_labs("{}/labels/brainmap_behavioral-domain_labels.txt".format(path))
		data = map_all.load_data(path, level, dual_key = dual_key)

		# Load lemmas
		lemma_dict = {}
		if lemmas:
			lemma_dict = map_all.load_lemmas(path, neighbors)
		
		# Initialize matrices with anatomical and functional terms
		anat_mat = map_all.init_mat(anat_list, anat_list)
		behav_mat = map_all.init_mat(behav_list, behav_list)
		func_mat = map_all.init_mat(anat_list, behav_list)

		# Initialize count for tracking studies in printed messages
		count = 1
		
		# If input is abstracts, map terms from preprocessed abstracts in data dictionary
		if input == 'abstracts':
			exc_count = 0
			for key, dict in data.iteritems():
				anat_mat = map_all.init_mat(anat_list, anat_list)
				behav_mat = map_all.init_mat(behav_list, behav_list)
				func_mat = map_all.init_mat(anat_list, behav_list)
				if dict['MNI_COORDINATES']:
					if window == 'sentences':
						sentences = map_all.get_text(path, input, key, data, level).split(".")
						for sentence in sentences:
							label_list = sentence.split()
							anat_mat, behav_mat, func_mat = map_all.get_mats(anat_list, behav_list, anat_mat, behav_mat, func_mat, label_list, anat = anat, behav = behav, func = func, input = input, lemmas = lemmas, lemma_dict = lemma_dict)
					else:
						label_list = dict['ABSTRACT_PREPROC'].split()
						anat_mat, behav_mat, func_mat = map_all.get_mats(anat_list, behav_list, anat_mat, behav_mat, func_mat, label_list, anat = anat, behav = behav, func = func, input = input, lemmas = lemmas, lemma_dict = lemma_dict)
					export_intermed(path, intermed_dir, key, anat_mat, behav_mat, func_mat, anat = anat, behav = behav, func = func)
					print("Mapped {}, study {} out of {}".format(key, count, len(data.keys())))
					count += 1
				else:
					exc_count += 1
					abstract = ""
			print("Excluded {} of {} studies due to missing coordinates".format(exc_count, len(data.keys())))

		elif input == 'brainmap':
			exc_count = 0
			for key, dict in data.iteritems():
				anat_mat = map_all.init_mat(anat_list, anat_list)
				behav_mat = map_all.init_mat(behav_list, behav_list)
				func_mat = map_all.init_mat(anat_list, behav_list)
				if dict['MNI_COORDINATES']:
					label_list = map_all.get_behav('brainmap', behav_list, path, key, data, level)
					behav_mat = map_all.map_brainmap(behav_list, behav_mat, label_list, input = 'brainmap')
					export_intermed(path, intermed_dir, key, anat_mat, behav_mat, func_mat, anat = anat, behav = behav, func = func)
					print("Mapped {}, study {} out of {}".format(key, count, len(data.keys())))
					count += 1
				else:
					exc_count += 1
					brainmap = ""
			print("Excluded {} of {} studies due to missing coordinates".format(exc_count, len(data.keys())))

		# If input is texts or results, map terms from preprocessed files
		elif input in ['texts', 'results']:
			files = filter(lambda f: not f.startswith("."), os.listdir("{}/texts/{}/preproc".format(path, input)))
			for file in files:
				anat_mat = map_all.init_mat(anat_list, anat_list)
				behav_mat = map_all.init_mat(behav_list, behav_list)
				func_mat = map_all.init_mat(anat_list, behav_list)
				key = file.replace(".txt", "")
				if key in data.keys():
					if window == 'sentences':
						sentences = map_all.get_text(path, input, key, data, level).split(".")
						for sentence in sentences:
							label_list = sentence.split()
							anat_mat, behav_mat, func_mat = map_all.get_mats(anat_list, behav_list, anat_mat, behav_mat, func_mat, label_list, anat = anat, behav = behav, func = func, input = input, lemmas = lemmas, lemma_dict = lemma_dict)
					else:
						label_list = map_all.get_text(path, input, key, data, level).split()
						anat_mat, behav_mat, func_mat = map_all.get_mats(anat_list, behav_list, anat_mat, behav_mat, func_mat, label_list, anat = anat, behav = behav, func = func, input = input, lemmas = lemmas, lemma_dict = lemma_dict)
					export_intermed(path, intermed_dir, key, anat_mat, behav_mat, func_mat, anat = anat, behav = behav, func = func)
					print("Mapped {}, study {} out of {}".format(key, count, len(files)))
				elif key not in data.keys():
					print("Omitted {} from mapping due to missing key in data dictionary".format(key))
				count += 1

		# If input is coordinates, map terms from files of coords and preprocessed texts using specified behavioral input and strategy
		elif input == 'coordinates':
			if smooth:
				query_path = "{}/queries/{}/gaussian_{}mm".format(path, level, sigma)
			elif not smooth:
				query_path = "{}/queries/{}/raw".format(path, level)
			files = filter(lambda f: not f.startswith("."), os.listdir(query_path))
			for file in files:
				anat_mat = map_all.init_mat(anat_list, anat_list)
				behav_mat = map_all.init_mat(behav_list, behav_list)
				func_mat = map_all.init_mat(anat_list, behav_list)
				key = file.replace(".txt", "")
				if key in data.keys():
					coords = map_all.load_coords(query_path, file)
					behav_terms = []
					if func:
						behav_terms = map_all.get_behav(behav_input, behav_list, path, key, data, level, lemmas = lemmas, lemma_dict = lemma_dict)
					if strategy == 'winner-takes-all':
						anat_terms = [coord[0][0] for coord in coords if coord]
						anat_mat, behav_mat, func_mat = map_all.get_mats(anat_list, behav_list, anat_mat, behav_mat, func_mat, behav_terms + anat_terms, anat = anat, func = func, input = input, strategy = 'winner-takes-all', lemmas = lemmas, lemma_dict = lemma_dict)
					elif strategy == 'probabilistic':
						if func:
							behav_terms = [[[term, 100] for term in behav_terms]]
						anat_terms = []
						for coord in coords:
							anat_terms.append([label for label in coord if coord])
						anat_mat, behav_mat, func_mat = map_all.get_mats(anat_list, behav_list, anat_mat, behav_mat, func_mat, behav_terms + anat_terms, anat = anat, func = func, input = input, strategy = 'probabilistic', lemmas = lemmas, lemma_dict = lemma_dict)
					elif strategy == 'probabilistic-winner-takes-all':
						if func:
							behav_terms = [[term, 100] for term in behav_terms]
						anat_terms = [coord[0] for coord in coords if coord]
						anat_mat, behav_mat, func_mat = map_all.get_mats(anat_list, behav_list, anat_mat, behav_mat, func_mat, behav_terms + anat_terms, anat = anat, func = func, input = input, strategy = 'probabilistic-winner-takes-all', lemmas = lemmas, lemma_dict = lemma_dict)
					export_intermed(path, intermed_dir, key, anat_mat, behav_mat, func_mat, anat = anat, behav = behav, func = func)
					print("Mapped {}, study {} out of {}".format(key, count, len(files)))
				elif key not in data.keys():
					print("Omitted {} from mapping due to missing key in data dictionary".format(key))
				count += 1
			
		# Print message with path to output
		map_all.print_message(path, input, smooth, sigma, 0)

# Parameters for mapping
inputs = ["abstracts", "texts", "results"]
lemmas = [True, None]
windows = ["sentences", None]
sigmas = [0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50]
strategies = ["winner-takes-all", "probabilistic-winner-takes-all", "probabilistic"]

# Instantiate path
path = "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/psychiatry"

# Execute mapping of intermediary networks from texts
for inp in inputs:
	for win in windows:
		for lem in lemmas:
			map_intermeds(path, inp, anat = True, level = "studies", window = win, lemmas = lem, neighbors = 100)
			map_intermeds(path, inp, behav = True, level = "studies", window = win, lemmas = lem, neighbors = 100)
			map_intermeds(path, inp, func = True, level = "studies", window = win, lemmas = lem, neighbors = 100)

# Execute mapping of intermediary networks from coordinates
for strat in strategies:
	for sig in sigmas:
		map_intermeds(path, "coordinates", anat = True, behav = None, func = None, level = "studies", strategy = strat, sigma = sig)
		#map_intermeds(path, "coordinates", anat = True, behav = None, func = None, level = "experiments", strategy = strat, sigma = sig, dual_key = True)		
		for inp in inputs:
			for lem in lemmas:
				map_intermeds(path, "coordinates", anat = None, behav = None, func = True, level = "studies", behav_input = inp, strategy = strat, sigma = sig, lemmas = lem, neighbors = 100)
				#map_intermeds(path, "coordinates", anat = None, behav = None, func = True, level = "experiments", behav_input = inp, strategy = strat, sigma = sig, lemmas = lem, neighbors = 100, dual_key = True)
						
