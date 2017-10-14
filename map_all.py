#!/usr/bin/python

# Created by Elizabeth Beam on 7/9/17
# Compute an xml matrix of anatomical term co-occurrences within full text of articles (abstracts removed)
# Example function call: map("/path/to/psychiatlas/directory", threshold = 100, input = "abstracts")

import ast
import csv
import itertools
import networkx
import nltk
import os
import re
import string

# Function to get base of network file names
def get_filename(path, input, type, behav_input = None, smooth = None, sigma = None, strategy = None, threshold = None, window = None, lemmas = None, level = None, raw_only = None):
    base = "{}/networks/{}_{}_".format(path, input, level)
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
        base += "lemmas_"
    base += type
    if type == 'func' and behav_input:
        base += "_" + behav_input
    if threshold and not raw_only:
        base += "_thres{}".format(threshold)
    return base

# Function to check whether networks exist
def networks_exist(path, input, anat = None, behav = None, func = None, behav_input = None, strategy = None, smooth = None, sigma = None, window = None, lemmas = None, level = None, threshold = None):
    anat_map = os.path.isfile(get_filename(path, input, 'anat', smooth = smooth, sigma = sigma, strategy = strategy, threshold = threshold, window = window, lemmas = lemmas, level = level, raw_only = True) + ".xml")
    behav_map = os.path.isfile(get_filename(path, input, 'behav', smooth = smooth, sigma = sigma, strategy = strategy, threshold = threshold, window = window, lemmas = lemmas, level = level, raw_only = True) + ".xml")
    func_map = os.path.isfile(get_filename(path, input, 'func', behav_input = behav_input, smooth = smooth, sigma = sigma, strategy = strategy, window = window, lemmas = lemmas, threshold = threshold, level = level, raw_only = True) + ".xml")
    if input == 'abstracts' or input == 'texts':
        if anat_map and behav_map and func_map:
            return True
    if input == 'coordinates':
        if anat and anat_map:
            return True
        if func and func_map:
            return True
    return None

# Function to load data dictionary
def load_data(path, level):
    data_file = "{}/data/data_studies.csv".format(path)
    if level:
        data_file = "{}/data/data_{}.csv".format(path, level)
    data = {}
    fdat = open(data_file, "rU")
    dat_reader = csv.DictReader(fdat)
    for dict in dat_reader:
        if level == 'experiments':
            key = dict['KEY'] + " " + dict['EXPERIMENT'].translate(None, string.punctuation).replace("  ", " ")
        else:
            key = dict['KEY']
        data.update({key: dict})
    return data

# Function to load a list of labels
def init_labs(label_file):
    label_list = []
    with open(label_file, "r") as flab:
        lines = flab.readlines()
        for label in lines:
            label = label.translate(None, string.punctuation).lower().replace(" ", "_").replace("\n", "")
            label_list.append(label)
    return label_list

# Function to initiate a matrix with terms from one list
def init_mat(label_list_1, label_list_2):
    matrix = {}
    for label_1 in label_list_1:
        for label_2 in label_list_2:
            matrix[(label_1, label_2)] = 0
            matrix[(label_2, label_1)] = 0
    return matrix

# Function to load a dictionary of terms and lemmas
def load_lemmas(path):
    lemma_file = "{}/vecs/lemmas_texts.csv".format(path)
    lemma_dict = {}
    flem = open(lemma_file, "rU")
    lem_reader = csv.DictReader(flem)
    for dict in lem_reader:
        key = dict['TERM']
        lemma_dict.update({key: dict})
    return lemma_dict

# Function to load terms from full text of article
def get_text(path, input, key, data, level):
    if level == 'experiments':
        file = open("{}/texts/{}/preproc/{}.txt".format(path, input, data[key]['KEY']), "r")
    else:
        file = open("{}/texts/{}/preproc/{}.txt".format(path, input, key), "r")
    return file.read().replace("\n", "")

# Function to generalize Harvard-Oxford anatomical labels
def gen_coord(label):
    if "juxtapositional_lobule_cortex" in label:
        label = label.replace("juxtapositional_lobule_cortex_(formerly_", "").replace(")", "")
    if "_(includes_h1_and_h2)" in label:
        label = label.replace("_(includes_h1_and_h2)", "")
    if "left_" in label:
        label = label.replace("left_", "")
    if "right_" in label:
        label = label.replace("right_", "")
    if "_pars_triangularis" in label:
        label = label.replace("_pars_triangularis", "")
    if "_pars_opercularis" in label:
        label = label.replace("_pars_opercularis", "")
    if "_anterior_division" in label:
        label = label.replace("_anterior_division", "")
    if "_posterior_division" in label:
        label = label.replace("_posterior_division", "")
    if "_temporooccipital_part" in label:
        label = label.replace("_temporooccipital_part", "")
    if "_superior_division" in label:
        label = label.replace("_superior_division", "")
    if "_inferior_division" in label:
        label = label.replace("_inferior_division", "")
    label = label.replace(",", "")
    return label

# Function to load behavioral terms for a study from its full text, abstract, or BrainMap behavioral domains
def get_behav(behav_input, behav_list, path, key, data, level, lemmas = None, lemma_dict = None):
    if behav_input == 'texts':
        full_text = get_text(path, behav_input, key, data, level)
        if lemmas:
            behav_terms = list(set([lemma_dict[term]['LEMMA'] for term in full_text.split() if term in lemma_dict.keys() and lemma_dict[term]['CLASS'] == "behavior"]))
        else:
            behav_terms = list(set([term for term in full_text.split() if term in behav_list]))
    elif behav_input == 'abstracts':
        if lemmas:
            behav_terms = list(set([lemma_dict[term]['LEMMA'] for term in data[key]['ABSTRACT_PREPROC'].split() if term in lemma_dict.keys() and lemma_dict[term]['CLASS'] == "behavior"]))
        else:
            behav_terms = list(set([term for term in data[key]['ABSTRACT_PREPROC'].split() if term in behav_list]))
    elif behav_input == 'brainmap':
        behav_literal = []
        try:
            behav_literal = [data[key]['BEHAVIORAL_DOMAIN']]
        except:
            print("No BrainMap behavioral domains for {}".format(key))
        behav_set = set()
        for experiment in behav_literal:
            behav_terms = experiment.replace(",", ".").split(".")
            for label in behav_terms:
                behav_set.add(label.lower())
            if 'explicit' in behav_set:
                behav_set.add('explicit_memory')
            if 'implicit' in behav_set:
                behav_set.add('implicit_memory')
            if 'working' in behav_set:
                behav_set.add('working_memory')
            if 'motor learning' in behav_set:
                behav_set.add('motor_learning')
            if 'social cognition' in behav_set:
                behav_set.add('social_cognition')
            if 'respiration regulation' in behav_set:
                behav_set.add('respiration')
            if 'gastrointestinal/genitourinary (gi/gu)' in behav_set:
                behav_set.add('gastrointestinal')
                behav_set.add('genitourinary')
            if 'heartbeat detection' in behav_set:
                behav_set.add('heartbeat')
            if 'temporal' in behav_set:
                behav_set.add('temporal_cognition')
        behav_terms = [term for term in behav_set if term in behav_list]
    return behav_terms

# Function to map an edge with probabilistic weight
def link_prob(matrix, edge):
    node_1, node_2 = edge
    edge_labels = (gen_coord(node_1[0]), gen_coord(node_2[0]))
    if edge_labels in matrix.keys() and edge_labels[0] != edge_labels[1]:
        matrix[edge_labels] += float(node_1[1]) * float(node_2[1]) * 0.00005
        matrix[edge_labels[::-1]] += float(node_1[1]) * float(node_2[1]) * 0.00005

# Function for loading a matrix of links within one semantic class
def load_mat(matrix, type = None, nodes = None, anat_nodes = None, behav_nodes = None, input = None, strategy = None):
    if strategy == 'probabilistic':
        if type == 'anat':
            for i, coord in enumerate(nodes):
                others = list(itertools.chain(*[other for other in nodes]))
                for edge in list(itertools.product(coord, others)):
                    link_prob(matrix, edge)
        elif type == 'func':
            anat_nodes = list(itertools.chain(*anat_nodes))
            for edge in list(itertools.product(anat_nodes, behav_nodes)):
                link_prob(matrix, edge)
    elif strategy == 'probabilistic-winner-takes-all':
        for edge in list(itertools.product(nodes, nodes)):
            link_prob(matrix, edge)
    else:
        for edge in list(itertools.product(nodes, nodes)):
            node_1, node_2 = edge
            edge_labels = (gen_coord(node_1), gen_coord(node_2))
            if edge_labels in matrix.keys() and edge_labels[0] != edge_labels[1]:
                matrix[edge_labels] += 0.5
                matrix[edge_labels[::-1]] += 0.5
    return matrix

# Function to load undirected links for anatomical network from a single study or experiment
def map_anat(anat_list, anat_mat, label_list, input = None, strategy = None, lemmas = None, lemma_dict = None):
    if lemmas and input in ['abstracts', 'texts']:
        anat_nodes = list(set([lemma_dict[gen_coord(term)]['LEMMA'] for term in label_list if gen_coord(term) in lemma_dict.keys() and lemma_dict[gen_coord(term)]['CLASS'] == "anatomy"]))
    else:
        if input in ['abstracts', 'texts']:
            anat_nodes = list(set([term for term in label_list if term in anat_list]))
        elif strategy == 'winner-takes-all':
            anat_nodes = list(set([gen_coord(term) for term in label_list if gen_coord(term) in anat_list]))
        elif strategy == 'probabilistic':
            anat_nodes = []
            for labels in label_list:
                anat_nodes.append([tuple([gen_coord(term[0]), term[1]]) for term in labels if gen_coord(term[0]) in anat_list])
        elif strategy == 'probabilistic-winner-takes-all':
            anat_nodes = [tuple([gen_coord(term[0]), term[1]]) for term in label_list if gen_coord(term[0]) in anat_list]
    return load_mat(anat_mat, nodes = anat_nodes, type = 'anat', input = input, strategy = strategy)

# Function to load undirected links for behavioral network from a single study or experiment
def map_behav(behav_list, behav_mat, label_list, input = None, strategy = None, lemmas = None, lemma_dict = None):
    if lemmas and input in ['abstracts', 'texts']:
        behav_nodes = list(set([lemma_dict[term]['LEMMA'] for term in label_list if term in lemma_dict.keys() and lemma_dict[term]['CLASS'] == "behavior"]))
    else:
        behav_nodes = list(set([term for term in label_list if term in behav_list]))
    return load_mat(behav_mat, nodes = behav_nodes, type = 'behav', input = input, strategy = strategy)

# Function to load undirected links for functional network from a single study or experiment
def map_func(anat_list, behav_list, func_mat, label_list, input = None, strategy = None, lemmas = None, lemma_dict = None):
    if lemmas:
        if input in ['abstracts', 'texts']:
            anat_nodes = list(set([lemma_dict[term]['LEMMA'] for term in label_list if term in lemma_dict.keys() and lemma_dict[term]['CLASS'] == "anatomy"]))
            behav_nodes = list(set([lemma_dict[term]['LEMMA'] for term in label_list if term in lemma_dict.keys() and lemma_dict[term]['CLASS'] == "behavior"]))
        elif strategy == 'winner-takes-all':
            anat_nodes = list(set([gen_coord(term) for term in label_list if gen_coord(term) in anat_list]))
            behav_nodes = list(set([lemma_dict[term]['LEMMA'] for term in label_list if term in lemma_dict.keys() and lemma_dict[term]['CLASS'] == "behavior"]))
        elif strategy == 'probabilistic':
            anat_nodes = []
            behav_nodes = []
            for labels in label_list:
                anat_nodes.append([tuple([gen_coord(term[0]), term[1]]) for term in labels if gen_coord(term[0]) in anat_list])
                behav_nodes.append([tuple([lemma_dict[term[0]]['LEMMA'], term[1]]) for term in labels if term[0] in lemma_dict.keys() and lemma_dict[term[0]]['CLASS'] == "behavior"])
            behav_nodes = list(set(behav_nodes[0]))
        elif strategy == 'probabilistic-winner-takes-all':
            anat_nodes = [tuple([gen_coord(term[0]), term[1]]) for term in label_list if gen_coord(term[0]) in anat_list]
            behav_nodes = list(set([tuple([lemma_dict[term[0]]['LEMMA'], term[1]]) for term in label_list if term[0] in lemma_dict.keys() and lemma_dict[term[0]]['CLASS'] == "behavior"]))
    else:
        if input in ['abstracts', 'texts']:
            anat_nodes = list(set([term for term in label_list if term in anat_list]))
            behav_nodes = list(set([term for term in label_list if term in behav_list]))
        elif strategy == 'winner-takes-all':
            anat_nodes = list(set([gen_coord(term) for term in label_list if gen_coord(term) in anat_list]))
            behav_nodes = list(set([term for term in label_list if term in behav_list]))
        elif strategy == 'probabilistic':
            anat_nodes = []
            behav_nodes = []
            for labels in label_list:
                anat_nodes.append([tuple([gen_coord(term[0]), term[1]]) for term in labels if gen_coord(term[0]) in anat_list])
                behav_nodes.append([tuple(term) for term in labels if term[0] in behav_list])
            behav_nodes = list(set(behav_nodes[0]))
        elif strategy == 'probabilistic-winner-takes-all':
            anat_nodes = [tuple([gen_coord(term[0]), term[1]]) for term in label_list if gen_coord(term[0]) in anat_list]
            behav_nodes = list(set([tuple(term) for term in label_list if term[0] in behav_list]))
    return load_mat(func_mat, nodes = anat_nodes + behav_nodes, anat_nodes = anat_nodes, behav_nodes = behav_nodes, type = 'func', input = input, strategy = strategy)

# Function to load undirected links between BrainMap behavioral domains in a single study or experiment
def map_brainmap(behav_list, behav_mat, label_list, input = None):
    behav_nodes = list(set([term for term in label_list if term in behav_list]))
    behav_mat = load_mat(behav_mat, nodes = behav_nodes, type = 'behav', input = input, strategy = 'winner-takes-all')
    return behav_mat

# Function to load matrices for anatomical, behavioral, and functional networks
def get_mats(anat_list, behav_list, anat_mat, behav_mat, func_mat, label_list, anat = None, behav = None, func = None, input = None, lemmas = None, lemma_dict = None, strategy = None):
    if anat:
        anat_mat = map_anat(anat_list, anat_mat, label_list, input = input, strategy = strategy, lemmas = lemmas, lemma_dict = lemma_dict)
    if behav:
        behav_mat = map_behav(behav_list, behav_mat, label_list, input = input, lemmas = lemmas, lemma_dict = lemma_dict)
    if func:
        func_mat = map_func(anat_list, behav_list, func_mat, label_list, input = input, strategy = strategy, lemmas = lemmas, lemma_dict = lemma_dict)
    return anat_mat, behav_mat, func_mat

# Function to determine threshold for connection weights
def find_thres(threshold, matrix):
    weights = matrix.values()
    weights.sort()
    try:
        threshold_weight = weights[-int(threshold)]
    except IndexError:
        threshold_weight = 0
    return threshold_weight

# Function to add punctuation to select node labels
def presentable(node):
    if node == "heschls_gyrus":
        node = "heschl's gyrus"
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

# Function to convert co-occurrence matrix to graph and optional thresholded graph
def build_graph(matrix, threshold = None):
    G = networkx.Graph()
    if threshold:
        threshold_weight = find_thres(threshold, matrix)
    for edge, weight in matrix.iteritems():
        edge = list(edge)
        node1 = presentable(edge[0])
        node2 = presentable(edge[1])
        if threshold:
            if weight > threshold_weight:
                G.add_edge(node1, node2, weight = weight)
        else:
            G.add_edge(node1, node2, weight = weight)
    return G

# Function to build graphs and write to files
def export(path, matrix, behav_input = None, threshold = None, type = None, input = None, smooth = None, sigma = None, strategy = None, window = None, lemmas = None, level = None):
    graph = build_graph(matrix, threshold = threshold)
    filename = get_filename(path, input, type, behav_input = behav_input, smooth = smooth, sigma = sigma, strategy = strategy, threshold = threshold, window = window, lemmas = lemmas, level = level)
    networkx.write_graphml(graph, filename + ".xml")
    networkx.write_weighted_edgelist(graph, filename + ".csv", delimiter = ",")

# Function to print message to consolue upon completion
def print_message(path, input, smooth, sigma, threshold):
    print("All done! Check out your co-occurrence mappings of {} in {}/networks".format(input, path))
    if smooth:
        print("Coordinates were smoothed with a Gaussian kernel, sigma = {} mm".format(sigma))
    if threshold:
        print("Threshold of {} was applied".format(threshold))

# Function to execute the full mapping procedure
def map(path, input, anat = None, behav = None, func = None, level = None, behav_input = None, strategy = None, smooth = None, sigma = None, threshold = None, window = None, lemmas = None, force = None):

    # Check if raw networks exist
    if force or not networks_exist(path, input, anat = anat, behav = behav, func = func, behav_input = behav_input, strategy = strategy, smooth = smooth, sigma = sigma, window = window, lemmas = lemmas, level = level, threshold = threshold):
        
        # Instantiate labels
        anat_labels = "{}/labels/harvard-oxford_anatomical_labels.txt".format(path)
        behav_labels = "{}/labels/brainmap_behavioral-domain_labels.txt".format(path)

        # Load labels and data
        anat_list = init_labs(anat_labels)
        behav_list = init_labs(behav_labels)
        data = load_data(path, level)

        # Load lemmas
        lemma_dict = {}
        if lemmas:
            lemma_dict = load_lemmas(path)
        
        # Initialize matrices with anatomical and functional terms
        anat_mat = init_mat(anat_list, anat_list)
        behav_mat = init_mat(behav_list, behav_list)
        func_mat = init_mat(anat_list, behav_list)
    
        # Initialize count for tracking studies in printed messages
        count = 1
        
        # If input is abstracts, map terms from preprocessed abstracts in data dictionary
        if input == 'abstracts':
            exc_count = 0
            for key, dict in data.iteritems():
                if dict['MNI_COORDINATES']:
                    if window == 'sentences':
                        sentences = get_text(path, input, key, data, level).split(".")
                        for sentence in sentences:
                            label_list = sentence.split()
                            anat_mat, behav_mat, func_mat = get_mats(anat_list, behav_list, anat_mat, behav_mat, func_mat, label_list, anat = anat, behav = behav, func = func, input = input, lemmas = lemmas, lemma_dict = lemma_dict)
                    else:
                        label_list = dict['ABSTRACT_PREPROC'].split()
                        anat_mat, behav_mat, func_mat = get_mats(anat_list, behav_list, anat_mat, behav_mat, func_mat, label_list, anat = anat, behav = behav, func = func, input = input, lemmas = lemmas, lemma_dict = lemma_dict)
                    print("Mapped {}, study {} out of {}".format(key, count, len(data.keys())))
                    count += 1
                else:
                    exc_count += 1
                    abstract = ""
            print("Excluded {} of {} studies due to missing coordinates".format(exc_count, len(data.keys())))

        elif input == 'brainmap':
            exc_count = 0
            for key, dict in data.iteritems():
                if dict['MNI_COORDINATES']:
                    label_list = get_behav('brainmap', behav_list, path, key, data, level)
                    behav_mat = map_brainmap(behav_list, behav_mat, label_list, input = 'brainmap')
                    print("Mapped {}, study {} out of {}".format(key, count, len(data.keys())))
                    count += 1
                else:
                    exc_count += 1
                    brainmap = ""
            print("Excluded {} of {} studies due to missing coordinates".format(exc_count, len(data.keys())))

        # If input is texts, map terms from files of preprocessed texts
        elif input == 'texts':
            files = filter(lambda f: not f.startswith("."), os.listdir("{}/texts/texts/preproc".format(path)))
            for file in files:
                key = file.replace(".txt", "")
                if key in data.keys():
                    if window == 'sentences':
                        sentences = get_text(path, input, key, data, level).split(".")
                        for sentence in sentences:
                            label_list = sentence.split()
                            anat_mat, behav_mat, func_mat = get_mats(anat_list, behav_list, anat_mat, behav_mat, func_mat, label_list, anat = anat, behav = behav, func = func, input = input, lemmas = lemmas, lemma_dict = lemma_dict)
                    else:
                        label_list = get_text(path, input, key, data, level).split()
                        anat_mat, behav_mat, func_mat = get_mats(anat_list, behav_list, anat_mat, behav_mat, func_mat, label_list, anat = anat, behav = behav, func = func, input = input, lemmas = lemmas, lemma_dict = lemma_dict)
                    print("Mapped {}, study {} out of {}".format(key, count, len(files)))
                elif key not in data.keys():
                    print("Omitted {} from mapping due to missing key in data dictionary".format(key))
                count += 1

        # If input is coordinates, map terms from files of coords and preprocessed texts using specified behavioral input and strategy
        elif input == 'coordinates':
            if smooth:
                query_path = "{}/queries/{}/gaussian_{}mm".format(path, level, sigma)
                files = filter(lambda f: not f.startswith("."), os.listdir(query_path))
            elif not smooth:
                query_path = "{}/queries/{}/raw".format(path, level)
                files = filter(lambda f: not f.startswith("."), os.listdir(query_path))
            for file in files:
                key = file.replace(".txt", "")
                if key in data.keys():
                    behav_terms = []
                    if func:
                        behav_terms = get_behav(behav_input, behav_list, path, key, data, level, lemmas = lemmas, lemma_dict = lemma_dict)
                    coords = ast.literal_eval(open("{}/{}".format(query_path, file), "r").read())
                    if strategy == 'winner-takes-all':
                        anat_terms = [coord[0][0] for coord in coords if coord]
                        anat_mat, behav_mat, func_mat = get_mats(anat_list, behav_list, anat_mat, behav_mat, func_mat, behav_terms + anat_terms, anat = anat, func = func, input = input, strategy = 'winner-takes-all', lemmas = lemmas, lemma_dict = lemma_dict)
                    elif strategy == 'probabilistic':
                        if func:
                            behav_terms = [[[term, 100] for term in behav_terms]]
                        anat_terms = []
                        for coord in coords:
                            anat_terms.append([label for label in coord if coord])
                        anat_mat, behav_mat, func_mat = get_mats(anat_list, behav_list, anat_mat, behav_mat, func_mat, behav_terms + anat_terms, anat = anat, func = func, input = input, strategy = 'probabilistic', lemmas = lemmas, lemma_dict = lemma_dict)
                    elif strategy == 'probabilistic-winner-takes-all':
                        if func:
                            behav_terms = [[term, 100] for term in behav_terms]
                        anat_terms = [coord[0] for coord in coords if coord]
                        anat_mat, behav_mat, func_mat = get_mats(anat_list, behav_list, anat_mat, behav_mat, func_mat, behav_terms + anat_terms, anat = anat, func = func, input = input, strategy = 'probabilistic-winner-takes-all', lemmas = lemmas, lemma_dict = lemma_dict)
                    print("Mapped {}, study {} out of {}".format(key, count, len(files)))
                elif key not in data.keys():
                    print("Omitted {} from mapping due to missing key in data dictionary".format(key))
                count += 1

        # Build graphs and write to files
        if anat:
            export(path, anat_mat, threshold = threshold, type = 'anat', input = input, smooth = smooth, sigma = sigma, strategy = strategy, window = window, lemmas = lemmas, level = level)
        if behav or input == 'brainmap':
            export(path, behav_mat, threshold = threshold, type = 'behav', input = input, smooth = smooth, sigma = sigma, strategy = strategy, window = window, lemmas = lemmas, level = level)
        if func:
            export(path, func_mat, behav_input = behav_input, threshold = threshold, type = 'func', input = input, smooth = smooth, sigma = sigma, strategy = strategy, window = window, lemmas = lemmas, level = level)
            
        # Print message with path to output
        print_message(path, input, smooth, sigma, threshold)

    # If raw networks available and threshold specified, generate thresholded networks
    elif threshold and networks_exist(path, input, anat = anat, behav = behav, func = func, behav_input = behav_input, strategy = strategy, smooth = smooth, sigma = sigma, window = window, lemmas = lemmas, level = level, threshold = threshold):
        
        # Print message that networks found
        print("Raw networks found in {}/networks".format(path))
    
        # Raw network file names
        anat_raw = get_filename(path, input, 'anat', smooth = smooth, sigma = sigma, strategy = strategy, window = window, lemmas = lemmas, level = level)
        behav_raw = get_filename(path, input, 'behav', smooth = smooth, sigma = sigma, strategy = strategy, window = window, lemmas = lemmas, level = level)
        func_raw = get_filename(path, input, 'func', behav_input = behav_input, smooth = smooth, sigma = sigma, strategy = strategy, window = window, lemmas = lemmas, level = level)
        
        # Thresholded network file names
        anat_thres = get_filename(path, input, 'anat', smooth = smooth, sigma = sigma, strategy = strategy, threshold = threshold, window = window, lemmas = lemmas, level = level)
        behav_thres = get_filename(path, input, 'behav', smooth = smooth, sigma = sigma, strategy = strategy, threshold = threshold, window = window, lemmas = lemmas, level = level)
        func_thres = get_filename(path, input, 'func', behav_input = behav_input, smooth = smooth, sigma = sigma, strategy = strategy, threshold = threshold, window = window, lemmas = lemmas, level = level)
        
        if input == 'abstracts' or input == 'texts':
            graphs = [(anat_raw, anat_thres), (behav_raw, behav_thres), (func_raw, func_thres)]
        elif input == 'coordinates':
            graphs = [(anat_raw, anat_thres), (func_raw, func_thres)]
        
        for graph in graphs:
            
            # Load graph and outfile type
            G = networkx.read_graphml(graph[0] + ".xml")
            outfile = graph[1]
            
            # Find weight for specified threshold
            weights = []
            for edge in networkx.edges_iter(G):
                weights.append(G.get_edge_data(*edge)['weight'])
            weights.sort()
            try:
                threshold_weight = weights[-int(threshold)]
            except IndexError:
                threshold_weight = 0
            
            # Build thresholded graph
            G_thres = networkx.Graph()
            for edge in networkx.edges_iter(G):
                weight = G.get_edge_data(*edge)['weight']
                if weight > threshold_weight:
                    G_thres.add_edge(edge[0], edge[1], weight = weight)
        
            # Export to GraphML file
            networkx.write_graphml(G_thres, outfile + ".xml")
    
            # Export to CSV file
            networkx.write_weighted_edgelist(G_thres, outfile + ".csv", delimiter = ",")
        
        # Print message with path to output
        print_message(path, input, smooth, sigma, threshold)

    # If raw networks available and no threshold specified, do nothing
    elif not threshold:
        if input == 'coordinates':
            print("Nothing to do! Raw networks already generated for coordinates with the {} strategy".format(strategy))
        else:
            print("Nothing to do! Raw networks already generated for {}".format(input))
        print("FYI, you can generate thresholded networks with the -t flag")

