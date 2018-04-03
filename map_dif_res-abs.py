#!/usr/bin/python

# Created by Elizabeth Beam on 7/9/17
# Compute an xml matrix of anatomical term co-occurrences within full text of articles (abstracts removed)
# Example function call: map("/path/to/psychiatlas/directory", "abstracts", anat = True)

import ast
import csv
import itertools
import map_all
import networkx
import nltk
import os
import re
import string

# Function to get base of network file names
def get_dif_filename(path, input, anat = None, behav = None, func = None, behav_input = None, smooth = None, sigma = None, strategy = None, 
    threshold = None, window = None, lemmas = None, neighbors = None, level = None, scrambled = None, raw_only = None):
    base = "{}/networks/_results-abstracts/{}_{}_".format(path, input, level)
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
    if anat == True:
        base += "anat"
    if behav == True:
        base += "behav"
    if func == True:
        base += "func"
        if input == "coordinates":
            base += "_" + behav_input
    return base

# Function to build graphs and write to files
def export(path, matrix, behav_input = None, threshold = None, type = None, input = None, smooth = None, sigma = None, strategy = None, window = None, lemmas = None, neighbors = None, level = None, scrambled = None):
    graph = map_all.build_graph(matrix, threshold = threshold)
    if type == "anat":
        filename = get_dif_filename(path, input, anat = True, behav_input = behav_input, smooth = smooth, sigma = sigma, strategy = strategy, threshold = threshold, window = window, lemmas = lemmas, neighbors = neighbors, level = level, scrambled = scrambled)
    if type == "behav":
        filename = get_dif_filename(path, input, behav = True, behav_input = behav_input, smooth = smooth, sigma = sigma, strategy = strategy, threshold = threshold, window = window, lemmas = lemmas, neighbors = neighbors, level = level, scrambled = scrambled)
    if type == "func":
        filename = get_dif_filename(path, input, func = True, behav_input = behav_input, smooth = smooth, sigma = sigma, strategy = strategy, threshold = threshold, window = window, lemmas = lemmas, neighbors = neighbors, level = level, scrambled = scrambled)
    networkx.write_graphml(graph, filename + ".xml")
    networkx.write_weighted_edgelist(graph, filename + ".csv", delimiter = ",")


# Function to execute the full mapping procedure
def map_dif(path, anat = None, behav = None, func = None, level = None, behav_input = None, strategy = None, smooth = None, 
    sigma = None, threshold = None, window = None, lemmas = None, neighbors = None, force = None, scrambled = None, dual_key = None):

    # Create directory for networks if it does not exist
    if not os.path.exists("{}/networks/_results-abstracts".format(path)):
        os.mkdir("{}/networks/_results-abstracts".format(path))

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
    exc_count = 0

    # Iterate over keys in the data, skipping those with missing coordinates
    for key, dict in data.iteritems():
        if dict['MNI_COORDINATES']:

            # Load connections from results
            res_anat_mat = map_all.init_mat(anat_list, anat_list)
            res_behav_mat = map_all.init_mat(behav_list, behav_list)
            res_func_mat = map_all.init_mat(anat_list, behav_list)
            res_list = map_all.get_text(path, "results", key, data, level).split(".")
            for sentence in res_list:
                label_list = sentence.split()
                res_anat_mat, res_behav_mat, res_func_mat = map_all.get_mats(anat_list, behav_list, res_anat_mat, res_behav_mat, res_func_mat, label_list, anat = anat, behav = behav, func = func, input = "abstracts", lemmas = lemmas, lemma_dict = lemma_dict)

            # Load connections from abstract
            abs_anat_mat = map_all.init_mat(anat_list, anat_list)
            abs_behav_mat = map_all.init_mat(behav_list, behav_list)
            abs_func_mat = map_all.init_mat(anat_list, behav_list)
            abs_list = map_all.get_text(path, "abstracts", key, data, level).split(".")
            for sentence in abs_list:
                label_list = sentence.split()
                abs_anat_mat, abs_behav_mat, abs_func_mat = map_all.get_mats(anat_list, behav_list, abs_anat_mat, abs_behav_mat, abs_func_mat, label_list, anat = anat, behav = behav, func = func, input = "results", lemmas = lemmas, lemma_dict = lemma_dict)

            # Load matrices with connections that appear in results but not abstracts
            for edge, weight in res_anat_mat.iteritems():
                if weight > 0:
                    if abs_anat_mat[edge] == 0:
                        anat_mat[edge] += 1
            for edge, weight in res_behav_mat.iteritems():
                if weight > 0:
                    if abs_behav_mat[edge] == 0:
                        behav_mat[edge] += 1
            for edge, weight in res_func_mat.iteritems():
                if weight > 0:
                    if abs_func_mat[edge] == 0:
                        func_mat[edge] += 1

        # Print message to console
            print("Mapped {}, study {} out of {}".format(key, count, len(data.keys())))
            count += 1
        else:
            exc_count += 1
            abstract = ""
    print("Excluded {} of {} studies due to missing coordinates".format(exc_count, len(data.keys())))

    # Build graphs and write to files
    if anat:
        export(path, anat_mat, threshold = threshold, type = 'anat', input = "results-abstracts", smooth = smooth, sigma = sigma, strategy = strategy, window = window, lemmas = lemmas, neighbors = neighbors, level = level, scrambled = scrambled)
    if behav:
        export(path, behav_mat, threshold = threshold, type = 'behav', input = "results-abstracts", smooth = smooth, sigma = sigma, strategy = strategy, window = window, lemmas = lemmas, neighbors = neighbors, level = level, scrambled = scrambled)
    if func:
        export(path, func_mat, behav_input = behav_input, threshold = threshold, type = 'func', input = "results-abstracts", smooth = smooth, sigma = sigma, strategy = strategy, window = window, lemmas = lemmas, neighbors = neighbors, level = level, scrambled = scrambled)
        
    # Print message with path to output
    map_all.print_message(path, input, smooth, sigma, threshold)

# Execute mapping
path = "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro"
map_dif(path, anat = True, level = "studies", window = "sentences", lemmas = True, neighbors = 100)
map_dif(path, behav = True, level = "studies", window = "sentences", lemmas = True, neighbors = 100)
map_dif(path, func = True, level = "studies", window = "sentences", lemmas = True, neighbors = 100)


