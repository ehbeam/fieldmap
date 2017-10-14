#! /usr/bin/env python

# Created by Elizabeth Beam on 7/9/17
# Example command: python run_psychiatlas.py -p /Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program -a mni --all

import agg_exps
import agg_studies
import argparse
import comp_stats
import fetch_texts
import map_all
import preproc_words
import preproc_coords
import sys

# Initialize parser
parser = argparse.ArgumentParser(description = "Run preprocessing, mapping, visualization, and analysis steps on abstracts and/or coordinates.")

# Arguments to run processes
parser.add_argument("--agg_data", action = "store_true", help = "Aggregate BrainMap data into a dictionary by studies")
parser.add_argument("--agg_disorders", action = "store_true", help = "Aggregate files downloaded by disorder from BrainMap")
parser.add_argument("--agg_exps", action = "store_true", help = "Aggregate BrainMap data into a dictionary by experiments")
parser.add_argument("--reform_citations", action = "store_true", help = "Reformat citations for upload to EndNote")
parser.add_argument("--fetch_abstracts", action = "store_true", help = "Fetch abstracts from PubMed")
parser.add_argument("--fetch_texts", action = "store_true", help = "Fetch PDFs of articles from PubMed")
parser.add_argument("--preproc_abstracts", action = "store_true", help = "Preprocess texts, converting ngrams to labels")
parser.add_argument("--preproc_texts", action = "store_true", help = "Extract full text from PDFs and preprocess")
parser.add_argument("--preproc_coords", action = "store_true", help = "Preprocess coordinates via FSL atlasquery")
parser.add_argument("--map", action = "store_true", help = "Map networks of term co-occurrences")
parser.add_argument("--comp_stats", action = "store_true", help = "Compute and compare network stats")

# Arguments to specify parameters
parser.add_argument("--anat", action = "store_true", help = "Map network of anatomical terms")
parser.add_argument("-b", "--behav_input", type = str, help = "Behavioral input to functional maps of coordinates")
parser.add_argument("--behav", action = "store_true", help = "Map network of behavioral terms")
parser.add_argument("-e", "--experiments", action = "store_true", help = "Preprocess or map experiments instead of studies")
parser.add_argument("-f", "--force", action = "store_true", help = "Force mapping if networks already exist")
parser.add_argument("--from_pdf", action = "store_true", help = "Extract free text from PDFs")
parser.add_argument("--func", action = "store_true", help = "Map functional network (anatomical and behavioral terms)")
parser.add_argument("-i", "--input", type = str, choices = ['abstracts', 'brainmap', 'texts', 'coordinates'], help = "Specify input data for mapping")
parser.add_argument("-l", "--level", type = str, choices = ['studies', 'experiments'], help = "Level for mapping coordinates")
parser.add_argument("--lemmas", action = "store_true", help = "Map embeddings of term lemmas")
parser.add_argument("-o", "--sigma", type = int, default = 5, help = "Sigma value for smoothing coordinates in preprocessing stage")
parser.add_argument("-p", "--path", type = str, help = "Path to the Psychiatlas directory containing a 'data' folder for input and output")
parser.add_argument("-s", "--strategy", type = str, default = None, choices = ['probabilistic', 'winner-takes-all', 'probabilistic-winner-takes-all'], help = "When mapping coordinates, specify mapping strategy: 'probabilistic' (map all regions associated with coordinate, edges weighted by probability), 'winner-takes-all' (map only the top region for each coordinate, edge weight = 1), 'probabilistic-winner-takes-all' (map only the top region for each coordinate, edge weighted by probability")
parser.add_argument("--smooth", action = "store_true", help = "Convert coordinates to Gaussian spheres")
parser.add_argument("-t", "--threshold", type = str, default = None, help = "Specify threshold level for edges")
parser.add_argument("-w", "--window", type = str, default = None, choices = ['sentences', 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20])

# Load variable with function for parsing arguments
args = parser.parse_args()

# Aggregate experiments
if args.agg_exps or (args.fetch_abstracts and args.experiments) or (args.reform_citations and args.experiments):
    print("AGGREGATING EXPERIMENTS")
    agg_exps.agg(args.path, agg_disorders = args.agg_disorders, agg_exps = args.agg_exps, reform_citations = args.reform_citations)

# Aggregate studies
if args.agg_data or args.agg_disorders or args.reform_citations:
    print("AGGREGATING STUDIES")
    agg_studies.agg(args.path, agg_disorders = args.agg_disorders, agg_data = args.agg_data, reform_citations = args.reform_citations)

# Fetch abstracts
if args.fetch_abstracts:
    if args.experiments:
        print("FETCHING ABSTRACTS BY EXPERIMENT")
        agg_exps.agg(args.path, fetch_abstracts = args.fetch_abstracts)
    elif not args.experiments:
        print("FETCHING ABSTRACTS BY STUDY")
        agg_studies.agg(args.path, fetch_abstracts = args.fetch_abstracts)

# Fetch full text
if args.fetch_texts:
    print("FETCHING FULL TEXT")
    fetch_texts.fetch(args.path)

# Preprocess abstracts
if args.preproc_abstracts:
    print("PREPROCESSING ABSTRACTS")
    preproc_words.preproc(args.path, input = 'abstracts', level = args.level)

# Preprocess texts
if args.preproc_texts:
    print("PREPROCESSING FULL TEXT")
    preproc_words.preproc(args.path, input = 'texts', from_pdf = args.from_pdf)

# Preprocess coordinates
if args.preproc_coords:
    print("PREPROCESSING COORDINATES")
    preproc_coords.preproc(args.path, smooth = args.smooth, sigma = args.sigma, level = args.level)

# Map abstracts
if args.map and args.input == 'abstracts':
    print("MAPPING ABSTRACTS")
    map_all.map(args.path, 'abstracts', anat = args.anat, behav = args.behav, func = args.func, level = args.level, threshold = args.threshold, window = args.window, lemmas = args.lemmas, force = args.force)

# Map behavioral domains
if args.map and args.input == 'brainmap':
    print("MAPPING BEHAVIORAL DOMAINS")
    map_all.map(args.path, 'brainmap', level = args.level, threshold = args.threshold, force = args.force)

# Map full texts
if args.map and args.input == 'texts':
    print("MAPPING FULL TEXT")
    map_all.map(args.path, 'texts', anat = args.anat, behav = args.behav, func = args.func, level = args.level, threshold = args.threshold, window = args.window, lemmas = args.lemmas, force = args.force)

# Map coordinates
if args.map and args.input == 'coordinates':
    print("MAPPING COORDINATES")
    if not args.level:
        print("Uh oh! Please specify -l 'studies' or 'experiments' for the level of input")
        sys.exit()
    if not args.strategy:
        print("Uh oh! Please specify -s 'probabilistic', 'winner-takes-all', or 'probabilistic-winner-takes-all' for the mapping strategy")
        sys.exit()
    if args.func and not args.behav_input:
        print("Uh oh! Please specify -b 'abstracts', 'texts', or 'brainmap' for the behavioral input")
        sys.exit()
    if args.smooth and not args.sigma:
        print("Uh oh! Please specify sigma for Gaussian smoothing with the -o or --sigma flag")
        sys.exit()
    if args.lemmas and (args.anat or args.behav_input == 'brainmap'):
        print("Uh oh! This mapping should not be performed with lemmatization")
        sys.exit()
    else:
        map_all.map(args.path, 'coordinates', anat = args.anat, func = args.func, level = args.level, behav_input = args.behav_input, strategy = args.strategy, smooth = args.smooth, sigma = args.sigma, threshold = args.threshold, window = args.window, lemmas = args.lemmas, force = args.force)

# Compute statistics
if args.comp_stats:
    print("COMPUTING STATISTICS")
    comp_stats.comp(args.path, smooth = args.smooth)

