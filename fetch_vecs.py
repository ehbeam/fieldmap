#!/usr/bin/python

# Created by Elizabeth Beam on 8/24/18
# Extracts vector embeddings meeting user-specified critiera for terms of interest

import string
import itertools

# Function for extracting vector embeddings
def get(path, infile, anat_thres = 0, func_thres = 0):

    # Instantiate infiles
    anat_labels = "{}/labels/harvard-oxford_anatomical_labels.txt".format(path)
    func_labels = "{}/labels/brainmap_behavioral-domain_labels.txt".format(path)
    embeddings = "{}/embeddings/{}".format(path, infile)

    # Instantiate outfile
    embeddings_preproc = "{}/vecs/embeddings_preproc.txt".format(path)
    open(embeddings_preproc, "w+").close()

    # Initialize list of labels
    anat_label_list = []
    func_label_list = []
    
    # Load lists of anatomical and combined labels
    with open(anat_labels) as anat_flab:
        lines = anat_flab.readlines()
        for label in lines:
            
            # Remove punctuation, convert to lowercase, replace spaces with underscores, and remove newline characters
            label = label.translate(None, string.punctuation).lower().replace(" ", "_").replace("\n", "")
            anat_label_list.append(label)

    # Load lists of functional and combined labels
    with open(func_labels) as func_flab:
        lines = func_flab.readlines()
        for label in lines:
            
            # Remove punctuation, convert to lowercase, replace spaces with underscores, and remove newline characters
            label = label.translate(None, string.punctuation).lower().replace(" ", "_").replace("\n", "")
            func_label_list.append(label)

    # Extract embeddings
    femb = open(embeddings, "r")
    fproc = open(embeddings_preproc, "a")
    for splitter, embedding in itertools.groupby(femb, lambda line: line == "\n"):
        if not splitter:
            embedding = list(embedding)
            term = embedding[0].strip()
            vec_strs = embedding[3:]
            vec_tups = []
            for entry in vec_strs:
                items = entry.split()
                if (term in anat_label_list and float(items[1]) > anat_thres) or (term in func_label_list and float(items[1]) > func_thres):
                    skip = False
                    if term not in items[0]:
                        if items[0] in "".join(anat_label_list + func_label_list).replace(term, ""):
                            skip = True
                        for label in anat_label_list + func_label_list:
                            if label in items[0]:
                                skip = True
                    if skip == False:
                        vec_tups.append(tuple(items))
            fproc.write("------------------------------\n" + term + "\n")
            for tup in vec_tups[:2]:
                fproc.write(tup[1] + "\t" + tup[0] + "\n")

get("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program", "embeddings_neuro-analogies_all_preproc_epochs15_len200.txt", anat_thres = 0.6, func_thres = 0.6)


# # Code for preprocessing to extract ngrams for words to embed
# # Convert ngrams of interest
# for n in range(4, 1, -1):
#     ngrams = nltk.ngrams(prc_list, n)
#     for ngram in ngrams:
#         gramterm = " ".join(ngram)
#         gramlem = "_".join(ngram)
                    
#         # Anatomical and behavioral labels
#         if gramlem in label_list:
#             prc_txt = prc_txt.replace(gramterm, " " + gramlem + " ")
            
#             # Brodmann areas
#             elif gramlem[:3] == "ba_" and gramlem.count("_") == 1 and gramlem[-1:].isdigit():
#                 prc_txt = prc_txt.replace(gramterm, gramlem)
#             elif gramlem[:14] == "brodmann_area_" and gramlem.count("_") == 2 and gramlem[-1:].isdigit():
#                 prc_txt = prc_txt.replace(gramterm, " " + gramlem + " ")
                
#             # Other anatomy
#             elif (gramlem[-6:] == "_gyrus" or gramlem[-7:] == "_sulcus" or gramlem[-5:] == "_area" or gramlem[-5:] == "_lobe" or gramlem[-7:] == "_lobule" or gramlem[-7:] == "_cortex" or gramlem[-8:] == "_nucleus") and gramlem.count("_") == 1:
#             prc_txt = prc_txt.replace(gramterm, " " + gramlem + " ")
#             prc_txt.replace("  ", " ")