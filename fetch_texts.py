#!/usr/bin/python

# Created by Elizabeth Beam on 7/28/17
# Download PDFs of articles from PubMed
# Prerequisite installation:
#   - PubMed Batch Download Master - https://github.com/billgreenwald/Pubmed-Batch-Download/blob/master/README.md
# Example function call: fetch("/path/to/psychiatlas/directory")

import csv
import httplib2
import os
import pandas
import shlex
import subprocess
from time import sleep

# Function for aggregating data into an array
def fetch(path):

    # Instantiate infiles
    data_file = "{}/data/data.csv".format(path)
    
    # Instantiate path to outfiles
    pdf_path = "{}/texts/pdf".format(path)
    if not os.path.isdir(pdf_path):
        os.makedirs(pdf_path)

    # Load dictionary from data file
    data = {}
    fdat = open(data_file, "rU")
    dat_reader = csv.DictReader(fdat)
    for study_dict in dat_reader:
        study_key = study_dict['KEY']
        data.update({study_key: study_dict})

    # First-pass fetch of PDFs from PubMed via wget
    http = httplib2.Http(".cache", disable_ssl_certificate_validation = True)
    for key, dict in data.iteritems():
        if 'MEDLINE' not in data[key].keys():
            continue
        id = data[key]['MEDLINE']
        if not os.path.isfile("{}/{}.pdf".format(pdf_path, id)):

            # Insert the Medline ID into the PubMed entrez url
            # url = "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/eurl.fcgi?dbfrom=pubmed&id={}&retmode=ref&cmd=prurls".format(id)
            url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&id={}&cmd=prlinks&retmode=ref".format(id)
            
            # Attempt to download pdf
            try:
                status, response = http.request(url)
                for line in response.split():
                    if "http" in line and "pdf" in line:
                        pdf_url = line.split('"')[1]
                        pdf_file = "{}/{}.pdf".format(pdf_path, id)
                        comm = "wget -O {} {}".format(pdf_file, pdf_url)
                        print(comm)
                        args = shlex.split(comm)
                        proc = subprocess.call(args)
                        
                        # Wait for PDF to download, then remove if corrupt
                        sleep(5)
                        try:
                            if os.path.getsize(pdf_file) == 0:
                                os.remove(pdf_file)
                                print("Removed empty PDF for {}".format(key))
                                continue
                            else:
                                try:
                                    PyPDF2.PdfFileReader(open(pdf_file, "rb"))
                                except PyPDF2.utils.PdfReadError:
                                    os.remove(pdf_file)
                                    print("Removed corrupt PDF for {}".format(key))
                                    continue
                        except:
                            print("Error removing empty or corrupted PDF for {}".format(key))
            except:
                print("Error retrieving PDF from PubMed for {}".format(key))

    # Second-pass fetch of PDFs via PubMed Batch Download Master
    os.chdir("{}/texts".format(path))
    script_dirs = path.split("/")[:-1]
    script_path = "/".join(script_dirs) + "/scripts"
    for key, dict in data.iteritems():
        if 'MEDLINE' not in data[key].keys():
            continue
        id = data[key]['MEDLINE']
        if not os.path.isfile("{}/{}.pdf".format(pdf_path, id)):
            comm = "ruby {}/_borrowed/Pubmed-Batch-Download-master/pubmedid2pdf.rb {}".format(script_path, id)
            args = shlex.split(comm)
            proc = subprocess.call(args)
    os.chdir(script_path)

    # Store urls and PDF download status to data
    pdf_tracker = "{}/data/data_pdfs.csv".format(path)
    for key, dict in data.iteritems():
        if 'MEDLINE' not in data[key].keys():
            continue
        id = data[key]['MEDLINE']
        url = "http://sfx.stanford.edu/local?sid=stanford:laneweb-search-pubmed&id=pmid:{}".format(id)
        data[key]['URL'] = url
        if os.path.isfile("{}/{}.pdf".format(pdf_path, id)):
            data[key]['PDF'] = 'yes'
        elif not os.path.isfile("{}/{}.pdf".format(pdf_path, id)):
            data[key]['PDF'] = 'no'
    column_list = ['KEY', '1st_AUTHOR', 'YEAR', 'JOURNAL', 'ABSTRACT', 'URL', 'MEDLINE', 'PDF']
    with open(pdf_tracker, "w+") as fout:
        output = pandas.DataFrame(data)
        output_trans = pandas.DataFrame.transpose(output)
        output_trans.to_csv(pdf_tracker, index = False, quoting = 1, columns = column_list)

