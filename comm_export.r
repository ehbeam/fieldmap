# Clear workspace
rm(list=ls())

# Import libraries
library(igraph)
library(tools)

# Set working directory
setwd("/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/networks")
clust.path = "/Users/ehbeam/Dropbox/Stanford/Research/Projects/Psychiatlas/program/cogneuro/clusters"
dir.create(clust.path)
dir.create(paste(clust.path, "/nodes", sep = ""))

# Load networks
networks <- list.files(pattern = ".*csv")
#networks.brainmap <- grep("brainmap_behav", networks.all, value = TRUE)
#networks.sentences <- grep("sentences", networks.all, value = TRUE)
#networks.experiments <- grep("coords_experiments", networks.all, value = TRUE)
#networks.studies <-grep("coords_studies", networks.all, value = TRUE)
#networks.filtered <- c(networks.brainmap, networks.sentences, networks.experiments, networks.studies)

# Initialize file of metrics for node clusters
write(paste("NETWORK", "TYPE", "LEVEL", "INPUT_RAW", "BEHAV_INPUT", "INPUT", "WINDOW", "LEMMAS", "STRATEGY", "SIGMA_VIS", "SIGMA", "NODES", "EDGES", "THRESHOLD", "FAST_GREEDY_Communities", "FAST_GREEDY_Modularity", 
            "frontal_pole", "insular_cortex", "superior_frontal_gyrus", "middle_frontal_gyrus", "inferior_frontal_gyrus", "precentral_gyrus", "temporal_pole", "superior_temporal_gyrus", "middle_temporal_gyrus", "inferior_temporal_gyrus", "postcentral_gyrus", "superior_parietal_lobule", "supramarginal_gyrus", "angular_gyrus", "lateral_occipital_cortex", "intracalcarine_cortex", "frontal_medial_cortex", "supplementary_motor_cortex", "subcallosal_cortex", "paracingulate_gyrus", "cingulate_gyrus", "precuneous_cortex", "cuneal_cortex", "frontal_orbital_cortex", "parahippocampal_gyrus", "lingual_gyrus", "temporal_fusiform_cortex", "temporal_occipital_fusiform_cortex", "occipital_fusiform_gyrus", "frontal_operculum_cortex", "central_opercular_cortex", "parietal_operculum_cortex", "planum_polare", "heschls_gyrus", "planum_temporale", "supracalcarine_cortex", "occipital_pole", "brainstem", "thalamus", "caudate", "putamen", "pallidum", "hippocampus", "amygdala", "accumbens", 
            "action", "execution", "speech", "imagination", "inhibition", "motor learning", "observation", "preparation", "rest", "cognition", "attention", "language", "orthography", "phonology", "semantics", "syntax", "memory", "explicit memory", "implicit memory", "working memory", "music", "reasoning", "social cognition", "somatic", "spatial", "temporal cognition", "emotion", "anger", "anxiety", "disgust", "fear", "happiness", "humor", "sadness", "interoception", "baroregulation", "gastrointestinal", "genitourinary", "heartbeat", "hunger", "osmoregulation", "respiration", "sexuality", "sleep", "thermoregulation", "thirst", "vestibular", "perception", "audition", "gustation", "olfaction", "somesthesis", "pain", "vision", "color", "motion", "shape",
            sep = ","), 
      file = paste(clust.path, "/nodes/community_metrics_fastgreedy.csv", sep = ""), append = FALSE)

# Execute clustering of nodes and export of metrics
lapply(networks, function(g, method) {
  
  # Load and preprocess network
  file <- read.csv(g)
  dat <- as.matrix(file)
  net.raw <- graph.edgelist(dat[,1:2], directed = FALSE)
  E(net.raw)$weight = as.numeric(dat[,3])
  net <- delete_edges(net.raw, which(E(net.raw)$weight == 0))
  net <- simplify(net, remove.loops = TRUE)
  
  # Load network properties
  if (grepl("anat", g)) {type <- "anatomy"}
  if (grepl("behav", g)) {type <- "behavior"}
  if (grepl("func", g)) {type <- "function"}
  level <- "study"
  if (grepl("experiments", g)) {level <- "experiment"}
  input_raw <- gsub("_.*$", "", g)
  behav_input <- "NA"
  if (input_raw == "coords") {if (grepl("abstracts", g)) {behav_input <- "abstracts"}}
  if (input_raw == "coords") {if (grepl("brainmap", g)) {behav_input <- "brainmap"}}
  if (input_raw == "coords") {if (grepl("texts", g)) {behav_input <- "texts"}}
  input <- input_raw
  if (behav_input != "NA") {input <- paste(input_raw, " & ", behav_input, sep = "")}
  window <- "NA"
  if (input == "abstracts" | input == "texts") {window <- "all"}
  if (grepl("sentences", g)) {window <- "sentences"}
  lemmas <- "no"
  if (grepl("lemmas", g)) {lemmas <- "yes"}
  strategy <- "none"
  if (grepl("_probabilistic_", g)) {strategy <- "probabilistic"}
  if (grepl("_probabilistic-winner-takes-all_", g)) {strategy <- "probabilistic-winner-takes-all"}
  if (grepl("_winner-takes-all_", g)) {strategy <- "winner-takes-all"}
  sigma_vis <- 0
  sigma <- "NA"
  if (grepl("gaussian", g)) {sigma_vis <- gsub("(.*gaussian_)|(.)|(mm.*)", "\\2", g)}
  if (grepl("gaussian", g)) {sigma <- gsub("(.*gaussian_)|(.)|(mm.*)", "\\2", g)}
  nodes <- vcount(net)
  edges <- ecount(net)
  threshold <- "NA"
  if (grepl("thres", g)) {threshold <- sub(".*thres", "", file_path_sans_ext(g))}
  
  # Load community detection metrics
  clust.fg <- cluster_fast_greedy(net, weights = E(net)$weight)
  num.fg <- length(clust.fg)
  mod.fg <- modularity(clust.fg)
  
  # Load cluster memberships
  V(net)$community <- clust.fg$membership
  frontal_pole <- 'NA'
  insular_cortex <- 'NA'
  superior_frontal_gyrus <- 'NA'
  middle_frontal_gyrus <- 'NA'
  inferior_frontal_gyrus <- 'NA'
  precentral_gyrus <- 'NA'
  temporal_pole <- 'NA'
  superior_temporal_gyrus <- 'NA'
  middle_temporal_gyrus <- 'NA'
  inferior_temporal_gyrus <- 'NA'
  postcentral_gyrus <- 'NA'
  superior_parietal_lobule <- 'NA'
  supramarginal_gyrus <- 'NA'
  angular_gyrus <- 'NA'
  lateral_occipital_cortex <- 'NA'
  intracalcarine_cortex <- 'NA'
  frontal_medial_cortex <- 'NA'
  supplementary_motor_cortex <- 'NA'
  subcallosal_cortex <- 'NA'
  paracingulate_gyrus <- 'NA'
  cingulate_gyrus <- 'NA'
  precuneous_cortex <- 'NA'
  cuneal_cortex <- 'NA'
  frontal_orbital_cortex <- 'NA'
  parahippocampal_gyrus <- 'NA'
  lingual_gyrus <- 'NA'
  temporal_fusiform_cortex <- 'NA'
  temporal_occipital_fusiform_cortex <- 'NA'
  occipital_fusiform_gyrus <- 'NA'
  frontal_operculum_cortex <- 'NA'
  central_opercular_cortex <- 'NA'
  parietal_operculum_cortex <- 'NA'
  planum_polare <- 'NA'
  heschls_gyrus <- 'NA'
  planum_temporale <- 'NA'
  supracalcarine_cortex <- 'NA'
  occipital_pole <- 'NA'
  brainstem <- 'NA'
  thalamus <- 'NA'
  caudate <- 'NA'
  putamen <- 'NA'
  pallidum <- 'NA'
  hippocampus <- 'NA'
  amygdala <- 'NA'
  accumbens <- 'NA'
  action <- 'NA'
  execution <- 'NA'
  speech <- 'NA'
  imagination <- 'NA'
  inhibition <- 'NA'
  motor_learning <- 'NA'
  observation <- 'NA'
  preparation <- 'NA'
  rest <- 'NA'
  cognition <- 'NA'
  attention <- 'NA'
  language <- 'NA'
  orthography <- 'NA'
  phonology <- 'NA'
  semantics <- 'NA'
  syntax <- 'NA'
  memory <- 'NA'
  explicit_memory <- 'NA'
  implicit_memory <- 'NA'
  working_memory <- 'NA'
  music <- 'NA'
  reasoning <- 'NA'
  social_cognition <- 'NA'
  somatic <- 'NA'
  spatial <- 'NA'
  temporal_cognition <- 'NA'
  emotion <- 'NA'
  anger <- 'NA'
  anxiety <- 'NA'
  disgust <- 'NA'
  fear <- 'NA'
  happiness <- 'NA'
  humor <- 'NA'
  sadness <- 'NA'
  interoception <- 'NA'
  baroregulation <- 'NA'
  gastrointestinal <- 'NA'
  genitourinary <- 'NA'
  heartbeat <- 'NA'
  hunger <- 'NA'
  osmoregulation <- 'NA'
  respiration <- 'NA'
  sexuality <- 'NA'
  sleep <- 'NA'
  thermoregulation <- 'NA'
  thirst <- 'NA'
  vestibular <- 'NA'
  perception <- 'NA'
  audition <- 'NA'
  gustation <- 'NA'
  olfaction <- 'NA'
  somesthesis <- 'NA'
  pain <- 'NA'
  vision <- 'NA'
  color <- 'NA'
  motion <- 'NA'
  shape <- 'NA'
  if ('frontal pole' %in% V(net)$name) {frontal_pole <- V(net)['frontal pole']$community}
  if ('insular cortex' %in% V(net)$name) {insular_cortex <- V(net)['insular cortex']$community}
  if ('superior frontal gyrus' %in% V(net)$name) {superior_frontal_gyrus <- V(net)['superior frontal gyrus']$community}
  if ('middle frontal gyrus' %in% V(net)$name) {middle_frontal_gyrus <- V(net)['middle frontal gyrus']$community}
  if ('inferior frontal gyrus' %in% V(net)$name) {inferior_frontal_gyrus <- V(net)['inferior frontal gyrus']$community}
  if ('precentral gyrus' %in% V(net)$name) {precentral_gyrus <- V(net)['precentral gyrus']$community}
  if ('temporal pole' %in% V(net)$name) {temporal_pole <- V(net)['temporal pole']$community}
  if ('superior temporal gyrus' %in% V(net)$name) {superior_temporal_gyrus <- V(net)['superior temporal gyrus']$community}
  if ('middle temporal gyrus' %in% V(net)$name) {middle_temporal_gyrus <- V(net)['middle temporal gyrus']$community}
  if ('inferior temporal gyrus' %in% V(net)$name) {inferior_temporal_gyrus <- V(net)['inferior temporal gyrus']$community}
  if ('postcentral gyrus' %in% V(net)$name) {postcentral_gyrus <- V(net)['postcentral gyrus']$community}
  if ('superior parietal lobule' %in% V(net)$name) {superior_parietal_lobule <- V(net)['superior parietal lobule']$community}
  if ('supramarginal gyrus' %in% V(net)$name) {supramarginal_gyrus <- V(net)['supramarginal gyrus']$community}
  if ('angular gyrus' %in% V(net)$name) {angular_gyrus <- V(net)['angular gyrus']$community}
  if ('lateral occipital cortex' %in% V(net)$name) {lateral_occipital_cortex <- V(net)['lateral occipital cortex']$community}
  if ('intracalcarine cortex' %in% V(net)$name) {intracalcarine_cortex <- V(net)['intracalcarine cortex']$community}
  if ('frontal medial cortex' %in% V(net)$name) {frontal_medial_cortex <- V(net)['frontal medial cortex']$community}
  if ('supplementary motor cortex' %in% V(net)$name) {supplementary_motor_cortex <- V(net)['supplementary motor cortex']$community}
  if ('subcallosal cortex' %in% V(net)$name) {subcallosal_cortex <- V(net)['subcallosal cortex']$community}
  if ('paracingulate gyrus' %in% V(net)$name) {paracingulate_gyrus <- V(net)['paracingulate gyrus']$community}
  if ('cingulate gyrus' %in% V(net)$name) {cingulate_gyrus <- V(net)['cingulate gyrus']$community}
  if ('precuneous cortex' %in% V(net)$name) {precuneous_cortex <- V(net)['precuneous cortex']$community}
  if ('cuneal cortex' %in% V(net)$name) {cuneal_cortex <- V(net)['cuneal cortex']$community}
  if ('frontal orbital cortex' %in% V(net)$name) {frontal_orbital_cortex <- V(net)['frontal orbital cortex']$community}
  if ('parahippocampal gyrus' %in% V(net)$name) {parahippocampal_gyrus <- V(net)['parahippocampal gyrus']$community}
  if ('lingual gyrus' %in% V(net)$name) {lingual_gyrus <- V(net)['lingual gyrus']$community}
  if ('temporal fusiform cortex' %in% V(net)$name) {temporal_fusiform_cortex <- V(net)['temporal fusiform cortex']$community}
  if ('temporal occipital fusiform cortex' %in% V(net)$name) {temporal_occipital_fusiform_cortex <- V(net)['temporal occipital fusiform cortex']$community}
  if ('occipital fusiform gyrus' %in% V(net)$name) {occipital_fusiform_gyrus <- V(net)['occipital fusiform gyrus']$community}
  if ('frontal operculum cortex' %in% V(net)$name) {frontal_operculum_cortex <- V(net)['frontal operculum cortex']$community}
  if ('central opercular cortex' %in% V(net)$name) {central_opercular_cortex <- V(net)['central opercular cortex']$community}
  if ('parietal operculum cortex' %in% V(net)$name) {parietal_operculum_cortex <- V(net)['parietal operculum cortex']$community}
  if ('planum polare' %in% V(net)$name) {planum_polare <- V(net)['planum polare']$community}
  if ("heschl's gyrus" %in% V(net)$name) {heschls_gyrus <- V(net)["heschl's gyrus"]$community}
  if ('planum temporale' %in% V(net)$name) {planum_temporale <- V(net)['planum temporale']$community}
  if ('supracalcarine cortex' %in% V(net)$name) {supracalcarine_cortex <- V(net)['supracalcarine cortex']$community}
  if ('occipital pole' %in% V(net)$name) {occipital_pole <- V(net)['occipital pole']$community}
  if ('brainstem' %in% V(net)$name) {brainstem <- V(net)['brainstem']$community}
  if ('thalamus' %in% V(net)$name) {thalamus <- V(net)['thalamus']$community}
  if ('caudate' %in% V(net)$name) {caudate <- V(net)['caudate']$community}
  if ('putamen' %in% V(net)$name) {putamen <- V(net)['putamen']$community}
  if ('pallidum' %in% V(net)$name) {pallidum <- V(net)['pallidum']$community}
  if ('hippocampus' %in% V(net)$name) {hippocampus <- V(net)['hippocampus']$community}
  if ('amygdala' %in% V(net)$name) {amygdala <- V(net)['amygdala']$community}
  if ('accumbens' %in% V(net)$name) {accumbens <- V(net)['accumbens']$community}
  if ('action' %in% V(net)$name) {action <- V(net)['action']$community}
  if ('execution' %in% V(net)$name) {execution <- V(net)['execution']$community}
  if ('speech' %in% V(net)$name) {speech <- V(net)['speech']$community}
  if ('imagination' %in% V(net)$name) {imagination <- V(net)['imagination']$community}
  if ('inhibition' %in% V(net)$name) {inhibition <- V(net)['inhibition']$community}
  if ('motor learning' %in% V(net)$name) {motor_learning <- V(net)['motor learning']$community}
  if ('observation' %in% V(net)$name) {observation <- V(net)['observation']$community}
  if ('preparation' %in% V(net)$name) {preparation <- V(net)['preparation']$community}
  if ('rest' %in% V(net)$name) {rest <- V(net)['rest']$community}
  if ('cognition' %in% V(net)$name) {cognition <- V(net)['cognition']$community}
  if ('attention' %in% V(net)$name) {attention <- V(net)['attention']$community}
  if ('language' %in% V(net)$name) {language <- V(net)['language']$community}
  if ('orthography' %in% V(net)$name) {orthography <- V(net)['orthography']$community}
  if ('phonology' %in% V(net)$name) {phonology <- V(net)['phonology']$community}
  if ('semantics' %in% V(net)$name) {semantics <- V(net)['semantics']$community}
  if ('syntax' %in% V(net)$name) {syntax <- V(net)['syntax']$community}
  if ('memory' %in% V(net)$name) {memory <- V(net)['memory']$community}
  if ('explicit memory' %in% V(net)$name) {explicit_memory <- V(net)['explicit memory']$community}
  if ('implicit memory' %in% V(net)$name) {implicit_memory <- V(net)['implicit memory']$community}
  if ('working memory' %in% V(net)$name) {working_memory <- V(net)['working memory']$community}
  if ('music' %in% V(net)$name) {music <- V(net)['music']$community}
  if ('reasoning' %in% V(net)$name) {reasoning <- V(net)['reasoning']$community}
  if ('social cognition' %in% V(net)$name) {social_cognition <- V(net)['social cognition']$community}
  if ('somatic' %in% V(net)$name) {somatic <- V(net)['somatic']$community}
  if ('spatial' %in% V(net)$name) {spatial <- V(net)['spatial']$community}
  if ('temporal cognition' %in% V(net)$name) {temporal_cognition <- V(net)['temporal cognition']$community}
  if ('emotion' %in% V(net)$name) {emotion <- V(net)['emotion']$community}
  if ('anger' %in% V(net)$name) {anger <- V(net)['anger']$community}
  if ('anxiety' %in% V(net)$name) {anxiety <- V(net)['anxiety']$community}
  if ('disgust' %in% V(net)$name) {disgust <- V(net)['disgust']$community}
  if ('fear' %in% V(net)$name) {fear <- V(net)['fear']$community}
  if ('happiness' %in% V(net)$name) {happiness <- V(net)['happiness']$community}
  if ('humor' %in% V(net)$name) {humor <- V(net)['humor']$community}
  if ('sadness' %in% V(net)$name) {sadness <- V(net)['sadness']$community}
  if ('interoception' %in% V(net)$name) {interoception <- V(net)['interoception']$community}
  if ('baroregulation' %in% V(net)$name) {baroregulation <- V(net)['baroregulation']$community}
  if ('gastrointestinal' %in% V(net)$name) {gastrointestinal <- V(net)['gastrointestinal']$community}
  if ('genitourinary' %in% V(net)$name) {genitourinary <- V(net)['genitourinary']$community}
  if ('heartbeat' %in% V(net)$name) {heartbeat <- V(net)['heartbeat']$community}
  if ('hunger' %in% V(net)$name) {hunger <- V(net)['hunger']$community}
  if ('osmoregulation' %in% V(net)$name) {osmoregulation <- V(net)['osmoregulation']$community}
  if ('respiration' %in% V(net)$name) {respiration <- V(net)['respiration']$community}
  if ('sexuality' %in% V(net)$name) {sexuality <- V(net)['sexuality']$community}
  if ('sleep' %in% V(net)$name) {sleep <- V(net)['sleep']$community}
  if ('thermoregulation' %in% V(net)$name) {thermoregulation <- V(net)['thermoregulation']$community}
  if ('thirst' %in% V(net)$name) {thirst <- V(net)['thirst']$community}
  if ('vestibular' %in% V(net)$name) {vestibular <- V(net)['vestibular']$community}
  if ('perception' %in% V(net)$name) {perception <- V(net)['perception']$community}
  if ('audition' %in% V(net)$name) {audition <- V(net)['audition']$community}
  if ('gustation' %in% V(net)$name) {gustation <- V(net)['gustation']$community}
  if ('olfaction' %in% V(net)$name) {olfaction <- V(net)['olfaction']$community}
  if ('somesthesis' %in% V(net)$name) {somesthesis <- V(net)['somesthesis']$community}
  if ('pain' %in% V(net)$name) {pain <- V(net)['pain']$community}
  if ('vision' %in% V(net)$name) {vision <- V(net)['vision']$community}
  if ('color' %in% V(net)$name) {color <- V(net)['color']$community}
  if ('motion' %in% V(net)$name) {motion <- V(net)['motion']$community}
  if ('shape' %in% V(net)$name) {shape <- V(net)['shape']$community}
                                            
  # Export metrics
  row <- paste(g, type, level, input_raw, behav_input, input, window, lemmas, strategy, sigma_vis, sigma, nodes, edges, threshold, num.fg, mod.fg, 
               frontal_pole, insular_cortex, superior_frontal_gyrus, middle_frontal_gyrus, inferior_frontal_gyrus, precentral_gyrus, temporal_pole, superior_temporal_gyrus, middle_temporal_gyrus, inferior_temporal_gyrus, postcentral_gyrus, superior_parietal_lobule, supramarginal_gyrus, angular_gyrus, lateral_occipital_cortex, intracalcarine_cortex, frontal_medial_cortex, supplementary_motor_cortex, subcallosal_cortex, paracingulate_gyrus, cingulate_gyrus, precuneous_cortex, cuneal_cortex, frontal_orbital_cortex, parahippocampal_gyrus, lingual_gyrus, temporal_fusiform_cortex, temporal_occipital_fusiform_cortex, occipital_fusiform_gyrus, frontal_operculum_cortex, central_opercular_cortex, parietal_operculum_cortex, planum_polare, heschls_gyrus, planum_temporale, supracalcarine_cortex, occipital_pole, brainstem, thalamus, caudate, putamen, pallidum, hippocampus, amygdala, accumbens, 
               action, execution, speech, imagination, inhibition, motor_learning, observation, preparation, rest, cognition, attention, language, orthography, phonology, semantics, syntax, memory, explicit_memory, implicit_memory, working_memory, music, reasoning, social_cognition, somatic, spatial, temporal_cognition, emotion, anger, anxiety, disgust, fear, happiness, humor, sadness, interoception, baroregulation, gastrointestinal, genitourinary, heartbeat, hunger, osmoregulation, respiration, sexuality, sleep, thermoregulation, thirst, vestibular, perception, audition, gustation, olfaction, somesthesis, pain, vision, color, motion, shape,
               sep = ",")
  write(row, file = paste(clust.path, "/nodes/community_metrics_fastgreedy.csv", sep = ""), append = TRUE)
  
})
