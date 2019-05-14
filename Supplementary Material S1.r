# Formalization of odometer thinking and indices for the classification of combinatorial strategies
# Supplementary Material S1
# R script for generating random sample of solutions, calculating characteristics (i.e. error ratios, measure of SOS and POS, constancy, cyclicity, odometricality and cyclicality) of the solutions and for classifying the used combinatorial strategy
# version: 14 May 2019
# tested under R 3.4.3, Windows 7 64bit

parameter_descriptions <- c(n = "number of solutions to be generated", card = "cardinalities of the sets", card_min = "minimum cardinality of the sets", card_max = "maximum cardinality of the sets", prep = "probability of repeated element", pinc = "probability of incorrect element", plack = "probability of lacking element", gen_samp = "whether sample should be generated", calc_char = "whether the characteristics should be calculated", ch_fold = "whether the output folder should be changed") # description of the parameters
parameter_defaults <- c(n = 1000, card_min = 2, card_max = 4, prep = 0.1143, pinc = 0.0593, plack = 0.1222) # default values of parameters
classification_levels <- c("completely odometrical", "completely cyclical", "nearly odometrical", "nearly cyclical", "slightly odometrical", "slightly cyclical", "random") # levels of the classification

# Generate one correct (i.e. complete) solution
# cardinalities: numeric vector of length 2, containing cardinality of set 1 and set 2, respectively
# returned value: character value containing cardinality1×cardinality2×2 characters
generate_correct_solution <- function(cardinalities) {
	cardinality1 <- cardinalities[1] # get cardinality of set 1
	cardinality2 <- cardinalities[2] # get cardinality of set 2
	symbols1 <- LETTERS[1:cardinality1] # symbols of set 1
	symbols2 <- as.character(1:cardinality2) # symbols of set 2
	enumerated_elements <- apply(X = expand.grid(symbols1, symbols2), MARGIN = 1, FUN = paste, collapse = "") # Cartesian product of the two set
	generated_correct_solution <- paste0(sample(x = enumerated_elements, size = cardinality1 * cardinality2, replace = FALSE), collapse = "") # concatenate the elements to form a solution
	return(generated_correct_solution) # return result
} # generate_correct_solution()

# Generate a corrupted solution based on a correct (i.e. complete) solution
# cardinalities_and_correct_solution: character vector of length 3, containing cardinality of set 1 and set 2, and the correct solution, respectively
# prep: probability of the fact that an element of a solution is a repeated one. Numeric value, element of [0, 1]
# pinc: probability of the fact that an element of a solution is incorrect (does not fulfill the formal requirements of the elements). Numeric value, element of [0, 1]
# plack: probability of the fact that an element of a solution is lacking. Numeric value, element of [0, 1]
# returned value: character value containing cardinality1×cardinality2×2 characters
corrupt_solution <- function(cardinalities_and_correct_solution, prep, pinc, plack) {
	cardinality1 <- as.numeric(cardinalities_and_correct_solution[1]) # get cardinality of set 1
	cardinality2 <- as.numeric(cardinalities_and_correct_solution[2]) # get cardinality of set 2
	correct_solution <- cardinalities_and_correct_solution[3] # get the solution
	complete_solution_length <- cardinality1 * cardinality2 # length of the solution
	generate_repetition <- as.logical(sample.int(n = 2, size = complete_solution_length, replace = TRUE, prob = c(1 - prep, prep)) - 1) # logical vector containing whether repetition should be generated in the place of an element
	generate_error <- as.logical(sample.int(n = 2, size = complete_solution_length, replace = TRUE, prob = c(1 - pinc, pinc)) - 1) # logical vector containing whether an element should be corrupted
	generate_lacking <- as.logical(sample.int(n = 2, size = complete_solution_length, replace = TRUE, prob = c(1 - plack, plack)) - 1) # logical vector containing whether an element should be deleted
	corrupted_solution <- correct_solution # copy the original solution
	number_of_repetitions <- 0 # counter of inserted repetitions
	for (iterator in 1:complete_solution_length) { # iterate through the elements of the solution
		order_of_error_generation <- sample.int(n = 2, size = 2, replace = FALSE) # random order of two corruption methods
		for (generation_no in order_of_error_generation) { # iterates through the three corruption methods
			if (generation_no == 1 & generate_error[iterator]) { # if erroneous element should be generated
				selected_element_part <- sample.int(n = 2, size = 1) # which part of the element should be corrupted
				position <- (iterator + number_of_repetitions) * 2 - 2 + selected_element_part
				substr(x = corrupted_solution, start = position, stop = position) <- "_" # replace element part
			} # if
			if (generation_no == 2 & generate_lacking[iterator]) { # if lacking element should be generated
				substr(x = corrupted_solution, start = (iterator + number_of_repetitions) * 2 - 1, stop = (iterator + number_of_repetitions) * 2) <- "__" # replace element
			} # if
		} # for
		if (generate_repetition[iterator]) { # if repetition should be generated
			selected_element_no <- sample.int(n = complete_solution_length + number_of_repetitions, size = 1) # which element should be repeated
			selected_element <- substr(x = corrupted_solution, start = selected_element_no * 2 - 1, stop = selected_element_no * 2) # the element that will be repeated
			corrupted_solution <- paste0(substr(x = corrupted_solution, start = 1, stop = (iterator + number_of_repetitions) * 2), selected_element, substr(x = corrupted_solution, start = (iterator + number_of_repetitions) * 2 + 1, stop = nchar(corrupted_solution))) # insert the repetition
			number_of_repetitions <- number_of_repetitions + 1 # increase counter
		} # if
	} # for
	corrupted_solution <- rearrange_solution(corrupted_solution) # rearrange (place lacking elements to the end of) the solution
	return(corrupted_solution) # return result
} # corrupt_solution()

# Rearrange the corrupted solution by placing lacking elements to the end of the solution
# solution: character value containing cardinality1×cardinality2×2 characters
# returned value: character value containing cardinality1×cardinality2×2 characters
rearrange_solution <- function(solution) {
	characters <- strsplit(x = solution, split = "")[[1]] # split to characters
	solution_elements <- paste0(characters[c(TRUE, FALSE)], characters[c(FALSE, TRUE)]) # elements of the solution
	lacking_indices <- grep(pattern = "__", x = solution_elements) # indices of the lacking elements
	rearranged_solution <- paste(c(solution_elements[!((1:length(solution_elements)) %in% lacking_indices)], solution_elements[lacking_indices]), collapse = "")# rearrange (place lacking elements to the end of) the solution
	return(rearranged_solution) # return result
} # rearrange_solution()

# Generate random sample of solutions
# n: number of solutions to be generated. Integer value. Default: 1000
# card: cardinalities of the sets from which the elements of the solutions are selected. List of two-element numeric vectors. Vectors express the cardinalities of the two sets. Default: as.list(data.frame(t(expand.grid(2:4, 2:4)))).
# prep: probability of the fact that an element of a solution is a repeated one. Numeric value, element of [0, 1]. Default: 0.1143
# pinc: probability of the fact that an element of a solution is incorrect (does not fulfill the formal requirements of the elements). Numeric value, element of [0, 1]. Default: 0.0593
# plack: probability of the fact that an element of a solution is lacking. Numeric value, element of [0, 1]. Default: 0.1222
# returned value: data.frame of numeric (cardinality of set 1), numeric (cardinality of set 2) and character (solution) columns
generate_random_sample <- function(n = parameter_defaults["n"], card = as.list(data.frame(t(expand.grid(parameter_defaults["card_min"]:parameter_defaults["card_max"], parameter_defaults["card_min"]:parameter_defaults["card_max"])))), prep = parameter_defaults["prep"], pinc = parameter_defaults["pinc"], plack = parameter_defaults["plack"]) {
	random_cardinalities <- sample(x = length(card), size = n, replace = TRUE) # choose n cardinality pairs
	resolved_cardinalities <- resolve_cardinalities(sample = random_cardinalities, key = card) # resolve the cardinality pairs
	generated_correct_solutions <- apply(X = resolved_cardinalities, MARGIN = 1, FUN = generate_correct_solution) # generate solutions
	corrupted_solutions <- data.frame(solution = apply(X = cbind(resolved_cardinalities, generated_correct_solutions, stringsAsFactors = FALSE), MARGIN = 1, FUN = corrupt_solution, prep = prep, pinc = pinc, plack = plack), stringsAsFactors = FALSE) # corrupt the correct solutions
	generated_sample <- cbind(resolved_cardinalities, corrupted_solutions) # bind cardinalities and solution
	return(generated_sample) # return result
} # generate_random_sample()

# Wrapper function that provides interactive way of setting parameters of sample generation and calculation of characteristics
# returned value: -
combinatorial_wrapper <- function() {
	writeLines("Wrapper function is started.")
	writeLines("It provides interactive way of setting parameters of sample generation, calculation of characteristics, and saving results.")
	writeLines("\nPART 1: Sample generation or loading from file")
	appropriate_input_is_needed <- TRUE # whether an appropriate input has not been given and is needed
	while (appropriate_input_is_needed) { # if an appropriate input has not been given
		appropriate_input_is_needed <- tryCatch({ # catch errors within the block
			writeLines("\tSample of solutions can be generated, or, alternatively, a file containing the sample can be loaded.")
			gen_samp <- tolower(readline(prompt = "\tShould I generate a random sample of solutions? (gen_samp) [y/n, default: y] ")) # read the parameter value
			gen_samp <- validate_gen_samp(x = gen_samp) # validate the parameter
			FALSE # answer is FALSE for the question whether an appropriate input is still needed
		}, # tryCatch - main block
		error = function(catched_error) { # if error is caught
			writeLines("The input is wrong. Type an adequate input or leave blank to set the default value.")
			writeLines(catched_error$message)
			return(TRUE) # answer is TRUE for the question whether an appropriate input is still needed
		} # error()
		) # tryCatch()
	} # while
	if (gen_samp) { # if sample is going to be generated
		for (parameter in names(parameter_defaults)) { # iterate through the parameters
			appropriate_input_is_needed <- TRUE # whether an appropriate input has not been given and is needed
			while (appropriate_input_is_needed) { # if an appropriate input has not been given
				appropriate_input_is_needed <- tryCatch({ # catch errors within the block
					assign(parameter, readline(prompt = paste0("\t", toupper(substr(parameter_descriptions[parameter], 1, 1)), substr(parameter_descriptions[parameter], 2, nchar(parameter_descriptions[parameter])), " (", parameter, ") [default: ", parameter_defaults[parameter], "]: "))) # read the parameter value
					if (get(parameter) == "") { # if left blank
						assign(parameter, parameter_defaults[parameter]) # set default value
					} else { # if not blank
						suppressWarnings(assign(parameter, as.numeric(get(parameter)))) # coerce to numeric
					} # else
					if (parameter != "card_max") { # if a one-parameter validation function should be called
						do.call(what = paste0("validate_", parameter), args = list(x = get(parameter))) # validate the parameter
					} else { # if a two-parameter validation function should be called
						validate_card_max(x = card_max,  card_min = card_min) # validate the parameter
					} # else
					FALSE # answer is FALSE for the question whether an appropriate input is still needed
				}, # tryCatch - main block
				error = function(catched_error) { # if error is caught
					writeLines("The input is wrong. Type an adequate input or leave blank to set the default value.")
					writeLines(catched_error$message)
					return(TRUE) # answer is TRUE for the question whether an appropriate input is still needed
				} # error()
				) # tryCatch()
			} # while
		} # for
		writeLines("\tGenerating sample of solutions... ", sep = "")
		id <- data.frame(id = 1:n) # IDs of the solutions
		generated_sample <- generate_random_sample(n = n, card = as.list(data.frame(t(expand.grid(card_min:card_max, card_min:card_max))))) # generate the sample from based on the inputs
		writeLines("DONE")
	} else { # if sample is going to be loaded from file
		## ERROR HANDLING IN THIS BLOCK IS NOT YET IMPLEMENTED - BE CAUTIOUS
		writeLines("\tSample is going to be loaded from a csv or Rdata file.")
		writeLines("\tData must be stored in a 4-column table with column names \"id\", \"cardinality1\", \"cardinality2\", and \"solution\", respectively.\n\tThe first column contains the IDs of the solution (numbers). The second and third columns contain the cardinality of the first and second sets, respectively, from which the solution was generated (integer numbers).\n\tThe fourth column contains the solutions (strings). Example: 'A1A3B1B_A2__'.\n\tElements are the composition of capital letters of English alphabet starting from A and integer numbers starting from 1.\n\tMissing half-element is indicated by \"_\". Lacking elements (\"__\") must be placed in the end of the solution.\n\tData must be separated by \";\" in the csv file.\n\tAny deviation from the above conditions may result in unwanted program operation.")
		chose_file <- choose.files(default = "", caption = "Select the file containing sample of solutions", multi = FALSE, filters = matrix(c("csv", "dat", "txt", "RData", "All", "*.csv", "*.dat", "*.txt", "*.RData", "*.*"), ncol = 2, byrow = FALSE), index = 1) # open file choosing window
		writeLines("\tLoding file... ", sep = "")
		if (substr(x = chose_file, start = nchar(chose_file) - 4, stop = nchar(chose_file)) == "RData") { # if RData is to be loaded
			loaded_variable_names <- load(file = chose_file) # load the file
			id <- get(loaded_variable_names[1])$id # get the IDs
			generated_sample <- get(loaded_variable_names[1])[, c("cardinality1", "cardinality2", "solution")] # get the data.frame of the solutions
		} else { # if csv is to be loaded
			column_number <- max(count.fields(file = chose_file, sep = ";")) # count the number of columns
			loaded_csv <- read.table(file = chose_file, header = TRUE, sep = ";", quote = "\"'", dec = ".", as.is = TRUE, colClasses = c("numeric", "numeric", "numeric", "character", rep(x = "NULL", times = column_number - 4)), flush = TRUE) # open the csv file
			id <- loaded_csv$id # get the IDs
			generated_sample <- loaded_csv[, c("cardinality1", "cardinality2", "solution")] # get the data.frame of the solutions
		} # else
		writeLines("DONE")
	} # else
	writeLines("\nPART 2: Calculation of characteristics")
	appropriate_input_is_needed <- TRUE # whether an appropriate input has not been given and is needed
	while (appropriate_input_is_needed) { # if an appropriate input has not been given
		appropriate_input_is_needed <- tryCatch({ # catch errors within the block
			calc_char <- tolower(readline(prompt = "\tShould I calculate the characteristics of the generated sample of solutions? (calc_char) [y/n, default: y] ")) # read the parameter value
			calc_char <- validate_calc_char(x = calc_char) # validate the parameter
			FALSE # answer is FALSE for the question whether an appropriate input is still needed
		}, # tryCatch - main block
		error = function(catched_error) { # if error is caught
			writeLines("The input is wrong. Type an adequate input or leave blank to set the default value.")
			writeLines(catched_error$message)
			return(TRUE) # answer is TRUE for the question whether an appropriate input is still needed
		} # error()
		) # tryCatch()
	} # while
	writeLines("\tCalculating characteristics of solutions... ", sep = "")
	if(calc_char) { # if answer is yes
		calculated_characteristics <- calculate_characteristics(sample = generated_sample) # calculate the characteristics
		output <- cbind(id, generated_sample, calculated_characteristics) # bind results
	} else { # if answer is no
		output <- cbind(id, generated_sample) # bind results
	} # else
	writeLines("DONE")
	writeLines("\nPART 3: Saving results")
	writeLines("\tResults are going to be saved in csv and Rdata files.")
	writeLines("\tThe former can be opened in any spreadsheet software, the latter can be opened in R statistical software.")
	output_folder <- paste0(getwd(), "/output_", format(Sys.time(), "%Y_%m_%d_%H_%M")) # a new subfolder in the working folder
	writeLines(paste0("\tDefault folder of the output files is ", output_folder, "."))
	appropriate_input_is_needed <- TRUE # whether an appropriate input has not been given and is needed
	while (appropriate_input_is_needed) { # if an appropriate input has not been given
		appropriate_input_is_needed <- tryCatch({ # catch errors within the block
			ch_fold <- tolower(readline(prompt = "\tWould you like to change the location of output files? (ch_fold) [y/n, default: n] ")) # read the parameter value
			ch_fold <- validate_ch_fold(x = ch_fold) # validate the parameter
			FALSE # answer is FALSE for the question whether an appropriate input is still needed
		}, # tryCatch - main block
		error = function(catched_error) { # if error is caught
			writeLines("The input is wrong. Type an adequate input or leave blank to set the default value.")
			writeLines(catched_error$message)
			return(TRUE) # answer is TRUE for the question whether an appropriate input is still needed
		} # error()
		) # tryCatch()
	} # while
	writeLines("\tSaving results... ", sep = "")
	if(ch_fold) { # if answer is yes
		chose_folder <- choose.dir(default = getwd(), caption = "Select folder of output files") # chose the folder of output files
		output_folder <- chose_folder # the chosen folder will be the output folder
	} else { # if answer is no
		dir.create(path = output_folder, showWarnings = FALSE, recursive = TRUE) # create the new subfolder
	} # else
	save(output, file = paste0(output_folder, "/output.RData")) # save the results in RData
	write.table(x = output, file = paste0(output_folder, "/output.csv"), append = FALSE, quote = TRUE, sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE) # save the results in csv
	writeLines("DONE")
	writeLines("Wrapper function is successfully finished.")
} # combinatorial_wrapper()

# Resolve the generated random numbers to cardinality pairs
# sample: numeric vector containing integers (place in the key parameter) to be resolved to cardinality pairs
# key: list of cardinality pairs (numeric vectors of length 2) that provide the key for resolving
# returned value: data.frame of numeric (cardinality of set 1) and numeric (cardinality of set 2) columns
resolve_cardinalities <- function(sample, key) {
	resolved_cardinalities <- as.data.frame(t(sapply(X = sample, FUN = function(element) {return(c(cardinality1 = key[[c(element, 1)]], cardinality2 = key[[c(element, 2)]]))}, simplify = "array"))) # resolve
	return(resolved_cardinalities) # return result
} # resolve_cardinalities

# Calculate the characteristics of the sample of solutions
# sample: data.frame of numeric (cardinality of set 1), numeric (cardinality of set 2) and character (solution) columns
# returned value: data.frame of numeric (repetition ratios), numeric (incorrect ratios), numeric (lacking ratios), logical (if the solution is perfect), numeric (measures of Strict Odometer Strategy), numeric (measures of Permissive Odometer Strategy), numeric (constancies of first part of the solutions), numeric (constancies of second part of the solutions), numeric (cyclicities of first part of the solutions), numeric (cyclicities of second part of the solutions), numeric (odometricalities), numeric (cyclicalities) and character (classified strategies) columns
calculate_characteristics <- function(sample) {
	ratios <- calculate_ratios(sample = sample) # calculate the error ratios of the solutions
	measures_SOS <- calculate_measures_of_SOS(sample = sample) # calculate the measure of Strict Odometer Strategy of the solutions
	measures_POS <- calculate_measures_of_POS(sample = sample) # calculate the measure of Permissive Odometer Strategy of the solutions
	constancies <- calculate_constancies(sample = sample) # calculate constancies of the solutions
	cyclicities <- calculate_cyclicities(sample = sample) # calculate cyclicities of the solutions
	odometricalities <- calculate_odometricalities(constancies1 = constancies[, 1], constancies2 = constancies[, 2], cyclicities1 = cyclicities[, 1], cyclicities2 = cyclicities[, 2]) # calculate odometricalities of the solutions
	cyclicalities <- calculate_cyclicalities(cyclicities1 = cyclicities[, 1], cyclicities2 = cyclicities[, 2]) # calculate cyclicalities of the solutions
	strategies <- classify_strategies(odometricalities = odometricalities, cyclicalities = cyclicalities) # classify the strategy of the solutions
	characteristics <- cbind(ratios, measures_SOS, measures_POS, constancies, cyclicities, odometricalities, cyclicalities, strategies) # bind the results
	return(characteristics) # return result
} # calculate_characteristics()

# Calculate the three error ratio of a solution
# cardinalities_and_solution: character vector of length 3, containing cardinality of set 1 and set 2, and the correct solution, respectively
# returned value: numeric vector of length 3, all are elements of [0, 1], containing repetition ratio, incorrect ratio and lacking ratio, respectively
calculate_ratio <- function(cardinalities_and_solution) {
	cardinality1 <- as.numeric(cardinalities_and_solution[1]) # get cardinality of set 1
	cardinality2 <- as.numeric(cardinalities_and_solution[2]) # get cardinality of set 2
	complete_solution_length <- cardinality1 * cardinality2 # calculate the length of the complete solution
	actual_solution_length <- nchar(cardinalities_and_solution[3]) / 2 # calculate the length of the solution
	solution <- cardinalities_and_solution[3] # get the solution
	solution_elements <- sapply(X = 1:actual_solution_length, FUN = function(position) {return(substr(x = solution, start = position * 2 - 1, stop = position * 2))}) # partition the solution to elements
	symbols1 <- LETTERS[1:cardinality1] # symbols of set 1
	symbols2 <- as.character(1:cardinality2) # symbols of set 2
	enumerated_elements <- apply(X = expand.grid(symbols1, symbols2), MARGIN = 1, FUN = paste, collapse = "") # Cartesian product of the sets
	repetition_no <- 0 # number of the counted repetitions
	for (element in enumerated_elements) { # iterate through the possible elements
		if (sum(as.numeric(solution_elements == element)) > 1) { # if an element occurs more than once
			repetition_no <- repetition_no + 1 # increase the number
		} # if
	} # for
	repetition_ratio <- repetition_no / complete_solution_length # calculate ratio of repetition
	incorrect_ratio <- sum(as.numeric(!(solution_elements %in% c(enumerated_elements, "__")))) / complete_solution_length # calculate ratio of erroneous elements
	lacking_ratio <- length(grep(pattern = "__", x = solution_elements, fixed = TRUE)) / complete_solution_length # calculate ratio of lacking elements
	return(c(repetition_ratio = repetition_ratio, incorrect_ratio = incorrect_ratio, lacking_ratio = lacking_ratio)) # return result
} # calculate_ratio()

# Calculate the three error ratios and the logical flag of being perfect, for a sample of solutions
# sample: data.frame of numeric (cardinality of set 1), numeric (cardinality of set 2) and character (solution) columns
# returned value: data.frame of numeric (repetition ratios), numeric (incorrect ratios), numeric (lacking ratios) and logical (if the solution is perfect) columns
calculate_ratios <- function(sample) {
	ratios <- as.data.frame(t(apply(X = sample, MARGIN = 1, FUN = calculate_ratio))) # calculate the different error ratios of every solutions
	ratios$perfect <- ratios$repetition_ratio == 0 & ratios$incorrect_ratio == 0 & ratios$lacking_ratio == 0 # flag of perfect (complete) solutions
	return(ratios) # return result
} # calculate_ratios()

# Calculate the measure of Strict Odometer Strategy of a solution
# cardinalities_and_solution: character vector of length 3, containing cardinality of set 1 and set 2, and the correct solution, respectively
# returned value: numeric value, element of [0, 1]
calculate_measure_of_SOS <- function(cardinalities_and_solution) {
	cardinality1 <- as.numeric(cardinalities_and_solution[1]) # get cardinality of set 1
	cardinality2 <- as.numeric(cardinalities_and_solution[2]) # get cardinality of set 2
	complete_solution_length <- cardinality1 * cardinality2 # length of a complete solution
	solution <- cardinalities_and_solution[3] # get the solution
	characters <- strsplit(x = solution, split = "")[[1]] # split to characters
	solution_elements <- paste0(characters[c(TRUE, FALSE)], characters[c(FALSE, TRUE)]) # elements of the solution
	solution_elements <- solution_elements[solution_elements != "__"] # erase the lacking elements
	actual_solution_length <- length(solution_elements) # number of the given elements of the solution
	no_right_position_elements_final <- 0 # initial value of the number of the elements in right position
	if (actual_solution_length > 0) { # if not all of the elements are lacking
		symbols1 <- LETTERS[1:cardinality1] # symbols of set 1
		symbols2 <- as.character(1:cardinality2) # symbols of set 2
		ordered_enumerated_elements <- apply(X = expand.grid(symbols2, symbols1), MARGIN = 1, FUN = function(symbols) {return(paste(symbols[2], symbols[1], sep = ""))}) # expected elements in the SOS order
		for (shift in 0:max(0, actual_solution_length - complete_solution_length)) { # moving window
			no_right_position_elements <- 0 # initial value of the number of the elements in right position
			for (iterator in 1:min(actual_solution_length, complete_solution_length)) { # iterate through the elements of the solution
				if (solution_elements[iterator + shift] == ordered_enumerated_elements[iterator]) { # if the element is in the right position
					no_right_position_elements <- no_right_position_elements + 1 # increase the measure of SOS
				} # if
			} # for iterator
			no_right_position_elements_final <- max(no_right_position_elements_final, no_right_position_elements) # calculate maximum
		} # for shift
	} # if
	measure_SOS <- no_right_position_elements_final / actual_solution_length # calculate the measure of SOS
	return(measure_SOS) # return result
} # calculate_measure_of_SOS()

# Calculate the measures of Strict Odometer Strategy of a sample of solutions
# sample: data.frame of numeric (cardinality of set 1), numeric (cardinality of set 2) and character (solution) columns
# returned value: data.frame of one numeric column
calculate_measures_of_SOS <- function(sample) {
	measures_SOS <- data.frame(measures_SOS = apply(X = sample, MARGIN = 1, FUN = calculate_measure_of_SOS)) # calculate the measure of POS of every solutions
	return(measures_SOS) # return result
} # calculate_measures_of_SOS()

# Calculate the measure of Permissive Odometer Strategy of a solution
# cardinalities_and_solution: character vector of length 3, containing cardinality of set 1 and set 2, and the correct solution, respectively
# returned value: numeric value, element of [0, 1]
calculate_measure_of_POS <- function(cardinalities_and_solution) {
	cardinality1 <- as.numeric(cardinalities_and_solution[1]) # get cardinality of set 1
	cardinality2 <- as.numeric(cardinalities_and_solution[2]) # get cardinality of set 2
	complete_solution_length <- cardinality1 * cardinality2 # length of a complete solution
	solution <- cardinalities_and_solution[3] # get the solution
	characters <- strsplit(x = solution, split = "")[[1]] # split to characters
	solution_elements <- paste0(characters[c(TRUE, FALSE)], characters[c(FALSE, TRUE)]) # elements of the solution
	solution_elements <- solution_elements[solution_elements != "__"] # erase the lacking elements
	actual_solution_length <- length(solution_elements) # number of the given elements of the solution
	if (actual_solution_length > 0) { # if not all of the elements are lacking
		symbols1 <- LETTERS[1:cardinality1] # symbols of set 1
		symbols1_list <- list(symbols1) # symbols of set 1 in a list
		symbols2 <- as.character(1:cardinality2) # symbols of set 1
		symbols2_list <- list(symbols2) # symbols of set 2 in a list
		measures_SOS <- numeric(length = 0) # initial vector of the measures of SOS
		permutations_with_replacement1 <- expand.grid(symbols1_list[rep(x = 1, times = cardinality1)], stringsAsFactors = FALSE) # generate the permutations with replacement
		permutations_without_replacement1 <- permutations_with_replacement1[apply(X = permutations_with_replacement1, MARGIN = 1, function(row_of_symbols) {return(length(unique(row_of_symbols)) == cardinality1)}), ] # erase rows with repetitionfor (sets_in_strict_order in c(TRUE, FALSE)) { # iterates through the two types of set order
		permutations_with_replacement2 <- expand.grid(symbols2_list[rep(x = 1, times = cardinality2)], stringsAsFactors = FALSE) # generate the permutations with replacement
		permutations_without_replacement2 <- permutations_with_replacement2[apply(X = permutations_with_replacement2, MARGIN = 1, function(row_of_symbols) {return(length(unique(row_of_symbols)) == cardinality2)}), ] # erase rows with repetition
		for (sets_in_strict_order in c(TRUE, FALSE)) { # iterates through the two types of set order
			for (permutation1 in 1:factorial(cardinality1)) { # iterates through the permutations of set 1
				for (permutation2 in 1:factorial(cardinality2)) { # iterates through the permutations of set 2
					symbols1 <- as.character(permutations_without_replacement1[permutation1, , drop = TRUE]) # the selected symbol order
					symbols2 <- as.character(permutations_without_replacement2[permutation2, , drop = TRUE]) # the selected symbol order
					if (sets_in_strict_order) { # if set 1 is fixed and set 2 varies
						ordered_enumerated_elements <- apply(X = expand.grid(symbols2, symbols1), MARGIN = 1, FUN = function(symbols) {return(paste(symbols[2], symbols[1], sep = ""))}) # expected elements in the SOS order
					} else { # if set 2 is fixed and set 1 varies
						ordered_enumerated_elements <- apply(X = expand.grid(symbols1, symbols2), MARGIN = 1, FUN = paste, collapse = "") # expected elements in the SOS order
					} # else
					no_right_position_elements_final <- 0 # initial value of the number of the elements in right position
					for (shift in 0:max(0, actual_solution_length - complete_solution_length)) { # moving window
						no_right_position_elements <- 0 # initial value of the number of the elements in right position
						for (iterator in 1:min(actual_solution_length, complete_solution_length)) { # iterate through the elements of the solution
							if (solution_elements[iterator + shift] == ordered_enumerated_elements[iterator]) { # if the element is in the right position
								no_right_position_elements <- no_right_position_elements + 1 # increase the measure of SOS
							} # if
						} # for iterator
						no_right_position_elements_final <- max(no_right_position_elements_final, no_right_position_elements) # calculate maximum
					} # for shift
					measures_SOS <- c(measures_SOS, no_right_position_elements_final / actual_solution_length) # calculate the measure of SOS, append the the previously calculated ones
				} # for permutation2
			} # for permutation1
		} # for sets_in_strict_order
		measure_POS <- max(measures_SOS) # calculate the maximum of measures of SOS
	} else { # if all of the elements are lacking
		measure_POS <- 0 # measure of POS is set to 0
	} # else
	return(measure_POS) # return result
} # calculate_measure_of_POS()

# Calculate the measures of Permissive Odometer Strategy of a sample of solutions
# sample: data.frame of numeric (cardinality of set 1), numeric (cardinality of set 2) and character (solution) columns
# returned value: data.frame of one numeric column
calculate_measures_of_POS <- function(sample) {
	measures_POS <- data.frame(measures_POS = apply(X = sample, MARGIN = 1, FUN = calculate_measure_of_POS)) # calculate the measure of POS of every solutions
	return(measures_POS) # return result
} # calculate_measures_of_POS()

# Calculate the constancies of two parts of a solution
# cardinalities_and_solution: character vector of length 3, containing cardinality of set 1 and set 2, and the correct solution, respectively
# returned value: numeric vector of length 2, all are elements of [0, 1], containing constancy of the first part of the solution and constancy of the second part of the solution, respectively
calculate_constancy <- function(cardinalities_and_solution) {
	cardinality1 <- as.numeric(cardinalities_and_solution[1]) # get cardinality of set 1
	cardinality2 <- as.numeric(cardinalities_and_solution[2]) # get cardinality of set 2
	solution <- cardinalities_and_solution[3] # get the solution
	characters <- strsplit(x = solution, split = "")[[1]] # split to characters
	solution_elements <- paste0(characters[c(TRUE, FALSE)], characters[c(FALSE, TRUE)]) # elements of the solution
	solution_elements <- solution_elements[solution_elements != "__"] # erase the lacking elements
	actual_solution_length <- length(solution_elements) # number of the given elements of the solution
	if (actual_solution_length > 0) { # if not all of the elements are lacking
		solution_elements1 <- substr(x = solution_elements, start = 1, stop = 1) # first part of the elements selected from set 1
		solution_elements2 <- substr(x = solution_elements, start = 2, stop = 2) # second part of the elements selected from set 2
		symbol_change_no1 <- sum(as.numeric(solution_elements1[1:(actual_solution_length - 1)] != solution_elements1[2:actual_solution_length])) # number of the symbol changes, first parts are taken into account
		symbol_change_no2 <- sum(as.numeric(solution_elements2[1:(actual_solution_length - 1)] != solution_elements2[2:actual_solution_length])) # number of the symbol changes, second parts are taken into account
		if (actual_solution_length > cardinality1) { # if more elements are given than the cardinality of the set
			constancy1 <- min(1, (actual_solution_length - symbol_change_no1 - 1) / (actual_solution_length - cardinality1)) # calculate constancy
		} else { # if not more elements are given than the cardinality of the set
			constancy1 <- 1 # constancy is maximized at 1
		} # else
		if (actual_solution_length > cardinality2) { # if more elements are given than the cardinality of the set
			constancy2 <- min(1, (actual_solution_length - symbol_change_no2 - 1) / (actual_solution_length - cardinality2)) # calculate constancy
		} else { # if not more elements are given than the cardinality of the set
			constancy2 <- 1 # constancy is maximized at 1
		} # 
	} else { # if all of the elements are lacking
		constancy1 <- constancy2 <- 0 # constancies are set to 0
	} # else
	return(c(constancy1 = constancy1, constancy2 = constancy2)) # return result
} # calculate_constancy()

# Calculate the constancies of a sample of solutions
# sample: data.frame of numeric (cardinality of set 1), numeric (cardinality of set 2) and character (solution) columns
# returned value: data.frame of numeric (constancies of the first part of the solutions) and numeric (constancies of the second part of the solution) columns
calculate_constancies <- function(sample) {
	constancies <- as.data.frame(t(apply(X = sample, MARGIN = 1, FUN = calculate_constancy))) # calculate the constancy of every solutions
	return(constancies) # return result
} # calculate_constancies()

# Calculate the cyclicities of two parts of a solution
# cardinalities_and_solution: character vector of length 3, containing cardinality of set 1 and set 2, and the correct solution, respectively
# returned value: numeric vector of length 2, all are elements of [0, 1], containing cyclicity of the first part of the solution and cyclicity of the second part of the solution, respectively
calculate_cyclicity <- function(cardinalities_and_solution) {
	cardinality1 <- as.numeric(cardinalities_and_solution[1]) # get cardinality of set 1
	cardinality2 <- as.numeric(cardinalities_and_solution[2]) # get cardinality of set 2
	solution <- cardinalities_and_solution[3] # get the solution
	characters <- strsplit(x = solution, split = "")[[1]] # split to characters
	solution_elements <- paste0(characters[c(TRUE, FALSE)], characters[c(FALSE, TRUE)]) # elements of the solution
	solution_elements <- solution_elements[solution_elements != "__"] # erase the lacking elements
	actual_solution_length <- length(solution_elements) # number of the given elements of the solution
	solution_elements1 <- substr(x = solution_elements, start = 1, stop = 1) # first part of the elements selected from set 1
	solution_elements2 <- substr(x = solution_elements, start = 2, stop = 2) # second part of the elements selected from set 2
	if (actual_solution_length >= cardinality1) { # if not less elements are given than the cardinality of the set
		cyclicity_violation1 <- 0 # initial value of cyclicity violation
		for (iterator in 1:(actual_solution_length - cardinality1 + 1)) { # moving window
			if ((solution_elements1[iterator] == "_") | (solution_elements1[iterator] %in% solution_elements1[(iterator + 1):(iterator + cardinality1 - 1)])) { # if cyclicity is violated
				cyclicity_violation1 <- cyclicity_violation1 + 1 # increase cyclicity violation
			} # if
		} # for
		cyclicity1 <- 1 - cyclicity_violation1 / (actual_solution_length - cardinality1 + 1) # calculate cyclicity
	} else { # if less elements are given than the cardinality of the set
		cyclicity1 <- 1 # cyclicity is maximized at 1
	} # else
	if (actual_solution_length >= cardinality2) { # if not less elements are given than the cardinality of the set
		cyclicity_violation2 <- 0 # initial value of cyclicity violation
		for (iterator in 1:(actual_solution_length - cardinality2 + 1)) { # moving window
			if ((solution_elements2[iterator] == "_") | (solution_elements2[iterator] %in% solution_elements2[(iterator + 1):(iterator + cardinality2 - 1)])) { # if cyclicity is violated
				cyclicity_violation2 <- cyclicity_violation2 + 1 # increase cyclicity violation
			} # if
		} # for
		cyclicity2 <- 1 - cyclicity_violation2 / (actual_solution_length - cardinality2 + 1) # calculate cyclicity
	} else { # if less elements are given than the cardinality of the set
		cyclicity2 <- 1 # cyclicity is maximized at 1
	} # else
	return(c(cyclicity1 = cyclicity1, cyclicity2 = cyclicity2)) # return result
} # calculate_cyclicity()

# Calculate the cyclicities of a sample of solutions
# sample: data.frame of numeric (cardinality of set 1), numeric (cardinality of set 2) and character (solution) columns
# returned value: data.frame of numeric (cyclicities of the first part of the solutions) and numeric (cyclicities of the second part of the solution) columns
calculate_cyclicities <- function(sample) {
	cyclicities <- as.data.frame(t(apply(X = sample, MARGIN = 1, FUN = calculate_cyclicity))) # calculate the cyclicity of every solutions
	return(cyclicities) # return result
} # calculate_cyclicities()

# Calculate odometricality of a solution
# constancy1: numeric value, element of [0, 1], constancy of the first part of the solution
# constancy2: numeric value, element of [0, 1], constancy of the second part of the solution
# cyclicity1: numeric value, element of [0, 1], cyclicity of the first part of the solution
# cyclicity2: numeric value, element of [0, 1], cyclicity of the second part of the solution
# returned value: numeric value, element of [0, 1]
calculate_odometricality <- function(constancy1, constancy2, cyclicity1, cyclicity2) {
	odometricality <- max(constancy1 * cyclicity2, constancy2 * cyclicity1) # calculate odometricality
	return(odometricality) # return result
} # calculate_odometricality()

# Calculate odometricalities of a sample of solution
# constancies1: numeric vector, constancies of the first part of the solutions
# constancies2: numeric vector, constancies of the second part of the solutions
# cyclicities1: numeric vector, cyclicities of the first part of the solutions
# cyclicities2: numeric vector, cyclicities of the second part of the solutions
# returned value: numeric vector
calculate_odometricalities <- function(constancies1, constancies2, cyclicities1, cyclicities2) {
	odometricalities <- apply(X = cbind(constancies1 * cyclicities2, constancies2 * cyclicities1), MARGIN = 1, FUN = max) # calculate odometricalities
	return(data.frame(odometricalities = odometricalities)) # return result
} # calculate_odometricalities()

# Calculate cyclicality of a solution
# cyclicity1: numeric value, element of [0, 1], cyclicity of the first part of the solution
# cyclicity2: numeric value, element of [0, 1], cyclicity of the second part of the solution
# returned value: numeric value, element of [0, 1]
calculate_cyclicality <- function(cyclicity1, cyclicity2) {
	cyclicality <- cyclicity1 * cyclicity2 # calculate cyclicality
	return(cyclicality) # return result
} # calculate_cyclicality()

# Calculate cyclicalities of a sample of solution
# cyclicities1: numeric vector, cyclicities of the first part of the solutions
# cyclicities2: numeric vector, cyclicities of the second part of the solutions
# returned value: numeric vector
calculate_cyclicalities <- function(cyclicities1, cyclicities2) {
	cyclicalities <- cyclicities1 * cyclicities2 # calculate cyclicalities
	return(data.frame(cyclicalities = cyclicalities)) # return result
} # calculate_cyclicalities()

# Classify the used combinatorial strategy of a solution
# odometricality: numeric value, element of [0, 1], odometricality of the solution
# cyclicality: numeric value, element of [0, 1], cyclicity of the solution
# returned value: factor value
classify_strategy <- function(odometricality, cyclicality) {
	classification_levels <- c("random", "slightly cyclical", "slightly odometrical", "nearly cyclical", "nearly odometrical", "completely cyclical", "completely odometrical") # levels of the categorical vector
	if (odometricality == 1 & cyclicality <= odometricality) {classification <- "completely odometrical"} # classify strategy as completely odometrical
	else if (odometricality < cyclicality & cyclicality == 1) {classification <- "completely cyclical"} # classify strategy as completely cyclical
	else if (odometricality >= 0.666 & cyclicality <= odometricality) {classification <- "nearly odometrical"} # classify strategy as nearly odometrical
	else if (odometricality < cyclicality & cyclicality >= 0.666) {classification <- "nearly cyclical"} # classify strategy as nearly cyclical
	else if (odometricality >= 0.5 & cyclicality <= odometricality) {classification <- "slightly odometrical"} # classify strategy as slightly odometrical
	else if (odometricality < cyclicality & cyclicality >= 0.5) {classification <- "slightly cyclical"} # classify strategy as slightly cyclical
	else {classification <- "random"} # classify strategy as random;  abnormal situation of odometricality == 1 and cyclicality == 1 is also random
	return(factor(x = classification, levels = classification_levels)) # return result
} # classify_strategy()

# Classify the used combinatorial strategies of a sample of solutions
# odometricalities: numeric vector, odometricalities of the sample of solutions
# cyclicalities: numeric vector, cyclicalities of the sample of solutions
# returned value: factor vector
classify_strategies <- function(odometricalities, cyclicalities) {
	classification_levels <- c("random", "slightly cyclical", "slightly odometrical", "nearly cyclical", "nearly odometrical", "completely cyclical", "completely odometrical") # levels of the categorical vector
	classifications <- apply(X = cbind(odometricalities, cyclicalities), MARGIN = 1, FUN = function(pair) {
		return(classify_strategy(odometricality = pair[1], cyclicality = pair[2]))
	}) # apply function classify_strategy() (i.e. classify strategy) on each odometricality-cyclicality pairs
	return(data.frame(strategy = factor(x = classifications, levels = classification_levels))) # return result
} # classify_strategies()

# Concatenate an error message
# parameter: character value, the name of the parameter
# message: character value, the found error
# returned value: character value
paste_error_message <- function(parameter, message = "unknown error") {
	error_message <- paste0("Error in validity check of parameter '", parameter, "' (", parameter_descriptions[parameter], "): ", message, ".") # concatenate error message
	return(error_message) # return the pasted error message
} # error_message()

# Validate parameter n
# x: numeric value, the value of the parameter
# returned value: -
validate_n <- function(x) {
	if (is.na(x)) stop(paste_error_message("n", "must be numeric")) # throw error if not numeric (coerced to NA)
	if (!(is.numeric(x))) stop(paste_error_message("n", "must be numeric")) # throw error if not numeric
	if (length(x) != 1) stop(paste_error_message("n", "length must be 1")) # throw error if multiple values are given
	if (!(x %% 1 == 0)) stop(paste_error_message("n", "must be integer")) # throw error if not integer
	if (x < 1) stop(paste_error_message("n", "must be greater than 0")) # throw error if negative
} # validate_n()

# Validate parameter card_min
# x: numeric value, the value of the parameter
# returned value: -
validate_card_min <- function(x) {
	if (is.na(x)) stop(paste_error_message("card_min", "must be numeric")) # throw error if not numeric (coerced to NA)
	if (!(is.numeric(x))) stop(paste_error_message("card_min", "must be numeric")) # throw error if not numeric
	if (length(x) != 1) stop(paste_error_message("card_min", "length must be 1")) # throw error if multiple values are given
	if (!(x %% 1 == 0)) stop(paste_error_message("card_min", "must be integer")) # throw error if not integer
	if (x < 1) stop(paste_error_message("n", "must be greater than 0")) # throw error if negative
	if (x > 9) stop(paste_error_message("n", "must not be greater than 9")) # throw error if too great
} # validate_card_min()

# Validate parameter card_max
# x: numeric value, the value of the parameter
# returned value: -
validate_card_max <- function(x, card_min) {
	if (is.na(x)) stop(paste_error_message("card_max", "must be numeric")) # throw error if not numeric (coerced to NA)
	if (!(is.numeric(x))) stop(paste_error_message("card_max", "must be numeric")) # throw error if not numeric
	if (length(x) != 1) stop(paste_error_message("card_max", "length must be 1")) # throw error if multiple values are given
	if (!(x %% 1 == 0)) stop(paste_error_message("card_max", "must be integer")) # throw error if not integer
	if (x < card_min) stop(paste_error_message("card_max", paste0("must not be lower than the ", parameter_descriptions["card_min"]))) # throw error if lower then minimum cardinality
	if (x < 1) stop(paste_error_message("n", "must be greater than 0")) # throw error if negative
	if (x > 9) stop(paste_error_message("n", "must not be greater than 9")) # throw error if too great
} # validate_card_max()

# Validate parameter card
# x: list of numeric vectors, the cardinality pairs
# returned value: -
validate_card <- function(x) {
	if (!(is.list(x))) stop(paste_error_message("card", "must be a list"))  # throw error if not list
	if (length(x) < 1) stop(paste_error_message("card", "number of cardinality pairs must be greater than 0"))
	for (iterator in 1:length(x)) { # iterate through the list elements
		if (length(x[[iterator]]) != 2) stop(paste_error_message("card", "cardinality pairs must be pairs (two values)")) # throw error if not two values are given
		if (!(is.numeric(x[[iterator]]))) stop(paste_error_message("card", "cardinality pairs must be numeric")) # throw error if not numeric
		tryCatch({ # catch error within block
			validate_card_min(x[[iterator]][1]) # validate parameter
		}, error = function(catched_error) { # if error is caught
			stop(paste_error_message("card", paste0("one of the cardinality caused the following error: '", catched_error, "'"))) # throw error
		}) # tryCatch()
		tryCatch({ # catch error within block
			validate_card_max(x[[iterator]][2], x[[iterator]][1]) # validate parameter
		}, error = function(catched_error) { # if error is caught
			stop(paste_error_message("card", paste0("one of the cardinality caused the following error: '", catched_error, "'"))) # throw error
		}) # tryCatch()
	} # for
} # validate_card()

# Validate parameter prep
# x: numeric value, the value of the parameter
# returned value: -
validate_prep <- function(x) {
	if (is.na(x)) stop(paste_error_message("prep", "must be numeric")) # throw error if not numeric (coerced to NA)
	if (!(is.numeric(x))) stop(paste_error_message("prep", "must be numeric")) # throw error if not numeric
	if (length(x) != 1) stop(paste_error_message("prep", "length must be 1")) # throw error if multiple values are given
	if (x < 0) stop(paste_error_message("prep", "must not be lower than 0")) # throw error if negative
	if (x > 1) stop(paste_error_message("prep", "must not be greater than 1")) # throw error if too great
} # validate_prep()

# Validate parameter pinc
# x: numeric value, the value of the parameter
# returned value: -
validate_pinc <- function(x) {
	if (is.na(x)) stop(paste_error_message("pinc", "must be numeric")) # throw error if not numeric (coerced to NA)
	if (!(is.numeric(x))) stop(paste_error_message("pinc", "must be numeric")) # throw error if not numeric
	if (length(x) != 1) stop(paste_error_message("pinc", "length must be 1")) # throw error if multiple values are given
	if (x < 0) stop(paste_error_message("pinc", "must not be lower than 0")) # throw error if negative
	if (x > 1) stop(paste_error_message("pinc", "must not be greater than 1")) # throw error if too great
} # validate_pinc()

# Validate parameter plack
# x: numeric value, the value of the parameter
# returned value: -
validate_plack <- function(x) {
	if (is.na(x)) stop(paste_error_message("plack", "must be numeric")) # throw error if not numeric (coerced to NA)
	if (!(is.numeric(x))) stop(paste_error_message("plack", "must be numeric")) # throw error if not numeric
	if (length(x) != 1) stop(paste_error_message("plack", "length must be 1")) # throw error if multiple values are given
	if (x < 0) stop(paste_error_message("plack", "must not be lower than 0")) # throw error if negative
	if (x > 1) stop(paste_error_message("plack", "must not be greater than 1")) # throw error if too great
} # validate_plack()

# Validate parameter gen_samp
# x: character value, the answer for the question, generally starting with n or y
# returned value: -
validate_gen_samp <- function(x) {
	if (x == "") { # if answer is default
		return(TRUE) # default answer is TRUE
	} else if (substr(x = x, start = 1, stop = 1) == "n") { # if answer is no
		return(FALSE) # return FALSE
	} else if (substr(x = x, start = 1, stop = 1) == "y") { # if answer is yes
		return(TRUE) # return TRUE
	} else { # if answer is incorrect
		stop(paste_error_message("gen_samp", "must start with Y/y/N/n")) # throw error if answer is incorrect
	} # else
} # validate_gen_samp()

# Validate parameter calc_char
# x: character value, the answer for the question, generally starting with n or y
# returned value: -
validate_calc_char <- function(x) {
	if (x == "") { # if answer is default
		return(TRUE) # default answer is TRUE
	} else if (substr(x = x, start = 1, stop = 1) == "n") { # if answer is no
		return(FALSE) # return FALSE
	} else if (substr(x = x, start = 1, stop = 1) == "y") { # if answer is yes
		return(TRUE) # return TRUE
	} else { # if answer is incorrect
		stop(paste_error_message("calc_char", "must start with Y/y/N/n")) # throw error if answer is incorrect
	} # else
} # validate_calc_char()

# Validate parameter ch_fold
# x: character value, the answer for the question, generally starting with n or y
# returned value: -
validate_ch_fold <- function(x) {
	if (x == "") { # if answer is default
		return(FALSE) # default answer is FALSE
	} else if (substr(x = x, start = 1, stop = 1) == "n") { # if answer is no
		return(FALSE) # return FALSE
	} else if (substr(x = x, start = 1, stop = 1) == "y") { # if answer is yes
		return(TRUE) # return TRUE
	} else { # if answer is incorrect
		stop(paste_error_message("ch_fold", "must start with Y/y/N/n")) # throw error if answer is incorrect
	} # else
} # validate_ch_fold()

if (interactive()) combinatorial_wrapper() # call wrapper function, if R is in interactive mode
