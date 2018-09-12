
debug_mode = False
def setDebugMode(val):
	global debug_mode
	debug_mode = val

def beginTranslation():
	global tr_pairs
	tr_pairs = []

def addTranslationPair(tr_pair):
	global tr_pairs
	tr_pairs.append(tr_pair)
