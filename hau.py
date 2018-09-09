import argparse
import json
import readline

import spacy
nlp = spacy.load("en_core_web_sm", disable=["ner"])

from settings import setDebugMode
from rules import translate, translateUnknownPos, PHRASE_RULES

def translateAndPrint(text, debug):
	doc = nlp(text)
	for tree in doc.print_tree():
		if debug:
			print(json.dumps(doc.print_tree(), indent=2))
		
		#print(translate(tree, NP_RULES).inflect({"nominatiivi"}))
		if tree["POS_coarse"] == "VERB":
			fitree = translate(tree, PHRASE_RULES)
			if debug:
				fitree.printTree()
			
			print(fitree.inflect({"nominatiivi"}))
		else:
			fitree = translateUnknownPos(tree)
			if debug:
				fitree.printTree()
			
			print(fitree.inflect(set()))

def main():
	parser = argparse.ArgumentParser()
	parser.add_argument("text", nargs="?", type=str, help="the text to process")
	parser.add_argument("-d", "--debug", action="store_true", help="enable debug mode")
	args = parser.parse_args()
	
	setDebugMode(args.debug)
	
	if args.text:
		translateAndPrint(args.text, args.debug)
	else:
		while True:
			try:
				line = input("hau> ").lower()
				translateAndPrint(line, args.debug)
			except EOFError:
				break
		print("exit")

if __name__ == "__main__":
	main()

