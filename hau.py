# Hau-kääntäjä
# Copyright (c) Iikka Hauhio 2018
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as
# published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import argparse
import json
import readline

import spacy
nlp = spacy.load("en_core_web_sm", disable=["ner"])

import settings
from rules import translate, translateUnknownPos, PHRASE_RULES

def translateAndPrint(text, debug):
	settings.beginTranslation()
	doc = nlp(text)
	for tree in doc.print_tree():
		if debug:
			print(json.dumps(doc.print_tree(), indent=2))
		
		#print(translate(tree, NP_RULES).inflect({"nominatiivi"}))
		if tree["POS_coarse"] == "VERB":
			fitree = translate(tree, PHRASE_RULES)
			flags = {"nominatiivi"}
		else:
			fitree = translateUnknownPos(tree)
			flags = set()
		
		if debug:
			for tr_pair in settings.tr_pairs:
				print(tr_pair.enlemma, "->", tr_pair.filemma)
			
			fitree.printTree()
		
		print(fitree.inflect(flags))

def main():
	parser = argparse.ArgumentParser()
	parser.add_argument("text", nargs="?", type=str, help="the text to process")
	parser.add_argument("-d", "--debug", action="store_true", help="enable debug mode")
	args = parser.parse_args()
	
	settings.setDebugMode(args.debug)
	
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

