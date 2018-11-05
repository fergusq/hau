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

import random
from typing import Any, NamedTuple

import settings
from fitree import FiP, FiA, FiVP

OBJECT_CASES = {
	"jk": "akkusatiivi",
	"jokin": "akkusatiivi",
	"jku": "akkusatiivi",
	"joku": "akkusatiivi",
	
	"jtk": "partitiivi",
	"jtak": "partitiivi",
	"jotakin": "partitiivi",
	"jkta": "partitiivi",
	"jotakuta": "partitiivi",
	
	"jnk": "genetiivi",
	"jonkin": "genetiivi",
	"jkn": "genetiivi",
	"jonkun": "genetiivi",
	
	"jhk": "illatiivi",
	"johonkin": "illatiivi",
	"jnnk": "illatiivi",
	"jonnekin": "illatiivi",
	"johonkuhun": "illatiivi",
	
	"jstk": "elatiivi",
	"jostakin": "elatiivi",
	"jksta": "elatiivi",
	"jostakusta": "elatiivi",
	
	"jssk": "inessiivi",
	"jsskn": "inessiivi",
	"jossakin": "inessiivi",
	"jossakussa": "inessiivi",
	
	"jksk": "translatiivi",
	"jksik": "translatiivi",
	"joksikin": "translatiivi",
	"joksikuksi": "translatiivi",
	
	"jllek": "allatiivi",
	"jollekin": "allatiivi",
	"jklle": "allatiivi",
	"jollekulle": "allatiivi",
	
	"jltak": "ablatiivi",
	"joltakin": "ablatiivi",
	"jklta": "ablatiivi",
	"joltakulta": "ablatiivi",
	
	"jllak": "adessiivi",
	"jollakin": "adessiivi",
	"jollakulla": "adessiivi",
	
	"jnak": "essiivi",
	"jonakuna": "essiivi",
	"jkna": "essiivi",
	"jonakin": "essiivi",
}

trans = {
	"fi:n": {},
	"fi:a": {},
	"fi:v": {},
	"fi:r": {}
}

class TranslationPair(NamedTuple):
	enlemma: str
	filemma: str
	data: Any

with open("fiwn.txt", "r") as file:
	for i, line in enumerate(file):
		print("\r"+str(i), end="")
		ficode, filemma, encode, enlemma, rel = line.strip().split("\t")[:5]
		pos = ficode[:4]
		if rel == "synonym" and pos in trans:
			if enlemma not in trans[pos]:
				trans[pos][enlemma] = []
			
			words = [FiP(word) for word in filemma.split(" ")]
			
			if pos == "fi:v":
				for i, word in enumerate(words[1:]):
					if word.word in OBJECT_CASES:
						words.pop(i+1)
						objcase = OBJECT_CASES[word.word]
						break
				else:
					objcase = "akkusatiivi"
				
				data = (words[0], words[1:], objcase)
			else:
				data = FiA(words)
			
			trans[pos][enlemma].append(TranslationPair(enlemma=enlemma, filemma=filemma, data=data))

print()

def randomTranslation(word, pos):
	translations = trans[pos][word]
	tr_pair = random.choice(translations)
	settings.addTranslationPair(tr_pair)
	return tr_pair.data
