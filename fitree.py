from omorfi.omorfi import Omorfi

omorfi = Omorfi()
omorfi.load_analyser("/usr/local/share/omorfi/omorfi.analyse.hfst")
omorfi.load_generator("/usr/local/share/omorfi/omorfi.generate.hfst")

import settings

PROPERTIES = {
	"nominatiivi": [("CASE", "NOM")],
	"genetiivi": [("CASE", "GEN")],
	"partitiivi": [("CASE", "PAR")],
	"translatiivi": [("CASE", "TRA")],
	"essiivi": [("CASE", "ESS")],
	"inessiivi": [("CASE", "INE")],
	"elatiivi": [("CASE", "ELA")],
	"illatiivi": [("CASE", "ILL")],
	"adessiivi": [("CASE", "ADE")],
	"ablatiivi": [("CASE", "ABL")],
	"allatiivi": [("CASE", "ALL")],
	"abessiivi": [("CASE", "ABE")],
	
	"positiivi": [("CMP", "POS")],
	"komparatiivi": [("CMP", "CMP"), ("DRV", "MPI")],
	"superlatiivi": [("CMP", "SUP"), ("DRV", "IN²")],
	
	"yksikkö": [("NUM", "SG")],
	"monikko": [("NUM", "PL")],
	
	"minun": [("POSS", "SG1")],
	"sinun": [("POSS", "SG2")],
	"hänen": [("POSS", "3")],
	"meidän": [("POSS", "PL1")],
	"teidän": [("POSS", "PL2")],
	"heidän": [("POSS", "3")],
	
	"yks_1": [("PERS", "SG1"), ("NUM", "SG")],
	"yks_2": [("PERS", "SG2"), ("NUM", "SG")],
	"yks_3": [("PERS", "SG3"), ("NUM", "SG")],
	"mon_1": [("PERS", "PL1"), ("NUM", "PL")],
	"mon_2": [("PERS", "PL2"), ("NUM", "PL")],
	"mon_3": [("PERS", "PL3"), ("NUM", "PL")],
	"pe4": [("PERS", "PE4")],
	
	"preesens": [("TENSE", "PRESENT")],
	"preteriti": [("TENSE", "PAST")],
	
	"indikatiivi": [("MOOD", "INDV")],
	"potentiaali": [("MOOD", "POTN")],
	"konditionaali": [("MOOD", "COND")],
	"imperatiivi": [("MOOD", "IMPV")],
	
	"aktiivi": [("VOICE", "ACT")],
	"passiivi": [("VOICE", "PSS")],
	
	"kielto": [("NEG", "CON")],
	
	"-va": [("DRV", "VA")],
	"-tava": [("DRV", "TAVA")],
	"-nut": [("DRV", "NUT")],
	"-tu": [("DRV", "TU")],
	"-minen": [("DRV", "MINEN")],
	
	"-a": [("INF", "A")],
	"-e": [("INF", "E")],
	"-ma": [("INF", "MA")],
}

class FiP:
	def __init__(self, word, flags=None):
		self.word = word
		self.flags = flags or set()
	
	def printTree(self, indent=0):
		print(" "*indent + repr(self.word) + repr(self.flags))
	
	def inflect(self, par_flags):
		flags = par_flags | self.flags
		if "substantiivi" in flags:
			return self.inflectPos(flags, "NOUN")
		elif "adjektiivi" in flags:
			return self.inflectPos(flags, "ADJ")
		elif "verbi" in flags:
			return self.inflectPos(flags, "VERB")
		else:
			return self.inflectPos(flags, "NOUN")
	
	def inflectPos(self, flags, pos):
		flags = set(flags)
		properties = None
		prefix = ""
		if pos == "VERB":
			# passiivilauseen perfektin ja pluskvampergektin apuverbi olla taipuu aktiivissa (on tehty)
			if flags >= {"perfektin apuverbi", "pe4"}:
				flags -= {"pe4", "passiivi"}
				flags |= {"yks_3"}
			
			if flags & {"-va", "-tava", "-nut", "-tu"}:
				properties = ["WORD_ID", "UPOS", "DRV", "CMP", "NUM", "CASE"]
				values = {
					"WORD_ID": self.word,
					"UPOS": "VERB",
					"DRV": "-",
					"CMP": "POS",
					"NUM": "SG",
					"CASE": "NOM"
				}
			elif flags & {"-minen"}:
				properties = ["WORD_ID", "UPOS", "DRV", "NUM", "CASE"]
				values = {
					"WORD_ID": self.word,
					"UPOS": "VERB",
					"DRV": "-",
					"NUM": "SG",
					"CASE": "NOM"
				}
			elif flags & {"-a"}:
				properties = ["WORD_ID", "UPOS", "VOICE", "INF", "NUM", "CASE"]
				values = {
					"WORD_ID": self.word,
					"UPOS": "VERB",
					"VOICE": "ACT",
					"INF": "-",
					"NUM": "SG",
					"CASE": "LAT"
				}
				if flags & {"translatiivi"}:
					properties.remove("NUM")
			elif flags & {"-e", "-ma"}:
				properties = ["WORD_ID", "UPOS", "VOICE", "INF", "NUM", "CASE"]
				values = {
					"WORD_ID": self.word,
					"UPOS": "VERB",
					"VOICE": "ACT",
					"INF": "-",
					"NUM": "SG",
					"CASE": "INS"
				}
				if "-ma" in flags and self.word == "olla":
					properties.remove("NUM") # FIXME mitä purkkaa...
				
			elif flags & {"yks_1", "yks_2", "yks_3", "mon_1", "mon_2", "mon_3", "pe4"} or True:
				properties = ["WORD_ID", "UPOS", "VOICE", "MOOD", "TENSE", "PERS"]
				values = {
					"WORD_ID": self.word,
					"UPOS": "VERB",
					"VOICE": "ACT",
					"MOOD": "INDV",
					"TENSE": "PRESENT",
					"PERS": "SG1"
				}
			if self.word in {"olla", "pitää", "täytyä", "saattaa", "voida", "ei"}:
				values["UPOS"] = "AUX"
			
			if flags & {"kielto"}:
				if not flags & {"pe4"}:
					properties.remove("PERS")
				if flags & {"preteriti"}:
					properties.append("NUM")
					values["NUM"] = "SG"
				properties.append("NEG")
				values["NEG"] = "CON"
			
			if self.word == "ei":
				properties.remove("MOOD")
				properties.remove("TENSE")
				properties.insert(properties.index("UPOS")+1, "SUBCAT")
				values["SUBCAT"] = "NEG"
			
			if flags & {"konditionaali", "potentiaali", "imperatiivi"}:
				properties.remove("TENSE")
		else:
			if pos == "ADJ":
				wanted_properties = {"CMP", "NUM", "CASE"}
			else:
				wanted_properties = {"NUM", "CASE"}
			
			for flag in flags:
				if flag not in PROPERTIES:
					continue
				for key, _ in PROPERTIES[flag]:
					if key not in {"POSS", "DRV"}: # nämä ominaisuudet eivät ilmene perusmuodossa
						wanted_properties.add(key)
			
			properties = None
			
			for analys in omorfi.analyse(self.word):
				prefix2 = ""
				values2 = {}
				properties2 = []
				analys_str = analys["anal"][1:-1]
				if "[BOUNDARY=COMPOUND]" in analys_str:
					index = analys_str.rindex("[BOUNDARY=COMPOUND]") + len("[BOUNDARY=COMPOUND]")
					prefix2 = "[" + analys_str[:index]
					analys_str = analys_str[index + 1:]
				failed = False
				for part in analys_str.split("]["):
					key, val = part.split("=")
					if key in values2:
						failed = True
					if key in {"WEIGHT"}:
						continue
					properties2.append(key)
					values2[key] = val
				if failed or not wanted_properties < set(properties2):
					continue
				else:
					prefix = prefix2
					values = values2
					properties = properties2
		
		if properties is None:
			if flags and settings.debug_mode:
				return self.word + repr(flags)
			else:
				return self.word
		
		for flag in flags:
			if flag in PROPERTIES:
				for key, val in PROPERTIES[flag]:
					values[key] = val
		
		# heikko partitiivi lukuja varten, eli nominatiivin sijasta partitiivi
		# esim. ei kaksi luku vaan kaksi lukua
		if "heikko partitiivi" in flags and values["CASE"] == "NOM":
			values["CASE"] = "PAR"
		
		if "POSS" in values and "POSS" not in properties and pos not in ["ADJ", "PRON"] and "ei omistusliitettä" not in flags:
			# FIXME: adjektiiveilla ja pronomineilla voi olla joskus omistusliitteitä, mutta yleensä ei
			# tee jotain muuta kuin tällainen purkkaehto
			properties.append("POSS")
		
		if "DRV" in values and "CMP" in values and pos == "ADJ":
			properties.insert(properties.index("UPOS")+1, "DRV")
		
		omorfi_code = prefix
		for p in properties:
			if values[p]:
				omorfi_code += "[" + p + "=" + values[p] + "]"
		
		if settings.debug_mode:
			print(omorfi_code)
		
		for obj in omorfi.generate(omorfi_code):
			if "[" not in obj["surf"]:
				return obj["surf"]
		
		if not settings.debug_mode:
			return self.word
		elif flags:
			return self.word + repr(flags)
		else:
			return self.word + "{}"

class FiVP:
	def __init__(self, verb, subj=None, obj=None, objcase="akkusatiivi", subtrees=[], flags=None):
		self.verb = verb
		self.subj = subj
		self.obj = obj
		self.objcase = objcase
		self.subtrees = subtrees
		self.flags = set(flags or set())
		self.forwarded_flags = set() # liput jotka annetaan lapsille
		self.addSubjectBasedFlags()
	
	def printTree(self, indent=0):
		print(" "*indent + "VP" + repr(self.flags))
		print(" "*indent + " predicate")
		self.verb.printTree(indent+2)
		if self.subj:
			print(" "*indent + " subject")
			self.subj.printTree(indent+2)
		if self.obj:
			print(" "*indent + " object/" + self.objcase)
			self.obj.printTree(indent+2)
		for st in self.subtrees:
			print(" "*indent + " subtree")
			st.printTree(indent+2)
	
	def addSubjectBasedFlags(self):
		if not self.subj:
			self.forwarded_flags |= {"subjekti yks_3"}
		elif "minä" in self.subj.flags:
			self.forwarded_flags |= {"subjekti yks_1"}
		elif "sinä" in self.subj.flags:
			self.forwarded_flags |= {"subjekti yks_2"}
		elif "me" in self.subj.flags:
			self.forwarded_flags |= {"subjekti mon_1"}
		elif "te" in self.subj.flags:
			self.forwarded_flags |= {"subjekti mon_2"}
		elif "monikko" in self.subj.flags:
			self.forwarded_flags |= {"subjekti mon_3"}
		else:
			self.forwarded_flags |= {"subjekti yks_3"}
		
		self.flags |= self.forwarded_flags
		
	def inflect(self, flags):
		flags = flags|self.flags|{"verbi"}
		
		# puussa ylempänä olevan verbilausekkeen tietoja ei viedä tämän verbilausekkeen lapsille
		flags -= {"subjekti yks_1", "subjekti yks_2", "subjekti yks_3", "subjekti mon_1", "subjekti mon_2", "subjekti mon_3"}
		
		if "finiitti" in flags: # täydennä persoona subjektin perusteella
			if "subjekti yks_1" in self.flags:
				flags |= {"yks_1"}
			elif "subjekti yks_2" in self.flags:
				flags |= {"yks_2"}
			elif "subjekti mon_1" in self.flags:
				flags |= {"mon_1"}
			elif "subjekti mon_2" in self.flags:
				flags |= {"mon_2"}
			elif "subjekti mon_3" in self.flags:
				flags |= {"mon_3"}
			else:
				flags |= {"yks_3"}
		
		if "kielto" in flags and self.objcase == "akkusatiivi":
			objcase = "partitiivi"
		else:
			objcase = self.objcase
		
		fixed_objcase = self.obj and (objcase if objcase != "akkusatiivi" else fixAccusative(self.obj)) or ""
		passive_fixed_objcase = objcase if objcase != "akkusatiivi" else "nominatiivi"
		
		# muut lauseenjäsenet kuin predikaatti, subjekti ja objekti
		subtree_pairs = [(st, set()) for st in self.subtrees]
		pretrees = [(st, set()) for st in self.subtrees if "lauseen alkuun" in st.flags]
		posttrees = [(st, set()) for st in self.subtrees if "lauseen alkuun" not in st.flags]
		
		if flags & {"-a", "-e"}:
			template = [
				(self.subj, {"genetiivi"}),
				(self.obj, {fixed_objcase}),
				(FiNI("ei-") if "kielto" in flags else None, set()),
				(self.verb, flags),
			] + subtree_pairs
		elif flags & {"-minen", "-va", "-nut"}:
			template = subtree_pairs + [
				(self.subj, {"genetiivi"}),
				(self.obj, {fixed_objcase}),
				(FiNI("ei-") if "kielto" in flags else None, set()),
				(self.verb, flags),
			]
		elif flags & {"-tava", "-tu"}:
			template = subtree_pairs + [
				(self.subj, {"genetiivi"}),
				(self.subj and FiNI("toimesta"), set()),
				(self.verb, flags),
				(FiNI("ei-") if "kielto" in flags else None, set()),
				(self.obj, {passive_fixed_objcase}),
			]
		elif flags & {"passiivi", "pe4"}:
			template = pretrees + [
				(self.obj, {passive_fixed_objcase}),
				(FiP("ei") if "kielto" in flags else None, flags-{"kielto"}),
				(self.verb, flags),
				(self.subj, {"genetiivi"}),
				(self.subj and FiNI("toimesta"), set()),
			] + posttrees
		else:
			template = pretrees + [
				(self.subj, {"genetiivi" if "nollapersoona" in flags else "nominatiivi"}),
				(FiP("ei") if "kielto" in flags else None, flags-{"kielto"}),
				(self.verb, flags),
				(self.obj, {fixed_objcase}),
			] + posttrees
		ans = []
		for p, f in template:
			if p:
				ans.append(p.inflect(f|self.forwarded_flags))
		return " ".join(ans)

class FiA:
	def __init__(self, exprs, flags=None, compound=False, no_forwarding=False):
		self.exprs = exprs
		self.flags = flags or set()
		self.compound = compound
		self.no_forwarding = no_forwarding
	
	def printTree(self, indent=0):
		print(" "*indent + "A" + repr(self.flags) + " compound=" + repr(self.compound) + " no_forwarding=" + repr(self.no_forwarding))
		for expr in self.exprs:
			print(" "*indent + " subtree")
			expr.printTree(indent+2)
	
	def inflect(self, flags):
		if self.no_forwarding:
			flags = self.flags
		else:
			flags = flags|self.flags
		
		if self.compound:
			ans = ""
			for expr in self.exprs:
				word = expr.inflect(flags).strip()
				if not word:
					continue
				
				if ans.endswith("nen"):
					ans = ans[:-3] + "s"
				
				if " " in word:
					if ans:
						ans += "- "
					
					ans += word + " -"
				else:
					ans += word
			
			if ans.endswith(" -"):
				ans = ans[:-2]
			
			return ans
		else:
			ans = ""
			for expr in self.exprs:
				word = expr.inflect(flags).strip()
				if ans and ans[-1] == "-" and " " not in ans and " " not in word:
					ans = ans[:-1]
				elif ans and ans[-1] == "(":
					pass
				elif word and word[0] in {")", ",", ".", "!", "?"}:
					pass
				elif ans and word:
					ans += " "
				
				ans += word
			
			return ans

def niTree(tree, flags=None):
	return FiA([tree], flags or set(), no_forwarding=True)

class FiNI:
	def __init__(self, word):
		self.word = word
		self.flags = set()
	
	def printTree(self, indent=0):
		print(" "*indent + repr(self.word) + " (NI)")
	
	def inflect(self, flags):
		return self.word

class FiIf:
	def __init__(self, condflags, thenp, elsep, neg=False, no_forwarding=False, flags=set()):
		self.condflags = condflags
		self.thenp = thenp
		self.elsep = elsep
		self.neg = neg
		self.no_forwarding = no_forwarding
		self.flags = flags
	
	def printTree(self, indent=0):
		print(" "*indent + "If" + repr(self.flags) + " cond=" + repr(self.condflags) + " neg=" + repr(self.neg) + " no_forwarding=" + repr(self.no_forwarding))
		print(" "*indent + " then")
		self.thenp.printTree(indent+2)
		print(" "*indent + " else")
		self.elsep.printTree(indent+2)
	
	def inflect(self, flags):
		cond = flags & self.condflags
		if self.neg:
			cond = not cond
		
		flags = set() if self.no_forwarding else flags
		if cond:
			return self.thenp.inflect(flags|self.flags)
		else:
			return self.elsep.inflect(flags|self.flags)

def flagsFiIf(word, condflags, thenflags, elseflags):
	return FiIf(condflags, FiA([word], thenflags, no_forwarding=True), FiA([word], elseflags, no_forwarding=True), flags=word.flags)

def fixAccusative(val):
	if "monikko" in val.flags:
		return "nominatiivi"
	else:
		return "genetiivi"

class FiPossSuffixFromSubject:
	def __init__(self, subtree, no_forwarding=False, sibling_flags=set()):
		self.subtree = subtree
		self.flags = subtree.flags
		self.no_forwarding = no_forwarding
		self.sibling_flags = sibling_flags
	
	def printTree(self, indent):
		print(" "*indent + "PossSuffixFromSubject" + repr(self.flags) + " no_forwarding=" + repr(self.no_forwarding) + " sibling_flags=" + repr(self.sibling_flags))
		self.subtree.printTree(indent+1)
	
	def inflect(self, flags):
		condclags = flags|self.sibling_flags
		if "subjekti yks_1" in condclags:
			new_flags = {"minun"}
		elif "subjekti yks_2" in condclags:
			new_flags = {"sinun"}
		elif "subjekti yks_3" in condclags:
			new_flags = {"hänen"}
		elif "subjekti mon_1" in condclags:
			new_flags = {"meidän"}
		elif "subjekti mon_2" in condclags:
			new_flags = {"teidän"}
		elif "subjekti mon_3" in condclags:
			new_flags = {"hänen"}
		else:
			new_flags = {"hänen"}
		
		if not self.no_forwarding:
			return self.subtree.inflect(flags|new_flags)
		else:
			return self.subtree.inflect(new_flags)

