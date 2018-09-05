import argparse
import json
import copy
import random

import readline

import spacy
from omorfi.omorfi import Omorfi
omorfi = Omorfi()
omorfi.load_analyser("/usr/local/share/omorfi/omorfi.analyse.hfst")
omorfi.load_generator("/usr/local/share/omorfi/omorfi.generate.hfst")

nlp = spacy.load("en_core_web_sm", disable=["ner"])

# TODO
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
		return self.word + "(" + form + ")"
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
					if key != "POSS":
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
			if flags:
				return self.word + repr(flags)
			else:
				return self.word
		
		for flag in flags:
			if flag in PROPERTIES:
				for key, val in PROPERTIES[flag]:
					values[key] = val
		
		if "POSS" in values and "POSS" not in properties and pos not in ["ADJ", "PRON"] and "ei omistusliitettä" not in flags:
			# FIXME: adjektiiveilla ja pronomineilla voi olla joskus omistusliitteitä, mutta yleensä ei
			# tee jotain muuta kuin tällainen purkkaehto
			properties.append("POSS")
		
		omorfi_code = prefix
		for p in properties:
			if values[p]:
				omorfi_code += "[" + p + "=" + values[p] + "]"
		
		print(omorfi_code)
		
		for obj in omorfi.generate(omorfi_code):
			if "[" not in obj["surf"]:
				return obj["surf"]
		
		if flags:
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
		self.flags = flags or set()
	def inflect(self, flags):
		flags = flags|self.flags|{"verbi"}
		if "finiitti" in flags: # täydennä persoona subjektin perusteella
			if not self.subj:
				flags = flags | {"yks_3"}
			elif "minä" in self.subj.flags:
				flags = flags | {"yks_1"}
			elif "sinä" in self.subj.flags:
				flags = flags | {"yks_2"}
			elif "me" in self.subj.flags:
				flags = flags | {"mon_1"}
			elif "te" in self.subj.flags:
				flags = flags | {"mon_2"}
			elif "monikko" in self.subj.flags:
				flags = flags | {"mon_3"}
			else:
				flags = flags | {"yks_3"}
		
		if "kielto" in flags and self.objcase == "akkusatiivi":
			objcase = "partitiivi"
		else:
			objcase = self.objcase
		
		fixed_objcase = self.obj and (objcase if objcase != "akkusatiivi" else fixAccusative(self.obj)) or ""
		
		subtree_pairs = [(st, set()) for st in self.subtrees]
		pretrees = [(st, set()) for st in self.subtrees if "lauseen alkuun" in st.flags]
		posttrees = [(st, set()) for st in self.subtrees if "lauseen alkuun" not in st.flags]
		
		if flags & {"-a", "-e", "-va", "-nut"}:
			template = [
				(self.subj, {"genetiivi"}),
				(self.obj, {fixed_objcase}),
				(FiNI("ei-") if "kielto" in flags else None, set()),
				(self.verb, flags),
			] + subtree_pairs
		elif flags & {"-minen"}:
			template = [
				(self.subj, {"genetiivi"}),
				(self.obj, {fixed_objcase}),
				(FiNI("ei-") if "kielto" in flags else None, set()),
				(self.verb, flags),
			] + subtree_pairs
		elif flags & {"-tava", "-tu"}:
			template = subtree_pairs + [
				(self.subj, {"genetiivi"}),
				(self.subj and FiNI("toimesta"), set()),
				(self.verb, flags),
				(FiNI("ei-") if "kielto" in flags else None, set()),
				(self.obj, {objcase if objcase != "akkusatiivi" else "nominatiivi"}),
			]
		elif flags & {"passiivi", "pe4"}:
			template = pretrees + [
				(self.obj, {objcase if objcase != "akkusatiivi" else "nominatiivi"}),
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
				ans.append(p.inflect(f))
		return " ".join(ans)

class FiA:
	def __init__(self, exprs, flags=None, compound=False):
		self.exprs = exprs
		self.flags = flags or set()
		self.compound = compound
	def inflect(self, flags):
		if self.compound:
			ans = ""
			for expr in self.exprs:
				word = expr.inflect(flags|self.flags).strip()
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
				if ans:
					ans += " "
				
				ans += expr.inflect(flags|self.flags).strip()
			
			return ans

class FiNI:
	def __init__(self, word):
		self.word = word
		self.flags = set()
	def inflect(self, flags):
		return self.word

class FiIf:
	def __init__(self, word, condflags, thenflags, elseflags, neg=False):
		self.word = word
		self.condflags = condflags
		self.thenflags = thenflags
		self.elseflags = elseflags
		self.neg = neg
	def inflect(self, flags):
		cond = flags & self.condflags
		if self.neg:
			cond = not cond
		
		if cond:
			return self.word.inflect(self.thenflags)
		else:
			return self.word.inflect(self.elseflags)

def matchesObj(pattern, obj):
	ans = {}
	if (isinstance(pattern, list) or isinstance(pattern, set) or isinstance(pattern, dict)) and isinstance(obj, str):
		return obj in pattern, ans
	elif type(pattern) != type(obj):
		return False, None
	elif isinstance(pattern, str) or isinstance(pattern, int):
		return pattern == obj, ans
	elif isinstance(pattern, list):
		for pat in pattern:
			for i, val in enumerate(obj):
				m, d = matchesObj(pat, val)
				if m:
					ans.update(d)
					break
			else:
				return False, None
			if "_del" in pat:
				del obj[i]
	elif isinstance(pattern, dict):
		if "_not" in pattern:
			m, d = matchesObj(pattern["_not"], obj)
			if m:
				return False, None
		for key in pattern:
			if key in ["_del", "_name", "_not"]:
				continue
			if key not in pattern:
				return False, None
			m, d = matchesObj(pattern[key], obj[key])
			if not m:
				return False, None
			ans.update(d)
			if "_del" in pattern[key]:
				del obj[key]
	if "_name" in pattern:
		ans[pattern["_name"]] = obj
	return True, ans

class Rule:
	def __init__(self, pattern, f):
		self.pattern = pattern
		self.translate = f
	def matches(self, obj):
		return matchesObj(self.pattern, obj)

def match(rules, pattern):
	def f(g):
		rules.append(Rule(pattern, g))
	return f

def treeToStr(tree):
	words = [tree["word"]]
	for subtree in tree["modifiers"]:
		words.append(treeToStr(subtree))
	return " ".join(words)

def translate(tree, rules):
	print("translate(" + tree["word"] + ", ", end="")
	if rules == AP_RULES:
		print("AP_RULES)")
	elif rules == ADVP_RULES:
		print("ADVP_RULES)")
	elif rules == NP_RULES:
		print("NP_RULES)")
	elif rules == VP_RULES:
		print("VP_RULES)")
	elif rules == PHRASE_RULES:
		print("PHRASE_RULES)")
	else:
		print("<rules#"+str(len(rules))+">)")
	for i, rule in enumerate(rules):
		ctree = copy.deepcopy(tree)
		m, d = rule.matches(ctree)
		if m:
			return rule.translate(ctree, d, rules[i:])
	return FiNI("[" + treeToStr(tree) + "]")

def translateRest(tree):
	ts = []
	for subtree in tree["modifiers"]:
		ts.append(translateUnknownPos(subtree).inflect(set()))
	return FiNI(" ".join(ts))

def translateRestToArray(tree):
	ts = []
	for subtree in tree["modifiers"]:
		ts.append(translateUnknownPos(subtree))
	return ts

def translateUnknownPos(tree):
	if tree["POS_coarse"] == "ADJ":
		return translate(tree, AP_RULES)
	elif tree["POS_coarse"] == "ADV" or tree["arc"] == "npadvmod":
		return translate(tree, ADVP_RULES)
	elif tree["POS_coarse"] == "NOUN":
		return translate(tree, NP_RULES)
	elif tree["POS_coarse"] == "VERB":
		return translate(tree, VP_RULES)
	elif tree["POS_coarse"] == "ADP":
		return translate(tree, PP_RULES)
	else:
		return FiNI("[" + treeToStr(tree) + "]")

AP_RULES = []
NP_RULES = []
VP_RULES = []
PP_RULES = []
ADVP_RULES = []
PHRASE_RULES = []

@match(PHRASE_RULES, {"modifiers": [{"_del": True, "_name": "subtree", "arc": "ccomp"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	val2 = translate(params["subtree"], PHRASE_RULES)
	return FiA([val, FiNI(","), FiNI("että"), FiNI(val2.inflect(set()))], val.flags)

@match(VP_RULES, {"modifiers": [{"_del": True, "_name": "subtree", "arc": "ccomp"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	val2 = translate(params["subtree"], PHRASE_RULES)
	return FiA([val, FiNI(","), FiNI("että"), FiNI(val2.inflect(set()))], val.flags)

def addConjunction(en_conj, fi_conj):
	@match(PHRASE_RULES, {"modifiers": [{"_del": True, "_name": "subtree", "arc": "advcl", "modifiers": [{"_del": True, "word": en_conj, "POS_fine": "IN"}]}]})
	def _(tree, params, rest):
		val = translate(tree, rest)
		val2 = translate(params["subtree"], PHRASE_RULES)
		return FiA([val, FiNI(","), FiNI(fi_conj), FiNI(val2.inflect(set())), FiNI(",")], val.flags)

addConjunction("if", "jos")
addConjunction("unless", "paitsi jos")
addConjunction("while", "kun")
addConjunction("until", "kunnes")
addConjunction("for", "sillä")

@match(PHRASE_RULES, {"modifiers": [{"_del": True, "_name": "subtree", "arc": "advcl", "POS_fine": "VBG"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	val2 = translate(params["subtree"], VP_RULES)
	return FiA([val, FiNI(val2.inflect({"-e", "inessiivi", "hänen"}))], val.flags)

# rinnastuskonjunktiot
def addCConjunction(target_rules, cc_rules, en_conj, fi_conj):
	@match(target_rules, {"modifiers": [
		{"_del": True, "word": en_conj, "POS_fine": "CC"},
		{"_del": True, "_name": "cc", "arc": "conj"}
	]})
	def _(tree, params, rest):
		val = translate(tree, rest)
		val2 = translate(params["cc"], cc_rules)
		return FiA([val, FiNI(fi_conj), val2], val.flags)

addCConjunction(PHRASE_RULES, PHRASE_RULES, "and", "ja")
addCConjunction(PHRASE_RULES, PHRASE_RULES, "but", "mutta")
addCConjunction(PHRASE_RULES, PHRASE_RULES, "or", "tai")

addCConjunction(NP_RULES, NP_RULES, "and", "ja")
addCConjunction(NP_RULES, NP_RULES, "but", "mutta")
addCConjunction(NP_RULES, NP_RULES, "or", "tai")

addCConjunction(VP_RULES, VP_RULES, "and", "ja")
addCConjunction(VP_RULES, VP_RULES, "but", "vaan")
addCConjunction(VP_RULES, VP_RULES, "or", "tai")

# relatiivilauseet
@match(NP_RULES, {"modifiers": [{"_del": True, "_name": "relcl", "arc": "relcl", "POS_coarse": "VERB"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	val2 = translate(params["relcl"], PHRASE_RULES)
	return FiA([val, FiNI(","), FiNI(val2.inflect(set())), FiNI(",")], val.flags)

# artikkelit
@match(NP_RULES, {"modifiers": [{"_del": True, "word": {"a", "an"}, "POS_fine": "DT"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	return FiA([FiP("eräs"), val], val.flags|{"epämääräinen"})

@match(NP_RULES, {"modifiers": [{"_del": True, "word": "the", "POS_fine": "DT"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	#return FiA([FiP("se"), val], val.flags)
	return val

# tämä, tuo, nämä, nuo
@match(NP_RULES, {"modifiers": [{"_del": True, "word": "this", "POS_fine": "DT"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	return FiA([FiP("tämä"), val], val.flags)

@match(NP_RULES, {"modifiers": [{"_del": True, "word": "that", "POS_fine": "DT"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	return FiA([FiP("tuo"), val], val.flags)

@match(NP_RULES, {"modifiers": [{"_del": True, "word": "these", "POS_fine": "DT"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	return FiA([FiP("nämä", {"monikko"}), val], val.flags)

@match(NP_RULES, {"modifiers": [{"_del": True, "word": "those", "POS_fine": "DT"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	return FiA([FiP("nuo", {"monikko"}), val], val.flags)

# kaikki
@match(NP_RULES, {"modifiers": [{"_del": True, "word": "all", "POS_fine": "PDT"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	return FiA([FiP("kaikki"), val], val.flags)

# kuinka
@match(AP_RULES, {"modifiers": [{"_del": True, "word": "how", "POS_fine": "WRB"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	return FiA([FiNI("kuinka"), val], val.flags)

# partisiippi
@match(NP_RULES, {"modifiers": [{"_name": "subtree", "_del": True, "POS_fine": "VBN"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	acl = translate(params["subtree"], VP_RULES)
	return FiA([FiA([acl], {"-tu"}), val], val.flags)

# yhdyssanat
@match(NP_RULES, {"modifiers": [{"_del": True, "_name": "prefix", "POS_coarse": "NOUN", "arc": "compound"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	val2 = translate(params["prefix"], NP_RULES)
	return FiA([FiNI(val2.inflect({"nominatiivi"})), val], val.flags, compound=True)

@match(NP_RULES, {"modifiers": [{"_del": True, "_name": "proper", "POS_coarse": "PROPN", "arc": "amod"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	val2 = translate(params["proper"], NP_RULES)
	return FiA([FiNI(val2.inflect({"genetiivi"})), val], val.flags)

def prepositionToCase(preposition, case, verb=None, fi_preposition=None, fi_postposition=None, after=False):
	fi_pre_val = FiNI(fi_preposition or "")
	fi_post_val = FiNI(fi_postposition or "")
	@match(PP_RULES, {"word": preposition, "POS_fine": "IN", "modifiers": [{"_name": "subtree", "_del": True, "POS_coarse": "NOUN"}]})
	def _(tree, params, rest):
		val = translateRest(tree)
		val2 = translate(params["subtree"], NP_RULES)
		pp = FiNI(val2.inflect({case}))
		if after:
			return FiA([val, fi_pre_val, pp, fi_post_val])
		else:
			return FiA([fi_pre_val, pp, fi_post_val, val])
	
	@match(NP_RULES, {"modifiers": [{"_del": True, "word": preposition, "POS_fine": "IN", "modifiers": [{"_name": "subtree", "POS_coarse": "NOUN"}]}]})
	def _(tree, params, rest):
		val = translate(tree, rest)
		val2 = translate(params["subtree"], NP_RULES)
		pp = FiNI(val2.inflect({case}))
		verbp = FiP(verb, {"verbi", "-va"}) if verb else FiNI("")
		if after:
			return FiA([val, verbp, fi_pre_val, pp, fi_post_val], val.flags)
		return FiA([fi_pre_val, pp, fi_post_val, verbp, val], val.flags)
	
	@match(PHRASE_RULES, {"word": "was", "POS_fine": "VBD", "modifiers": [
		{"lemma": "there", "_del": True, "POS_fine": "EX"},
		{"_name": "subj", "_del": True, "modifiers": [{"_del": True, "word": preposition, "POS_fine": "IN", "modifiers": [{"_name": "subtree", "POS_coarse": "NOUN"}]}]}
	]})
	def _(tree, params, rest):
		val = translateRest(tree)
		subj = translate(params["subj"], NP_RULES)
		place = translate(params["subtree"], NP_RULES)
		verbp = FiNI(FiP(verb or "olla").inflect({"verbi", "preteriti", "yks_3"}))
		return FiA([FiNI(subj.inflect({"nominatiivi"})), verbp, fi_pre_val, FiNI(place.inflect({case})), fi_post_val, val])
	
	#@match(PHRASE_RULES, {"modifiers": [{"_del": True, "word": preposition, "POS_fine": "IN", "modifiers": [{"_name": "subtree", "POS_coarse": "NOUN"}]}]})
	#def _(tree, params, rest):
	#	val = translate(tree, rest)
	#	val2 = translate(params["subtree"], NP_RULES)
	#	return FiA([val, FiNI(val2.inflect({case}))], val.flags)

prepositionToCase("of", "genetiivi")
prepositionToCase("on", "adessiivi", "olla")
prepositionToCase("at", "adessiivi", "olla")
prepositionToCase("to", "illatiivi", "mennä")
prepositionToCase("into", "illatiivi", "olla")
prepositionToCase("from", "elatiivi", "olla")
prepositionToCase("about", "elatiivi", "kertoa")
prepositionToCase("in", "inessiivi", "olla")
prepositionToCase("for", "allatiivi", "olla")
prepositionToCase("as", "essiivi", after=True)
prepositionToCase("through", "genetiivi", fi_postposition="läpi")
prepositionToCase("with", "genetiivi", fi_postposition="kanssa")
prepositionToCase("over", "genetiivi", fi_postposition="yli")
prepositionToCase("after", "genetiivi", fi_postposition="jälkeen")
prepositionToCase("before", "partitiivi", fi_postposition="ennen")
prepositionToCase("like", "nominatiivi", fi_preposition="kuten")
prepositionToCase("above", "genetiivi", fi_postposition="yllä")
prepositionToCase("below", "genetiivi", fi_postposition="alla")
prepositionToCase("up", "partitiivi", fi_preposition="ylös")
prepositionToCase("down", "partitiivi", fi_preposition="alas")

# his thing -> hänen asiansa
def addPossPronoun(pronoun, fi_pronoun, flags):
	@match(NP_RULES, {"modifiers": [{"_del": True, "word": pronoun, "arc": "poss"}]})
	def _(tree, params, rest):
		val = translate(tree, rest)
		return FiA([FiNI(fi_pronoun), val], val.flags|flags)

addPossPronoun("my", "minun", {"minun"})
addPossPronoun("your", "sinun", {"sinun"})
addPossPronoun({"her", "his"}, "hänen", {"hänen"})
addPossPronoun("its", "sen", {})
addPossPronoun("our", "meidän", {"meidän"})
addPossPronoun("their", "niiden", {})

# the person's thing -> henkilön asia
@match(NP_RULES, {"modifiers": [{"_del": True, "_name": "ap", "POS_coarse": "NOUN", "arc": "poss", "modifiers": [{"_del": True, "word": "'s", "POS_fine": "POS"}]}]})
def _(tree, params, rest):
	ap = params["ap"]
	val = translate(tree, rest)
	val2 = translate(ap, NP_RULES)
	return FiA([FiNI(val2.inflect({"genetiivi"})), val], val.flags)

# something nice -> jokin kiva
@match(NP_RULES, {"lemma": "something", "modifiers": [{"_del": True, "_name": "ap", "arc": "amod"}]})
def _(tree, params, rest):
	ap = params["ap"]
	val = translate(tree, rest)
	val2 = translate(ap, AP_RULES)
	return FiA([FiA([val], {"ei omistusliitettä"}), val2], val.flags)

# nice thing -> kiva asia
@match(NP_RULES, {"modifiers": [{"_del": True, "_name": "ap", "arc": "amod"}]})
def _(tree, params, rest):
	ap = params["ap"]
	val = translate(tree, rest)
	val2 = translate(ap, AP_RULES)
	return FiA([FiA([val2], {"ei omistusliitettä"}), val], val.flags)

# very nice -> hyvin kiva
@match(AP_RULES, {"modifiers": [{"_del": True, "_name": "ap", "POS_coarse": {"ADV", "ADP"}, "arc": "advmod"}]})
def _(tree, params, rest):
	ap = params["ap"]
	val = translate(tree, rest)
	val2 = translate(ap, ADVP_RULES)
	return FiA([FiNI(val2.inflect(set())), val])

# very slowly -> hyvin hitaasti
@match(ADVP_RULES, {"modifiers": [{"_del": True, "_name": "ap", "POS_coarse": {"ADV", "ADP"}, "arc": "advmod"}]})
def _(tree, params, rest):
	ap = params["ap"]
	val = translate(tree, rest)
	val2 = translate(ap, ADVP_RULES)
	return FiA([val2, val])

# the thing above -> asia yllä
@match(NP_RULES, {"modifiers": [{"_del": True, "_name": "ap", "POS_coarse": {"ADV", "ADP"}, "arc": "advmod"}]})
def _(tree, params, rest):
	ap = params["ap"]
	val = translate(tree, rest)
	val2 = translate(ap, ADVP_RULES)
	return FiA([val, FiNI(val2.inflect(set()))])

# komparatiivi
@match(AP_RULES, {"POS_fine": "JJR"})
def _(tree, params, rest):
	val = translate(tree, rest[1:])
	return FiA([val], {"komparatiivi"})

# superlatiivi
@match(AP_RULES, {"POS_fine": "JJS"})
def _(tree, params, rest):
	val = translate(tree, rest[1:])
	return FiA([val], {"superlatiivi"})

# doing -> tekevä
@match(AP_RULES, {"POS_fine": "VBG"})
def _(tree, params, rest):
	val = translate(tree, VP_RULES)
	return FiA([val], {"-va"})

# done -> tehnyt
@match(AP_RULES, {"POS_fine": {"VBN", "VBD"}})
def _(tree, params, rest):
	val = translate(tree, VP_RULES)
	return FiA([val], {"-nut"})

# able to do -> kykenevä tekemään
@match(AP_RULES, {"lemma": "able", "modifiers": [
	{"_del": True, "_name": "subtree", "POS_fine": "VB", "modifiers": [{"_del": True, "word": "to", "POS_fine": "TO"}]}
]})
def _(tree, params, rest):
	val = translateRest(tree)
	val2 = translate(params["subtree"], VP_RULES)
	# TODO: predikatiivimuoto erikseen: kykenevä tekemään
	return FiA([FiNI(val2.inflect({"-minen", "illatiivi"})), FiP("kykenevä"), val])

def addAdjective(en_adj, fi_adj):
	# TODO vertailumuodot
	@match(AP_RULES, {"lemma": en_adj, "POS_coarse": "ADJ"})
	def _(tree, params, rest):
		val = translateRest(tree)
		return FiA([FiP(fi_adj), val])

addAdjective("good", "hyvä")
addAdjective("new", "uusi")
addAdjective("first", "ensimmäinen")
addAdjective("last", "viimeinen")
addAdjective("long", "pitkä")
addAdjective("great", "mahtava")
addAdjective("little", "pieni")
addAdjective("own", "oma")
addAdjective("other", "muu")
addAdjective("old", "vanha")
addAdjective("right", "oikea")
addAdjective("big", "iso")
addAdjective("high", "korkea")
addAdjective("different", "erilainen")
addAdjective("small", "pieni")
addAdjective("large", "suuri")
addAdjective("next", "seuraava")
addAdjective("early", "aikainen")
addAdjective("young", "nuori")
addAdjective("important", "tärkeä")
addAdjective("few", "harva")
addAdjective("public", "julkinen")
addAdjective("bad", "huono")
addAdjective("same", "sama")

addAdjective("left", "vasen")

def addAdverb(en_adv, fi_adv, flags=set()):
	@match(ADVP_RULES, {"lemma": en_adv, "POS_coarse": "ADV"})
	def _(tree, params, rest):
		val = translateRest(tree)
		return FiA([FiP(fi_adv), val], flags)

addAdverb("where", "missä", {"lauseen alkuun"})
addAdverb("when", "milloin", {"lauseen alkuun"})
addAdverb("why", "miksi", {"lauseen alkuun"})

addAdverb("up", "ylös")
addAdverb("down", "alas")
addAdverb("forward", "eteenpäin")
addAdverb("backward", "taaksepäin")
addAdverb("left", "vasemmalle")
addAdverb("right", "oikealle")
addAdverb("above", "yllä")
addAdverb("below", "alla")

# thing to do -> asia tehdä
@match(NP_RULES, {"modifiers": [{"_del": True, "_name": "subtree", "POS_fine": "VB", "modifiers": [{"_del": True, "word": "to", "POS_fine": "TO"}]}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	val2 = translate(params["subtree"], VP_RULES)
	return FiA([val, FiNI(val2.inflect({"-a"}))], val.flags)

# is done -> tehdään
@match(VP_RULES, {"POS_fine": "VBN", "modifiers": [{"_del": True, "arc": "auxpass"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	return FiA([val], {"passiivi", "pe4"})

# is doing -> [poistetaan apuverbi]
@match(VP_RULES, {"modifiers": [{"_del": True, "lemma": "be", "arc": "aux"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	return FiA([val], {})

# don't do not -> ei tee
@match(VP_RULES, {"modifiers": [{"_del": True, "word": {"not", "n't"}, "arc": "neg"}, {"_del": True, "lemma": "do", "arc": "aux"}]})
def _(tree, params, rest):
	val = translate(tree, rest[1:])
	return FiA([val], {"kielto"})

# do not -> ei tee
@match(VP_RULES, {"modifiers": [{"_del": True, "word": {"not", "n't"}, "arc": "neg"}]})
def _(tree, params, rest):
	val = translate(tree, rest[1:])
	return FiA([val], {"kielto"})

# do never -> ei tee [ikinä]
@match(VP_RULES, {"modifiers": [{"arc": "neg"}]})
def _(tree, params, rest):
	val = translate(tree, rest[1:])
	return FiA([val], {"kielto"})

# have to do -> olla tehtävä (subjektilla)
@match(VP_RULES, {"lemma": "have", "modifiers": [
	{"_del": True, "_name": "subj", "arc": "nsubj"},
	{"_del": True, "_name": "subtree", "POS_fine": "VB", "modifiers": [{"_del": True, "word": "to", "POS_fine": "TO"}]}
]})
def _(tree, params, rest):
	val = translateRest(tree)
	val2 = translate(params["subtree"], VP_RULES)
	subj = translate(params["subj"], NP_RULES)
	return FiVP(verb=FiA([FiP("olla"), FiNI(val2.inflect({"-tava"})), val]), subj=subj, flags={"nollapersoona"})

# have done -> olla tehnyt
def addAuxVerb(verb, fi_verb, verbflags, compflags, phraseflags=set(), compgen=None):
	@match(VP_RULES, {"modifiers": [{"_del": True, "lemma": verb, "arc": "aux"}]})
	def _(tree, params, rest):
		val = translate(tree, rest)
		if isinstance(val, FiVP):
			valverb = val.verb
			val.verb = FiA([FiP(fi_verb, verbflags), compgen(valverb) if compgen else FiNI(valverb.inflect(compflags))])
			val.flags |= phraseflags
			return val
		else:
			return FiA([FiP(fi_verb, verbflags), compgen(val) if compgen else FiNI(val.inflect(compflags))])

addAuxVerb("must", "täytyä", {}, {"-a"}, {"nollapersoona"})
addAuxVerb({"can", "ca"}, "voida", {}, {"-a"})
addAuxVerb("could", "voida", {"konditionaali"}, {"-a"})
addAuxVerb("may", "saattaa", {}, {"-a"})
addAuxVerb("might", "saattaa", {"konditionaali"}, {"-a"})
addAuxVerb("shall", "saada", {}, {"-a"})
addAuxVerb("should", "pitää", {"konditionaali"}, {"-a"}, {"nollapersoona"})
addAuxVerb("will", "tulla", {}, {"-ma", "illatiivi"})
addAuxVerb("have", "olla", {"perfektin apuverbi"}, {}, compgen=lambda val: FiIf(val, {"passiivi"}, {"-tu"}, {"-nut"}))

# have to do -> olla tehtävä
@match(VP_RULES, {"lemma": "have", "modifiers": [{"_del": True, "_name": "subtree", "POS_fine": "VB", "modifiers": [{"_del": True, "word": "to", "POS_fine": "TO"}]}]})
def _(tree, params, rest):
	val = translateRest(tree)
	val2 = translate(params["subtree"], VP_RULES)
	return FiA([FiP("olla"), FiNI(val2.inflect({"-tava"})), val])

# do to do -> tehdä tehdäkseen
@match(VP_RULES, {"modifiers": [{"_del": True, "_name": "subtree", "POS_fine": "VB", "modifiers": [{"_del": True, "word": "to", "POS_fine": "TO"}]}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	val2 = translate(params["subtree"], VP_RULES)
	return FiA([val, FiNI(val2.inflect({"verbi", "-a", "translatiivi", "hänen"}))], val.flags)

@match(PHRASE_RULES, {"POS_fine": "VBP"})
def _(tree, params, rest):
	val = translate(tree, VP_RULES)
	return FiNI(val.inflect({"verbi", "preesens", "finiitti"}))

@match(PHRASE_RULES, {"POS_fine": "VBZ"})
def _(tree, params, rest):
	val = translate(tree, VP_RULES)
	return FiNI(val.inflect({"verbi", "preesens", "finiitti"}))

@match(PHRASE_RULES, {"POS_fine": "VBD"})
def _(tree, params, rest):
	val = translate(tree, VP_RULES)
	return FiNI(val.inflect({"verbi", "preteriti", "finiitti"}))

# apuverbin aikamuoto
@match(PHRASE_RULES, {"POS_fine": {"VBN", "VBG", "VB"}, "modifiers": [{"POS_fine": {"VBZ", "VBP", "MD"}}]})
def _(tree, params, rest):
	val = translate(tree, VP_RULES)
	return FiNI(val.inflect({"verbi", "pe4" if "passiivi" in val.flags else "finiitti"}))

@match(PHRASE_RULES, {"POS_fine": {"VBN", "VBG", "VB"}, "modifiers": [{"POS_fine": {"VBD"}}]})
def _(tree, params, rest):
	val = translate(tree, VP_RULES)
	return FiNI(val.inflect({"verbi", "preteriti", "pe4" if "passiivi" in val.flags else "finiitti"}))

# by something -> jonkin toimesta
@match(PP_RULES, {"word": "by", "arc": "agent", "modifiers": [{"_name": "subtree", "_del": True, "POS_coarse": "NOUN"}]})
def _(tree, params, rest):
	val = translateRest(tree)
	val2 = translate(params["subtree"], NP_RULES)
	return FiA([FiNI(val2.inflect({"genetiivi"})), FiNI("toimesta"), val])

# as something -> kuin jokin
@match(PP_RULES, {"word": "as", "POS_fine": "IN", "modifiers": [{"_name": "subtree", "_del": True, "POS_coarse": "NOUN"}]})
def _(tree, params, rest):
	val = translateRest(tree)
	val2 = translate(params["subtree"], NP_RULES)
	return FiA([val, FiNI("kuin"), FiNI(val2.inflect({"nominatiivi"}))])

@match(AP_RULES, {"modifiers": [{"_del": True, "word": "as", "POS_fine": "IN", "modifiers": [{"_name": "subtree", "POS_coarse": "NOUN"}]}]})
def _(tree, params, rest):
	val = translateRest(tree)
	val2 = translate(params["subtree"], NP_RULES)
	return FiA([val, FiNI("kuin"), FiNI(val2.inflect({"nominatiivi"}))], val.flags)

def fixAccusative(val):
	if "monikko" in val.flags:
		return "nominatiivi"
	else:
		return "genetiivi"

def addVerb(en_verb, verb, object_case):
	def handleAccusative(val):
		if object_case == "akkusatiivi":
			return fixAccusative(val)
		else:
			return object_case
	
	parts = en_verb.split(" ")
	obj_pattern_noun = {"_name": "obj", "POS_coarse": {"NOUN", "PROPN", "PRON", "DET"}, "_not": {"arc": {"nsubj", "npadvmod"}}}
	obj_pattern_adj = {"_name": "obj", "POS_coarse": "ADJ", "_not": {"arc": "nsubj"}}
	if len(parts) > 1:
		en_verb = parts[0]
		prepositions = []
		for preposition in parts[1:]:
			obj_pattern_noun = {"word": preposition, "POS_fine": "IN", "modifiers": [obj_pattern_noun]}
			obj_pattern_adj = {"word": preposition, "POS_fine": "IN", "modifiers": [obj_pattern_adj]}
			prepositions = {"word": preposition, "POS_fine": "IN", "modifiers": prepositions}
	else:
		prepositions = []
	
	obj_pattern_noun["_del"] = True
	obj_pattern_adj["_del"] = True
	
	@match(NP_RULES, {"modifiers": [{"_del": True, "_name": "subtree", "lemma": en_verb, "POS_fine": "VBG"}]})
	def _(tree, params, rest):
		val = translate(tree, rest)
		verbp = translate(params["subtree"], VP_RULES)
		return FiA([FiA([verbp], {"-va"}), val], val.flags)
	
	@match(PHRASE_RULES, {"word": "was", "POS_fine": "VBD", "modifiers": [
		{"lemma": "there", "_del": True, "POS_fine": "EX"},
		{"_name": "subj", "_del": True, "modifiers": [{"word": en_verb, "POS_fine": "VBG", "modifiers": [obj_pattern_noun]}]}
	]})
	def _(tree, params, rest):
		val = translateRest(tree)
		subj = translate(params["subj"], NP_RULES)
		place = translate(params["obj"], NP_RULES)
		case = handleAccusative(place)
		return FiA([FiNI(subj.inflect({"nominatiivi"})), FiNI(FiP(verb, {"verbi"}).inflect({"preteriti", "yks_3"})), FiNI(place.inflect({case})), val])
	
	def addObjVerbRule(obj_pattern, obj_rules):
		# TODO muut lauseenjäsenet
		@match(VP_RULES, {"lemma": en_verb, "modifiers": [
			{"_del": True, "_name": "subj", "arc": "nsubj"},
			obj_pattern
		]})
		def _(tree, params, rest):
			val = translateRestToArray(tree)
			subj = translate(params["subj"], NP_RULES)
			obj = translate(params["obj"], obj_rules)
			return FiVP(verb=FiP(verb, {"verbi"}), subj=subj, obj=obj, objcase=object_case, subtrees=val)
		
		def addObjectless1():
			@match(VP_RULES, {"lemma": en_verb, "modifiers": [
				{"_del": True, "_name": "subj", "arc": "nsubj"}
			]})
			def _(tree, params, rest):
				val = translateRestToArray(tree)
				subj = translate(params["subj"], NP_RULES)
				return FiVP(verb=FiP(verb, {"verbi"}), subj=subj, subtrees=val)
		
		@match(VP_RULES, {"lemma": en_verb, "modifiers": [
			obj_pattern
		]})
		def _(tree, params, rest):
			val = translateRestToArray(tree)
			obj = translate(params["obj"], obj_rules)
			return FiVP(verb=FiP(verb, {"verbi"}), obj=obj, objcase=object_case, subtrees=val)
		
		def addObjectless2():
			@match(VP_RULES, {"lemma": en_verb, "modifiers": prepositions})
			def _(tree, params, rest):
				val = translateRestToArray(tree)
				return FiVP(verb=FiP(verb, {"verbi"}), subtrees=val)
		
		global queue
		queue += [addObjectless1, addObjectless2]
	
	for obj_pattern, obj_rules in [(obj_pattern_noun, NP_RULES), (obj_pattern_adj, AP_RULES)]:
		addObjVerbRule(obj_pattern, obj_rules)

queue = []

# TODO suomen postpositiot
addVerb("be", "olla", "nominatiivi")
addVerb("do", "tehdä", "akkusatiivi")
addVerb("say", "sanoa", "akkusatiivi")
addVerb("get", "saada", "akkusatiivi")
addVerb("make", "tehdä", "akkusatiivi")
addVerb("go", "mennä", "illatiivi")
addVerb("go to", "mennä", "illatiivi")
addVerb("know", "tietää", "akkusatiivi")
addVerb("take", "ottaa", "akkusatiivi")
addVerb("see", "nähdä", "akkusatiivi")
addVerb("come", "tulla", "illatiivi")
addVerb("come to", "tulla", "illatiivi")
addVerb("come from", "tulla", "elatiivi")
addVerb("think", "ajatella", "partitiivi")
addVerb("look", "katsoa", "partitiivi")
addVerb("look at", "katsoa", "partitiivi")
addVerb("want", "haluta", "partitiivi")
addVerb("give", "antaa", "akkusatiivi")
addVerb("use", "käyttää", "partitiivi")
addVerb("find", "löytää", "akkusatiivi")
addVerb("tell", "kertoa", "partitiivi")
addVerb("ask", "kysyä", "akkusatiivi")
addVerb("ask for", "pyytää", "partitiivi")
addVerb("work", "tehdä", "partitiivi")
addVerb("work for", "työskennellä", "allatiivi")
addVerb("work as", "toimia", "essiivi")
addVerb("seem", "vaikuttaa", "ablatiivi")
addVerb("feel", "tuntua", "ablatiivi")
addVerb("try", "kokeilla", "partitiivi")
addVerb("leave", "jättää", "akkusatiivi")
addVerb("call", "kutsua", "partitiivi")
addVerb("call for", "kutsua", "partitiivi")

addVerb("have", "omata", "akkusatiivi")
addVerb("love", "rakastaa", "partitiivi")
addVerb("like", "pitää", "elatiivi")
addVerb("eat", "syödä", "partitiivi")
addVerb("read", "lukea", "partitiivi")
addVerb("write", "kirjoittaa", "partitiivi")
addVerb("move on", "liikkua", "adessiivi")
addVerb("move to", "liikkua", "illatiivi")
addVerb("walk", "kävellä", "illatiivi")
addVerb("walk to", "kävellä", "illatiivi")
addVerb("walk at", "kävellä", "adessiivi")
addVerb("notice", "huomata", "partitiivi")
addVerb("bother", "häiritä", "partitiivi")
addVerb("become", "tulla", "translatiivi")
addVerb("compare", "verrata", "partitiivi")

# suorita verbien objektittomien muotojen lisääminen (jotka pitää lisätä aina kaikkien muiden muotojen jälkeen)
for task in queue:
	task()

def addSubtreeRule(rules):
	@match(rules, {"modifiers": [{"_del": True, "_name": "subtree"}]})
	def _(tree, params, rest):
		val = translate(tree, rest)
		val2 = translate(params["subtree"], rules)
		return FiA([val, val2], val.flags)

addSubtreeRule(NP_RULES)
addSubtreeRule(PHRASE_RULES)

def pronounToNoun(en_noun, fi_noun, flags=None):
	@match(NP_RULES, {"word": en_noun, "POS_coarse": {"PRON", "DET", "ADJ", "NOUN"}})
	def _(tree, params, rest):
		val = translateRest(tree)
		noun = FiP(fi_noun, flags=flags or set())
		return FiA([noun, val], noun.flags)

def nounToNoun(en_noun, fi_noun):
	@match(NP_RULES, {"lemma": en_noun, "POS_fine": "NN"})
	def _(tree, params, rest):
		val = translateRest(tree)
		noun = FiP(fi_noun)
		return FiA([noun, val], noun.flags)
	@match(NP_RULES, {"lemma": en_noun, "POS_fine": "NNS"})
	def _(tree, params, rest):
		val = translateRest(tree)
		noun = FiP(fi_noun, {"monikko"})
		return FiA([noun, val], noun.flags)

pronounToNoun("i", "minä", {"minä"})
pronounToNoun("me", "minä")
pronounToNoun("mine", "minä", {"genetiivi"})
pronounToNoun("myself", "itse", {"minun"})
pronounToNoun("you", "sinä", {"sinä"})
pronounToNoun("yours", "sinä", {"genetiivi"})
pronounToNoun("yourself", "itse", {"sinun"})
pronounToNoun("yourselves", "itse", {"teidän"})
pronounToNoun("we", "me", {"me"})
pronounToNoun("us", "me")
pronounToNoun("ours", "me", {"genetiivi"})
pronounToNoun("ourselves", "itse", {"meidän"})
pronounToNoun("who", "kuka")

pronounToNoun("it", "se")
pronounToNoun("its", "se", {"genetiivi"})
pronounToNoun("itself", "itse")
pronounToNoun("this", "tämä")
pronounToNoun("that", "tuo")
pronounToNoun("these", "nämä")
pronounToNoun("those", "nuo")
pronounToNoun("she", "hän")
pronounToNoun("hers", "hän", {"genetiivi"})
pronounToNoun("herself", "itse", {"hänen"})
pronounToNoun("he", "hän")
pronounToNoun("his", "hän", {"genetiivi"})
pronounToNoun("him", "hän", {"partitiivi"})
pronounToNoun("himself", "itse", {"hänen"})
pronounToNoun("they", "ne")
pronounToNoun("theirs", "ne", {"genetiivi"})
pronounToNoun("them", "ne", {"partitiivi"})
pronounToNoun("themselves", "itse", {"hänen"})

nounToNoun("something", "jokin")
nounToNoun("everybody", "jokainen")
nounToNoun("everyone", "jokainen")

nounToNoun("cat", "kissa")
nounToNoun("time", "aika")
nounToNoun("person", "henkilö")
nounToNoun("year", "vuosi")
nounToNoun("way", "polku")
nounToNoun("day", "päivä")
nounToNoun("thing", "asia")
nounToNoun("man", "mies")
nounToNoun("world", "maailma")
nounToNoun("life", "elämä")
nounToNoun("hand", "käsi")
nounToNoun("part", "osa")
nounToNoun("child", "lapsi")
nounToNoun("eye", "silmä")
nounToNoun("woman", "nainen")
nounToNoun("place", "paikka")
nounToNoun("work", "työ")
nounToNoun("week", "viikko")
nounToNoun("case", "tapaus")
nounToNoun("point", "piste")
nounToNoun("government", "hallitus")
nounToNoun("company", "seura")
nounToNoun("number", "luku")
nounToNoun("group", "joukko")
nounToNoun("problem", "ongelma")
nounToNoun("fact", "tosiasia")

trans = {
	"fi:n": {},
	"fi:a": {},
	"fi:v": {},
	"fi:r": {}
}
with open("fiwn.txt", "r") as file:
	for i, line in enumerate(file):
		print("\r"+str(i), end="")
		ficode, filemma, encode, enlemma, rel = line.strip().split("\t")[:5]
		if rel == "synonym" and ficode[:4] in trans:
			if enlemma not in trans[ficode[:4]]:
				trans[ficode[:4]][enlemma] = []
			
			trans[ficode[:4]][enlemma].append(FiA([FiP(word) for word in filemma.split(" ")]))

print()

def randomTranslation(word, pos):
	translations = trans[pos][word]
	return random.choice(translations)

@match(AP_RULES, {"lemma": trans["fi:a"], "POS_coarse": "ADJ"})
def _(tree, params, rest):
	val = translateRest(tree)
	return FiA([randomTranslation(tree["lemma"], "fi:a"), val])

@match(ADVP_RULES, {"lemma": trans["fi:r"]})
def _(tree, params, rest):
	val = translateRest(tree)
	return FiA([randomTranslation(tree["lemma"], "fi:r"), val])

@match(NP_RULES, {"lemma": trans["fi:n"], "POS_fine": "NN"})
def _(tree, params, rest):
	val = translateRest(tree)
	return FiA([randomTranslation(tree["lemma"], "fi:n"), val])
@match(NP_RULES, {"lemma": trans["fi:n"], "POS_fine": "NNS"})
def _(tree, params, rest):
	val = translateRest(tree)
	return FiA([randomTranslation(tree["lemma"], "fi:n"), val], {"monikko"})

@match(VP_RULES, {"lemma": trans["fi:v"], "POS_coarse": "VERB", "modifiers": [
	{"_name": "subj", "_del": True, "arc": "nsubj"},
	{"_name": "obj", "_del": True, "arc": "dobj"}
]})
def _(tree, params, rest):
	val = translateRestToArray(tree)
	subj = translate(params["subj"], NP_RULES)
	obj = translate(params["obj"], NP_RULES)
	return FiVP(randomTranslation(tree["lemma"], "fi:v"), subj=subj, obj=obj, subtrees=val)

@match(VP_RULES, {"lemma": trans["fi:v"], "POS_coarse": "VERB", "modifiers": [
	{"_name": "subj", "_del": True, "arc": "nsubj"}
]})
def _(tree, params, rest):
	val = translateRestToArray(tree)
	subj = translate(params["subj"], NP_RULES)
	return FiVP(randomTranslation(tree["lemma"], "fi:v"), subj=subj, subtrees=val)

@match(VP_RULES, {"lemma": trans["fi:v"], "POS_coarse": "VERB", "modifiers": [
	{"_name": "obj", "_del": True, "arc": "dobj"}
]})
def _(tree, params, rest):
	val = translateRestToArray(tree)
	obj = translate(params["obj"], NP_RULES)
	return FiVP(randomTranslation(tree["lemma"], "fi:v"), obj=obj, subtrees=val)

@match(VP_RULES, {"lemma": trans["fi:v"], "POS_coarse": "VERB"})
def _(tree, params, rest):
	val = translateRestToArray(tree)
	return FiVP(randomTranslation(tree["lemma"], "fi:v"), subtrees=val)

@match(NP_RULES, {"POS_fine": "NNP"})
def _(tree, params, rest):
	val = translateRest(tree)
	noun = FiP(tree["lemma"])
	return FiA([noun, val], noun.flags)
@match(NP_RULES, {"POS_fine": "NNPS"})
def _(tree, params, rest):
	val = translateRest(tree)
	noun = FiP(tree["lemma"], {"monikko"})
	return FiA([noun, val], noun.flags)

@match(PHRASE_RULES, {})
def _(tree, params, rest):
	return FiNI(translateUnknownPos(tree).inflect(set()))

def main():
	parser = argparse.ArgumentParser()
	parser.add_argument("text", nargs="?", type=str, help="the text to process")
	args = parser.parse_args()
	
	if args.text:
		doc = nlp(args.text)
		
		tree = doc.print_tree()[0]
		print(json.dumps(doc.print_tree(), indent=2))
		print(translate(tree, NP_RULES).inflect({"nominatiivi"}))
		print(translate(tree, PHRASE_RULES).inflect({"nominatiivi"}))
	else:
		while True:
			try:
				line = input("hau> ").lower()
				doc = nlp(line)
				tree = doc.print_tree()[0]
				print(json.dumps(doc.print_tree(), indent=2))
				#print(translate(tree, NP_RULES).inflect({"nominatiivi"}))
				if tree["POS_coarse"] == "VERB":
					print(translate(tree, PHRASE_RULES).inflect({"nominatiivi"}))
				else:
					print(translateUnknownPos(tree).inflect(set()))
			except EOFError:
				break
		print("exit")

if __name__ == "__main__":
	main()
