import copy
import random

from fitree import FiP, FiVP, FiA, niTree, FiNI, FiIf, flagsFiIf, fixAccusative, FiPossSuffixFromSubject
import settings

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

def treeToCode(tree):
	ans = ""
	if "_del" in tree:
		ans += "*"
	
	for code, pre, post in [("_name", "", "@"), ("arc", "", "="), ("POS_coarse", "", ""), ("POS_fine", ".", ""), ("word", ":", ""), ("lemma", "/", "")]:
		if code in tree and tree[code]:
			ans += pre
			if isinstance(tree[code], set) or isinstance(tree[code], dict):
				if len(tree[code]) > 5:
					ans += "<set of " + str(len(tree[code])) + " items>"
				else:
					ans += repr(tree[code])
			elif not isinstance(tree[code], str):
				ans += repr(tree[code])
			else:
				ans += tree[code]
			
			ans += post
	
	if "modifiers" in tree:
		ans += "[" + ", ".join(treeToCode(subtree) for subtree in tree["modifiers"]) + "]"
	
	if "_not" in tree:
		ans += " && not " + treeToCode(tree["_not"])
	
	return ans

translate_indent = 0

def translate(tree, rules):
	global translate_indent
	translate_indent += 1
	if settings.debug_mode:
		print(translate_indent*" " + "translate(" + tree["word"] + ", ", end="")
		if rules == AP_RULES:
			print("AP_RULES)", end="")
		elif rules == ADVP_RULES:
			print("ADVP_RULES)", end="")
		elif rules == NP_RULES:
			print("NP_RULES)", end="")
		elif rules == VP_RULES:
			print("VP_RULES)", end="")
		elif rules == PP_RULES:
			print("PP_RULES)", end="")
		elif rules == PHRASE_RULES:
			print("PHRASE_RULES)", end="")
		else:
			print("<rules#"+str(len(rules))+">)", end="")
	
	for i, rule in enumerate(rules):
		ctree = copy.deepcopy(tree)
		m, d = rule.matches(ctree)
		if m:
			if settings.debug_mode:
				print(" -> match " + treeToCode(rule.pattern))
			
			ans = rule.translate(ctree, d, rules[i:])
			translate_indent -= 1
			return ans
	
	if settings.debug_mode:
		print(" -> no match")
	
	translate_indent -= 1
	return FiNI("[" + treeToStr(tree) + "]")

def translateRest(tree):
	ts = []
	for subtree in tree["modifiers"]:
		ts.append(translateUnknownPos(subtree))
	return FiA(ts, no_forwarding=True)

def translateRestToArray(tree):
	ts = []
	for subtree in tree["modifiers"]:
		ts.append(translateUnknownPos(subtree))
	return ts

def translateUnknownPos(tree):
	if settings.debug_mode:
		print(" "*translate_indent + "translateUnknownPos("+tree["POS_coarse"]+":"+tree["word"] + "): ", end="")
	
	if tree["POS_coarse"] == "ADJ":
		return translate(tree, AP_RULES)
	elif tree["POS_coarse"] == "ADV" or tree["arc"] == "npadvmod":
		return translate(tree, ADVP_RULES)
	elif tree["POS_coarse"] == "NOUN":
		return translate(tree, NP_RULES)
	elif tree["POS_coarse"] == "NUM":
		return translate(tree, NP_RULES)
	elif tree["POS_coarse"] == "VERB":
		return translate(tree, VP_RULES)
	elif tree["POS_coarse"] == "ADP":
		return translate(tree, PP_RULES)
	elif tree["POS_coarse"] == "PUNCT":
		if tree["word"] in ['"', "'", ":", "!", "?"]:
			return FiNI(tree["word"])
		else:
			return FiNI("")
	else:
		return FiNI("[" + treeToStr(tree) + "]")

AP_RULES = []
NP_RULES = []
VP_RULES = []
PP_RULES = []
ADVP_RULES = []
PHRASE_RULES = []

@match(PHRASE_RULES, {"modifiers": [{"_del": True, "_name": "subtree", "arc": "ccomp", "modifiers": [{"_del": True, "lemma": "that", "arc": "mark"}]}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	val2 = translate(params["subtree"], PHRASE_RULES)
	return FiA([val, FiNI(","), FiNI("että"), niTree(val2)], val.flags)

@match(PHRASE_RULES, {"modifiers": [{"_del": True, "_name": "subtree", "arc": "ccomp"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	val2 = translate(params["subtree"], PHRASE_RULES)
	return FiA([val, FiNI(","), FiNI("että"), niTree(val2)], val.flags)

@match(VP_RULES, {"modifiers": [{"_del": True, "_name": "subtree", "arc": "ccomp"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	val2 = translate(params["subtree"], PHRASE_RULES)
	return FiA([val, FiNI(","), FiNI("että"), niTree(val2)], val.flags)

def addConjunction(en_conj, fi_conj):
	@match(PHRASE_RULES, {"modifiers": [{"_del": True, "_name": "subtree", "arc": "advcl", "modifiers": [{"_del": True, "word": en_conj, "POS_fine": "IN"}]}]})
	def _(tree, params, rest):
		val = translate(tree, rest)
		val2 = translate(params["subtree"], PHRASE_RULES)
		return FiA([val, FiNI(","), FiNI(fi_conj), niTree(val2), FiNI(",")], val.flags)

addConjunction("if", "jos")
addConjunction("unless", "paitsi jos")
addConjunction("while", "kun")
addConjunction("until", "kunnes")
addConjunction("for", "sillä")

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

addCConjunction(AP_RULES, NP_RULES, "and", "ja")
addCConjunction(AP_RULES, NP_RULES, "but", "mutta")
addCConjunction(AP_RULES, NP_RULES, "or", "tai")

addCConjunction(AP_RULES, AP_RULES, "and", "ja")
addCConjunction(AP_RULES, AP_RULES, "but", "mutta")
addCConjunction(AP_RULES, AP_RULES, "or", "tai")

addCConjunction(VP_RULES, VP_RULES, "and", "ja")
addCConjunction(VP_RULES, VP_RULES, "but", "vaan")
addCConjunction(VP_RULES, VP_RULES, "or", "tai")

# tunnistamattomat konjunktiot
@match(PHRASE_RULES, {"modifiers": [{"_del": True, "_name": "subtree", "arc": "conj", "POS_coarse": "VERB"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	val2 = translate(params["subtree"], PHRASE_RULES)
	return FiA([val, FiNI(","), niTree(val2)], val.flags)

# relatiivilauseet
@match(NP_RULES, {"modifiers": [{"_del": True, "_name": "relcl", "arc": "relcl", "POS_coarse": "VERB"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	val2 = translate(params["relcl"], PHRASE_RULES)
	return FiA([val, FiNI(","), niTree(val2), FiNI(",")], val.flags)

def addDeterminer(en_determiner, fi_determiner, det_flags=None, flags=None, pos="DT"):
	@match(NP_RULES, {"modifiers": [{"_del": True, "word": en_determiner, "POS_fine": pos}]})
	def _(tree, params, rest):
		val = translate(tree, rest)
		if fi_determiner:
			return FiA([FiP(fi_determiner, det_flags or set()), val], val.flags|(flags or set()))
		elif flags:
			return FiA([val], val.flags|flags)
		else:
			return val

# artikkelit
addDeterminer({"a", "an"}, "eräs", flags={"epämääräinen"})
#addDeterminer("the", "se")
addDeterminer("the", "")
addDeterminer("this", "tämä")
addDeterminer("that", "tuo")
addDeterminer("these", "nämä", {"monikko"})
addDeterminer("those", "nuo", {"monikko"})
addDeterminer("all", "kaikki", pos="PDT")
addDeterminer("every", "jokainen")

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

@match(NP_RULES, {"modifiers": [{"_del": True, "_name": "proper", "POS_coarse": "PROPN", "arc": "amod"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	val2 = translate(params["proper"], NP_RULES)
	return FiA([niTree(val2, {"genetiivi"}), val], val.flags)

def prepositionToCase(preposition, case, verb=None, fi_preposition=None, fi_postposition=None, after=False, pp_rule=True, np_rule=True, ap_rule=True, phrase_rule=True):
	fi_pre_val = FiNI(fi_preposition or "")
	fi_post_val = FiNI(fi_postposition or "")
	if pp_rule:
		@match(PP_RULES, {"word": preposition, "POS_fine": "IN", "modifiers": [{"_name": "subtree", "_del": True, "POS_coarse": {"NOUN", "PROPN", "PRON"}}]})
		def _(tree, params, rest):
			val = translateRest(tree)
			val2 = translate(params["subtree"], NP_RULES)
			pp = niTree(val2, {case})
			if after:
				return FiA([val, fi_pre_val, pp, fi_post_val])
			else:
				return FiA([fi_pre_val, pp, fi_post_val, val])
		
		@match(PP_RULES, {"word": preposition, "POS_fine": "IN", "modifiers": [{"_name": "subtree", "_del": True, "POS_coarse": {"VERB"}}]})
		def _(tree, params, rest):
			val = translateRest(tree)
			val2 = translate(params["subtree"], VP_RULES)
			pp = niTree(val2, {case, "-minen"})
			if after:
				return FiA([val, fi_pre_val, pp, fi_post_val])
			else:
				return FiA([fi_pre_val, pp, fi_post_val, val])
	
	def addRuleTo(target):
		@match(target, {"modifiers": [{"_del": True, "word": preposition, "POS_fine": "IN", "modifiers": [{"_name": "subtree", "POS_coarse": "NOUN"}]}]})
		def _(tree, params, rest):
			val = translate(tree, rest)
			val2 = translate(params["subtree"], NP_RULES)
			pp = niTree(val2, {case})
			verbp = FiP(verb, {"verbi", "-va"}) if verb else FiNI("")
			if after:
				return FiA([val, verbp, fi_pre_val, pp, fi_post_val], val.flags)
			return FiA([fi_pre_val, pp, fi_post_val, verbp, val], val.flags)
	
	if np_rule:
		addRuleTo(NP_RULES)
	
	if ap_rule:
		addRuleTo(AP_RULES)
	
	if phrase_rule:
		@match(PHRASE_RULES, {"word": "was", "POS_fine": "VBD", "modifiers": [
			{"lemma": "there", "_del": True, "POS_fine": "EX"},
			{"_name": "subj", "_del": True, "modifiers": [
				{"_del": True, "word": preposition, "POS_fine": "IN", "modifiers": [{"_name": "subtree", "POS_coarse": "NOUN"}]}
			]}
		]})
		def _(tree, params, rest):
			val = translateRest(tree)
			subj = translate(params["subj"], NP_RULES)
			place = translate(params["subtree"], NP_RULES)
			verbp = niTree(FiP(verb or "olla"), {"verbi", "preteriti", "yks_3"})
			return FiA([niTree(subj, {"nominatiivi"}), verbp, fi_pre_val, niTree(place, {case}), fi_post_val, val])
	
	#@match(PHRASE_RULES, {"modifiers": [{"_del": True, "word": preposition, "POS_fine": "IN", "modifiers": [{"_name": "subtree", "POS_coarse": "NOUN"}]}]})
	#def _(tree, params, rest):
	#	val = translate(tree, rest)
	#	val2 = translate(params["subtree"], NP_RULES)
	#	return FiA([val, niTree(val2, {case})], val.flags)

prepositionToCase("of", "genetiivi", pp_rule=False, ap_rule=False)
prepositionToCase("of", "elatiivi", np_rule=False, phrase_rule=False)
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
prepositionToCase("like", "nominatiivi", fi_preposition="kuten") # TODO voisiko nominatiivin sijasta taipua samoin kuin pääsanansa?
prepositionToCase("unlike", "nominatiivi", fi_preposition="toisin kuten")
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
addPossPronoun("its", "sen", set())
addPossPronoun("our", "meidän", {"meidän"})
addPossPronoun("their", "niiden", set())

# the person's thing -> henkilön asia
@match(NP_RULES, {"modifiers": [{"_del": True, "_name": "ap", "POS_coarse": "NOUN", "arc": "poss", "modifiers": [{"_del": True, "word": "'s", "POS_fine": "POS"}]}]})
def _(tree, params, rest):
	ap = params["ap"]
	val = translate(tree, rest)
	val2 = translate(ap, NP_RULES)
	return FiA([niTree(val2, {"genetiivi"}), val], val.flags)

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

# two things -> kaksi asiaa
@match(NP_RULES, {"modifiers": [{"_del": True, "_name": "np", "arc": "nummod"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	val2 = translate(params["np"], NP_RULES)
	
	flags = set(val.flags)
	if "monikko" in val2.flags:
		pass
		# TODO: lippujen poistaminen alemmilta tasoilta
		# käytä "heikko partitiivi" -lippua
	
	return FiA([FiA([val2], {"ei omistusliitettä"}), val], flags)

# very nice -> hyvin kiva
@match(AP_RULES, {"modifiers": [{"_del": True, "_name": "ap", "POS_coarse": {"ADV", "ADP"}, "arc": "advmod"}]})
def _(tree, params, rest):
	ap = params["ap"]
	val = translate(tree, rest)
	val2 = translate(ap, ADVP_RULES)
	return FiA([niTree(val2), val])

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
	return FiA([val, niTree(val2)])

# the thing doing -> tekevä asia
@match(NP_RULES, {"modifiers": [{"_del": True, "_name": "ap", "POS_fine": "VBG", "arc": "acl"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	val2 = translate(params["ap"], VP_RULES)
	return FiA([FiA([val2], {"-va"}), val])

# the thing done -> tehty asia
@match(NP_RULES, {"modifiers": [{"_del": True, "_name": "ap", "POS_fine": "VBD", "arc": "acl"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	val2 = translate(params["ap"], VP_RULES)
	return FiA([FiA([val2], {"-tu"}), val])

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
	return FiA([niTree(val2, {"-minen", "illatiivi"}), FiP("kykenevä"), val])

def addAdjective(en_adj, fi_adj, flags=None):
	# TODO vertailumuodot
	@match(AP_RULES, {"lemma": en_adj, "POS_coarse": "ADJ"})
	def _(tree, params, rest):
		val = translateRest(tree)
		return FiA([FiP(fi_adj, {"adjektiivi"}|(flags or set())), val])

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
addAdjective("most", "usea", {"superlatiivi"})

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
	return FiA([val, niTree(val2, {"-a"})], val.flags)

# is done -> tehdään
@match(VP_RULES, {"POS_fine": "VBN", "modifiers": [{"_del": True, "arc": "auxpass"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	return FiA([val], {"passiivi", "pe4"})

# is doing -> [poistetaan apuverbi]
@match(VP_RULES, {"modifiers": [{"_del": True, "lemma": "be", "arc": "aux"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	return FiA([val], set())

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
	return FiVP(verb=FiA([FiP("olla"), niTree(val2, {"-tava"}), val]), subj=subj, flags={"nollapersoona"})

# have done -> olla tehnyt
def addAuxVerb(verb, fi_verb, verbflags, compflags, phraseflags=set(), compgen=None):
	@match(VP_RULES, {"modifiers": [{"_del": True, "lemma": verb, "arc": "aux"}]})
	def _(tree, params, rest):
		val = translate(tree, rest)
		if isinstance(val, FiVP):
			valverb = val.verb
			val.verb = FiA([FiP(fi_verb, verbflags), compgen(valverb) if compgen else niTree(valverb, compflags)])
			val.flags |= phraseflags
			return val
		else:
			return FiA([FiP(fi_verb, verbflags), compgen(val) if compgen else niTree(val, compflags)])

addAuxVerb("must", "täytyä", set(), {"-a"}, {"nollapersoona"})
addAuxVerb({"can", "ca"}, "voida", set(), {"-a"})
addAuxVerb("could", "voida", {"konditionaali"}, {"-a"})
addAuxVerb("may", "saattaa", set(), {"-a"})
addAuxVerb("might", "saattaa", {"konditionaali"}, {"-a"})
addAuxVerb("shall", "saada", set(), {"-a"})
addAuxVerb("should", "pitää", {"konditionaali"}, {"-a"}, {"nollapersoona"})
addAuxVerb("will", "tulla", set(), {"-ma", "illatiivi"})
addAuxVerb({"have", "'ve"}, "olla", {"perfektin apuverbi"}, set(), compgen=lambda val: flagsFiIf(val, {"passiivi"}, {"-tu"}, {"-nut"}))

# have to do -> olla tehtävä
@match(VP_RULES, {"lemma": {"have", "'ve"}, "modifiers": [{"_del": True, "_name": "subtree", "POS_fine": "VB", "modifiers": [{"_del": True, "word": "to", "POS_fine": "TO"}]}]})
def _(tree, params, rest):
	val = translateRest(tree)
	val2 = translate(params["subtree"], VP_RULES)
	return FiA([FiP("olla"), niTree(val2, {"-tava"}), val])

# to do, doing
@match(VP_RULES, {"modifiers": [{"_del": True, "_name": "subtree", "arc": "advcl", "POS_fine": "VBG"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	val2 = translate(params["subtree"], VP_RULES)
	val2 = FiPossSuffixFromSubject(FiA([val2], {"verbi", "-e", "inessiivi"}), sibling_flags=val.flags, no_forwarding=True)
	return FiA([val, val2], val.flags)

# to do, having done
@match(VP_RULES, {"modifiers": [{"_del": True, "_name": "subtree", "arc": "advcl", "POS_fine": "VBN", "modifiers": [{"_del": True, "word": "having", "arc": "aux"}]}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	val2 = translate(params["subtree"], VP_RULES)
	val2 = FiPossSuffixFromSubject(FiA([val2], {"verbi", "-tu", "partitiivi"}), sibling_flags=val.flags, no_forwarding=True)
	return FiA([val, val2], val.flags)

# do to do -> tehdä tehdäkseen
@match(VP_RULES, {"modifiers": [{"_del": True, "_name": "subtree", "POS_fine": "VB", "modifiers": [{"_del": True, "word": "to", "POS_fine": "TO"}]}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	val2 = translate(params["subtree"], VP_RULES)
	val2 = FiPossSuffixFromSubject(FiA([val2], {"verbi", "-a", "translatiivi"}), sibling_flags=val.flags, no_forwarding=True)
	return FiA([val, val2], val.flags)

# do in order to -> tehdä tehdäkseen
@match(VP_RULES, {"modifiers": [
	{"_del": True, "POS_fine": "IN", "modifiers": [
		{"POS_fine": "NN", "word": "order", "modifiers": [
			{"_del": True, "_name": "subtree", "POS_fine": "VB", "modifiers": [
				{"_del": True, "word": "to", "POS_fine": "TO"}
			]}
		]}
	]}
]})
def _(tree, params, rest):
	val = translate(tree, rest)
	val2 = translate(params["subtree"], VP_RULES)
	val2 = FiPossSuffixFromSubject(FiA([val2], {"verbi", "-a", "translatiivi"}), sibling_flags=val.flags, no_forwarding=True)
	return FiA([val, val2], val.flags)

@match(PHRASE_RULES, {"POS_fine": "VBP"})
def _(tree, params, rest):
	val = translate(tree, VP_RULES)
	return niTree(val, {"verbi", "preesens", "finiitti"})

@match(PHRASE_RULES, {"POS_fine": "VBZ"})
def _(tree, params, rest):
	val = translate(tree, VP_RULES)
	return niTree(val, {"verbi", "preesens", "finiitti"})

@match(PHRASE_RULES, {"POS_fine": "VBD"})
def _(tree, params, rest):
	val = translate(tree, VP_RULES)
	return niTree(val, {"verbi", "preteriti", "finiitti"})

# relatiivilauseet voivat olla ing-muodossa, esim. "i don't mind the cat doing it"
@match(PHRASE_RULES, {"POS_fine": "VBG"})
def _(tree, params, rest):
	val = translate(tree, VP_RULES)
	return niTree(val, {"verbi", "preesens", "finiitti"})

# apuverbin aikamuoto
@match(PHRASE_RULES, {"POS_fine": {"VBN", "VBG", "VB"}, "modifiers": [{"POS_fine": {"VBZ", "VBP", "MD"}}]})
def _(tree, params, rest):
	val = translate(tree, VP_RULES)
	return niTree(val, {"verbi", "pe4" if "passiivi" in val.flags else "finiitti"})

@match(PHRASE_RULES, {"POS_fine": {"VBN", "VBG", "VB"}, "modifiers": [{"POS_fine": {"VBD"}}]})
def _(tree, params, rest):
	val = translate(tree, VP_RULES)
	return niTree(val, {"verbi", "preteriti", "pe4" if "passiivi" in val.flags else "finiitti"})

@match(PHRASE_RULES, {"POS_fine": "VB", "_not": {"modifiers": [{"POS_fine": {"VBD", "VBZ", "VBP", "MD"}}]}})
def _(tree, params, rest):
	val = translate(tree, VP_RULES)
	return niTree(val, {"verbi", "imperatiivi", "yks_2", "subjekti yks_2"})

# by something -> jonkin toimesta
@match(PP_RULES, {"word": "by", "arc": "agent", "modifiers": [{"_name": "subtree", "_del": True, "POS_coarse": "NOUN"}]})
def _(tree, params, rest):
	val = translateRest(tree)
	val2 = translate(params["subtree"], NP_RULES)
	return FiA([niTree(val2, {"genetiivi"}), FiNI("toimesta"), val])

# as/like something -> kuten jokin
@match(PP_RULES, {"word": {"as", "like"}, "POS_fine": "IN", "modifiers": [{"_name": "subtree", "_del": True}]})
def _(tree, params, rest):
	val = translateRest(tree)
	val2 = translateUnknownPos(params["subtree"])
	return FiA([val, FiNI("kuten"), val2])

# unlike something -> kuten jokin
@match(PP_RULES, {"word": "as", "POS_fine": "IN", "modifiers": [{"_name": "subtree", "_del": True}]})
def _(tree, params, rest):
	val = translateRest(tree)
	val2 = translateUnknownPos(params["subtree"])
	return FiA([val, FiNI("toisin kuten"), val2])

@match(AP_RULES, {"modifiers": [{"_del": True, "word": "as", "POS_fine": "IN", "modifiers": [{"_name": "subtree", "POS_coarse": "NOUN"}]}]})
def _(tree, params, rest):
	val = translateRest(tree)
	val2 = translate(params["subtree"], NP_RULES)
	return FiA([val, FiNI("kuin"), niTree(val2, {"nominatiivi"})], val.flags)

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
	
	@match(PHRASE_RULES, {"word": "was", "POS_fine": "VBD", "modifiers": [
		{"lemma": "there", "_del": True, "POS_fine": "EX"},
		{"_name": "subj", "_del": True, "modifiers": [{"word": en_verb, "POS_fine": "VBG", "modifiers": [obj_pattern_noun]}]}
	]})
	def _(tree, params, rest):
		val = translateRest(tree)
		subj = translate(params["subj"], NP_RULES)
		place = translate(params["obj"], NP_RULES)
		case = handleAccusative(place)
		return FiA([niTree(subj, {"nominatiivi"}), niTree(FiP(verb, {"verbi"}), {"preteriti", "yks_3"}), niTree(place, {case}), val])
	
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
addVerb("compare to", "verrata", "partitiivi")
addVerb("remind", "muistuttaa", "partitiivi")

# suorita verbien objektittomien muotojen lisääminen (jotka pitää lisätä aina kaikkien muiden muotojen jälkeen)
for task in queue:
	task()

# yhdyssanat
@match(NP_RULES, {"modifiers": [{"_del": True, "_name": "prefix", "POS_coarse": "NOUN", "arc": "compound"}]})
def _(tree, params, rest):
	val = translate(tree, rest)
	val2 = translate(params["prefix"], NP_RULES)
	return FiA([niTree(val2, {"nominatiivi"}), val], val.flags, compound=True)

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
		pos = ficode[:4]
		if rel == "synonym" and pos in trans:
			if enlemma not in trans[pos]:
				trans[pos][enlemma] = []
			
			words = [FiP(word) for word in filemma.split(" ")]
			
			if pos == "fi:v":
				trans[pos][enlemma].append(FiVP(verb=words[0], subtrees=words[1:]))
			else:
				trans[pos][enlemma].append(FiA(words))

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

# night -> yönä (FIXME purkkaa)
@match(ADVP_RULES, {"POS_coarse": "NOUN"})
def _(tree, params, rest):
	val = translate(tree, NP_RULES)
	return FiA([val], {"essiivi"})

@match(NP_RULES, {"lemma": trans["fi:n"], "POS_fine": {"NN", "CD"}})
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
	return niTree(translateUnknownPos(tree), set())
