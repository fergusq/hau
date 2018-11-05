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
