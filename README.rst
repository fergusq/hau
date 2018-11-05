==============
 Hau-kääntäjä
==============

Summary in English
==================

This program is an English-to-Finnish rule-based machine translator.
It uses the SpaCy library to parse English and Omorfi to generate Finnish words.

Riippuvuudet
============

Hau-kääntäjä käyttää `SpaCy-kirjastoa <spacy_>`_ englannin jäsentämiseen ja `Omorfia <omorfi_>`_ suomen sanojen taivuttamiseen.
Molemmat kirjastot on asennettava ennen käyttöä. Lisäksi SpaCylle on ladattava enlannin kielen malli
(esim. ``en_core_web_sm``, määritelty ``hau.py``-tiedostossa).

.. _spacy: https://github.com/explosion/spaCy
.. _omorfi: https://github.com/flammie/omorfi

Kääntäjä käyttää sanakirjanaan `FinnWordNetin käännössuhdetietokantaa <fiwn_>`_ (``rels/fiwn-transls.tsv``),
joka pitää uudelleennimetä ``fiwn.txt``:ksi ja sijoittaa samaan kansioon Python-tiedostojen kanssa.

.. _fiwn: http://www.ling.helsinki.fi/kieliteknologia/tutkimus/finnwordnet/download_files/fiwn_rels_fi-2.0.zip

Käyttäminen
===========

Suorita ``python3 hau.py``. Ohjelma lataa sanakirjan, minkä jälkeen sille voi syöttää englanninkielisiä lauseita,
jotka se kääntää suomeksi.

Lisenssi
========

GNU GPLv3 (ei välttämättä myöhempi)
