#!/usr/bin/env python
import re
import numpy as np 
import sys
import argparse

# passes the model name into python as a variable
parser = argparse.ArgumentParser()
parser.add_argument('--model', action='store', type=str, help="model name")
args=parser.parse_args()

# creates a variable that is the filename
vol = "volumeresults-%s.txt" % (args.model)
lh_thick = "lh_thickresults-%s.txt" % (args.model)
rh_thick = "rh_thickresults-%s.txt" % (args.model)

# find onsets: multi-digit number before EXP_INSTR_KEYBOARD
onset = re.compile(r"ROI:\s+(\w+)", re.MULTILINE)


names = {"vol" : "Volume", "lh_" : "LH Thickness", "rh_": "RH Thickness"}

# open session data file

for data in [vol, lh_thick, rh_thick]:
	with open(data, 'r') as f:
		s = f.read()
	match = onset.findall(s)
	print(names[str(data[0:3])])
	if match == []:
		print("No sig. group effects in any region")
	else:
		for elem in match:
			print elem
	print("---------------")
