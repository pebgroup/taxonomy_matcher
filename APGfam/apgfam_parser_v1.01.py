#!/usr/bin/python3
import re
#Open the file that copy-pasted from http://www.mobot.org/MOBOT/research/APweb/
InFileName = "./data/apgweb.txt"
InFile = open(InFileName, 'r')

OutFile = open("apgweb_parsed.csv", 'w')
HeaderLine = "Syn_Fam,Acc_Fam,Clade"
OutFile.write(HeaderLine + "\n")

Acc_Fam = ""
Syn_Fam = ""
Clade = ""
for line in InFile:
	line = line.strip()
	if line != "":
		#split infor
		LINE = re.split(r'\s?=\s', line)
		#remove space and "?"
		LINE = [w.replace(' ?', '').replace('?', '').replace(' ', '_') for w in LINE]
		if len(LINE) == 2:
			Syn_Fam = LINE[0]
			Acc_Fam = LINE[0]
			Clade = LINE[1]
		else:
			Syn_Fam = LINE[0]
			Acc_Fam = LINE[1]
			Clade = LINE[2]
		OutLine=[str(Syn_Fam), str(Acc_Fam ), str(Clade)]
		OutFile.write(",".join(OutLine)+"\n")
InFile.close()
OutFile.close()