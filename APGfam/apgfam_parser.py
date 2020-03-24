#!/usr/local/bin/python3

with open("from_apgweb.txt", "r") as apgweb, open("apgweb_parsed.txt", "w") as parsed:
	for line in apgweb:
		line = line.strip()
		if line != "": 
			LINE = line.split(" = ")
			if len(LINE) == 2:
				LINE = [LINE[0], LINE[0], LINE[1]]
			print(";".join(LINE), file=parsed)
			