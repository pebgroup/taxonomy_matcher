#! /usr/bin/python

#this script will remove duplicate lines

lines_seen = set() # holds lines already seen
outfile = open("Spermatophyta58024_plnDB02032020_nodupl.csv", "w")
for line in open("Spermatophyta_plnDB_02012020.txt", "r"):
    if line not in lines_seen: # not a duplicate
        outfile.write(line)
        lines_seen.add(line)
outfile.close()
