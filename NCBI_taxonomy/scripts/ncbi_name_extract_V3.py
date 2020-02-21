import subprocess, sqlite3

from collections import Counter

# connect to ncbi database
# need modify directory

DB = "plnDB20191101.db"
conn = sqlite3.connect(DB)
c = conn.cursor()

#output file name
outfile = open("Spermatophyta_plnDB_02012020.txt", "w")

sqlcmd = "SELECT ncbi_id,parent_ncbi_id,name,node_rank FROM taxonomy WHERE name_class = 'scientific name' OR name_class = 'authority'"
c.execute(sqlcmd)

data = c.fetchall()
#remove duplicats in the list
data = list(set(data))
    #print(data)

# parse output as dictionary for extract target clade
count = 0
# pattern: pid = {"child id"=parentid} or pid["cid"]=parentid
pid = {} # key is the child id (cid) and the value is the parent id

#cid["parentid"]=list(child id)
cid = {} # key is the parent and value is the list of children

#taxon id and name dict
nid = {}

#taxon id and rank dict
nrank = {}

targetid = ""

for i in data:
    tid = str(i[0])
    parentid = str(i[1])
    name = str(i[2])
    rank = str(i[3])
    
    #built dictionary
    nid[tid] = name
    nrank[tid] = rank
    pid[tid] = parentid

    if tid == "58024":
        targetid = tid
        pid[tid] = ""
    if parentid not in cid: 
        cid[parentid] = []
    cid[parentid].append(tid)
    count += 1
    #print(tid+"\t|\t"+parentid+"\t|\t"+name+"\t|\t"+rank+"\n")
stack = [targetid]
while len(stack) > 0:
    tempid = stack.pop()
    outfile.write(tempid+"\t|\t"+pid[tempid]+"\t|\t"+nid[tempid]+"\t|\t"+nrank[tempid]+"\n")
    #print(tempid+"\t|\t"+pid[tempid]+"\t|\t"+nid[tempid]+"\t|\t"+nrank[tempid]+"\t|\t")
    if tempid in cid:
        for j in cid[tempid]:
            stack.append(j)
conn.close()
