import sqlite3

# connect to ncbi database
# need modify directory

DB = "/data_vol/miao/plnDB20191101/plnDB20191101.db"
conn = sqlite3.connect(DB)
c = conn.cursor()

sqlcmd = "SELECT ncbi_id,parent_ncbi_id,name,node_rank FROM taxonomy WHERE name_class = 'scientific name' OR name_class = 'authority'"
c.execute(sqlcmd)

data = c.fetchall()

conn.close() #moved this up here just to make sure database always gets closed

#remove duplicats in the list
data = list(set(data))
    #print(data)

# parse output as dictionary for extract target clade
# pattern: pid = {"child id"=parentid} or pid["cid"]=parentid
pid = {} # key is the child id (cid) and the value is the parent id

#cid["parentid"]=list(child id)
cid = {} # key is the parent and value is the list of children

#taxon id and name dict
nid = {}

#taxon id and rank dict
nrank = {}

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
        pid[tid] = ""
    if parentid not in cid: 
        cid[parentid] = []
    cid[parentid].append(tid)
    #print(tid+"\t|\t"+parentid+"\t|\t"+name+"\t|\t"+rank+"\n")

stack = ["58024"] #removed unnecessary variable targetid

with open("Spermatophyta_plnDB_02012020.txt", "w") as outfile: #making sure to close file
	while len(stack) > 0:
		tempid = stack.pop()
    	outfile.write(tempid+"\t|\t"+pid[tempid]+"\t|\t"+nid[tempid]+"\t|\t"+nrank[tempid]+"\n")
    	#print(tempid+"\t|\t"+pid[tempid]+"\t|\t"+nid[tempid]+"\t|\t"+nrank[tempid]+"\t|\t")
    	if tempid in cid:
    		stack += cid[tempid] #replaced unnecessary for loop