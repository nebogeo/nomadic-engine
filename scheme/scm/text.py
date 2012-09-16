o = open("out.txt","w")
for line in open("startup.scm"):
	o.write("\""+line[:-1]+"\\n\"\n")
o.close()
