#!/usr/bin/python

import sys

fichero=open(sys.argv[1],"r")
fichero2=open(sys.argv[1]+"s","w")
while True:
	t=fichero.read(1)
	if t=="":
		break
	if t=="\012":
		fichero2.write("\015\012")
		continue
	if ord(t)>31:
		fichero2.write(t)
		
fichero.close()
fichero2.close()
