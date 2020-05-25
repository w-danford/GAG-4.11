#!/usr/bin/python

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

import dircache

salida=open("haz.bat","w")
salida.write("@echo off\ndel *.com\ndel gag.asm\ndel messages.msg\n\ncopy .\src\gag.asm .\gag.asm\n\n")

directories=dircache.listdir("src/")
directories2=directories[:]
dircache.annotate("src/",directories2)
for elemento in directories2:
	if elemento[-1]=="/":
		elem=elemento[:-1]
		salida.write("copy .\\src\\"+elem+"\\messages.msg .\\\n")
		salida.write("copy .\\src\\"+elem+"\\font.fnt .\\\n")
		salida.write("tasm gag.asm\n")
		salida.write("tlink /t gag.obj\n")
		salida.write("del *.obj\n")
		salida.write("del *.map\n")
		salida.write("del messages.msg\n")
		salida.write("del font.fnt\n")
		salida.write("ren gag.com "+elem+".com\n\n")

salida.write("tasm src\\boot.asm\ntlink /t boot.obj\ntasm src\\install.asm\ntlink /t install.obj\n\n")
salida.write("tasm src\\hdboot.asm\ntlink /t hdboot.obj\n\n")
salida.write("del *.obj\ndel *.map\ndel gag.asm\n\n@echo on\n")
salida.close()
