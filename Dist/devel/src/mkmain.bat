D:\APPS\TASM\BIN\tasm /ml /os boot.asm, boot.obj, ,
D:\APPS\TASM\BIN\tasm /ml /os install.asm, install.obj, ,
D:\APPS\TASM\BIN\tasm /ml /os hdboot.asm, hdboot.obj, ,
D:\APPS\TASM\BIN\tlink /Tdc /c /C /x boot.obj, , , ,
D:\APPS\TASM\BIN\tlink /Tdc /c /C /x install.obj, , , ,
D:\APPS\TASM\BIN\tlink /Tdc /c /C /x hdboot.obj, , , ,
del *.lst
@rem del *.map
del *.xrf
del *.obj
