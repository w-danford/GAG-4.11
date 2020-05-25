@echo off
if "%1" == "" goto error

echo on
D:\APPS\TASM\BIN\tasm /ml /i.\%1 /os gag.asm, %1.obj, ,
D:\APPS\TASM\BIN\tlink /Tdc /c /C /x %1.obj, , , ,
del *.lst
@rem del *.map
del *.xrf
del *.obj

@goto end

:error

echo Command format: mklang lang_name
echo where lang_name is the sub directory, ".\lang_name"
echo where the language specific message and font files,
echo message.msg and font.fnt, are located.
echo gag.asm includes these files.
echo Final output is lang_name.com.

:end

