if exist OZ.LST del OZ.LST /F /Q
if exist OZ.OBJ del OZ.OBJ /F /Q
if exist OZ.SYM del OZ.SYM /F /Q
if exist V64_VER1.30I del V64_VER1.30I /F /Q
dasm OZ.asm -v3 -f3 -oOZ.obj -sOZ.sym -lOZ.lst
dos32a makebios OZ.obj V64_VER1.30I
pause
