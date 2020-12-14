#-------------------------------------------------------------------------------
# Interpretador para el lenguaje BOT.
# Hecho por: Ricardo Münch. Carnet: 11-10684.
#-------------------------------------------------------------------------------

import yacc
import sys
from LexBot import tokens
from ContBot import *

ast = parse(sys.argv[1])

if ast is not None:
	ast.correr(None, None, None, None, {}, None)