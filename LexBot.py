#-------------------------------------------------------------------------------
# Analizador léxico para el lenguaje BOT.
# Hecho por: Ricardo Münch. Carnet: 11-10684.
#-------------------------------------------------------------------------------

import lex
import sys

#-------------------------------------------------------------------------------
# En este bloque se especifican los tokens y las acciones a tomar para cada uno.
#-------------------------------------------------------------------------------

claves = {'deactivation' : 'TkDeactivation', 'deactivate' : 'TkDeactivate',
		  'activation' : 'TkActivation', 'activate' : 'TkActivate',
		  'advance' : 'TkAdvance','execute' : 'TkExecute',
		  'collect' : 'TkCollect', 'default' : 'TkDefault', 'create' : 'TkCreate',
		  'right' : 'TkRight', 'while' : 'TkWhile', 'store' : 'TkStore',
		  'false' : 'TkFalse', 'else' : 'TkElse', 'bool' : 'TkBool', 'left' : 'TkLeft',
		  'drop' : 'TkDrop', 'down' : 'TkDown', 'char' :'TkChar', 'true' : 'TkTrue',
		  'read' : 'TkRead', 'send' : 'TkSend', 'int' : 'TkInt', 'end' : 'TkEnd',
		  'bot' : 'TkBot', 'on' : 'TkOn', 'if' : 'TkIf', 'as' : 'TkAs', 'up' : 'TkUp'}

tokens = ['TkIdent', 'TkNum', 'TkCaracter', 'TkComa', 'TkPunto', 'TkDosPuntos',
		  'TkParAbre', 'TkParCierra', 'TkSuma', 'TkResta', 'TkMult', 'TkDiv',
		  'TkMod', 'TkConjuncion', 'TkDisyuncion', 'TkNegacion', 'TkMenor',
		  'TkMenorIgual', 'TkMayor', 'TkMayorIgual', 'TkIgual', 'TkDistinto'] + list(claves.values())

t_TkComa = r','
t_TkPunto = r'\.'
t_TkDosPuntos = r':'
t_TkParAbre = r'\('
t_TkParCierra = r'\)'
t_TkSuma = r'\+'
t_TkResta = r'-'
t_TkMult = r'\*'
t_TkDiv = r'/'
t_TkMod = r'%'
t_TkConjuncion = r'/\\'
t_TkDisyuncion = r'\\/'
t_TkNegacion = r'~'
t_TkMenor = r'<'
t_TkMenorIgual = r'<='
t_TkMayor = r'>'
t_TkMayorIgual = r'>='
t_TkIgual = r'='
t_TkDistinto = r'/='
t_ignore = r' '

def t_tab(t):
	r'\t'
	pass

def t_newline(t):
	r'\n+'
	t.lexer.lineno += len(t.value)
	pass

def t_comentario(t):
	r'(\$-([^-] | -[^$])*-\$) | (\${2}.*\n)'
	t.lexer.lineno += t.value.count("\n")
	pass

def t_TkCaracter(t):
	r'\'([^\'] | \\[nt] | \\\')\''
	t.value = t.value.translate(str.maketrans({"'":None}))
	return t;


def t_TkIdent(t):
	r'[a-zA-Z][a-zA-Z0-9_]*'
	t.type = claves.get(t.value,'TkIdent') # Si no es una palabra clave,
                                           # se toma como un identificador
	return t

def t_TkNum(t):
	r'\d+'
	t.value = int(t.value)
	return t

def t_error(t):
	t.type = 'error'
	t.lexer.skip(1)
	return t

lexer = lex.lex()

#-------------------------------------------------------------------------------
# getTokens() - función que devuelve la lista de tokens, si hay algún error
# devueve la lista de los tokens de tipo 'error'
#-------------------------------------------------------------------------------

def getTokens(archivo):
	tokens = [] # lista de tokens validos
	errors = [] # lista de tokens de tipo 'error'
	data = open(archivo,'r')
	lexer.input(data.read())

	for tok in lexer:
		if tok.type == 'error':
			errors.append(tok)
		else:
			tokens.append(tok)

	if len(errors) > 0:
		return errors
	else:
		return tokens

#-------------------------------------------------------------------------------
# imprimirTokens() - función para imprimir tokens
#-------------------------------------------------------------------------------  

def imprimirTokens(tokens):
	if (tokens[0].type == 'error'):
		for token in tokens:
			print(token)
	else:
		i = 0
		last = tokens[len(tokens)-1]

		for token in tokens:
			if token == last:
				print(token)
			else:
				if i < 4:
					print(token, end=", ")
					i = i+1
				elif i == 4:
					i = 0
					print(token, end=",\n")

#-------------------------------------------------------------------------------
# imprimirErrores() - función para imprimir tokens de error
#-------------------------------------------------------------------------------  

def imprimirErrores(tokens):
	if (tokens[0].type == 'error'):
		for token in tokens:
			print(token)
		return True
	else:
		return False

#-------------------------------------------------------------------------------

#tokens = getTokens(sys.argv[1])
#imprimirTokens(tokens)