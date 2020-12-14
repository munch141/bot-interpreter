#-------------------------------------------------------------------------------
# Analizador de contexto, sintáxis y léxico para el lenguaje BOT.
# Hecho por: Ricardo Münch. Carnet: 11-10684.
#-------------------------------------------------------------------------------

import yacc
import sys
from LexBot import tokens
from copy import deepcopy

#-------------------------------------------------------------------------------
# En este bloque se define la tabla de símbolos
#-------------------------------------------------------------------------------

class celda:
	def __init__(self, tipo, valor):
		self.tipo = tipo   # tipo de la celda
		self.valor = valor # valor en la celda

class Symbol:
	def __init__(self, tipo, valor, comps, activo, tablas_comps):
		self.tipo = tipo                 # tipo de la variable
		self.valor = valor               # valor de la variable
		self.celda = (0, 0)              # celda donde se encuentra el bot
		self.comps = comps               # lista de los comportamientos del bot
		self.activo = activo             # booleano para saber si está activo
		self.tablas_comps = tablas_comps # diccionario con las tablas de
		                                 # símbolos de los comportamientos

class Env:
	def __init__(self, p):
		self.table = {} # diccionario para representar la tabla de símbolos
		self.prev = p   # apuntador a la tabla del nivel superior

	def put(self, s, sym):
		self.table[s] = sym

	def get(self, s):
		e = self
		while (e != None):
			found = e.table.get(s)
			if (found != None):
				return found
			e = e.prev
		return None

	def imprimir(self):
		print("|======================================================|")
		print("|    ID    |   TIPO   |   VALOR  |   CELDA  |  ACTIVO  |")
		print("|======================================================|")
		for item in self.table.items():
			print("|", end="")
			for i in range(10-len(item[0])):
				print(" ", end="")
			print("%s|" %item[0], end="")

			for i in range(10-len(item[1].tipo)):
				print(" ", end="")
			print("%s|" %item[1].tipo, end="")

			for i in range(10-len(str(item[1].valor))):
				print(" ", end="")
			print("%s|" %str(item[1].valor), end="")

			for i in range(10-len(str(item[1].celda))):
				print(" ", end="")
			print("%s|" %str(item[1].celda), end="")

			for i in range(10-len(str(item[1].activo))):
				print(" ", end="")
			print("%s|" %str(item[1].activo))
		print("|======================================================|")


bottom = None #tabla de símbolos global

#-------------------------------------------------------------------------------
# imprimir_tabs() - imprime la cantidad de \t especificada por 'tabs'
#-------------------------------------------------------------------------------

def imprimir_tabs(tabs):
	for i in [x for x in range(tabs)]:
		print("    ", end="")

#-------------------------------------------------------------------------------
# calcular_tipo() - devuelve un string con el tipo del nodo 'e', se usa para
# comparar el tipo de un nodo con uno guardado en la tabla de símbolos
#-------------------------------------------------------------------------------

def calcular_tipo(e):
	if issubclass(e.__class__, EXPRESION_BOOL):
		return "bool"
	elif issubclass(e.__class__, EXPRESION_ARITM):
		return "int"
	else:
		return "char"

#-------------------------------------------------------------------------------
# En este bloque se especifican los nodos del AST, cada nodo corresponde a una
# variable no terminal de la gramática (ver el archivo 'parser.out' generado por
# el parser para revisar la gramática. Cada nodo tiene un método imprimir() que 
# imprime el nodo y sus hijos. Los nodos de instrucciones tienen un método
# correr() para ejecutarse y los nodos de expresiones tienen un método evaluar()
# para determinar su valor.
#-------------------------------------------------------------------------------

class PROGRAMA:
	def __init__(self, declaraciones, ejecucion, tabla_sim):
		self.declaraciones = declaraciones # bloque de declaraciones, 
		                                   # puede ser None
		self.ejecucion = ejecucion         # bloque de ejecución
		self.tabla_sim = tabla_sim         # tabla de símbolos

	def imprimir(self, tabs, poner_tabs):
		if (self.declaraciones != None):
			if poner_tabs:
				imprimir_tabs(tabs)
			print("- declaraciones:")
			for d in self.declaraciones:
				d.imprimir(tabs+1, True)
		
		if poner_tabs:
			imprimir_tabs(tabs)
		print("- instrucción: ", end="")
		self.ejecucion.imprimir(tabs+1, False)

	def correr(
		  self, bot, tabla_global, tabla_local, tablas_comps, superficie, line):
		self.ejecucion.correr(
			None, self.tabla_sim, None, None, superficie, None)

#-------------------------------------------------------------------------------
# Clases del bloque de declaraciones
#-------------------------------------------------------------------------------

class DECLARACION:
	def __init__(self, tipo, ids, comps, line):
		self.tipo = tipo    # tipo del bot(s) declarado
		self.ids = ids      # lista identificadores de los bots declarados
		self.comps = comps  # lista de comportamientos
		self.line = line    # linea de la declaración, se usa para reporte de
		                    # errores

	def __str__(self):
		return "DECLARACION"

	def imprimir(self, tabs, poner_tabs):
		if (poner_tabs):
			imprimir_tabs(tabs)
		print(self)

		
		imprimir_tabs(tabs+1)
		print("- tipo: ", end="")
		print(self.tipo)


		imprimir_tabs(tabs+1)
		print("- variables: ")
		for ident in self.ids:
			imprimir_tabs(tabs+2)
			print("- var: ", end="")
			ident.imprimir(tabs+2, False)


		if (self.comps != None):
			imprimir_tabs(tabs+1)
			print("- comportamientos: ")
			for comp in self.comps:
				comp.imprimir(tabs+2, True)

class COMPORTAMIENTO:
	def __init__(self, cond, instr, tabla_sim):
		self.cond = cond           # condición (activation, deactivation o 
			                       # default) para ejecutar la instrucción
                                   # deseada
		self.instr = instr         # instrucción del robot
		self.tabla_sim = tabla_sim # tabla de símbolos local para las variables
		                           # declaradas por 'collect as' y/o 'read as'

	def __str__(self):
		return "COMPORTAMIENTO"

	def imprimir(self, tabs, poner_tabs):
		if (poner_tabs):
			imprimir_tabs(tabs)
		print(self)

		imprimir_tabs(tabs+1)
		print("- condicion: ", end="")
		if (issubclass(self.cond.__class__, EXPRESION)):
			self.cond.imprimir(tabs+1, False)
		else:
			print(self.cond)

		imprimir_tabs(tabs+1)
		print("- instruccion: ", end="")
		self.instr.imprimir(tabs+1, False)

	def correr(
		  self, bot, tabla_global, tabla_local, tablas_comps, superficie, line):
		bot_sym = tabla_global.get(bot).tablas_comps
		if isinstance(self.cond, EXPRESION):
			self.instr.correr(
				bot, tabla_global, bot_sym[str(id(self.cond))],
				bot_sym, superficie, line)
		else:
			self.instr.correr(
				bot, tabla_global, bot_sym[self.cond],
				bot_sym, superficie, line)

#-------------------------------------------------------------------------------
# Clases del bloque de declaraciones - Instrucciones de robot
#-------------------------------------------------------------------------------

class STORE:
	def __init__(self, valor):
		self.valor = valor # valor a guardar en el bot

	def __str__(self):
		return "STORE"

	def imprimir(self, tabs, poner_tabs):
		if (poner_tabs):
			imprimir_tabs(tabs)
		print(self)

		imprimir_tabs(tabs+1)
		print("- valor: ", end="")
		if isinstance(self.valor, EXPRESION):
			self.valor.imprimir(tabs+1, False)
		else:
			print("'"+self.valor+"'")

	def correr(
		  self, bot, tabla_global, tabla_local, tablas_comps, superficie, line):
		if tabla_local.get("me").tipo == "char":
			if (self.valor == "\\n"):
				value = "\n"
			elif (self.valor == "\\t"):
				value = "\t"
			elif (self.valor == "\\'"):
				value = "'"
			else:
				value = self.valor
		else:
			value = self.valor.evaluar(tabla_local)
		tabla_global.get(bot).valor = value
		tabla_local.get("me").valor = value

class COLLECT:
	def __init__(self, ident):
		self.ident = ident # identificador de la variable donde almacenar el
		                   # valor recolectado (por defecto es "me" a menos que
		                   # se use 'collect as')

	def __str__(self):
		return "COLLECT"

	def imprimir(self, tabs, poner_tabs):
		if (poner_tabs):
			imprimir_tabs(tabs)
		print("%s as %s" %(self, self.ident))

	def correr(
		  self, bot, tabla_global, tabla_local, tablas_comps, superficie, line):
		cell = superficie[tabla_local.get("me").celda]
		if cell.valor is None:
			error_celda_vacia(bot, line)
		elif cell.tipo != tabla_local.get("me").tipo:
			error_coleccion_inadecuada(bot, tabla_local.get("me").tipo,
				tabla_local.get("me").celda, cell.tipo, line)
		else:
			tabla_local.get(self.ident).valor = cell.valor
			if self.ident == "me":
				tabla_global.get(bot).valor = cell.valor

class DROP:
	def __init__(self, expr):
		self.expr = expr # expresión cuyo valor se dejará en la casilla de la
		                 # matriz

	def __str__(self):
		return "DROP"

	def imprimir(self, tabs, poner_tabs):
		if (poner_tabs):
			imprimir_tabs(tabs)
		print(self)

		imprimir_tabs(tabs+1)
		print("- expresion: ", end="")
		self.expr.imprimir(tabs+1, False)

	def correr(
		  self, bot, tabla_global, tabla_local, tablas_comps, superficie, line):
		superficie[tabla_local.get("me").celda] = celda(
			calcular_tipo(self.expr), self.expr.evaluar(tabla_local))

class MOVER:
	def __init__(self, direc, expr):
		self.direc = direc # dirección del movimiento (right, left, up , down)
		self.expr = expr   # expresión aritmética para calcular la cantidad de
		                   # pasos a moverse

	def __str__(self):
		return "MOVER"

	def imprimir(self, tabs, poner_tabs):
		if (poner_tabs):
			imprimir_tabs(tabs)
		print(self)

		imprimir_tabs(tabs+1)
		print("- direccion: ", end="")
		print(self.direc)

		imprimir_tabs(tabs+1)
		print("- expresion: ", end="")
		self.expr.imprimir(tabs+1, False)

	def correr(
		  self, bot, tabla_global, tabla_local, tablas_comps, superficie, line):
		robot_g = tabla_global.get(bot)
		robot_l = tabla_local.get("me")
		cell = robot_g.celda
		if self.direc == "up":
			robot_g.celda = (cell[0] , cell[1]+self.expr.evaluar(tabla_local))
			robot_l.celda = (cell[0] , cell[1]+self.expr.evaluar(tabla_local))
		elif self.direc == "down":
			robot_g.celda = (cell[0] , cell[1]-self.expr.evaluar(tabla_local))
			robot_l.celda = (cell[0] , cell[1]-self.expr.evaluar(tabla_local))
		elif self.direc == "left":
			robot_g.celda = (cell[0]-self.expr.evaluar(tabla_local) , cell[1])
			robot_l.celda = (cell[0]-self.expr.evaluar(tabla_local) , cell[1])
		elif self.direc == "right":
			robot_g.celda = (cell[0]+self.expr.evaluar(tabla_local) , cell[1])
			robot_l.celda = (cell[0]+self.expr.evaluar(tabla_local) , cell[1])

class READ:
	def __init__(self, ident):
		self.ident = ident # identificador de la variable donde almacenar el
		                   # valor a leer (por defecto es "me" a menos que se
		                   # use 'read as')

	def __str__(self):
		return "READ"

	def imprimir(self, tabs, poner_tabs):
		if (poner_tabs):
			imprimir_tabs(tabs)
		print("%s as %s" %(self, self.ident))

	def correr(
		  self, bot, tabla_global, tabla_local, tablas_comps, superficie, line):
		robot_g = tabla_global.get(bot)
		if robot_g.tipo == "int":
			try:
				i = int(input())
			except ValueError:
				error_lectura_inadecuada(bot, robot_g.tipo,line)
		elif robot_g.tipo == "bool":
			i = input()
			if (i != "true") and (i != "false"):
				error_lectura_inadecuada(bot, robot_g.tipo,line)
			i = (i == "true")
		elif robot_g.tipo == "char":
			i = input()
			if (len(i) > 1) and (i != "\\n") and (i != "\\t") and (i != "\\'"):
				error_lectura_inadecuada(bot, robot_g.tipo,line)
			elif (i == "\\n"):
				i = "\n"
			elif (i == "\\t"):
				i = "\t"
			elif (i == "\\'"):
				i = "'"
		if self.ident == "me":
			robot_g.valor = i
		tabla_local.get(self.ident).valor = i



class SEND:	
	def __str__(self):
		return "SEND"

	def imprimir(self, tabs, poner_tabs):
		if (poner_tabs):
			imprimir_tabs(tabs)
		print(self)

	def correr(
		  self, bot, tabla_global, tabla_local, tablas_comps, superficie, line):
		print("%s" %tabla_global.get(bot).valor, end="")

#-------------------------------------------------------------------------------
# Clases del bloque de ejecución - Instrucciones de controlador
#-------------------------------------------------------------------------------

class ACTIVATE:
	def __init__(self, ids, linea):
		self.ids = ids     # lista de identificadores de los bots a activar
		self.linea = linea # linea para el reporte de errores
	
	def __str__(self):
		return "ACTIVATE"

	def imprimir(self, tabs, poner_tabs):
		if (poner_tabs):
			imprimir_tabs(tabs)
		print(self)

		for ident in self.ids:
			imprimir_tabs(tabs+1)
			print("- var: ", end="")	
			ident.imprimir(tabs+2, False)

	def correr(
		  self, bot, tabla_global, tabla_local, tablas_comps, superficie, line):
		for i in self.ids:
			robot = tabla_global.get(i.valor)

			for comp in robot.comps:
				if comp.cond == "activation":
					if robot.activo:
						error_doble_activacion(i.valor, self.linea)
					else:
						comp.correr(i.valor, tabla_global, tabla_local,
							tablas_comps, superficie, self.linea)
						robot.activo = True
				
class ADVANCE:
	def __init__(self, ids, linea):
		self.ids = ids     # lista de identificadores de los bots a actuar
		self.linea = linea # linea para el reporte de errores

	def __str__(self):
		return "ADVANCE"

	def imprimir(self, tabs, poner_tabs):
		if (poner_tabs):
			imprimir_tabs(tabs)
		print(self)

		for ident in self.ids:
			imprimir_tabs(tabs+1)
			print("- var: ", end="")	
			ident.imprimir(tabs+2, False)

	def correr(
		  self, bot, tabla_global, tabla_local, tablas_comps, superficie, line):
		for i in self.ids:
			robot = tabla_global.get(i.valor)
			tables = robot.tablas_comps

			for comp in robot.comps:
				if (  (isinstance(comp.cond, EXPRESION)
					   and comp.cond.evaluar(tables[str(id(comp.cond))])
					   )
					  or (comp.cond == "default")  ):
					if not robot.activo:
						error_avance_desactivado(i.valor, self.linea)
					else:
						comp.correr(i.valor, tabla_global, tabla_local,
							tablas_comps, superficie, self.linea)
						break

class DEACTIVATE:
	def __init__(self, ids, linea):
		self.ids = ids     # lista de identificadores de los bots a desactivar
		self.linea = linea # linea para el reporte de errores

	def __str__(self):
		return "DEACTIVATE"

	def imprimir(self, tabs, poner_tabs):
		if (poner_tabs):
			imprimir_tabs(tabs)
		print(self)

		for ident in self.ids:
			imprimir_tabs(tabs+1)
			print("- var: ", end="")	
			ident.imprimir(tabs+2, False)

	def correr(
		  self, bot, tabla_global, tabla_local, tablas_comps, superficie, line):
		for i in self.ids:
			robot = tabla_global.get(i.valor)

			for comp in robot.comps:
				if comp.cond == "deactivation":
					if not robot.activo:
						error_doble_desactivacion(i.valor, self.linea)
					else:
						comp.correr(i.valor, tabla_global, tabla_local,
							tablas_comps, superficie, self.linea)
						robot.activo = False

class CONDICIONAL:
	def __init__(self, cond, exito, fracaso):
		self.cond = cond       # condición booleana para ejecutar la instrucción
		self.exito = exito     # instrucción a ejecutar si se cumple la
		                       # condición
		self.fracaso = fracaso # instrucción a ejecutar si no se cumple la
		                       # condición (else)

	def __str__(self):
		return "CONDICIONAL"

	def imprimir(self, tabs, poner_tabs):
		if (poner_tabs):
			imprimir_tabs(tabs)
		print(self)


		imprimir_tabs(tabs+1)
		print("- guardia: ",end="")
		self.cond.imprimir(tabs+1, False)


		imprimir_tabs(tabs+1)
		print("- exito: ", end="")
		self.exito.imprimir(tabs+1, False)


		if (self.fracaso != None):
			imprimir_tabs(tabs+1)
			print(" - fracaso: ", end="")
			self.fracaso.imprimir(tabs+1, False)

	def correr(
		  self, bot, tabla_global, tabla_local, tablas_comps, superficie, line):
		if self.cond.evaluar(tabla_global):
			self.exito.correr(bot, tabla_global, tabla_local, tablas_comps,
				superficie, line)
		elif self.fracaso is not None:
			self.fracaso.correr(bot, tabla_global, tabla_local, tablas_comps,
				superficie, line)

class REPETICION_INDET:
	def __init__(self, cond, instr):
		self.cond = cond   # condición de parada del ciclo
		self.instr = instr # instrucción a ejecutar en cada iteración

	def __str__(self):
		return "REPETICION_INDET"

	def imprimir(self, tabs, poner_tabs):
		if (poner_tabs):
			imprimir_tabs(tabs)
		print(self)


		imprimir_tabs(tabs+1)
		print("- guardia: ",end="")
		self.cond.imprimir(tabs+1, False)

		imprimir_tabs(tabs+1)
		print("- cuerpo: ", end="")
		self.instr.imprimir(tabs+1, False)

	def correr(
		  self, bot, tabla_global, tabla_local, tablas_comps, superficie, line):
		while self.cond.evaluar(tabla_global):
			self.instr.correr(bot, tabla_global, tabla_local, tablas_comps,
				superficie, line)

class INCORPORACION_ALCANCE:
	def __init__(self, declaraciones, ejecucion, tabla_sim):
		self.declaraciones = declaraciones # bloque de declaraciones
		self.ejecucion = ejecucion         # bloque de instrucciones
		self.tabla_sim = tabla_sim         # tabla de símbolos para el alcance

	def __str__(self):
		return "INCORPORACION_ALCANCE"

	def imprimir(self, tabs, poner_tabs):
		if (poner_tabs):
			imprimir_tabs(tabs)
		print(self)

		if (self.declaraciones != None):
			if poner_tabs:
				imprimir_tabs(tabs+1)
			print("- declaraciones:")
			for d in self.declaraciones:
				d.imprimir(tabs+2, True)
			
		if poner_tabs:
			imprimir_tabs(tabs+1)
		print("- instrucción: ", end="")
		self.ejecucion.imprimir(tabs+2, False)

	def correr(
		  self, bot, tabla_global, tabla_local, tablas_comps, superficie, line):
		self.ejecucion.correr(None, self.tabla_sim, None, None,
			superficie, None)

		for v_global in self.tabla_sim.table.values():
			v_global.valor = None
			v_global.celda = (0,0)
			v_global.activo = False

class SECUENCIACION:
	def __init__(self, instr):
		self.instr = instr # lista de instrucciones a ejecutar en secuencia

	def __str__(self):
		return "SECUENCIACION"

	def imprimir(self, tabs, poner_tabs):
		if (poner_tabs):
			imprimir_tabs(tabs)
		print(self)

		for i in self.instr:
			i.imprimir(tabs+1, True)

	def correr(
		  self, bot, tabla_global, tabla_local, tablas_comps, superficie, line):
		for i in self.instr:
			i.correr(bot, tabla_global, tabla_local, tablas_comps,
				superficie, line)

#-------------------------------------------------------------------------------
# Clases de expresiones
#-------------------------------------------------------------------------------

# clase padre de las expresiones
class EXPRESION:
	pass

#clase para identificar a las expresiones booleanas
class EXPRESION_BOOL(EXPRESION):
	pass

# clase para identificar a las expresiones aritméticas
class EXPRESION_ARITM(EXPRESION):
	pass

# clase con la estrtuctura de las expresiones binarias
class EXPR_BIN(EXPRESION):
	def __init__(self, op, izq, der, linea):
		self.op = op                  # operador
		self.izq = izq                # operando izquierdo
		self.der = der                # operando derecho
		self.me = (izq.me or der.me)  # booleano para determinar si se usa 'me'
		                              # en la expresión (para el chequeo de
		                              # errores)
		self.linea = linea            # linea para el reporte de errores

	def imprimir(self, tabs, poner_tabs):
		if (poner_tabs):
			imprimir_tabs(tabs)
		print(self)

		
		imprimir_tabs(tabs+1)
		print("- operación: ", end="")
		print(self.op)


		imprimir_tabs(tabs+1)
		print("- operando izquierdo: ", end="")
		self.izq.imprimir(tabs+1, False)


		imprimir_tabs(tabs+1)
		print("- operando derecho: ", end="")
		self.der.imprimir(tabs+1, False)

# clase con la estrtuctura de las expresiones unarias
class EXPR_UN(EXPRESION):
	def __init__(self, op, expr):
		self.op = op      # operador
		self.expr = expr  # operando
		self.me = expr.me # booleano para determinar si se usa 'me' en la
		                  # expresión (para el chequeo de errores)

	def imprimir(self, tabs, poner_tabs):
		if (poner_tabs):
			imprimir_tabs(tabs)
		print(self)


		imprimir_tabs(tabs+1)
		print("- operación: ", end="")
		print(self.op)


		imprimir_tabs(tabs+1)
		print("- operando: ", end="")
		self.expr.imprimir(tabs+1, False)

# expresiones binarias y booleanas
class BIN_BOOL(EXPR_BIN, EXPRESION_BOOL):
	def __str__(self):
		return "BIN_BOOL"

	def evaluar(self, tabla):
		if self.op == "Disyunción":
			return (self.izq.evaluar(tabla) or self.der.evaluar(tabla))
		elif self.op == "Conjunción":
			return (self.izq.evaluar(tabla) and self.der.evaluar(tabla))

# expresiones binarias y relacionales
class BIN_REL(EXPR_BIN, EXPRESION_BOOL):
	def __str__(self):
		return "BIN_REL"

	def evaluar(self, tabla):
		if self.op == "Igual a":
			return (self.izq.evaluar(tabla) == self.der.evaluar(tabla))
		elif self.op == "Distinto de":
			return (self.izq.evaluar(tabla) != self.der.evaluar(tabla))
		elif self.op == "Mayor que":
			return (self.izq.evaluar(tabla) > self.der.evaluar(tabla))
		elif self.op == "Mayor o igual que":
			return (self.izq.evaluar(tabla) >= self.der.evaluar(tabla))
		elif self.op == "Menor que":
			return (self.izq.evaluar(tabla) < self.der.evaluar(tabla))
		elif self.op == "Menor o igual que":
			return (self.izq.evaluar(tabla) <= self.der.evaluar(tabla))


# expresiones binarias y aritméticas
class BIN_ARITM(EXPR_BIN, EXPRESION_ARITM):
	def __str__(self):
		return "BIN_ARITM"

	def evaluar(self, tabla):
		if self.op == "Suma":
			return (self.izq.evaluar(tabla) + self.der.evaluar(tabla))
		elif self.op == "Resta":
			return (self.izq.evaluar(tabla) - self.der.evaluar(tabla))
		elif self.op == "Multiplicación":
			return (self.izq.evaluar(tabla) * self.der.evaluar(tabla))
		elif self.op == "División":
			der = self.der.evaluar(tabla)
			if der == 0:
				error_division_cero(self.linea)
			else:
				return (self.izq.evaluar(tabla) // der)
		elif self.op == "Mod":
			return (self.izq.evaluar(tabla) % self.der.evaluar(tabla))

# expresiones unarias y booleaneas
class UN_BOOL(EXPR_UN, EXPRESION_BOOL):
	def __str__(self):
		return "UN_BOOL"

	def evaluar(self, tabla):
		return not self.expr.evaluar(tabla)

# expresiones unarias y aritméticas
class UN_ARITM(EXPR_UN, EXPRESION_ARITM):
	def __str__(self):
		return "UN_ARITM"

	def evaluar(self, tabla):
		return - self.expr.evaluar(tabla)

# enteros
class NUM(EXPRESION_ARITM):
	def __init__(self, valor):
		self.valor = valor # valor del entero
		self.me = False    # booleano para determinar si se usa 'me' en la
		                   # expresión (para el chequeo de errores)

	def imprimir(self, tabs, poner_tabs):
		if (poner_tabs):
			imprimir_tabs(tabs)
		print(self.valor)

	def evaluar(self, tabla):
		return self.valor

# booleanos (true o false)
class BOOL(EXPRESION_BOOL):
	def __init__(self, valor):
		self.valor = valor # valor del booleano
		self.me = False    # booleano para determinar si se usa 'me' en la
		                   # expresión (para el chequeo de errores)

	def imprimir(self, tabs, poner_tabs):
		if (poner_tabs):
			imprimir_tabs(tabs)
		print(self.valor)

	def evaluar(self, tabla):
		if self.valor == "TkTrue":
			return True
		elif self.valor == "TkFalse":
			return False

# identificadores
class IDENT(EXPRESION):
	def __init__(self, valor, line):
		self.valor = valor        # identificador
		self.me = (valor == "me") # booleano para determinar si se usa 'me' en
		                          # la expresión (para el chequeo de errores)
		self.line = line          # linea donde se leyo, se usa para el
		                          # reporte de errores

	def imprimir(self, tabs, poner_tabs):
		if (poner_tabs):
			imprimir_tabs(tabs)
		print(self.valor)

	def evaluar(self, tabla):
		value = tabla.get(self.valor).valor
		if value is None:
			error_variable_sin_valor(self.valor, self.line)
		
		return value

# clases para chequear el tipo de la variable asociada al identificador
class ID_BOOL(IDENT, EXPRESION_BOOL):
	pass

class ID_ARITM(IDENT, EXPRESION_ARITM):
	pass

#-------------------------------------------------------------------------------
# Aquí se establece la precedencia de los operadores par eliminar conflictos
#-------------------------------------------------------------------------------

precedence = (
    ('left', 'TkDisyuncion'),
    ('left', 'TkConjuncion'),
    ('right', 'TkNegacion'),
    ('nonassoc', 'TkMayor', 'TkMayorIgual', 'TkMenorIgual', 'TkIgual',
    	'TkDistinto'),
    ('left', 'TkSuma', 'TkResta'),
    ('left', 'TkMult', 'TkDiv', 'TkMod'),
    ('right', 'TkRestaU')
)

#-------------------------------------------------------------------------------
# En este bloque se arma la gramática de atributos
#-------------------------------------------------------------------------------

def p_programa(p):
	'''PROGRAMA : BLQ_DECLARACIONES BLQ_EJECUCION
		 		| BLQ_EJECUCION'''
	if (len(p) == 3):
		p[0] = PROGRAMA(p[1], p[2], bottom)
	else:
		p[0] = PROGRAMA(None, p[1], bottom)

#-------------------------------------------------------------------------------
# Bloque de declaraciones
#-------------------------------------------------------------------------------

def p_bloque_declaraciones(p):
	'''BLQ_DECLARACIONES : TkCreate LISTA_DECLARACIONES'''
	global bottom
	bottom = Env(bottom)

	for d in p[2]:
		for ident in d.ids:
			if (bottom.table.get(ident.valor) == None):
				tablas = {}
				for c in d.comps:
					if isinstance(c.cond, EXPRESION):
						tablas[str(id(c.cond))] = deepcopy(c.tabla_sim)
					else:
						tablas[c.cond] = deepcopy(c.tabla_sim)
				if d.comps != []:
					me = deepcopy(c.tabla_sim.table["me"])
					for tab in tablas.values():
						tab.table["me"] = me
				bottom.put(ident.valor, Symbol(d.tipo, None, d.comps, False,
					tablas))
			else:
				error_redeclaracion(ident.valor, d.line)

	p[0] = p[2]

def p_lista_declaraciones(p):
	'''LISTA_DECLARACIONES : LISTA_DECLARACIONES DECLARACION
			   			   | DECLARACION'''
	if (len(p) == 3):
		p[0] = p[1] + [p[2]]
	else:
		p[0] = [p[1]]

def p_declaracion(p):
	'''DECLARACION : TIPO TkBot LISTA_IDENTIFICADORES LISTA_COMPORTAMIENTOS TkEnd
				   | TIPO TkBot LISTA_IDENTIFICADORES TkEnd'''
	if (len(p) == 6):
		c = [x.cond for x in p[4]]

		if c.count("activation") > 1:
			error_comp_duplicado("activation", p.lineno(2))
		elif c.count("deactivation") > 1:
			error_comp_duplicado("deactivation", p.lineno(2))
		elif c.count("default") > 1:
			error_comp_duplicado("default", p.lineno(2))
		elif c.count("default") > 0 and c[len(c)-1] != "default":
			error_comp_default(p.lineno(2))

		p[0] = DECLARACION(p[1], p[3], p[4], p.lineno(2))
	else:
		p[0] = DECLARACION(p[1], p[3], None, p.lineno(2))


def p_tipo(p):
	'''TIPO : TkInt
	 		| TkBool
	 		| TkChar'''
	p[0] = p[1]

def p_lista_comportamientos(p):
	'''LISTA_COMPORTAMIENTOS : LISTA_COMPORTAMIENTOS tipo_h1 COMPORTAMIENTO
		       			     | tipo_h2 COMPORTAMIENTO'''
	if (len(p) == 4):
		p[0] = p[1] + [p[3]]
	else:
		p[0] = [p[2]]

def p_tipo_heredado_1(p):
	'''tipo_h1 :'''
	p[0] = p[-4]

def p_tipo_heredado_2(p):
	'''tipo_h2 :'''
	p[0] = p[-3]

def p_comportamiento(p):
	'''COMPORTAMIENTO : TkOn cambiar_bottom CONDICION TkDosPuntos INST_ROBOT TkEnd'''
	global bottom
	p[0] = COMPORTAMIENTO(p[3], p[5], bottom)
	bottom = p[2]

def p_cambiar_bottom(p):
	'''cambiar_bottom :'''
	global bottom
	p[0] = bottom
	bottom = Env(None)
	bottom.put("me", Symbol(p[-2], None, None, None, None))

def p_condicion(p):
	'''CONDICION : TkActivation
		  		 | TkDeactivation
				 | EXPRESION
		  		 | TkDefault'''
	p[0] = p[1]

def p_instr_robot(p):
	'''INST_ROBOT : INST_R_SIMPLE
				  | INST_R_COMPUESTA'''
	p[0] = p[1]

def p_store(p):
	'''INST_R_SIMPLE : TkStore EXPRESION TkPunto
					 | TkStore TkCaracter TkPunto'''
	global bottom
	var = bottom.get("me")
	t1 = calcular_tipo(p[2])
	t2 = var.tipo

	if t1 == t2:
			p[0] = STORE(p[2])
	else:
		error_tipo_store(t1, t2, p.lineno(1))

def p_collect(p):
	'''INST_R_SIMPLE : TkCollect TkAs TkIdent TkPunto
					 | TkCollect TkPunto'''
	if (len(p) == 5):
		if bottom.get(p[3]) == None:
			bottom.put(p[3], Symbol(bottom.get("me").tipo, None, None, None,
				None))
		else:
			if p[3] == "me":
				error_me(p.lineno(1))
			error_redeclaracion(p[3], p.lineno(1))

		p[0] = COLLECT(p[3])
	else:
		p[0] = COLLECT("me")

def p_drop(p):
	'''INST_R_SIMPLE : TkDrop EXPRESION TkPunto'''
	p[0] = DROP(p[2])

def p_mover(p):
	'''INST_R_SIMPLE : DIRECCION EXPRESION TkPunto
					 | DIRECCION TkPunto'''
	if (len(p) == 4):
		p[0] = MOVER(p[1], p[2])
	else:
		p[0] = MOVER(p[1], NUM(1))

def p_direccion(p):
	'''DIRECCION : TkLeft
		  		 | TkRight
		  		 | TkUp
		  		 | TkDown'''
	p[0] = p[1]

def p_read(p):
	'''INST_R_SIMPLE : TkRead TkAs TkIdent TkPunto
					 | TkRead TkPunto'''
	if (len(p) == 5):
		if bottom.get(p[3]) == None:
			bottom.put(p[3], Symbol(bottom.get("me").tipo, None, None, None,
				None))
		else:
			if p[3] == "me":
				error_me(p.lineno(1))
			error_redeclaracion(p[3], p.lineno(1))

		p[0] = READ(p[3])
	else:
		p[0] = READ("me")

def p_send(p):
	'''INST_R_SIMPLE : TkSend TkPunto'''
	p[0] = SEND()

def p_secuencia_r_1(p): #para secuencias de tamaño 2
	'''INST_R_COMPUESTA : INST_R_SIMPLE INST_R_SIMPLE'''
	p[0] = SECUENCIACION([p[1],p[2]])

def p_secuencia_r_2(p): #para secuencias de tamaño mayor a 2
	'''INST_R_COMPUESTA : INST_R_COMPUESTA INST_R_SIMPLE'''
	p[0] = SECUENCIACION(p[1].instr+[p[2]])

#-------------------------------------------------------------------------------
# Bloque de ejecución
#-------------------------------------------------------------------------------

def p_bloque_ejecucion(p):
	'''BLQ_EJECUCION : TkExecute INST_CONTROLADOR TkEnd'''
	p[0]  = p[2]

def p_instr_controlador(p):
	'''INST_CONTROLADOR : INST_C_SIMPLE
				   		| INST_C_COMPUESTA'''
	p[0] = p[1]

def p_secuencia_c_1(p): #para secuencias de tamaño 2
	'''INST_C_COMPUESTA : INST_C_SIMPLE INST_C_SIMPLE'''
	p[0] = SECUENCIACION([p[1],p[2]])

def p_secuencia_c_2(p): #para secuencias de tamaño mayor a 2
	'''INST_C_COMPUESTA : INST_C_COMPUESTA INST_C_SIMPLE'''
	p[0] = SECUENCIACION(p[1].instr+[p[2]])

def p_activate(p):
	'''INST_C_SIMPLE : TkActivate LISTA_IDENTIFICADORES TkPunto'''
	global bottom

	for i in p[2]:
		if i.me:
			error_me(p.lineno(1))

		if (bottom.get(i.valor) == None):
			error_no_declarada(i.valor, p.lineno(1))

	p[0] = ACTIVATE(p[2], p.lineno(1))

def p_advance(p):
	'''INST_C_SIMPLE : TkAdvance LISTA_IDENTIFICADORES TkPunto'''
	global bottom

	for i in p[2]:
		if i.me:
			error_me(p.lineno(1))

		if (bottom.get(i.valor) == None):
			error_no_declarada(i.valor, p.lineno(1))

	p[0] = ADVANCE(p[2], p.lineno(1))

def p_deactivate(p):
	'''INST_C_SIMPLE : TkDeactivate LISTA_IDENTIFICADORES TkPunto'''
	global bottom

	for i in p[2]:
		if i.me:
			error_me(p.lineno(1))

		if (bottom.get(i.valor) == None):
			error_no_declarada(i.valor, p.lineno(1))

	p[0] = DEACTIVATE(p[2], p.lineno(1))

def p_lista_id(p):
	'''LISTA_IDENTIFICADORES : LISTA_IDENTIFICADORES TkComa TkIdent
					         | TkIdent'''
	if (len(p) == 4):
		p[0] = p[1] + [IDENT(p[3], p.lineno(2))]
	else:
		p[0] = [IDENT(p[1], p.lineno(1))]

def p_condicional(p):
	'''INST_C_SIMPLE : TkIf EXPRESION TkDosPuntos INST_CONTROLADOR TkElse TkDosPuntos INST_CONTROLADOR TkEnd
					 | TkIf EXPRESION TkDosPuntos INST_CONTROLADOR TkEnd'''
	if p[2].me:
			error_me(p.lineno(1))

	if (len(p) == 9):
		p[0] = CONDICIONAL(p[2], p[4], p[7])
	else:
		p[0] = CONDICIONAL(p[2], p[4], None)

def p_repeticion_indet(p):
	'''INST_C_SIMPLE : TkWhile EXPRESION TkDosPuntos INST_CONTROLADOR TkEnd'''
	if p[2].me:
			error_me(p.lineno(1))

	p[0] = REPETICION_INDET(p[2], p[4])

def p_alcance(p):
	'''INST_C_SIMPLE : PROGRAMA'''
	global bottom
	bottom = bottom.prev

	p[0] = INCORPORACION_ALCANCE(p[1].declaraciones, p[1].ejecucion,
		p[1].tabla_sim)

#-------------------------------------------------------------------------------
# Expresiones booleanas y relacionales
#-------------------------------------------------------------------------------

def p_disyuncion(p):
	'''EXPRESION : EXPRESION TkDisyuncion EXPRESION'''
	if ( issubclass(p[1].__class__, EXPRESION_BOOL) 
	     and issubclass(p[3].__class__, EXPRESION_BOOL) ):
		p[0] = BIN_BOOL("Disyunción", p[1], p[3], None)
	else: 
		error_tipo("Disyunción", calcular_tipo(p[1]), calcular_tipo(p[3]),
			p.lineno(2))

def p_conjuncion(p):
	'''EXPRESION : EXPRESION TkConjuncion EXPRESION'''
	if ( issubclass(p[1].__class__, EXPRESION_BOOL)
	   and issubclass(p[3].__class__, EXPRESION_BOOL) ):
		p[0] = BIN_BOOL("Conjunción", p[1], p[3], None)
	else: 
		error_tipo("Conjunción", calcular_tipo(p[1]), calcular_tipo(p[3]),
			p.lineno(2))

def p_igual(p):
	'''EXPRESION : EXPRESION TkIgual EXPRESION'''
	if ( (issubclass(p[1].__class__, EXPRESION_BOOL)
		  and issubclass(p[3].__class__, EXPRESION_BOOL)) 
	     or
	     (issubclass(p[1].__class__, EXPRESION_ARITM)
	   	  and issubclass(p[3].__class__, EXPRESION_ARITM)) ):
		p[0] = BIN_REL("Igual a", p[1], p[3], None)
	else: 
		error_tipo("Igual a", calcular_tipo(p[1]), calcular_tipo(p[3]),
			p.lineno(2))

def p_distinto(p):
	'''EXPRESION : EXPRESION TkDistinto EXPRESION'''
	if ( (issubclass(p[1].__class__, EXPRESION_BOOL)
		  and issubclass(p[3].__class__, EXPRESION_BOOL))
	     or
	     (issubclass(p[1].__class__, EXPRESION_ARITM)
	      and issubclass(p[3].__class__, EXPRESION_ARITM)) ):
		p[0] = BIN_REL("Distinto de", p[1], p[3], None)
	else: 
		error_tipo("Distinto de", calcular_tipo(p[1]), calcular_tipo(p[3]),
			p.lineno(2))		


def p_mayor(p):
	'''EXPRESION : EXPRESION TkMayor EXPRESION'''
	if (issubclass(p[1].__class__, EXPRESION_ARITM)
	    and issubclass(p[3].__class__, EXPRESION_ARITM)):
		p[0] = BIN_REL("Mayor que", p[1], p[3], None)
	else: 
		error_tipo("Mayor que", calcular_tipo(p[1]), calcular_tipo(p[3]),
			p.lineno(2))

def p_mayor_igual(p):
	'''EXPRESION : EXPRESION TkMayorIgual EXPRESION'''
	if (issubclass(p[1].__class__, EXPRESION_ARITM)
	    and issubclass(p[3].__class__, EXPRESION_ARITM)):
		p[0] = BIN_REL("Mayor o igual que", p[1], p[3], None)
	else: 
		error_tipo("Mayor o igual que", calcular_tipo(p[1]),
			calcular_tipo(p[3]), p.lineno(2))

def p_menor(p):
	'''EXPRESION : EXPRESION TkMenor EXPRESION'''
	if (issubclass(p[1].__class__, EXPRESION_ARITM)
	    and issubclass(p[3].__class__, EXPRESION_ARITM)):
		p[0] = BIN_REL("Menor que", p[1], p[3], None)
	else: 
		error_tipo("Menor que", calcular_tipo(p[1]), calcular_tipo(p[3]),
			p.lineno(2))

def p_menor_igual(p):
	'''EXPRESION : EXPRESION TkMenorIgual EXPRESION'''
	if (issubclass(p[1].__class__, EXPRESION_ARITM)
	    and issubclass(p[3].__class__, EXPRESION_ARITM)):
		p[0] = BIN_REL("Menor o igual que", p[1], p[3], None)
	else: 
		error_tipo("Menor o igual que", calcular_tipo(p[1]),
			calcular_tipo(p[3]), p.lineno(2))

def p_negacion(p):
	'''EXPRESION : TkNegacion EXPRESION'''
	if issubclass(p[2].__class__, EXPRESION_BOOL):
		p[0] = UN_BOOL("Negación", p[2])
	else:
		error_tipo("Negación", calcular_tipo(p[2]), None, p.lineno(1))

#-------------------------------------------------------------------------------
# Expresiones aritméticas
#-------------------------------------------------------------------------------

def p_suma(p):
	'''EXPRESION : EXPRESION TkSuma EXPRESION'''
	if (issubclass(p[1].__class__, EXPRESION_ARITM)
	    and issubclass(p[3].__class__, EXPRESION_ARITM)):
		p[0] = BIN_ARITM("Suma", p[1], p[3], p.lineno(2))
	else:
		error_tipo("Suma", calcular_tipo(p[1]), calcular_tipo(p[3]), p.lineno(2))

def p_resta(p):
	'''EXPRESION : EXPRESION TkResta EXPRESION'''
	if (issubclass(p[1].__class__, EXPRESION_ARITM)
	    and issubclass(p[3].__class__, EXPRESION_ARITM)):
		p[0] = BIN_ARITM("Resta", p[1], p[3], p.lineno(2))
	else:
		error_tipo("Resta", calcular_tipo(p[1]), calcular_tipo(p[3]), p.lineno(2))

def p_mult(p):
	'''EXPRESION : EXPRESION TkMult EXPRESION'''
	if (issubclass(p[1].__class__, EXPRESION_ARITM)
	    and issubclass(p[3].__class__, EXPRESION_ARITM)):
		p[0] = BIN_ARITM("Multiplicación", p[1], p[3], p.lineno(2))
	else:
		error_tipo("Multiplicación", calcular_tipo(p[1]), calcular_tipo(p[3]), p.lineno(2))

def p_div(p):
	'''EXPRESION : EXPRESION TkDiv EXPRESION'''
	if (issubclass(p[1].__class__, EXPRESION_ARITM)
	    and issubclass(p[3].__class__, EXPRESION_ARITM)):
		p[0] = BIN_ARITM("División", p[1], p[3], p.lineno(2))
	else:
		error_tipo("División", calcular_tipo(p[1]), calcular_tipo(p[3]), p.lineno(2))

def p_mod(p):
	'''EXPRESION : EXPRESION TkMod EXPRESION'''
	if (issubclass(p[1].__class__, EXPRESION_ARITM)
	    and issubclass(p[3].__class__, EXPRESION_ARITM)):
		p[0] = BIN_ARITM("Mod", p[1], p[3], p.lineno(2))
	else:
		error_tipo("Mod", calcular_tipo(p[1]), calcular_tipo(p[3]), p.lineno(2))

def p_negativo(p):
	'''EXPRESION : TkResta EXPRESION %prec TkRestaU'''
	if issubclass(p[2].__class__, EXPRESION_ARITM):
		p[0] = UN_ARITM("Negativo", p[2])
	else:
		error_tipo("Negativo", calcular_tipo(p[2]), None, p.lineno(1))

#-------------------------------------------------------------------------------
# El resto de las expresiones
#-------------------------------------------------------------------------------

def p_paren(p):
	'''EXPRESION : TkParAbre EXPRESION TkParCierra'''
	p[0] = p[2]

def p_expr_terminal_true(p):
	'''EXPRESION : TkTrue'''
	p[0] = BOOL(p[1])

def p_expr_terminal_false(p):
	'''EXPRESION : TkFalse'''
	p[0] = BOOL(p[1])

def p_expr_terminal_num(p):
	'''EXPRESION : TkNum'''
	p[0] = NUM(p[1])

def p_expr_terminal_ident(p):
	'''EXPRESION : TkIdent'''
	global bottom
	t = bottom.get(p[1])

	if (bottom == None) or (t == None):
		error_no_declarada(p[1], p.lineno(1))
	else:
		if t.tipo == "bool":
			p[0] = ID_BOOL(p[1], p.lineno(1))
		elif t.tipo == "int":
			p[0] = ID_ARITM(p[1], p.lineno(1))
		else:
			p[0] = IDENT(p[1], p.lineno(1))

#-------------------------------------------------------------------------------
# Manejo de errores
#-------------------------------------------------------------------------------

errores_lex = []

def p_error(p):
	if (p == None):
		print("Error sintáctico: se llegó al final del archivo inesperadamente."
			)
		sys.exit()
	elif (p.type != 'error'):
		print("Error sintáctico: hay un '%s' inválido. (linea "
			%p.value, end="")
		print(p.lineno, end="")
		print(", columna ", end="")
		print(p.column, end="")
		print(")")
		sys.exit()
	else:
		errores_lex.append(p)
		parser.errok()

def error_redeclaracion(ident, line):
	print("Error: redeclaración inválida del bot '%s'. (linea %d)"
		  %(ident, line))
	sys.exit()

def error_me(line):
	print("Error: uso inválido de la palabra clave 'me'. (linea %d)"
	      %line)
	sys.exit()

def error_no_declarada(ident, line):
	print("Error: el bot '%s' no ha sido declarado. (linea %d)"
		  %(ident, line))
	sys.exit()

def error_tipo(op, izq, der, line):
	if der != None:
		print("Error: los operandos son de tipos inválidos para"
			  " la operación %s: '%s' y '%s'. (linea %d)"
			  %(op, izq, der, line))
	else:
		print("Error: el operando es de un tipo inválido para "
			  "la operación %s: '%s'. (linea %d)" %(op, izq, line))
	sys.exit()

def error_comp_duplicado(c, line):
	print("Error: hay más de una definición del comportamiento "
		  "'on %s'. (linea %d)" %(c, line))
	sys.exit()

def error_comp_default(line):
	print("Error: la definición de 'on default' no se encuentra de"
		  " última en la lista de comportamientos. (linea %d)" %line)
	sys.exit()

def error_tipo_store(t1, t2, line):
	print("Error: se está guardando un valor de "
		  "tipo '%s' en un bot de tipo '%s'. (linea %d)" %(t1, t2, line))
	sys.exit()

def error_coleccion_inadecuada(ident, t1, celda, t2, line):
	print("Error: el bot '%s' (tipo '%s') intentó recolectar el valor de la "
		  "celda '%s' (tipo '%s'). (linea %d)")
	sys.exit()

def error_lectura_inadecuada(ident, tipo, line):
	print("Error: se leyó un valor de tipo inadecuado para el bot '%s' (tipo "
		  "'%s'). (linea %d)" %(ident, tipo, line))
	sys.exit()

def error_doble_activacion(ident, line):
	print("Error: se intentó activar al bot '%s' cuando ya estaba activado."
		  " (linea %d)" %(ident, line))
	sys.exit()

def error_avance_desactivado(ident, line):
	print("Error: se intentó avanzar al bot '%s' cuando estaba desactivado."
		  " (linea %d)" %(ident, line))
	sys.exit()

def error_doble_desactivacion(ident, line):
	print("Error: se intentó desactivar al bot '%s' cuando ya estaba"
		  " desactivado. (linea %d)" %(ident, line))
	sys.exit()

def error_variable_sin_valor(ident, line):
	print("Error: el bot '%s' no está inicializado, no se pudo evaluar la"
		  " expresión. (linea %d)" %(ident, line))
	sys.exit()

def error_division_cero(line):
	print("Error: división por cero. (linea %d)" %line)
	sys.exit()

#-------------------------------------------------------------------------------
# Ejecución del parser e impresión del AST
#-------------------------------------------------------------------------------

parser = yacc.yacc()

def parse(p):

	data = open(p,'r')
	s = data.read()

	ast = parser.parse(s)

	if (len(errores_lex) != 0):
		for err in errores_lex:
			print(err)
		return None
	else:
		return ast
