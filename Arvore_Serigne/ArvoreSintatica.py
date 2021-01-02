import ply.lex as lex
import ply.yacc as yacc

################### Analise Lexica - Expressões regulares
tokens = (
   'NUMBER',
   'PLUS',
   'MINUS',
   'LPAREN',
   'RPAREN',
   'MULT',
   'DIV',
   'FLOAT',
   'POT'
)

t_PLUS    = r'\+'
t_MINUS   = r'\-'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_MULT    = r'\*'
t_DIV     = r'\/'
t_POT     = r'\^'

precedence = (
    ('left', 'PLUS', 'MINUS', 'MULT', 'DIV', 'POT'),
)

#A regular expression float
def t_FLOAT(t):
  r'[0-9]+\.[0-9]+'
  t.value = float(t.value)
  return t

def t_NUMBER(t):
    r'-?\d+'
    t.value = int(t.value)
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

t_ignore  = ' \t'

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()

################### Analise Sintatica - Gramaticas Livre de Contexto
def p_exp_ops(p):
    '''exp : exp PLUS exp
                  | exp MINUS exp
                          | exp MULT exp
                                  | exp DIV exp
                                          | exp POT exp'''


    p[0] = (p[2],p[1],p[3])

def p_exp_group(p):
    'exp : LPAREN exp RPAREN'
    p[0] = (p[2])

def p_exp_number(p):
    'exp : NUMBER'
    p[0] = (p[1])

def p_exp_float(p):
    'exp : FLOAT'
    p[0] = p[1]

def p_error(p):
    print("Syntax error in input!")

parser = yacc.yacc()

while True:
   try:
       s = input('calc > ')
   except EOFError:
       break
   if not s: continue
   result = parser.parse(s)
   print(result)
