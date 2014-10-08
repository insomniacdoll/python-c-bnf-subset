# parser.py
#
#   A subset-C parser, (Partial BNF taken from 1996 International Obfuscated C Code Contest)
#
#
"""
http://www.ioccc.org/1996/august.hint

The following is a description of the OC grammar:

	OC grammar
	==========
	Terminals are in quotes, () is used for bracketing.

	program:	decl*

	decl:		vardecl
			fundecl

	vardecl:	type NAME ;
			type NAME "[" INT "]" ;

	fundecl:	type NAME "(" args ")" "{" body "}"

	args:		/*empty*/
			( arg "," )* arg

	arg:		type NAME

	body:		vardecl* stmt*

	stmt:		ifstmt
			whilestmt
			dowhilestmt
			"return" expr ";"
			expr ";"
			"{" stmt* "}"
			";"

	ifstmt:		"if" "(" expr ")" stmt
			"if" "(" expr ")" stmt "else" stmt

	whilestmt:	"while" "(" expr ")" stmt

	dowhilestmt:	"do" stmt "while" "(" expr ")" ";"

	expr:		expr binop expr
			unop expr
			expr "[" expr "]"
			"(" expr ")"
			expr "(" exprs ")"
			NAME
			INT
			CHAR
			STRING

	exprs:		/*empty*/
			(expr ",")* expr

	binop:		"+" | "-" | "*" | "/" | "%" |
			"=" |
			"<" | "==" | "!="

	unop:		"!" | "-" | "*"

	type:		"int" stars
			"char" stars

	stars:		"*"*
"""

from pyparsing import *
from file_processor import *

# brace definition
LPAR,RPAR,LBRACK,RBRACK,LBRACE,RBRACE,SEMI,COMMA = map(Suppress, "()[]{};,")

#type keywords definition
VOID = Keyword("void")
CHAR = Keyword("char")
SHORT = Keyword("short")
INT = Keyword("int")
LONG = Keyword("long")
FLOAT = Keyword("float")
DOUBLE = Keyword("double")
SIGNED = Keyword("signed")
UNSIGNED = Keyword("unsigned")
CUSTOM_TYPE = Word(alphas+"_", alphanums+"_")

#STORAGE_CLASS_SPEC keywords definition
AUTO = Keyword("auto")
REGISTER = Keyword("register")
STATIC = Keyword("static")
EXTERN = Keyword("extern")
TYPEDEF = Keyword("typedef")

#STRUCT_OR_UNION keywords definition
STRUCT = Keyword("struct")
UNION = Keyword("union")

#control flow keywords definition
WHILE = Keyword("while")
DO = Keyword("do")
IF = Keyword("if")
ELSE = Keyword("else")
RETURN = Keyword("return")

NAME = Word(alphas+"_", alphanums+"_")
integer = Regex(r"[+-]?\d+")
char = Regex(r"'.'")
string_ = dblQuotedString

TYPE = Group((VOID | INT | CHAR | SHORT | LONG | FLOAT | DOUBLE | SIGNED | UNSIGNED ) + ZeroOrMore("*"))

STORAGE_CLASS_SPEC = Group(( AUTO | REGISTER | STATIC | EXTERN | TYPEDEF ) + ZeroOrMore("*"))

STRUCT_OR_UNION = Group(( STRUCT | UNION ) + CUSTOM_TYPE + ZeroOrMore("*"))

expr = Forward()
operand = NAME | integer | char | string_
expr << (operatorPrecedence(operand, 
    [
    (oneOf('! - *'), 1, opAssoc.RIGHT),
    (oneOf('++ --'), 1, opAssoc.RIGHT),
    (oneOf('++ --'), 1, opAssoc.LEFT),
    (oneOf('* / %'), 2, opAssoc.LEFT),
    (oneOf('+ -'), 2, opAssoc.LEFT),
    (oneOf('< == > <= >= != ->'), 2, opAssoc.LEFT),
    (Regex(r'=[^=]'), 2, opAssoc.LEFT),
    ]) + 
    Optional( LBRACK + expr + RBRACK | 
              LPAR + Group(Optional(delimitedList(expr))) + RPAR )
    )

stmt = Forward()

ifstmt = IF - LPAR + expr + RPAR + stmt + Optional(ELSE + stmt)
whilestmt = WHILE - LPAR + expr + RPAR + stmt
dowhilestmt = DO - stmt + WHILE + LPAR + expr + RPAR + SEMI
returnstmt = RETURN - expr + SEMI

stmt << Group( ifstmt |
          whilestmt |
          dowhilestmt |
          returnstmt | 
          expr + SEMI |
          LBRACE + ZeroOrMore(stmt) + RBRACE |
          SEMI)

vardecl = Group(Optional(STORAGE_CLASS_SPEC) + Optional(STRUCT_OR_UNION | TYPE) + NAME + Optional(LBRACK + integer + RBRACK)) + SEMI

arg = Group(Optional(STRUCT_OR_UNION | TYPE) + NAME)
body = ZeroOrMore(vardecl) + ZeroOrMore(stmt)
fundecl = Group(Optional(STORAGE_CLASS_SPEC) + Optional(STRUCT_OR_UNION | TYPE) + NAME + LPAR + Optional(Group(delimitedList(arg))) + RPAR +
            LBRACE + Group(body) + RBRACE)
decl = fundecl | vardecl
program = ZeroOrMore(decl)

program.ignore(cStyleComment)

# set parser element names
for vname in ("ifstmt whilestmt dowhilestmt returnstmt TYPE "
               "NAME fundecl vardecl program arg body stmt".split()):
    v = vars()[vname]
    v.setName(vname)

#~ for vname in "fundecl stmt".split():
    #~ v = vars()[vname]
    #~ v.setDebug()

test = r"""
/* A factorial program */
int
putstr(char *s)
{
    while(*s)
	putchar(*s++);
}

int
fac(int n)
{
    if (n == 0)
	return 1;
    else
	return n*fac(n-1);
}

int
putn(int n)
{
    if (9 < n)
	putn(n / 10);
    putchar((n%10) + '0');
}

int
facpr(int n)
{
    putstr("factorial ");
    putn(n);
    putstr(" = ");
    putn(fac(n));
    putstr("\n");
}

void
main(int* a)
{
    int i;
    i = 0;
    while(i < 10)
	facpr(i++);
    return 0;
}
"""

test2 = r"""
int a(int x){}
"""

#ast = program.parseString(test2, parseAll=True)

#import pprint
#pprint.pprint(ast.asList())

program = ZeroOrMore(ifstmt)
src_text = file_reader('sem.c')
#print src_text
ast = program.parseString(src_text, parseAll=True)
import pprint
pprint.pprint(ast.asList())
