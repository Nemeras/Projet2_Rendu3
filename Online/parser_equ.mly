
%token <int> VAR	


%token LPAREN
%token RPAREN
%token EQU
%token DIS
%token IMPLY
%token AND
%token OR
%token NOT
%token EOF		/* Fin de fichier */


%right IMPLY
%left EQU
%left OR
%left AND
%nonassoc NOT
%nonassoc EOF
%nonassoc DIS



%start formula
%type <(int*int) Tseitin.formula> form		/* On construit la form */
%type <(int*int) Tseitin.formula list> formula
			

%%
formula:
	| form formula		{ $1::$2 }
	| EOF			{ [] }
;


form:
	| atom			{ Lit ($1,0) }
	| LPAREN form RPAREN	{ $2 }
	| NOT form		{ Not ($2,0) }
	| form OR form		{ Or ($1, $3, 0) }
	| form AND form		{ And ($1, $3, 0) }
	| form IMPLY form	{ Or (Not ($1,0), $3, 0) }


atom:
	| VAR EQU VAR		{ $1, $3 }
	| VAR DIS VAR		{ -$1 - 1, -$3 - 1 }


