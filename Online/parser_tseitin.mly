

%token <int> LIT	


%token LPAREN
%token RPAREN
%token IMPLY
%token AND
%token OR
%token NOT
%token EOF		/* Fin de fichier */


%nonassoc EOF
%right IMPLY
%left OR
%left AND 
%nonassoc NOT

%start formula
%type <Tseitin.form> form		/* On construit la form */
%type <Tseitin.formlist> formula	/* On construit la form */
			

%%
formula:
	| form formula		{ (fst $1)::(fst $2), max (snd $1) (snd $2) }
	| EOF			{ [], 0 }
;

form:
	| LIT			{ Lit ($1,0), $1 }
	| LPAREN form RPAREN	{ $2 }
	| NOT form		{ Not (fst $2,0), snd $2 }
	| form OR form		{ Or (fst $1, fst $3, 0), max (snd $1) (snd $3) }
	| form AND form		{ And (fst $1, fst $3, 0), max (snd $1) (snd $3) }
	| form IMPLY form	{ Or (Not (fst $1,0), fst $3, 0), max (snd $1) (snd $3) }

