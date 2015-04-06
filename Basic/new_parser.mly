%{
open Form
%}

%token <int> LIT	/* Indice de littéral */

%token NOT
%token IMPLY
%token AND
%token LPAREN
%token RPAREN
%token EOF		/* Fin de fichier */

%start form
%type <Cnf.form> form	/* On construit la FORM et on compte son nombre de clauses
			/* et l'indice max des variables en même temps qu'on lit le fichier */

%%

form:			/* Construction d'une FORM
    | LIT                 {Lit($1)}
    | LPAREN form RPAREN  {$2}
    | NOT form            {Not($2)}
    | form OR form        {Or($1,$2)}
    | form AND form       {And($1,$3)}
    | form IMPLY form     {Imply($1,$3)}
    | form EOF            {$1}
