all :
	ocamlbuild -yaccflag -v -lib unix -Is DPLL,General,Lexers_Parsers,Theories main.native
	ln -fs main.native resol

byte :
	ocamlbuild -yaccflag -v -lib unix -Is DPLL,General,Lexers_Parsers,Theories main.byte
	ln -fs main.byte resol

clean:
	ocamlbuild -clean
	rm resol

prof:
	ocamlbuild -yaccflag -v -lib unix Code/main.p.native
	mv main.p.native resol.p

exemples:
	wget http://perso.ens-lyon.fr/clement.sartori/Tests.tar.gz
	tar -xvf Tests.tar.gz
	rm Tests.tar.gz
