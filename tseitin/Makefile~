all :
	ocamlbuild -yaccflag -v -lib unix Basic/main.native; ln -fs main.native resol
	ocamlbuild -yaccflag -v -lib unix Watched_literals/main_wl.native; ln -fs main_wl.native resol-wl

byte :
	ocamlbuild -yaccflag -v -lib unix Basic/main.byte; ln -fs main.byte resol
	ocamlbuild -yaccflag -v -lib unix Watched_literals/main_wl.byte; ln -fs main_wl.byte resol-wl

clean:
	ocamlbuild -clean

prof:
	ocamlbuild -yaccflag -v -lib unix Basic/main.p.native
	mv main.p.native resol.p
	ocamlbuild -yaccflag -v -lib unix Watched_literals/main_wl.p.native
	mv main_wl.p.native resol-wl.p

exemples:
	wget http://perso.ens-lyon.fr/clement.sartori/Tests.tar.gz
	tar -xvf Tests.tar.gz
	rm Tests.tar.gz
