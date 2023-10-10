CMAS = unix.cma threads.cma graphics.cma 

RM   = rm   # use rm for Linux,   del  for Windows
TRUE = true # use true for Linux, cd . for Windows
OEXT = .out	# use .out for Linux, .exe for Windows

.SUFFIXES:
.SUFFIXES: .cmo .cmi .ml .mli

.mli.cmi:;	ocamlc -c -o $@ $<
.ml.cmo:;	ocamlc -c -thread -o $@ $< 

all: vector basis board drawer pose command player game app app_cc app_kk
	echo done.

vector: globals.cmo ivector.cmi vector.cmo test_vector.cmo
	$(eval CMOS = globals.cmo vector.cmo)
	ocamlc -thread $(CMOS) test_vector.cmo -o $@$(OEXT)

basis: vector ibasis.cmi basis.cmo test_basis.cmo
	$(eval CMOS = $(CMOS) basis.cmo)
	ocamlc -thread $(CMOS) test_basis.cmo -o $@$(OEXT) 

board: basis iboard.cmi board.cmo test_board.cmo
	$(eval CMOS = $(CMOS) board.cmo)
	ocamlc -thread $(CMOS) test_board.cmo -o $@$(OEXT) 

drawer: board idrawer.cmi ipose.cmi drawer.cmo 
	$(eval CMOS = $(CMOS) drawer.cmo)

pose: drawer ipose.cmi pose.cmo test_pose.cmo
	$(eval CMOS = $(CMOS) pose.cmo)
	ocamlc -I threads -thread $(CMAS) $(CMOS) test_pose.cmo -o $@$(OEXT) 

command: pose icommand.cmi command.cmo test_command.cmo
	$(eval CMOS = $(CMOS) command.cmo)
	ocamlc -I threads -thread $(CMAS) $(CMOS) test_command.cmo -o $@$(OEXT) 

player: command iplayer.cmi player_com.cmo player_key.cmo test_player_com.cmo
	$(eval CMOS = $(CMOS) player_com.cmo player_key.cmo)
	ocamlc -I threads -thread $(CMAS) $(CMOS) test_player_com.cmo -o $@$(OEXT) 

game: player igame.cmi game.cmo test_game.cmo
	$(eval CMOS = $(CMOS) game.cmo)
	ocamlc -I threads -thread $(CMAS) $(CMOS) test_game.cmo -o $@$(OEXT) 

app: game app.cmo
	ocamlc -I threads -thread $(CMAS) $(CMOS) app.cmo -o $@$(OEXT) 

app_cc: game app_cc.cmo
	ocamlc -I threads -thread $(CMAS) $(CMOS) app_cc.cmo -o $@$(OEXT) 

app_kk: game app_kk.cmo
	ocamlc -I threads -thread $(CMAS) $(CMOS) app_kk.cmo -o $@$(OEXT) 

clean:
	$(RM) *.cmi | $(TRUE)
	$(RM) *.cmo | $(TRUE)
