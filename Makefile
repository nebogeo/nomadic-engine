# _____     ___ ____     ___ ____
#  ____|   |    ____|   |        | |____|
# |     ___|   |____ ___|    ____| |    \    PS2DEV Open Source Project.
#-----------------------------------------------------------------------
# Copyright 2001-2004, ps2dev - http://www.ps2dev.org
# Licenced under Academic Free License version 2.0
# Review ps2sdk README & LICENSE files for further details.
#

EE_BIN = nomadic.elf
EE_LIBS = -ldraw -lpad -lgraph -ldebug -lmath3d -lmf -lpacket -ldma -lm
EE_CXXFLAGS += -fno-exceptions -w
EE_DVP = dvp-as
VCL = openvcl

EE_OBJS = ps2-main.o \
	core/list.o \
	core/geometry.o \
	scheme/scheme.o \
	engine/engine.o \
	engine/scenegraph.o \
	engine/scenenode.o \
	engine/primitive.o \
	engine/ee/ps2-renderer.o \
	engine/ee/ps2-nomadic.o \
	engine/ee/pdk/dma.o \
	engine/ee/pdk/math.o \
	engine/ee/pdk/matrix.o \
	engine/ee/pdk/output.o \
	engine/ee/pdk/screen.o \
	engine/ee/pdk/vram.o \
	engine/ee/pdk/vu1.o \
	engine/ee/asm/vu1_unlit.o

all: $(EE_BIN)
	ee-strip --strip-all $(EE_BIN)

engine/ee/asm/vu1_unlit.o : engine/ee/asm/vu1_unlit.vsm
	$(EE_DVP) $< -o $@

%.vsm : %.vcl
	$(VCL) -o $@ $<

clean:
	rm -f *.elf *.o *.a */*.o */*.a */*/*.o */*/*.o */*/*/*.o */*/*/*.vsm

run: $(EE_BIN)
	ps2client execee host:$(EE_BIN)

reset:
	ps2client reset

include $(PS2SDK)/samples/Makefile.pref
include $(PS2SDK)/samples/Makefile.eeglobal

