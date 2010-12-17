CXX		= g++
LD		= g++
YACC		= bison -y
YACCFLAGS	= -d
LEX		= flex
MV		= mv
CXXFILES	= parser.cc lexer.cc tree.cc icode.cc bullet.cc
CFILES		= main.c panic.c gl_util.c image.c
OBJS		= $(CFILES:.c=.o) $(CXXFILES:.cc=.o)
TARGET		= curtain
CFLAGS		= -Wall -O2 -g `sdl-config --cflags`
LDFLAGS		= -g
LIBS		= -lGL -lGLU -lpng `sdl-config --libs`

$(TARGET): $(OBJS)
	$(LD) $(LDFLAGS) $(OBJS) -o $@ $(LIBS)

parser.cc parser.h: parser.y
	$(YACC) $(YACCFLAGS) -b $(<:.y=) $<
	$(MV) $(<:.y=).tab.c $(<:.y=).cc
	$(MV) $(<:.y=).tab.h $(<:.y=).h

lexer.cc: lexer.l parser.h
	$(LEX) $(LFLAGS) -o$@ $<

.cc.o:
	$(CXX) $(CFLAGS) -c $<

.c.o:
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f *.o lexer.cc parser.h parser.cc $(TARGET)
