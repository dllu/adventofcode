GHC = ghc-9.6

TARGET = day02a

all: $(TARGET).exe

$(TARGET).exe: Main.hs
	$(GHC) -o $@ $<

clean:
	rm -f *.hi *.o *.exe
