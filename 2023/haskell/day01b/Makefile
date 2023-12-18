GHC = ghc

TARGET = day01b

all: $(TARGET).exe

$(TARGET).exe: Main.hs
	$(GHC) -o $@ $<

clean:
	rm -f *.hi *.o *.exe
