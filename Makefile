

build:
	stack ghc Interpreter.hs

run:
	Interpreter.exe

clean:
	del Interpreter.exe Interpreter.o Interpreter.hi