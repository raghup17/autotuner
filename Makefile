FILES=Autotuner.scala Util.scala Tunable.scala

all: ${FILES}
	scalac ${FILES}

clean:
	rm -f *.class

