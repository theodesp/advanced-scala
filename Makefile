MILL=mill
TARGET=Examples
FLAGS=--watch

all: compile
compile:
	$(MILL) $(TARGET).compile

watch:
	$(MILL) $(FLAGS) $(TARGET).compile

watch-tests:
	$(MILL) $(FLAGS) $(TARGET).test

run:
	$(MILL) $(TARGET).run

resolve:
	$(MILL) resolve _

launcher:
	$(MILL) $(TARGET).launcher

assembly:
	$(MILL) $(TARGET).assembly

console:
	$(MILL) -i $(TARGET).console

repl:
	$(MILL) -i $(TARGET).repl

inspect:
	$(MILL) inspect $(TARGET).run

clean:
	$(MILL) clean $(TARGET)

test:
	$(MILL) $(TARGET).test

gen-idea:
	$(MILL) mill.scalalib.GenIdea/idea