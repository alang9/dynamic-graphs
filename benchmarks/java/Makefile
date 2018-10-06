LIB_PATH="Undin-DynamicConnectivity/src/main/java/"

BenchmarkProgram.jar: BenchmarkProgram.class
	jar vcfe $@ BenchmarkProgram *.class -C $(LIB_PATH) .

BenchmarkProgram.class: BenchmarkProgram.java
	javac -cp $(LIB_PATH) BenchmarkProgram.java

.PHONY: clean
clean:
	rm -f BenchmarkProgram.jar
	find . -name '*.class' -delete
