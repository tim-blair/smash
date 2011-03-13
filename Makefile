native = src/main/c
src = src/main/scala
headers = target/headers
classDir = target/classes
soDir = target/resources
soFile = $(soDir)/libLauncher.so

all: sharedobj

sharedobj: gen_header
	gcc -shared -I/opt/java/include/ -I/opt/java/include/linux -o $(soFile) $(native)/Launcher.c -fPIC

gen_header: scala
	javah -d $(headers) -classpath $(classDir) Launcher 

scala: project
	fsc -d $(classDir) -deprecation $(src)/*.scala

project:
	mkdir -p $(soDir)
	mkdir -p $(headers)
	mkdir -p $(classDir)

lines:
	wc -l $(src)/*.scala

clean:
	rm -rf target

.PHONY: clean
