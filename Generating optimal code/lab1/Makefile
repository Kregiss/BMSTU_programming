lab1:
	g++ -g -std=c++14 -I`gcc -print-file-name=plugin`/include -fPIC -fno-rtti -shared src/plugin/lab1.cpp -o src/plugin/$@.so

test:
	gcc -fplugin=src/plugin/lab1.so -O0 src/test/test.c -o src/test/test
	@rm src/test/test

clean:
	rm -rf plugin src/test/test
