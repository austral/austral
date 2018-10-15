#!/usr/bin/env bash

make clean
make boreal
make test

echo "test.au"
./boreal test/valid/test.au --output=test/valid/test.cpp
clang++ -shared -fPIC -std=c++11 test/valid/test.cpp

echo "fib.au"
./boreal test/valid/fib.au --output=test/valid/fib.cpp
clang++ -shared -fPIC -std=c++11 test/valid/fib.cpp

echo "tuple.au"
./boreal test/valid/tuple.au --output=test/valid/tuple.cpp
clang++ -shared -fPIC -std=c++11 test/valid/tuple.cpp

echo "class.au"
./boreal test/valid/class.au --output=test/valid/class.cpp
clang++ -shared -fPIC -std=c++11 test/valid/class.cpp

echo "string.au"
./boreal test/valid/string.au --output=test/valid/string.cpp
clang++ -shared -fPIC -std=c++11 test/valid/string.cpp

echo "hello-world.au"
./boreal test/valid/hello-world.au --output=test/valid/hello-world.cpp
clang++ -shared -fPIC -std=c++11 test/valid/hello-world.cpp
