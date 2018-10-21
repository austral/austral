#!/usr/bin/env bash
set -e
set -o pipefail

make boreal
make test

echo "test.au"
./boreal test/valid/test.au --output=test/valid/test.cpp --entrypoint=austral-user:test
clang++ -std=c++11 test/valid/test.cpp -o test/valid/test.bin
./test/valid/test.bin

echo "fib.au"
./boreal test/valid/fib.au --output=test/valid/fib.cpp --entrypoint=austral-user:main
clang++ -std=c++11 test/valid/fib.cpp -o test/valid/fib.bin
./test/valid/fib.bin

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

echo "generic.au"
./boreal test/valid/generic.au --output=test/valid/generic.cpp --entrypoint=austral-user:main
clang++ -std=c++11 test/valid/generic.cpp -o test/valid/generic.bin
./test/valid/generic.bin

echo "size-of.au"
./boreal test/valid/size-of.au --output=test/valid/size-of.cpp
clang++ -shared -fPIC -std=c++11 test/valid/size-of.cpp

echo "cffi.au"
./boreal test/valid/cffi.au --output=test/valid/cffi.cpp --entrypoint=cffi-test:main
clang++ -std=c++11 test/valid/cffi.cpp -o test/valid/cffi.bin
./test/valid/cffi.bin
