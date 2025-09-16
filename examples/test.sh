clang++ -c examples/runtime/runtime.cpp -o examples/obj/runtime.o
for filename in "examples/src"/*.txt; do
    name=$(basename "$filename" .txt)
    ./build/implang -i "$filename" -o "examples/obj/$name.o"
    clang++ "examples/obj/$name.o" "examples/obj/runtime.o" -o "examples/bin/$name.exe"
done