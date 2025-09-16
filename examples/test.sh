clang++ -c examples/runtime/runtime.cpp -o examples/obj/runtime.o
for filename in "examples/src"/*.txt; do
    echo "compiling $filename"
    name=$(basename "$filename" .txt)
    ./build/implang -i "$filename" -o "examples/obj/$name.o" > "examples/log/$name.ll"
    clang++ "examples/obj/$name.o" "examples/obj/runtime.o" -lgc -o "examples/bin/$name.exe"
done