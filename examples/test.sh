for filename in "examples"/*.txt; do
    basename="${filename%.*}"
    ./implang -i "$filename" -o "$basename.o"
    clang "$basename.o" "runtime/runtime.o" -o "$basename.exe"
done