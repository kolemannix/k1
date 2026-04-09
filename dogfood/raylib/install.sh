# How to get raylib and build for shared+static
git clone --depth=1 https://github.com/raysan5/raylib.git

cd raylib/src
# Build static
make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=STATIC
mv libraylib.a ../libraylib.a

# Clean and build shared
make clean
make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED
mv libraylib.dylib ../libraylib.dylib
