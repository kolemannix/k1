# gengame links libs/libraylib.a statically. Either copy Homebrew's build:
mkdir -p libs && cp "$(brew --prefix raylib)/lib/libraylib.a" libs/
# or build it from source:
#   git clone --depth=1 https://github.com/raysan5/raylib.git
#   make -C raylib/src PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=STATIC
#   mv raylib/src/libraylib.a libs/
