#include <clang-c/Index.h>
#include <stdio.h>

enum CXChildVisitResult visitor(CXCursor c, CXCursor parent, CXClientData data) {
    enum CXCursorKind k = clang_getCursorKind(c);
    CXString ks = clang_getCursorKindSpelling(k);
    CXString sp = clang_getCursorSpelling(c);

    printf("cursor kind=%s raw=%d spelling=%s\n",
           clang_getCString(ks),
           k,
           clang_getCString(sp));

    clang_disposeString(ks);
    clang_disposeString(sp);
    return CXChildVisit_Continue;
}

int main(int argc, char **argv) {
    CXIndex idx = clang_createIndex(0, 0);
    CXTranslationUnit tu = NULL;

    enum CXErrorCode err = clang_parseTranslationUnit2(
        idx,
        argv[1],
        NULL,
        0,
        NULL,
        0,
        CXTranslationUnit_SkipFunctionBodies,
        &tu
    );

    printf("err=%d\n", err);

    CXCursor root = clang_getTranslationUnitCursor(tu);
    clang_visitChildren(root, visitor, NULL);

    return 0;
}
