#include <stdlib.h>
#include <unistd.h>

int main(int argc, char **argv) {
    // Remove "file://" prefix from the single argument (if given)
    if (argc == 2) {
        char *path = argv[1];
        const char *prefix = "file://";
        for (; path && prefix && *path == *prefix; ++path, ++prefix);
        if (*path && !*prefix) {
            // Patching 'argv'
            argv[1] = path;
        }
    }
    // Patching file name
    argv[0] = "freemind";
    execvp(argv[0], argv);
    return EXIT_FAILURE;
}
