#include <unistd.h>
#include <sys/types.h>
#include <errno.h>

#ifdef __APPLE__
#include <sys/attr.h>
#include <sys/clonefile.h>
#include <fcntl.h>
#endif

#ifdef __linux__
#include <sys/sendfile.h>
#include <sys/stat.h>
#endif

#define BUFSIZE 131072

static int copy_fallback(int src_fd, int dst_fd) {
    char buf[BUFSIZE];
    ssize_t n;
    while ((n = read(src_fd, buf, BUFSIZE)) > 0) {
        char *p = buf;
        while (n > 0) {
            ssize_t w = write(dst_fd, p, n);
            if (w < 0) return -1;
            p += w;
            n -= w;
        }
    }
    return (n < 0) ? -1 : 0;
}

int direct_copy(int src_fd, int dst_fd) {
#ifdef __linux__
    struct stat st;
    if (fstat(src_fd, &st) == 0 && S_ISREG(st.st_mode)) {
        off_t offset = 0;
        ssize_t remaining = st.st_size;
        while (remaining > 0) {
            ssize_t sent = sendfile(dst_fd, src_fd, &offset, remaining);
            if (sent < 0) {
                if (errno == EINVAL || errno == ENOSYS) break;
                return -1;
            }
            if (sent == 0) break;
            remaining -= sent;
        }
        if (remaining <= 0) return 0;
    }
#endif
    return copy_fallback(src_fd, dst_fd);
}
