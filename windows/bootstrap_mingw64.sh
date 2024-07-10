#!/usr/
pacman -Syu
pacman -Sy
pacman -Sy --needed \
        bash filesystem libarchive libcurl libgpgme libiconv libintl libreadline \
        msys2-runtime ncurses pacman

pacman -Syu \
        autoconf autogen automake automake-wrapper diffutils git guile libgc \
        libguile libidn-devel libltdl libnettle-devel libopenssl \
        libp11-kit-devel libtasn1-devel libunistring make \
        mingw-w64-x86_64-wxwidgets3.2-gtk3 mingw-w64-x86_64-binutils \
        mingw-w64-x86_64-bzip2 mingw-w64-x86_64-cairo mingw-w64-x86_64-crt-git \
        mingw-w64-x86_64-dbus mingw-w64-x86_64-emacs-pdf-tools-server \
        mingw-w64-x86_64-expat mingw-w64-x86_64-fontconfig \
        mingw-w64-x86_64-freetype mingw-w64-x86_64-gcc mingw-w64-x86_64-gcc-libs \
        mingw-w64-x86_64-gdk-pixbuf2 mingw-w64-x86_64-gettext \
        mingw-w64-x86_64-giflib mingw-w64-x86_64-glib2 mingw-w64-x86_64-gmp \
        mingw-w64-x86_64-gnutls mingw-w64-x86_64-harfbuzz \
        mingw-w64-x86_64-headers-git mingw-w64-x86_64-imagemagick \
        mingw-w64-x86_64-isl mingw-w64-x86_64-jansson mingw-w64-x86_64-jbigkit \
        mingw-w64-x86_64-libffi mingw-w64-x86_64-libgccjit \
        mingw-w64-x86_64-libiconv mingw-w64-x86_64-libidn2 \
        mingw-w64-x86_64-libjpeg-turbo mingw-w64-x86_64-libpng \
        mingw-w64-x86_64-librsvg mingw-w64-x86_64-libsystre \
        mingw-w64-x86_64-libtasn1 mingw-w64-x86_64-libtiff \
        mingw-w64-x86_64-libunistring mingw-w64-x86_64-libwinpthread-git \
        mingw-w64-x86_64-libxml2 mingw-w64-x86_64-mpc mingw-w64-x86_64-mpfr \
        mingw-w64-x86_64-nettle mingw-w64-x86_64-p11-kit mingw-w64-x86_64-pango \
        mingw-w64-x86_64-pixman mingw-w64-x86_64-winpthreads \
        mingw-w64-x86_64-xpm-nox mingw-w64-x86_64-xz mingw-w64-x86_64-zlib nano \
        openssl pkgconf tar texinfo wget
