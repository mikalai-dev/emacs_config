# Emacs configuration files

## Prerequisites

- Enable deb-src in /etc/apt/sources.list. 
- Install all the dependencies for building emacs:

```shell
sudo apt-get build-dep emacs
```


## Building emacs

- Clone the latest emacs version:

```
git clone git://git.savannah.gnu.org/emacs.git
```

- Build and install
```shell
./configure \
    --with-native-compilation \
    --with-json \
    --with-tree-sitter \
    --with-imagemagick \
    --with-xwidgets && make --jobs=$(nproc) && sudo make install`
```

- Clone this repository to ~/.emacs.d directory and run it for installing all the required packages.
This configuration was tested on the Emacs 29.1 built from the sources.

## Know issues

### 1. Incorrect symbols in the status line. 

Solution: run

```
M-x all-the-icons-install-fonts
M-x nerd-icons-install-fonts
```
