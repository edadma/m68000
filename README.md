m68k
====

*m68k* is a simulator for the venerable Motorola 68000 microprocessor written in the Scala programming language.

Building a Toolchain
--------------------

A complete Gnu toolchain can be built using the [crosstool-ng](https://crosstool-ng.github.io/) project.  Firstly, a number of packages are needed to run crosstool-ng.  If you are using Linux Mint (as I am), then type

```bash
sudo apt-get install -y gcc gperf bison flex texinfo help2man make libncurses5-dev \
                        python-dev autoconf automake libtool libtool-bin gawk g++
```

To get an up-to-date toolchain, clone the *crosstool-ng* repository.  Begin by entering a folder where you want to put the repository, then type

```bash
git clone https://github.com/crosstool-ng/crosstool-ng.git
cd crosstool-ng
./bootstrap
```

Now you need to decide where you want crosstool-ng to be installed.  I chose to put it in `tools/ct-ng` in my home folder.  To build and install type the following where `<you>` is your user name.

```bash
./configure --prefix=/home/<you>/tools/ct-ng
make
make install
```

Add the following lines to the end of `.profile` in your home folder so that it's on your executable path.

```bash
# crosstool-ng
PATH="$HOME/tools/ct-ng/bin:$PATH"

# m68k "bare metal" tools
PATH="$HOME/x-tools/m68k/bin:$PATH"
```

Now type (in the home folder)

```bash
source .profile
```

Create the folder where crosstool-ng will store component tarballs.  Type (while in the home folder)

```bash
mkdir src
```

Create a work folder where toolchain builds are done.  Type

```bash
mkdir -p ct-ng-work/m68k
```

Copy the `toolchain-config` file from the `m68k` repo into the newly created `ct-ng-work/m68k` folder, but call it `.config`.  Type

```bash
cp /path/to/m68k/toolchain-config ct-ng-work/m68k/.config
```

Build the toolchain by typing

```bash
ct-ng build
```

Crosstool-ng will download all needed components are build the entire cross development toolchain.  The toolchain will be installed under `x-tools/m68k` and the individual tools will be accessible with the prefix `m68k-`, so for example the cross C compiler will be `m68k-gcc`.