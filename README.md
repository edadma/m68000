m68k
====

*m68k* is an emulator for the venerable Motorola 68000 microprocessor written in the Scala programming language.  The emulator can load executables in Motorola S-record format.


Building a Toolchain
--------------------

A complete Gnu cross toolchain for the 68000 can be built using the [crosstool-ng](https://crosstool-ng.github.io/) project.  Firstly, a number of packages are needed to run *crosstool-ng*.  If you are using Ubuntu Linux (as I am), then type

```bash
sudo apt-get install -y gcc g++ gperf bison flex texinfo help2man make libncurses5-dev \
                        python3-dev autoconf automake libtool libtool-bin gawk wget bzip2 xz-utils \
                        unzip patch libstdc++6 rsync
```

The toolchain will be build using version 1.24.0, but first we need to download and build *crosstool-ng*.  Start by downloading [crosstool-ng v1.24.0](http://crosstool-ng.org/download/crosstool-ng/crosstool-ng-1.24.0.tar.xz) and extract.  The folder where you extract to will be discarded after building and installing *crosstool-ng* so it doesn't matter where you extract to.  This folder will be referred to as `/path/to/crosstool-ng/sources`.

Now you need to decide where you want crosstool-ng to be installed.  I chose to put it in `tools/ct-ng` in my home folder, or `$HOME/tools/ct-ng`.  To build and install type the following:

```bash
cd /path/to/crosstool-ng/sources
./configure --prefix=$HOME/tools/ct-ng
make && make install
```

Add the following lines to the end of `.profile` in your home folder so that it's on your executable path:

```bash
# crosstool-ng
PATH="$HOME/tools/ct-ng/bin:$PATH"

# m68k "bare metal" tools
PATH="$HOME/x-tools/m68k/bin:$PATH"
```

Now source the changes to `~./profile` by typing:

```bash
source ~/.profile
```

or log out and log back in.

Create the folder where *crosstool-ng* will store component tarballs by typing:

```bash
cd
mkdir -p ct-ng/src
```

Create a work folder where toolchain builds are done.  Type

```bash
mkdir ct-ng/m68k
```

Copy the file `toolchain/crosstool-ng-newlib/.config` file from the `m68k` repository into the newly created `ct-ng-work/m68k` folder.  Type

```bash
cd ct-ng/m68k
wget https://raw.githubusercontent.com/edadma/m68k/master/toolchain/crosstool-ng-newlib/.config
```

Build the entire toolchain by typing

```bash
ct-ng build
```

*Crosstool-ng* will download all needed components and build the entire cross development toolchain (could take up to an hour).  The toolchain will be installed under `x-tools/m68k`.  The individual tools will be accessible with the prefix `m68k-`, so for example the cross C compiler will be `m68k-gcc`.

Using the Emulator
------------------

To test the newly built toolchain and start using the emulator, create a folder where a small 68000 test program can be built and also where the emulator executable can be placed.  I'm assuming that folder is called `m68k` in your home folder.

Downloaded the emulator executable from [here](https://dl.bintray.com/edadma/generic/m68k-0.1.1.jar) and move it to `~/m68k` (the folder just created).  Now, open two terminals to that folder, one to interact with the emulator and the other to run the newly built tools.

In the emulator terminal type

```bash
cd ~/m68k
java -jar m68k-0.1.1.jar
```

You should be greeted by

    Motorola 68000 Emulator v0.1.1
    Type 'help' for list of commands.
    
    > 
    
Now type `setup`, and the emulator will generate a number of helpful files that will be needed.

In the tools terminal, type

```bash
cd ~/m68k
sh gcc main.c
sh ld main
```

The above should create a 68000 executable called `main.srec`.  Don't worry about the "numeric overflow" warnings resulting from extracting debug data that the emulator can use.

In the emulator terminal, type

    l main
    e
    
which loads `main.srec` into emulator "ROM" (`l main`) and then executes the program (`e`).  You should see

    sin(1.2)^2 + cos(1.2)^2 = 1
    
    D0=00000009 D1=00000000 D2=00000000 D3=00000000 D4=00000000 D5=00000000 D6=00000000 D7=00000000 
    A0=00010362 A1=00010A33 A2=000109C8 A3=00000000 A4=00000000 A5=00000000 A6=00000000 A7=00120A10 
    T S  III   XNZVC
      *             
       77C  4E56 0000                 currentTime:   LINK     A6, #0              116: services.s
    
    >

The emulator always displays the current state of the processor and disassembles the instruction at the current PC address after executing any code.  This little test program demonstrates that `printf()` and math library functions (`sin()`, `cos()` and `pow()`) work.

Type `re` to reset the emulated CPU, and then type `u` to disassemble (the *u* stands for unassemble) the program at the beginning of the normal startup code.

Type `help` to see all the different emulator commands.