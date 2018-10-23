m68k
====

*m68k* is an emulator for the venerable Motorola 68000 microprocessor written in the Scala programming language.

Building a Toolchain
--------------------

A complete Gnu cross toolchain can be built using the [crosstool-ng](https://crosstool-ng.github.io/) project.  Firstly, a number of packages are needed to run crosstool-ng.  If you are using Linux Mint (as I am), then type

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

or log out and log back in.

Create the folder where crosstool-ng will store component tarballs.  Type (while in the home folder)

```bash
mkdir src
```

Create a work folder where toolchain builds are done.  Type

```bash
mkdir -p ct-ng-work/m68k
```

Copy the file `toolchain/crosstool-ng-newlib/.config` file from the `m68k` repository into the newly created `ct-ng-work/m68k` folder.  Type

```bash
cd ct-ng-work/m68k
wget https://raw.githubusercontent.com/edadma/m68k/master/toolchain/crosstool-ng-newlib/.config
```

Build the entire toolchain by typing

```bash
ct-ng build
```

Crosstool-ng will download all needed components and build the entire cross development toolchain (could take up to an hour).  The toolchain will be installed under `x-tools/m68k`.  The individual tools will be accessible with the prefix `m68k-`, so for example the cross C compiler will be `m68k-gcc`.

Using the Emulator
------------------

To test the newly built toolchain and start using the emulator, create a folder where a small 68000 test program can be built and also where the emulator executable can be placed.  I'm assuming that folder is called `m68k` under the root of your home folder.

Downloaded the emulator executable from [here](https://dl.bintray.com/edadma/generic/m68k-0.1.jar) and move it to `m68k` (the folder just created).  Now, open two terminals to that folder, one to run the tools and the other to interact with the emulator.

In one terminal type

```bash
cd m68k
java -jar m68k-0.1.jar
```

You should be greeted by

    Motorola 68000 Emulator v0.1
    Type 'help' for list of commands.
    
    > 
    
Now type `setup`, and the emulator will generate a number of helpful files that will be needed.

In the other terminal, type

```bash
sh gcc main.c
sh ld main
```

The above should create a 68000 executable called `main.srec`.  Don't worry about the "numeric overflow" warnings, resulting from extracting debug data that the emulator can use.

In the emulator terminal, type

    l main
    e
    
which loads `main.srec` into emulator "ROM" and then executes the program.  You should see

    sin(1.2)^2 + cos(1.2)^2 = 1
    
    D0=00000009 D1=00000000 D2=00000000 D3=00000000 D4=00000000 D5=00000000 D6=00000000 D7=00000000 
    A0=00010362 A1=00010A33 A2=000109C8 A3=00000000 A4=00000000 A5=00000000 A6=00000000 A7=00120A10 
    T S  III   XNZVC
      *             
       77C  4E56 0000                 currentTime:   LINK     A6, #0              116: services.s
    
    >     

The emulator always displays the current state of the processor and disassembles the instruction at the current PC address after executing any code.  This little test program demonstrates that `printf` and math library functions work.

Type `help` to see all the emulator commands.