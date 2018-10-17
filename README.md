m68k
====

*m68k* is a simulator for the venerable Motorola 68000 microprocessor written in the Scala programming language.

Building a Toolchain
--------------------

A complete Gnu toolchain can be built using the [crosstool-ng](https://crosstool-ng.github.io/) project.  To get an up-to-date toolchain, clone the *crosstool-ng* repository.  Begin by entering a folder where you want to put the repository, then enter the following.

todo: check if os setup is good for mint: apt-get install -y gcc gperf bison flex texinfo help2man make libncurses5-dev \
                                        python-dev autoconf automake libtool libtool-bin gawk
                                        
```bash
git clone https://github.com/crosstool-ng/crosstool-ng.git
cd crosstool-ng
./bootstrap
```

