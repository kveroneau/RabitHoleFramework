# Web6502 Rabit Hole Pascal Examples

In this directory you'll find a few files to work with the new [Web6502](https://github.com/kveroneau/Web6502/) Rabit Hole integration.  While *Web6502* isn't needed to use **Rabit Hole Framework**, if you choose to use it, there is a package file with a new Application class you can use, along with some example Web6502 programs written in Pascal here.

A fully working example can be found in the new `example2.lpi` Lazarus project.  The idea here is to create a specific standard Web6502 system for the Rabit Hole Framework, this will allow programs built on Web6502 for the Rabit Hole Framework to run as efficiently as possible, as the installed devices and cards will be known immediately without needing to perform any costly detection routines.

In this examples directory there are two files, `rb6502.pas` which is a Pascal unit file, which will hook into a new Rabit Hole specific 6502 Interface Card, and this unit will provide access to that virtual card's routines.  The next file `rabitboot.pas` is a bootloader to use with the Rabit Hole Framework over the original `WEB6502.SYS` provided by the current release of Web6502.  This special bootloader works very differently from the original, in that it does not detect any cards, the needed cards are hardcoded into the bootloader.  It will require a compatible disk card in slot 6 to run, and will attempt to load `RBKERNEL.SYS` into address `$801` and start it.

Please stay tuned as a Rabit Hole Kernel is being developed.
