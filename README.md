# DOS Image Filter

This is a simple application that runs on the [DOSBox emulator](https://www.dosbox.com/) that can apply different filters to images. It is completely written in assembly code for DOS.

## How to run

The project must be assembled using [nasm](https://www.nasm.us/) and linked using a linker (such as [freelink](https://www.pcorner.com/list/ASSEMBLY)). For simplicity, this process happens inside de DOSBox enviroment. To launch the DOSBox emulator, assemble and run the program, run

    make

This should do everything mentioned above, and you should be greeted by a DOSBox window running the program.

If you want to simply assemble the code, and no link it afterwards, run

    make nasm

Doing this, DOSBox should launch and, in it, the program should be assembled. This should also create the files `IMGFLTR.LST` and `IMGFLTR.OBJ` in the root of this project.

## Functionality

### Interface

The program has a very simple interface (all in portuguese) where you can load an image, apply a low-pass, high-pass or gradient filter and exit exit the program.

### Opening a file

By default, the program will try to open the file `images/original.txt` as the image. Afterwards, the image should be loaded and displayed in the left-hand-side of the screen.

To change the file you want to read, search for the `file_path` variable in the `imgfltr.asm` file and change its value. I wouldn't recommend doing this, as, currently, the messages that pop up on the screen depend on the fixed length of string (such as the file's name). So if you want to open another image in the program, its bet to create a new one and rename it `images/original.txt`.

### Applying the filter

Once the image is loaded, it is possible to apply the different filters. Simply click on the one you want to apply and watch it as its loaded onto the right side of the display.

### Exiting 

By clicking exit, you leave the program and return to the DOSBox command line. You may run the program again by calling its executable `IMGFLTR.EXE`.