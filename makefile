DOSBOX = dosbox
CURRENT_DIR = $(shell pwd)
SET_PATH = "SET PATH=%PATH%;C:\TOOLS\FRASM"
ASM = imgfltr
BUILD = "BUILD $(ASM)"\
NASM = "NASM $(ASM)"

all:
	dosbox -c "MOUNT C: $(CURRENT_DIR)" -c "C:" -c $(SET_PATH) -c $(BUILD)

dos:
	dosbox -c "MOUNT C: $(CURRENT_DIR)" -c "C:" -c $(SET_PATH)

nasm:
	dosbox -c "MOUNT C: $(CURRENT_DIR)" -c "C:" -c $(SET_PATH) -c $(NASM)
