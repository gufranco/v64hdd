; Disassembly of v64_ver1.30
; Disassembled Fri May 30 17:06:59 1997
; New comments by OzOnE. Everything found manually using the Datasheets, so please don't laugh at some of the comments!
;
; UPDATE 26-1-08 -  OzOnE....
;
; !! Code can now load a ROM from HDD and finally play on the real N64 !!
;
; UPDATE 23-11-09 - OzOnE...
;
; !! Code can now (finally) read the FAT32 filesystem and will follow the cluster chains !!
; (This means you can easily put some ROMs on a HDD or Compact Flash card and there is no need to defrag!)
;
;
; WARNING: This compiled code does actually run on my V64, but I'm using an EPROM programmer to test the code using my (REMOVABLE) V64 flash chip.
; (on older V64's, the FLASH chip is under a blob which is epoxied directly to the V64 motherboard, so if something goes wrong with the code or the update,
; you might never be able to re-program the FLASH chip again and your V64 would be useless.)
;
; I can NOT guarantee that you will be able to update (or repair) your own V64 BIOS again if you decide to try this code on your V64 and something goes wrong!
;
; (In other words, it's safe for me to re-flash my chip if I need to using my EPROM programmer, but I have NOT properly tested the original V64 BIOS update code
; (via CD-R / parallel port).
;
;
; DISCLAIMER - !!!! If you use any of this code - I accept no responsibility for broken V64's / PC's / injured humans or pets / carpet burns / pointy hair / house fires etc. !!!! ;)
;
;
; ------- Much credit is due to Lac, Gaston, and all others for providing the code / tools / suggestions and inspiring this project. --------
;
; Note: First call to IDE routine starts after LD392 - (you can see where it jumps to IDE routine directly after Play button is pressed.)
;           (The label of this first call is "IDESt*rt")  - NOTE: I've missed out the "a" because it's easier for me to search the source.
;
; $DE on V64 is NTSC/PAL register, depending on switch position. 0=PAL, 1=NTSC

; LD167 WRITEPPU     -  Writes a hex value directly to the NEC 6450 PPU chip
; LD17D PRINTSTRING  -  Displays a text string from a following address with FFh as a termination char. X = ASCII lookup table ??
; LD2A1 BLINKON      -  Turns on PPU character blink. Call BLINKON, then print the string, Call BLINKOFF
; LD2A6 BLINKOFF     -  Cancels PPU character blink after printing the string.

; LD66B - Prints the string " Err " to the PPU and changes Rom banks ?
; LD67A - Prints the string " OK " to the PPU
; LD682 - Prints the string " Err " to the PPU (sets blink on first)
; LD3DA - Prints the string "Stop" to the PPU

; L944E DMRAMACCESS  - Requests DM Bus/Ram access from W9925P. 
; -----------------    After, put address High and Low bytes on DATAHIGH then MPEG_ADD respectively, then put data on MPEG_DPR0

; L945A DMRAMCANCEL  - Cancels DM Bus/Ram access after reading/writing is complete.

; LD543 WRITEMPEG    -  Writes to W9925P MPEG chip A=address, X=data
; LD534 WRITEMPEG16  -  Writes 16-bits to W9925P MPEG chip A=address, Y=high byte, X=low byte

; $A8 holds DRAM/Rom size depending on state of BACKUPMD ? (#$01 = 64Mbit, #$02 = 128Mbit, #$03 = 192Mbit, #$04 = 256Mbit) 

; $DD appears to reflect the state of BACKUPMD in RAM; #$00 = Normal mode, #$01 = Backup mode (read from cart?).

; LD07A 	- Prints game name in DRAM to PPU (needs starting position variables.)

;   ----------------------------------------------------------
;  |C000h  |    | V64STATUS                                   |
;  |       | RW | W bit 2-0    ROM BANK                       |
;  |       |    | R bit 7      IDE INT                        |
;  |       |    |   bit 6      MPEG INT (?)                   |
;  |       |    |   bit 1      N64 CART POWER(?)              |
;  |       |    |   bit 0      PPU READY (1=READY)            |
;  |----------------------------------------------------------|
;  |C001h  |    | W bit 7-0    PPU - WRITE TO PPU             | - NEC uPD6450GT (On-screen display chip - often used for camcorder viewfinders.)
;  |       |    | R            ?                              |
;  |----------------------------------------------------------|
;  |C002h  |    | RW           CARTDATA LOW ? (BACKUP MODE)   |
;  |----------------------------------------------------------|
;  |C003h  |    | RW           ?                              |
;  |----------------------------------------------------------|
;  |C004h  |    | RW        DRAMADD0 - DRAM / CART ADDRESS 0  |
;  |C005h  |    | RW        DRAMADD1 - DRAM / CART ADDRESS 1  |
;  |C006h  |    | RW        DRAMADD2 - DRAM / CART ADDRESS 2  |
;  |C007h  |    | RW        DRAMADD3 - DRAM / CART ADDRESS 3  |
;  |----------------------------------------------------------|
;  |C008h  |    | RW        Written to only by L9E50          |
;  |----------------------------------------------------------|
;  |C009h  |    | RW        Written to only by L9E50          |
;  |----------------------------------------------------------|
;  |C00Ah  |    | RW        Written to only by L9E50          |
;  |----------------------------------------------------------|
;  |C00Bh  |    | RW        Written to only by L9E50          |
;  |----------------------------------------------------------|
;  |C00Ch  |    | RW        Written to only by L9E50          |
;  |----------------------------------------------------------|
;  |C00Dh  |    | RW       IDEADD - IDE ADDRESS REGISTER      |
;  |----------------------------------------------------------|
;  |C00Eh  |    | RW       Set to 0x01 before "Verify" mode?? | Set to 0x02 for other tasks??
;  |----------------------------------------------------------|
;  |C00Fh  |    | W        Set to 00h for IDE / DRAM rd & wr  |
;  |C00Fh  |    |          Set to 01h for DRAM to MPEG ?????  |
;  |C00Fh  |    |          Set to 02h for CART reads to DRAM ?|
;  |       |    |          00h should enable game play from   |
;  |       |    |          DRAM, but doesn't work on Test :-( |
;  |----------------------------------------------------------|
;  |C010h  |    | RW       IDE DATA (LOW BYTE)                |
;  |----------------------------------------------------------|
;  |C011h  |    | RW       DRAM DATA (LOW BYTE)               |
;  |----------------------------------------------------------|
;  |C012h  |    | RW       Not used ??                        |
;  |----------------------------------------------------------|
;  |C013h  |    | RW       Not used ??                        |
;  |----------------------------------------------------------|
;  |C014h  |    | RW       DATAHIGH - IDE/DRAM/MPEG High byte |
;  |----------------------------------------------------------|
;  |C015h  |    | RW       PRINTER PORT DATA (?)              |
;  |----------------------------------------------------------|
;  |C016h  |    | RW       PRINTER PORT EXTRA PINS (?)        |
;  |----------------------------------------------------------|
;  |C017h  |    | RW       PRINTER PORT EXTRA PINS (?)        |
;  |----------------------------------------------------------|
;  |C018h  |    | RW       W9925P Mpeg Chip Address bus       | - AIR (Address Index Register)
;  |C019h  |    | RW       W9925P Mpeg Chip	Data (Low byte)   | - DPR0 (Data Port Register low byte) - high byte (DPR1) is mapped to $C014
;  |C01Ah  |    | RW       W9925P DRAM address 0 ?            | - DARL (DRAM Address Register Low)
;  |C01Bh  |    | RW       W9925P DRAM address 1 ?            | - DARH (DRAM Address Register High)
;  |C01Ch  |    | RW       W9925P DRAM Data low byte          | - DDPR (DRAM Data Port) 
;  |C01Dh  |    | RW       For sending MPEG data to chip      | - IDPR (Input-FIFO Data Port register) - for MPEG bitstream data input...
;  |C01Eh  |    | RW       Not used (?)                       | - MPEG_IDPR is also the WRR (Word Remaining Register) when read....
;  |C01Fh  |    | RW       Not used (?)                       | - The WRR bits 5:0 show the number of words currently in the input FIFO (ranges from 0 to 32)
;   ----------------------------------------------------------

; V64 specific register addresses.
V64STATUS	=		$C000     

PPU         =     	$C001	; NEC uPD6450GT (On-screen display chip - often used for camcorder viewfinders.)

RESET       =     	$FFFC	; Reset vector of 65C02 CPU

IDEADD      =     	$C00D	; Address for selecting IDE Registers
IDELOW      =     	$C010	; IDE data Low byte

DRAMADD0	=     	$C004	; Address for DRAM Reads/Writes
DRAMADD1	=		$C005	; Address for DRAM Reads/Writes
DRAMADD2	=     	$C006	; Address for DRAM Reads/Writes
DRAMADD3	=     	$C007	; Address for DRAM Reads/Writes

DRAMLOW     =     	$C011	; DRAM data Low byte (Muxed with CARTLOW?)

DATAHIGH	=		$C014	; DRAM data High byte for all DRAM/CART/IDE Transfers

CARTADD		=		$C004	; Address for CARTRIDGE Reads/Write (4 Bytes)
CARTLOW     =     	$C011	; Cartridge data Low byte (Muxed with DRAMLOW?)

MPEG_ADD	=		$C018	; W9925P Mpeg chip address bus
MPEG_DPR0	=		$C019	; W9925P Mpeg chip data

MPEG_DARL	=		$C01A
MPEG_DARH	=		$C01B
MPEG_DDPR	=		$C01C	; - DDPR (DRAM Data Port)
MPEG_IDPR	=		$C01D 	; - IDPR (Input-FIFO Data Port register) - for MPEG bitstream data input.

BACKUPMD    =		$C00F	; Set to 00h for IDE Reads/Writes, 01h for DRAM Enable and 02h for Backup Enable (read from Cart to DRAM ?)

; HDD access registers
LBA0		=		$20		; 28-bit LBA Address (least-significant byte first - 4 Bytes used)
LBA1		=		$21
LBA2		=		$22
LBA3		=		$23

IDELBAF		=		$24		; Flag: 1 = drive supports LBA

IDESECC		=		$25		; IDE Sector Count
NEXTADD		=		$26
NEXTADD1	=		$27

TEMP		=		$28
TEMP2		=		$29

; File on-screen menu stuff
XPOS		=		$2a
YPOS		=		$2b

TOTALFILES	=		$2c		; Counter for total number of FILEs / ROMs found on the HDD (FAT32)

ARROWPOS	=		$2d		; Selection arrow Y position (doesn't have to follow FILEINDEX, so menu can be moved around)

FILEINDEX	=		$2e		; Index to selected FILE / ROM on screen (starting from one)

PAGEMIN		=		$2f		; Lowest FILEINDEX number displayed on current page.
PAGEMAX		=		$30		; Highest FILEINDEX number displayed on current page.
PAGE		=		$31		; Current PAGE number (on screen)
OLDPAGE		=		$32		; Previous page number displayed on screen (so we don't keep reading all the file entries)

BACKUP		=		$33		; Backup reg for temp storage

HDDMODE		=		$34		; Flag, for switching between HDD and CD mode

LOOPCOUNT	=		$35

TEMP3		=		$36

; Storage for FAT32 BPB params.
SECT_PER_CLUS	=		$35
NUM_RESV_SECT0	=		$36
NUM_RESV_SECT1	=		$37
NUM_FATS		=		$38
SECT_PER_FAT0	=		$39
SECT_PER_FAT1	=		$3a
SECT_PER_FAT2	=		$3b
SECT_PER_FAT3	=		$3c
ROOT_DIR_FIRST_CLUS0	=		$3d
ROOT_DIR_FIRST_CLUS1	=		$3e
ROOT_DIR_FIRST_CLUS2	=		$3f
ROOT_DIR_FIRST_CLUS3	=		$40

; Storage for other "calculated" (or extracted) FAT32 params
PART_BEGIN_LBA0	=		$41		; Storage for LBA of Volume ID sector
PART_BEGIN_LBA1	=		$42
PART_BEGIN_LBA2	=		$43
PART_BEGIN_LBA3	=		$44
FAT_BEGIN_LBA0	=		$45		; Storage for start LBA of FAT cluster chain entries
FAT_BEGIN_LBA1	=		$46
FAT_BEGIN_LBA2	=		$47
FAT_BEGIN_LBA3	=		$48

CLUS_BEGIN_LBA0	=		$49		; Storage for LBA of first sector for the file/folder entries (ie. ROOT dir start SECTOR)
CLUS_BEGIN_LBA1	=		$4a
CLUS_BEGIN_LBA2	=		$4b
CLUS_BEGIN_LBA3	=		$4c

FILE_START_CLUS0 =		$4d		; Storage for the start cluster number of the current file
FILE_START_CLUS1 =		$4e
FILE_START_CLUS2 =		$4f
FILE_START_CLUS3 =		$50

FILE_SIZE0		=		$51
FILE_SIZE1		=		$52
FILE_SIZE2		=		$53
FILE_SIZE3		=		$54

; File loading stuff (FAT32)
FILE_NUM		=		$55

ENTRY_INDEX0	=		$56
ENTRY_INDEX1	=		$57

CURRENT_CLUS0	=		$58
CURRENT_CLUS1	=		$59
CURRENT_CLUS2	=		$5a
CURRENT_CLUS3	=		$5b

CLUS_LBA0		=		$5c
CLUS_LBA1		=		$5d
CLUS_LBA2		=		$5e
CLUS_LBA3		=		$5f

NEXT_CLUS0		=		$60
NEXT_CLUS1		=		$61
NEXT_CLUS2		=		$62
NEXT_CLUS3		=		$63

SECT_COUNT0		=		$64
SECT_COUNT1		=		$65
SECT_COUNT2		=		$66
SECT_COUNT3		=		$67

TEMP_0			=		$68
TEMP_1			=		$69
TEMP_2			=		$6a
TEMP_3			=		$6b

ENTRY_ATTRIB	=		$6c
ENTRY_TYPE		=		$6d

FILECOUNTER		=		$6e

FILE_NAME		=		$6f		; File name / file extension registers must be kept continuous !
								; FILE_NAME also uses $70, $71, $72 ,$73, $74, $75, $76.
FILE_EXT		=		$77		; FILE_EXT also uses $78, $79.

C00B_SPY		=		$7a
C00A_SPY		=		$7b
C009_SPY		=		$7c
C008_SPY		=		$7d

ROM_REGION		=		$7e

IDEBUF0		=		$0480	; First half of buffer for data to/from IDE device - 512 Bytes total.
IDEBUF1		=		$0680

				processor 6502

				.ORG	$8000

L8000:			.byte		"Reading... "
				.byte		$FF

L800C:			.byte		"64M"
				.byte		$FF

L8010:			.byte		"128M"
				.byte		$FF

L8015:			.byte		"192M"
				.byte		$FF

L801A:			.byte		"256M"
				.byte		$FF

L801F:			.byte		"Now Backup Game Card"
				.byte		$FF

L8034:			.byte		"Power for Backup error."
				.byte		$FF

WaitingMSG:		.byte		"Waiting for HDD...    "
				.byte		$FF
				
ReadyMSG:		.byte		"HDD Ready             "
				.byte		$FF

DriveErrorMSG:	.byte 		"HDD Error: "
				.byte		$FF
				
LBAHeadMSG:		.byte		"LBA"
				.byte		$FF
				
TooManyMSG:		.byte 		"No files, or more than 255 files!"
				.byte		$FF

;PlayMPEGMSG:	.byte 		"MPEG File - Press PLAY to view"
;				.byte		$FF
				
L804C:			LDA			#$01
				BNE			L805A

L8050:			LDA			#$02
				BNE			L805A

L8054:			LDA			#$03
				BNE			L805A

L8058:			LDA			#$04

L805A:			STA			$A8		; Sets CART/DRAM size (#$01 = 64Mbit, #$02 = 128Mbit, #$03 = 192Mbit, #$04 = 256Mbit)
				JMP			L8090

L805F:			JSR			BLANKPPU
				JSR			LDFC1
				BEQ			L806A
				JMP			PRINT_STOP

L806A:			LDA			V64STATUS
				AND			#$02		; Test for N64 cart power, skip error message if power on?
				BEQ			L8084

L8071:			JSR			BLINKON
				JSR			PRINTSTRING

				.byte		$00, $0A
				.word		L8034			; ...else: Display "Power for Backup error."

				JSR			BLINKOFF
				LDA			#$00
				STA			BACKUPMD	; Zero BACKUP MODE
				RTS

L8084:			LDA			#$01
				STA			$A8
				LDA			$8C
				AND			#$40
				BEQ			L8090
				INC			$A8

L8090:			LDA			#$02
				STA			BACKUPMD	; Set BACKUPMD to #$02 (Read from cart?)
				LDX			#$4A
				JSR			LD0AE		; Test BIT $2002 (X times?)
				LDA			V64STATUS
				AND			#$02		; Test for N64 cart power on
				BEQ			L8071		; Print error message if power ON ???
				JSR			PRINTSTRING

				.byte		$02, $06
				.word		L801F			; "Now Backup Game Card"

				JSR			BLINKON
				JSR			PRINTSTRING

				.byte		$00, $0A
				.word		L8000			; "Reading... "

				LDA			$A8		; Check cart size register ?
				CMP			#$01
				BEQ			L80C8		; Display "64M" if $A8 = 1... Set X=#$08, Y=#$00
				CMP			#$02
				BEQ			L80D6		; Display "128M" if $A8 = 2... Set X=#$98, Y=#$00
				CMP			#$03
				BEQ			L80E4		; Display "192M" if $A8 = 3... Set X=#$98, Y=#$0A
				CMP			#$04
				BEQ			L80F2		; Display "256M" if $A8 = 4... Set X=#$98, Y=#$BA
				LDA			#$01
				STA			$A8

L80C8:			JSR			PRINTSTRING

				.byte		$00, $80
				.word		L800C		; "64M"

				LDX			#$08
				LDY			#$00
				JMP			L80FD

L80D6:			JSR			PRINTSTRING

				.byte		$00, $80
				.word		L8010		; "128M"

				LDX			#$98
				LDY			#$00
				JMP			L80FD

L80E4:			JSR			PRINTSTRING

				.byte		$00, $80
				.word		L8015		; "192M"

				LDX			#$98
				LDY			#$0A
				JMP			L80FD

L80F2:			JSR			PRINTSTRING

				.byte		$00, $80
				.word		L801A		; "256M"

				LDX			#$98
				LDY			#$BA

L80FD:			STX			$C002		; Store (Cart size address high?) to $C002
				STX			$A7		; Store (Cart size address high?) to $A7
				STY			$C003		; Store (Cart size address low?) to $C003
				STY			$A9		; Store (Cart size address low?) to $A9
				JSR			BLINKOFF
				LDA			#$00		; Load A with #$00
				STA			DRAMADD1	; Zero DRAM Addresses
				STA			DRAMADD2	; Zero DRAM Addresses
				STA			DRAMADD3	; Zero DRAM Addresses

				LDA			#$00
				STA			$B0		; Set $B0 to 00h
				LDA			#$08
				STA			$E5		; Set $E5 to 08h

				JSR			L8147		; Jump to L8147 (Print game name in CART on screen).

L8120:			LDY			$B0		; Load Y with $B0 (Starts at 00h)
				LDA			#$80
				STA			DRAMADD0,Y	; Set DRAMADD0 to 80h, Y offset

				LDA			#$40
				STA			$14		; Start $14 at 40h (number of times to repeat cart read - 64 in decimal)?

				JSR			L81B2	; Zero $16, $12, $13. Then: Updates variables (L9E50), Read from $C002 256 times
									; ,inc $12 ($13 increments after $12 wraps to 00h), repeat until $14 decrements to 00h, return.
									; Note - as $14 starts at 64 (dec), it will read 16384 times from $C002									

				INC			$E5		; Increment $E5
				INC			$B0		; Increment $B0 (DRAMADD0 offset)
				DEC			$A8
				BNE			L8120	; Repeat read until $A8 decrements to 00h ($A8 starts at 1 for 64M)

				LDX			#$20	; Note, this means that for a 64M cart, 
				JSR			PRINTSTRING

				.byte		$00, $06
				.word		$0098

				LDA			#$00
				STA			BACKUPMD	; Leave BACKUP Mode
				JMP			L817A		; Jump, to display game name in DRAM etc.

L8147:			LDA			#$08		; ****** Displays cart name (from actual CART itself in backup slot) on screen
				STA			$E5		; Set $E5 to #$08
				LDA			#$10
				STA			$16		; Set $16 to #$10 (start offset / 2 ?)
				LDA			#$00
				STA			$12		; Set $12 to #$00
				STA			$13		; Set $13 to #$00
				STA			$0A		; Set $0A to #$00
				JSR			L9E50		; Puts variables into C008,9,A,B,C ?
				LDX			#$20
				JSR			PRINTSTRING

				.byte		$01, $08
				.word		$0081

L8163:			LDA			$C003		; Load A with $C003	(Cart DATA low) ??
				PHA					; Push $C003 to stack

				LDA			DATAHIGH	; Load A with DATAHIGH
				JSR			PRINTASCII		; Jump to ASCII compare table then WRITE to PPU

				PLA					; Pull $C003 from stack
				JSR			PRINTASCII		; Jump to ASCII compare table then WRITE to PPU

				INC			$0A		; Increment $0A
				LDA			$0A		; Load A with $0A
				CMP			#$0A
				BNE			L8163		; Keep printing DATAHIGH then $C003 to screen, until $0A reaches #$0A
				RTS

L817A:			JSR			LD05E		; ******* Displays cart name (from DRAM) on screen.
				LDA			#$00
				STA			$DC
				STA			$E5
				STA			$16
				JSR			L9E50		; Puts variables into C008,9,A,B,C ?
				LDA			#$01
				STA			$DD

;				JSR			SHOW_DRAM_HEX		; TESTING !!!!!!!!!!!!!!

				JSR			BLINKON
				JSR			PRINTSTRING

				.byte		$00, $0A
				.word		L8199			; "<You Can Play Game Now.>"

				JMP			BLINKOFF		; As this is a JMP, the RTS after BLINKOFF will return to whichever called THIS routine !!!

L8199:			.byte		"<You Can Play Game Now.>"
				.byte		$FF

L81B2:			LDY			#$00
				STY			$16
				STY			$12
				STY			$13

L81BA:			JSR			L9E50		; Puts variables into C008,9,A,B,C ?

L81BD:			LDA			$C002		; Read from $C002 32 times (loop is run 8 times, so 256 reads until Y wraps to 00h)
				LDA			$C002		; In backup mode, the V64 appears to transfer a word into DRAM automatically each
				LDA			$C002		; time $C002 (cart?) is read.
				LDA			$C002		;This allows for faster transfer, as it's not necessary to write back to DRAM.
				LDA			$C002		; Since this loop (L81BD) is read 256 times, it transfers 256 words = 512 bytes =1 "sector"
				LDA			$C002		; of the cart into DRAM before $12 is incremented.
				LDA			$C002
				LDA			$C002
				LDA			$C002
				LDA			$C002
				LDA			$C002
				LDA			$C002
				LDA			$C002
				LDA			$C002
				LDA			$C002
				LDA			$C002
				LDA			$C002
				LDA			$C002
				LDA			$C002
				LDA			$C002
				LDA			$C002
				LDA			$C002
				LDA			$C002
				LDA			$C002
				LDA			$C002
				LDA			$C002
				LDA			$C002
				LDA			$C002
				LDA			$C002
				LDA			$C002
				LDA			$C002
				LDA			$C002
				TYA						; Transfer Y into A (Y starts at 00h before loop)
				CLC						; Clear carry flag
				ADC			#$20		; Add #$20 to Y (32 in decimal)
				TAY						; Transfer A back into Y
				BEQ			L8227		; NOTE: This means that it will do this loop (L81BD) 8 times (256 reads of $C002)
				JMP			L81BD		; then, after 256 reads - increments $12, updates pointers (L9E50), and repeats....
										; If $12 wraps to 00h - inc $13, dec $14, update pointers (L9E50) and repeats
										; until $14 decrements to 00h.

L8227:			INC			$12
				BEQ			L822E

L822B:			JMP			L81BA

L822E:			INC			$13
				DEC			$14
				BNE			L822B
				RTS

L8235:			.byte		$01,$02,$04,$08
L8239:			.byte		$AA
L823A:			.byte		$55,$D5,$2A,$6A,$95,$B5,$4A
L8241:			.byte		"   < DRAM FAST TEST >   "
				.byte		$FF

L825A:			JSR			PRINTSTRING

				.byte		$00, $03
				.word		L8241			; "   < DRAM FAST TEST >   "

				LDA			$4017
				AND			#$01
				STA			$0D
				LDA			#$04
				STA			$A8
				LDA			#$21
				STA			$C002
				LDA			#$84
				STA			$C003
				LDA			#$00
				STA			$BF
				STA			DRAMADD0
				STA			DRAMADD1
				STA			DRAMADD2
				STA			DRAMADD3
				STA			$B0

L8288:			LDA			#$04
				JSR			L885A
				LDA			$B0
				ASL
				TAY
				LDA			L8239,Y
				STA			$0B
				LDA			L823A,Y
				STA			$0A
				JSR			PRINTSTRING

				.byte		$00, $80
				.word		LD566			; "Writing..."

				JSR			PRINTSTRING

				.byte		$00, $80
				.word		L898D			; "DRAM"

				LDA			$B0
				ORA			#$30
				JSR			PRINTASCII
				LDY			$B0
				LDA			#$80
				STA			DRAMADD0,Y
				LDA			L8235,Y
				STA			$E5
				LDY			#$00
				STY			$16
				STY			$12
				STY			$13
				JSR			L9E50		; Puts variables into C008,9,A,B,C ?

				LDA			$0B
				STA			DATAHIGH
				LDA			$0A
L82CE:			STA			DRAMLOW
				INC			DATAHIGH
				SEC
				SBC			#$01
				INY
				BNE			L82CE
				LDX			DATAHIGH
				INX
				STX			$0B
				TAX
				DEX
				STX			$0A
				INC			$12

L82E6:			JSR			L9E50		; Puts variables into C008,9,A,B,C ?
				LDA			$0B
				STA			DATAHIGH
				LDA			$0A
				STA			DRAMLOW
				INC			$0B
				DEC			$0A
				INC			$12
				BNE			L82E6
				INC			$0B
				DEC			$0A
				LDA			$0B
				LSR
				ROR			$0A
				ROR			$0B
				INC			$13
				LDX			$13
				CPX			#$40
				BCC			L82E6
				LDY			$B0
				LDA			#$00
				STA			DRAMADD0,Y
				INC			$B0
				LDA			$B0
				CMP			$A8
				BEQ			L8320
				JMP			L8288

L8320:			LDX			#$20
				JSR			PRINTSTRING

				.byte		$00, $03
				.word		$00B0

				LDA			#$00
				STA			$B0

L832D:			LDA			#$03
				CLC
				ADC			$B0
				JSR			L885A
				LDA			$B0
				ASL
				TAY
				LDA			L8239,Y
				STA			$0B
				LDA			L823A,Y
				STA			$0A
				JSR			PRINTSTRING

				.byte		$00, $80
				.word		LD573			; "Verify "

				JSR			PRINTSTRING

				.byte		$00, $80
				.word		L898D			; "DRAM"

				LDA			$B0
				ORA			#$30
				JSR			PRINTASCII
				LDY			$B0
				LDA			#$80
				STA			DRAMADD0,Y
				LDA			L8235,Y			; Point to array of #$01, #$02, #$04, #$08
				STA			$E5				; Store array byte to $E5
				LDY			#$00
				STY			$16
				STY			$12
				STY			$13
				STY			$0100
				STY			$0101
				STY			$0102
				STY			$0103
				JSR			L9E50		; Puts variables into C008,9,A,B,C ?
				LDX			$0B
				LDA			$0A

L837F:			CMP			DRAMLOW
				BNE			L839C
				CPX			DATAHIGH
				BNE			L839C

L8389:			INX
				SEC
				SBC			#$01
				INY
				BNE			L837F
				INX
				STX			$0B
				TAX
				DEX
				STX			$0A
				INC			$12
				JMP			L83AA

L839C:			JSR			L88AC
				BNE			L83EF
				LDA			$A2
				LDX			$A3
				LDY			$A4
				JMP			L8389

L83AA:			JSR			L9E50		; Puts variables into C008,9,A,B,C ?
				LDX			$0B
				LDA			$0A
				CMP			DRAMLOW
				BNE			L83EA
				CPX			DATAHIGH
				BNE			L83EA

L83BB:			INC			$0B
				DEC			$0A
				INC			$12
				BNE			L83AA
				INC			$0B
				DEC			$0A
				LDA			$0B
				LSR
				ROR			$0A
				ROR			$0B
				INC			$13
				LDX			$13
				CPX			#$40
				BCC			L83AA
				LDA			$0100
				ORA			$0101
				ORA			$0102
				ORA			$0103
				BNE			L83EF
				JSR			PRINT_OK
				JMP			L83EF

L83EA:			JSR			L88AC
				BEQ			L83BB

L83EF:			LDY			$B0
				LDA			#$00
				STA			DRAMADD0,Y
				INC			$B0
				LDA			$B0
				CMP			$A8
				BEQ			L8401
				JMP			L832D

L8401:			LDA			$8C
				CMP			#$D0
				BNE			L840E
				LDA			#$02
				STA			$A8
				JMP			L8463

L840E:			CMP			#$C1
				BNE			L8419
				LDA			#$04
				STA			$A8
				JMP			L8463

L8419:			JSR			READKEYPAD
				LDA			$8C
				CMP			#$86				; If you hold FR, NEXT, and MENU buttons after self test, "Copy Right 1997 Bung" etc is displayed !!?
				BNE			L842E
				BNE			L8463
				JSR			BLANKPPU
				JSR			PRINTSTRING

				.byte		$00, $45
				.word		L8944				; Points to array of bytes - is actually "Copy Right 1997 Bung..." etc banner text !!? (bit of an easter egg I suppose)

L842E:			JMP			L851E

L8431:			.byte		"< Fully 256M Testing >  "
				.byte		$FF

L844A:			.byte		"< Fully 128M Testing >  "
				.byte		$FF

L8463:			JSR			LD2AB
				LDX			#$20
				JSR			PRINTSTRING

				.byte		$00, $03
				.word		$00E0

				LDA			$A8
				CMP			#$04
				BNE			L847F
				JSR			PRINTSTRING

				.byte		$00, $03
				.word		L8431			; "< Fully 256M Testing >  "

				JMP			L8486

L847F:			JSR			PRINTSTRING

				.byte		$00, $03
				.word		L844A			; "< Fully 128M Testing >  "

L8486:			LDA			#$22
				STA			$C002
				STA			$C003
				LDA			#$02
				STA			$E5
				LDA			#$FF
				STA			$BF
				LDY			#$00
				LDA			#$00

L849A:			STA			$0200,Y
				INY
				CPY			#$10
				BNE			L849A
				LDA			#$00
				STA			$C1
				STA			$C4
				LDA			#$FF
				STA			$C2
				STA			$C3
				JSR			L853A
				LDA			#$FF
				STA			$C1
				STA			$C4
				LDA			#$00
				STA			$C2
				STA			$C3
				JSR			L853A
				LDA			#$80
				STA			$BF
				JSR			L8586
				LDA			#$00
				STA			$B0

L84CB:			JSR			L8866
				LDA			$B0
				ASL
				ASL
				STA			$00
				LDA			#$02
				STA			$01
				LDY			#$00
				LDA			($00),Y
				INY
				ORA			($00),Y
				INY
				ORA			($00),Y
				INY
				ORA			($00),Y
				BNE			L84ED
				JSR			PRINT_OK
				JMP			L850C

L84ED:			JSR			PRINT_ERR
				LDA			#$00
				STA			$10

L84F4:			LDY			$10
				LDA			($00),Y
				BEQ			L8504
				LDX			$10
				INX
				JSR			PRINTSTRING

				.byte		$00, $C0
				.word		$0081

L8504:			INC			$10
				LDA			$10
				CMP			#$04
				BNE			L84F4

L850C:			INC			$B0
				LDA			$B0
				CMP			$A8
				BEQ			L8517
				JMP			L84CB

L8517:			JSR			PRINTSTRING

				.byte		$11, $06
				.word		L8524			; " Done "

L851E:			JSR			L982E			; Parallel port stuff ??? (preliminary)
				JMP			L851E			; Loop ?

L8524:			.byte		" Done "
				.byte		$FF

L852B:			LDY			#$00
				LDA			#$FF

L852F:			AND			$0200,Y
				INY
				CPY			#$10
				BNE			L852F
				CMP			#$00
				RTS

L853A:			JSR			L852B
				BEQ			L8540
				RTS

L8540:			LDA			#$00
				STA			$B0
				LDA			#$80
				STA			DRAMADD0
				STA			DRAMADD1
				STA			DRAMADD2
				STA			DRAMADD3
				JSR			L85C2
				LDA			#$00
				STA			DRAMADD0
				STA			DRAMADD1
				STA			DRAMADD2
				STA			DRAMADD3
				JSR			L8707

L8566:			JSR			L8615
				INC			$B0
				LDA			$B0
				CMP			$A8
				BNE			L8566
				LDX			#$26
				JSR			LD0AE
				LDX			#$20
				JSR			PRINTSTRING

				.byte		$00, $03
				.word		$00E0

				LDA			#$00
				STA			$B0
				JMP			L86FF

L8586:			JSR			L852B
				BEQ			L858C
				RTS

L858C:			LDA			#$00
				STA			$B0
				LDA			#$80
				STA			DRAMADD0
				STA			DRAMADD1
				STA			DRAMADD2
				STA			DRAMADD3
				JSR			L8716
				LDA			#$00
				STA			DRAMADD0
				STA			DRAMADD1
				STA			DRAMADD2
				STA			DRAMADD3
				JSR			L8707

L85B2:			JSR			L876E
				INC			$B0
				LDA			$B0
				CMP			$A8
				BNE			L85B2
				LDX			#$26
				JMP			LD0AE

L85C2:			JSR			PRINTSTRING

				.byte		$00, $80
				.word		LD566			; "Writing... "

				JSR			PRINTSTRING

				.byte		$00, $80
				.word		L898D			; "DRAM"

				JSR			PRINTSTRING

				.byte		$00, $80
				.word		L8992			; ":"

				LDX			$C1
				JSR			PRINTSTRING

				.byte		$00, $80
				.word		$0001

				LDY			#$00
				STY			$16
				STY			$12
				STY			$13

L85E8:			JSR			L9E50		; Puts variables into C008,9,A,B,C ?

L85EB:			LDA			$C2
				STA			DATAHIGH
				LDA			$C1
				STA			DRAMLOW
				LDA			$C4
				STA			DATAHIGH
				LDA			$C3
				STA			DRAMLOW
				INY
				INY
				BNE			L85EB
				INC			$12
				BNE			L85E8
				INC			$13
				LDX			$13
				CPX			#$40
				BCC			L85E8
				RTS

L8610:			.byte		"1234"
				.byte		$FF

L8615:			JSR			L86FF
				JSR			PRINTSTRING

				.byte		$00, $80
				.word		LD573			; "Verify "

				JSR			PRINTSTRING

				.byte		$00, $80
				.word		L898D			; "DRAM"

				LDA			$B0
				ORA			#$30
				JSR			PRINTASCII
				JSR			PRINTSTRING

				.byte		$00, $80
				.word		L8992			; ":"

				LDX			$C1
				JSR			PRINTSTRING

				.byte		$00, $80
				.word		$0001

				LDA			$B0
				ASL
				ASL
				STA			$00
				LDA			#$02
				STA			$01
				LDA			#$FF
				LDY			#$00

L864B:			AND			($00),Y
				INY
				CPY			#$04
				BNE			L864B
				CMP			#$00
				BEQ			L8661
				JSR			PRINT_ERR
				JSR			PRINTSTRING

				.byte		$00, $80
				.word		L8610			; "1234"

				RTS

L8661:			LDA			#$80
				LDY			$B0
				STA			DRAMADD0,Y
				LDA			#$00
				STA			$0100
				STA			$0101
				STA			$0102
				STA			$0103
				LDY			#$00
				STY			$16
				STY			$12
				STY			$13

L867E:			JSR			L9E50		; Puts variables into C008,9,A,B,C ?
				LDA			$4017
				AND			#$01
				CMP			$0D
				BEQ			L868F
				STA			$0D
				JMP			L86DF

L868F:			LDA			$C1
				CMP			DRAMLOW
				BNE			L86D1
				LDX			$C2
				CPX			DATAHIGH
				BNE			L86D1

L869D:			INY
				LDA			$C3
				CMP			DRAMLOW
				BNE			L86D1
				LDX			$C4
				CPX			DATAHIGH
				BNE			L86D1

L86AC:			INY
				BNE			L868F
				INC			$12
				BNE			L867E
				INC			$13
				LDX			$13
				CPX			#$40
				BCC			L867E
				LDA			$0100
				ORA			$0101
				ORA			$0102
				ORA			$0103
				AND			#$80
				BNE			L86DF
				JSR			PRINT_OK
				JMP			L86DF

L86D1:			JSR			L88AC
				BNE			L86DF
				LDY			$A4
				TYA
				AND			#$01
				BEQ			L869D
				BNE			L86AC

L86DF:			LDA			#$00
				LDY			$B0
				STA			DRAMADD0,Y
				LDA			$B0
				ASL
				ASL
				STA			$00
				LDA			#$02
				STA			$01
				LDY			#$00

L86F2:			LDA			$0100,Y
				ORA			($00),Y
				STA			($00),Y
				INY
				CPY			#$04
				BNE			L86F2
				RTS

L86FF:			LDA			#$03
				CLC
				ADC			$B0
				JMP			L885A

L8707:			JSR			L86FF
				LDX			#$20
				JSR			PRINTSTRING

				.byte		$00, $80
				.word		$0098

				JMP			L86FF

L8716:			LDA			L8239
				STA			$0B
				LDA			L823A
				STA			$0A
				JSR			PRINTSTRING

				.byte		$00, $80
				.word		LD566			; "Writing... "

				JSR			PRINTSTRING

				.byte		$00, $80
				.word		L898D			; "DRAM"

				LDY			#$00
				STY			$16
				STY			$12
				STY			$13

L8736:			JSR			L9E50		; Puts variables into C008,9,A,B,C ?
				LDA			$0B
				STA			DATAHIGH
				LDA			$0A

L8740:			STA			DRAMLOW
				INC			DATAHIGH
				SEC
				SBC			#$01
				INY
				BNE			L8740
				LDX			DATAHIGH
				INX
				STX			$0B
				TAX
				DEX
				STX			$0A
				INC			$12
				BNE			L8736
				INC			$0B
				DEC			$0A
				LDA			$0B
				LSR
				ROR			$0A
				ROR			$0B
				INC			$13
				LDX			$13
				CPX			#$40
				BCC			L8736
				RTS

L876E:			JSR			L86FF
				JSR			PRINTSTRING

				.byte		$00, $80
				.word		LD573			; "Verify "

				JSR			PRINTSTRING

				.byte		$00, $80
				.word		L898D			; "DRAM"

				LDA			$B0
				ORA			#$30
				JSR			PRINTASCII
				LDA			$B0
				ASL
				ASL
				STA			$00
				LDA			#$02
				STA			$01
				LDA			#$FF
				LDY			#$00

L8794:			AND			($00),Y
				INY
				CPY			#$04
				BNE			L8794
				CMP			#$00
				BEQ			L87AA
				JSR			PRINT_ERR
				JSR			PRINTSTRING

				.byte		$00, $80
				.word		L8610			; "1234"

				RTS

L87AA:			LDA			#$80
				LDY			$B0
				STA			DRAMADD0,Y
				LDA			#$00
				STA			$0100
				STA			$0101
				STA			$0102
				STA			$0103
				LDA			L8239
				STA			$0B
				LDA			L823A
				STA			$0A
				LDY			#$00
				STY			$16
				STY			$12
				STY			$13

L87D1:			JSR			L9E50		; Puts variables into C008,9,A,B,C ?
				LDA			$4017
				AND			#$01
				CMP			$0D
				BEQ			L87E2
				STA			$0D
				JMP			L883A

L87E2:			LDX			$0B
				LDA			$0A

L87E6:			CMP			DRAMLOW
				BNE			L882C
				CPX			DATAHIGH
				BNE			L882C

L87F0:			INX
				SEC
				SBC			#$01
				INY
				BNE			L87E6
				INX
				STX			$0B
				SEC
				SBC			#$01
				STA			$0A
				INC			$12
				BNE			L87D1
				INC			$0B
				DEC			$0A
				LDA			$0B
				LSR
				ROR			$0A
				ROR			$0B
				INC			$13
				LDX			$13
				CPX			#$40
				BCC			L87D1
				LDA			$0100
				ORA			$0101
				ORA			$0102
				ORA			$0103
				AND			#$80
				BNE			L883A
				JSR			PRINT_OK
				JMP			L883A

L882C:			JSR			L88AC
				BNE			L883A
				LDA			$A2
				LDX			$A3
				LDY			$A4
				JMP			L87F0

L883A:			LDA			#$00
				LDY			$B0
				STA			DRAMADD0,Y
				LDA			$B0
				ASL
				ASL
				STA			$00
				LDA			#$02
				STA			$01
				LDY			#$00

L884D:			LDA			$0100,Y
				ORA			($00),Y
				STA			($00),Y
				INY
				CPY			#$04
				BNE			L884D
				RTS

L885A:			AND			#$0F
				ORA			#$90
				JSR			WRITEPPU
				LDA			#$A0
				JMP			WRITEPPU

L8866:			LDA			#$03
				LDX			$BF
				BMI			L886E
				LDA			#$03

L886E:			CLC
				ADC			$B0
				AND			#$0F
				ORA			#$90
				PHA
				JSR			WRITEPPU
				BIT			$BF
				BVC			L888F
				LDA			#$AF
				PHA
				JSR			WRITEPPU
				LDX			#$20
				JSR			PRINTSTRING

				.byte		$00, $80
				.word		$0089

				JMP			L889E

L888F:			LDA			#$AC
				PHA
				JSR			WRITEPPU
				LDX			#$20
				JSR			PRINTSTRING

				.byte		$00, $80
				.word		$008C

L889E:			PLA
				TAX
				PLA
				TAY
				TXA
				PHA
				TYA
				JSR			WRITEPPU
				PLA
				JMP			WRITEPPU

L88AC:			STA			$06
				STA			$A2
				STX			$07
				STX			$A3
				STY			$16
				STY			$A4
				LDA			$0100
				ORA			$0101
				ORA			$0102
				ORA			$0103
				AND			#$80
				BNE			L88CE
				JSR			L8866
				JSR			PRINT_ERR

L88CE:			JSR			L9E50		; Puts variables into C008,9,A,B,C ?
				LDA			DRAMLOW
				EOR			$06
				STA			$06
				LDA			DATAHIGH
				EOR			$07
				STA			$07
				LDA			$06
				AND			#$1D
				BEQ			L88F4
				LDA			$0100
				BMI			L88F4
				LDA			#$31
				JSR			PRINTASCII
				LDA			#$FF
				STA			$0100

L88F4:			LDA			$06
				AND			#$E2
				BEQ			L8909
				LDA			$0101
				BMI			L8909
				LDA			#$32
				JSR			PRINTASCII
				LDA			#$FF
				STA			$0101

L8909:			LDA			$07
				AND			#$1D
				BEQ			L891E
				LDA			$0102
				BMI			L891E
				LDA			#$33
				JSR			PRINTASCII
				LDA			#$FF
				STA			$0102

L891E:			LDA			$07
				AND			#$E2
				BEQ			L8933
				LDA			$0103
				BMI			L8933
				LDA			#$34
				JSR			PRINTASCII
				LDA			#$FF
				STA			$0103

L8933:			LDA			#$00
				STA			$16
				LDA			$0100
				AND			$0101
				AND			$0102
				AND			$0103
				RTS

; Array at L8944 is actually "Copy Right 1997 Bung Enterprises Limited HONG  KONG" !!! (appears if FR, NEXT, and MENU are held down after self test has finished!?)
L8944:			.byte		$7F,$7F,$7F,$7F,$13,$5F,$60,$69,$7F,$22,$59,$57,$58,$64,$7F,$01
				.byte		$09,$09,$07,$7F,$7F,$7F,$7F,$7F,$12,$65,$5E,$57,$7F,$15,$5E,$64
				.byte		$55,$62,$60,$62,$59,$63,$55,$63,$7F,$1C,$59,$5D,$59,$64,$55,$54
				.byte		$7F,$7F,$7F,$7F,$7F,$7F,$7F,$18,$5F,$5E,$57,$7F,$7F,$1B,$5F,$5E
				.byte		$57,$7F,$7F,$7F,$7F,$7F,$7F,$7F
				.byte		$FF

L898D:			.byte		"DRAM"
				.byte		$FF

L8992:			.byte		":"
				.byte		$FF

L8994:			JSR			PRINTSTRING

				.byte		$00, $07
				.word		L8AB7			; "SRAM1 "

				LDY			#$00
				STY			$00
				LDA			#$02
				STA			$01
				LDA			#$AA
				STA			$11
				LDA			#$55
				STA			$10
				LDX			#$06

L89AD:			LDA			$11
				STA			($00),Y
				INY
				LDA			$10
				STA			($00),Y
				DEC			$11
				INC			$10
				INY
				BNE			L89AD
				DEC			$11
				INC			$10
				INC			$01
				DEX
				BNE			L89AD
				LDA			#$02
				STA			$01
				LDA			#$AA
				STA			$11
				LDA			#$55
				STA			$10
				LDX			#$06

L89D4:			LDA			$11
				CMP			($00),Y
				BNE			L8A4A
				INY
				LDA			$10
				CMP			($00),Y
				BNE			L8A4A
				DEC			$11
				INC			$10
				INY
				BNE			L89D4
				DEC			$11
				INC			$10
				INC			$01
				DEX
				BNE			L89D4

L89F1:			LDA.wy		$0000,Y
				STA			$0600,Y
				LDA			$0100,Y
				STA			$0700,Y
				INY
				BNE			L89F1

L8A00:			LDA.wy		$0000,Y
				CMP			$0600,Y
				BNE			L8A4A
				LDA			$0100,Y
				CMP			$0700,Y
				BNE			L8A4A
				INY
				BNE			L8A00

L8A13:			LDA			$0200,Y
				STA.wy		$0000,Y
				LDA			$0300,Y
				STA			$0100,Y
				INY
				BNE			L8A13

L8A22:			LDA.wy		$0000,Y
				CMP			$0200,Y
				BNE			L8A4A
				LDA			$0100,Y
				CMP			$0300,Y
				BNE			L8A4A
				INY
				BNE			L8A22

L8A35:			LDA			$0600,Y
				STA.wy		$0000,Y
				LDA			$0700,Y
				STA			$0100,Y
				INY
				BNE			L8A35
				JSR			PRINT_OK
				JMP			L8A4D

L8A4A:			JSR			PRINT_ERR
L8A4D:			JSR			PRINTSTRING

				.byte		$00, $08
				.word		L8ABE			; "SRAM2 "

				LDY			#$00
				LDA			#$AA
				STA			$11
				LDA			#$55
				STA			$10
				LDX			#$08
				LDA			$2002
				LDA			#$20
				STA			$2006
				STY			$2006

L8A6B:			LDA			$11
				STA			$2007
				INY
				LDA			$10
				STA			$2007
				DEC			$11
				INC			$10
				INY
				BNE			L8A6B
				DEX
				BNE			L8A6B
				LDA			#$AA
				STA			$11
				LDA			#$55
				STA			$10
				LDX			#$08
				LDA			$2002
				LDA			#$20
				STA			$2006
				STY			$2006
				LDA			$2007

L8A98:			LDA			$2007
				CMP			$11
				BNE			L8AB4
				INY
				LDA			$2007
				CMP			$10
				BNE			L8AB4
				DEC			$11
				INC			$10
				INY
				BNE			L8A98
				DEX
				BNE			L8A98
				JMP			PRINT_OK

L8AB4:			JMP			PRINT_ERR

L8AB7:			.byte		"SRAM1 "
				.byte		$FF

L8ABE:			.byte		"SRAM2 "
				.byte		$FF

L8AC5:			LDY			#$00
				STY			$C016
				STY			$B1
				LDA			#$20
				STA			$C017

L8AD1:			LDA			$B1
				STA			$C015
				LDA			$C016
				ASL
				LDA			$C016
				ROL
				CMP			$B1
				BNE			L8B06
				INC			$B1
				BNE			L8AD1
				LDA			$4017
				AND			#$04
				BNE			L8B06
				LDA			#$21
				STA			$C017
				LDA			$4017
				AND			#$04
				BEQ			L8B06
				JSR			PRINTSTRING

				.byte		$0A, $08
				.word		L8B17			; "Com. Port "

				JSR			PRINT_OK
				LDA			#$00
				RTS

L8B06:			LDA			$CE
				BEQ			L8B14
				JSR			PRINTSTRING

				.byte		$0A, $08
				.word		L8B17			; "Com. Port "

				JSR			PRINT_ERR

L8B14:			LDA			#$FF
				RTS

L8B17:			.byte		"Com. Port "
				.byte		$FF

L8B22:			LDX			#$00
				STX			$10
				STX			$11
				STX			$13
				STX			IDEADD
				STX			DATAHIGH
				STX			IDELOW
				LDA			#$08
				STA			IDEADD
				LDA			IDELOW
				STA			$14
				LDA			IDELOW
				STA			$15
				LDX			#$40

L8B44:			LDA			$13
				STA			IDEADD
				STX			DATAHIGH
				STX			IDELOW
				LDA			$13
				ORA			#$08
				STA			IDEADD
				LDY			#$00

L8B58:			LDA			IDELOW
				CLC
				ADC			$10
				STA			$10
				LDA			$11
				ADC			IDELOW
				STA			$11
				INY
				CPY			#$40
				BNE			L8B58
				LDA			$13
				EOR			#$01
				STA			$13
				AND			#$01
				BNE			L8B44
				INX
				BNE			L8B44
				LDA			$13
				EOR			#$02
				STA			$13
				AND			#$02
				BNE			L8B44
				JSR			PRINTSTRING

				.byte		$0A, $07
				.word		L8BB0			; "IDE Port "

				LDA			#$04
				STA			IDEADD			; Set IDE address to 04h...
				LDA			V64STATUS
				BPL			L8BAD			; If MSB of V64STATUS = 0, branch to print "Err"
				LDA			#$00
				STA			IDEADD			; Set IDE address to 00h
				LDA			V64STATUS
				BMI			L8BAD			; MSB of V64STATUS should now go high, branch to print "Err" if not.
				LDA			$10
				CMP			$14				; $10 reg should match $14
				BNE			L8BAD			; If not, print "Err"
				LDA			$11
				CMP			$15				; $11 reg should match $15
				BNE			L8BAD			; If not, print "Err"
				JMP			PRINT_OK		; All fine, print "OK"

L8BAD:			JMP			PRINT_ERR

L8BB0:			.byte		"IDE Port "
				.byte		$FF

L8BBA:			JSR			LD497
				LDA			#$00
				STA			$13
				LDX			#$40

L8BC3:			LDA			$13
				STA			IDEADD
				STX			DATAHIGH
				STX			IDELOW
				LDA			$13
				ORA			#$08		; OR bit 3 of $13 (force bit 3 high, but still allow other bits high or low)
				STA			IDEADD		; Store into IDEADD?
				JSR			L8C5C		; Wait until MPEG FIFO is empty, then transfer 32 words of data from CD drive to MPEG bitstream input port.
				JSR			L8C5C		; Wait until MPEG FIFO is empty, then transfer 32 words of data from CD drive to MPEG bitstream input port.
				LDA			$13
				EOR			#$01
				STA			$13
				AND			#$01
				BNE			L8BC3
				INX
				BNE			L8BC3
				LDA			$13
				EOR			#$02
				STA			$13
				AND			#$02
				BNE			L8BC3
				RTS

L8BF3:			LDA			#$00
				STA			$72

L8BF7:			JSR			LD42D		; Check for correct MPEG chip ID + soft reset + reset parser + enable sector counting + disable DMA / IRQs
				JSR			LD71F		; Set MPEG border colour 1
				JSR			LD519
				JSR			LD88A		; Set MPEG chip to "MPEG Stream input" mode, then select "Normal Play Forward" mode.

L8C03:			JSR			L8BBA
				JSR			READKEYPAD
				BNE			L8C56		; Branch if keypad pressed
				LDA			$72
				EOR			#$FF
				STA			$72
				LDA			#$00
				STA			DATAHIGH
				STA			MPEG_DARH
				LDA			#$6D
				STA			MPEG_DARL
				LDA			$72
				BEQ			L8C49

				LDA			#$C2		; MPEG System Parsing control register
				LDY			#$10
				LDX			#$00		; Data is 1000h (set bit 12 - reset parser)
				JSR			WRITEMPEG16

				LDX			#$00
				JSR			WRITEMPEG
				LDA			#$00
				STA			MPEG_DDPR

				LDA			#$89		; MPEG ARG0 register
				LDX			#$08		; Set to 08h (decoding speed divisor)
				JSR			WRITEMPEG

				LDA			#$88		; MPEG Decoder command register
				LDX			#$04		; Slow Motion Forward command (to set speed from above)
				JSR			WRITEMPEG

				JSR			LD509		; Mute MPEG audio output while fast forwarding ??
				JMP			L8C03		; Loop this routine

L8C49:			LDA			$DE
				AND			#$01
				CLC
				ADC			#$01
				STA			MPEG_DDPR
				JMP			L8BF7

L8C56:			JSR			LD42D		; Check for correct MPEG chip ID + soft reset + reset parser + enable sector counting + disable DMA / IRQs
				JMP			LD71F		; Set MPEG border colour 1

; ********* Routine transfers 32 words of data from CD drive to MPEG chip... *******
; ********* Only one BYTE at a time is read from the CD drive (high, the low I guess), the 16-bit word is then written to the MPEG chip's bitstream input **********
L8C5C:			LDA			MPEG_IDPR		; Load A with MPEG_IDPR (read Word Remaining Register) to see if MPEG FIFO is empty
				AND			#$3F		; Mask lower bits (5:0)
				BNE			L8C5C		; Loop until WRR is 0 (= FIFO empty), transfer 32 more words immediately !!!!....

				LDY			IDELOW		; Load first IDELOW into Y
				LDA			IDELOW		; Load second IDELOW into A
				STA			DATAHIGH	; Store second IDELOW into A
				STY			MPEG_IDPR		; Store first IDELOW into MPEG_IDPR

				LDY			IDELOW		; Load first IDELOW into Y
				LDA			IDELOW		; Load second IDELOW into A
				STA			DATAHIGH	; Store second IDELOW into A
				STY			MPEG_IDPR		; Store first IDELOW into MPEG_IDPR

				LDY			IDELOW		; ...and so on.
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR

				LDY			IDELOW
				LDA			IDELOW
				STA			DATAHIGH
				STY			MPEG_IDPR
				RTS

L9025:			LDA			$C003
				TAX
				AND			#$0F
				ASL
				ASL
				ASL
L902E:			BIT			$C015
				BPL			L902E
				STA			$C016
				TXA
				AND			#$F0
				LSR
				ORA			#$80

L903C:			BIT			$C015
				BMI			L903C
				STA			$C016
				LDA			DATAHIGH
				TAX
				AND			#$0F
				ASL
				ASL
				ASL

L904D:			BIT			$C015
				BPL			L904D
				STA			$C016
				TXA
				AND			#$F0
				LSR
				ORA			#$80

L905B:			BIT			$C015
				BMI			L905B
				STA			$C016
				DEY
				BNE			L9025
				RTS

L9067:			BIT			$C015			; Bit test $C015
				BPL			L9067			; Branch to L9067 if MSB = 0
				STA			$B1
				AND			#$0F
				ASL
				ASL
				ASL
				STA			$C016

L9076:			LDA			$C015			; Bit test $C015
				BMI			L9076			; Branch to L9067 if MSB = 1
				LDA			$B1
				AND			#$F0
				LSR
				ORA			#$80
				STA			$C016
				RTS

L9086:			LDX			#$00

L9088:			LDA			$0200,X			; Load bytes starting at $0200
				JSR			L9067
				INX
				CPX			#$0B
				BNE			L9088			; Loop until X = #$0B (read 11 bytes)
				RTS

L9094:			LDX			#$00			; Start X index at 0

L9096:			JSR			L9AAF			; Get byte from parallel port
				STA			$0200,X			; Store at $0200 + X
				INX							; Increment X
				CPX			#$0B			; Compare X to #$0B
				BNE			L9096			; Loop until X = #$0B (read 11 bytes)
				RTS

L90A2:			LDA			#$FF			; I think this converts ROM name loaded into SRAM into a printable string
				STA			$020C			; Store #$FF into $020C
				LDA			$020A			; Byteswap rest ???
				STA			$020B
				LDA			$0209
				STA			$020A
				LDA			$0208
				STA			$0209
				LDA			#$2E
				STA			$0208
				JSR			PRINTSTRING

				.byte		$06, $05
				.word		$0200			; Print string

				RTS

L90C6:			.byte		"Linking..."
L90D0:			.byte		$FF

L90D1:			.byte		"Loading..."
				.byte		$FF

L90DC:			LDX			#$20
				JSR			PRINTSTRING

				.byte		$00, $07
				.word		$0098

				RTS

L90E6:			LDA			#$40
				STA			$15
				LDY			#$00
				STY			$14
				STY			$16
				STY			$12
				STY			$13
				STY			$18
				JSR			DMRAMACCESS

L90F9:			JSR			L9E50		; Puts variables into C008,9,A,B,C ?
				LDA			$A6
				BNE			L9109
				JSR			L947B
				JSR			L9489
				JMP			L910C

L9109:			JSR			L9025

L910C:			DEC			$14
				BNE			L9117
				JSR			LDD14
				DEC			$15
				BEQ			L9120
L9117:			INC			$12
				BNE			L90F9
				INC			$13
				JMP			L90F9

L9120:			JMP			DMRAMCANCEL

L9123:			JSR			L9754
				LDA			#$01
				JSR			L9067
				LDA			#$00
				JSR			L9067
				LDA			#$00
				JSR			L9067
				LDA			#$00
				STA			$0F
				LDA			$A8
				LDX			#$07

L913D:			ASL
				ROL			$0F
				DEX
				BNE			L913D
				JSR			L9067
				LDA			$0F
				JSR			L9067
				JMP			L9086

L914E:			.byte		"Uploaded:   0 M"
				.byte		$FF

L915E:			.byte		"Send Out Card "
				.byte		$FF

L916D:			.byte		"Send Out DRAM "
				.byte		$FF

L917C:			.byte		"Back Up Card To PC OK   "
				.byte		$FF

L9195:			.byte		"Sent Back DRAM To PC OK "
				.byte		$FF

L91AE:			JSR			L9094
				JSR			BLANKPPU
				LDA			$8C
				AND			#$20
				BEQ			L91CA
				JSR			BLINKON
				JSR			PRINTSTRING

				.byte		$06, $0A
				.word		LD566			; "Writing.... "

				JSR			BLINKOFF
				JMP			LF072

L91CA:			LDA			$8C
				AND			#$01
				BEQ			L91D3
				JMP			L9231

L91D3:			JSR			L9754
				JSR			LDFC1
				BEQ			L91E1
				JSR			L975A
				JMP			L9A1D

L91E1:			LDA			#$01
				STA			$A8
				LDA			$8C
				AND			#$40
				BEQ			L91ED
				INC			$A8

L91ED:			JSR			L9123
				LDA			#$02
				STA			BACKUPMD
				LDX			#$4A
				JSR			LD0AE
				LDA			V64STATUS
				AND			#$02
				BNE			L920A
				JSR			L975A
				JSR			L8071
				JMP			L9A1D

L920A:			JSR			L8147			; Print game name in CART on screen.
				JMP			L9223

L9210:			LDA			#$01			; 64M (8MBytes)
				JMP			L9221

L9215:			LDA			#$02			; 128M (16 MBytes)
				JMP			L9221

L921A:			LDA			#$03			; 192M (24 MBytes)
				JMP			L9221

L921F:			LDA			#$04			; 256M (32 MBytes)

L9221:			STA			$A8				; $A8 holds DRAM/Rom size (01=64M,02=128M,03=192M,04=256M)

L9223:			JSR			PRINTSTRING

				.byte		$00, $09
				.word		L915E			; "Send Out Card "

				LDA			#$FF
				STA			$A6
				JMP			L92AC			; Set parallel port pins for upload and send carT.

L9231:			LDA			$A7
				ORA			$A9
				BNE			L923D
				JSR			L975A
				JMP			L9A1D

L923D:			LDA			#$01
				STA			$A8
				LDA			$A7
				AND			#$F0
				BEQ			L9259
				INC			$A8
				LDA			$A9
				AND			#$0F
				BEQ			L9259
				INC			$A8
				LDA			$A9
				AND			#$F0
				BEQ			L9259
				INC			$A8

L9259:			JSR			L9123
				JMP			L9286

L925F:			LDA			#$01
				LDX			#$08
				LDY			#$00
				JMP			L9280

L9268:			LDA			#$02
				LDX			#$98
				LDY			#$00
				JMP			L9280

L9271:			LDA			#$03
				LDX			#$98
				LDY			#$0A
				JMP			L9280

L927A:			LDA			#$04
				LDX			#$98
				LDY			#$BA
L9280:			STA			$A8
				STX			$A7
				STY			$A9
L9286:			JSR			PRINTSTRING

				.byte		$00, $09
				.word		L916D			; "Send Out DRAM "

				LDA			#$00
				STA			$A6
				LDA			#$98
				STA			$C002
				LDA			#$BA
				STA			$C003
				LDA			#$80
				STA			DRAMADD0
				STA			DRAMADD1
				STA			DRAMADD2
				STA			DRAMADD3
				JMP			L92AC

L92AC:			JSR			L9754		; Set Parallel port pins for upload ?
				LDA			$A8			; $A8 holds DRAM/Rom size (01=64M,02=128M,03=192M,04=256M)

				CMP			#$01
				BEQ			L92C5		; Branch, to display "64M" if $A8 = #$01
				CMP			#$02
				BEQ			L92CF		; Branch, to display "128M" if $A8 = #$02
				CMP			#$03
				BEQ			L92D9		; Branch, to display "192M" if $A8 = #$03
				CMP			#$04
				BEQ			L92E3		; Branch, to display "256M" if $A8 = #$04

				LDA			#$01		; If $A8 (rom size) other than above, then use 64M (01h)
				STA			$A8
L92C5:			JSR			PRINTSTRING

				.byte		$00, $80
				.word		L800C			; "64M"

				JMP			L92EA

L92CF:			JSR			PRINTSTRING

				.byte		$00, $80
				.word		L8010			; "128M"

				JMP			L92EA

L92D9:			JSR			PRINTSTRING

				.byte		$00, $80
				.word		L8015			; "192M"

				JMP			L92EA

L92E3:			JSR			PRINTSTRING

				.byte		$00, $80
				.word		L801A			; "256M"

L92EA:			JSR			BLINKON
				JSR			PRINTSTRING

				.byte		$06, $0A
				.word		LD566			; "Writing.... "

				JSR			BLINKOFF
				JSR			PRINTSTRING

				.byte		$00, $07
				.word		L914E			; "Uploaded:   0 M"

				LDA			#$00
				STA			$5D
				STA			$5E
				STA			$5F
				LDA			#$08
				STA			$E5

L930A:			JSR			L90E6		; Send data to parallel port
				INC			$E5		; Increment $E5

				DEC			$A8		; Decrement $A8
				BNE			L930A		; Loop until $A8 = 00h

				JSR			L975A		; Reset Parallel port pins (Upload finished?)
				LDA			#$00
				STA			$C002		; Zero addresses
				STA			$C003		; Zero addresses
				STA			DRAMADD0	; Zero addresses
				STA			DRAMADD1	; Zero addresses
				STA			DRAMADD2	; Zero addresses
				STA			DRAMADD3	; Zero addresses
				LDA			#$00
				STA			BACKUPMD	; Reset BACKUPMD

				LDA			$A6		; If $A6 = 00h... 
				BEQ			L933D		; ...then branch to display "Sent Back DRAM To PC OK "
				JSR			PRINTSTRING	; Else, show...

				.byte		$00, $0A
				.word		L917C			; "Back Up Card To PC OK "

				JMP			L9A1D

L933D:			JSR			PRINTSTRING

				.byte		$00, $0A
				.word		L9195			; "Sent Back DRAM To PC OK "

				JMP			L9A1D

L9347:			JSR			L9738
				LDA			$4A
				ORA			$4B
				ORA			$4C
				ORA			$4D
				BEQ			L9374
				LDA			$49
				BPL			L937C
				AND			#$7F
				ORA			$48
				BNE			L9374
				LDA			#$FF
				SEC
				SBC			$46
				LDA			#$BF
				SBC			$47
				BCS			L937C
				LDA			#$FF
				SEC
				SBC			$46
				LDA			#$CF
				SBC			$47
				BCC			L937C

L9374:			LDA			#$00
				JSR			L9067
				JMP			L975A

L937C:			LDA			#$A0
				JSR			L9067
				LDA			$49
				BPL			L93A5
				LDA			$4A
				STA			$14
				LDA			$4B
				STA			$15
				JSR			LDF52
				LDY			#$00

L9392:			LDA			($46),Y
				JSR			L9067
				INC			$46
				BNE			L939D
				INC			$47

L939D:			JSR			LDF52
				BCS			L9392
				JMP			L975A

L93A5:			LDA			$46
				AND			#$01
				STA			$AE
				LDA			#$01
				STA			$C00E
				JSR			DMRAMACCESS
				JSR			L96EE
				PHP
				JSR			L967A
				PLP
				BCC			L93DC
				JSR			L943E
				LDA			DATAHIGH
				JSR			L9067
				JSR			L96D4
				BCC			L9416
				JSR			L9725
				LDA			#$01
				STA			$18
				LDA			#$00
				STA			$19
				JSR			L9657
				JMP			L93DF

L93DC:			JSR			L9725

L93DF:			LDA			$16
				BEQ			L9401
				EOR			#$FF
				STA			$18
				INC			$18
				LDA			#$00
				STA			$19
				JSR			L9E50		; Puts variables into C008,9,A,B,C ?
				LDA			$4B
				ORA			$4C
				BNE			L93FE
				LDA			$4A
				CMP			$18
				BCS			L93FE
				STA			$18

L93FE:			JSR			L948E

L9401:			JSR			L9E50		; Puts variables into C008,9,A,B,C ?
				LDA			$4A
				ORA			$4B
				ORA			$4C
				BNE			L941F
				LDA			$AD
				BEQ			L9416
				JSR			L943E
				JSR			L9067

L9416:			JSR			L975A
				JSR			DMRAMCANCEL
				JMP			L95F6

L941F:			LDA			$4B
				ORA			$4C
				BNE			L9430
				LDA			$4A
				STA			$18
				LDA			#$00
				STA			$19
				JMP			L9438

L9430:			LDA			#$00
				STA			$18
				LDA			#$01
				STA			$19

L9438:			JSR			L948E
				JMP			L9401

L943E:			JSR			L9E50		; Puts variables into C008,9,A,B,C ?
				JSR			L9466
				LDA			$C00C
				JSR			L9466
				LDA			MPEG_DDPR
				RTS

DMRAMACCESS:	LDY			#$00
				STY			DATAHIGH
				LDA			#$81		; MPEG Proc. control register
				LDX			#$00		; Write 00h (DM RAM bus Access request)
				JMP			WRITEMPEG	; Write the command, then return.

DMRAMCANCEL:	LDY			#$00
				STY			DATAHIGH
				LDA			#$81		; MPEG Proc. control register
				LDX			#$08		; Write 08h (DM RAM bit back to 1) to cancel DM access
				JMP			WRITEMPEG	; Write the command, then return

L9466:			LDA			#$00
				STA			DATAHIGH
				LDA			#$03
				STA			MPEG_DARH		; MPEG DRAM Address 1
				LDA			#$FF
				STA			DATAHIGH
				LDA			#$00
				STA			MPEG_DARL		; MPEG DRAM Address 2
				RTS

L947B:			JSR			L9466
				LDY			$18

L9480:			LDA			$C00C
				DEY
				BNE			L9480
				JMP			L9466

L9489:			LDY			#$00
				JMP			L9499

L948E:			JSR			L947B
				LDY			$18
				JSR			L9499
				JMP			L9644

L9499:			LDA			MPEG_DDPR
				TAX
				AND			#$0F
				ASL
				ASL
				ASL

L94A2:			BIT			$C015
				BPL			L94A2
				STA			$C016
				TXA
				AND			#$F0
				LSR
				ORA			#$80

L94B0:			BIT			$C015
				BMI			L94B0
				STA			$C016
				LDA			DATAHIGH
				TAX
				AND			#$0F
				ASL
				ASL
				ASL

L94C1:			BIT			$C015
				BPL			L94C1
				STA			$C016
				TXA
				AND			#$F0
				LSR
				ORA			#$80

L94CF:			BIT			$C015
				BMI			L94CF
				STA			$C016
				DEY
				BNE			L9499
				RTS

L94DB:			JSR			L9738
				LDA			$4A
				ORA			$4B
				ORA			$4C
				ORA			$4D
				BEQ			L952C
				LDA			$49
				BPL			L9549
				AND			#$7F
				ORA			$48
				BNE			L952C
				LDA			#$1F
				SEC
				SBC			$46
				LDA			#$01
				SBC			$47
				BCC			L9516
				LDA			$46
				CLC
				ADC			$4A
				STA			$B1
				LDA			$47
				ADC			$4B
				BEQ			L9549
				CMP			#$02
				BCS			L952C
				LDA			$B1
				CMP			#$20
				BCC			L9549
				BCS			L952C

L9516:			LDA			#$FF
				SEC
				SBC			$46
				LDA			#$01
				SBC			$47
				BCS			L952C
				LDA			#$FF
				SEC
				SBC			$46
				LDA			#$07
				SBC			$47
				BCS			L9534

L952C:			LDA			#$00
				JSR			L9067
				JMP			L975A

L9534:			CLC
				LDA			$46
				ADC			$4A
				STA			$B1
				LDA			$47
				ADC			$4B
				CMP			#$08
				BCC			L9549
				BNE			L952C
				LDA			$B1
				BNE			L952C

L9549:			LDA			#$A1
				JSR			L9067
				JSR			L975A
				LDA			$49
				BPL			L9573
				LDA			$4A
				STA			$14
				LDA			$4B
				STA			$15
				JSR			LDF52
				LDY			#$00

L9562:			JSR			L9AAF			; Get byte from parallel port
				STA			($46),Y
				INC			$46
				BNE			L956D
				INC			$47

L956D:			JSR			LDF52
				BCS			L9562
				RTS

L9573:			LDA			$46
				AND			#$01
				STA			$AE
				JSR			L96EE
				PHP
				JSR			L967A
				PLP
				BCC			L95AD
				JSR			L9E50		; Puts variables into C008,9,A,B,C ?
				LDA			DRAMLOW
				PHA
				JSR			L9E50		; Puts variables into C008,9,A,B,C ?
				JSR			L9AAF			; Get byte from parallel port
				STA			DATAHIGH
				PLA
				STA			DRAMLOW
				JSR			L96D4
				BCC			L95F6
				JSR			L9725
				LDA			#$01
				STA			$18
				LDA			#$00
				STA			$19
				JSR			L9657
				JMP			L95B0

L95AD:			JSR			L9725

L95B0:			LDA			$16
				BEQ			L95D2
				EOR			#$FF
				STA			$18
				INC			$18
				LDA			#$00
				STA			$19
				JSR			L9E50		; Puts variables into C008,9,A,B,C ?
				LDA			$4B
				ORA			$4C
				BNE			L95CF
				LDA			$4A
				CMP			$18
				BCS			L95CF
				STA			$18

L95CF:			JSR			L9635

L95D2:			JSR			L9E50		; Puts variables into C008,9,A,B,C ?
				LDA			$4A
				ORA			$4B
				ORA			$4C
				BNE			L9616
				LDA			$AD
				BEQ			L95F6
				LDA			DRAMLOW
				LDA			DATAHIGH
				PHA
				JSR			L9E50		; Puts variables into C008,9,A,B,C ?
				JSR			L9AAF		; Get byte from parallel port
				TAY
				PLA
				STA			DATAHIGH
				STY			DRAMLOW

L95F6:			LDA			$A7
				STA			$C002
				LDA			$A9
				STA			$C003
				LDA			#$80
				STA			DRAMADD0
				STA			DRAMADD1
				STA			DRAMADD2
				STA			DRAMADD3
				LDA			#$01
				STA			$DD
				STA			BACKUPMD
				RTS

L9616:			LDA			$4B
				ORA			$4C
				BNE			L9627
				LDA			$4A
				STA			$18
				LDA			#$00
				STA			$19
				JMP			L962F

L9627:			LDA			#$00
				STA			$18
				LDA			#$01
				STA			$19

L962F:			JSR			L9635
				JMP			L95D2

L9635:			LDY			$18
				LDA			$AE
				BNE			L9641
				JSR			L9684
				JMP			L9644

L9641:			JSR			L99B6

L9644:			SEC
				LDA			$4A
				SBC			$18
				STA			$4A
				LDA			$4B
				SBC			$19
				STA			$4B
				LDA			$4C
				SBC			#$00
				STA			$4C

L9657:			CLC
				LDA			$16
				ADC			$18
				STA			$16
				LDA			$12
				ADC			$19
				STA			$12
				LDA			$13
				ADC			#$00
				STA			$13
				AND			#$40
				BEQ			L9683
				LDA			#$00
				STA			$13
				INC			$B0
				LDA			$B0
				AND			#$03
				STA			$B0

L967A:			LDY			$B0
				STY			$E5
				LDA			#$80
				STA			DRAMADD0,Y

L9683:			RTS

L9684:			LDX			#$10

L9686:			BIT			$C017
				BPL			L9686
				LDA			$C015
				STX			$C017
				STA			$B1
				LDX			#$12

L9695:			BIT			$C017
				BMI			L9695
				LDA			$C015
				STX			$C017
				STA			DATAHIGH
				LDA			$B1
				STA			DRAMLOW
				DEY
				BNE			L9684
				RTS

				CLC
				LDA			$16
				ADC			$18
				STA			$B1
				LDA			$12
				ADC			$19
				STA			$0F
				LDA			$13
				ADC			#$00
				AND			#$40
				BEQ			L96C5
				LDA			$B1
				ORA			$0F

L96C5:			RTS

				SEC
				LDA			$4A
				SBC			#$01
				LDA			$4B
				SBC			#$01
				LDA			$4C
				SBC			#$00
				RTS

L96D4:			LDA			$4A
				SEC
				SBC			#$01
				STA			$4A
				LDA			$4B
				SBC			#$00
				STA			$4B
				LDA			$4C
				SBC			#$00
				STA			$4C
				LDA			$4D
				SBC			#$00
				STA			$4D
				RTS

L96EE:			LDA			#$10
				STA			$C002
				LDA			#$32
				STA			$C003
				LDA			#$00
				STA			BACKUPMD
				STA			DRAMADD0
				STA			DRAMADD1
				STA			DRAMADD2
				STA			DRAMADD3
				LDA			$48
				ASL
				LDA			$49
				ROL
				AND			#$03
				STA			$B0
				LDA			$48
				AND			#$7F
				LSR
				STA			$13
				LDA			$47
				ROR
				STA			$12
				LDA			$46
				ROR
				STA			$16
				RTS

L9725:			LDA			$4A
				AND			#$01
				STA			$AD
				LSR			$4D
				ROR			$4C
				ROR			$4B
				ROR			$4A
				LDA			#$00
				STA			$4D
				RTS

L9738:			LDY			#$03

L973A:			JSR			L9AAF			; Get byte from parallel port
				STA.wy	$0046,Y
				DEY
				BPL			L973A
				LDA			$49
				AND			#$81
				STA			$49
				LDY			#$03

L974B:			JSR			L9AAF			; Get byte from parallel port
				STA.wy	$004A,Y
				DEY
				BPL			L974B
L9754:			LDA			#$FE
				STA			$C016
				RTS

L975A:			LDA			#$FF
				STA			$C016
				RTS

L9760:			.byte		"  Up Load Rom To PC OK  "
				.byte		$FF

L9779:			.byte		" This is not a V64 file "
				.byte		$FF

L9792:			JSR			L9AAF			; Get byte from parallel port
				STA			$0C
				JSR			L9AAF			; Get byte from parallel port
				STA			$0B
				JSR			L9754
				LDA			$0C
				CMP			#$02
				BCC			L97D6
				CMP			#$08
				BCC			L97B5
				CMP			#$80
				BCC			L97D6
				CMP			#$C0
				BCC			L97B5
				CMP			#$D0
				BCC			L97D6

L97B5:			LDA			#$A2
				JSR			L9067
				JSR			L975A
				LDA			#$4C
				STA			$0A
				JMP.w		$000A

L97C4:			JSR			L9AAF			; Get byte from parallel port
				STA			$0A
				JSR			L9AAF			; Get byte from parallel port
				STA			$0B
				LDA			$0A
				AND			#$3F
				CMP			#$0B
				BCC			L97E1

L97D6:			JSR			L9754
				LDA			#$00
				JSR			L9067
				JMP			L975A

L97E1:			LDA			$0B
				CMP			#$18
				BCS			L97D6
				JSR			L9754
				LDA			#$A3
				JSR			L9067
				JSR			L975A
				JSR			BLINKOFF
				LDA			$0A
				BPL			L97FE
				JSR			BLINKON
				LDA			$0A

L97FE:			AND			#$3F
				ORA			#$90
				JSR			WRITEPPU
				LDA			$0B
				ORA			#$A0
				JSR			WRITEPPU
				BIT			$0A
				BVS			L981D

L9810:			JSR			L9AAF			; Get byte from parallel port
				CMP			#$FF
				BEQ			L982A
				JSR			PRINTASCII
				JMP			L9810

L981D:			JSR			L9AAF			; Get byte from parallel port
				CMP			#$FF
				BEQ			L982A
				JSR			WRITEPPU
				JMP			L981D

L982A:			JSR			BLINKOFF

L982D:			RTS

L982E:			JSR			L9A34
				BNE			L982D
				LDA			$C0
				CMP			#$57
				BEQ			L9860
				CMP			#$52
				BEQ			L986A
				CMP			#$45
				BNE			L982D
				JSR			L9AAF			; Get byte from parallel port
				CMP			#$52
				BEQ			L9857
				CMP			#$57
				BEQ			L985A
				CMP			#$43
				BEQ			L985D
				CMP			#$50
				BNE			L982D
				JMP			L97C4

L9857:			JMP			L9347

L985A:			JMP			L94DB

L985D:			JMP			L9792

L9860:			JMP			L91AE

L9863:			.byte		"V64_VER"

L986A:			JSR			BLANKPPU
				JSR			L9AAF			; Get byte from parallel port
				JSR			L90DC			; Print string to PPU (from #0098?)
				LDA			#$00
				STA			$5D				; Clear reg
				STA			$5E				; Clear reg
				STA			$5F				; Clear reg
				STA			$72				; Clear reg
				JSR			READKEYPAD		
				LDA			$8C
				AND			#$20			; Check for STOP button ???
				BEQ			L9897			; Jump to print " Loaded:", and continue....
				STA			$72
				LDA			#$01
				STA			$C00E			; ...Else, set $C00E to #$01 (verify mode?)
				JSR			PRINTSTRING

				.byte		$00, $07
				.word		LDF9F			; "Verified:   0 M"

				JMP			L989E			; Jumps to same routine, but $C00E is set to 1 for verify?

L9897:			JSR			PRINTSTRING

				.byte		$00, $07
				.word		LDF8F			; "  Loaded:   0 M"

L989E:			LDX			#$00			; Start X index at 0

L98A0:			JSR			L9AAF			; Get byte from parallel port
				STA			$020E,X			; Store at $020E + X
				INX							; Increment X (index)
				CPX			#$04			; Compare X to #$04
				BNE			L98A0			; Loop until X = #$04 (read 5 bytes)
				JSR			L9094			; Read 11 bytes into RAM starting at $0200
				JSR			L90A2			; Convert name in SRAM to printable string and print to PPU ??
				LDA			$0210
				AND			#$03
				ORA			$020E
				ORA			$020F
				BNE			L98C5			; Branch to L98C5 if file "is not a V64 file "
				LDA			$0211
				AND			#$FC
				BEQ			L98DD			; Branch to L98DD if file checks out?

L98C5:			JSR			PRINTSTRING

				.byte		$00, $07
				.word		L9779			; " This is not a V64 file "

				LDX			#$A0
				JSR			LD0AE
				LDX			#$20
				JSR			PRINTSTRING

				.byte		$00, $05
				.word		$00C8

				JMP			PRINT_STOP

L98DD:			LDA			$0211			; Possibly checksum stuff done before loading
				LSR							; V64 file from parallel port?
				ROR			$0210
				LSR
				ROR			$0210
				LDA			$0210
				STA			$18
				CMP			#$81
				BCS			L98C5			; Branch to L98C5 if file "is not a V64 file "
				LDA			$0210			; Load A with $0210
				CMP			#$01			; Compare with #$01
				BNE			L990A			; Branch to start loading ROM if $0210 does NOT equal #$01
				
				LDX			#$00			; Start X index at 0
L98FA:			LDA			$0200,X			; Read string starting at $0200
				CMP			L9863,X			; Compare to "V64_VER" - First characters of V64 BIOS file name?
				BNE			L990A			; Branch to L990A if it DOESN'T match BIOS header (starting loading ROM instead?)
				INX
				CPX			#$07			; Keep reading until all 7 bytes match
				BNE			L98FA
				JMP			L9AD7			; If all matched, jump to L9AD7 (Loads V64 BIOS into MPEG DRAM?)

L990A:			LDX			#$20			; ************ Start loading ROM from parallel port into DRAM *************
				JSR			PRINTSTRING

				.byte		$17, $0B
				.word		$0081

				LDA			#$80			; Start DRAM address at #$80 (DRAMADD0 = b'1000 0000')
				STA			DRAMADD0
				LDA			#$00
				STA			DRAMADD1
				STA			DRAMADD2
				STA			DRAMADD3
				
				LDA			#$08
				STA			$C002			; Set $C002 to #$08 - (ROM is 64M or lower?)
				STA			$A7				; Set $A7 to #$08
				STA			$E5				; Set $E5 to #$08
				JSR			L9971			; Load from parallel into DRAM
				LDA			$18
				BEQ			L996E			; If $18 = 0 then ROM has loaded - jump, to START ROM PLAY !!!
				INC			$E5				; Increment $E5
				LDA			#$80
				STA			DRAMADD1
				
				LDA			#$98
				STA			$C002			; Set $C002 to #$98 - (ROM is 128M or lower?)
				STA			$A7				; Set $A7 to #$98
				JSR			L9971			; Load from parallel into DRAM
				LDA			$18
				BEQ			L996E			; If $18 = 0 then ROM has loaded - jump, to START ROM PLAY !!!
				INC			$E5				; Increment $E5
				LDA			#$80
				STA			DRAMADD2
				
				LDA			#$0A
				STA			$C003			; Set $C003 to #$0A - (ROM is 192M or lower?)
				STA			$A9				; Set $A9 to #$0A
				JSR			L9971			; Load from parallel into DRAM
				LDA			$18
				BEQ			L996E			; If $18 = 0 then ROM has loaded - jump, to START ROM PLAY !!!
				INC			$E5				; Increment $E5
				LDA			#$80
				STA			DRAMADD3
				
				LDA			#$BA
				STA			$C003			; Set $C003 to #$BA - (ROM is 256M or lower?)
				STA			$A9				; Set $A9 to #$BA
				JSR			L9971			; Load from parallel into DRAM

L996E:			JMP			L817A			; Display " You can play game now " and allow ROM to run on N64 !!!!

L9971:			LDA			$18				; This routine loads words from parallel port into DRAM
				CMP			#$21
				BCS			L997F			; If reg $18 >= 20h, then branch to L997F (subtracts 20h)
				ASL							; Else, shift "A" ($18) left (multiply by 2)
				LDX			#$00
				STX			$18
				JMP			L9985

L997F:			SBC			#$20			; Subtract #$20 from "A" ("A" should contain $18 if called from L9971)
				STA			$18				; Store  back to $18
				LDA			#$40

L9985:			STA			$15
				LDY			#$00
				STY			$14
				STY			$16
				STY			$12
				STY			$13
				
L9991:			JSR			L9E50		; Puts variables into C008,9,A,B,C ?
				LDA			$72
				BEQ			L999E
				JSR			L99DE
				JMP			L99A1		; Update counter regs $12 to $15

L999E:			JSR			L99B6		; Load "Y" words from parallel into DRAM (would be 256 words if Y starts at 00h)

L99A1:			DEC			$14			; Decrement $14
				BNE			L99AC		; Branch to L99AC if $14 is NOT 0
				JSR			LDD14
				DEC			$15			; Decrement $15
				BEQ			L99B5		; Branch to L99B5 (return) if $15 IS 0

L99AC:			INC			$12			; Increment $12
				BNE			L9991		; Branch to L9991 until $12 = 0
				INC			$13			; Increment $13
				JMP			L9991		; Loop

L99B5:			RTS



L99B6:			LDX			#$12		; Load X with #$12 (to set extra pins to after low byte received)
L99B8:			BIT			$C017		; Bit test $C017
				BMI			L99B8		; Keep testing until MSB = 0
				LDA			$C015		; Load A with parallel data (low byte)
				STX			$C017		; Store X at $C017 (parallel extra pins)
				STA			$B1			; Store A (par data) into $B1
				
				LDX			#$10		; Load X with #$10 (to set extra pins to after high byte received)
L99C7:			BIT			$C017		; Bit test $C017
				BPL			L99C7		; Keep testing until MSB = 1
				LDA			$C015		; Load A with parallel data (high byte)
				STX			$C017		; Store X at $C017 (parallel extra pins)
				STA			DATAHIGH	; Store A (par high byte) to DATAHIGH
				LDA			$B1			; Load A with $B1 (par low byte)
				STA			DRAMLOW		; Store low byte to DRAMLOW
				DEY						; Decrement Y
				BNE			L99B6		; Load next word from parallel into DRAM until Y = 0
				RTS						; Return

				
				
L99DE:			TYA						; Transfer Y to A
				PHA						; Push A ("Y") to stack
				JSR			L9466		; Set some bytes in MPEG DRAM ?

L99E3:			LDA			$C00C		; No idea !
				DEY						; Decrement Y
				BNE			L99E3		; Loop to L99E3 until Y = 0
				JSR			L9466		; Set some bytes in MPEG DRAM ?
				PLA						; Pull "Y" from stack
				TAY						; Transfer stored "Y" back to Y

L99EE:			LDA			MPEG_DDPR	; Load A with MPEG DATA low? byte

				LDX			#$12		; Load X with #$12 (to set extra pins to after low byte received)
L99F3:			BIT			$C017		; Bit test $C017
				BMI			L99F3		; ....until MSB = 0
				CMP			$C015		; Compare A (MPEG DATA low byte) to $C015 (parallel low byte)
				STX			$C017		; Set $C017 to X (#$12)
				BNE			L9A16		; Display "Verify error" if compare doesn't match
				
				LDX			#$10		; Load X with #$10 (to set extra pins to after high byte received)
				LDA			DATAHIGH	; Load A with DATA high byte
L9A05:			BIT			$C017		; Bit test $C017
				BPL			L9A05		; ....until MSB 1
				CMP			$C015		; Compare A (MPEG DATA high byte) to $C015 (parallel high byte)
				STX			$C017		; Set $C017 to X (#$10)
				BNE			L9A16		; Display "Verify error" if compare doesn't match
				DEY						; Decrement Y
				BNE			L99EE		; Loop until Y = 0
				RTS

L9A16:			JSR			PRINTSTRING

				.byte		$06, $0A
				.word		L9A24			; "Verify error"

L9A1D:			JSR			PRINT_STOP		; Display "Stop"
				LDX			$B8
				TXS
				RTS

L9A24:			.byte		"Verify error"
				.byte		$FF

L9A31:			LDA			#$FF
				RTS

L9A34:			LDA			$4017
				AND			#$04
				BEQ			L9A31
				LDA			#$10
				STA			$C017
				LDA			#$FF
				STA			$C016
				LDA			#$AA
				STA			$1F
				JSR			L9A78
				BNE			L9A31
				LDA			#$55
				STA			$1F
				JSR			L9A78
				BNE			L9A31
				LDA			#$00
				STA			$AC
				JSR			L9AAF			; Get byte from parallel port
				CMP			#$47
				BNE			L9A31
				JSR			L9AAF			; Get byte from parallel port
				CMP			#$44
				BNE			L9A31
				JSR			L9AAF			; Get byte from parallel port
				CMP			#$36
				BNE			L9A31
				JSR			L9AAF			; Get byte from parallel port
				STA			$C0
				LDA			#$00
				RTS

L9A78:			LDA			#$00
				STA			$B1

L9A7C:			LDA			$C016
				AND			#$02
				BEQ			L9A8A
				INC			$B1
				BNE			L9A7C

L9A87:			LDA			#$FF
				RTS

L9A8A:			LDA			$C015
				CMP			$1F
				BNE			L9A87
				LDA			#$18
				STA			$C017
				LDA			#$00
				STA			$B1

L9A9A:			LDA			$C016
				AND			#$02
				BNE			L9AA7
				INC			$B1
				BEQ			L9A87
				BNE			L9A9A

L9AA7:			LDA			#$10
				STA			$C017
				LDA			#$00
				RTS

L9AAF:			LDA			$AC
				BNE			L9AC1			; If $AC = 0 then branch to L9AC1

L9AB3:			BIT			$C017			; Else, bit test $C017 
				BMI			L9AB3			; ....until sign bit (MSB) = 1
				LDA			$C015			; Load $C015 (parallel byte) into A
				PHA							; Push it to the stack
				LDA			#$12			; - set $C017 to b'1 0010' (set bits 4 and 1)
				JMP			L9ACC

L9AC1:			BIT			$C017			; Bit test $C017
				BPL			L9AC1			; ....until sign bit (MSB) = 0
				LDA			$C015			; Load $C015 (parallel byte) into A
				PHA							; Push it to the stack
				LDA			#$10			; - set $C017 to b'1 0000' (set bit 4)

L9ACC:			STA			$C017			; Set $C017 to "A" (could be #$12 or #$10)
				LDA			$AC
				EOR			#$FF
				STA			$AC
				PLA							; Pull parallel data from stack
				RTS							; Return

L9AD7:			JSR			L90DC			; Print string at $0098 ??
				JSR			PRINTSTRING

				.byte		$07, $07
				.word		L90D1			; "Loading..."

				LDY			#$00
				STY			DATAHIGH
				LDA			#$01
				STA			MPEG_DARH			; MPEG DRAM address 1
				STY			MPEG_DARL			; MPEG DRAM address 0
				LDA			#$02
				STA			$15
				STY			$14
				JSR			LDF52

L9AF7:			JSR			L9CB7		; Load next word from parallel port ?
				JSR			LDF52
				BCS			L9AF7
L9AFF:			JSR			L9C3E

				BEQ			L9B07
L9B04:			JMP			L98C5

L9B07:			JSR			L9B9F
				BNE			L9B04


; L9B0E Routine which writes eeprom flashing code (512 bytes??) from L9CDF into SRAM memory at 0600h

				LDY			#$00		; Start Y at 00h
L9B0E:			LDA			L9CDF,Y
				STA			$0600,Y
				LDA			L9CDF+$0100,Y
				STA			$0700,Y
				INY
				BNE			L9B0E		; Write next word (sort of!) to V64 memory until Y = 00h

				JSR			L0742		; Get EEPROM ID's, Erase EEPROM and Disable EEPROM protection
				TYA					; Transfer EEPROM Device ID from Y to A
				PHA					; Push A to stack
				TXA					; Transfer EEPROM Manufacturer ID from X to A
				PHA					; Push A to stack
				JSR			PRINTSTRING

				.byte		$00, $08
				.word		L9B5E			; "Manufacturer Code: "

				PLA					; Retrieve EEPROM Manufacturer ID from stack
				TAX					; Transfer A to X
				JSR			PRINTSTRING

				.byte		$00, $80
				.word		$0001			; Print Manufacturer ID to PPU

				JSR			PRINTSTRING

				.byte		$00, $09
				.word		L9B72			; "Device Code:     : "

				PLA					; Retrieve EEPROM Device ID from stack
				TAX					; Transfer A to X
				JSR			PRINTSTRING

				.byte		$00, $80
				.word		$0001			; Print Device ID to PPU

				JSR			BLINKON
				JSR			PRINTSTRING

				.byte		$00, $07
				.word		L9B86			; " Re-Writing System Rom  "

				JSR			BLINKOFF
				LDY			#$00
				STY			$13
				STY			$12
				STY			$00
				STY			$0C
				JMP			$0600		; JUMP TO EEPROM writing routine in V64 memory

L9B5E:			.byte		"Manufacturer Code: "
				.byte		$FF

L9B72:			.byte		"Device Code:     : "
				.byte		$FF

L9B86:			.byte		" Re-Writing System Rom  "
				.byte		$FF

L9B9F:			LDY			#$00
				STY			$10
				STY			$11
				STY			DATAHIGH
				LDA			#$01
				STA			MPEG_DARH		; Store 01h to MPEG_DARH
				STY			MPEG_DARL		; Store 00h to MPEG_DARL
				LDA			MPEG_DDPR		; Load A with MPEG_DDPR
				PHA					; Push A to stack
				LDA			DATAHIGH	; Load A with DATAHIGH
				PHA					; Push A to stack
				LDA			#$1F
				STA			DATAHIGH	; Store 1Fh to DATAHIGH
				LDA			#$FC
				STA			MPEG_DARL		; Store FCh to MPEG_DARL
				LDA			MPEG_DDPR
				STA			$14
				LDA			DATAHIGH
				STA			$15
				LDA			#$08
				STA			DATAHIGH
				STY			MPEG_DARL		; Store 00h to MPEG_DARL
				LDX			#$18

L9BD6:			LDA			MPEG_DDPR
				CLC
				ADC			$10
				STA			$10
				LDA			DATAHIGH
				ADC			$11
				STA			$11
				INY
				BNE			L9BD6
				DEX
				BNE			L9BD6
				SEC
				LDA			$10
				SBC			$14
				STA			$10
				LDA			$11
				SBC			$15
				STA			$11
				LDA			$10
				CMP			$14
				BNE			L9C3D
				LDA			$11
				CMP			$15
				BNE			L9C3D
				PLA
				STA			$15
				PLA
				STA			$14
				LDY			#$00
				STY			$10
				STY			$11
				LDA			#$01
				STA			$B0

L9C14:			LDX			#$20

L9C16:			LDA			MPEG_DDPR
				CLC
				ADC			$10
				STA			$10
				LDA			DATAHIGH
				ADC			$11
				STA			$11
				INY
				BNE			L9C16
				DEX
				BNE			L9C16
				INC			$B0
				LDA			$B0
				CMP			#$08
				BNE			L9C14
				LDA			$10
				CMP			$14
				BNE			L9C3D
				LDA			$11
				CMP			$15

L9C3D:			RTS

L9C3E:			LDY			#$00
				STY			$12
				STY			$13
				LDA			#$06
				STA			$01
				STY			$00
				STY			$14
				LDA			#$02
				STA			$15
				JSR			LDF52

L9C53:			JSR			L9C66
				BNE			L9C65
				INC			$12
				BNE			L9C5E
				INC			$13

L9C5E:			JSR			LDF52
				BCS			L9C53
				LDA			#$00

L9C65:			RTS

L9C66:			LDY			#$00
				STY			DATAHIGH
				LDA			#$01
				STA			MPEG_DARH
				LDA			$13
				LSR
				LDA			$12
				ROR
				STA			DATAHIGH
				LDA			#$00
				ROR
				STA			MPEG_DARL

L9C7F:			LDA			MPEG_DDPR
				STA			($00),Y
				INY
				LDA			DATAHIGH
				STA			($00),Y
				INY
				BNE			L9C7F
				STY			DATAHIGH
				LDA			#$02
				STA			MPEG_DARH
				LDA			$13
				LSR
				LDA			$12
				ROR
				STA			DATAHIGH
				LDA			#$00
				ROR
				STA			MPEG_DARL

L9CA4:			LDA			MPEG_DDPR
				CMP			($00),Y
				BNE			L9CB6
				INY
				LDA			DATAHIGH
				CMP			($00),Y
				BNE			L9CB6
				INY
				BNE			L9CA4

L9CB6:			RTS

L9CB7:			LDX			#$12		; Routine which loads a word from the parallel port into MPEG DRAM (i think)

L9CB9:			BIT			$C017
				BMI			L9CB9
				LDA			$C015		; Load A with low byte from parallel port ??
				STX			$C017		; Store X (12h) to parallel port pins ??
				STA			$B1			; Store low byte from parallel port to $B1 (temporary)

				LDX			#$10
L9CC8:			BIT			$C017
				BPL			L9CC8
				LDA			$C015		; Load A with high byte from parallel port ??
				STX			$C017		; Store X (10h) to parallel port pins ??
				STA			DATAHIGH	; Store high byte from parallel port to DATAHIGH
				LDA			$B1			; Load A with low byte from $B1
				STA			MPEG_DDPR		; Store WORD to MPEG DRAM ?
				INY
				BNE			L9CB7		; Read next word from parallel port until Y = 00h
				RTS

UPDATE_PAGE:			; Prints ROM names to screen (from PAGEMIN to PAGEMAX) - returns if TOTALFILES is reached
	LDA FILEINDEX		; Backup original FILEINDEX first !!!!
	STA BACKUP			; (Necessary, as we're are about to mess with FILEINDEX)

	DEC	FILEINDEX	; (added, because with the new FAT32 routines, FILEINDEX starts at 1)
	LDA FILEINDEX
	LSR
	LSR
	LSR				; Divide FILEINDEX by 8 (remove lower nibble)
	STA PAGE		; PAGE number = FILEINDEX / 8
	ASL
	ASL
	ASL				; Multiply FILEINDEX by 8 again to generate PAGEMIN
	STA PAGEMIN
	INC	FILEINDEX

	LDA	PAGE				; Check to see if we've already displayed these file names...
	CMP	OLDPAGE				; ...(speedup, because it's slow to read all file entries every time the arrow moves)
	BNE PAGE_DIFFERENT
	JMP UPDATE_PAGE_DONE	; Quit, if page already displayed

PAGE_DIFFERENT:
	LDA PAGE
	STA	OLDPAGE				; Else (page is different), store PAGE into OLDPAGE and display new page.

	LDA PAGEMIN
	CLC
	ADC #$07
	STA PAGEMAX		; PAGEMAX = PAGEMIN + 7

	INC	PAGEMIN		; (added, because with the new FAT32 routines, FILEINDEX starts at 1)
	INC PAGEMAX
	
	LDA PAGEMIN
	STA FILEINDEX		; Start FILEINDEX at PAGEMIN

	JSR BLANKPPU		; Clear the screen first

	LDA #$A1			; Set start position for where ROM list will print to screen
	STA XPOS
	LDA #$91
	STA YPOS
DO_UPDATE_PAGE:
	JSR	READ_FILE_INDEX		; Read index to generate correct ENTRY_INDEX (also puts correct ENTRY sector into buffer, gets the start cluster and 8.3 file name / ext).

CHECK_IF_EXT_V64:		; Do a quick file extension check to see if it's worth checking first sector for... 
	LDX #$00			; ...a V64 file (byteflipped ROM), then printing the real ROM name (from the ROM header).
	LDA FILE_EXT,X
	CMP	#$56			; "V". Note: All 8.3 DOS filenames are CAPITALS! (No need to check for Lower-case).
	BNE EXT_NOT_MATCHED
	INX
	LDA	FILE_EXT,X
	CMP	#$36			; "6"
	BNE	EXT_NOT_MATCHED
	INX
	LDA	FILE_EXT,X
	CMP	#$34			; "4"
	BNE	EXT_NOT_MATCHED
	JMP	EXT_IS_V64

EXT_NOT_MATCHED:
	JMP NOT_A_V64

EXT_IS_V64:						; File extension matches "V64", check if it really is a byteflipped V64 ROM, then
								; print actual ROM header name to screen (overwrite simple 8.3 filename)

	LDA FILE_START_CLUS3		; Put file start cluster values into CURRENT_CLUS.
	STA CURRENT_CLUS3
	LDA FILE_START_CLUS2
	STA CURRENT_CLUS2
	LDA FILE_START_CLUS1
	STA CURRENT_CLUS1
	LDA FILE_START_CLUS0
	STA CURRENT_CLUS0

	JSR	CONVERT_CLUS_TO_LBA		; Converts cluster number in CURRENT_CLUS3-0 into LBA3-0 values.
								; ...and also reads the next cluster chain number into NEXT_CLUS3-0.

	JSR	IDE_RSEC		; Read first sector of current file into buffer
;	JSR	IDE_RSEC_BURST	; BURST Read first sector of current file into buffer (only read the first 0x60 words, dummy read the rest for speed-up). OzOnE.
	LDA	#$00
	TAX
	LDA	IDEBUF0,X
	CMP	#$37			; Check if first four bytes of file matches a byteflipped N64 (.V64) ROM type.
	BNE	NOT_A_V64
	INX
	LDA	IDEBUF0,X
	CMP	#$80
	BNE	NOT_A_V64
	INX
	LDA	IDEBUF0,X
	CMP	#$40
	BNE	NOT_A_V64
	INX
	LDA	IDEBUF0,X
	CMP	#$12
	BNE	NOT_A_V64
ROM_IS_VALID:				; Current file appears to be a V64 (byteflipped) ROM...
	LDA	XPOS
	JSR	WRITEPPU
	LDA	YPOS
	JSR	WRITEPPU
;	JSR	SHOW_ROMNAME	; ...Display current game name (from ROM header) on screen.
	JSR SHOW_ROMNAME_SWAPPED

PRINT_REGION_CODE:
	LDA #$B6				; Set the cursor X position to just past the ROM name.
	JSR WRITEPPU

	LDA #$3F
	TAX
	LDA	IDEBUF0,X			; Read the Cart's ROM Region (Country Code).
	STA ROM_REGION
	CMP #$44
	BEQ ROM_GERMANY
	CMP #$45
	BEQ ROM_USA
	CMP #$4A
	BEQ ROM_JAPAN
	CMP #$50
	BEQ ROM_EUROPE
	CMP #$55
	BEQ ROM_AUS
	JMP CONTINUE_UPDATE_PAGE	; Region unknown, skip printing.
	
ROM_GERMANY
	LDA #$44		; 'D'
	JSR PRINTASCII
	JMP CONTINUE_UPDATE_PAGE
ROM_USA	
	LDA #$55		; 'U'
	JSR PRINTASCII
	JMP CONTINUE_UPDATE_PAGE
ROM_JAPAN
	LDA #$4A		; 'J'
	JSR PRINTASCII
	JMP CONTINUE_UPDATE_PAGE
ROM_EUROPE
	LDA #$45		; 'E'
	JSR PRINTASCII
	JMP CONTINUE_UPDATE_PAGE
ROM_AUS
	LDA #$41		; 'A'
	JSR PRINTASCII
	JMP CONTINUE_UPDATE_PAGE

NOT_A_V64:				; Branches here if file extension is NOT "V64", or if the first four bytes of file do not match
						; a byteflipped N64 ROM...
	LDA	XPOS			; ...So, just print old style (8.3) file name on screen instead.
	JSR	WRITEPPU
	LDA	YPOS
	JSR	WRITEPPU
;	JSR	READ_FILE_INDEX		; Read index to generate correct ENTRY_INDEX and also read correct ENTRY sector into buffer
							; (Don't think we need to do this, as Entries in buffer are only over-written if file ext IS a V64). OzOnE.

	JSR PRINT_FILENAME		; Print FILE name on screen (at current cursor position) based on ENTRY_INDEX

CONTINUE_UPDATE_PAGE:
	LDA FILEINDEX
	CMP PAGEMAX				; Stop updating current page when FILEINDEX = PAGEMAX
	BEQ UPDATE_PAGE_DONE

	LDA FILEINDEX
	CMP TOTALFILES			; Also, stop updating page if FILEINDEX reaches TOTALFILES
	BEQ UPDATE_PAGE_DONE	

	INC FILEINDEX			; Increment FILEINDEX to point to next entry
	INC YPOS				; Increment YPOS ready for next line
	JMP DO_UPDATE_PAGE		; Loop, to print next file name (until one of the above checks is true).
UPDATE_PAGE_DONE:
	LDA BACKUP
	STA FILEINDEX			; Restore saved FILEINDEX
;	JSR	READ_FILE_INDEX		; Don't think we need this?
	RTS

IDE_RESET:
	JSR	IDE_BUSY	; Wait for drive not busy
	JSR	IDE_DRDY	; Wait for drive to accept commands

	LDA	#$07		; Load A with 07h (Drive/Head Register)
	STA	IDEADD
	LDA	#$08		; Load A with 08h (Reset Hard Drive Command)
	STA	IDELOW
	RTS

IDE_INIT:
	JSR	IDE_BUSY	; Wait for drive not busy
	JSR	IDE_DRDY	; Wait for drive to accept commands

	LDA	#$00		; Set V64 to Normal mode (DRAM/BACKUP Disabled)
	STA	BACKUPMD

	LDA	#$06		; Load A with 06h (Device Head Register)
	STA	IDEADD

	LDA	#$E0		; Load A with E0h (Select MASTER Drive AND Set bits 5-7 for LBA mode)
	STA	IDELOW

	JSR	IDE_BUSY	; Wait for command to complete

	LDA	#$07		; Load A with 07h (Command/Status Register)
	STA	IDEADD
	LDA	#$91		; Load A with 91h (Init Drive Parameters Command)
	STA	IDELOW

	JSR	IDE_BUSY	; Wait for command to complete

	LDA	#$07		; Load A with 07h (Command/Status Register)
	STA	IDEADD
	LDA	#$10		; Load A with 10h (Drive Recalibrate Command)
	STA	IDELOW

	JSR	IDE_BUSY	; Wait for command to complete

;	LDA	#$02		; Load A with 02h (Sector Count Register)
;	STA	IDEADD
;	LDA	#$0a		; Load A with 0Ah (PIO transfer mode 2)
;	STA	IDELOW

;	LDA	#$01		; Load A with 01h (Error/Features Register)
;	STA	IDEADD
;	LDA	#$03		; Load A with 03h (Set Transfer Mode subcommand)
;	STA	IDELOW

;	LDA	#$07		; Load A with 07h (Command/Status Register)
;	STA	IDEADD
;	LDA	#$EF		; Load A with 10h (Set Features Command)
;	STA	IDELOW

;	JSR	IDE_BUSY	; Wait for command to complete

	RTS

; *** Read drive identification sector into buffer
DRIVE_ID:
	JSR	IDE_BUSY	; Wait for command to complete
	JSR	IDE_DRQ		; Wait for drive data request

	LDA	#$07		; Load A with 07h (Status/Command Register)
	STA	IDEADD
	LDA	#$EC		; Load A with ECh (Identify Drive Command)
	STA	IDELOW
	JSR	IDE_BUSY	; Wait for command to complete

	JSR	IDERBLK		; Read data into buffer
	JSR	SWAP_BUFFER	; Swap byte order (for some reason, ASCII strings
					; are sent with opposite-to-normal byte order)
	RTS

IDE_BUSY:
	LDA	#$07		; Load A with 07h (Status/Command Register)
	STA	IDEADD
	LDA	IDELOW		; Load A with IDELOW (Status Byte)
	AND	#$80		; AND A with 80h (Busy bit 7)
	BNE	IDE_BUSY	; Loop if bit set
	RTS

IDE_DRDY:
	LDA	#$07		; Load A with 07h (Status/Command Register)
	STA	IDEADD
	LDA	IDELOW		; Load A with IDELOW (Status Byte)
	AND	#$40		; mask A with 40h (DRDY bit 6)
	BEQ	IDE_DRDY	; Loop if bit NOT set
	RTS

IDE_DRQ:
	LDA	#$07		; Load A with 07h (Status/Command Register)
	STA	IDEADD
	LDA	IDELOW		; Load A with IDELOW (Status Byte)
	AND	#$08		; AND A with 08h (DRQ bit)
	BEQ	IDE_DRQ		; Loop if bit NOT set
	RTS
				
SWAP_BUFFER:		; Byteswap the whole buffer
	LDX	#$00		; Start Word offset at 00h
BUFFER_SWAP0:
	LDA	IDEBUF0,X	; Get byte 1 of Word
	TAY				; Store byte 1 in Y
	
	LDA	IDEBUF0+1,X	; Get byte 2 of Word
	STA	IDEBUF0,X	; Write it to byte 1 position
	
	TYA				; Retrieve byte 1 from Y 
	STA	IDEBUF0+1,X	; Store it to byte 2 position

	INX				; Increment X twice (Words, not Bytes!)
	INX
	BNE	BUFFER_SWAP0	; Loop until all 256 Words are swapped
	RTS

;*** Show Buffer - Display buffer to screen (first half at least)
SHOW_BUFFER:
	LDA	#$A0		; x position
	JSR	WRITEPPU
	LDA	#$90		; y position
	JSR	WRITEPPU

	LDX	#$00		; Start address
NEXT1:
	LDA	IDEBUF0,X	; Load A with IDEBUF0, offset
	CMP	#$10
	BMI	NOTASC1
	CMP	#$7E
	BMI	PRINT
NOTASC1:
	LDA	#$6E		; Use 6Eh (.) instead for non-ascii
PRINT:
	JSR	PRINTASCII
	INX
	CPX	#$FF		; End address
	BNE	NEXT1
	RTS

;*** Show Buffer HEX - Display buffer to screen in HEX (TESTING - modified end address etc.)
SHOW_BUFFER_HEX:
	LDA	#$A0		; x position
	JSR	WRITEPPU
	LDA	#$90		; y position
	STA YPOS
	JSR	WRITEPPU

	LDX	#$00		; Start address
SHOW_NEXT_HEX:
	LDA	IDEBUF0,X	; Load A with IDEBUF0, offset
	JSR	PRINTHEX
	INX
	CPX	#$90		; End address
	BNE	SHOW_NEXT_HEX
	RTS

;*** Show Buffer ASCII - Display buffer to screen as ASCII characters (TESTING - modified end address etc.)
SHOW_BUFFER_ASCII:
	LDA	#$A0		; x position
	JSR	WRITEPPU
	LDA	#$90		; y position
	STA YPOS
	JSR	WRITEPPU

	LDX	#$00		; Start address
SHOW_NEXT_ASCII:
	LDA	IDEBUF0,X	; Load A with IDEBUF0, offset
	JSR	PRINTASCII
	INX
	CPX	#$F0		; End address
	BNE	SHOW_NEXT_ASCII
	RTS
	
SHOW_DRAM_HEX:				; Routine displays first "sector" (512 bytes) of V64 DRAM on screen (in hex)
	LDA			#$08
	STA			$E5

	LDA			#$00		; Set start offset ?
	STA			$16
	LDA			#$00		; Zero other DRAM pointers
	STA			$12
	STA			$13
	STA			$0A
	JSR			L9E50		; Puts variables into C008,9,A,B,C ? (update DRAM pointers)
	LDX			#$20

	LDA			#$A0		; Zero X / Y cursor pos first
	JSR			WRITEPPU	
	LDA			#$90
	JSR			WRITEPPU	
SHOW_DRAM_LOOP:
	LDA			DRAMLOW			; Load A with DRAMLOW
	PHA							; Push A to stack

	LDA			DATAHIGH		; Load A with DATAHIGH
	JSR			PRINTHEX		; WRITE to PPU (in hex)

	PLA							; Pull A (DRAMLOW) from stack
	JSR			PRINTHEX		; WRITE to PPU (in hex)

	INC			$0A			; Increment $0A
	LDA			$0A			; Load A with $0A
	CMP			#$1F		; Check if reg $0A has reached max (TESTING - modified to show enough on screen to allow debug)
	BNE			SHOW_DRAM_LOOP	; Write next char in DRAM to PPU until max
	RTS

;*** Show Rom name - Display name of ROM in buffer to screen (first sector of ROM should be put into buffer and BYTESWAPPED first if V64)
SHOW_ROMNAME:
	LDX	#$20		; Start address
ROMNAME_NEXT:
	LDA	IDEBUF0,X	; Load A with IDEBUF0, offset
	JSR PRINT_CHECKED_ASCII
	INX
	CPX	#$34		; End address
	BNE	ROMNAME_NEXT
	RTS

;*** Show Rom name SWAPPED - Display name of ROM in buffer to screen (first sector of ROM should be put into buffer - No need to BYTESWAP V64 first!)
SHOW_ROMNAME_SWAPPED:
	LDX	#$20		; Start address
SWAPPED_NEXT:
	LDA	IDEBUF0,X	; Load A with IDEBUF0, offset
	TAY
	INX
	LDA	IDEBUF0,X	; Load A with IDEBUF0, offset
	JSR PRINT_CHECKED_ASCII
	TYA
	JSR PRINT_CHECKED_ASCII
	INX
	CPX	#$34		; End address
	BNE	SWAPPED_NEXT
	RTS
	
PRINT_CHECKED_ASCII:	
	CMP	#$10
	BMI	NOTASC3
	CMP	#$7E
	BMI	PRINT3
NOTASC3:
	LDA	#$6E		; Use 6Eh (.) instead for non-ascii
PRINT3:
	JSR	PRINTASCII
	RTS

; *** Write LBA values to drive registers
; Input: LBA at LBA+0-LBA+3
WRITE_LBA:
	JSR	IDE_BUSY	; Wait for drive to not be busy

	LDA	#$02		; Load A with 02h (Sector Count Register)
	STA	IDEADD
	LDA	IDESECC		; Load A with Sector Count
	STA	IDELOW		; Store it to drive

	LDA	#$03		; Load A with 03h (Sector Register)
	STA	IDEADD 
	LDA	LBA0		; Load LBA0 into A
	STA	IDELOW		; Store it to drive

	LDA	#$04		; Load A with 04h (Cylinder Low Register)
	STA	IDEADD 
	LDA	LBA1		; Load LBA1 into A
	STA	IDELOW		; Store it to drive

	LDA	#$05		; Load A with 05h (Cylinder High Register)
	STA	IDEADD 
	LDA	LBA2		; Load LBA2 into A
	STA	IDELOW		; Store it to drive

	LDA	#$06		; Load A with 06h (Drive/Head Register)
	STA	IDEADD 
	LDA	LBA3		; Load LBA3 into A

	ORA	#$E0		; OR A with E0h    --- Mask LBA accept bits ---  (use E0h to select MASTER drive)

	STA	IDELOW		; Store it to drive
	RTS

; *** Read 1 sector into IDEBUF (both halves are read, and split between IDEBUF0 and IDEBUF1)
IDE_RSEC:
	LDA	#$01		; Set Sector Count to 1
	STA	IDESECC

	JSR	WRITE_LBA	; Write LBA registers and sector count to HDD

	LDA	#$07		; Load A with 07h (Status/Command Register)
	STA	IDEADD
	LDA	#$20		; Send command: $20 = read sector(s)
	STA	IDELOW
;	JSR	IDE_BUSY	; Wait for command to complete
	JSR	IDE_DRQ		; Wait for data ready

	JSR	IDERBLK		; Read 256 Words into memory
	RTS

IDE_RSEC_BURST:
	LDA	#$01		; Set Sector Count to 1
	STA	IDESECC

	JSR	WRITE_LBA	; Write LBA registers and sector count to HDD

	LDA	#$07		; Load A with 07h (Status/Command Register)
	STA	IDEADD
	LDA	#$20		; Send command: $20 = read sector(s)
	STA	IDELOW
	JSR	IDE_BUSY	; Wait for command to complete
	JSR	IDE_DRQ		; Wait for data ready

	JSR	IDERBLK_BURST	; BURST Read 256 Words into memory (do a dummy read of unneeded words).
	RTS
	
; *** Write 1 sector from IDEBUF0
IDE_WSEC:
	LDA	#$01		; Set Sector Count to 1
	STA	IDESECC

	JSR	WRITE_LBA	; Write LBA registers and sector count to HDD

	JSR	IDE_DRDY	; Wait for drive ready

	LDA	#$07		; Load A with 07h (Status/Command Register)
	STA	IDEADD
	LDA	#$30		; Set command: $30 = Write sector(s) Command
	STA	IDELOW
	JSR	IDE_DRQ		; Wait for drive to request data

	JSR	IDEWBLK		; Write 256 Words to drive
	RTS

; *** Read 1 sector from HDD into DRAM
IDE_R2DRAM:
	JSR	WRITE_LBA	; Write LBA registers and sector count to HDD
	
	LDA	#$07		; Load A with 07h (Status/Command Register)
	STA	IDEADD
	LDA	#$20		; Send command: $20 = read sector(s)
	STA	IDELOW
	JSR	IDE_BUSY	; Wait for command to complete
	JSR	IDE_DRQ		; Wait for data ready
	
	JSR	IDE2DRAM	; Read 256 Words into DRAM
	RTS

; *** Read 256 sectors from HDD into DRAM
IDE_R2DRAM_256:
	LDA	#$00		; Set Sector Count to 256 (00h)
	STA	IDESECC

	JSR	WRITE_LBA	; Write LBA registers and sector count to HDD

	LDA	#$07		; Load A with 07h (Status/Command Register)
	STA	IDEADD
	LDA	#$20		; Send command: $20 = read sector(s)
	STA	IDELOW
	JSR	IDE_BUSY	; Wait for command to complete

	LDA	#$00		; (read 256 sectors)
	STA TEMP2
IDE_R2DRAM_256_LOOP:
	JSR	IDE2DRAM		; Read 1 sector (256 words / 512 bytes) into DRAM
	INC	$12				; increment DRAM "sector" pointer (512 bytes)
	JSR L9E50			; Update DRAM sector pointers

	DEC TEMP2
	BNE IDE_R2DRAM_256_LOOP	; Repeat sector reading until TEMP2 = 00h (read 256 sectors)
	RTS

; *** Read whole CLUSTER from HDD into DRAM (number of sectors transferred = SECT_PER_CLUS)
IDE_R2DRAM_CLUSTER:
	LDA	SECT_PER_CLUS	; Set the Sector Count to the number given by the SECT_PER_CLUS param.
	STA	IDESECC
	STA	TEMP2			; Also put SECT_PER_CLUS in TEMP2 (to use as the sector read loop count)

	JSR	WRITE_LBA	; Write LBA registers and sector count to HDD

	LDA	#$07		; Load A with 07h (Status/Command Register)
	STA	IDEADD
	LDA	#$20		; Send command: $20 = read sector(s)
	STA	IDELOW
	JSR	IDE_BUSY	; Wait for command to complete
; NOTE: I tried removing "JSR L9E50" from the loop just to test if it would speed up loading,
; but it took exactly the same 45 seconds to load a 96Mbit ROM from my CF card...
; So, the bottleneck is that the V64 CPU clock is too slow.
;
; UPDATE 3-2-12: Loading has been improved (by about 33%) by transferring in 16-Word chunks.
; It IS the V64 CPU that is too slow. The loading speed is dependant on the V64 instruction cycle.
; It now takes only 30 seconds to load a 96Mbit ROM from CF.
; I still don't know exactly how the V64 is set up to transfer from CD as quickly as it does? OzOnE.
IDE_R2DRAM_CLUSTER_LOOP:
	JSR	IDE2DRAM		; Read 1 sector (256 words / 512 bytes) into DRAM
	INC	$12				; increment DRAM "sector" pointer (512 bytes)
	JSR L9E50			; Update DRAM sector pointers

	DEC TEMP2			; Number of sectors to transfer (set by "SECT_PER_CLUS" above)
	BNE IDE_R2DRAM_CLUSTER_LOOP	; Repeat sector reading until TEMP2 = 0
	RTS

; *** Read block of data from hard drive to V64 DRAM (256 Words / 512 Bytes / 1 Sector)
; Input: called from IDE_R2DRAM. IDE2DRAM0 reads first words bytes to buffer.
; (The HIGH byte is routed automatically by the Gate Array chip.)
IDE2DRAM:
	JSR	IDE_DRQ			; Wait for data ready

	LDA	#$00			; Load A with 00h (Select Data Register)
	STA	IDEADD

	LDX	#$10			; Set X to 0x10, 16 decimal (loop count)
IDE2DRAMLOOP:
	LDA	IDELOW			; Read low byte from drive (HIGH byte gets read onto bus too!)
	STA DRAMLOW			; Write low byte to DRAM (HIGH byte gets written to DRAM too!)
	LDA	IDELOW			; Need to transfer the Words in a chunk so that the INX / CMP / BNE
	STA DRAMLOW			; instructions don't cause a slow down in the ROM loading.
	LDA	IDELOW
	STA DRAMLOW			; NOTE: Reading more than 16 Words at a time didn't seem to
	LDA	IDELOW			; speed up ROM loading any further.
	STA DRAMLOW
	LDA	IDELOW
	STA DRAMLOW
	LDA	IDELOW
	STA DRAMLOW
	LDA	IDELOW
	STA DRAMLOW
	LDA	IDELOW
	STA DRAMLOW
	LDA	IDELOW
	STA DRAMLOW
	LDA	IDELOW
	STA DRAMLOW
	LDA	IDELOW
	STA DRAMLOW
	LDA	IDELOW
	STA DRAMLOW
	LDA	IDELOW
	STA DRAMLOW
	LDA	IDELOW
	STA DRAMLOW
	LDA	IDELOW
	STA DRAMLOW
	LDA	IDELOW
	STA DRAMLOW
	DEX					; Decrement X
	BNE	IDE2DRAMLOOP	; Read next 16 Words into buffer until X=00h (reads 256 words)
						; / Check if we've read 16 Words, 16 (decimal) times. ie. 256 Words.
	RTS

; *** Read 1 sector from HDD into MPEG chip
IDE_R2MPEG:
	LDA	#$01		; Set Sector Count to 1
	STA	IDESECC

	JSR	WRITE_LBA	; Write LBA registers and sector count to HDD
	
	LDA	#$07		; Load A with 07h (Status/Command Register)
	STA	IDEADD
	LDA	#$20		; Send command: $20 = read sector(s)
	STA	IDELOW
	JSR	IDE_BUSY	; Wait for command to complete
	JSR	IDE_DRQ		; Wait for data ready

	JSR	IDE2MPEG	; Read 256 Words into MPEG chip
	RTS

; **** Transfers 256 words from HDD to MPEG chip (1 sector)
; Input: called from IDE_R2MPEG. Waits for MPEG FIFO to be empty before transferring next block of 32 words.
; Finishes transferring after 8 blocks of 32 words (256 words total)
IDE2MPEG:
	LDA	#$00		; Load A with 00h (Select Data Register)
	STA	IDEADD

	LDY	#$08		; Start Y at 08h (8) = number of times to transfer 32 words.
WAIT_FOR_FIFO:
	LDA	MPEG_IDPR		; Load A with MPEG_IDPR (read Word Remaining Register) to see if MPEG FIFO is empty
	AND	#$3F			; Mask lower bits (5:0)
	BNE	WAIT_FOR_FIFO	; Loop until WRR is 0 (= FIFO empty), transfer 32 more words immediately !!!!....

	LDX	#$20			; Start X at 20h (32) = transfer 32 words
IDE2MPEG_INNER_LOOP:
	LDA	IDELOW			; Read low byte from drive (HIGH byte gets read onto bus too!)
	STA MPEG_IDPR		; Write low byte to MPEG chip bitstream input (HIGH byte gets written to MPEG too!)
	DEX	
	BNE	IDE2MPEG_INNER_LOOP	; Read next Word into buffer until X=00h (transfers 32 words, as X
							; is incremented by 08h after each word (256 / 8)
	DEY
	BNE WAIT_FOR_FIFO	; Send another 32 words, until Y reaches 00h (256 words in total are sent to MPEG before RTS)				
	RTS

; *** Read block of data from hard drive into buffer (256 Words / 512 Bytes / 1 Sector)
; Input: called from IDE_RSEC or DRIVE_ID. IDERBLK0 reads first 256 bytes to buffer, IDERBLK1 reads second half
IDERBLK:
	LDA	#$00		; Load A with 00h (Select Data Register)
	STA	IDEADD

	LDX	#$00		; Start Word at index 00h (X = 00-7F 256 bytes)
IDERBLK0:
	LDA	IDELOW		; Read low byte from drive
	STA	IDEBUF0,X	; store low byte to first half of buffer
	INX				; Increment X
	LDA	DATAHIGH	; Read high byte from drive
	STA	IDEBUF0,X	; store high byte to first half of buffer
	INX				; Increment X
	BNE	IDERBLK0	; Read next Word into buffer until X=00h
IDERBLK1:
	LDA	IDELOW		; Read low byte from drive
	STA	IDEBUF1,X	; store low byte to second half of buffer
	INX				; Increment X
	LDA	DATAHIGH	; Read high byte from drive
	STA	IDEBUF1,X	; store high byte to second half of buffer
	INX				; Increment X
	BNE	IDERBLK1	; Read next Word into buffer until X=00
	RTS

; *** BURST Read block of data from hard drive into buffer (256 Words / 512 Bytes / 1 Sector)
; This will only read a specific number of words into the buffer, and then do a "dummy" read of the rest of the sector
; (to speed up sector reading when we only want a few words for displaying the ROM / File names etc).
; Input: called from IDE_RSEC_BURST. IDERBLK0_BURST reads the first bytes to the buffer (up to the CMP amount),
; IDERBLK0_BURST_REST does a "dummy" read of the rest of the first sector half...
; IDERBLK1_BURST does a "dummy" read of the second sector half.
IDERBLK_BURST:
	LDA	#$00			; Load A with 00h (Select Data Register)
	STA	IDEADD

	LDX	#$00			; Start Word at index 00h (X = 00-7F 256 bytes)
IDERBLK0_BURST:
	LDA	IDELOW			; Read low byte from drive
	STA	IDEBUF0,X		; store low byte to first half of buffer
	INX					; Increment X
	LDA	DATAHIGH		; Read high byte from drive
	STA	IDEBUF0,X		; store high byte to first half of buffer
	INX					; Increment X
	CMP #$60
	BNE	IDERBLK0_BURST	; Read next Word into buffer until X=00h
IDERBLK0_REST:
	LDA	IDELOW			; Read low byte from drive
	INX					; Increment X
	INX					; Increment X
	BNE	IDERBLK0_REST	; Read next Word into buffer until X=00

IDERBLK1_BURST:
	LDA	IDELOW			; Read low byte from drive
	INX					; Increment X
	INX					; Increment X (ignore the high byte)
	BNE	IDERBLK1_BURST	; Read next Word into buffer until X=00
	RTS

; *** Write block of data from buffer to hard drive (256 Words / 512 bytes / 1 Sector)
; Input: Called from IDEWSEC
IDEWBLK:
	LDA	#$00		; Load A with 00h (Select Data Register)
	STA	IDEADD

	LDX	#$00		; Start Word at index 00h (X = 00-FF or 512 bytes)
IDEWBLK0:
	LDA	IDEBUF0,X	; Read high byte from first half of buffer
	STA	DATAHIGH	; Store high byte to drive
	INX				; Increment X
	LDA	IDEBUF0,X	; Read low byte from first half of buffer
	STA	IDELOW		; Store low byte to drive
	INX				; Increment X
	BNE	IDEWBLK0	; Read next Word into buffer until X=00h

IDEWBLK1:
	LDA	IDEBUF1,X	; Read high byte from second half of buffer
	STA	DATAHIGH	; Store high byte to drive
	INX				; Increment X
	LDA	IDEBUF1,X	; Read low byte from second half of buffer
	STA	IDELOW		; Store low byte to drive
	INX				; Increment X
	BNE	IDEWBLK1	; Read next Word into buffer until X=00
	RTS

; *** Read block of data from DRAM to Buffer (512 bytes / 256 words)
; Input:
DRAMREAD:
	LDA	#$00
	STA	DRAMADD1	; Set DRAMADD+1 to 00h
	STA	DRAMADD2	; Set DRAMADD+2 to 00h
	STA	DRAMADD3	; Set DRAMADD+3 to 00h
	LDA	#$80
	STA	DRAMADD0	; Setup DRAM start Address

	LDX	#$00		; Start at Buffer index 00h
DRAMREAD0:
	LDA	DRAMLOW		; Read low byte from DRAM
	STA	IDEBUF0,X	; Store byte to buffer, offset
	INX				; Increment X
	LDA	DATAHIGH	; Read high byte from DRAM
	STA	IDEBUF0,X	; Store byte to buffer+1, offset
	INX				; Increment X
	BNE	DRAMREAD0	; Read next Word into buffer until X=0
	RTS

; *** Write block of data from buffer to DRAM (512 bytes / 256 words)
; Input: just call it. Loaded Roms are byteswapped as High byte needs to be written to DRAM first
DRAMWRITE:

	LDX	#$00		; Start at Buffer offset 00h
DRAMWRITE0:
	LDA	IDEBUF0,X	; Read low byte from first half of buffer
	STA	DATAHIGH	; Store low byte to DATAHIGH
	INX			; Increment X
	LDA	IDEBUF0,X	; Read high byte from first half of buffer
	STA	DRAMLOW		; Store high byte to DRAMLOW
	INX			; Increment X
	BNE	DRAMWRITE0	; Write next Word into DRAM until X=00 (X = 00-FF)

DRAMWRITE1:
	LDA	IDEBUF1,X	; Read high byte from second half of buffer
	STA	DATAHIGH	; Store high byte to DATAHIGH
	INX				; Increment X
	LDA	IDEBUF1,X	; Read low byte from second half of buffer
	STA	DRAMLOW		; Store low byte to DRAMLOW
	INX				; Increment X
	BNE	DRAMWRITE1	; Read next Word into DRAM until X=00h
	RTS

WAIT_FOR_PLAY_PRESSED:
	JSR READKEYPAD
	LDA $8C
	CMP #$10
	BNE WAIT_FOR_PLAY_PRESSED
WAITNOTPRESSED_2:
	JSR READKEYPAD
	LDA $8C
	BNE WAITNOTPRESSED_2	; Wait until key NOT pressed (so we don't get repeated triggering)
	RTS

HDD_ERROR:
	JSR			PRINT_STOP
	
	LDA	#$A0
	JSR	WRITEPPU
	LDA	#$95
	JSR	WRITEPPU
	LDA #$4f			; "O"
	JSR PRINTASCII
	LDA #$46			; "F"
	JSR PRINTASCII
	LDA #$3a			; ":"
	JSR PRINTASCII
	LDA #$20			; " "
	JSR PRINTASCII
	LDA TEMP2
	JSR	PRINTHEX

	LDA	#$A0
	JSR	WRITEPPU
	LDA	#$96
	JSR	WRITEPPU
	LDA #$44			; "D"
	JSR PRINTASCII
	LDA #$41			; "A"
	JSR PRINTASCII
	LDA #$3a			; ":"
	JSR PRINTASCII
	LDA #$20			; " "
	JSR PRINTASCII
	LDA TEMP
	JSR	PRINTHEX

	JSR			BLINKON
	JSR			PRINTSTRING
	
	.byte		$00, $02
	.word		DriveErrorMSG		; "HDD ERROR"
	
	JSR	BLINKOFF
	JSR	SHOW_BUFFER_ASCII
	
	JSR	WAIT_FOR_PLAY_PRESSED
	JMP LD33F			; Start over (display main "Bung" screen etc.)

IDEStart:
	JSR			PRINTSTRING
				
	.byte		$01, $03
	.word		WaitingMSG

	JSR	IDE_RESET
	JSR	IDE_INIT

	JSR BLANKPPU

WAITNOTPRESSED:
	JSR READKEYPAD
	LDA $8C
	BNE WAITNOTPRESSED	; Wait until key NOT pressed (so we don't get repeated triggering)

READ_MBR:
	LDA #$00			; Zero LBA start address to read MBR
	STA	LBA0
	STA	LBA1
	STA	LBA2
	STA	LBA3
	JSR	IDE_RSEC		; Read MBR into buffer (LBA values get written to drive before reading sector)

CHECK_MBR_SIG:
	LDX	#$FE
	LDA IDEBUF1,X		; Read first sig byte from MBR (actually reading 0x1FE - second half of buffer)
	CMP #$55			; Should match 0x55
	BEQ CHECK_SECOND_SIG	; NOTE: Had to change branch and add CHECK_SECOND_SIG to fix "branch out of range" error
	JMP NO_MATCH
CHECK_SECOND_SIG:
	INX
	LDA IDEBUF1,X		; Read second sig byte from MBR (actually reading 0x1FF - second half of buffer)
	CMP #$AA			; Should match 0xAA
	BNE NO_MATCH

CHECK_IF_AT_VOL_ID:		; For some reason, the volume ID sometimes appears at sector 0 when disk is formatted via Windows?
	LDX	#$52
	LDA IDEBUF0,X
	CMP #$46			; "F"
	BNE JUMP_TO_VOL_ID
	INX
	LDA IDEBUF0,X
	CMP #$41			; "A"
	BNE JUMP_TO_VOL_ID
	INX
	LDA IDEBUF0,X
	CMP #$54			; "T"
	BNE JUMP_TO_VOL_ID
	INX
	LDA IDEBUF0,X
	CMP #$33			; "3"
	BNE JUMP_TO_VOL_ID
	INX
	LDA IDEBUF0,X
	CMP #$32			; "2"
	BNE JUMP_TO_VOL_ID
	JMP	READ_FAT32_BPB_PARAMS	; All matched (Volume ID was at sector 0), read params...

JUMP_TO_VOL_ID:			; ...else, use "LBA Begin" param from MBR to jump to Volume ID, then try checking for "FAT32" again
	LDX	#$C6
	LDA IDEBUF1,X
	STA PART_BEGIN_LBA0
	STA LBA0
	INX
	LDA IDEBUF1,X
	STA PART_BEGIN_LBA1
	STA LBA1
	INX
	LDA IDEBUF1,X
	STA PART_BEGIN_LBA2
	STA LBA2
	INX
	LDA IDEBUF1,X
	STA PART_BEGIN_LBA3
	STA LBA3
	
	JSR IDE_RSEC
	
RETRY_READ_VOL_ID:
	LDX	#$52
	LDA IDEBUF0,X
	CMP #$46			; "F"
	BNE NO_MATCH
	INX
	LDA IDEBUF0,X
	CMP #$41			; "A"
	BNE NO_MATCH
	INX
	LDA IDEBUF0,X
	CMP #$54			; "T"
	BNE NO_MATCH
	INX
	LDA IDEBUF0,X
	CMP #$33			; "3"
	BNE NO_MATCH
	INX
	LDA IDEBUF0,X
	CMP #$32			; "2"
	BNE NO_MATCH
	JMP	READ_FAT32_BPB_PARAMS	; All matched (Volume ID was where it should be), read params...

NO_MATCH:			; ...else, one (or none) of the letters in "FAT32" matched where they should do...
	STA TEMP
	STX	TEMP2
	JMP	HDD_ERROR	; Display the offset and data of the mismatch on screen (for DEBUG)
	
READ_FAT32_BPB_PARAMS:
	LDX	#$0D
	LDA IDEBUF0,X
	STA	SECT_PER_CLUS

	LDX	#$0E
	LDA IDEBUF0,X
	STA	NUM_RESV_SECT0
	INX
	LDA IDEBUF0,X
	STA	NUM_RESV_SECT1

	LDX	#$10
	LDA IDEBUF0,X
	STA	NUM_FATS
	
	LDX	#$24
	LDA IDEBUF0,X
	STA	SECT_PER_FAT0
	INX
	LDA IDEBUF0,X
	STA	SECT_PER_FAT1
	INX
	LDA IDEBUF0,X
	STA	SECT_PER_FAT2
	INX
	LDA IDEBUF0,X
	STA	SECT_PER_FAT3
	
	LDX	#$2C
	LDA IDEBUF0,X
	STA	ROOT_DIR_FIRST_CLUS0
	INX
	LDA IDEBUF0,X
	STA	ROOT_DIR_FIRST_CLUS1
	INX
	LDA IDEBUF0,X
	STA	ROOT_DIR_FIRST_CLUS2
	INX
	LDA IDEBUF0,X
	STA	ROOT_DIR_FIRST_CLUS3

;
;	Need to calc FAT_BEGIN_LBA = PART_BEGIN_LBA + NUM_RESV_SECT...
;
	LDA PART_BEGIN_LBA0		; Copy PART_BEGIN_LBA into FAT_BEGIN_LBA to start with...
	STA FAT_BEGIN_LBA0
	LDA PART_BEGIN_LBA1
	STA FAT_BEGIN_LBA1
	LDA PART_BEGIN_LBA2
	STA FAT_BEGIN_LBA2
	LDA PART_BEGIN_LBA3
	STA FAT_BEGIN_LBA3
	
	CLD						; Clear decimal mode (just in case)
	CLC						; ...Clear carry bit before we begin
	LDA FAT_BEGIN_LBA0		; Put byte 0 of FAT_BEGIN_LBA into A
	ADC NUM_RESV_SECT0		; Add the low byte of NUM_RESV_SECT to it
	STA FAT_BEGIN_LBA0		; Store the result back to FAT_BEGIN_LBA0
	
	LDA FAT_BEGIN_LBA1		; Put byte 1 of FAT_BEGIN_LBA into A
	ADC NUM_RESV_SECT1		; Add the high byte of NUM_RESV_SECT to it (plus the carry, if set)
	STA FAT_BEGIN_LBA1		; Store the result back to FAT_BEGIN_LBA1

	LDA	FAT_BEGIN_LBA2		; Load FAT_BEGIN_LBA2
	ADC	#$00				; Add 0x00 (add carry only, if set)
	STA	FAT_BEGIN_LBA2		; Store back.
	
	LDA	FAT_BEGIN_LBA3		; Load FAT_BEGIN_LBA3
	ADC	#$00				; Add 0x00 (add carry only, if set)
	STA	FAT_BEGIN_LBA3		; Store back.

;
;	Need to calc CLUS_BEGIN_LBA = (Number_of_FATs * Sectors_Per_FAT) + FAT_BEGIN_LBA
;
	ASL SECT_PER_FAT0	; Just multiply SECT_PER_FAT by 2 for now (Number of FATs is pretty much always 2 anyway)
	ROL SECT_PER_FAT1
	ROL SECT_PER_FAT2
	ROL SECT_PER_FAT3
	
	CLD						; Clear decimal mode (just in case)
	CLC						; ...Clear carry bit before we begin
	LDA SECT_PER_FAT0		; Put byte 0 of SECT_PER_FAT into A
	ADC FAT_BEGIN_LBA0		; Add byte 0 of FAT_BEGIN_LBA to it
	STA CLUS_BEGIN_LBA0		; Store the result back to CLUS_BEGIN_LBA
	
	LDA SECT_PER_FAT1		; Put byte 1 of SECT_PER_FAT into A
	ADC FAT_BEGIN_LBA1		; Add byte 1 of FAT_BEGIN_LBA to it (plus carry, if set)
	STA CLUS_BEGIN_LBA1		; Store the result back to CLUS_BEGIN_LBA

	LDA SECT_PER_FAT2		; Put byte 2 of SECT_PER_FAT into A
	ADC FAT_BEGIN_LBA2		; Add byte 2 of FAT_BEGIN_LBA to it (plus carry, if set)
	STA CLUS_BEGIN_LBA2		; Store the result back to CLUS_BEGIN_LBA

	LDA SECT_PER_FAT3		; Put byte 3 of SECT_PER_FAT into A
	ADC FAT_BEGIN_LBA3		; Add byte 3 of FAT_BEGIN_LBA to it (plus carry, if set)
	STA CLUS_BEGIN_LBA3		; Store the result back to CLUS_BEGIN_LBA
CLUS_BEGIN_CALC_DONE:

;
;	***** FAT32 PARAM CALCS DONE - START READING FILE ENTRIES  !!! *****
;
GET_TOTALFILES:
	LDA #$00				; Zero TOTALFILES first
	STA TOTALFILES
	LDA #$FF				; Set FILEINDEX to 0xFF to select "get TOTALFILES" mode (254 file limit for time being - OzOnE).
	STA FILEINDEX
	JSR	READ_FILE_INDEX		; Should return with TOTALFILES set. (or will display error if no files, or more than 254 file entries found.)

LIGHTS_CAMERA_MENU:
	LDA FILE_START_CLUS3		; Put file start cluster values into CURRENT_CLUS.
	STA CURRENT_CLUS3
	LDA FILE_START_CLUS2
	STA CURRENT_CLUS2
	LDA FILE_START_CLUS1
	STA CURRENT_CLUS1
	LDA FILE_START_CLUS0
	STA CURRENT_CLUS0

;	LDA #$A0
;	JSR WRITEPPU
;	LDA #$93
;	JSR WRITEPPU
;	LDA CURRENT_CLUS3	; Print current cluster number to screen (DEBUG)
;	JSR	PRINTHEX
;	LDA CURRENT_CLUS2
;	JSR	PRINTHEX
;	LDA CURRENT_CLUS1
;	JSR	PRINTHEX
;	LDA CURRENT_CLUS0
;	JSR	PRINTHEX

SHOW_FILE_LIST:
	LDA	#$FF
	STA	OLDPAGE			; Initialize OLDPAGE (doesn't really matter what, as long as it's not zero)

	LDA #$01
	STA FILEINDEX		; Point FILEINDEX to first file! (1)
DRAWARROW:				; NOTE - Loops back here every time FILEINDEX gets changed by FR / FF buttons
	JSR UPDATE_PAGE		; Update PAGE (display ROM list) based on current FILEINDEX (only redraws file list if page has changed)

;	LDA	#$A0
;	JSR	WRITEPPU
;	LDA	#$98
;	JSR	WRITEPPU
;	LDA ENTRY_ATTRIB
;	JSR PRINTHEX

;	LDA	#$A0
;	JSR	WRITEPPU
;	LDA	#$99
;	JSR	WRITEPPU
;	LDA ENTRY_TYPE
;	JSR PRINTHEX

;	LDA #$A0
;	JSR WRITEPPU
;	LDA #$9a
;	JSR WRITEPPU
;	LDA ENTRY_INDEX1
;	JSR	PRINTHEX
;	LDA ENTRY_INDEX0
;	JSR	PRINTHEX

;	LDA #$A0
;	JSR WRITEPPU
;	LDA #$9b
;	JSR WRITEPPU
;	LDA FILE_START_CLUS3
;	JSR	PRINTHEX
;	LDA FILE_START_CLUS2
;	JSR	PRINTHEX
;	LDA FILE_START_CLUS1
;	JSR	PRINTHEX
;	LDA FILE_START_CLUS0
;	JSR	PRINTHEX

	LDA FILEINDEX
	STA	TEMP3
	DEC TEMP3			; Need to decrement FILEINDEX (because it now starts from 1 with the new FAT32 routines)
	LDA	TEMP3			; Load back into A
	AND	#$07			; Mask off the lower three bits so ARROWPOS cycles through 0-7.
	STA ARROWPOS
	INC ARROWPOS		; Need to increment the result by 1 again to make ARROWPOS cycles through 1-8 instead.

	LDA #$A0			; Set X offset for selection arrow
	JSR WRITEPPU
	LDA ARROWPOS		; ARROWPOS = vertical position of ROM choice - cycles through 0 to 7 (calculated from FILEINDEX)
	CLC
	ADC #$90			; Add #$90 to ARROWPOS to set proper Y position on PPU
	JSR WRITEPPU
	LDA #$0C			; Print arrow on screen to point to chosen ROM name
	JSR WRITEPPU

;	LDA	#$B6			; Print to screen so I can see at least something happening
;	JSR	WRITEPPU
;	LDA	#$90
;	JSR	WRITEPPU
;	LDA FILEINDEX		; Print FILEINDEX on screen (for debugging.)
;	JSR PRINTHEX
	
;	LDA	#$B6			; Print to screen so I can see at least something happening
;	JSR	WRITEPPU
;	LDA	#$91
;	JSR	WRITEPPU
;	LDA ARROWPOS		; Print ARROWPOS on screen (for debugging.)
;	JSR PRINTHEX

;	LDA	#$B6			; Print to screen so I can see at least something happening
;	JSR	WRITEPPU
;	LDA	#$92
;	JSR	WRITEPPU
;	LDA PAGEMIN			; Print PAGEMIN on screen (for debugging.)
;	JSR PRINTHEX

;	LDA	#$B6			; Print to screen so I can see at least something happening
;	JSR	WRITEPPU
;	LDA	#$93
;	JSR	WRITEPPU
;	LDA PAGEMAX			; Print PAGEMAX on screen (for debugging.)
;	JSR PRINTHEX

;	LDA	#$B6			; Print to screen so I can see at least something happening
;	JSR	WRITEPPU
;	LDA	#$94
;	JSR	WRITEPPU
;	LDA PAGE			; Print PAGE on screen (for debugging.)
;	JSR PRINTHEX
	
;	LDA	#$B6			; Print to screen so I can see at least something happening
;	JSR	WRITEPPU
;	LDA	#$95
;	JSR	WRITEPPU
;	LDA TOTALFILES			; Print TOTALFILES on screen (for debugging.)
;	JSR PRINTHEX

READKEYLOOP:
	JSR READKEYPAD
;	LDA	#$A0
;	JSR	WRITEPPU
;	LDA	#$99
;	JSR	WRITEPPU
;	LDA $8C				; TESING (Print keypad hex to screen)
;	JSR PRINTHEX

	LDA $8C
	CMP #$01
	BEQ FFPRESSED

	LDA $8C
	CMP #$02
	BEQ FRPRESSED

	LDA $8C
	CMP #$10
	BEQ PLAYPRESSED
	
	LDA $8C
	CMP #$20
	BEQ STOPPRESSED
	JMP READKEYLOOP

FFPRESSED:
	JSR READKEYPAD
	LDA $8C
	BNE FFPRESSED		; Wait until key NOT pressed
	
	LDA #$A0			; Set X position to 0
	JSR WRITEPPU
	LDA ARROWPOS
	CLC
	ADC #$90			; Add #$90 to ARROWPOS to set proper Y position on PPU
	JSR WRITEPPU
	LDA #$10			; Blank current arrow
	JSR WRITEPPU

	LDA FILEINDEX
	CMP	TOTALFILES				; Check to see if FILEINDEX has reached last ROM
	BEQ FILEINDEX_UPPER_LIMIT	; Set FILEINDEX back to 1 if TOTALFILES (max) reached - ie. wrap.
	
	INC FILEINDEX				; Increment FILEINDEX (ARROWPOS gets updated by UPDATE_PAGE routine)
	JMP DRAWARROW				; Loop
	
FILEINDEX_UPPER_LIMIT:			; FILEINDEX has reached last ROM
	LDA #$01
	STA FILEINDEX				; Set FILEINDEX back to 1 (wrap)
	JMP DRAWARROW				; Loop

FRPRESSED:
	JSR READKEYPAD
	LDA $8C
	BNE FRPRESSED		; Wait until key NOT pressed

	LDA #$A0			; Set X to 0
	JSR WRITEPPU
	LDA ARROWPOS
	CLC
	ADC #$90			; Add #$90 to ARROWPOS to set proper Y position on PPU
	JSR WRITEPPU
	LDA #$10			; Blank current arrow
	JSR WRITEPPU

	LDA FILEINDEX
	CMP	#$01
	BEQ FILEINDEX_LOWER_LIMIT	; Check to see if FILEINDEX = 1....
	
	DEC FILEINDEX			; Decrement FILEINDEX (ARROWPOS gets updated by UPDATE_PAGE routine)
	JMP DRAWARROW			; Loop

FILEINDEX_LOWER_LIMIT:		; FILEINDEX has reached 1...
	LDA TOTALFILES
	STA FILEINDEX			; Set  FILEINDEX to TOTALFILES, ie. wrap to last ROM
	JMP DRAWARROW			; Loop
	
STOPPRESSED:
	JSR READKEYPAD
	LDA $8C
	BNE STOPPRESSED			; Wait until key NOT pressed
	JMP LD33F				; Restart V64 (jump to main startup screen)

PLAYPRESSED:
	JSR READKEYPAD
	LDA $8C
	BNE PLAYPRESSED			; Wait until key NOT pressed

;PICTURE_TEST
;	JSR	LD73F		; Init MPEG chip for playback.

;	LDA #$88		; MPEG Proc. Decoder command register
;	LDX #$11		; Write 11h (WRQ - Write ReQuest command, ready for Still image).
;	JSR	WRITEMPEG	; Write the command.

;	JSR	LE866		; Wait for CACK (Command ACKnowledge bit in MPEG status reg).

;	JSR DMRAMACCESS
	
;	LDA #$7A		; Set start address for writing the image to MPEG DRAM...
;	STA	MPEG_DARH
;	LDA #$00
;	STA	MPEG_DARL

;	LDX #$00
;	LDA #$0E
;PIC_LOOP1
;	STA	MPEG_DDPR	; Write to MPEG DRAM. (TESTING).
;	STX	MPEG_DARL
;	INX
;	BNE PIC_LOOP1

;	LDX #$00
;	LDA #$B0
;PIC_LOOP2
;	STA	MPEG_DDPR	; Write to MPEG DRAM. (TESTING).
;	STX	MPEG_DARL
;	INX
;	BNE PIC_LOOP2

;	JSR DMRAMCANCEL
	
;	LDA #$88		; MPEG Proc. Decoder command register
;	LDX #$12		; Write 12h (WCP - Write ComPlete command, display Still image).
;	JSR WRITEMPEG	; Write the command.

CHECK_IF_MPG:	; Test if MPG. Note: All 8.3 DOS filenames are CAPITALS! (No need to check for Lower-case).
	LDX	#$00
	LDA FILE_EXT,X
	CMP	#$4D			; "M"
	BNE NOT_MPG
	INX
	LDA	FILE_EXT,X
	CMP	#$50			; "P"
	BNE	NOT_MPG
	INX
	LDA	FILE_EXT,X
	CMP	#$47			; "G"
	BNE	NOT_MPG
	JMP	PLAY_MPEG		; File extension matches "MPG", play MPEG file.
NOT_MPG:

LOADROM:
	JSR	READ_FILE_INDEX		; Will return with starting cluster of chosen file index.

	LDA	ENTRY_TYPE
	CMP	#$01
	BEQ	FILE_ENTRY_OK
	JMP	HDD_ERROR
FILE_ENTRY_OK:
	JSR BLANKPPU

	JSR PRINTSTRING

	.byte		$07, $07
	.word		L90D1			; "Loading..."

	LDA FILE_START_CLUS3		; Put file start cluster values into CURRENT_CLUS (to start off the cluster chain following)
	STA CURRENT_CLUS3
	LDA FILE_START_CLUS2
	STA CURRENT_CLUS2
	LDA FILE_START_CLUS1
	STA CURRENT_CLUS1
	LDA FILE_START_CLUS0
	STA CURRENT_CLUS0

	JSR	CONVERT_CLUS_TO_LBA		; Converts cluster number in CURRENT_CLUS3-0 into LBA3-0 values.
								; ...and also reads the next cluster chain number into NEXT_CLUS3-0.
	
	JSR	IDE_RSEC				; Read first sector of ROM into buffer
	JSR	SWAP_BUFFER				; Byteswap the buffer first (to unscramble game name)
	
	LDA #$A3
	JSR WRITEPPU
	LDA #$99
	JSR WRITEPPU
	JSR	SHOW_ROMNAME			; Display game name on screen before starting load loop.

;	LDX #$08		; "64M"
;	LDY #$00

	LDX #$98		; "128M"
	LDY #$00

;	LDX #$98		; "192M"
;	LDY #$0A

;	LDX #$98		; "256M"
;	LDY #$BA

	STX	$C002		; Store (Cart size address high?) to $C002
	STX	$A7			; Store (Cart size address high?) to $A7
	STY	$C003		; Store (Cart size address low?) to $C003
	STY	$A9			; Store (Cart size address low?) to $A9

	LDA	#$00
	STA	SECT_COUNT3		; Zero sector count.
	STA	SECT_COUNT2
	STA	SECT_COUNT1
	STA	SECT_COUNT0

	LDA	#$40
	STA $14

	LDA	#$00		; Load A with #$00
	STA	DRAMADD1	; Zero DRAM Addresses
	STA	DRAMADD2	; Zero DRAM Addresses
	STA	DRAMADD3	; Zero DRAM Addresses
	
	LDA	#$00
	STA	$B0			; Set $B0 to 00h (offset from DRAMADD0)
	LDA	#$08
	STA	$E5			; Set $E5 to 08h

LOADSTART:
	LDY	#$00
	STY	$16			; Offset within each DRAM "sector" (possibly) ???
	STY	$12			; Pointer to each DRAM "sector" (512 bytes)
	STY	$13			; Pointer to each DRAM block of 256 DRAM "sectors" (128KB)

	LDA	#$40			; Start $14 at 40h (number of 128KB blocks to read from HDD - 64 in decimal)
	STA	$14				; This will load 64 * 128K blocks (8192KB = 64Mbits)
	
	LDY	$B0			; Load Y with $B0 (Starts at 00h)
	LDA	#$80
	STA	DRAMADD0,Y	; Set DRAMADD0 to 80h, Y offset
LOADING_LOOP:
	JSR L9E50			; Update DRAM pointers
	
	JSR	CONVERT_CLUS_TO_LBA		; Converts cluster number in CURRENT_CLUS3-0 into LBA3-0 values.
								; ...and also reads the next cluster chain number into NEXT_CLUS3-0.

	JSR IDE_R2DRAM_CLUSTER		; Transfer whole cluster from HDD to DRAM (cluster size set by SECT_PER_CLUS)

	JSR	READKEYPAD
	LDA $8C
	CMP #$20				; Check if STOP button pressed while loading ROM.
	BNE CONTINUE_LOADING
	JSR	PRINT_STOP			; STOP pressed ! Stop loading ROM, then wait for PLAY button etc.
	JMP LD392				; Restart main V64 loop. (wait for Play button etc.)
CONTINUE_LOADING:
	
;	LDA #$A0
;	JSR WRITEPPU
;	LDA #$93
;	JSR WRITEPPU
;	LDA CURRENT_CLUS3	; Print current clus to screen (DEBUG)
;	JSR	PRINTHEX
;	LDA CURRENT_CLUS2
;	JSR	PRINTHEX
;	LDA CURRENT_CLUS1
;	JSR	PRINTHEX
;	LDA CURRENT_CLUS0
;	JSR	PRINTHEX

	LDA #$A0
	JSR WRITEPPU
	LDA #$94
	JSR WRITEPPU
;	LDA SECT_COUNT3
;	JSR	PRINTHEX
;	LDA SECT_COUNT2
;	JSR	PRINTHEX
	LDA SECT_COUNT1
	JSR	PRINTHEX
;	LDA SECT_COUNT0
;	JSR	PRINTHEX

;	LDA #$A0
;	JSR WRITEPPU
;	LDA #$95
;	JSR WRITEPPU
;	LDA C009_SPY		; Print SPY values to screen (DEBUG - trying to figure out what C008-C00C do while loading a ROM)
;	JSR	PRINTHEX	; NOTE: These values can't be read directly, as it messes up the screen etc???
;	LDA C00B_SPY		; So, they're stored into "C00x_SPY" after being written to the real registers.
;	JSR	PRINTHEX	; Also note: C00C and C009 are given the same value, so only using "C009_SPY" for both. OzOnE
;	LDA C00A_SPY
;	JSR	PRINTHEX
;	LDA C009_SPY
;	JSR	PRINTHEX
;	LDA C008_SPY
;	JSR	PRINTHEX

;
;	TODO: Need to check NEXT_CLUS3-0 to see if it is the last cluster, else...
;   Put NEXT_CLUS3-0 into CURRENT_CLUS3-0, then call CONVERT_CLUS_TO_LBA (to get LBA of next cluster in the chain)...
;	Then, copy LBA3-0 into LBA3-0, then loop to read next cluster into DRAM.
;
;   Also need to increment $13 when SECT_COUNT

	JSR	GET_NEXT_CLUS

	LDA	NEXT_CLUS2
	CMP	#$FF				; If End Of File is found...
	BEQ	LOADING_FINISHED	; ...Loading of file is finished !!

	CLD
	CLC
	LDA SECT_COUNT0
	ADC	SECT_PER_CLUS		; Add SECT_PER_CLUS to SECT_COUNT after each transfer (to update the sector count.)
	STA	SECT_COUNT0

	LDA	SECT_COUNT1			; Load SECT_COUNT1
	ADC	#$00				; Add 0x00 (add carry only, if set)
	STA	SECT_COUNT1			; Store back

	LDA	SECT_COUNT2			; Load SECT_COUNT2
	ADC	#$00				; Add 0x00 (add carry only, if set)
	STA	SECT_COUNT2			; Store back

	LDA	SECT_COUNT3			; Load SECT_COUNT3
	ADC	#$00				; Add 0x00 (add carry only, if set)
	STA	SECT_COUNT3			; Store back

	LDA	NEXT_CLUS3			; If EOF not found, put NEXT cluster number into CURRENT cluster...
	STA	CURRENT_CLUS3		; ...then read next cluster into DRAM.
	LDA	NEXT_CLUS2
	STA	CURRENT_CLUS2
	LDA	NEXT_CLUS1
	STA	CURRENT_CLUS1
	LDA	NEXT_CLUS0
	STA	CURRENT_CLUS0

	LDA SECT_COUNT0
	BEQ	INC_DRAM_128K	; When SECT_COUNT0 wraps to zero, this represents a 128K block
	JMP	LOADING_LOOP
INC_DRAM_128K:
	INC	$13				; Increment 128K block DRAM pointer
	DEC $14
	BEQ INC_DRAM_8M		; NOTE: Needed to reverse logic and add "INC_DRAM_8M" to fix "branch out of range" error. OzOnE.
	JMP	LOADING_LOOP

INC_DRAM_8M:
	INC $E5				; Increment $E5 ($E5 appears to be pointer to each 8192KByte bank of DRAM.) - Note: starts at 0x08 before loading
	INC	$B0				; Increment $B0 (DRAMADD0 offset??)
	JMP	LOADSTART		; Continue loading next 8192K of ROM (will quit when last cluster chain is found)

LOADING_FINISHED:
	LDA #$00
	STA	$DF
	
;	LDA	#$07		; Load A with 07h (Command/Status Register)
;	STA	IDEADD
;	LDA	#$E0		; Load A with E0h (Spin down drive)
;	STA	IDELOW
	
	JSR BLANKPPU

	JSR L817A		; Display game name on screen, and jump to "You Can Play Game Now" !! (JSR instead of JMP)
	JMP LD392		; Restart main V64 loop. (wait for Play button etc.)

; **** SUBROUTINES ****
PLAY_MPEG:
	JSR	READ_FILE_INDEX			; Will return with starting cluster of current file index.

	LDA FILE_START_CLUS3		; Put file start cluster values into CURRENT_CLUS (to start off the cluster chain following)
	STA CURRENT_CLUS3
	LDA FILE_START_CLUS2
	STA CURRENT_CLUS2
	LDA FILE_START_CLUS1
	STA CURRENT_CLUS1
	LDA FILE_START_CLUS0
	STA CURRENT_CLUS0

	LDA	#$40			; ****** VCD disk found - start playing ! ******* (stole this routine from the original V64 BIOS code. OzOnE)
	STA	$FF
	STA	$A5
	LDA	#$E7
	JSR	WRITEPPU
	JSR	LE48F			; Print "Play" to screen, zero $DA and $D9
	JSR	LD519			; Init MPEG Volume control (unmute + set volume level)
	
	LDY	#$00
	STY	DATAHIGH
	LDA	#$E1			; W9925P "CD Mode control" register
	LDX	#$10			; Set MWE (Mode Write Enable?)  bit - allows CD input format to be set manually
	JSR	WRITEMPEG		; Also, since bit 7 is 0, selects parallel MPEG stream input (as opposed to serial CD interface)
						; When bits 2:0 are zero, this sets the parallel input bitstram type to "MPEG System Stream".

	LDA	#$88			; W9925P Command Register
	LDX	#$01			; Send Normal PLAY Forward command
	JSR	WRITEMPEG

	LDA	#$00
	STA	$C8
	LDA	#$07
	STA	$DE
	JSR	LD73F			; Init MPEG chip for VCD playback (set for PAL / NTSC, setup video registers etc.)

	LDA	#$85			; Select MPEG "Interrupt Service Register" - used to clear interrupt flags....
	LDX	#$04			; "set" IFDC (I-Frame Detected - Clear) interrupt flag. (in other words, clears I-Frame interrupt)
	JSR	WRITEMPEG
	LDA	#$0A
	STA	$F2
	JSR	LD89D			; Zero addresses $0100 to $010B
	LDA	#$B9
	STA	$0100			; Set $0100 to #$B9
	LDA	#$10
	STA	$0109			; Set $0109 to #$10
	LDA #$00
	STA	$F3
	LDA	#$80
	STA	$F1
	JSR	LE7D5			; Update some sort of pointers ($B1 etc)???
	JSR	LE7EB			; Similar to above, but uses different start offsets??? Don't know, don't care!

MPEGLOOP:
	JSR	CONVERT_CLUS_TO_LBA		; Converts cluster number in CURRENT_CLUS3-0 into LBA3-0 values.
								; ...and also reads the next cluster chain number into NEXT_CLUS3-0.
	
	JSR	IDE_R2MPEG			; Send sector of the file to the MPEG chip.
	
	JSR	GET_NEXT_CLUS		; Get next cluster number

	LDA	NEXT_CLUS3			; If EOF not found, put NEXT cluster number into CURRENT cluster...
	STA	CURRENT_CLUS3		; ...then read next cluster into DRAM.
	LDA	NEXT_CLUS2
	STA	CURRENT_CLUS2
	LDA	NEXT_CLUS1
	STA	CURRENT_CLUS1
	LDA	NEXT_CLUS0
	STA	CURRENT_CLUS0

	LDA	#$84			; W9925 Proc. status register
	STA MPEG_ADD
	LDA MPEG_DPR0
	AND #$01
	BEQ	MPEG_UNDER_OVER_FLOW
	LDA MPEG_DPR0
	AND #$02
	BEQ	MPEG_ERROR
	JMP MPEG_IS_OK
MPEG_UNDER_OVER_FLOW:
MPEG_ERROR:
;	JSR	LD717			; Display "W9925P Error"

MPEG_IS_OK:
	
	LDA	NEXT_CLUS2
	CMP	#$FF				; If End Of File is found...
	BEQ	MPEG_FINISHED
	JMP	MPEGLOOP
MPEG_FINISHED:

ENDLESS:
	JMP ENDLESS

READ_FILE_INDEX:			; Reads through file entries based on FILEINDEX...
							; ...Also works out TOTALFILES: need to set FILEINDEX to max (0xff) first, this routine will then return with TOTALFILES updated.
	LDA	#$00
	STA	FILECOUNTER

	LDA	#$00
	STA	ENTRY_INDEX0		; Start reading from first file entry.
	STA ENTRY_INDEX1
READ_FILE_INDEX_LOOP:
	JSR READ_ENTRY			; Read file entry and return with ENTRY_TYPE set (0x00 = unused / DIR / LFN, 0x01 = FILE, 0xFF = END OF DIR)

	LDA ENTRY_TYPE
	CMP	#$01
	BEQ	FILE_FOUND
	
	LDA	ENTRY_TYPE
	CMP	#$FF
	BEQ	EOD_FOUND

	LDA	ENTRY_TYPE
	BEQ	READ_NEXT_ENTRY			; Not a file or EOD marker (is unused, or DIR), ignore, then read next entry until we see a file.

	JMP	READ_NEXT_ENTRY			; (Just in case ENTRY_TYPE checking above fails!)

FILE_FOUND:
	CLC
	INC	FILECOUNTER
	BCS	TOO_MANY_FILES			; FILECOUNTER has gone past 0xff (254 file limit) - STOP, and display error !!

	LDA	FILEINDEX
	CMP	#$FF
	BEQ	GET_TOTALS_ONLY			; If FILEINDEX is set to #$FF, skip the FILECOUNTER / FILEINDEX match check ("get TOTALFILES mode" is selected)
	
	LDA	FILECOUNTER
	CMP	FILEINDEX
	BEQ	GET_FILE_START_CLUS		; FILECOUNTER matches FILEINDEX, so we have the file we want. Jump, to extract file cluster then return.
GET_TOTALS_ONLY:
	JMP	READ_NEXT_ENTRY
EOD_FOUND:						; End Of Directory marker found, so...
	LDA	FILECOUNTER				; Get last FILECOUNTER value...
	STA	TOTALFILES				; Use this to set TOTALFILES
	RTS

TOO_MANY_FILES:
	JSR			PRINT_STOP

	JSR			BLINKON
	JSR			PRINTSTRING
	
	.byte		$00, $02
	.word		TooManyMSG		; "No files, or more than 255 files!"
	
	JSR	WAIT_FOR_PLAY_PRESSED
	JMP LD33F					; Start over (display main "Bung" screen etc.)

READ_NEXT_ENTRY:				; Inc ENTRY_INDEX0 by 0x20 (point to next file entry), then read next entry
	CLD
	CLC							; Clear carry
	LDA ENTRY_INDEX0
	ADC #$20					; Add 0x20 to ENTRY_INDEX0
	STA ENTRY_INDEX0

	LDA ENTRY_INDEX1			; Load ENTRY_INDEX1
	ADC	#$00					; Add 0x00 (add carry only, if set)
	STA ENTRY_INDEX1			; Store back

	JMP	READ_FILE_INDEX_LOOP
	
GET_FILE_START_CLUS:		; Get start cluster number for current entry. (updated - also gets file size)
	LDA	ENTRY_INDEX0
	CLC
	ADC	#$14
	TAX
	JSR	READ_BUFFER
	STA FILE_START_CLUS2	; High and low words are split up for some strange reason?
	INX
	JSR	READ_BUFFER
	STA	FILE_START_CLUS3
	LDA	ENTRY_INDEX0
	CLC
	ADC	#$1A
	TAX
	JSR	READ_BUFFER
	STA	FILE_START_CLUS0
	INX
	JSR	READ_BUFFER
	STA	FILE_START_CLUS1
	INX
	JSR	READ_BUFFER
	STA	FILE_SIZE0
	INX
	JSR	READ_BUFFER
	STA	FILE_SIZE1
	INX
	JSR	READ_BUFFER
	STA	FILE_SIZE2
	INX
	JSR	READ_BUFFER
	STA	FILE_SIZE3
		RTS

READ_ENTRY:
	LDA	#$00				; Clear ENTRY_TYPE before we begin! (Might be some unresolved attrib cases).
	STA	ENTRY_TYPE

	JSR	GET_ENTRY_SECT		; Read the sector for current entry index into the buffer.
	LDA ENTRY_INDEX0
	TAX						; Put low byte of current file / folder entry into X
	JSR	READ_BUFFER			; Read byte from sector buffer (using X as the offset, and ENTRY_INDEX1 to point to first or second half of buffer)
	CMP	#$E5				; Check to see if first character of filename = 0xe5 (usually means that file was deleted)...
	BEQ	ENTRY_IS_UNUSED		; ...if so, branch to ENTRY_IS_UNUSED

	LDA ENTRY_INDEX0
	TAX						; Put low byte of current file / folder entry into X
	JSR READ_BUFFER
	BEQ	ENTRY_IS_EOD		; If first character of filename = 0x00, this is the "End Of Directory" marker (stop reading / counting entries)

	CLD
	CLC						; Read attrib bytes of each entry to determine if Vol ID / File / Directory / LFN etc.
	LDA ENTRY_INDEX0
	ADC #$0b				; Do ENTRY_INDEX0 + 0x0b (to read ATTRIB byte of current entry)...
	TAX						; ...then store in X.
	JSR	READ_BUFFER
	STA	ENTRY_ATTRIB		; Also store into ENTRY_ATTRIB

	LDA ENTRY_ATTRIB
	CMP #$20				; Check if ONLY the Archive bit is set...
	BEQ	ENTRY_IS_A_FILE		; Should mean that this is a file (already checked this entry for Unused / EOD).

	LDA ENTRY_ATTRIB
	AND	#$02				; Check bit 1
	BNE	ENTRY_IS_HIDDEN		; If bit 1 is high, the entry is HIDDEN, branch to HIDDEN handling

	LDA ENTRY_ATTRIB
	AND	#$04				; Check bit 2
	BNE	ENTRY_IS_SYSTEM		; If bit 2 is high, the entry is SYSTEM, branch to SYSTEM handling
	
	LDA ENTRY_ATTRIB
	AND	#$08				; Check bit 3
	BNE	ENTRY_IS_A_VOL_ID	; If bit 3 is high, the entry is a VOLUME ID, branch to VOL ID handling
	
	LDA ENTRY_ATTRIB
	AND	#$10				; Check bit 4
	BNE	ENTRY_IS_A_DIR		; Entry is a DIRECTORY, branch to Directory handling
	
	LDA ENTRY_ATTRIB
	AND	#$0f				; Check bits 0-3
	CMP	#$0f
	BEQ	ENTRY_IS_A_LFN		; If all bits 0-3 are SET, the entry is a LFN (Long File Name), branch to LFN handling.

	JMP ENTRY_IS_A_FILE		; Else, entry must be a file (most probably!) OzOnE.

ENTRY_IS_UNUSED:			; Ignore unused entries
ENTRY_IS_HIDDEN:			; Ignore Hidden files for now
ENTRY_IS_SYSTEM:			; Ignore System files for now
ENTRY_IS_A_VOL_ID:			; Ignore Volume ID (volume label) for now
ENTRY_IS_A_DIR:				; Ignore directories for now
ENTRY_IS_A_LFN:				; Ignore Long File Names for now
	LDA	#$00
	STA	ENTRY_TYPE
	RTS

ENTRY_IS_A_FILE:			; Found a file!
	LDA ENTRY_INDEX0
	TAX						; X is the buffer offset pointer.
	LDY #$00				; Start Y at zero (file NAME storage index)
STORE_FILE_NAME_LOOP:
	JSR	READ_BUFFER			; (Offset pointed to by X)
	STA	FILE_NAME,Y
	INX						; Point to next char in buffer
	INY						; Increment Y (char storage pointer)
	CPY	#$08				; Check if we've stored all 8 FILE chars.
	BEQ STORE_FILE_EXT
	JMP STORE_FILE_NAME_LOOP

STORE_FILE_EXT:
	LDY	#$00				; Start Y at zero (file EXT storage index)
STORE_FILE_EXT_LOOP:		; Note, X should now be at the start of the file EXT (incremented by file name storage above).
	JSR	READ_BUFFER			; (Offset pointed to by X)
	STA	FILE_EXT,Y
	INX						; Point to next char in buffer
	INY						; Increment Y (char storage pointer)
	CPY #$03				; Check if we've stored all 3 EXT chars.
	BEQ STORE_FILE_EXT_DONE
	JMP STORE_FILE_EXT_LOOP
STORE_FILE_EXT_DONE:
	LDA	#$01
	STA	ENTRY_TYPE
	RTS

ENTRY_IS_EOD:				; End Of Directory found!
	LDA #$FF
	STA	ENTRY_TYPE
	RTS

;
;	To convert cluster number into LBA, need to calc:   LBA = ((CLUSTER - ROOT_DIR_FIRST_CLUS) * SECT_PER_CLUS) + CLUS_BEGIN_LBA
;
CONVERT_CLUS_TO_LBA:
								; Subtract ROOT_DIR_FIRST_CLUS from CURRENT_CLUS...
	SEC							; Set carry (works opposite way to addition)
	LDA	CURRENT_CLUS0			; Load cluster byte 0
	SBC	ROOT_DIR_FIRST_CLUS0	; Subtract ROOT_DIR_FIRST_CLUS0
	STA	LBA0				; Store into cluster LBA byte 0 (temporary: first part of calc)

	LDA	CURRENT_CLUS1			; Load cluster byte 1
	SBC	ROOT_DIR_FIRST_CLUS1	; Subtract ROOT_DIR_FIRST_CLUS1
	STA	LBA1				; Store into cluster LBA byte 1 (temporary: first part of calc)

	LDA	CURRENT_CLUS2			; Load cluster byte 2
	SBC	ROOT_DIR_FIRST_CLUS2	; Subtract ROOT_DIR_FIRST_CLUS2
	STA	LBA2				; Store into cluster LBA byte 2 (temporary: first part of calc)

	LDA	CURRENT_CLUS3			; Load cluster byte 3
	SBC	ROOT_DIR_FIRST_CLUS3	; Subtract ROOT_DIR_FIRST_CLUS3
	STA	LBA3				; Store into cluster LBA byte 3 (temporary: first part of calc)
SUBTRACT_CLUS_DONE:
;
;	Need to now multiply the result from above (in CLUS_LBA) by SECT_PER_CLUS... Sectors per cluster is usually a
;   multiple of 2 (ie. 8, 16, 32, 64, 128), so only ONE bit is set and take advantage of this by counting the
;   number of times it takes to shift the bit right (until the carry bit is set)...
;   Then, use this to multiply the result from above (CLUSTER - ROOT_DIR_FIRST_CLUS) by shifting it's bits left by the
;   same number of times. Phew ! OzOnE.
;
	LDA	SECT_PER_CLUS
	STA	TEMP2			; Put SECT_PER_CLUS into TEMP2 (because we're going to mess with it).
SHIFT_LOOP:
	CLC					; Clear carry
	LDA	TEMP2
	ROR					; Shift SECT_PER_CLUS bit(s) right until carry is set (in TEMP2)
	BCS	SHIFT_DONE
	STA	TEMP2

	CLC
	ASL LBA0	; Shift result left by one bit
	ROL LBA1	; Do same for other bytes (shifts in carry bit, if set)
	ROL LBA2
	ROL LBA3
	JMP SHIFT_LOOP
SHIFT_DONE:
;
;	Now add CLUS_BEGIN_LBA and we should get our final result - yeah right! :) OzOnE.
;
	CLD						; Clear decimal mode (just in case)
	CLC						; ...Clear carry bit before we begin
	LDA LBA0
	ADC CLUS_BEGIN_LBA0
	STA LBA0
	
	LDA LBA1
	ADC CLUS_BEGIN_LBA1
	STA LBA1

	LDA LBA2
	ADC CLUS_BEGIN_LBA2
	STA LBA2

	LDA LBA3
	ADC CLUS_BEGIN_LBA3
	STA LBA3
	RTS
	
GET_NEXT_CLUS:			; Grab the next cluster number from the FAT table (to follow the chain)
	LDA	CURRENT_CLUS3	; Load CURRENT_CLUS number into TEMP reg(s)
	STA TEMP_3
	LDA	CURRENT_CLUS2
	STA TEMP_2
	LDA	CURRENT_CLUS1
	STA TEMP_1
	LDA	CURRENT_CLUS0
	STA TEMP_0
	
	CLC					; Shift it RIGHT seven times..
	LSR TEMP_3			; (divide by 128, so that it only increments the LBA pointer after each block of 128 ENTRIES)
	ROR	TEMP_2
	ROR TEMP_1
	ROR TEMP_0
	CLC
	LSR TEMP_3
	ROR	TEMP_2
	ROR TEMP_1
	ROR TEMP_0
	CLC
	LSR TEMP_3
	ROR	TEMP_2
	ROR TEMP_1
	ROR TEMP_0
	CLC
	LSR TEMP_3
	ROR	TEMP_2
	ROR TEMP_1
	ROR TEMP_0
	CLC
	LSR TEMP_3
	ROR	TEMP_2
	ROR TEMP_1
	ROR TEMP_0
	CLC
	LSR TEMP_3
	ROR	TEMP_2
	ROR TEMP_1
	ROR TEMP_0
	CLC
	LSR TEMP_3
	ROR	TEMP_2
	ROR TEMP_1
	ROR TEMP_0

	CLC
	LDA FAT_BEGIN_LBA0		; Load FAT_BEGIN_LBA0 (to use as a base)
	ADC TEMP_0				; Add TEMP0 to FAT_BEGIN_LBA to get correct sector of cluster chains
	STA LBA0				; Store into LBA (ready for actual sector read)
	LDA	FAT_BEGIN_LBA1
	ADC	TEMP_1
	STA LBA1
	LDA	FAT_BEGIN_LBA2
	ADC	TEMP_2
	STA LBA2
	LDA	FAT_BEGIN_LBA2
	ADC	TEMP_2
	STA LBA2
	
GET_CHAIN_SECT_DONE:
	JSR	IDE_RSEC			; Read the FAT "entries" sector into the buffer !

	CLC
	LDA	CURRENT_CLUS0		; Put low byte of CURRENT_CLUS number into A
	ASL						; Shift it LEFT twice (mutliply by 4, so that X will point to start of each 32-bit entry)
	ASL
	TAX
	BCS	READ_CHAIN_FROM_BUF1
READ_CHAIN_FROM_BUF0:
	LDA	IDEBUF0,X			; Read 32-bit entry from buffer and store into NEXT_CLUS
	STA NEXT_CLUS0
	INX
	LDA	IDEBUF0,X
	STA NEXT_CLUS1
	INX
	LDA	IDEBUF0,X
	STA NEXT_CLUS2
	INX
	LDA	IDEBUF0,X
	STA NEXT_CLUS3
	JMP	CHAIN_SECT_DONE
READ_CHAIN_FROM_BUF1:
	LDA	IDEBUF1,X			; Read 32-bit entry from buffer and store into NEXT_CLUS
	STA NEXT_CLUS0
	INX
	LDA	IDEBUF1,X
	STA NEXT_CLUS1
	INX
	LDA	IDEBUF1,X
	STA NEXT_CLUS2
	INX
	LDA	IDEBUF1,X
	STA NEXT_CLUS3

CHAIN_SECT_DONE:
	RTS

GET_ENTRY_SECT:				; Read correct sector of file / folder entries depending on current ENTRY_INDEX1 value.
	LDA	CLUS_BEGIN_LBA1		; Set LBA values to start at CLUS_BEGIN_LBA first (point to first sector of entries)
	STA	LBA1
	LDA	CLUS_BEGIN_LBA2
	STA	LBA2
	LDA	CLUS_BEGIN_LBA3
	STA	LBA3

	LDA	ENTRY_INDEX1		; Load ENTRY_INDEX1 into A
	STA TEMP2				; Store into TEMP2 reg
	LSR TEMP2				; Shift bits right once (so the maths work out - don't ask!) OzOnE.

	CLD
	CLC
	LDA CLUS_BEGIN_LBA0		; NOTE: LBA0 is read here, so didn't need to load it above.
	ADC TEMP2
	STA LBA0

	LDA	LBA1					; Increment LBA1
	ADC	#$00					; Add 0x00 (add carry only, if set)
	STA	LBA1					; Store back.

	LDA	LBA2					; Increment LBA2
	ADC	#$00					; Add 0x00 (add carry only, if set)
	STA	LBA2					; Store back.

	LDA	LBA3					; Increment LBA3
	ADC	#$00					; Add 0x00 (add carry only, if set)
	STA	LBA3					; Store back.
	
	JSR	IDE_RSEC				; Read the FILE entries sector into the buffer !
	RTS

READ_BUFFER:			; Read from first or second half of buffer (depending on bit 0 of ENTRY_INDEX1) - OFFSET should be put in X before calling !!
	LDA	ENTRY_INDEX1
	AND #$01			; Check if bit 0 of ENTRY_INDEX1 is set
	BNE	READ_FROM_BUF1	; If bit 0 is set, read from second half of buffer...
READ_FROM_BUF0:			; Else, just read from first half of buffer.
	LDA	IDEBUF0,X		; Note: X must contain offset when this routine is called!
	RTS					; Return (with read byte in A)
READ_FROM_BUF1:
	LDA	IDEBUF1,X		; Note: X must contain offset when this routine is called!
	RTS					; Return (with read byte in A)

PRINT_FILENAME:
	LDX	#$00				; Character offset pointer
	LDY #$08				; Loop (for up to 8 characters)
PRINT_FILE_PREFIX_LOOP:		; Print first part of file name (up to 8 characters)
	LDA	FILE_NAME,X			; Get character of file NAME (pointed to by X)
	CMP #$20				; If a space is found, the file name is shorter than 8 characters...
	BEQ PRINT_FILE_EXT		; ...So, skip to print the file extension.
	JSR PRINTASCII			; Else, keep printing the file name characters to screen
	INX						; Increment char pointer
	DEY						; Decrement char counter
	BEQ	PRINT_FILE_EXT		; File name was 8 chars, print extension.
	JMP PRINT_FILE_PREFIX_LOOP	; Loop until all of file prefix (up to 8 characters) has been displayed.

PRINT_FILE_EXT:
	LDA #$2e				; ...print a full-stop before the extension.
	JSR PRINTASCII
	LDX	#$00				; Character offset pointer
	LDY #$03				; Loop for the 3 extension characters.
PRINT_FILE_EXT_LOOP:
	LDA	FILE_EXT,X			; Get character of file EXT (pointed to by X)
	JSR PRINTASCII
	INX						; Increment char pointer
	DEY						; Decrement char counter
	BEQ PRINT_FNAME_DONE	; All 3 EXT chars printed, DONE.
	JMP PRINT_FILE_EXT_LOOP
PRINT_FNAME_DONE:
	RTS

L9CDF:								; EEPROM Flashing routine (gets written to V64 memory at 0600h before execution)
				RORG		$0600

				LDY			#$00
				STY			$BB
				STY			$12
				STY			$13
				STA			$B4

L060A:			LDA			#$80
				STA			$01

L060E:			JSR			L06DF
				JSR			L0729		; Start EEPROM page write

				LDY			#$00		; Start Y at 00h
L0616:			LDA			MPEG_DDPR	; Read low byte of BIOS ROM from MPEG_DDPR
				STA			($00),Y		; Store low byte to 00h, Y offset
				INY
				LDA			DATAHIGH	; Read high byte of BIOS ROM from DATAHIGH
				STA			($00),Y		; Store high byte to 00h(+1), Y offset
				INY
				CPY			#$80
				BNE			L0616		; Write next word until Y = 80h (128 words)
				JSR			L0739		; Delay for approx 512 ticks
				JSR			L072E
				JSR			L06DF		; Select next EEPROM Page ?????

				LDY			#$00		; EEPROM Verify routine ?, Start Y at 00h
L0631:			LDA			MPEG_DDPR		; Load A with low byte of BIOS Rom from MPEG_DDPR
				CMP			($00),Y		; Compare low byte with low byte in EEPROM
				BNE			L0648		; Branch to L0648 if not equal to display 'Err'
				INY
				LDA			DATAHIGH	; Load A with high byte of BIOS Rom from DATAHIGH
				CMP			($00),Y		; Compare high byte with high byte in EEPROM
				BNE			L0648		; Branch to L0648 if not equal to display 'Err'
				INY
				CPY			#$80
				BNE			L0631		; Verify next word until Y = 80h (128 words)
				JMP			L067C		; Jump to L067C to advance BIOS Rom pointer (then write next page)

L0648:			INC			$0C
				LDA			#$9A
				JSR			L06D3		; Write 9Ah to PPU 
				LDA			#$AB
				JSR			L06D3		; Write ABh to PPU
				LDA			$0C
				JSR			L06D3
				LDA			$0C
				CMP			#$08
				BCC			L0679
				LDA			#$9A
				JSR			L06D3		; Write 9Ah to PPU
				LDA			#$AB
				JSR			L06D3		; Write ABh to PPU
				LDA			#$15
				JSR			L06D3		; Write 15h to PPU ('E')
				LDA			#$62
				JSR			L06D3		; Write 62h to PPU ('r')
				JSR			L06D3		; Write 62h to PPU ('r')

L0676:			JMP			L0676

L0679:			JMP			L060E

L067C:			INC			$12
				BNE			L0682
				INC			$13

L0682:			LDA			$00
				CLC
				ADC			#$80
				STA			$00
				LDA			$01
				ADC			#$00
				STA			$01
				CMP			#$C0
				BEQ			L0696
				JMP			L060E

L0696:			INC			$BB
				LDA			$BB
				CMP			#$08
				BEQ			L06A1
				JMP			L060A

L06A1:			LDA			#$01
				STA			$BB
				STA			V64STATUS
				JSR			LFFE3
				JSR			LFFE0

				.byte		$00, $07
				.word		L06BC			; "Press PLAY for restart"

L06B2:			JSR			LFFE9
				AND			#$10
				BEQ			L06B2
				JMP			(LFFFC)		; RESTART V64

L06BC:			.byte		"Press PLAY for restart"
				.byte		$FF

L06D3:			STA			PPU
				PHA

L06D7:			LDA			V64STATUS
				LSR
				BCC			L06D7
				PLA
				RTS

L06DF:			LDY			#$00		; Looks like this selects the next start address of the BIOS Rom
				STY			DATAHIGH	; Store 00h to DATAHIGH
				LDA			#$01
				STA			MPEG_DARH	; Store 01h to MPEG_DARH
				STY			$0F			; Store 00h to $0Fh
				LDA			$12
				STA			$B1
				LDA			$13
				LSR
				ROR			$B1
				ROR			$0F
				LSR
				ROR			$B1
				ROR			$0F
				LDA			$B1
				STA			DATAHIGH
				LDA			$0F
				STA			MPEG_DARL
				RTS

L0706:			NOP
				NOP
				NOP
				NOP
				NOP
				NOP
				NOP
				NOP
				RTS

L070F:			PHA						; Routine writes AAh to $9555h, 55h to $EAAAh, 
				LDA			#$01		; then byte in 'A' to $9555h (for sending EEPROM software commands)
				STA			V64STATUS
				LDA			#$AA
				STA			$9555
				LDA			#$55
				STA			$EAAA
				PLA
				STA			$9555
				LDA			$BB
				STA			V64STATUS
				RTS

L0729:			LDA			#$A0
				JMP			L070F		; Write AAh, 55h, then A0h to EEPROM (Starts write process)

L072E:			LDY			#$00		; Start Y at 00h
L0730:			LDA			($00),Y
				EOR			($00),Y
				AND			#$40
				BNE			L0730
				RTS

L0739:			LDY			#$64		; Start Y at 64h
L073B:			JSR			L0706		; Small NOP delay
				DEY						; Decrement Y
				BNE			L073B		; Repeat NOP delay until Y = 00h
				RTS

L0742:			LDA			#$80
				JSR			L070F		; Write AAh, 55h, then 80h to EEPROM
				LDA			#$60
				JSR			L070F		; Write AAh, 55h, then 60h to EEPROM (Enter Software ID mode)
				JSR			L0706		; Small NOP delay (should be 10us for Winbond w29ee011)
				LDX			L8000		; Load X with manufacturer ID from EEPROM (DAh for Winbond w29ee011 ?)
				LDY			$8001		; Load Y with device ID from EEPROM (C1h for Winbond w29ee011 ?)
				LDA			#$F0
				JSR			L070F		; Write AAh, 55h, then F0h to EEPROM (Exit software ID mode)
				JMP			L0706		; Small NOP delay (should be 10us for Winbond w29ee011)

				LDA			#$80
				JSR			L070F		; Write AAh, 55h, then 80h to EEPROM
				LDA			#$10
				JMP			L070F		; Write AAh, 55h, then 10h to EEPROM (Erase EEPROM chip command)

				LDA			#$80		; Write AAh, 55h, then 80h to EEPROM
				JSR			L070F
				LDA			#$20		; Write AAh, 55h, then 20h to EEPROM (Disable EEPROM software protection)
				JMP			L070F

				REND

L9E50:			LDA			$16		; Routine called VERY often to write $16,$12,$13,$E5 etc to $C008:$C00C
				ASL
				STA			$C00A	; Multiply contents of $16 by 2, and store at $C00A
				STA			C00A_SPY
				
				LDA			$12		; Appears to be DRAM / CART "sector" reg - 256 words per sector??
				ROL
				STA			$C00B
				STA			C00B_SPY
				
				LDA			$13		; Often incremented after $12 rolls over to 00h....
				ROL					;  ...if so, $13 would point to each block of 256 word "sectors" - 65536 words / 131072 bytes / 128KBytes)
				STA			$B1		; 
				
				LDA			$E5		; Load A with $E5
				LSR					; Shift right (put LSB into carry)
				ROR					; Rotate right (put carry into MSB)
				AND			#$80	; Mask MSB
				ORA			$B1		; OR with contents of $B1 (force MSB high if LSB of $E5 was high)
				STA			$C008	; Write to $C008
				STA			C008_SPY

				LDA			$E5		; Load A with $E5 
				LSR					; Divide by 2
				AND			#$01	; Mask bit 1
				STA			$B1		; Store at $B5
				
				LDA			$E5		; Load A with $E5
				AND			#$0C	; Mask bits 2 and 3
				ASL					; Multiply by 2 (shift left)
				ORA			$B1
				STA			$C009
				STA			$C00C
				STA			C009_SPY
				LDY			#$00
				RTS

; Note - The directives below are used to reserve the space for the V64's internal registers.
; These are the registers in the Gate Array chip which control everything -
; THERE SHOULD NOT BE ANY COMPILED CODE IN AT LEAST THE RANGE $C000 to $C01F !!
; (I don't yet know how much of the range from $C000 to $D000 is actually used on the V64. OzOnE)
;
				ds.b		$A9FF - ., 0

				ds.b		V64STATUS - ., 0

				.byte		$00,$00,$64
LC003:			.byte		"FOR GAME TEST ONLY"


				ds.b		$D000 - ., 0

LD000:			PHA
				PHA
				TXA
				PHA
				TYA
				PHA
				TSX
				LDA			$BB
				STA			$0104,X
				LDA			$0105,X
				STA			$3E
				LDA			$0106,X
				STA			$3F
				LDY			#$01
				LDA			($3E),Y
				STA			$B1
				INY
				LDA			($3E),Y
				TAX
				INY
				LDA			($3E),Y
				STA			$BB
				STA			V64STATUS
				STX			$3F
				LDA			$B1
				STA			$3E
				TSX
				LDA			$0105,X
				CLC
				ADC			#$03
				STA			$0105,X
				LDA			$0106,X
				ADC			#$00
				STA			$0106,X
				PLA
				TAY
				PLA
				TAX
				PLA
				JSR			LD05B
				PHP
				STA			$0F
				PLA
				STA			$B1
				PLA
				STA			$BB
				STA			V64STATUS
				LDA			$B1
				PHA
				LDA			$0F
				PLP
				RTS

LD05B:			JMP.ind			($003E)

LD05E:			LDA			#$08		; LD05E routine displays game name in CART/DRAM to screen. 
				STA			$E5			; Game name starts at rom offset 20h and is 20 bytes (decimal) long.
				LDA			#$10
				STA			$16
				LDA			#$00
				STA			$12
				STA			$13
				STA			$0A			; Just a counter for LD07A loop below (incremented until last char displayed)
				JSR			L9E50		; Puts variables into C008,9,A,B,C ?
				LDX			#$20
				JSR			PRINTSTRING

				.byte		$01, $08
				.word		$0081

LD07A:			LDA			DRAMLOW	; Load A with DRAMLOW
				PHA					; Push A to stack

				LDA			DATAHIGH	; Load A with DATAHIGH
				JSR			PRINTASCII		; Jump to ASCII compare table and WRITE to PPU

				PLA					; Pull A (DRAMLOW) from stack
				JSR			PRINTASCII		; Jump to ASCII compare table and WRITE to PPU

				INC			$0A			; Increment $0A
				LDA			$0A			; Load A with $0A
				CMP			#$0A		; Check if reg $0A has reached 0Ah (10 words)
				BNE			LD07A		; Write next char in DRAM to PPU until $0A = #$0A (10 Words, 20 chars.)
				RTS

LD091:			BIT			$2002		; Bit test $2002 (wait for VSYNC on NES?)
				BPL			LD09F		; Branch to LD09F if MSB = 0
				INC			$1C			; Else, increment $1C
				BNE			LD091		; Loop if $1C is NOT 0
				INC			$1D			; Increment $1D when $1C wraps to 0
				JMP			LD091		; Repeat

LD09F:			BIT			$2002		; Bit test $2002 (wait for VSYNC on NES?)
				BMI			LD0AD		; RTS if MSB = 1
				INC			$1C			; Increment $1C
				BNE			LD09F		; Loop if $1C is NOT 0
				INC			$1D			; Increment $1D when $1C wraps to 0
				JMP			LD09F		; Repeat

LD0AD:			RTS

LD0AE:			JSR			LD091		; Wait for MSB of $2002 to go high for some reason?
				DEX						; Also increments $1C / $1D while waiting?
				BNE			LD0AE		; Repeats this until X decrements to 00h ? wtf?
				RTS						; NOTE - MSB of $2002 appears to mean that "PPU is generating
										; vertical blanking impulse" on the NES - possible that V64 uses similar method?
LD0B5:			JSR			LD091

READKEYPAD:			LDA			$DF
				TAX
				ORA			#$01
				STA			$4016
				STX			$4016
				LDA			$8C
				STA			$8B
				LDX			#$08

LD0C9:			LDA			$4016
				LSR
				ROL			$8C
				DEX
				BNE			LD0C9
				LDA			$8C
				EOR			$8B
				AND			$8C
				STA			$8D
				RTS

LD0DB:			JSR			READKEYPAD
				LDA			$8C
				CMP			$8B
				BEQ			LD0ED
				LDA			#$00
				STA			$7D
				STA			$C4

LD0EA:			LDA			$8D
				RTS

LD0ED:			LDA			$7D
				BMI			LD104
				INC			$7D
				LDA			$7D
				CMP			#$40
				BCC			LD0EA
				LDA			#$80
				STA			$7D
				LDA			#$08
				STA			$C4
				LDA			$8D
				RTS

LD104:			LDA			$C4
				BEQ			LD10D
				DEC			$C4
				LDA			$8D
				RTS

LD10D:			LDA			#$08
				STA			$C4
				LDA			$8C
				STA			$8D
				RTS

LD116:			LDA			#$FE
				JSR			WRITEPPU
				LDA			#$44
				JSR			WRITEPPU
				LDA			#$C5
				JSR			WRITEPPU
				LDY			#$0B

LD127:			TYA
				CLC
				ADC			#$80
				JSR			WRITEPPU
				DEY
				BPL			LD127
				LDA			#$FC
				JSR			WRITEPPU
				LDA			#$EF
				JSR			WRITEPPU
				LDA			#$F0
				JSR			WRITEPPU
				LDA			#$80
				JSR			WRITEPPU
				LDA			#$C1
				JSR			WRITEPPU

BLANKPPU:		LDA			#$90
				JSR			WRITEPPU
				LDA			#$A0
				JSR			WRITEPPU
				LDY			#$00		; Start Y at 0
				LDA			#$7F		; Load A with #$7F (space?)
LD158:			JSR			WRITEPPU	; Write to PPU
				INY						; Increment Y
				BNE			LD158		; Repeat until Y = 0 again (wraps around 0-255-0)
				
				LDY			#$1F		; Start Y at #$1F
LD160:			JSR			WRITEPPU	; Write to PPU
				DEY						; Decrement Y
				BPL			LD160		; Repeat until negative (when MSB = 0??)
				RTS

WRITEPPU:		STA			PPU		; Store A to PPU
				PHA					; Push A to stack
LD16B:			LDA			V64STATUS	; Load V64STATUS ($C000) into A
				LSR					; Shift it right one bit (to test PPU Ready)
				BCC			LD16B		; Branch back to LD16B until PPU is ready again
				PLA					; Pull A from stack
				RTS

				LDA			#$E9
				JMP			WRITEPPU

				LDA			#$E1
				JMP			WRITEPPU

PRINTSTRING:	TXA					; Transfer X to A
				PHA					; Push A to stack
				TSX					; Transfer stack pointer to X
				LDA			$0102,X
				STA			$BC
				LDA			$0103,X
				STA			$BD
				LDA			$BC
				CLC
				ADC			#$04
				STA			$0102,X
				LDA			$0103,X
				ADC			#$00
				STA			$0103,X
				PLA
				TAX
				LDY			#$02
				LDA			($BC),Y
				ASL
				STA			$B1
				BIT			$B1
				PHP
				LDA			($BC),Y
				BMI			LD1BB
				AND			#$0F
				ORA			#$90
				JSR			WRITEPPU
				DEY
				LDA			($BC),Y
				AND			#$1F
				ORA			#$A0
				JSR			WRITEPPU

LD1BB:			LDY			#$04
				LDA			($BC),Y
				BNE			LD1EB
				DEY
				LDA			($BC),Y
				BMI			LD1D2
				STA			$B1
				PLA

LD1C9:			TXA
				JSR			LD287
				DEC			$B1
				BNE			LD1C9
				RTS

LD1D2:			AND			#$7F
				STA			$B1
				PLP
				BPL			LD1E2

LD1D9:			TXA
				JSR			WRITEPPU
				DEC			$B1
				BNE			LD1D9
				RTS

LD1E2:			TXA
				JSR			PRINTASCII
				DEC			$B1
				BNE			LD1E2
				RTS

LD1EB:			STA			$BA
				DEY
				LDA			($BC),Y
				STA			$B9
				LDY			#$00
				PLP
				BMI			LD208

LD1F7:			LDA			($B9),Y
				CMP			#$FF
				BEQ			LD207
				JSR			PRINTASCII
				INY
				BNE			LD1F7
				INC			$BA
				BNE			LD1F7

LD207:			RTS

LD208:			BVS			LD21B

LD20A:			LDA			($B9),Y
				CMP			#$FF
				BEQ			LD207
				JSR			WRITEPPU
				INY
				BNE			LD20A
				INC			$BA
				BNE			LD20A
				RTS

LD21B:			STX			$B1

LD21D:			LDA			($B9),Y
				JSR			LD287
				LDA			#$20
				JSR			PRINTASCII
				INY
				DEC			$B1
				BNE			LD21D
				RTS

PRINTASCII:		CMP			#$20
				BNE			LD235
				LDA			#$7F
				BNE			LD282

LD235:			CMP			#$3C
				BNE			LD23D
				LDA			#$0B
				BNE			LD282

LD23D:			CMP			#$3E
				BNE			LD245
				LDA			#$0C
				BNE			LD282

LD245:			CMP			#$2D
				BEQ			LD24D
				CMP			#$5F
				BNE			LD251

LD24D:			LDA			#$0D
				BNE			LD282

LD251:			CMP			#$2E
				BNE			LD259
				LDA			#$0E
				BNE			LD282

LD259:			CMP			#$2C
				BNE			LD261
				LDA			#$0F
				BNE			LD282

LD261:			CMP			#$4F
				BNE			LD269
				LDA			#$00
				BEQ			LD282

LD269:			CMP			#$2F
				BNE			LD271
				LDA			#$6D
				BNE			LD282

LD271:			CMP			#$61
				BCC			LD27F
				CMP			#$7B
				BCS			LD282
				SEC
				SBC			#$10
				JMP			WRITEPPU

LD27F:			SEC
				SBC			#$30

LD282:			AND			#$7F
				JMP			WRITEPPU

LD287:			PHA
				LSR
				LSR
				LSR
				LSR
				CMP			#$0A
				BCC			LD292
				ADC			#$06
LD292:			JSR			WRITEPPU
				PLA
				AND			#$0F
				CMP			#$0A
				BCC			LD29E
				ADC			#$06
LD29E:			JMP			WRITEPPU

BLINKON:			LDA			#$88
				JMP			WRITEPPU

BLINKOFF:			LDA			#$80
				JMP			WRITEPPU

LD2AB:			JSR			LD091
				JSR			READKEYPAD
				BEQ			LD2AB
				RTS

PRINTHEX:
	STA TEMP    ; save A
	LSR			; shift A 4 times
	LSR
	LSR
	LSR
	JSR HEXTA   ; convert bit 4..7 to HEX and print
	LDA TEMP
	JSR HEXTA   ; convert bit 0..7 to HEX and print
	LDA TEMP    ; restore A
	RTS

HEXTA:
	AND #$0F    ; mask bit 0..4
	CMP #$0A    ; > 10 ?
	CLC
	BMI HEXTA1  ; no ->

	ADC #7      ; A..F
HEXTA1:
	ADC #$30    ; convert to ASCII-char...
	JSR PRINTASCII   ;  ...and print it
	RTS

LD2B4:			.byte		"Play    "
				.byte		$FF

LD2BD:			.byte		"Stop    "
				.byte		$FF

LD2C6:			.byte		"Pause   "
				.byte		$FF

LD2CF:			.byte		"  Doctor V64      V1.30"
				.byte		$FF

LD2E7:			.byte		"Products of"
				.byte		$FF

LD2F3:			.byte		"Bung Enterprises Limited"
				.byte		$FF

LD30C:			LDA			#$00				; Zero all variables
				STA			$2000
				STA			$2001
				STA			BACKUPMD
				STA			$DD
				STA			$FF
				STA			$DC
				STA			$8D
				STA			$8C
				STA			$7D
				STA			$D2
				STA			$A7
				STA			$A9
				LDA			#$04
				STA			$DE
				LDA			#$02
				STA			$DF
				LDA			#$01
				STA			$F8
				STA			$BB
				STA			V64STATUS
				LDA			#$03
				STA			$AF
				RTS

LD33F:			SEI							; !!!!! CODE START POINT AFTER RESET VECTOR !!!!!
				CLD
				LDX			#$FF
				TXS
				JSR			LD30C			; Zero / reset all variables
				JSR			LD43D			; Transfer stack pointer to X, transfer X to $B8
				JSR			LD420			; Reset CD drive, select Error / Features reg
				LDX			#$04
				JSR			LD0AE			; Read $2002 ??? (VSYNC???)
				JSR			LD42D			; Check for correct MPEG chip ID + soft reset + reset parser + enable sector counting + disable DMA / IRQs
				JSR			LD71F			; Set MPEG border colour 1
				JSR			LD116			; Init PPU options then BLANKPPU
				JSR			PRINTSTRING

				.byte		$00, $01
				.word		LD2CF			; "  Doctor V64      V1.30"

				JSR			L982E			; Check parallel port?
				JSR			LDFC1			; Do something with V64STATUS and processor status?
				BNE			LD36F			; Print main screen idents etc....
				LDA			#$42
				JSR			PRINTASCII

LD36F:			JSR			PRINTSTRING

				.byte		$00, $09
				.word		LD2E7			; "Products of"

				JSR			PRINTSTRING

				.byte		$00, $0A
				.word		LD2F3			; "Bung Enterprises Limited"

				LDA			#$60
				STA			$CB
;				JSR			READKEYPAD
;				LDA			$8C
;				AND			#$C0
;				CMP			#$C0			; Test to see if "Menu" + "CH" buttons pressed while V64 first switched on....
;				BNE			SET_HDD_DEFAULT	; ....Set HDD mode, then jump to main start screen if buttons NOT pressed,
;				JSR			BLANKPPU		; Else, blank the screen...
;				JSR			LD3E2			; and jump to self-test routines.

SET_HDD_DEFAULT:
				JMP			SET_HDD_MODE	; Set HDD mode and update screen to start off (set HDD as default)
LD392:			
				JSR			LD091			; Not sure yet. Test for MSB = 0 at $2002 (wait for VSYNC on NES?)
				JSR			READKEYPAD		; Read Keypad (der!)
				CMP			#$10
				BEQ			START_READING	; Check for Play pressed, if play pressed then goto next instruction (Else, skip to LD3B9).....

				LDA			$8C
				CMP			#$01			; Check if FR pressed
				BEQ			SET_CD_MODE

				LDA			$8C
				CMP			#$02			; Check if FF pressed
				BEQ			SET_HDD_MODE
				
				JMP			LD3B9			; Update display (updates Mono / Stereo mode etc, then loops back to LD392)
				
SET_CD_MODE:
				JSR			READKEYPAD
				BNE			SET_CD_MODE		; Loop until key NOT pressed (debounce)
				LDA #$00
				STA HDDMODE					; Set to CD reading mode
				LDA #$A1
				JSR WRITEPPU
				LDA #$93
				JSR WRITEPPU
				LDA #$13			; "C"
				JSR WRITEPPU
				LDA #$14			; "D"
				JSR WRITEPPU
				LDA #$10			; " "  (blank)
				JSR WRITEPPU
				JMP LD392					; Continue main loop
				
SET_HDD_MODE:
				JSR			READKEYPAD
				BNE			SET_HDD_MODE	; Loop until key NOT pressed (debounce)
				LDA #$01
				STA HDDMODE					; Set to HDD reading mode
				LDA #$A1
				JSR WRITEPPU
				LDA #$93
				JSR WRITEPPU
				LDA #$18			; "H"
				JSR WRITEPPU
				LDA #$14			; "D"
				JSR WRITEPPU
				LDA #$14			; "D"
				JSR WRITEPPU
				JMP LD392					; Continue main loop
				
START_READING:

				JSR	BLANKPPU		; PLAY pressed ! .....				

				LDA	HDDMODE
				BEQ	CD_MODE
				
IDE_MODE:
				JSR			IDEStart		; (Read from HDD instead of CD)

CD_MODE:									; Continue on with original V64 CD routines
				JSR			BLINKON
				JSR			PRINTSTRING

				.byte		$02, $07
				.word		LE484			; "Waiting..."

				JSR			BLINKOFF

				JSR			LE565			; Jump, to start CD reading ?

				LDA			#$00			; Finished CD reading, return to main "Bung" screen.....
				STA			$DC
				JSR			PRINT_STOP
				JMP			LD392			; Repeat main loop (show "Bung" screen + wait for Play button etc.)

LD3B9:			AND			#$03			; Play not pressed, keep updating display etc.
				BEQ			LD3CE
				LDA			$8C
				AND			#$03
				CMP			#$03
				BNE			LD3CE
				JSR			BLANKPPU
				JSR			L805F
				JMP			LD392			; Repeat main loop (wait for Play button etc.)

LD3CE:			JSR			L982E			; Check parallel port?
				JSR			LD47C			; Update "Mono" / "Stereo" / "By Pass" message
				JSR			LD497			; 
				JMP			LD392			; Repeat main loop (wait for Play button etc.)

PRINT_STOP:			JSR			PRINTSTRING

				.byte		$00, $00
				.word		LD2BD		; "Stop"

				RTS

LD420:			LDA			#$07		; Select IDE Status/Command register
				STA			IDEADD
				LDA			#$08		; Send 08h (reset drive) command
				STA			IDELOW
				JMP			LD8A8		; Select device 0

LD42D:			JSR			LD68F		; Test W9925P chip ID. if OK then reset MPEG processor stuff, and enable sector counting.
				LDA			#$FF
				STA			$DB
				JMP			LD437

LD437:			JSR			LD7F3		; Set MPEG bitstream high / low byte input order to SWAPPED.
				JMP			LD846		; Setup MPEG audio parameters etc.

LD43D:			TSX
				STX			$B8
				RTS

LD441:			LDA			$DF
				BNE			LD44D
				JSR			PRINTSTRING

				.byte		$10, $00
				.word		LD461			; "Mono    "

				RTS

LD44D:			CMP			#$02
				BNE			LD459
				JSR			PRINTSTRING

				.byte		$10, $00
				.word		LD46A			; "Stereo  "

				RTS

LD459:			JSR			PRINTSTRING

				.byte		$10, $00
				.word		LD473			; "By Pass "
				
				RTS

LD461:			.byte		"Mono    "
				.byte		$FF

LD46A:			.byte		"Stereo  "
				.byte		$FF

LD473:			.byte		"By Pass "
				.byte		$FF

LD47C:			LDA			$8D
				AND			#$40
				BEQ			LD496
				SEC
				LDA			$DF
				SBC			#$02
				STA			$DF
				BCS			LD48F
				LDA			#$04
				STA			$DF
LD48F:			LDA			#$00
				STA			$D9
				JMP			LD441		; Display "Mono    ", "Stereo ", or "By Pass " on screen (depending on value of $DF)

LD496:			RTS

LD497:			LDA			V64STATUS	; Load A with V64STATUS
				AND			#$02		; Check bit 2
				BEQ			LD4C2		; Branch to LD4C2 if bit 2 = 0
				LDA			$DC			; Continue here if bit 2 of V64STATUS is SET
				BNE			LD4B6
				LDA			$F8
				BEQ			LD4C2
				LDA			$DD
				BEQ			LD4AF		; If $DD = 0, skip to LD4AF (skip enabling of backup mode)
				LDA			#$01
				STA			BACKUPMD	; Enable backup mode ?
LD4AF:			LDA			$DE
				AND			#$04
				JMP			LD4C8

LD4B6:			JSR			LD420
				LDA			#$00
				STA			$FF
				STA			$DC
				JMP			L9A1D

LD4C2:			LDA			$DE
				AND			#$04
				ORA			#$02
LD4C8:			STA			$DE
				LDA			$4017
				AND			#$01
				EOR			#$01
				ORA			$DE
				STA			$DE
				LDA			$DE
				CMP			$DB
				BEQ			LD496
				STA			$DB
				LDY			#$00
				STY			DATAHIGH
				STY			$C8
				LDA			#$85		; Interrupt Service Register
				LDX			#$04		; IFD (I-frame detect) Clear
				JSR			WRITEMPEG
				LDA			$DE
				AND			#$02
				BNE			LD4F7
LD4F1:			JSR			LD509
				JMP			LD506

LD4F7:			LDA			$FF
				CMP			#$40
				BNE			LD4F1
				LDA			$DA
				AND			#$C0
				BNE			LD4F1
				JSR			LD519		; Set MPEG audio volume (unmute)
LD506:			JMP			LD73F		; Init MPEG chip for VCD playback (set for PAL / NTSC, setup video registers etc.)

LD509:			LDA			#$C0		; MPEG Audio Config Register
				LDX			#$1A		; AEO (Audio Output Enable) = 0
				JSR			WRITEMPEG
				LDA			#$C1		; MPEG Audio volume control register
				LDY			#$00
				LDX			#$00		; Set to 0000h (mute both channels)
				JMP			WRITEMPEG16

LD519:			LDA			#$C0		; MPEG Audio Config Register
				LDX			#$1E		; AEO (Audio Output Enable) = 1
				JSR			WRITEMPEG

				LDY			$AF
				LDA			LD52C,Y		; Cycle through volume level steps at LD52C ? (based on offset in $AF)
				TAY
				TAX
				LDA			#$C1		; MPEG Audio volume control register
				JMP			WRITEMPEG16

LD52C:			.byte		$08,$10,$18,$20,$28,$30,$38,$3F

WRITEMPEG16:	STA			MPEG_ADD
				STY			DATAHIGH
				STX			MPEG_DPR0
				LDY			#$00
				STY			DATAHIGH
				RTS

WRITEMPEG:		STA			MPEG_ADD
				STX			MPEG_DPR0
				RTS

				LDY			#$00
				STY			DATAHIGH
				LDA			#$88
				LDX			#$07
				JSR			WRITEMPEG
				LDX			#$4A
				JMP			LD0AE

LD55B:			.byte		" OK "
				.byte		$FF

LD560:			.byte		" Err "
				.byte		$FF

LD566:			.byte		"Writing.... "
				.byte		$FF

LD573:			.byte		"Verify "
				.byte		$FF

LD57B:			.byte		"ROM1  <    > "
				.byte		$FF

LD589:			.byte		"ROM2  <    > "
				.byte		$FF

LD597:			JSR			PRINTSTRING

				.byte		$00, $09
				.word		LD57B		; "ROM1  <    > "

				LDY			#$00
				STY			$10
				STY			$11
				STY			$00
				LDA			#$D0
				STA			$01
				LDX			#$30

LD5AC:			LDA			($00),Y
				CLC
				ADC			$10
				STA			$10
				INY
				LDA			($00),Y
				ADC			$11
				STA			$11
				INY
				BNE			LD5AC
				INC			$01
				DEX
				BNE			LD5AC
				SEC
				LDA			$10
				SBC			LFFF8
				STA			$10
				LDA			$11
				SBC			LFFF9
				STA			$11
				LDX			$11
				JSR			PRINTSTRING

				.byte		$07, $09
				.word		$0001

				LDX			$10
				JSR			PRINTSTRING

				.byte		$00, $80
				.word		$0001

				LDA			$10
				CMP			LFFF8
				BNE			LD5FB
				LDA			$11
				CMP			LFFF9
				BNE			LD5FB
				JSR			PRINTSTRING

				.byte		$0D, $09
				.word		LD55B		; " OK "

				JMP			LD602

LD5FB:			JSR			PRINTSTRING

				.byte		$0D, $09
				.word		LD560		; " Err "

LD602:			JSR			PRINTSTRING

				.byte		$00, $0A
				.word		LD589		; "ROM2  <    > "

				LDY			#$00
				STY			$10
				STY			$11
				STY			$00
				LDA			#$01
				STA			$B0

LD615:			LDA			$B0
				STA			V64STATUS
				LDA			#$80
				STA			$01
				LDX			#$40

LD620:			LDA			($00),Y
				CLC
				ADC			$10
				STA			$10
				INY
				LDA			($00),Y
				ADC			$11
				STA			$11
				INY
				BNE			LD620
				INC			$01
				DEX
				BNE			LD620
				INC			$B0
				LDA			$B0
				CMP			#$08
				BNE			LD615
				STY			V64STATUS
				LDX			$11
				JSR			PRINTSTRING

				.byte		$07, $0A
				.word		$0001

				LDX			$10
				JSR			PRINTSTRING

				.byte		$00, $80
				.word		$0001

				LDA			$10
				CMP			L8000
				BNE			LD66B
				LDA			$11
				CMP			$8001
				BNE			LD66B
				JSR			PRINTSTRING

				.byte		$0D, $0A
				.word		LD55B		; " OK "	

				JMP			LD672

LD66B:			JSR			PRINTSTRING

				.byte		$0D, $0A
				.word		LD560		; " Err "

LD672:			LDA			#$01
				STA			$BB
				STA			V64STATUS
				RTS

PRINT_OK:			JSR			PRINTSTRING

				.byte		$00, $80
				.word		LD55B		; " OK "

				RTS

PRINT_ERR:			JSR			BLINKON
				JSR			PRINTSTRING

				.byte		$00, $80
				.word		LD560		; " Err "

				JMP			BLINKOFF

LD68F:			LDY			#$00
				STY			DATAHIGH
				LDA			#$90		; W9925 ID Register
				STA			MPEG_ADD
				LDA			MPEG_DPR0
				AND			#$07
				CMP			#$02		; Should get 02h back ! ...
				BEQ			LD6B4		; If so, branch to LD6B4 (W9925P reset)
				JSR			LD717		; Else, branch to show "W9925P Error" then return.

				LDX			#$AA		; 
				LDY			#$55
LD6A9:			STX			DATAHIGH
				STY			DRAMLOW
				INX
				DEY
				JMP			LD6A9

LD6B4:			STY			DATAHIGH

				LDA			#$81		; MPEG Processor Control register
				LDX			#$00		; Set to 00h
LD6BB:			JSR			WRITEMPEG
				INY
				CPY			#$0A
				BNE			LD6BB		; Loop

				LDY			#$00
				LDA			#$81		; MPEG Processor Control register
				LDX			#$02		; Soft reset
LD6C9:			JSR			WRITEMPEG
				INY
				CPY			#$0A
				BNE			LD6C9

				LDY			#$00
				LDA			#$81		; MPEG Processor Control register
				LDX			#$04		; Input FIFO Reset
LD6D7:			JSR			WRITEMPEG
				INY
				CPY			#$0A
				BNE			LD6D7

				LDX			#$00		; "A" should still contain #$81 (MPEG Processor Control reg).....
				JSR			WRITEMPEG	; Zero reg (releases soft reset / FIFO reset)

				LDA			#$C2		; MPEG System Parsing control register
				LDY			#$10
				LDX			#$00		; Set bit 12 (Reset Parser)
				JSR			WRITEMPEG16
								
				LDX			#$00		; (release parser reset)
				JSR			WRITEMPEG
			
				LDA			#$E3		; MPEG CD Input Sector register
				LDY			#$01
				LDX			#$00		; Set bit 8 (VISN=1 enable sector counting)
				JSR			WRITEMPEG16

				LDA			#$87		; MPEG DMA Req. enable register
				LDX			#$00		; Set to 00h (disable all)
				JSR			WRITEMPEG

				LDA			#$86		; MPEG Interrupt enable register
				LDX			#$00		; Set to 00h (disable all)
				JSR			WRITEMPEG
				RTS

LD70A:			.byte		"W9925P Error"
				.byte		$FF

LD717:			JSR			PRINTSTRING

				.byte		$00, $00
				.word		LD70A			; "W9925P Error"

				RTS

; ********* Set MPEG border colours - normally only one or other routine is called (LD71F or LD72F) for choice of two border colors ? **********
LD71F:			LDA			#$A6		; MPEG border color register 0
				LDX			#$1D		; Set R/Y component to 1Dh
				JSR			WRITEMPEG

				LDA			#$A7		; MPEG border color register 1
				LDY			#$80		; Set G/Cb component 80h
				LDX			#$6B		; Set B/Cr component 6Bh
				JMP			WRITEMPEG16	; JMP (will RTS after)

LD72F:			LDA			#$A6		; MPEG border color register 0
				LDX			#$00		; Set R/Y component to 00h
				JSR			WRITEMPEG

				LDA			#$A7		; MPEG border color register 1
				LDY			#$80		; Set G/Cb component 80h
				LDX			#$80		; Set B/Cr component 80h
				JMP			WRITEMPEG16	; JMP (will RTS after)

; ******** MPEG chip init routines - sets up for PAL / NTSC based on V64's switch and inits all registers etc. **********				
LD73F:			LDY			#$00		; Init MPEG chip for VCD playback - Gets called by LE58A (VCD play start) routine
				STY			DATAHIGH
				LDA			$FF
				CMP			#$40
				BNE			LD74E
				BIT			$DA
				BVS			LD760
LD74E:			STY			MPEG_DARH
				LDX			#$6D
				STX			MPEG_DARL
				LDA			$DE
				AND			#$01
				CLC
				ADC			#$01
				STA			MPEG_DDPR

LD760:			LDA			$DE
				AND			#$01
				LDX			#$69
				STX			MPEG_DARL
				LDX			#$04
				STX			MPEG_DDPR
				CMP			#$00
				BEQ			LD774		; Skip next instruction if $DE = 0 (V64 switch = PAL)
				LDX			#$03		; Else, Load X with 03h

LD774:			STX			MPEG_DDPR
				STX			MPEG_DDPR
				CMP			#$00
				BEQ			LD799
				LDX			#$70
				STX			MPEG_DARL
				LDX			#$00
				STX			MPEG_DDPR
				LDA			#$F4		; Set PPU to PAL mode
				JSR			WRITEPPU

				LDA			#$A1		; MPEG Vertical display width register
				LDY			#$01
				LDX			#$20		; Set to 0120h (288 vertical lines)
				JSR			WRITEMPEG16
				JMP			LD7A3

LD799:			LDX			#$F0
				JSR			WRITEMPEG
				LDA			#$F0		; Set PPU to NTSC mode
				JSR			WRITEPPU

LD7A3:			LDX			#$18		; MPEG Don't Invert Sync, Set to NTSC
				LDA			$DE			; Must be NTSC/PAL register on V64
				AND			#$01
				BEQ			LD7AD		; If $DE=1, then skip next command (leave in NTSC mode)...

				LDX			#$1A		; Else, Set MPEG Invert HSYNC,Invert VSYNC, Set to PAL
LD7AD:			LDA			#$A9		; MPEG video scan mode register
				JSR			WRITEMPEG
				LDA			#$A8		; MPEG Video Output Control Register
				LDX			#$02		; TPD=0 (enable Pixel Data), Enable 2PCLK, Enable video output
				JSR			WRITEMPEG
				LDA			#$A0		; MPEG Vertical Display Start Register
				LDX			#$16		; Start 22 horizontal lines from falling edge of VSYNC
				JSR			WRITEMPEG
				LDA			#$A2		; MPEG Horizontal Display Start Register
				LDX			#$78		; Start 120 pixels from falling edge of HSYNC
				JSR			WRITEMPEG
				LDA			#$A3		; MPEG Horizontal Display Width register
				LDY			#$02
				LDX			#$C0		; Set to 02C0h (704 pixels width)
				JSR			WRITEMPEG16
				LDA			#$A4		; MPEG Update Enable register (set to one to update previous values)
				LDX			#$01		; Set to 1 (no need to reset to 0)
				JSR			WRITEMPEG
				LDA			#$89		; MPEG Argument Register 0
				LDX			#$00		; Set to I-P Mode ???
				JSR			WRITEMPEG
				LDA			#$91		; MPEG General Purpose Control register
				LDX			$DE			; Set General Purpose IO Pins using variable at $DE
				JMP			WRITEMPEG

LD7E5:			LDA			#$A5		; MPEG Video Config. Register
				LDX			#$31		; Set Y/Cb/Cr Output, 16-bit, Enable horiz. & vert. interp, Exchange field order
				JSR			WRITEMPEG
				LDA			#$83		; MPEG SCLK Selection Register
				LDX			#$00		; Set to 00 (Don't ask, I'd be here all day!)
				JMP			WRITEMPEG

LD7F3:			JSR			LD497
				LDX			#$72
				STX			MPEG_DARL		; MPEG DRAM address port?
				STY			MPEG_DDPR		; MPEG host data port
				LDX			#$01
				STX			MPEG_DDPR
				JSR			LD7E5
				LDA			#$82		; MPEG Bit Stream Control Register
				LDX			#$01		; BYS=1 (16-bit Word high/low bytes are swapped)
				JSR			WRITEMPEG
				RTS

; These tables are loaded into MPEG chip's "DM RAM Parameter" regs to set it up. The bytes are actually word pairs (low byte, then high byte)
; This table is apparently loaded into MPEG DM RAM starting at offset 76Fh - not sure what the parameters are as datasheet for W9925PF chip is incomplete.
LD80E:			.byte		$5F,$01,$EA,$00,$C9,$00,$B0,$00,$8C,$00,$75,$00,$64,$00,$58,$00
				.byte		$46,$00,$3A,$00,$32,$00,$2C,$00,$23,$00,$1D,$00

; This table starts at 7CFh (FS32) reg, which sets the sampling rate divisors for MPEG / CD-DA audio based on the master clock.
; So, the V64 has a 40.5MHz master clock, and the values are....
; FS32 = 013Ah, FS44 = 00E3h, FS48 = 00D1h, FSBY = 00e0h, DIVCLK = 0300h, LFBRV1 = 0005h, LFBR2 = 0000h, ASDW = 0A00h
; NORMP = 0001h, APCMS = 0000h, UPTS = 0001h, HWOF = 0000h, MINAR = 0100h, SPEED = 0000h
LD82A:			.byte		$3A,$01,$E3,$00,$D1,$00,$E0,$00,$00,$03,$05,$00,$00,$00,$00,$0A
				.byte		$01,$00,$00,$00,$01,$00,$00,$00,$00,$01,$00,$00

LD846:			JSR			DMRAMACCESS	; Request MPEG DM Ram access
				LDA			#$47		; Set bit 14 (access DM space) + set index bits (10:8)
				STA			DATAHIGH
				LDA			#$6F		; Low index byte is 6Fh (actual address is 76Fh  b'11101101111')
				STA			MPEG_ADD
				LDY			#$00		; Start Y at 00h
LD855:			LDA			LD80E,Y		; Load low byte from LD80E table into A
				TAX						; Transfer A to X
				INY						; Increment Y
				LDA			LD80E,Y		; Load high byte from LD80E table into A
				STA			DATAHIGH	; Store high byte to DATAHIGH
				STX			MPEG_DPR0	; Store low byte (in X) to MPEG_DPR0
				INY						; Increment Y
				CPY			#$1C
				BNE			LD855		; Send next word in table, until Y = 1Ch

				LDA			#$47		; Set bit 14 (access DM space) + set index bits (10:8)
				STA			DATAHIGH
				LDA			#$CF		; Low index byte is CFh (actual address is 7CFh b'11111101111')
				STA			MPEG_ADD		; 7CFh is FS32 reg (sampling freq for 32KHz audio)
				LDY			#$00		; Start Y at 00h
LD874:			LDA			LD82A,Y		; Load low byte from LD82A table into A	
				TAX						; Transfer A to X
				INY						; Increment Y
				LDA			LD82A,Y		; Load high byte from LD82A table into A
				STA			DATAHIGH	; Store high byte to DATAHIGH
				STX			MPEG_DPR0	; Store low byte (in X) to MPEG_DPR0
				INY						; Increment Y
				CPY			#$1C
				BNE			LD874		; Send next word in table, until Y = 1Ch
				JMP			DMRAMCANCEL	; Cancel DM Ram access request

LD88A:			LDY			#$00
				STY			DATAHIGH
				LDA			#$E1		; W9925P "CD Mode control" register
				LDX			#$10		; Set MWE (Mode Write Enable?)  bit - allows CD input format to be set manually
				JSR			WRITEMPEG	; Also, since bit 7 is 0, selects parallel MPEG stream input (as opposed to serial CD interface)
				LDA			#$88		; W9925P Command Register
				LDX			#$01		; send Normal Play Forward command
				JMP			WRITEMPEG

LD89D:			LDY			#$0B		; *** Zero addresses $0100 to $010B ***
				LDA			#$00
LD8A1:			STA			$0100,Y		; Zero address , Y
				DEY						; Decrement Y
				BPL			LD8A1		; Loop if MSB of Y is still 0 (Y decrements from 0Bh through FFh)
				RTS						; , so $0100 to $010B gets zeroed? (RTS when Y = FFh)

LD8A8:			LDA			#$06		; Device Select register (in packet mode)
				STA			IDEADD
				LDA			#$A0		; Select Device 0 (obsolete bits 7:5 are set)
				STA			IDELOW

				LDA			#$0E		;  
				STA			IDEADD
				LDA			#$08		;
				STA			IDELOW

				LDA			#$01		; Select Error/Features register
				STA			IDEADD
				LDA			#$00		; Set to 00h
				STA			IDELOW
				RTS

LD8C7:			LDA			#$08
				STA			$D1		; $D1
				LDA			#$00
				STA			$D0		; $D0
				STA			$CF
				RTS

LD8D2:			.byte		"Send ATAPI command error"
				.byte		$FF

LD8EB:			JSR			BLINKON
				JSR			PRINTSTRING

				.byte		$00, $07
				.word		LD8D2			; Display "Send ATAPI command error"

				JSR			BLINKOFF
				JSR			LD420
				JMP			LDF60

LD8FE:			JSR			LD8C7
				LDA			#$07		; Select IDE Status/Command register
				STA			IDEADD
LD906:			LDA			IDELOW		; Read status byte into A
				AND			#$80
				BEQ			LD915		; Branch to LD915 if drive is Busy
				JSR			LD9FA
				BNE			LD906
				JMP			LD8EB

LD915:			LDA			#$07		; Select IDE Status/Command register
				STA			IDEADD
				LDA			#$A0		; Send A0h (Packet) command
				STA			IDELOW
				JSR			LD8C7

LD922:			LDA			#$07		; Select IDE Status/Command register
				STA			IDEADD
				LDA			IDELOW
				AND			#$08
				BNE			LD936		; Branch to LD936 if DRQ (Data request) is NOT set
				JSR			LD9FA		; JSR to LD9FA
				BNE			LD922
				JMP			LD8EB		; Jump to display "Send ATAPI command error"

LD936:			LDA			#$02		; Select IDE Interrupt Reason register
				STA			IDEADD
				LDA			IDELOW
				AND			#$03
				CMP			#$01
				BNE			LD8EB		; If reason byte is not 01h, branch to display "Send ATAPI command error"

				LDY			#$00		; Select IDE Data register (also, start Y at 00h)
				STY			IDEADD
LD949:			LDA			$0100,Y		; Load A with first byte of memory at 0100h + Y offset
				TAX						; Transfer A to X
				INY						; Increment Y
				LDA			$0100,Y		; Load A with second byte of memory at 0100h + Y offset
				STA			DATAHIGH	; Store second byte to DATAHIGH
				STX			IDELOW		; Store first byte to IDELOW
				INY						; Increment Y
				CPY			#$0C
				BNE			LD949		; Read next word until Y = 0Ch (store 12 Words from CD drive to SRAM?).
				RTS

LD95D:			.byte		"Sense command error"
				.byte		$FF

LD971:			.byte		$03,$00,$00,$00,$12,$00,$00,$00,$00,$00,$00,$00		;  CD-Rom Command Packet

LD97D:			JSR			BLINKON
				JSR			PRINTSTRING

				.byte		$00, $07
				.word		LD95D				; "Sense command error"

				JSR			BLINKOFF
				JSR			LD420
				JMP			LDF60

LD990:			JSR			LD8C7			; Set $CF and $D0 to 00h, $D1 to 08h (set delay loop time)
				LDA			#$07			; Select IDE Status/Command register
				STA			IDEADD

LD998:			LDA			IDELOW		; -- Drive Busy bit waiting routine --
				AND			#$80
				BEQ			LD9A7			; Branch to LD9A7 if Busy bit is SET
				JSR			LD9FA			; Else, branch to timeout delay loop
				BNE			LD998			; Retry reading Busy bit, then wait if NOT SET
				JMP			LD97D			; Display "Sense command error" if timeout on Busy bit

LD9A7:			LDA			#$07			; Select IDE Status/Command register
				STA			IDEADD
				LDA			#$A0			; Send A0h (Packet) command
				STA			IDELOW
				JSR			LD8C7			; Set $CF and $D1 to 00h, $D1 to 08h (Set delay loop time)

LD9B4:			LDA			#$07			; Select IDE Status/Command register
				STA			IDEADD
				LDA			IDELOW
				AND			#$08
				BNE			LD9C8			; Branch to LD9C8 if DRQ is NOT set
				JSR			LD9FA			; Else, branch to timeout delay loop
				BNE			LD9B4			; Retry reading DRQ bit, then wait if SET
				JMP			LD97D			; Display "Sense command error" if timeout on DRQ bit

LD9C8:			LDA			#$02			; Select IDE Interrupt Reason register
				STA			IDEADD
				LDA			IDELOW
				AND			#$03
				CMP			#$01			; Device should be awaiting command packet if bit 0 is SET (01h)
				BNE			LD97D			; Else, Display "Sense command error" 

				LDY			#$00			; Select IDE Data register
				STY			IDEADD
LD9DB:			LDA			LD971,Y		; Load first byte of LD971 command packet to A
				TAX						; Transfer A to X
				INY						; Increment Y
				LDA			LD971,Y		; Load second byte to LD971 command packet to A
				STA			DATAHIGH		; Store second byte to DATAHIGH
				STX			IDELOW		; Store first byte to IDELOW
				INY						; Increment Y
				CPY			#$0C
				BNE			LD9DB			; Read next word from LD971, until Y = 0Ch
				RTS

				INC			$CF
				BNE			LD9F9
				INC			$D0
				BNE			LD9F9
				INC			$D1
LD9F9:			RTS

LD9FA:			DEC			$CF			; General Delay loop for IDE timeouts etc.
				BNE			LD9F9			; On second thoughts, could be cd-rom sector incrementing?
				DEC			$D0
				BNE			LD9F9
				DEC			$D1
				RTS

LDA05:			.byte		"Command Time Out Error."
				.byte		$FF

LDA1D:			LDA			#$00
				STA			$F7
				LDX			#$04
				STX			IDEADD
				LDA			$F5
				STA			IDELOW
				INX
				STX			IDEADD
				LDA			$F6
				STA			IDELOW
				JSR			LD8FE

LDA37:			JSR			LD8C7
				LDA			$0100
				CMP			#$1B
				BNE			LDA45
				LDA			#$28
				STA			$D1

LDA45:			BIT			V64STATUS
				BMI			LDA70
				JSR			LD9FA
				BNE			LDA45
				LDA			$FF
				CMP			#$40
				BNE			LDA5D
				JSR			LD420
				LDX			#$A0
				JMP			LD0AE

LDA5D:			JSR			BLINKON
				JSR			PRINTSTRING

				.byte		$00, $07
				.word		LDA05			; "Command Time Out Error."

				JSR			BLINKOFF
				JSR			LD420
				JMP			L9A1D

LDA70:			BIT			V64STATUS
				JSR			LDA88
				STA			$FE
				AND			#$01
				BNE			LDA91
				LDA			$FE
				AND			#$08
				BEQ			LDA88
				JSR			LDE27
				JMP			LDA37

LDA88:			LDA			#$07		; Select IDE Status/Command register
				STA			IDEADD
				LDA			IDELOW	; Load A with Status byte
				RTS

LDA91:			LDA			#$01		; Select IDE Error/Features register
				STA			IDEADD
				LDA			IDELOW
				STA			$BE		; Store Error byte to $BE

				LDA			#$02		; Select IDE Interrupt Reason register (when using packet device)
				STA			IDEADD
				LDA			IDELOW
				STA			$F9		; Store Interrupt Reason byte to $F9

				LDX			#$04		; Select IDE Byte Count Low register
				STX			IDEADD
				LDA			#$12
				STA			IDELOW	; Set Byte Count Low to 12h

				INX					; Increment X (04h to 05h ?)
				STX			IDEADD	; Select IDE Byte Count High register
				LDA			#$00
				STA			IDELOW	; Set Byte Count High to 00h
				JSR			LD990		; Retry sending command packet again ???
				LDA			#$00
				STA			$CF
				STA			$D0
				LDA			#$0A
				STA			$D1		; Set timeout delay time

LDAC5:			BIT			V64STATUS
				BMI			LDAF0
				JSR			LD9FA
				BNE			LDAC5
				LDA			$FF
				CMP			#$40
				BNE			LDADD
				JSR			LD420
				LDX			#$A0
				JMP			LD0AE

LDADD:			JSR			BLINKON		; Turn character blink ON
				JSR			PRINTSTRING

				.byte		$00, $07
				.word		LDA05			; "Command Time Out Error."

				JSR			BLINKOFF		; Turn character blink OFF
				JSR			LD420
				JMP			L9A1D

LDAF0:			LDA			#$07
				STA			IDEADD
				LDA			IDELOW
				AND			#$08
				BEQ			LDB5F
				JSR			LDBAE
				LDA			$0530
				CMP			#$3A
				BNE			LDB1C
				JSR			BLINKON
				JSR			PRINTSTRING

				.byte		$00, $07
				.word		LDB86			; "No Disc In CD Drive."

				JMP			LDB16

LDB13:			JSR			LD420
LDB16:			JSR			BLINKOFF
				JMP			L9A1D

LDB1C:			LDA			$BE
				AND			#$F0
				CMP			#$40
				BNE			LDB31
				JSR			BLINKON
				JSR			PRINTSTRING

				.byte		$00, $07
				.word		LDB9B			; "CD Hardware Error."

				JMP			LDB13

LDB31:			CMP			#$30
				BNE			LDB48
				LDA			$FF
				CMP			#$40
				BEQ			LDB65
				JSR			BLINKON
				JSR			PRINTSTRING

				.byte		$00, $07
				.word		LDB66			; "CD Read Error."

				JMP			LDB13

LDB48:			CMP			#$50
				BNE			LDB5F
				LDA			$FF
				CMP			#$40
				BEQ			LDB65
				JSR			BLINKON
				JSR			PRINTSTRING

				.byte		$00, $07
				.word		LDB75			; "Parameter Error."

				JMP			LDB13

LDB5F:			JSR			LDA88
				JMP			LDA1D

LDB65:			RTS

LDB66:			.byte		"CD Read Error."
				.byte		$FF

LDB75:			.byte		"Parameter Error."
				.byte		$FF

LDB86:			.byte		"No Disc In CD Drive."
				.byte		$FF

LDB9B:			.byte		"CD Hardware Error."
				.byte		$FF

LDBAE:			LDX			#$04
				STX			IDEADD
				LDA			IDELOW
				INX
				STX			IDEADD
				LDA			IDELOW
				LDY			#$00			; Select IDE Data register (also, start Y at 00h)
				STY			IDEADD
LDBC2:			LDA			IDELOW		; Load A with Low data byte
				STA			$0524,Y		; Store Low byte to $0524, Y offset
				INY						; Increment Y
				LDA			DATAHIGH		; Load A with High data byte
				STA			$0524,Y		; Store High byte to $0524, Y offset
				INY						; Increment Y
				CPY			#$12
				BNE			LDBC2			; Read next Word from drive, until Y = 12h (18)
				RTS

LDBD5:			JSR			LE558			;  Clear something out at $0100 ???
				LDA			#$42
				STA			$0100
				LDA			#$40
				STA			$0102
				LDA			#$01
				STA			$0103
				LDA			#$10
				STA			$0107
				STA			$F5
				LDA			#$00
				STA			$F6
				JMP			LDA1D

LDBF5:			PHA
				JSR			LD89D			; Zero addresses $0100 to $010B
				LDA			#$1B
				STA			$0100
				PLA
				STA			$0104
				CMP			#$03
				BEQ			LDC0F
				LDA			#$00
				STA			$F5
				STA			$F6
				JMP			LDA1D

LDC0F:			JSR			LDC38
				LDA			$8C
				AND			#$20
				PHP
				BEQ			LDC1E
				LDA			#$00
				JSR			LDC5D

LDC1E:			JSR			LDBD5
				PLP
				BEQ			LDC27
				JSR			LDC5D

LDC27:			LDA			$053B
				AND			#$F0
				CMP			#$10
				BNE			LDC1E
				LDA			$053B
				AND			#$04
				STA			$E4
				RTS

LDC38:			LDA			#$02

LDC3A:			PHA
				JSR			LE558
				LDA			#$43
				STA			$0100
				PLA
				STA			$0101
				LDA			#$01
				STA			$0106
				LDA			#$03
				STA			$0107
				STA			$F6
				LDA			#$24
				STA			$0108
				STA			$F5
				JMP			LDA1D

LDC5D:			BNE			LDC6A
				LDA			#$00
				STA			$08
				LDA			#$02
				STA			$09
				JMP			LDC72

LDC6A:			LDA			#$36
				STA			$08
				LDA			#$05
				STA			$09

LDC72:			JSR			LDCCA

LDC75:			JSR			BLANKPPU
				LDA			$AA
				TAX
				SEC
				SBC			#$60
				LDA			$AB
				SBC			#$00
				BCC			LDC86
				LDX			#$60

LDC86:			STX			$0A
				LDA			$AA
				SEC
				SBC			$0A
				STA			$AA
				LDA			$AB
				SBC			#$00
				STA			$AB
				LDA			$08
				STA			$0105
				LDA			$09
				STA			$0106
				JSR			$0100
				JSR			LD2AB
				LDA			$8C
				AND			#$20
				BEQ			LDCB1
				JSR			BLANKPPU
				JMP			L9A1D

LDCB1:			LDA			$AA
				ORA			$AB
				BNE			LDCBA
				JMP			BLANKPPU

LDCBA:			CLC
				LDA			$08
				ADC			$0A
				STA			$08
				LDA			$09
				ADC			#$00
				STA			$09
				JMP			LDC75

LDCCA:			LDY			#$07

LDCCC:			LDA			LDCD6,Y
				STA			$0100,Y
				DEY
				BPL			LDCCC
				RTS

LDCD6:			JSR			PRINTSTRING

				.byte		$00, $60
				.word		$0536

				RTS

LDCDE:			LSR			$15
				ROR			$14
				CLC
				LDA			$14
				ADC			$4C
				STA			$4C
				LDA			$15
				ADC			$4D
				STA			$4D
				PHP
				JSR			READKEYPAD
				CMP			#$20
				BNE			LDD00
				JSR			BLANKPPU
				JSR			LD420
				JMP			L9A1D

LDD00:			JSR			LD497
				INC			$15
				LDY			$14
				BEQ			LDD0C

LDD09:			JSR			LDD5B

LDD0C:			DEC			$15
				BNE			LDD09
				PLP
				BCS			LDD14

LDD13:			RTS

LDD14:			LDA			$5D
				SEC
				ADC			#$00
				STA			$5D
				TAX
				CPX			#$0A
				PHP
				BNE			LDD25
				LDX			#$00
				STX			$5D

LDD25:			JSR			PRINTSTRING

				.byte		$0C, $47
				.word		$0081

				PLP
				BNE			LDD13
				LDA			$5E
				ADC			#$00
				STA			$5E
				TAX
				CPX			#$0A
				PHP
				BNE			LDD3F
				LDX			#$00
				STX			$5E

LDD3F:			JSR			PRINTSTRING

				.byte		$0B, $47
				.word		$0081

				PLP
				BNE			LDD13
				INC			$5F
				LDX			$5F
				JSR			PRINTSTRING

				.byte		$0A, $47
				.word		$0081

				RTS

LDD55:			JSR			LD420
				JMP			L9A16

LDD5B:			STY			$0F
				JSR			L9E50		; Puts variables into C008,9,A,B,C ?
				LDA			$72
				BEQ			LDD7A

LDD64:			LDX			IDELOW
				LDA			DATAHIGH
				CPX			DRAMLOW
				BNE			LDD55
				CMP			DATAHIGH
				BNE			LDD55
				DEY
				BNE			LDD64
				JMP			LDD99

LDD7A:			CPY			#$00
				BEQ			LDD82

LDD7E:			CPY			#$20
				BCC			LDD93

LDD82:			STY			$B1
				LDY			#$20
				JSR			LDDAC
				LDA			$B1
				SEC
				SBC			#$20
				TAY
				BNE			LDD7E
				BEQ			LDD99

LDD93:			LDA			PPU
				DEY
				BNE			LDD93

LDD99:			LDA			$0F
				BEQ			LDDA5
				CLC
				ADC			$16
				STA			$16
				BCS			LDDA5
				RTS

LDDA5:			INC			$12
				BNE			LDDAB
				INC			$13
LDDAB:			RTS

LDDAC:			LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				LDA			PPU
				RTS

LDE0D:			JSR			LDF52

LDE10:			LDA			IDELOW
				STA			$2007
				JSR			LDF52
				BCC			LDE26
				LDA			DATAHIGH
				STA			$2007
				JSR			LDF52
				BCS			LDE10
LDE26:			RTS

LDE27:			LDX			#$04
				STX			IDEADD
				LDA			IDELOW
				STA			$14
				STA			$AA
				INX
				STX			IDEADD
				LDA			IDELOW
				STA			$15
				STA			$AB
				ORA			$14
				BEQ			LDE91
				LDY			#$00
				STY			IDEADD
				STY			$00
				LDA			$FF
				AND			#$C0
				BEQ			LDE5A
				CMP			#$80
				BEQ			LDE0D
				CMP			#$40
				BEQ			LDE92
				JMP			LDCDE

LDE5A:			LDA			$0100
				CMP			#$43
				BEQ			LDE6C
				LDA			#$36
				STA			$00
				LDA			#$05
				STA			$01
				JMP			LDE70

LDE6C:			LDA			#$02
				STA			$01

LDE70:			JSR			LDF52

LDE73:			LDA			IDELOW
				STA			($00),Y
				INY
				BNE			LDE7D
				INC			$01

LDE7D:			JSR			LDF52
				BCC			LDE91
				LDA			DATAHIGH
				STA			($00),Y
				INY
				BNE			LDE8C
				INC			$01

LDE8C:			JSR			LDF52
				BCS			LDE73

LDE91:			RTS

LDE92:			LSR			$15
				ROR			$14
				INC			$15
				LDA			$FF
				AND			#$01
				BNE			LDEAA
				LDY			$14
				BEQ			LDEA5

LDEA2:			JSR			LDEBC

LDEA5:			DEC			$15
				BNE			LDEA2
				RTS

LDEAA:			LDY			$14
				BEQ			LDEB7

LDEAE:			LDA			IDELOW
				STA			MPEG_DDPR		; MPEG DRAM data reg
				DEY
				BNE			LDEAE

LDEB7:			DEC			$15
				BNE			LDEAE
				RTS

LDEBC:			LDA			#$00
				STA			$B4
				STA			$B7
				CPY			#$00
				BEQ			LDECA

LDEC6:			CPY			#$20
				BCC			LDEDA

LDECA:			STY			$B1
				LDY			#$20
				JSR			LDEDA
				LDA			$B1
				SEC
				SBC			#$20
				TAY
				BNE			LDEC6
				RTS

; ******** Routine appears to be loop for transferring data from CD drive to MPEG chip... *********
; Waits for MPEG FIFO to be empty first, then transfers certain number of words from CD.
; Counters seem to be based on $C7, $C6, $C5.
; LDF29 loads these counters with 04h, 8Ah, 02h, which is possibly a multiple of 2352 ??? (2352 * 126)
; (ie. there are 2352 bytes per sector in the VCD format.)
LDEDA:			LDA			MPEG_IDPR		; Read MPEG bitstream data port (to read WRR: Word Remaining Register)
				AND			#$3F
				BEQ			LDEF5		; If FIFO buffer empty, branch to LDEF5
				INC			$B4			; ...else...
				BNE			LDEDA		; Wait for FIFO empty again until $B4 wraps to 00h
				INC			$B7
				BNE			LDEDA		; Wait for FIFO empty again until $B7 wraps to 00h
				JSR			LD42D		; Check for correct MPEG chip ID + soft reset + reset parser + enable sector counting + disable DMA / IRQs
				JSR			LD71F		; Set MPEG border colour 1
				JSR			LD88A		; Set MPEG chip to "MPEG Stream input" mode, then select "Normal Play Forward" mode.
				JMP			LDEDA		; Repeat this loop until MPEG FIFO is empty

LDEF5:			LDA			$C6			; (Branches here when MPEG FIFO is empty) - Start transferring data I guess?
				ORA			$C7
				BNE			LDF0E

LDEFB:			LDA			IDELOW
				DEY
				DEC			$C5
				BEQ			LDF29
				CPY			#$00
				BEQ			LDF51		; RTS
				LDA			IDELOW
				DEY
				JMP			LDF29

LDF0E:			LDA			$C7
				BNE			LDF39
				TYA
				CMP			$C6
				BCC			LDF39

LDF17:			LDA			IDELOW		; Read byte from CD
				STA			MPEG_IDPR		; Write to MPEG bitstream data port
				DEY
				DEC			$C6
				BNE			LDF17		; Loop until $C6 = 00h
				CPY			#$00
				BEQ			LDF51
				JMP			LDEFB

LDF29:			LDA			#$04
				STA			$C7
				LDA			#$8A
				STA			$C6
				LDA			#$02
				STA			$C5
				CPY			#$00
				BEQ			LDF51		; RTS

LDF39:			STY			$0F			; Start Y at 0x0f (15)
				SEC						; Set carry (ready for subtract)
				LDA			$C6			; Get $c6
				SBC			$0F			; Subtract address $06 from $c6
				STA			$C6			; Store result back into $c6
				LDA			$C7			; Get $c7
				SBC			#$00		; Subtract (with cary) 0x00 from $c7
				STA			$C7			; Store result back into $c7

LDF48:			LDA			IDELOW		; Read byte from CD
				STA			MPEG_IDPR		; Write to MPEG bitstream data port
				DEY
				BNE			LDF48		; Loop "Y" times

LDF51:			RTS

LDF52:			LDA			$14
				SEC
				SBC			#$01
				STA			$14
				LDA			$15
				SBC			#$00
				STA			$15
				RTS

LDF60:			LDX			$B8
				TXS
				LDA			#$FF
				RTS

LDF66:			JSR			LD89D			; Zero addresses $0100 to $010B
				LDA			#$12
				STA			$0100
				LDA			#$24
				STA			$0104
				STA			$F5
				LDA			#$00
				STA			$F6
				JMP			LDA1D

LDF7C:			.byte		"No Any Games In CD"
				.byte		$FF

LDF8F:			.byte		"  Loaded:   0 M"
				.byte		$FF

LDF9F:			.byte		"Verified:   0 M"
				.byte		$FF

LDFAF:			.byte		"FOR GAME TEST ONLY"

LDFC1:			LDA			$BB			; Load A with $BB (V64STATUS backup reg??)
				PHA						; Push A ($BB) to stack
				LDY			#$00		; Load Y with 0
				STY			$BB			; Zero $BB
				STY			V64STATUS	; Zero V64STATUS

LDFCB:			LDA			$8003,Y
				CMP			LDFAF,Y		; compare string at $8003 to "FOR GAME TEST ONLY" ????
				BNE			LDFDD
				INY
				CPY			#$12
				BNE			LDFCB		; Loop until Y = 0
				LDX			$8002
				LDA			#$00

LDFDD:			PHP			; Push processor status on stack
				PLA			; Pull status? from stack into A
				TAY			; Transfer A (status?) to Y
				PLA			; Pull A from stack
				STA			$BB
				STA			V64STATUS
				TYA			; Transfer status? to A
				PHA			; Push A to stack
				PLP			; Pull processor status back from stack
				RTS

LDFEA:			JSR			LEBAF			; Get number of files, or file size?
				LDA			$A8				; (the register at $A8 usually stores the file size. ie. 1 = 64M, 2 = 128M, 3 = 192M, 4 = 256M)??
				BNE			LDFF9			; Start loading or verifying ROM if file size is NOT zero...
				JSR			PRINTSTRING		; ..Else, display this...

				.byte		$00, $07
				.word		LDF7C			; "No Any Games In CD"

				RTS							; ...then return.

LDFF9:			LDA			#$00			; Start loading or verifying ROM !! (I think, OzOnE).
				STA			V64STATUS
				LDA			$A8
				CMP			$8002
				BCC			LE00A
				LDA			$8002
				STA			$A8

LE00A:			LDA			$BB				; Load V64STATUS backup reg?
				STA			V64STATUS		; Write back to V64STATUS
				JSR			LEF0E
				LDA			#$C0
				STA			$FF
				LDA			#$08
				STA			$E5				; ($E5 appears to be pointer to each 8192KByte bank of DRAM, but usually starts at 0x08?)
				LDA			#$80
				STA			DRAMADD0
				LDA			#$00			; Zero some registers, and zero DRAMADD3:1
				STA			$72
				STA			BACKUPMD
				STA			$4C
				STA			$4D
				STA			$5D
				STA			$5E
				STA			$5F
				STA			DRAMADD1
				STA			DRAMADD2
				STA			DRAMADD3
				LDA			#$08
				STA			$C002
				STA			$A7
				LDA			$8C
				BPL			LE050				; Skip to print "Loaded" instead of "Verified" if MSB of $8C = 0
				DEC			$72					; Decrement $72 (start at 0xff if starting verify?)
				JSR			PRINTSTRING

				.byte		$00, $07
				.word		LDF9F				;  "Verified:   0 M"

				JMP			LE057

LE050:			JSR			PRINTSTRING

				.byte		$00, $07
				.word		LDF8F				; "  Loaded:   0 M"
				
; ***** START LOADING / VERIFYING ROM ***** (can't quite see how ROM data is transferring yet?) OzOnE
LE057:			JSR			LE0CF				; Load the next part of the ROM ??
				LDA			$18					; Check if finished loading?
				BEQ			LE0CC				; (Jump to L817A) to display game name on screen, then jump to "You Can Play Game Now" !!

				INC			$E5					; ROM must be bigger than 64M (educated guess)...
				LDA			#$80				; Increment DRAM pointers to next 64M (8192K) block, then continue loading / verifying.
				STA			DRAMADD1
				LDA			#$98
				STA			$C002
				STA			$A7
				LDA			$0104				; Load $0104
				CLC								; (clear carry bit first)
				ADC			#$10				; Add 0x10 (16)
				STA			$0104				; Store back into $0104
				LDA			$0103				; Load $0103
				ADC			#$00				; Add zero (just add the carry, if set)
				STA			$0103				; Store back into $0103
				JSR			LE0CF				; Load the next part of the ROM ??
				LDA			$18					; Check if finished loading?
				BEQ			LE0CC				; If finished, (Jump to L817A) to display game name on screen, then jump to "You Can Play Game Now" !!

				INC			$E5					; ROM must be bigger than 128M (educated guess)...
				LDA			#$80				; Increment DRAM pointers to next 64M (8192K) block, then continue loading / verifying.
				STA			DRAMADD2
				LDA			#$0A
				STA			$C003
				STA			$A9
				LDA			$0104				; Load $0104
				CLC								; (clear carry bit first)
				ADC			#$10				; Add 0x10 (16)
				STA			$0104				; Store back into $0104
				LDA			$0103				; Load $0103
				ADC			#$00				; Add zero (just add the carry, if set)
				STA			$0103				; Store back into $0103
				JSR			LE0CF				; Load the next part of the ROM ??
				LDA			$18					; Check if finished loading?
				BEQ			LE0CC				; If finished, (Jump to L817A) to display game name on screen, then jump to "You Can Play Game Now" !!

				INC			$E5					; ROM must be bigger than 192M (educated guess)...
				LDA			#$80				; Increment DRAM pointers to next 64M (8192K) block, then continue loading / verifying.
				STA			DRAMADD3
				LDA			#$BA
				STA			$C003
				STA			$A9
				LDA			$0104				; Load $0104
				CLC								; (clear carry bit first)
				ADC			#$10				; Add 0x10 (16)
				STA			$0104				; Store back into $0104
				LDA			$0103				; Load $0103
				ADC			#$00				; Add zero (just add the carry, if set)
				STA			$0103				; Store back into $0103
				JSR			LE0CF				; Load the next part of the ROM ??

LE0CC:			JMP			L817A				; Reached 256M, must be finished - Display game name on screen, and jump to "You Can Play Game Now" !!

LE0CF:			LDA			$18
				CMP			#$21
				BCC			LE0E6
				SBC			#$20
				STA			$18
				LDA			#$10
				STA			$0107
				LDA			#$00
				STA			$0108
				JMP			LE0F4

LE0E6:			LSR
				STA			$0107
				ROR
				AND			#$80
				STA			$0108
				LDA			#$00
				STA			$18

LE0F4:			LDA			#$00
				STA			$16
				STA			$12
				STA			$13

LE0FC:			LDA			#$FF
				STA			$F5
				STA			$F6
				JMP			LDA1D

				JSR			LD89D			; Zero addresses $0100 to $010B
				LDA			#$BB
				STA			$0100
				LDA			#$FF
				STA			$0102
				STA			$0103
				JMP			LDA1D

LE118:			JSR			PRINTSTRING

				.byte		$01, $00
				.word		LD2B4			; "Play    "

				.byte		$60

LE120:			JSR			PRINTSTRING

				.byte		$00, $00
				.word		LD2C6			; "Pause   "

				RTS

LE128:			LDX			#$00

LE12A:			CMP			#$0A
				BCC			LE133
				SBC			#$0A
				INX
				BNE			LE12A

LE133:			STA			$B1
				TXA
				ASL
				ASL
				ASL
				ASL
				ORA			$B1
				RTS

LE13D:			.byte		"Total    Programs"
				.byte		$FF

LE14F:			.byte		"Track"
				.byte		$FF

LE155:			.byte		$EE,$02,$DC,$05,$CA,$08,$B8,$0B,$A6,$0E,$94,$11

LE161:			LDA			#$00
				JSR			LDC3A
				JSR			LE819
				INC			$F4
				JSR			LE118
				JSR			PRINTSTRING

				.byte		$03, $07
				.word		LE13D				; "Total    Programs"

				LDA			$F4
				JSR			LE128
				TAX
				JSR			PRINTSTRING

				.byte		$09, $07
				.word		$0001

				LDA			#$01
				STA			$E3
				STA			$F3
				JSR			LD441

LE18B:			JSR			LE431

LE18E:			JSR			LE558
				LDA			#$A5
				STA			$0100
				LDY			#$00

LE198:			LDA.wy		$00D3,Y
				STA			$0103,Y
				INY
				CPY			#$03
				BNE			LE198
				CLC
				LDY			#$02

LE1A6:			LDA.wy		$00FB,Y
				SBC.wy		$00D3,Y
				STA			$0107,Y
				DEY
				BPL			LE1A6
				LDA			#$00
				STA			$F5
				STA			$F6
				JSR			LDA1D
				JSR			LE41C

LE1BE:			JSR			LDBD5
				LDA			$053F
				CMP			#$06
				BCS			LE1BE
				LDA			$053F
				CMP			$D3
				BCC			LE1BE
				BNE			LE1E1
				LDA			$0540
				CMP			$D4
				BCC			LE1BE
				BNE			LE1E1
				LDA			$0541
				CMP			$D5
				BCC			LE1BE

LE1E1:			SEC
				LDA			$0541
				SBC			$D5
				STA			$18
				LDY			#$02

LE1EB:			LDA			$053F,Y
				STA.wy		$00EB,Y
				DEY
				BPL			LE1EB
				JSR			LE3B1
				BCC			LE1FC
				JMP			LE299

LE1FC:			LDA			$B2
				CMP			$F3
				BEQ			LE207
				STA			$F3
				JSR			LE41C

LE207:			JSR			LE323
				BIT			$E3
				BPL			LE247
				LDA			$18
				CMP			#$E1
				BCC			LE26C
				LDA			$B3
				ASL
				TAY
				LDX			#$02
				CLC

LE21B:			LDA			$053F,X
				ADC			LE155,Y
				STA			$D3,X
				STA			$EB,X
				INY
				DEX
				BNE			LE21B
				LDA			$053F
				ADC			#$00

LE22E:			STA			$D3
				STA			$EB
				JSR			LE3A8
				JSR			LE3B1
				BCC			LE23D
				JMP			LE28E

LE23D:			LDA			$B2
				STA			$F3
				JSR			LE41C
				JMP			LE18E

LE247:			BVC			LE26C
				LDA			$18
				CMP			#$E1
				BCC			LE26C
				LDA			$B3
				ASL
				TAY
				LDX			#$02
				SEC

LE256:			LDA			$053F,X
				SBC			LE155,Y
				STA			$D3,X
				STA			$EB,X
				INY
				DEX
				BNE			LE256
				LDA			$053F
				SBC			#$00
				JMP			LE22E

LE26C:			JSR			LD091
				JSR			READKEYPAD
				JSR			LD47C			; Update "Mono" / "Stereo" / "By Pass" message
				JSR			LD497
				LDA			$8D
				CMP			#$20
				BEQ			LE28E
				CMP			#$10
				BNE			LE29C
				JSR			LE850
				JSR			LD441
				JSR			LE456
				JMP			LE1BE

LE28E:			JSR			LD89D			; Zero addresses $0100 to $010B
				LDA			#$4E
				STA			$0100
				JSR			LDA1D

LE299:			JMP			BLANKPPU

LE29C:			LDA			$E3
				AND			#$01
				BNE			LE2A5
				JMP			LE1BE

LE2A5:			LDA			$8D
				CMP			#$08
				BNE			LE2B6
				DEC			$F3
				BNE			LE2B3
				LDA			$F4
				STA			$F3

LE2B3:			JMP			LE18B

LE2B6:			CMP			#$04
				BNE			LE2CB
				INC			$F3
				LDA			$F3
				CMP			$F4
				BCC			LE2B3
				BEQ			LE2B3
				LDA			#$01
				STA			$F3
				JMP			LE18B

LE2CB:			CMP			#$01
				BNE			LE2F2
				JSR			LE850
				JSR			LD441
				JSR			PRINTSTRING

				.byte		$01, $00
				.word		LE308				; "Fast Forward"

				LDA			#$81

LE2DE:			STA			$E3
				LDA			#$00
				STA			$B3
				LDY			#$02

LE2E6:			LDA			$053F,Y
				STA.wy		$00D3,Y
				DEY
				BPL			LE2E6

LE2EF:			JMP			LE1BE

LE2F2:			CMP			#$02
				BNE			LE2EF
				JSR			LE850
				JSR			LD441
				JSR			PRINTSTRING

				.byte		$01, $00
				.word		LE315				; "Fast Backward"

				LDA			#$41
				JMP			LE2DE

LE308:			.byte		"Fast Forward"
				.byte		$FF

LE315:			.byte		"Fast Backward"
				.byte		$FF

LE323:			LDY			#$02

LE325:			LDA			$053F,Y
				STA.wy		$005D,Y
				DEY
				BPL			LE325
				LDX			#$00
				LDY			#$00

LE332:			LDA			$5F
				SEC
				SBC			#$94
				STA			$5F
				LDA			$5E
				SBC			#$11
				STA			$5E
				LDA			$5D
				SBC			#$00
				STA			$5D
				BCC			LE34B
				INX
				JMP			LE332

LE34B:			CLC
				LDA			$5F
				ADC			#$94
				STA			$5F
				LDA			$5E
				ADC			#$11
				STA			$5E
				LDA			$5D
				ADC			#$00
				STA			$5D

LE35E:			LDA			$5F
				SEC
				SBC			#$4B
				STA			$5F
				LDA			$5E
				SBC			#$00
				STA			$5E
				LDA			$5D
				SBC			#$00
				STA			$5D
				BCC			LE377
				INY
				JMP			LE35E

LE377:			STY			$0A
				TXA
				JSR			LE128
				TAX
				JSR			PRINTSTRING

				.byte		$03, $01
				.word		$0001

				LDA			#$3A
				JSR			PRINTASCII
				LDA			$0A
				JSR			LE128
				JMP			LD287

				LDX			$053F
				JSR			PRINTSTRING

				.byte		$02, $01
				.word		$0001

				LDA			$0540
				JSR			LD287
				LDA			$0541
				JMP			LD287

LE3A8:			LDA			$B3
				CMP			#$05
				BEQ			LE3B0
				INC			$B3

LE3B0:			RTS

LE3B1:			LDA			#$01
				STA			$B2
				JSR			LE3EE
				BEQ			LE3BE
				BCS			LE3C0
LE3BC:			SEC
				RTS

LE3BE:			CLC
				RTS

LE3C0:			LDA			$B2
				CMP			$F4
				BCS			LE3D2
				INC			$B2
				JSR			LE3EE
				BEQ			LE3BE
				BCS			LE3C0
				DEC			$B2
				RTS

LE3D2:			CLC
				LDA			$FD
				SBC			$ED
				STA			$B1
				LDA			$FC
				SBC			$EC
				ORA			$B1
				STA			$B1
				LDA			$FB
				SBC			$EB
				ORA			$B1
				BEQ			LE3BC
				BCC			LE3BC
				JMP			LE3BE

LE3EE:			LDA			#$00
				STA			$B1
				LDA			$B2
				ASL
				ROL			$B1
				ASL
				ROL			$B1
				ASL
				ROL			$B1
				STA			$00
				LDA			$B1
				CLC
				ADC			#$02
				STA			$01
				LDY			#$01
				LDA			$EB
				CMP			($00),Y
				BEQ			LE40F

LE40E:			RTS

LE40F:			INY
				LDA			$EC
				CMP			($00),Y
				BNE			LE40E
				INY
				LDA			$ED
				CMP			($00),Y
				RTS

LE41C:			JSR			PRINTSTRING

				.byte		$01, $03
				.word		LE14F			; "Track"

				LDA			$F3
				JSR			LE128
				TAX
				JSR			PRINTSTRING

				.byte		$07, $03
				.word		$0001

				RTS

LE431:			LDA			#$00
				STA			$B1
				LDA			$F3
				ASL
				ROL			$B1
				ASL
				ROL			$B1
				ASL
				ROL			$B1
				STA			$00
				LDA			$B1
				CLC
				ADC			#$02
				STA			$01
				LDY			#$01

LE44B:			LDA			($00),Y
				STA.wy		$00D2,Y
				INY
				CPY			#$04
				BNE			LE44B
				RTS

LE456:			LDA			$E3
				AND			#$C0
				BEQ			LE463
				LDA			#$01
				STA			$E3
				JMP			LE118

LE463:			JSR			LD89D			; Zero addresses $0100 to $010B
				LDA			#$4B
				STA			$0100
				LDA			$E3
				AND			#$01
				EOR			#$01
				STA			$E3
				STA			$0108
				BEQ			LE47E
				JSR			LE118
				JMP			LDA1D

LE47E:			JSR			LE120
				JMP			LDA1D

LE484:			.byte		"Waiting..."
				.byte		$FF

LE48F:			JSR			LE118
				LDA			#$00
				STA			$D9
				STA			$DA
				RTS

LE499:			.byte		"L / R   "
				.byte		$FF

LE4A2:			.byte		"R / L   "
				.byte		$FF

LE4AB:			.byte		"L Only  "
				.byte		$FF

LE4B4:			.byte		"R Only  "
				.byte		$FF

LE4BD:			INC			$D2
				LDA			$D2
				CMP			#$04
				BNE			LE4C9
				LDA			#$00
				STA			$D2

LE4C9:			LDA			$D2
				BNE			LE4D7
				JSR			PRINTSTRING

				.byte		$10, $00
				.word		LE499			; "L / R   "

				JMP			LE4FA

LE4D7:			CMP			#$01
				BNE			LE4E5
				JSR			PRINTSTRING

				.byte		$10, $00
				.word		LE4A2			; "R / L   "

				JMP			LE4FA

LE4E5:			CMP			#$02
				BNE			LE4F3
				JSR			PRINTSTRING

				.byte		$10, $00
				.word		LE4AB			; "L Only  "

				JMP			LE4FA

LE4F3:			JSR			PRINTSTRING

				.byte		$10, $00
				.word		LE4B4			; "R Only  "

LE4FA:			LDA			$D2
				ASL
				ASL
				ASL
				ASL
				ASL
				ORA			#$1C
				TAX
				LDA			#$C0
				JMP			WRITEMPEG

LE509:			LDA			$8D
				AND			#$08
				BEQ			LE524
				DEC			$AF
				BPL			LE517
				LDA			#$00

LE515:			STA			$AF

LE517:			LDX			$AF
				INX
				JSR			PRINTSTRING

				.byte		$07, $40
				.word		$0081

				JMP			LD519

LE524:			LDA			$8D
				AND			#$04
				BEQ			LE537
				INC			$AF
				LDA			$AF
				CMP			#$08
				BCC			LE517
				LDA			#$07
				JMP			LE515

LE537:			RTS

LE538:			LDA			$EB
				JSR			LE128
				TAX
				JSR			PRINTSTRING

				.byte		$09, $00
				.word		$0001

				LDA			#$3A
				JSR			PRINTASCII
				LDA			$EC
				JSR			LE128
				TAX
				JSR			PRINTSTRING

				.byte		$00, $80
				.word		$0001

				RTS

LE558:			JSR			LD89D			; Zero addresses $0100 to $010B

LE55B:			JSR			LDA1D
				LDA			$FE
				AND			#$01
				BNE			LE55B
				RTS

LE565:			LDA			#$00
				STA			$FF
				LDA			#$FF
				STA			$DC
				JSR			LDF66			; Start of CD Reading / disk check routines ??
				LDA			#$03
				JSR			LDBF5			; Print filename to screen (big guess!)
				JSR			L90DC			; Print something else to screen ??
				JSR			LE819			; Don't know, too vague
				LDA			$E4
				BNE			LE582			; Branch to LE582 (below) if not equal.
				JMP			LE161			; ...else Jump to LE161 to display "Total    Programs" for CD Audio tracks ?

LE582:			JSR			LEB3F			; Search CD for "CD-RTOS CD-BRIDGE " string ? (VCD system ID header)
				BEQ			LE58A			; Branch to LE58A (below) if match found (or whatever LEB3F returns).
				JMP			LDFEA			; ...else Jump to LDFEA to check disk for / load game files.

LE58A:			LDA			#$40			; ****** VCD disk found - start playing ! *******
				STA			$FF
				STA			$A5
				LDA			#$E7
				JSR			WRITEPPU
				JSR			LE48F			; Print "Play" to screen, zero $DA and $D9
				JSR			LD519			; Init MPEG Volume control (unmute + set volume level)
				JSR			LD88A			; Set MPEG chip to "MPEG Stream input" mode, then select "Normal Play Forward" mode.
				LDA			#$00
				STA			$C8
				LDA			#$07
				STA			$DE
				JSR			LD73F			; Init MPEG chip for VCD playback (set for PAL / NTSC, setup video registers etc.)
				LDA			#$85			; Select MPEG "Interrupt Service Register" - used to clear interrupt flags....
				LDX			#$04			; "set" IFDC (I-Frame Detected - Clear) interrupt flag. (in other words, clears I-Frame interrupt)
				JSR			WRITEMPEG
				LDA			#$0A
				STA			$F2
				JSR			LD89D			; Zero addresses $0100 to $010B
				LDA			#$B9
				STA			$0100			; Set $0100 to #$B9
				LDA			#$10
				STA			$0109			; Set $0109 to #$10
				LDA			#$00
				STA			$F3
				LDA			#$80
				STA			$F1
				JSR			LE7D5			; Update some sort of pointers ($B1 etc)???
				JSR			LE7EB			; Similar to above, but uses different start offsets??? Don't know, don't care!

LE5CF:			BIT			$DA				; Test register
				BPL			LE613			; Branch to LE613 if MSB = 0
				JSR			LD7E5
				JSR			LE4C9			; Update "L / R", "R / L", "L only", "R only" audio channels message
				JSR			LE538			; Does ROL stuff with $B1, then appears to blank screen text or something? - could be timeout to.....
				LDA			#$00			; blank "L / R" stuff, or to display track / time info ??
				STA			$0C

LE5E0:			JSR			LD091			; Wait for PPU / MPEG VSYNC ??
				INC			$0C
				LDA			$0C
				CMP			#$D0
				BNE			LE5EE
				JSR			LE846			; Shows track / time info or something?

LE5EE:			JSR			READKEYPAD
				AND			#$20			; Test for STOP button?
				BNE			LE62B
				LDA			$8D
				AND			#$10			; Test for PLAY / PAUSE button?
				BNE			LE60A
				JSR			LE509
				LDA			$8D
				AND			#$01			; Test for FF >> button?
				BEQ			LE5E0
				JSR			LE4BD			; Step through "L / R", "R / L", "L only", "R only" audio modes (for Karaoke VCD disks)
				JMP			LE5E0

LE60A:			LDA			$DA
				AND			#$7F
				STA			$DA
				JSR			LE846			; Print string to screen? (possibly update track / time info?)

LE613:			JSR			LE6A6			; Update screen counters?
				LDA			$A5
				BNE			LE61F
				LDA			#$EF
				JSR			WRITEPPU

LE61F:			JSR			LE6DA
				JSR			LE8F6
				BNE			LE631
				LDA			$FA
				BEQ			LE62E

LE62B:			JMP			LE69A

LE62E:			JSR			LE784

LE631:			BIT			$F1
				BMI			LE5CF
				INC			$F1
				LDA			$F1
				AND			#$3F
				CMP			#$0F
				BNE			LE5CF
				LDA			$F1
				AND			#$40
				STA			$F1
				LDA			$F2
				CMP			#$3C
				BCS			LE64F
				ADC			#$0A
				STA			$F2

LE64F:			STA			$EF
				JSR			LE538
				LDA			#$E7
				JSR			WRITEPPU
				JSR			LD42D		; Check for correct MPEG chip ID + soft reset + reset parser + enable sector counting + disable DMA / IRQs
				JSR			LD72F		; Set MPEG border colour 2
				JSR			LEA46
				BIT			$F1
				BVC			LE66E
				JSR			LE792
				BNE			LE69A
				JMP			LE5CF

LE66E:			LDA			#$00
				STA			$EE
				STA			$F0
				LDA			$0103
				STA			$EB
				LDA			$0104
				STA			$EC
				LDA			$0105
				STA			$ED
				JSR			LE738
				BNE			LE69A
				LDA			$EB
				STA			$0103
				LDA			$EC
				STA			$0104
				LDA			$ED
				STA			$0105
				JMP			LE5CF

LE69A:			JSR			LD42D		; Check for correct MPEG chip ID + soft reset + reset parser + enable sector counting + disable DMA / IRQs
				JSR			LD71F		; Set MPEG border colour 1
				LDA			#$00
				RTS

LE6A3:			.byte		$00,$00,$0A

LE6A6:			LDY			#$02

LE6A8:			LDA			LE6A3,Y
				STA.wy		$00EE,Y
				LDA			$0103,Y
				STA.wy		$00EB,Y
				DEY
				BPL			LE6A8
				JSR			LE738
				LDY			#$02

LE6BC:			LDA.wy		$00EB,Y
				STA			$0106,Y
				DEY
				BPL			LE6BC
				LDA			#$04
				STA			$C7
				LDA			#$8A
				STA			$C6
				LDA			#$02
				STA			$C5
				LDA			#$FF
				STA			$F5
				STA			$F6
				JMP			LDA1D

LE6DA:			LDA			$0106
				CMP			$D6
				BCC			LE71B
				BNE			LE6F3
				LDA			$0107
				CMP			$D7
				BCC			LE71B
				BNE			LE6F3
				LDA			$0108
				CMP			$D8
				BCC			LE71B

LE6F3:			INC			$F3
				LDY			#$00

LE6F7:			LDA.wy		$00D6,Y
				STA			$0106,Y
				INY
				CPY			#$03
				BNE			LE6F7
				LDA			$F3
				CMP			$F4
				BCC			LE70D
				LDA			#$FF
				STA			$FA
				RTS

LE70D:			JSR			LE7EB
				LDA			$F1
				AND			#$C0
				STA			$F1
				BMI			LE71B
				JSR			LE71C

LE71B:			RTS

LE71C:			LDA			#$00
				STA			$D9
				JSR			PRINTSTRING

				.byte		$00, $00
				.word		LE14F				; "Track"

				LDA			$F3
				CLC
				ADC			#$01
				JSR			LE128
				TAX
				JSR			PRINTSTRING

				.byte		$06, $00
				.word		$0001

				RTS

LE738:			LDA			$ED
				CLC
				ADC			$F0
				STA			$ED
				CMP			#$4B
				BCC			LE748
				SBC			#$4B
				STA			$ED
				SEC

LE748:			LDA			$EC
				ADC			$EF
				STA			$EC
				CMP			#$3C
				BCC			LE757
				SBC			#$3C
				STA			$EC
				SEC

LE757:			LDA			$EB
				ADC			$EE
				STA			$EB
				CMP			$FB
				BCC			LE781
				BEQ			LE771

LE763:			LDY			#$02

LE765:			LDA.wy		$00FB,Y
				STA.wy		$00EB,Y
				DEY
				BPL			LE765
				DEC			$FA
				RTS

LE771:			LDA			$EC
				CMP			$FC
				BCC			LE781
				BNE			LE763
				LDA			$ED
				CMP			$FD
				BCC			LE781
				BNE			LE763

LE781:			LDA			#$00
				RTS

LE784:			LDY			#$00

LE786:			LDA			$0106,Y
				STA			$0103,Y
				INY
				CPY			#$03
				BNE			LE786
				RTS

LE792:			LDA			$0104
				SEC
				SBC			$F2
				STA			$0104
				BCS			LE7A3
				ADC			#$3C
				STA			$0104
				CLC

LE7A3:			LDA			$0103
				SBC			#$00
				STA			$0103
				CMP			$0211
				BCC			LE7C7
				BNE			LE7C4
				LDA			$0104
				CMP			$0212
				BCC			LE7C7
				BNE			LE7C4
				LDA			$0105
				CMP			$0213
				BCC			LE7C7

LE7C4:			LDA			#$00
				RTS

LE7C7:			LDY			#$02

LE7C9:			LDA			$0211,Y
				STA			$0103,Y
				DEY
				BPL			LE7C9
				DEC			$FA
				RTS

LE7D5:			LDA			$F3
				JSR			LE800
				LDY			#$11

LE7DC:			LDA			($00),Y
				STA.wy		$00F2,Y
				INY
				CPY			#$14
				BNE			LE7DC
				LDA			#$00
				STA			$FA
				RTS

LE7EB:			LDA			$F3
				CLC
				ADC			#$01
				JSR			LE800
				LDY			#$11

LE7F5:			LDA			($00),Y
				STA.wy		$00C5,Y
				INY
				CPY			#$14
				BNE			LE7F5
				RTS

LE800:			LDX			#$00
				STX			$B1
				ASL
				ROL			$B1
				ASL
				ROL			$B1
				ASL
				ROL			$B1
				STA			$00
				CLC
				LDA			$B1
				ADC			#$02
				STA			$01
				RTS

				.byte		$00,$02

LE819:			LDA			#$00
				STA			$B1
				LDA			$0203
				SEC
				SBC			$0202
				STA			$F4
				CLC
				ADC			#$02
				ASL
				ROL			$B1
				ASL
				ROL			$B1
				ASL
				ROL			$B1
				STA			$00
				CLC
				LDA			$B1
				ADC			#$02
				STA			$01
				LDY			#$03

LE83D:			LDA			($00),Y
				STA.wy		$00FA,Y
				DEY
				BNE			LE83D
				RTS

LE846:			LDX			#$20
				JSR			PRINTSTRING

				.byte		$00, $00
				.word		$0098

				RTS

LE850:			LDX			#$20
				JSR			PRINTSTRING

				.byte		$00, $00
				.word		$00B0

				RTS

; ***** Clear MPEG "CACKC" flag - clears ACK flag in Interrupt Service Reg *****
LE85A:			LDY			#$00
				STY			DATAHIGH
				LDA			#$85		; MPEG ISR reg
				LDX			#$20		; Clear CACKC (Command ACKnowledge) flag
				JMP			WRITEMPEG

; ***** Wait for MPEG "CACK" (Command ACKnowledge) flag to go high, then RTS *****
LE866:			LDY			#$00
				STY			DATAHIGH
				LDA			#$84		; W9925 Processor status register
				STA			MPEG_ADD
				LDA			MPEG_DPR0
				AND			#$20		; Test bit 5 (Command ACKnowledge)
				BEQ			LE866		; Loop, until CACK bit = 1
				RTS

LE878:			LDY			#$00
				STY			DATAHIGH
				LDA			#$89		; ARG0 reg
				LDX			#$01		; Set to 0001h (I think?)
				JSR			WRITEMPEG
				LDA			#$8A		; ARG1 reg
				LDX			#$00		; Set to 0000h
				JSR			WRITEMPEG
				JSR			LE85A		; Clear CACKC flag in ISR
				
				LDA			#$88		; Decoder Command Register
				LDX			#$14		; Command b'10100' appears to be a reserved command ???
				JSR			WRITEMPEG
				JSR			LE866		; Wait for MPEG "CACK" (Command ACKnowledge) flag
				STY			DATAHIGH
				
				LDA			#$8B		; Set Address Index Reg lower byte to 8Bh
				STA			MPEG_ADD
				LDA			MPEG_DPR0	; Read 
				STA			$C9
				LDA			DATAHIGH
				STA			$CA
				STY			DATAHIGH
				LDA			#$85		; Select ISR reg
				LDX			#$20		; Clear CACKC flag
				JSR			WRITEMPEG
				JSR			LD42D		; Check for correct MPEG chip ID + soft reset + reset parser + enable sector counting + disable DMA / IRQs
				JSR			LD72F		; Set MPEG border colour 2
				JSR			LEA46
				LDA			#$00
				STA			$A5
				LDY			#$00
				STY			DATAHIGH

				LDA			$CA			; Load A with $CA
				BEQ			LE8D9		; Branch if $CA = 0 (select NTSC-to-PAL conversion)

				LDA			$DE			; Load A with $DE
				LSR						; Shift right
				BCS			LE8E8		; Branch if carry set (disable NTSC / PAL conversion if LSB of $DE = 1)

				LDA			#$89		; Else, Select ARG0 reg
				LDX			#$02		; PAL-to-NTSC conversion (drop every 6th line)
				JSR			WRITEMPEG
				JMP			LE8EF		; Continue (write RESizing / conversion command)

LE8D9:			LDA			$DE
				LSR
				BCC			LE8E8
				LDA			#$89		; Select ARG0 reg
				LDX			#$01		; NTSC-to-PAL conversion (duplicate every 5th line)
				JSR			WRITEMPEG
				JMP			LE8EF

LE8E8:			LDA			#$89		; Select ARG0 reg
				LDX			#$00		; Disable conversion / RESizing
				JSR			WRITEMPEG

LE8EF:			LDA			#$88
				LDX			#$1B		; RESizing command (NTSC / PAL conversion based on above ARG0 setting)
				JMP			WRITEMPEG

LE8F6:			JSR			READKEYPAD
				LDA			$C8
				BNE			LE91A
				LDY			#$00
				STY			DATAHIGH
				LDA			#$84		; W9925 Proc. status register
				STA			MPEG_ADD
				LDA			MPEG_DPR0
				AND			#$04		; Check "I-frame detected" flag.
				BEQ			LE91A		; Branch to LE91A if flag is NOT set
				LDA			#$FF		; Continue here if I-frame is detected.
				STA			$DB
				JSR			LD437		; Set MPEG input bistream high / low byte order to swapped, then setup MPEG audio params??
				JSR			LE878
				DEC			$C8

LE91A:			JSR			LD47C		; Update "Mono" / "Stereo" / "By Pass" message
				JSR			LD497
				LDA			$D9
				CMP			#$17
				BCS			LE931
				INC			$D9
				LDA			$D9
				CMP			#$17
				BCC			LE931
				JSR			LE846

LE931:			LDA			$8D
				BEQ			LE981
				TAY
				AND			#$08
				BEQ			LE982
				JSR			LE846
				DEC			$F3
				LDA			$F3
				CMP			#$FF
				BNE			LE94C
				LDA			$F4
				SEC
				SBC			#$01

LE94A:			STA			$F3

LE94C:			LDA			#$E7
				JSR			WRITEPPU
				JSR			LD42D		; Check for correct MPEG chip ID + soft reset + reset parser + enable sector counting + disable DMA / IRQs
				JSR			LD72F		; Set MPEG border colour 2
				JSR			LEA46
				JSR			LE7D5
				JSR			LE7EB
				JSR			LE6A6
				LDA			#$EF
				JSR			WRITEPPU
				JSR			LE71C
				LDA			#$00
				STA			DATAHIGH
				LDA			$F1
				AND			#$C0
				STA			$F1
				STA			$F2
				LDA			$FA
				BEQ			LE97F
				LDA			#$00
				RTS

LE97F:			LDA			#$FF

LE981:			RTS

LE982:			TYA
				AND			#$04
				BEQ			LE997
				JSR			LE846
				INC			$F3
				LDA			$F3
				CMP			$F4
				BCC			LE94C
				LDA			#$00
				JMP			LE94A

LE997:			TYA
				AND			#$01
				BEQ			LE9A9
				JSR			LE846
				JSR			LEA46
				LDA			#$00
				STA			$F1
				STA			$F2
				RTS

LE9A9:			TYA
				AND			#$10
				BEQ			LEA24
				JSR			LE846
				LDA			#$00
				STA			DATAHIGH
				STA			MPEG_DARH
				LDA			#$6D
				STA			MPEG_DARL
				LDA			$8C
				BPL			LE9E6
				LDA			#$00
				STA			MPEG_DDPR
				LDA			#$00
				STA			$D9
				JSR			PRINTSTRING

				.byte		$00, $00
				.word		LEA64			; "Slow   "

				LDA			#$40
				STA			$DA
				LDA			#$89
				LDX			#$02
				JSR			WRITEMPEG
				JSR			LD509
				LDA			#$88
				LDX			#$04
				BNE			LEA1A

LE9E6:			LDA			$DE
				AND			#$01
				CLC
				ADC			#$01
				STA			MPEG_DDPR
				LDA			$DA
				AND			#$40
				BEQ			LE9FF
LE9F6:			JSR			LE48F
				JSR			LD519
				JMP			LEA16

LE9FF:			LDA			$F1
				AND			#$C0
				CMP			#$80
				BNE			LE9F6
				LDA			#$80
				STA			$DA
				LDA			#$00
				STA			$D9
				JSR			PRINTSTRING

				.byte		$00, $00
				.word		LD2C6				; "Pause"

LEA16:			LDA			#$88
				LDX			#$01

LEA1A:			JSR			WRITEMPEG
				LDA			#$80
				STA			$F1
				LDA			#$00
				RTS

LEA24:			TYA
				AND			#$20
				BEQ			LEA31
				JSR			LE846
				DEC			$FA
				LDA			#$00
				RTS

LEA31:			TYA
				AND			#$02
				BNE			LEA37
				RTS

LEA37:			LDA			#$40
				STA			$F1
				JSR			LE846
				JSR			LEA46
				LDA			#$00
				STA			$F2
				RTS

LEA46:			LDA			#$00
				STA			DATAHIGH
				STA			MPEG_DARH
				LDA			#$6D
				STA			MPEG_DARL
				LDA			$DE
				AND			#$01
				CLC
				ADC			#$01
				STA			MPEG_DDPR
				LDA			#$88		; Select MPEG Decoder Command register
				LDX			#$01		; Send NORMAL PLAY Forward command
				JMP			WRITEMPEG

LEA64:			.byte		"Slow   "
				.byte		$FF

LEA6C:			LDA			MPEG_DDPR
				DEX
				BNE			LEA6C
				RTS

LEA73:			LDA			MPEG_DDPR
				STA			$2007
				DEX
				BNE			LEA73
				RTS

LEA7D:			LDY			#$00
				STY			DATAHIGH
				STY			MPEG_DARH
				LDA			#$10
				STA			DATAHIGH
				STY			MPEG_DARL
				RTS

LEA8E:			LDA			#$41
				STA			$FF
				JSR			LEA7D
				JSR			LD89D			; Zero addresses $0100 to $010B
				LDA			#$43
				STA			$0100
				LDA			#$01
				STA			$0106
				LDA			#$03
				STA			$0107
				STA			$F6
				LDA			#$24
				STA			$0108
				STA			$F5
				JSR			LDA1D
				JSR			LEA7D
				LDA			#$00
				STA			$00
				LDA			MPEG_DDPR
				LDA			MPEG_DDPR
				LDA			DATAHIGH
				SEC
				SBC			#$01
				ASL
				ROL			$00
				ASL
				ROL			$00
				STA			$01
				LDY			#$00
				STY			DATAHIGH
				STY			MPEG_DARH
				CLC
				LDA			#$10
				ADC			$00
				STA			DATAHIGH
				LDA			$01
				ADC			#$04
				STA			MPEG_DARL
				JSR			LD89D			; Zero addresses $0100 to $010B
				LDA			MPEG_DDPR
				STA			$0102
				LDA			DATAHIGH
				STA			$0103
				LDA			MPEG_DDPR
				STA			$0104
				LDA			DATAHIGH
				CLC
				ADC			#$10
				STA			$0105
				LDY			#$02

LEB05:			LDA			#$00
				ADC			$0102,Y
				STA			$0102,Y
				DEY
				BPL			LEB05
				LDA			#$10
				STA			$0109

LEB15:			LDA			#$BE
				STA			$0100
				LDA			#$01
				STA			$0108
				JSR			LEA7D
				JSR			LE0FC
				JSR			LEA7D

LEB28:			LDA			MPEG_DDPR
				CMP			LEB72,Y
				BNE			LEBA5			; Display "CD VOLUMN DESCRIPTER ERR", Jump to L9A1D to display "Stop"
				INY
				LDA			DATAHIGH
				CMP			LEB72,Y
				BNE			LEBA5
				INY
				CPY			#$08
				BNE			LEB28
				RTS

LEB3F:			JSR			LD89D			; Zero addresses $0100 to $010B
				LDA			#$10
				STA			$0105
				STA			$0109
				LDA			#$41
				STA			$FF
				JSR			LEB15
				JSR			LEA7D
				LDX			#$04
				JSR			LEA6C
				LDY			#$00

LEB5B:			LDA			MPEG_DDPR
				CMP			LEB93,Y
				BNE			LEB71
				INY
				LDA			DATAHIGH
				CMP			LEB93,Y
				BNE			LEB71
				INY
				CPY			#$12
				BNE			LEB5B

LEB71:			RTS

LEB72:			.byte		$01

LEB73:			.byte		"CD001"
				.byte		$01,$00

LEB7A:			.byte		"CD VOLUMN DESCRIPTER ERR"
				.byte		$FF

LEB93:			.byte		"CD-RTOS CD-BRIDGE "

LEBA5:			JSR			PRINTSTRING

				.byte		$00, $07
				.word		LEB7A			; "CD VOLUMN DESCRIPTER ERR"

				JMP			L9A1D		; Jump to L9A1D to display "Stop"


LEBAF:			JSR			LEA8E
				JSR			LEA7D		; Set MPEG DRAM address to 0x00001000 ??
				LDX			#$51
				JSR			LEA6C		; Read from MPEG DRAM Data port 0x51 times ??
				LDY			#$00		; Start Y at 0x00

LEBBC:			LDA			MPEG_DDPR	; Read low byte from MPEG DRAM
				STA			$0102,Y		; Store into CPU SRAM?
				INY
				LDA			DATAHIGH	; Read high byte from MPEG DRAM
				STA			$0102,Y		; Store into CPU SRAM?
				INY
				CPY			#$04
				BNE			LEBBC		; Loop, until 4 words have been transferred
				LDA			MPEG_DDPR	; Read low byte from MPEG DRAM
				STA			$B1			; Store at $B1
				LDA			DATAHIGH	; Read high byte from MPEG DRAM
				CMP			#$08		; Compare with 0x08
				BCC			LEBE3
				BNE			LEBE0
				LDA			$B1
				BEQ			LEBE3

LEBE0:			INC			$0108

LEBE3:			JSR			LEA7D
				JSR			LE0FC
				LDA			#$00
				STA			$A8
				STA			$00
				LDA			#$10
				STA			$01
				LDA			$2002
				LDA			#$20
				STA			$2006
				LDA			#$00
				STA			$2006

LEC00:			LDA			#$00
				STA			$CD
				LDY			#$00		; Put 0x00 into Y
				STY			DATAHIGH	; Store into DATAHIGH
				STY			MPEG_DARH		; Store into MPEG_DARH (MPEG DRAM ADDRESS 1)
				LDA			$01			; Put 0x01 into A
				STA			DATAHIGH	; Store into DATAHIGH
				LDA			$00			; Put 0c00 into A
				STA			MPEG_DARL		; Store into MPEG_DARL (MPEG DRAM ADDRESS 0)
				LDA			MPEG_DDPR		; Read byte from MPEG DRAM
				STA			$0A			; Store it at $0A
				BNE			LEC20		; Branch to LEC20 if NOT zero
				JMP			LED30

LEC20:			CMP			#$22
				BCS			LEC27
				JMP			LED2A

LEC27:			LDA			MPEG_DDPR
				STA			$0569
				LDA			DATAHIGH
				STA			$056A
				LDA			MPEG_DDPR
				STA			$056B
				LDX			#$02
				JSR			LEA6C
				LDA			MPEG_DDPR
				ORA			DATAHIGH
				BEQ			LEC49
				JMP			LED2A

LEC49:			LDA			$8C
				AND			#$40
				BEQ			LEC5D
				LDA			MPEG_DDPR
				CMP			#$04
				BNE			LEC60
				LDA			#$FF
				STA			$CD
				JMP			LEC83

LEC5D:			LDA			MPEG_DDPR

LEC60:			STA			$B1
				AND			#$03
				BNE			LEC71
				LDA			$B1
				ASL
				LDA			DATAHIGH
				ROL
				CMP			#$05
				BCC			LEC74

LEC71:			JMP			LED2A

LEC74:			LDA			DATAHIGH
				LSR
				ROR			$B1
				LSR
				ROR			$B1
				LDA			$B1
				CMP			#$81
				BCS			LEC71

LEC83:			STA			$0568
				LDX			#$06
				JSR			LEA6C
				LDA			DATAHIGH
				CMP			#$00
				BEQ			LEC95
				JMP			LED2A

LEC95:			LDX			#$04
				JSR			LEA6C
				SEC
				SBC			#$01
				STA			$B1
				LSR			$B1
				PHP
				LDY			DATAHIGH
				STY			$056C
				LDY			#$00
				LDX			#$01
				CMP			#$0B
				BCC			LECD9

LECB0:			LDA			MPEG_DDPR
				CMP			#$3B
				BEQ			LED07
				STA			$056C,X
				INX
				LDA			DATAHIGH
				CMP			#$3B
				BEQ			LED07
				STA			$056C,X
				INX
				INY
				CPY			#$05
				BNE			LECB0
				LDA			MPEG_DDPR
				CMP			#$3B
				BEQ			LED07
				STA			$056C,X
				PLA
				JMP			LED12

LECD9:			LDA			$B1
				BEQ			LED07

LECDD:			LDA			MPEG_DDPR
				CMP			#$3B
				BEQ			LED07
				STA			$056C,X
				INX
				LDA			DATAHIGH
				CMP			#$3B
				BEQ			LED07
				STA			$056C,X
				INX
				INY
				CPY			$B1
				BNE			LECDD
				PLP
				PHP
				BCC			LED07
				LDA			MPEG_DDPR
				CMP			#$3B
				BEQ			LED07
				STA			$056C,X
				INX

LED07:			PLA
				LDA			#$20

LED0A:			STA			$056C,X
				INX
				CPX			#$0C
				BNE			LED0A

LED12:			LDX			#$00

LED14:			LDA			$0568,X
				STA			$2007
				INX
				CPX			#$10
				BNE			LED14
				JSR			LEDCF
				INC			$A8
				LDA			$A8
				CMP			#$64
				BCS			LED4C

LED2A:			JSR			LED4D
				JMP			LEC00

LED30:			LDA			#$00
				STA			$CD
				LDA			$0108
				CMP			#$01
				BEQ			LED4C
				LDA			$01
				CMP			#$12
				BCS			LED4C
				LDA			#$12
				STA			$01
				LDA			#$00
				STA			$00
				JMP			LEC00

LED4C:			RTS

LED4D:			LSR			$0A
				CLC
				LDA			$00
				ADC			$0A
				STA			$00
				LDA			$01
				ADC			#$00
				STA			$01
				RTS

LED5D:			.byte		"V64_VER"

LED64:			.byte		"It is a : "
				.byte		$FF

LED6F:			.byte		"upgrade file in CD,pressMENU to confirm upgrade your V64 control program or press STOP to quit."
				.byte		$FF

LEDCF:			LDA			$CD
				BNE			LEDD4
				RTS

LEDD4:			LDA			#$00
				STA			$E0
				LDA			$A8
				JSR			LEE93
				LDX			#$04
				JSR			LEE8C
				LDY			#$00

LEDE4:			LDA			$2007
				STA			$0578,Y
				CMP			LED5D,Y
				BNE			LEE01
				INY
				CPY			#$07
				BNE			LEDE4

LEDF4:			LDA			$2007
				STA			$0578,Y
				INY
				CPY			#$0C
				BNE			LEDF4
				BEQ			LEE06

LEE01:			PLA
				PLA
				JMP			LED2A

LEE06:			LDA			#$FF
				STA			$0584
				JSR			PRINTSTRING

				.byte		$01, $01
				.word		LED64			; Display "It is a : "

				JSR			PRINTSTRING

				.byte		$00, $80
				.word		$0578

				JSR			PRINTSTRING

				.byte		$00, $02
				.word		LED6F			; Display "upgrade file in CD,pressMENU to confirm upgrade 
									;               your V64 control program or press STOP to quit."

LEE20:			JSR			LD2AB
				LDA			$8D
				AND			#$80
				BNE			LEE35
				LDA			$8D
				AND			#$20
				BEQ			LEE20
				JSR			BLANKPPU
				JMP			L9A1D		; Display "Stop"

LEE35:			LDA			#$00
				STA			$E0
				LDA			$A8
				JSR			LEE93
				LDA			$2007
				STA			$18
				JSR			LD89D			; Zero addresses $0100 to $010B
				LDY			#$02

LEE48:			LDA			$2007
				STA			$0103,Y
				DEY
				BPL			LEE48
				LDA			#$BE
				STA			$0100
				LDA			#$80
				STA			$0108
				LDA			#$10
				STA			$0109
				JSR			PRINTSTRING

				.byte		$04, $07
				.word		L90D1			; "Loading..."

				LDY			#$00
				STY			DATAHIGH
				LDA			#$01
				STA			MPEG_DARH
				STY			MPEG_DARL
				LDA			#$41
				STA			$FF
				JSR			LE0FC
				JMP			L9AFF

LEE7E:			.byte		$00,$14,$28,$3C,$50
LEE83:			.byte		"<PAGE  >"
				.byte		$FF

LEE8C:			LDA			$2007
				DEX
				BNE			LEE8C
				RTS

LEE93:			LDY			#$00
				STY			$B1
				LDY			$E0
				CLC
				ADC			LEE7E,Y
				ASL
				ROL			$B1
				ASL
				ROL			$B1
				ASL
				ROL			$B1
				ASL
				ROL			$B1
				TAX
				LDA			$B1
				ADC			#$20
				BIT			$2002
				STA			$2006
				STX			$2006
				LDA			$2007
				RTS

;
;	Print file names to screen, to update page? (I think, OzOnE)
;
LEEBB:			JSR			BLANKPPU
				LDA			#$00
				JSR			LEE93

LEEC3:			LDX			#$04			; Put 0x04 into X
				JSR			LEE8C			; Read X (4) times from $2007, return (skip some chars?)
				LDA			#$20			; Load A with 0x20 (" " SPACE, in ASCII)
				JSR			PRINTASCII		; Print " " (SPACE) to screen
				LDX			#$00			; Start X at 0x00

LEECF:			LDA			$2007			; Read byte from $2007
				JSR			PRINTASCII		; Print char to screen
				INX
				CPX			#$0B
				BNE			LEECF			; Loop until 0x0b (11) chars have been printed
				LDX			#$01			; Put 0x01 into X
				JSR			LEE8C			; Read X (one) time from $2007, return (skip some chars?)
				DEC			$14				; Decrement $14 (number of file names to print?
				BNE			LEEC3
				LDA			$A8
				CMP			#$15
				BCC			LEEFF
				JSR			PRINTSTRING

				.byte		$10, $0A
				.word		LEE83				; "<PAGE  >"

				LDA			$E0
				CLC
				ADC			#$01
				ORA			#$30
				TAX
				JSR			PRINTSTRING

				.byte		$16, $0A
				.word		$0081

LEEFF:			RTS

LEF00:			LDA			$E1
				LSR
				STA			$E2
				LDA			#$00
				BCC			LEF0B
				LDA			#$0C

LEF0B:			STA			$60
				RTS

LEF0E:			LDA			#$00
				STA			$E0

LEF12:			LDA			$A8
				LDX			$E0
				SEC
				SBC			LEE7E,X
				CMP			#$15
				BCC			LEF20
				LDA			#$14

LEF20:			STA			$14
				PHA
				JSR			LEEBB		; Print ($14) number of file names to screen??
				PLA
				STA			$14

LEF29:			LDA			#$00
				STA			$E1
				STA			$60
				STA			$E2
				LDA			$A8
				CMP			#$01
				BNE			LEF3A
				JMP			LEFE8

LEF3A:			JSR			LEF00
				JSR			LF051

LEF40:			JSR			LD091
				JSR			LD0DB
				LDA			$8D
				AND			#$3F
				STA			$B1
				LDA			$8C
				EOR			$8B
				AND			$8C
				AND			#$C0
				ORA			$B1
				STA			$8D
				BEQ			LEF40
				JSR			LF04A
				LDA			$8D
				AND			#$01
				BEQ			LEF6E
				INC			$E1
				LDA			$E1
				CMP			$14
				BCS			LEF29
				JMP			LEF3A

LEF6E:			LDA			$8D
				AND			#$02
				BEQ			LEF81
				LDA			$E1
				BNE			LEF7C

LEF78:			LDA			$14
				STA			$E1

LEF7C:			DEC			$E1
				JMP			LEF3A

LEF81:			LDA			$8D
				AND			#$04
				BEQ			LEF95
				CLC
				LDA			$E1
				ADC			#$02
				STA			$E1
				CMP			$14
				BCS			LEF29
				JMP			LEF3A

LEF95:			LDA			$8D
				AND			#$08
				BEQ			LEFA7
				LDA			$E1
				SEC
				SBC			#$02
				STA			$E1
				BMI			LEF78
				JMP			LEF3A

LEFA7:			LDA			$8D
				AND			#$40
				BEQ			LEFB9
				LDA			$E0
				BNE			LEFB4
				JMP			LEF3A

LEFB4:			DEC			$E0
				JMP			LEF12

LEFB9:			LDA			$8D
				AND			#$80
				BEQ			LEFD3
				LDY			$E0
				LDA			LEE7E,Y
				CLC
				ADC			#$14
				CMP			$A8
				BCC			LEFCE
				JMP			LEF0E

LEFCE:			INC			$E0
				JMP			LEF12

LEFD3:			LDA			$8D
				AND			#$10
				BNE			LEFE8
				LDA			$8D
				AND			#$20
				BEQ			LEFE5
				JSR			BLANKPPU
				JMP			L9A1D

LEFE5:			JMP			LEF3A

LEFE8:			JSR			LF051
				LDA			$E1
				JSR			LEE93
				JSR			LD89D			; Zero addresses $0100 to $010B
				LDA			$2007
				STA			$18
				LDY			#$02

LEFFA:			LDA			$2007
				STA			$0103,Y
				DEY
				BPL			LEFFA
				LDA			#$BE
				STA			$0100
				LDA			#$10
				STA			$0109
				JSR			BLANKPPU
				LDX			#$20
				JSR			PRINTSTRING

				.byte		$06, $03
				.word		$0081

				LDA			#$00
				STA			$0A

LF01D:			LDA			$2007
				JSR			PRINTASCII
				INC			$0A
				LDA			$0A
				CMP			#$0C
				BNE			LF01D
				RTS

LF02C:			LDA			$E2
				ORA			#$90
				JSR			WRITEPPU
				LDA			$60
				ORA			#$A0
				JMP			WRITEPPU

LF03A:			TXA
				AND			#$0F
				ORA			#$90
				JSR			WRITEPPU
				TYA
				AND			#$17
				ORA			#$A0
				JMP			WRITEPPU

LF04A:			JSR			LF02C
				LDA			#$7F
				BNE			LF056

LF051:			JSR			LF02C
				LDA			#$7D

LF056:			JMP			WRITEPPU

LF059:			.byte		"Send Out Control Program"
				.byte		$FF

LF072:			JSR			L9754
				JSR			PRINTSTRING

				.byte		$00, $07
				.word		LF059			; "Send Out Control Program"

				LDA			#$01
				JSR			L9067
				LDA			#$00
				JSR			L9067
				LDA			#$00
				JSR			L9067
				LDA			#$04
				JSR			L9067
				LDA			#$00
				JSR			L9067
				JSR			L9086
				LDY			#$00
				STY			$00
				STY			$BB

LF09E:			LDA			#$80
				STA			$01
				LDA			$BB
				AND			#$07
				STA			V64STATUS

LF0A9:			LDA			($00),Y
				TAX
				AND			#$0F
				ASL
				ASL
				ASL

LF0B1:			BIT			$C015
				BPL			LF0B1
				STA			$C016
				TXA
				AND			#$F0
				LSR
				ORA			#$80

LF0BF:			BIT			$C015
				BMI			LF0BF
				STA			$C016
				INY
				BNE			LF0A9
				INC			$01
				LDA			$01
				CMP			#$C0
				BNE			LF0A9
				INC			$BB
				LDA			$BB
				CMP			#$10
				BNE			LF09E
				LDA			#$01
				STA			$BB
				STA			V64STATUS
				JSR			L90DC
				JSR			PRINTSTRING

				.byte		$00, $0A
				.word		L9760		; Display "  Up Load Rom To PC OK  "

				JMP			L9A1D

LF0EE:			RTI				; Just do "Return from Interrupt"

				ds.b		$FE00 - ., 0

				JSR			LD000
				.word		L804C		; Backup CART to DRAM (64M)
				.byte		$01
				RTS

				JSR			LD000
				.word		L8050		; Backup CART to DRAM (128M)
				.byte		$01
				RTS

				JSR			LD000
				.word		L8054		; Backup CART to DRAM (192M)
				.byte		$01
				RTS

				JSR			LD000
				.word		L8058		; Backup CART to DRAM (256M)
				.byte		$01
				RTS

				JSR			LD000
				.word		L9210		; Upload CART via Parallel port (64M)
				.byte		$01
				RTS

				JSR			LD000
				.word		L9215		; Upload CART via Parallel port (128M)
				.byte		$01
				RTS

				JSR			LD000
				.word		L921A		; Upload CART via Parallel port (192M)
				.byte		$01
				RTS

				JSR			LD000
				.word		L921F		; Upload CART via Parallel port (256M)
				.byte		$01
				RTS

				JSR			LD000
				.word		L925F		; Upload DRAM via Parallel port (64M)
				.byte		$01
				RTS

				JSR			LD000
				.word		L9268		; Upload DRAM via Parallel port (128M)
				.byte		$01
				RTS

				JSR			LD000
				.word		L9271		; Upload DRAM via Parallel port (192M)
				.byte		$01
				RTS

				JSR			LD000
				.word		L927A		; Upload DRAM via Parallel port (256M)
				.byte		$01
				RTS
				
				ds.b		$FFCE - ., 0

				JMP			LF03A		; Set PPU cursor

				JMP			BLINKON

				JMP			BLINKOFF

				JMP			PRINTASCII

				JMP			LD287

				JMP			WRITEPPU

LFFE0:			JMP			PRINTSTRING

LFFE3:			JMP			BLANKPPU

				JMP			L90DC		; Prints a char

LFFE9:			JMP			LD0B5		; (Jumps to LD091) - Wait for PPU / MPEG VSYNC ??

				JMP			LD05E		; Print game name in DRAM on screen.

				JMP			L8147		; Print game name in CART on screen.

				ds.b		$FFF8 - ., 0

LFFF8:	.byte		$00
LFFF9:	.byte		$00
LFFFA:	.word		LF0EE		; FFFA / FFFB (NMI - Non-maskable Interrupt vector) - just does "Return from Interrupt"
LFFFC:	.word		LD33F		; FFFC / FFFD (RESET vector) - LD33F = start of main program code (well, in this V64 BIOS at least!)
LFFFE:	.word		LF0EE		; FFFE / FFFF (BREAK command vector) - just does "Return from Interrupt"
