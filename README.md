# Jaguar_MiSTer

Atari Jaguar FPGA core, written by Torlus.

Initial attempt to port to MiSTer.

It's using the later framework now, from a recent release of the GBA core.

Both analog video and HDMI are working, but the sync timings still need a few tweaks, so the image may be shifted slightly.


The proper Jag BIOS is now being used, but it's still failing the checksum atm, so I've added a patch for that to the menu.

The patch allows more games to be tested with the BIOS, and shows the spinning cube logo now.

But the checksum failure is likely stopping other games from working atm.

The BIOS should be copied into the Jaguar folder on the SD card, and named boot.rom.


I've added a region switch to the OSD menu, which is normally a pin on the motherboard that gets tied high or low.
(for NTSC or PAL, respectively.)

AFAIK, that signal gets read by the software (instead of affecting the hardware directly), but it may allow more games to run.

It might also change the video sync timings between NTSC and PAL if the BIOS reads the flag.
But, the master clock frequency is fixed at 26.59 MHz atm, so the sync timings won't be exactly right for both region modes.


The controls for player 1 are now hooked up to MiSTer.

I tried hooking up player 2 before as well, but for some reason it stopped the core booting?


Audio doesn't work on many games yet. And when it does, it sounds pretty terrible. lol

This is due to the way the Jag outputs audio using 2-bit (yes, 2-bit) PWM originally, so some work is needed there on the filtering and FIFO logic.


The core is now using DDR3 for main RAM, and SDRAM for cart loading. So SDRAM is *required*.

The latency of DDR is a bit too high for more games to work correctly.

Games like Atari Karts and Rayman exhibit lines across the screen, probably because it's unable to fill the line buffers quickly enough from DDR.

Ideally, the main RAM needs to be moved into SDRAM, where the latency is lower.

Cart ROMs could either kept in SDRAM or moved into DDR, since carts have slower access times anyway.

In hindsight, it should have been that way around from the start, but the Jag does 64-bit accesses to RAM,
and handling burst writes on SDRAM (with byte masking) will take a while to implement.


A handful of games now boot, but there will be some which don't boot at all, others that crash to a black screen (but often the game keeps running).
 
 
Cybermorph is one of the few games that generally runs quite well.
Not quite at full speed until the latency issue is solved, but it usually stays running for hours.


The older j68 CPU core has been replaced with FX68K, which is claimed to be cycle-accurate, and has shown to be very accurate so far.
(on cores like Genesis). 

I added a Turbo option for the 68000 to the MiSTer OSD, which does help speed up some things, like the intro to Flashback.

It does not speed up the main "Tom and Jerry" custom chips, though.


Some games seem to boot only after loading the ROM twice from the menu. I'm not sure what's causing that yet?

Sometimes the core might even need to be re-loaded from the SOF or RBF before a game will boot.
I don't think all of the internal registers in the core are being correctly reset yet.


In summary: still a fair bit of work to be done. lol

ElectronAsh.
