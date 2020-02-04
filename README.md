# Jaguar_MiSTer

Atari Jaguar FPGA core, written by Torlus.

Initial attempt to port to MiSTer.

It's using the later framework now, from a recent release of the GBA core.

Both analog video and HDMI are working, but the sync timings still need a few tweaks, so the image may be shifted slightly.

(or might not display correctly on an analog monitor.)


The proper Jag BIOS is now being used, but it's still failing the cart checksum on most games atm, so I've added a patch for that to the menu.

A RESET (or new cart ROM load) is required after changing the Checksum patch option.

The patch allows more games to be tested with the BIOS, and shows the spinning cube logo now.

But the checksum failure is likely stopping other games from working atm.

The BIOS file (usually "jagboot.rom") should be renamed to boot.rom, then copied into the Jaguar folder on the SD card.


I've added a video mode switch to the OSD menu, which is normally a pin on the Jag motherboard that gets tied high or low.
(for NTSC or PAL mode.)

That signal gets read by the BIOS at start-up, so a RESET (or new cart ROM load) is required for the NTSC/PAL change to take effect.

It's possible that some games might detect the video mode, and refuse to boot if there is a region mismatch.

It does now change the video sync timings between NTSC and PAL, but the master clock frequency is fixed at 26.59 MHz atm,
so the sync timings might not be exactly per-spec for NTSC/PAL.


The controls for player 1 are now hooked up to MiSTer.

I tried hooking up player 2 before as well, but for some reason it stopped the core booting?


Audio doesn't work on many games yet. And when it does, it sounds pretty terrible. lol


The core is now using DDR3 for main RAM, and SDRAM for cart loading. So SDRAM is *required*.

The latency of DDR is a bit too high for more games to work correctly.

Games like Atari Karts and Rayman exhibit lines across the screen, probably because it's unable to fill the line buffers quickly enough from DDR.

Ideally, the main RAM needs to be moved into SDRAM, where the latency is lower.

Cart ROMs could either kept in SDRAM or moved into DDR, since carts have slower access times anyway.

In hindsight, it should have been that way around from the start, but the Jag does 64-bit accesses to RAM,
and handling burst writes on SDRAM (with byte masking) will take a while to implement.


A handful of games now boot, but there will be some which don't boot at all, some with glitches or no audio, and others that might crash to a black screen (but often the game keeps running).
 
 
Cybermorph is one of the few games that generally runs quite well.
Not quite at full speed until the RAM latency issue is solved, but it usually stays running for hours.


The older j68 CPU core has been replaced with FX68K, which is claimed to be cycle-accurate, and has shown to be very accurate so far.
(on cores like Genesis). 

I added a Turbo option for the 68000 to the MiSTer OSD, which does help speed up some things, like the intro to Flashback.

It does not speed up the main "Tom and Jerry" custom chips, though.



In summary: still a fair bit of work to be done. lol

ElectronAsh.
