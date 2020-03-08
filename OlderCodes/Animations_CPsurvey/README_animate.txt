README_animate

--------------------------------------------------------------------------------
README file for Animations_ColorImages subdirectory on the CD containing electronic 
enhancements for the book:
														 "A SURVEY OF COMPUTATIONAL PHYSICS"
															by RH Landau, MJ Paez, and CC BORDEIANU
															Copyright Princeton University Press, Princeton, 2008.

The electronic materials were developed with support from the US National
Science Foundation. Copyright for them are held by RH Landau, Oregon State Univ,
2008; MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
--------------------------------------------------------------------------------
GENERAL NOTES:

Files ending in .mpg (mpeg), .avi, or .fli are movies (animations), each in a 
different format. The .mpg movies can be viewed by most of the popular movie players, 
although some players may not show the entire image.

---
For Windows and Macs, these include:

	Real Player		 Windows Media Player		QuickTime Player		VLC.

**We recommend VLC because it appears to accept the largest number of file types.**

All .avi files can be viewed with VLC, and most with the other players, although
laplace.avi runs only with VLC (however we give an mpg version that runs universally, 
but is of lower quality). The file slit.fli can be viewed with Quicktime, either 
directly or via a browser plugin, but not with the other players.

---

For Linux, in particular SUSE, useful movie players include:

	Totem Movie Player		Kaffeine		MPlayer		VLC		Konquerer (browser/file manager).
	
The .fli files can be played by MPlayer, but not others. The .avi files opened with 
all players.

----------

Files ending in .gif are animated gifs (generally .gif files are not animated, but 
those on the CD are).	 Animated gifs can be viewed with media players or browsers. 
They are generally of lower quality than avi or mpeg movies, but are smaller.

The .pdf files are still images. They can be opened with Acrobat enabled browsers. 
The .bmp files are bitmap images and can be viewed with picture viewers such as 
IrfanView, PictureViewer, or Konqueror (Linux).

======================================================================================
2Dsoliton (dir)

2dsol.mpg				tdsmall.mpg		 twodsol1.pdf		 twodsol10.pdf		 twodsol2.pdf		 
	twodsol3.pdf		twodsol4.pdf		twodsol5.pdf		twodsol6.pdf			twodsol7.pdf
	twodsol8.pdf		twodsol8.pdf		twodsol9.pdf		TwodsolDX (dir)

	2-D soliton (Sine-Gordon equation) color animation (tdsmall.mpg) and individual 
	frames of animation (.pdf files). The subdirectory TwodsolDX contains the components 
	for an OpenDX animation (you need OpenDX running to view).
	
	TwodsolDX (dir in 2Dsoliton) [Used in the creation of an animation with OpenDX]
	
		twodsol.net					 twodsol001.general		twodsol002.general	 twodsol003.general		
		twodsol004.general	 twodsol005.general		twodsol006.general	 twodsol007.general		
		twodsol008.general	 twodsol009.general		twodsol010.general	 twodsol10.dat
		twodsol100.dat			 twodsol20.dat				twodsol30.dat				 twodsol40.dat				
		twodsol50.dat				 twodsol60.dat				twodsol70.dat				 twodsol80.dat
		twodsol90.dat
--------------------------------------------------------------------------------------

DoublePendulum (dir)

	 DoublePend.mpg		 mode1.mpg	 mode2.mpg	 neat.mpg		 nomode.mpg	 twopendulums.mpg

	A number of .mpg movies of the different modes of a realistic double pendulum and 
	of the motion of two realistic pendulums (twopendulums.mpg).
	
--------------------------------------------------------------------------------------

Fractals (dir)

	Fractal Growth.mpg		Islands.pov (Pov-Ray source code)		Pollock.pdf

	An .mpg movie of fractal growth, a PovRay source file used to create
	fractal images, and a .pdf picture of Pollock #8, useful for fractal analysis.
	
--------------------------------------------------------------------------------------
	
Laplace	 (dir)
		
	laplace.avi		laplace.mpg
	
	An .avi and an .mpg movie created with OpenDX of the electric potential of a
	finite size capacitor with a variable voltage.
	
--------------------------------------------------------------------------------------
				
MD (dir)

	2-D animations (.avi and .gif) of molecular dynamic simulations. The numbers 25 
	and 100 on the files indicate the number of particles in the simulations.
		 
		 md2d_100.avi			MD2d_25particles.avi			 MDanimate2D4particle.gif
--------------------------------------------------------------------------------------

Waves	 (dir)

		AnimationGaussianWave.gif		CatFriction.gif							CatWave.gif		
		MapleWaveMovie (dir)				ShockAnnimation.gif					SolitonAnimation.gif				SolitonCrossAnimation.gif
		TwoSlitDiffraction (dir)		Utilities (dir)
		
	Animated .gif files:	
		AnimationGaussianWave; 2-D Gaussian wave packet on a membrane
		CatFriction, CatWave: 1-D waves on a catenary with & without friction
		ShockAnimation: 1-D shock wave formation
		SolitonAnimation; 1-D soliton formation
		SolitonCrossAnimation: two solitons crossing
								
	TwoSlitDiffraction (dir): subdirectory of Waves containing an .mpg and a .fli 
	visualization of quantum wave packets passing through two slits: 
	 
	 2slits.mpg		AnimationDiffraction.gif	 Slit.fli		SlitDX (dir)
	 
	 AnimationDiffraction shows one-slit diffraction and reflection off surrounding 
	 box.) The subdirectory "SlitDX" contains the files used to create an OpenDX 
	 animation (needs OpenDX running to view).
						
	Utilities (dir): a subdirectory of Waves containing a script and a data file used 
	to create .gif animation of quantum wave packets with gnuplot.
	
		MakeGifs.script		samp_colormap
	
	MapleWaveMovie (dir): Subdirectory of Waves containing animations and visualizations 
	(.bmp) of waves on a catenary.	The Java, C, and Maple files used to create the
	animations are also given:
		
		CatMoview.gif													eqstring.c								 function.dat								function2.dat			
		string.java														string2.java							 wave.mws										wave_1_u(x).gif		
		wave_1_y(x).gif												wave_1_y(x)_3d.bmp				 wave_1_y(x)_3d.gif					wave_2_1_y(x).gif							wave_2_2_y(x).gif
		wave_2_y(x)_3d_normal.bmp							wave_2_y(x)_3d_normal.gif			
		wave_2_y(x)_3d_smallamp.bmp						wave_2_y(x)_3d_smallamp.gif		
		wave_friction_1_y(x).gif							wave_friction_2_y(x).gif

