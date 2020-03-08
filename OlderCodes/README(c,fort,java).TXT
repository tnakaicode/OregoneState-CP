README.txt for the CD containing electronic enhancements for the book:

	"A SURVEY OF COMPUTATIONAL PHYSICS"
	 by RH Landau, MJ Paez, and CC BORDEIANU
	 Copyright Princeton University Press, Princeton, 2008.

The electronic materials were developed with support from the US National
Science Foundation. Copyright for them are held by R Landau, Oregon State Univ,
2008; MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.

ANIMATIONS (Subdirectories)

2Dsoliton	 DoublePendulum		Fractals (incl. Pollock)	 Laplace		MapleWaveMovie
MD				 README_animate		TwoSlits									 Utilities	Waves

N.B. Not all files play on all media viewers, please see notes in README_animate.

APPLETS:

	The Chaotic Pendulum	4 Centers Chaotic Scatt							Planetary Orbits
	Waves on a String			Normal Mode Solitons								Cellular Automata
	Spline Interpolation	Relativistic Scattering							Lagrange Interpolation
	Young's 2 Slit Int		Starbrite (H-R Diag)								Feynman Path Integral
	HearData player				Photoelectric Effect								Sonifications
	Lissajous Figures			Wavepacket-Wavepacket Collisions		Heat Equation
	Coping with Unix			Wave Functions											Fractals
	JParabola							Jstring				README_applets.html		README_applets.txt

N.B. Open the file README_applets.html with a Web browser to view the applets.


CCODES:

	ABM.c							 Daub4_2.c					int_10d.c				 random.c
	accm2d.cpp				 Daub4Compress_2.c	integ.c					 rk45_rhl_2.c
	AdvecLax_2.c			 DaubMariana.c			integ.dat				 rk4.c
	AFileIO.c					 decay.c						invfour.c				 shms.cpp
	area.c						 diff.c							ising.c					 Shock_2.c
	armadillo.dat			 dla.c							laplaceAnal_2.c	 sierpin.c
	Beam_2.c					 eqheat.c						laplace.c				 soliton.c
	bessel.c					 eqstring.c					LaplaceSOR.c		 SplineAppl_2.c
	Bisection.c				 exp-bad.c					limit.c					 tree.c
	bound.c						 exp-good.c					lineq.c					 twodsol_2.c
	bugs.c						 fern.c							mariana.dat			 unim1d.cpp
	call.c						 film.c							MD1D_2.c				 unimot2d.cpp
	CellAut.c					 fit.c							numerov.c				 walk.c
	CmdLineArgsDemo.c	 fourier.c					over.c
	column.c					 gauss.c						pond.c
	CWT3_2.c					 HeatCNTridiag_2.c	qmc.c



FORTRAN77CODES:

	area.f			bessel.f		bound.f		 bugs.f		 complex.f	 decay.f
	diff.f			eqheat.f		eqstring.f exp-bad.f exp-good.f	 fit.f
	fourier.f		gauss.f			harmos.f	 integ.f	 integ.lst	 int_10d.f
	ising.f			lagrange.f	laplace.f	 limit.f	 over.f			 pond.f
	qmc.f				random.f		rk4.f			 scatt.f	 slit.f			 soliton.f
	spline.f		sqwell.f		twodsol.f	 walk.f


FORTRAN95CODES:

	AdvecLax.f95	fit.f95						 Newton_cd.f95	slit.f95
	BeamFlow.f95	harmos.f95				 Newton_fd.f95	soliton.f95
	bessel.f95		HeatCNTridiag.f95	 overflow.f95		Spline.f95
	CWT.f95				int10d.f95				 pond.f95				sqwell.f95
	DAUB4.f95			integ.f95					 qmc.f95				tune.f95
	decay.f95			Ising3D.f95				 random.f95			twodsol.f95
	diff.f95			lagrange.f95			 rk45.f95				twoplates.f95
	eqheat.f95		LaplaceSOR.f95		 rk4.f95				walk.f95
	eqstring.f95	limit.f95					 scatt.f95
	exp-bad.f95		MD.f95						 shock.f95


JAVACODES:

	ABM.java							EqHeat.java						 OOPPlanet.java
	Accm2D.java						EqString.java					 outdata.dat
	AdvecLax.java					FDTD.java							 OutofEight.java
	AFileIO.java					FFT.java							 Path.java
	Area.java							FileCatchThrow.java		 PredatorPrey.java
	AreaScanner.java			Fit.java							 ptolemy (dir)
	Ball.java							Fractals/							 ptplot5.6 (dir)
	Baton.java						FT.java								 QMCbouncer.java
	Beam.java							Gauss.java						 QMC.java
	Beats.java						HarmOscillator.java		 QuantumEigen.java
	Bessel.java						Harmos.java						 QuantumNumerov.java
	Bisection.java				HeatCNTridiag.java		 RandNum.java
	Bound.java						IntegGauss.java				 README.txt
	Bugs.java							Ising.java						 ReadStnd2.java
	CellAut.java					Jama (dir)						 ReadStnd.java
	CmdLineArgsDemo.java	JamaCP (dir)					 rk2.java
	CommandLineArgs.java	KomplexInterface.java	 rk45.java
	comp-info.dat					Komplex.java					 rk4.java
	ComplexDyn.java				KomplexTest.java			 Scale.java
	Complex.java					LaplaceFEM.java*			 Scatt.java
	CompressDaub.java			Laplace.java					 Shock.java
	CWTapplication.java		LaplaceLine.java			 Soliton.java
	CWT.java							LaplaceSOR.java				 SplineAppl.java
	CWTzd.java						LifeGame.java					 Trap.java
	dat2image.sh					Limits.java						 Tridiag.java
	Data/									LyapLog.java					 Tune4.java
	Daub4Compress2.java		mariana.pgm						 Tune.java
	Daub4.java						MD.java								 TwoDsol.java
	DaubMariana.java			Moon.java							 twoplates.java
	Decay.java						Newton_cd.java				 TwoPlotExample.java
	DFT.java							Newton_fd.java				 Walk.java
	DWT.java							Noise.java						 WangLandau.java
	EasyPtPlot.java				NoiseSincFilter.java	 WaveGuide.java
	EMcirc.java						Numerov.java					 WaveletSmall.java
	Entropy.java					OOPBeats.java


	Wavelet Codes Details:

	CWT.java							CWT of signal, outputs 3D gnuplot format
	CWTzd.java						alternate CWT of signal, outputs 3D gnuplot shown in text
	CWTapplication.java		CWT of image file, outputs B&W spectrogram image, uses
												frames and java.awt.image package
	Daub4.java						DWT, gnuplot output, Daub4 e6, Daub4 e10+Daub4 e58 wavelets
	Daub4Compress2.java		DWT of 1D signal with threshold to eliminate noise
	CompressDaub.java			DWT of 2D image (ph.dat) with threshold, outputs compressed
												image, uses frames and java.awt.image package
	DaubMariana.java			DWT of 2D image (Mariana.dat) with threshold, outputs		
												compressed image mariana.pgm for photo editor			
		 
							
	Fractals (dir):

		COLUMN.java			 Coastline.java		DLA.java
		FERN.java				 Film.java				SIERPIN.java		 TREE.java


	Data (dir):

		2Dsol.dat	 Efield.dat	 Grace.dat	 indata.dat		mariana.dat	 outdata.dat
		A.dat			 EH.dat			 H.dat			 Laplace.dat	mariana.pgm	 ph.dat
		E.dat			 Grace2.dat	 Hfield.dat	 laplR.dat		Name.dat		 ptPlotdat.plt


	JamaCP (dir):

		Jama (dir)						 JamaFit.java
		JamaEigen.java				 PspaceJamaEigen.java		JamaEigen2.java	 Newton_Jama.java


MPICODES:

	hello.c				 MPIdeadlock-fixed.c		MPIdeadlock.c		 MPImessage2.c
	MPImessage3.c	 MPIpi.c								MPIstring.c			 run_mpi.sh
	TuneMPI.c


OPENDX:

	DXdata (dir)				DXjava (dir)      README_DX.TXT

PVM:

	PVMCODES:

	bugs.f					 input						Makefile				 PVMbugSlave.f
	PVMbugsMaster.c	 PVMbugsMaster.f	PVMbugsSlave.c	 PVMcommunMaster.c
	PVMcommunSlave.c PVMmonteMaster.c PVMmonteSlave1.c PVMmonteSlave2.c
	PVMmonteSlave3.c PVMmonteSlave4.c

	PVM.pdf (Listing of our PVM tutorial)
