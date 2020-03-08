!CWT.f90: Continous Wavelet Transform
!
!	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!		by RH Landau, MJ Paez, and CC BORDEIANU 
!		Copyright Princeton University Press, Princeton, 2008.
!		Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!		MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!		Supported by the US National Science Foundation															 
!		
Program CWT
Implicit none
Integer ::i, j, k, o, t, n=120, m=100
Real *8 :: tau,dtau, omega, domega, x, WTreal, WTimag, max,omega1 = 1.0, omega2= 5.0,tau1= -81.92,WaveletReal,WaveletImag
Real *8 :: c(100,120),input(1024),PsiReal(16834),PsiImag(16834)
! c(100,120) -amplitude
! input(1024) -input signal
! omega1=1.0,omega2=5.0 -init & final freq 
! tau1=-81.92 - initial tau 
! PsiReal(1024) -real part
! PsiImag(1024) -imag part
! save amplitude in spectrogram.dat,and signal in input.dat
	 Open(6, FILE='CWTspectrum.dat', Status='Unknown')
	 Open(7, FILE='CWTinput.dat', Status='Unknown')
do t=1,1024
		if((t>=1).and.(t<=200)) then
		input(t)=5.*sin(6.28*t/100.)
		endif
		if((t>=200).and.(t<=800)) then
		input(t)= 5.*sin(6.28*t/100.)+10.*sin(2.*6.28*t/100.)
		endif
		if((t>=800).and.(t<=1024)) then
		input(t)= 2.5*sin(6.28*t/100.)+6.*sin(2.*6.28*t/100.)+ 10.*sin(3.*6.28*t/100.) 
		endif
write(7,*) input(t)				
end do		
 close(7)			 
! CWT:	Psi(t)=Psi((t-tau)/s) = Psi((t-tau)*omega) 
				! tau2 = tau1 + n*dtau			translation
				dtau=1.0/m
				domega = (omega2/omega1)** (1.0/m)			! scaling
				omega = omega1
				do i=1,m	 ! compute daughter wavelet function
					 tau=tau1
						do o=1,16834 
						! for signal files up to 2^13 = 8192 long
						 PsiReal(o) = WaveletReal( tau*omega )
						 PsiImag(o) = WaveletImag( tau*omega )
							tau = tau + dtau						! translation
						end do
						do j=1,n	 ! compute CWT 
							 
						 WTreal = 0.0
						 WTimag = 0.0
							do o=1,1024
							 WTreal = WTreal+input(o)*PsiReal(8192-(j*1024)/n+o)
							 WTimag = WTimag+input(o)*PsiImag(8192-(j*1024)/n+o) 
							end do
						c(i,j) = sqrt(WTreal*WTreal+WTimag*WTimag)
						end do
						omega = omega*domega	! scaling
					end do
	max=0.0001
	do i = 1,m 
		
				do j=1,n	!renormalize
				 if(c(i,j)>max) then
					max=c(i,j)
					endif								 
				write(6,*) c(i,j)/max 
							
				end do
				write(6,*) 
		 
	 end do
	 close(6)		
stop'Output in file CWTspectrum.dat' 
 end program CWT		
!	 Morlet wavelet
		function WaveletReal(t)
		implicit none		
		Real*8 ::sigma=4.0,t,WaveletReal,PI=3.14
		
		WaveletReal= cos(2.0*PI*t)*exp(-1.0*t*t/(2.0*sigma*sigma)) / (sigma*sqrt(2.0*PI))
		return
		end
		function WaveletImag( t )
		implicit none		
		Real*8 ::sigma=4.0,t,WaveletImag,PI=3.14
		
		WaveletImag= sin(2.0*PI*t)*exp(-1.0*t*t/(2.0*sigma*sigma)) / (sigma*sqrt(2.0*PI))
		return
		end

