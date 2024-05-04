pro PHOTOEXC_input,abund_unity=abund_unity,rmax=rmax,Nr=Nr,zElem=zElem,zIon=zIon,wvl=wvl

; This program calculates the emissivities logG(logT,logNe,Rs) for a
; specific ion's emission line from the CHIANTI database up to
; 6 Rs or on a grid  and prints out
; into one file to be used for BATS-R-US.
;
; Uses many SolarSoft functions and so recommended to run in SolarSoft.
;  
; This program needs in input the following things:
;
;               ion fraction file
;               abundance file
;               masterlist file
;               if /notstandard keyword is set, the CHIANTI directory
;               if /grid is set then file containing grid exponents (grid_awsom.dat)
;
;           Keywords:
;               /abund_unity : All element abundances are set to 1.
;
;                rmax : Maximum radius up to calculation runs.
;                Nr : number of points between 1 and rmax on a logarithmic grid
;                zElem : atomic number of element to be used
;                zIon : ion number (spectroscopic notation eg 10 for FeX)
;                wvl : wavelength in [A]
; NOTES
;
; 1 - Output file name is set to PHOTOEXC_chianti_tbl.dat
;     (containing ion.erg files data)
;
; 2 - It does not deal with dielectronic satellite lines
;
; 3 - Delta logT = as in ioneq file
;
; 4 - logG(logT,logNe, R) are calculated at temperatures defined in
;     ionequation file and densities 1e6-1e14 cm^3.
;
; VERSION
;
;  This is a modified version of SPECTRUM_input.pro.  
;
; 
; EXAMPLES
;

;     PHOTOEXC_input,zElem=26,zIon=10,wvl=6376.2900
;6376.2900    Fe X              3s2 3p5 2P3/2 - 3s2 3p5 2P1/2  

;     PHOTOEXC_input,zElem=26,zIon=11,wvl=7894.0308
;7894.0308    Fe XI             3s2 3p4 3P2 - 3s2 3p4 3P1

;     PHOTOEXC_input,zElem=26,zIon=13,wvl=10749.1055 
;10749.1055   Fe XIII           3s2 3p2 3P0 - 3s2 3p2 3P1

;     PHOTOEXC_input,zElem=26,zIon=13,wvl=10800.7695 
;10800.7695   Fe XIII           3s2 3p2 3P1 - 3s2 3p2 3P2

;     PHOTOEXC_input,zElem=26,zIon=14,wvl=5304.4771
;5304.4771    Fe XIV            3s2.3p 2P1/2 - 3s2.3p 2P3/2

;     PHOTOEXC_input,zElem=26,zIon=15,wvl=7062.1470
;7062.1470    Fe XV             3s 3p 3P1 - 3s 3p 3P2

;     PHOTOEXC_input,zElem=14,zIon=10,wvl=14304.7188
;14304.7188   Si X              2s2 2p 2P1/2 - 2s2 2p 2P3/2

;     PHOTOEXC_input,zElem=16,zIon=8,wvl=9916.7002
;9916.7002    S VIII            2s2.2p5 2P3/2 - 2s2.2p5 2P1/2

;     PHOTOEXC_input,zElem=16,zIon=12,wvl=7613.0732
;7613.0732    S XII             2s2 2p 2P1/2 - 2s2 2p 2P3/2

;     PHOTOEXC_input,zElem=28,zIon=15,wvl=6703.5361 
;6703.5361    Ni XV             3s2 3p2 3P0 - 3s2 3p2 3P1

;     PHOTOEXC_input,zElem=28,zIon=15,wvl=8026.3262 
;8026.3262    Ni XV             3s2 3p2 3P1 - 3s2 3p2 3P2

;     PHOTOEXC_input,zElem=12,zIon=8,wvl=30284.6758
;30284.6758   Mg VIII           2s2 2p 2P1/2 - 2s2 2p 2P3/2
  
;     PHOTOEXC_input,zElem=16,zIon=9,wvl=12523.4824
;12523.4824   S IX              2s2.2p4 3P2 - 2s2.2p4 3P1
  
;     PHOTOEXC_input,zElem=14,zIon=9,wvl=39293.0000
;39293.0000   Si IX             2s2 2p2 3P0 - 2s2 2p2 3P1

;     PHOTOEXC_input,zElem=26,zIon=9,wvl=22183.2480 
;22183.2480   Fe IX             3s2 3p5 3d 3F3 - 3s2 3p5 3d 3F2

;     PHOTOEXC_input,zElem=26,zIon=12,wvl=22063.0000
;22063.0000   Fe XII            3s2 3p3 2D3/2 - 3s2 3p3 2D5/2

;     PHOTOEXC_input,zElem=28,zIon=14,wvl=12792.6201
;12792.6201   Ni XIV            3s2.3p3 2D3/2 - 3s2.3p3 2D5/2

;     PHOTOEXC_input,zElem=20,zIon=15,wvl=5695.0850  
;5695.0850    Ca XV             2s2 2p2 3P0 - 2s2 2p2 3P1

;     PHOTOEXC_input,zElem=28,zIon=11,wvl=24307.2441  
;24307.2441   Ni XI            3s2 3p5 3d 3F4 - 3s2 3p5 3d 3F3

;     PHOTOEXC_input,zElem=20,zIon=13,wvl=22609.0000  
;22609.0000   Ca XIII          2s2 2p4 3P1 - 2s2 2p4 3P0

;     PHOTOEXC_input,zElem=14,zIon=7,wvl=24826.0000  
;24826.0000   Si VII            2s2 2p4 3P2 - 2s2 2p4 3P1

;     PHOTOEXC_input,zElem=14,zIon=9,wvl=25846.0000   
;25846.0000   Si IX             2s2 2p2 3P1 - 2s2 2p2 3P2

;     PHOTOEXC_input,zElem=26,zIon=9,wvl=28562.4512  
;28562.4512   Fe IX            3s2 3p5 3d 3F4 - 3s2 3p5 3d 3F3

;     PHOTOEXC_input,zElem=12,zIon=8,wvl=30284.6758  
;30284.6758   Mg VIII          2s2 2p 2P1/2 - 2s2 2p 2P3/2

;     PHOTOEXC_input,zElem=16,zIon=9,wvl=37552.0000  
;37552.0000   S IX             2s2 2p4 3P1 - 2s2 2p4 3P0

;     PHOTOEXC_input,zElem=14,zIon=9,wvl=39293.0000 
;39293.0000   Si IX            2s2 2p2 3P0 - 2s2 2p2 3P1

;     PHOTOEXC_input,zElem=18,zIon=13,wvl= 10149.0000 
;10149.0000  Ar XIII             2s2 2p2 3P0 - 2s2 2p2 3P1

;     PHOTOEXC_input,zElem=16,zIon=13,wvl= 10301.0000
;10301.0000   3.19e-03 S XIII        6.40   2s 2p 3P1 - 2s 2p 3P2

;     PHOTOEXC_input,zElem=16,zIon=11,wvl= 13927.0000    
;13927.0000   1.91e-02 S XI          6.25   2s2 2p2 3P1 - 2s2 2p2 3P2  

;     PHOTOEXC_input,zElem=20,zIon=12,wvl=3328.4519   
;3328.4519    2.59e-02 Ca XII        -      2s2.2p5 2P3/2 - 2s2.2p5 2P1/2
  
;     PHOTOEXC_input,zElem=26,zIon=13,wvl=3388.9109     
;3388.9109    2.56e-01 Fe XIII       -      3s2 3p2 3P2 - 3s2 3p2 1D2

;     PHOTOEXC_input,zElem=26,zIon=10,wvl=3455.2710      
;3455.2710    5.18e-01 Fe X          -      3s2 3p4 3d 4D7/2 - 3s2 3p4 3d 4F9/2

;     PHOTOEXC_input,zElem=28,zIon=16,wvl=3602.2539
;3602.2539    6.11e-08 Ni XVI        -      3s2.3p 2P1/2 - 3s2.3p 2P3/2

;     PHOTOEXC_input,zElem=20,zIon=13,wvl=4087.4719     
;4087.4719    1.59e-04 Ca XIII       -      2s2 2p4 3P2 - 2s2 2p4 3P1

;     PHOTOEXC_input,zElem=28,zIon=12,wvl=4232.0879      
;4232.0879    2.02e-01 Ni XII        -      3s2 3p5 2P3/2 - 3s2 3p5 2P1/2

;     PHOTOEXC_input,zElem=18,zIon=14,wvl=4413.8032     
;4413.8032    4.38e-09 Ar XIV        -      2s2 2p 2P1/2 - 2s2 2p 2P3/2

;     PHOTOEXC_input,zElem=28,zIon=13,wvl=5117.2358      
;5117.2358    6.68e-03 Ni XIII       -      3s2.3p4 3P2 - 3s2.3p4 3P1
  
;     PHOTOEXC_input,zElem=20,zIon=15,wvl=5445.4370                                      
;5445.4370    5.01e-11 Ca XV         -      2s2 2p2 3P1 - 2s2 2p2 3P2

;     PHOTOEXC_input,zElem=16,zIon=11,wvl=19201.0000      
;19201.0000   9.09e-04 S XI          -      2s2 2p2 3P0 - 2s2 2p2 3P1
  
;     PHOTOEXC_input,zElem=18,zIon=11,wvl=6918.7871                                   
;6918.7871    8.02e-03 Ar XI         -      2s2 2p4 3P2 - 2s2 2p4 3P1

;     PHOTOEXC_input,zElem=18,zIon=10,wvl=5535.5488     
;5535.5488    1.19e-01 Ar X          -      2s2.2p5 2P3/2 - 2s2.2p5 2P1/2  
  
; CHIANTI version
  Result = CH_GET_VERSION()
  version=Result

; Set output file name
  NameFileOut='PHOTOEXC_'+strtrim(string(wvl,format='(f13.4)'),2)+'.dat'

; Input the ion fraction file
  ioneqfile = 'chianti_dblpre.ioneq'
  read_ioneq,ioneqfile,t_ioneq,ioneq,ref

; Input the abundance file
  abundancefile= !xuvtop+'/abundance/sun_coronal_2021_chianti.abund'
  if keyword_set(abund_unity) then abundance=fltarr(nElement)+1.0d0
  if not keyword_set(abund_unity) then read_abund,abundancefile,abundance,ref

; Density grid 
  LogN=[6,7,8,9,10,11,12,13,14]*1.0
  nLogN=n_elements(LogN)
  LogNmin=min(LogN)
  LogNmax=max(LogN)
  dLogN=(max(LogN)-min(LogN))/(nLogN-1.0)

; Temperature grid mathes one from chianti.ioneq
  LogT = t_ioneq
  nLogT = n_elements(t_ioneq)

; Provide photoexcitation grid
  r_max = 6.
  nLogR = 400
  if keyword_set(rmax) then r_max = rmax
  if keyword_set(Nr) then nLogR = Nr
  r_min = 1.001
  logr_min = alog10(r_min)
  logr_max = alog10(r_max)
  
  dLogr = (logr_max - logr_min)/nLogR
  logrPhot = findgen(nLogR,start=logr_min,increment=dLogr)

; Surface temperature for photoexcitation  
  RadTemp = 5770.

; Close lun=10 because we are going to use that for writing the output file
  close,10

; Write header into the output file
  openw,10,NameFileOut
  printf,10,'log(G(LogT,LogN,LogRs)) table in units [K] [cm^-3] [Rs] [erg cm^3 s^-1] []'
  printf,10,'0 0.0 3 3 2'
  printf,10,nLogT,nLogN,nLogR
  printf,10,strtrim(string(zElem,format='(i2)'),2)+' '+strtrim(string(zIon,format='(i2)'),2)+' '+strtrim(string(wvl,format='(f13.3)'),2)
  printf,10,'lgT lgN logR logG logIonEq zElem zIon Wvl'  
  
; Select ioneq
  ion_eq=ioneq(*,zElem-1,zIon-1)

; Calculate the emissivities
  for i = 0L,n_elements(logrPhot)-1 do begin
     data=emiss_calc(zElem,zIon,dens=LogN,temp=LogT,$
                     rphot=10^logrPhot[i],radtemp=RadTemp,/quiet)

; Select the element abundance
     ElementAbundance=abundance(zElem-1)

; Select line
     iLine = where(data.lambda eq wvl)
     
; Calculate log(G(T,Ne))
     G=data.em
     G = G[*,*,iLine]

     G[where(~finite(G),/null)] = 1.0d-99
     if total(where(G lt 1.0d-99)) ge 0 then G[where(G lt 1.0d-99)] = 1.0d-99
     
     G=G*ElementAbundance 
     for l=0,nLogT-1 do begin
        for j=0,nLogN-1 do begin
           G(l,j)=G(l,j)*ion_eq(l)/(10^LogN(j))
           position=abs(LogT(l)-t_ioneq)
        endfor
     endfor

     LogG=alog10(G)
     LogG[where(~finite(LogG),/null)] = -99
     
     LogIonEq=alog10(ion_eq)
     LogIonEq[where(~finite(LogIonEq),/null)] = -99

     print,'writing file at R = ',10^logrPhot[i],' Rs'

; Write into output file
     for j=0,nLogN-1 do begin
        for l=0,nLogT-1 do begin
           printf,10,strtrim(string(LogT(l),format='(f13.2)'),2)+STRING(9B)+ $
                  strtrim(string(LogN(j),format='(f13.1)'),2)+STRING(9B)+ $
                  strtrim(string(logrPhot[i],format='(e14.5e3)'),2)+STRING(9B)+ $
                  strtrim(string(LogG(l,j),format='(f13.3)'),2)+STRING(9B) + $
                  strtrim(string(LogIonEq(l),format='(f13.3)'),2)
        endfor
     endfor
  endfor
  close,10
end
