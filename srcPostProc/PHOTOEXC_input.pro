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
; EXAMPLE
;
;     PHOTOEXC_input,zElem=26,zIon=10,wvl=6376.290
;     PHOTOEXC_input,zElem=26,zIon=11,wvl=7894.0308
; 
;  
; CHIANTI version
  Result = CH_GET_VERSION()
  version=Result

; Set output file name
  NameFileOut='PHOTOEXC_'+strtrim(string(wvl,format='(f13.4)'),2)+'.dat'

; Input the ion fraction file
  ioneqfile = 'chianti_dblpre.ioneq'
  read_ioneq,ioneqfile,t_ioneq,ioneq,ref

; Input the abundance file
  abundancefile= !xuvtop+'/abundance/sun_coronal_1992_feldman.abund'
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
  printf,10,'logT logN logR logG logIonEq zElem zIon Wvl'  
  
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
