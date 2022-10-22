pro SPECTRUM_input,abund_unity=abund_unity,notstandard=notstandard,photoexc=photoexc,interval=interval

; This program calculates the emissivities logG(logT,logNe) for all the ions in
; the CHIANTI database and prints out into one file to be used for BATS-R-US.
;
; Uses many SolarSoft functions and so recommended to run in SolarSoft.
;  
; This program needs in input the following things:
;
;               ion fraction file
;               abundance file
;               masterlist file
;               if /notstandard keyword is set, the CHIANTI directory
;
;           Keywords:
;               /notstandard : User chooses CHIANTI version to use
;               /photoexc    : Include photoexcitation in calculation
;               /abund_unity : All element abundances are set to 1.
;
; NOTES
;
; 1 - Output file name is set to SPECTRUM_chianti_tbl.dat
;     (containing ion.erg files data)
;
; 2 - It does not deal with dielectronic satellite lines
;
; 3 - Delta logT = as in ioneq file
;
; 4 - logG(logT,logNe) are calculated at temperatures defined in
;     ionequation file and densities 1e6-1e14 cm^3.
;
; VERSION
;
;  This is a modified version of crea_files.pro.  
;
; 06-Apr-2017 - Generating full tables into one file.
;               04-Nov-2021 - Generate table with ion. equ. fraction
;
; EXAMPLE
;
; 
;  
; CHIANTI version
  Result = CH_GET_VERSION()
  version=Result

; Set output file name
  NameFileOut='SPECTRUM_chianti_tbl.dat'

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
  
; Wavelengthrange
  wmin = 10
  wmax = 1700

  if keyword_set(interval) then begin
     read,'minimum wavelength [A]= ',wmin
     read,'minimum wavelength [A]= ',wmax
  endif

; List of elements and their atomic weight of periodic table
  Element=['h' ,'he','li','be','b' ,'c' ,'n' ,'o' ,'f' ,'ne','na','mg','al', $
           'si','p' ,'s' ,'cl','ar','k' ,'ca','sc','ti','v' ,'cr','mn','fe', $
           'co','ni','cu','zn']

  nElement=n_elements(Element)
  
; Set CHIANTI version (if keyword 'notstandard' is set)
  if keyword_set(notstandard) then begin 
     version=''
     versionpath=''
     read,'CHIANTI version (e.g. 6, 5.2.1, set 0 to current version) : '$
          ,version
     read,'CHIANTI version path : ',versionpath
     version=strcompress(versionpath,/remove_all) + $
             strcompress(version,/remove_all)
     use_chianti,version
  endif

; Provide photoexcitation parameters (if keyword 'photoexc' is set)
  if keyword_set(photoexc) then begin
     read,'rPhot = ',rPhot
     read,'RadTemp = ',RadTemp
  endif

; Input the masterlist file
  read_masterlist,!xuvtop+'/masterlist/masterlist.ions',masterlist

; Remove dielectronic list elements
  position=strpos(masterlist,'d')
  masterlist = masterlist[where(position LT 2)]
  nIon = n_elements(masterlist)

; Close lun=10 because we are going to use that for writing the output file
  close,10

; Write header into the output file
  openw,10,NameFileOut
  printf,10,'log(G(LogN,LogT)) table created with CHIANTI : ',version, $
         format='(a50)'
  printf,10,'ioneqfile = ',ioneqfile
  printf,10,'abundancefile = ',abundancefile
  printf,10,'#GRID',format='(a)'
  printf,10,LogNmin,LogNmax,dLogN
  printf,10,t_ioneq[0],t_ioneq[-1],t_ioneq[1]-t_ioneq[0]
  printf,10,'number of ions = ',nIon,format='(a17,i3)'
  printf,10,'ion ','lvl1 ','lvl2 ','wavelength ','logn ','logT ',$
         'logG(n,T) ','logIonEq ',format='(a5,a12,2a5,a11,2a5,a9,a9)'
  printf,10,'in units [A] [cm^-3] [K] [erg cm^3 s^-1]'
  printf,10,'#START',format='(a)'

; START main loop f calculation and writing
  for i=0L,nIon-1 do begin
     
; Select ion
     position=strpos(masterlist(i),'_')
     LocalElement=strmid(masterlist(i),0,position)
     zElem=where(Element eq LocalElement)+1
     zElem=zElem(0)
     Ion=masterlist(i)
     
     ; rename to match SWMF naming
     string=masterlist(i)
     position = strpos(string, '_')
     ionname=string.substring(0,position-1)
     ionnumb=string.substring(position+1)
     if zElem lt 9 then masterlist(i) = strtrim(string(ionname),2)+strtrim('_',2)+strtrim(string(ionnumb),2)
     if zElem ge 9 and ionnumb lt 10 then masterlist(i) = strtrim(string(ionname),2)+strtrim('_',2)+strtrim(string(0),2)+strtrim(string(ionnumb),2)
     if zElem ge 9 and ionnumb ge 10 then masterlist(i) = strtrim(string(ionname),2)+strtrim('_',2)+strtrim(string(ionnumb),2)
     zIon=float(strmid(Ion,position+1,strlen(Ion)))
     print,'doing ',masterlist(i),' which is the ',i+1,'th ion out of ',nIon

; Select ioneq
     ion_eq=ioneq(*,zElem-1,zIon-1)

; Calculate the emissivities
     data=emiss_calc(zElem,zIon,dens=LogN,temp=LogT,/quiet)
     if keyword_set(photoexc) then data=emiss_calc(zElem,zIon,dens=LogN,$
                                                   temp=LogT,rphot=rPhot,$
                                                   radtemp=RadTemp,/quiet)

; Select the element abundance
     ElementAbundance=abundance(zElem-1)

; Calculate log(G(T,Ne))
     LevelFrom=data.level1
     LevelTo=data.level2
     Flag=data.flag
     G=data.em
     Wavelength=data.lambda

     G[where(~finite(G),/null)] = 1.0d-99
     if total(where(G lt 1.0d-99)) ge 0 then G[where(G lt 1.0d-99)] = 1.0d-99
     
     G=G*ElementAbundance 
     for l=0,nLogT-1 do begin
        for j=0,nLogN-1 do begin
           G(l,j,*)=G(l,j,*)*ion_eq(l)/(10^LogN(j))
           position=abs(LogT(l)-t_ioneq)
        endfor
     endfor

     LogG=alog10(G)
     LogG[where(~finite(LogG),/null)] = -99
     
     LogIonEq=alog10(ion_eq)
     LogIonEq[where(~finite(LogIonEq),/null)] = -99

; Write into output file
     nTransition=n_elements(data)   
     help,nTransition
     for k=0L,nTransition-1 do begin
        if Wavelength(k) gt wmin and $
           Wavelength(k) lt wmax and $
           max(LogG(*,*,k)) gt -26 then begin 
           for j=0,nLogN-1 do begin
              for l=0,nLogT-1 do begin
                 if Flag(k) eq 0 then begin
                    printf,10,strtrim(masterlist(i),2)+STRING(9B)+ $
                           strtrim(string(LevelFrom(k)),2)+STRING(9B)+ $
                           strtrim(string(LevelTo(k)),2)+STRING(9B)+ $
                           strtrim(string(Wavelength(k),format='(f13.3)'),2)+STRING(9B)+ $
                           strtrim(string(LogN(j),format='(f13.1)'),2)+STRING(9B)+ $
                           strtrim(string(LogT(l),format='(f13.2)'),2)+STRING(9B)+ $
                           strtrim(string(LogG(l,j,k),format='(f13.3)'),2)+STRING(9B) + $
                           strtrim(string(LogIonEq(l),format='(f13.3)'),2)
                 endif
                 if Flag(k) lt 0 then begin
                    printf,10,strtrim(masterlist(i),2)+STRING(9B)+ $
                           strtrim(string(LevelFrom(k)),2)+STRING(9B)+ $
                           strtrim(string(LevelTo(k)),2)+STRING(9B)+ $
                           strtrim(string(Wavelength(k)*Flag(k),format='(f13.3)'),2)+STRING(9B)+ $
                           strtrim(string(LogN(j),format='(f13.1)'),2)+STRING(9B)+ $
                           strtrim(string(LogT(l),format='(f13.2)'),2)+STRING(9B)+ $
                           strtrim(string(LogG(l,j,k),format='(f13.3)'),2)+STRING(9B)+ $
                           strtrim(string(LogIonEq(l),format='(f13.3)'),2)
                 endif
              endfor
           endfor
        endif
     endfor
  endfor
  close,10
end
