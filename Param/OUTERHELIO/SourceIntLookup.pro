;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function cx_int, g

; cross-section from Lindsay & Stebbings (2005)
; for H-H+ collision  up to 250 keV

common constants, mp, kb, cgs, kev_J

  a1=double(4.15)
  a2=double(0.531)
  a3=double(67.3)

  e=double(5d-1*mp*g*g/kev_J) ; convert relative vel. to energy

  sig1=double(a1-a2*alog(e))
  sig1=sig1*sig1
  sig2=double(1-exp(-a3/e))
  sig2=sig2*sig2*sig2*sig2*sqrt(sig2)
  sigma=sig1*sig2
  sigma=sigma*1d-20

  winf=where(sigma ne sigma, ct)
  if ct gt 0 then stop

  return, sigma

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function integrandr, g, k, du

; density integrand from McNutt et al. 1998

common constants, mp, kb, cgs, kev_J

  sr_int=dblarr(n_elements(g))
 
  a=2d*k*du*g
  q=k*g*g
  s=k*du*du

  sr_int=double((1/2d)*g^2*cx_int(g) $
            *(exp(-s-q+a)-exp(-s-q-a)))

  winf=where(sr_int ne sr_int or finite(sr_int) eq 0 ,ct)
  if ct gt 0 then stop

  return, sr_int

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function integrandm, g, k, du

; momentum integrand from McNutt et al. 1998

common constants, mp, kb, cgs, kev_J

  sm_int=dblarr(n_elements(g))

  a=2d*k*du*g
  q=k*g*g
  s=k*du*du

  sm_int=double((1/2d)*g^2*mp*cx_int(g) $
            *((a-1)*exp(-s-q+a)+(a+1)*exp(-s-q-a)))

  winf=where(sm_int ne sm_int,ct)
  if ct gt 0 then stop

  return, sm_int

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function integrande, g, k, du

; first energy integrand from McNutt et al. 1998

common constants, mp, kb, cgs, kev_J

  se_int=dblarr(n_elements(g))

  a=2d*k*du*g
  q=k*g*g
  s=k*du*du

  se_int=double((1/2d)*g^4*mp*cx_int(g) $
         *(exp(-s-q+a)-exp(-s-q-a)))

  winf=where(se_int ne se_int,ct)
  if ct gt 0 then stop

  return, se_int

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro SourceIntLookup

; Defining constants to be used in procedure and functions
common constants, mp, kb, cgs, kev_J
  mp=1.6726d-27 ; proton mass in kg
  kb=1.3807d-23 ; Boltzmann constant in mks
  kev_J=1.60217646d-16 ; convert keV to J

  ; du = relative bulk flow velocity
  ; k = 1/(vth_p^2+vth_h^2)

  n=101 ; number of table elements

  ; creating source term arrays
  sr=dblarr(n,n)
  sm=dblarr(n,n)
  se=dblarr(n,n)

  dg=1d ; m/s 
  g=dindgen(1d7)*dg+dg ; relative velocity (vh-vp) for integration

  ; sqrt of relative bulk velocity of H and H+ (m/s)^1/2
   sdu=double(dindgen(n)*32d-2+1d-5)*sqrt(1d3)
   du=sdu*sdu
   du2=du*du
    
  ; sqrt of combination of thermal velocities (m/s)^1/2
  sw=double(dindgen(n)*32d-2+1d)*sqrt(1d3)
  w1=sw*sw
  w2=w1*w1

  k=double(1d/w2)

  i=0

  while i lt n do begin
       j=0
     while j lt n do begin

 	; Calculating Density Source Term
        cr=2d/sqrt(double(!pi))*sqrt(k[j])/du[i]
	sr[i,j]=cr*total(integrandr(g,k[j],du[i])*dg)

	; Calculating Momentum Source Term
	if i eq 0 then sm[i,j]=0 else begin
           cm=1d/sqrt(double(!pi)*k[j])/(du2[i]*du[i])
	   sm[i,j]=cm*total(integrandm(g,k[j],du[i])*dg) ; calculate momentum source
	endelse

	; Calculating Energy Source Term
        ce=2d/sqrt(double(!pi))*sqrt(k[j]^(5.))
        ce1=1d/(2d*k[j]*du[i])
        ce2=1d/(4d*k[j]^2*du[i])
	se1=ce1*total(integrande(g,k[j],du[i])*dg) ; calculate energy source
        if sm[i,j] ne 0. then se2=ce2*sm[i,j]/cm else se2=0d
        se[i,j]=ce*(se1-se2)

        if sr[i,j] ne sr[i,j] or sm[i,j] ne sm[i,j] or se[i,j] ne se[i,j] then stop      
	j++
     endwhile
     print, string(round((i/float(n))*100.))+'%'
     i++
  endwhile



close, 1
openw, 1, "ExchangeRate.dat"
for i=0, n-1 do begin
   for j=0, n-1 do begin
	if i eq 0 then sdu[i]=0d
	printf, 1, sw[j]/sqrt(1d3), sdu[i]/sqrt(1d3), sr[i,j], sm[i,j], se[i,j]
   endfor
endfor 
close, 1
	
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
