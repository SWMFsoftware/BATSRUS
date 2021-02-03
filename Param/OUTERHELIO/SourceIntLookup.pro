;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function cx_int, g

; cross-section from Lindsay & Stebbings (2005)
; for H-H+ collision  up to 250 keV

common constants, mp, kb, cgs, kev_J

  a1=double(4.15)
  a2=double(0.531)
  a3=double(67.3)

  e=double(0.5*mp*g*g/kev_J) ; convert relative vel. to energy

  sig1=(a1-a2*alog(e))
  sig1=sig1*sig1
  sig2=(1-exp(-a3/e))
  sig2=sig2*sig2*sig2*sig2*sqrt(sig2)
  sigma=sig1*sig2
  sigma=sigma*1d-20

  w=where(sigma ne sigma, ct)
  if ct gt 0 then stop

  return, sigma

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function integrandr, g, k, du

; density integrand from McNutt et al. 1998

common constants, mp, kb, cgs, kev_J

  sr_int=dblarr(n_elements(g))

  w=where(2*k*du*g lt 1d2,complement=nw)

  sr_int[w]=double(g[w]^2*cx_int(g[w])*exp(-k*g[w]*g[w]) $
        *sinh(2.*k*du*g[w]))

  sr_int[nw]=0.

  w1=where(sr_int ne sr_int,ct)
  if ct gt 0 then stop

  return, sr_int

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function integrandm, g, k, du

; momentum integrand from McNutt et al. 1998

common constants, mp, kb, cgs, kev_J

  sm_int=dblarr(n_elements(g))

  w=where(2*k*du*g lt 1d2,complement=nw)

  sm_int[w]=double(g[w]^2*mp*cx_int(g[w])*exp(-k*g[w]*g[w]) $
        *(2.*k*du*g[w]*cosh(2.*k*du*g[w]) $
        -sinh(2.*k*du*g[w])))

  sm_int[nw]=0.

  w1=where(sm_int ne sm_int,ct)
  if ct gt 0 then stop

  return, sm_int

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function integrande, g, k, du

; first energy integrand from McNutt et al. 1998

common constants, mp, kb, cgs, kev_J

  se_int=dblarr(n_elements(g))

  w=where(2*k*du*g lt 1d2,complement=nw)

  se_int[w]=double(g^4*mp*cx_int(g[w])*exp(-k*g[w]*g[w]) $
         *sinh(2*k*du*g[w]))

  se_int[nw]=0.

  w1=where(se_int ne se_int,ct)
  if ct gt 0 then stop

  return, se_int

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro SourceIntLookup

; IDL procedure to populate lookup table of density,
; momentum, and enegry source terms using integrals
; from McNutt et al. (1998) equations 24, 25, and 27

; Defining constants to be used in procedure and functions
common constants, mp, kb, cgs, kev_J
  mp=1.6726d-27 ; proton mass in kg
  kb=1.3807d-23 ; Boltzmann constant in mks
  kev_J=1.60217646d-16 ; convert keV to J

  ; du = relative bulk flow velocity
  ; k = 1/(vth_p^2+vth_h^2)

  n=100 ; number of table elements

  ; creating source term arrays
  sr=dblarr(n,n)
  sm=dblarr(n,n)
  se=dblarr(n,n)

  dg=1. ; m/s 
  g=dindgen(1e7)*dg+dg ; relative velocity (vh-vp) for integration

  ; relative bulk velocity of H and H+ squared (m/s)^2
  du2=(dindgen(n)*1d4+1.)*1d6
  du=sqrt(du2)

  ; combination of thermal velocities squared (m/s)^2
  w2=((dindgen(n)*1d4)+1.)*1d6

  k=double(1./w2)

  i=0

  while i lt n do begin
       j=0
     while j lt n do begin

        b=exp(-k[j]*du2[i])

 	; Calculating Density Source Term
        cr=2./sqrt(!pi)*sqrt(k[j])/du[i]*b
	sr[i,j]=cr*total(integrandr(g,k[j],du[i])*dg)

	; Calculating Momentum Source Term
        cm=1./sqrt(!pi*k[j])/(du2[i])*b
	sm[i,j]=cm*total(integrandm(g,k[j],du[i])*dg) ; calculate momentum source

	; Calculating Energy Source Term
        ce=2./sqrt(!pi)*sqrt(k[j]^(5.))*b
        ce1=1./(k[j]*du[i])
        ce2=1./(4.*k[j]^2*du[i])
	se1=ce1*total(integrande(g,k[j],du[i])*dg) ; calculate energy source
        if sm[i,j] ne 0. then se2=ce2*sm[i,j]/cm else se2=0.; total(integrande2(g,k[j],du[i])*dg)
        se[i,j]=ce*(se1+se2)

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
	printf, 1, w2[j]/(1d6), du2[i]/(1d6), sr[i,j], sm[i,j], se[i,j]
   endfor
endfor 
close, 1
	
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
