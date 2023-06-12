filename='IO2/z*.outs'
func='rho 0*rho'
plottitle='Density in rotating velocity field;Block AMR grid with 4 levels'
plotmode='contfill contfillgrid'
bottomline=1
loadct_bw,4
autorange='n'
fmin=[0.9,-0.001]
fmax=[2.1,1]
!x.range=[-19,19]
!y.range=[-19,19]
savemovie='mov'
window
animate_data
