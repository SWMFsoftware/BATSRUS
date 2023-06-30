import numpy as np
import scipy as sp
import scipy.integrate as integrate

#Cross Section from Kim and Rudd (1994)
def sigma_fast(E):
    sigma = np.zeros(np.size(E))
    a0 = 5.29177210903e-11      #Bohr Radius [m]
    T = E
    U = 13.6057                 #Average Kinetic Energy of Bound Electron [eV]
    R = 13.6057                 #Rydberg Energy [eV]
    B = 13.6057                 #Binding Energy [eV]
    N = 1                       #Number of Bound Electrons
    t = T/B
    u = U/B
    S = 4*np.pi*a0**2*N*(R/B)**2
    Mi2 = 0.2834
    Q = 2*B*Mi2/N/R

    sigma = ((S/(t+u+1))*(0.5*Q*(1.-1./t**2)*np.log(t) \
            + (2.-Q)*((1.-1./t) - np.log(t)/(t+1))))*np.greater(E,B)
    return np.maximum(sigma, 0)


#Integrands
def integrand1(kappa, DeltaU, g):
    E = 0.5*me*g*g*J2eV
    a = 2*kappa*DeltaU*g
    q = kappa*g*g
    s = kappa*DeltaU*DeltaU
    if a < 1:
        return g*g*sigma_fast(E)*np.exp(-q-s)*np.sinh(a)
    else:
        return 0.5*g*g*sigma_fast(E)*(np.exp(-s-q+a)-np.exp(-s-q-a))

def integrand2(kappa, DeltaU, g):
    E = 0.5*me*g*g*J2eV
    a = 2*kappa*DeltaU*g
    q = kappa*g*g
    s = kappa*DeltaU*DeltaU
    if a < 1:
        return g*g*sigma_fast(E)*np.exp(-q-s)*(a*np.cosh(a)-np.sinh(a))
    else:
        return 0.5*g*g*sigma_fast(E)*((a-1)*np.exp(-s-q+a)+(a+1)*np.exp(-s-q-a))

def integrand3(kappa, DeltaU, g):
    E = 0.5*me*g*g*J2eV
    a = 2*kappa*DeltaU*g
    q = kappa*g*g
    s = kappa*DeltaU*DeltaU
    if a < 1:
        return g**4*sigma_fast(E)*np.exp(-s-q)*np.sinh(a)
    else:
        return 0.5*g**4*sigma_fast(E)*(np.exp(-s-q+a)-np.exp(-s-q-a))



def table_values(sw, sdu):
    w1 = sw*sw
    w2 = w1*w1
    kappa = 1/(w2+1e-32)
    DeltaU = sdu*sdu
    DeltaU2 = DeltaU*DeltaU
    gmin = np.sqrt(2*QE/me)
    gleft = max(gmin, DeltaU-4/np.sqrt(kappa))
    gright = max(DeltaU+4/np.sqrt(kappa), gleft)
    gright = max(gright, 6e7)
    # Calculating integrals
    # Splitting the integrals into three intervals helps ensure
    # high resolution around the peaks
    I1 = integrate.quad(lambda g: integrand1(kappa, DeltaU, g), gmin, gleft, \
            epsrel = 2e-14, epsabs = 0, full_output= True)[0]+\
        integrate.quad(lambda g: integrand1(kappa, DeltaU, g), gleft, gright, \
            epsrel = 2e-14, epsabs = 0, full_output= True)[0]+\
        integrate.quad(lambda g: integrand1(kappa, DeltaU, g), gright, np.inf, \
            epsrel = 2e-14, epsabs = 0, full_output= True)[0]
    I2 = integrate.quad(lambda g: integrand2(kappa, DeltaU, g), gmin, gleft, \
            epsrel = 2e-14, epsabs = 0, full_output= True)[0]+\
        integrate.quad(lambda g: integrand2(kappa, DeltaU, g), gleft, gright, \
            epsrel = 2e-14, epsabs = 0, full_output= True)[0]+\
        integrate.quad(lambda g: integrand2(kappa, DeltaU, g), gright, np.inf, \
            epsrel = 2e-14, epsabs = 0, full_output= True)[0]
    I3 = integrate.quad(lambda g: integrand3(kappa, DeltaU, g), gmin, gleft, \
            epsrel = 2e-14, epsabs = 0, full_output= True)[0]+\
        integrate.quad(lambda g: integrand3(kappa, DeltaU, g), gleft, gright, \
            epsrel = 2e-14, epsabs = 0, full_output= True)[0]+\
        integrate.quad(lambda g: integrand3(kappa, DeltaU, g), gright, np.inf, \
            epsrel = 2e-14, epsabs = 0, full_output= True)[0]

    S1 = 2*mH*np.sqrt(kappa/np.pi)/DeltaU*I1
    S2 = mH/np.sqrt(kappa*np.pi)/DeltaU2/DeltaU*I2
    S3 = 2*mH*np.sqrt(kappa**5/np.pi)*(1/(2*kappa*DeltaU)*I3 \
            - 1/(4*kappa**2*DeltaU)*I2)

    return S1, S2, S3

def main():
    n1 = 101
    n2 = 101             #Number of elements for each variable
    S1 = np.zeros([n1,n2])
    S2 = S1*0
    S3 = S1*0

    sw = np.linspace(6, 240, n1)*np.sqrt(1e3)
    sdu = np.linspace(0, 33, n2)*np.sqrt(1e3)+1e-5

    for i in range(n1):
        for j in range(n2):
            S1[i,j], S2[i,j], S3[i,j] = table_values(sw[i], sdu[j])
        print(str(round(i/n1*100*10)/10.), "%")

    with open("ElectronImpactRate.dat", 'w') as file:
        file.write(f'Integrals of electron impact ionization between'\
                f'a Maxwellian neutral and electron. sCSum and SDU are sqrt of'\
                f'sum of sound speeds sqrd and velocity diff [km/s]^1/2\n')
        file.write(f"0 0.0 2 0 3\n")
        file.write(f"{n1} {n2}\n")
        file.write(f"sCSum sDU Density Momentum Energy\n")
        for j in range(n2):
            for i in range(n1):
                file.write(f"{sw[i]/np.sqrt(1e3):16.6f}"\
                        f"{sdu[j]/np.sqrt(1e3):16.6f}{S1[i,j]:16.6e}"\
                        f"{S2[i,j]:16.6e}{S3[i,j]:16.6e}\n")



#Constants
me = 9.1094E-31     #electron mass [kg]
mp = 1.6726E-27     #proton mass [kg]
mH = mp             #Hydrogen Atom mass [kg] (Assumed same as proton mass)
kB = 1.3807E-23     #Boltzmann Constant
J2eV = 6.242e18     #convert Joules to eV
QE = 13.6057/J2eV   #Ionization Energy [J]


#run main
if __name__ == "__main__":
    main()
