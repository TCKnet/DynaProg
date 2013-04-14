#ifndef __VIENNA_RNA_PACKAGE_LOOP_ENERGIES_H__
#define __VIENNA_RNA_PACKAGE_LOOP_ENERGIES_H__

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include <string.h>

/**
 *  <H2>Compute the Energy of a hairpin-loop</H2>
 *  To evaluate the free energy of a hairpin-loop, several parameters have to be known.
 *  A general hairpin-loop has this structure:<BR>
 *  <PRE>
 *        a3 a4
 *      a2     a5
 *      a1     a6
 *        X - Y
 *        |   |
 *        5'  3'
 *  </PRE>
 *  where X-Y marks the closing pair [e.g. a <B>(G,C)</B> pair]. The length of this loop is 6 as there are
 *  six unpaired nucleotides (a1-a6) enclosed by (X,Y). The 5' mismatching nucleotide is
 *  a1 while the 3' mismatch is a6. The nucleotide sequence of this loop is &quot;a1.a2.a3.a4.a5.a6&quot; <BR>
 *  \note The parameter sequence should contain the sequence of the loop in capital letters of the nucleic acid
 *  alphabet if the loop size is below 7. This is useful for unusually stable tri-, tetra- and hexa-loops
 *  which are treated differently (based on experimental data) if they are tabulated.
 *  @see scale_parameters()
 *  @see paramT
 *  \warning Not (really) thread safe! A threadsafe implementation will replace this function in a future release!\n
 *  Energy evaluation may change due to updates in global variable "tetra_loop"
 *
 *  \param  size  The size of the loop (number of unpaired nucleotides)
 *  \param  type  The pair type of the base pair closing the hairpin
 *  \param  si1   The 5'-mismatching nucleotide
 *  \param  sj1   The 3'-mismatching nucleotide
 *  \param  string  The sequence of the loop
 *  \param  P     The datastructure containing scaled energy parameters
 *  \return The Free energy of the Hairpin-loop in dcal/mol
 */
inline static int E_Hairpin(int size, int type, int si1, int sj1, const char *string, paramT *P){
  int energy = (size <= 30) ? P->p0.hairpin[size] : P->p0.hairpin[30]+(int)(P->p0.lxc*log((size)/30.));
  if (size == 4) { /* check for tetraloop bonus */
    char tl[7]={0}, *ts;
    strncpy(tl, string, 6);
    if ((ts=strstr(P->p0.Tetraloops, tl))) return (P->p0.Tetraloop_E[(ts - P->p0.Tetraloops)/7]);
  } else if (size == 6) {
    char tl[9]={0}, *ts;
    strncpy(tl, string, 8);
    if ((ts=strstr(P->p0.Hexaloops, tl))) return (energy = P->p0.Hexaloop_E[(ts - P->p0.Hexaloops)/9]);
  } else if (size == 3) {
    char tl[6]={0,0,0,0,0,0}, *ts;
    strncpy(tl, string, 5);
    if ((ts=strstr(P->p0.Triloops, tl))) return (P->p0.Triloop_E[(ts - P->p0.Triloops)/6]);
    return (energy + (type>2 ? P->p0.TerminalAU : 0));
  }
  energy += P->p0.mismatchH[type][si1][sj1];
  return energy;
}

/**
 *  <H2>Compute the Energy of an interior-loop</H2>
 *  This function computes the free energy \f$\Delta G\f$ of an interior-loop with the
 *  following structure: <BR>
 *  <PRE>
 *        3'  5'
 *        |   |
 *        U - V
 *    a_n       b_1
 *     .        .
 *     .        .
 *     .        .
 *    a_1       b_m
 *        X - Y
 *        |   |
 *        5'  3'
 *  </PRE>
 *  This general structure depicts an interior-loop that is closed by the base pair (X,Y).
 *  The enclosed base pair is (V,U) which leaves the unpaired bases a_1-a_n and b_1-b_n
 *  that constitute the loop. In this example, the length of the interior-loop is \f$(n+m)\f$
 *  where n or m may be 0 resulting in a bulge-loop or base pair stack.
 *  The mismatching nucleotides for the closing pair (X,Y) are:<BR>
 *  5'-mismatch: a_1<BR>
 *  3'-mismatch: b_m<BR>
 *  and for the enclosed base pair (V,U):<BR>
 *  5'-mismatch: b_1<BR>
 *  3'-mismatch: a_n<BR>
 *  \note Base pairs are always denoted in 5'->3' direction. Thus the enclosed base pair
 *  must be 'turned arround' when evaluating the free energy of the interior-loop
 *  \see scale_parameters()
 *  \see paramT
 *  \note This function is threadsafe
 *
 *  \param  n1      The size of the 'left'-loop (number of unpaired nucleotides)
 *  \param  n2      The size of the 'right'-loop (number of unpaired nucleotides)
 *  \param  type    The pair type of the base pair closing the interior loop
 *  \param  type_2  The pair type of the enclosed base pair
 *  \param  si1     The 5'-mismatching nucleotide of the closing pair
 *  \param  sj1     The 3'-mismatching nucleotide of the closing pair
 *  \param  sp1     The 3'-mismatching nucleotide of the enclosed pair
 *  \param  sq1     The 5'-mismatching nucleotide of the enclosed pair
 *  \param  P       The datastructure containing scaled energy parameters
 *  \return The Free energy of the Interior-loop in dcal/mol
 */
inline static int E_IntLoop(int n1, int n2, int type, int type_2, int si1, int sj1, int sp1, int sq1, paramT *P){
  /* compute energy of degree 2 loop (stack bulge or interior) */
  int nl, ns, energy;

  if (n1>n2) { nl=n1; ns=n2;}
  else {nl=n2; ns=n1;}

  if (nl == 0) return P->p0.stack[type][type_2];  /* stack */

  if (ns==0) { /* bulge */
    energy = (nl<=MAXLOOP)?P->p0.bulge[nl]:(P->p0.bulge[30]+(int)(P->p0.lxc*log(nl/30.)));
    if (nl==1) energy += P->p0.stack[type][type_2];
    else {
      if (type>2) energy += P->p0.TerminalAU;
      if (type_2>2) energy += P->p0.TerminalAU;
    }
    return energy;
  } else { /* interior loop */
    if (ns==1) {
      if (nl==1) return P->int11[type][type_2][si1][sj1]; /* 1x1 loop */
      if (nl==2) { /* 2x1 loop */
        if (n1==1) energy = P->int21[type][type_2][si1][sq1][sj1];
        else energy = P->int21[type_2][type][sq1][si1][sp1];
        return energy;
      } else {  /* 1xn loop */
        energy = (nl+1<=MAXLOOP)?(P->p0.internal_loop[nl+1]) : (P->p0.internal_loop[30]+(int)(P->p0.lxc*log((nl+1)/30.)));
        energy += MIN(P->p0.ninio[1], (nl-ns)*P->p0.ninio[0]);
        energy += P->p0.mismatch1nI[type][si1][sj1] + P->p0.mismatch1nI[type_2][sq1][sp1];
        return energy;
      }
    } else if (ns==2) {
      if(nl==2) return P->int22[type][type_2][si1][sp1][sq1][sj1]; /* 2x2 loop */
      else if (nl==3) { /* 2x3 loop */
        energy = P->p0.internal_loop[5]+P->p0.ninio[0];
        energy += P->p0.mismatch23I[type][si1][sj1] + P->p0.mismatch23I[type_2][sq1][sp1];
        return energy;
      }
    }
    { /* generic interior loop (no else here!)*/
      energy = (n1+n2<=MAXLOOP)?(P->p0.internal_loop[n1+n2]) : (P->p0.internal_loop[30]+(int)(P->p0.lxc*log((n1+n2)/30.)));
      energy += MIN(P->p0.ninio[1], (nl-ns)*P->p0.ninio[0]);
      energy += P->p0.mismatchI[type][si1][sj1] + P->p0.mismatchI[type_2][sq1][sp1];
    }
  }
  return energy;
}

/**
 *  \def E_ExtLoop(A,B,C,D)
 *  <H2>Compute the Energy contribution of an Exterior loop stem</H2>
 *  This definition is a wrapper for the E_Stem() funtion.
 *  It is substituted by an E_Stem() funtion call with argument
 *  extLoop=1, so the energy contribution returned reflects a
 *  stem introduced in an exterior-loop.<BR>
 *  As for the parameters B (si1) and C (sj1) of the substituted
 *  E_Stem() function, you can inhibit to take 5'-, 3'-dangles
 *  or mismatch contributions to be taken into account by passing
 *  -1 to these parameters.
 *
 *  \see    E_Stem()
 *  \param  A The pair type of the stem-closing pair
 *  \param  B The 5'-mismatching nucleotide
 *  \param  C The 3'-mismatching nucleotide
 *  \param  D The datastructure containing scaled energy parameters
 *  \return   The energy contribution of the introduced exterior-loop stem
 */
inline static int E_ExtLoop(int type, int si1, int sj1, paramT *P){
  int energy = 0;
  if(si1 >= 0 && sj1 >= 0) energy += P->p0.mismatchExt[type][si1][sj1];
  else if (si1 >= 0) energy += P->p0.dangle5[type][si1];
  else if (sj1 >= 0) energy += P->p0.dangle3[type][sj1];
  if(type > 2) energy += P->p0.TerminalAU;
  return energy;
}

/**
 *  \def E_MLstem(A,B,C,D)
 *  <H2>Compute the Energy contribution of a Multiloop stem</H2>
 *  This definition is a wrapper for the E_Stem() funtion.
 *  It is substituted by an E_Stem() funtion call with argument
 *  extLoop=0, so the energy contribution returned reflects a
 *  stem introduced in a multiloop.<BR>
 *  As for the parameters B (si1) and C (sj1) of the substituted
 *  E_Stem() function, you can inhibit to take 5'-, 3'-dangles
 *  or mismatch contributions to be taken into account by passing
 *  -1 to these parameters.
 *
 *  \see    E_Stem()
 *  \param  A The pair type of the stem-closing pair
 *  \param  B The 5'-mismatching nucleotide
 *  \param  C The 3'-mismatching nucleotide
 *  \param  D The datastructure containing scaled energy parameters
 *  \return   The energy contribution of the introduced multiloop stem
 */
inline static int E_MLstem(int type, int si1, int sj1, paramT *P){
  int energy = 0;
  if(si1 >= 0 && sj1 >= 0) energy += P->p0.mismatchM[type][si1][sj1];
  else if (si1 >= 0) energy += P->p0.dangle5[type][si1];
  else if (sj1 >= 0) energy += P->p0.dangle3[type][sj1];
  if(type > 2) energy += P->p0.TerminalAU;
  energy += P->p0.MLintern[type];
  return energy;
}

#endif
