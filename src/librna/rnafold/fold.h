#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include <string.h>
#include <limits.h>

/**
 *  Compute minimum free energy and an appropriate secondary structure of an RNA sequence
 *
 *  The first parameter given, the RNA sequence, must be \a uppercase and should only contain
 *  an alphabet \f$\Sigma\f$ that is understood by the RNAlib\n
 *  (e.g. \f$ \Sigma = \{A,U,C,G\} \f$)\n
 *
 *  The second parameter, \a structure, must always point to an allocated
 *  block of memory with a size of at least \f$\mathrm{strlen}(\mathrm{sequence})+1\f$
 *
 *  After a successful call of fold_par(), a backtracked secondary structure (in dot-bracket notation)
 *  that exhibits the minimum of free energy will be written to the memory \a structure is pointing to.
 *  The function returns the minimum of free energy for any fold of the sequence given.
 *
 *  \param sequence       RNA sequence
 *  \param structure      A pointer to the character array where the
 *                        secondary structure in dot-bracket notation will be written to
 *  \return the minimum free energy (MFE) in kcal/mol
 */
float fold_par(const char *sequence, char *structure);

// -----------------------------------------------------------------------------

#include "vienna.h"
#include "energy_const.h"
#include "energy_par.h"

#define MAX(a,b) ({__typeof__(a) _a=(a); __typeof__(b) _b=(b); _a>_b?_a:_b; })
#define MIN(a,b) ({__typeof__(a) _a=(a); __typeof__(b) _b=(b); _a<_b?_a:_b; })

#include "loop_energies.h"

extern void nrerror(const char message[]);

typedef struct sect { int i,j,ml; } sect; /** stack of partial structures for backtracking */
typedef struct bondT { unsigned int i,j; } bondT; /** base pair */

// TCK: complementary basepair index (swapped bases)
static int rtype[8] = {0, 2, 1, 4, 3, 6, 5, 7}; /* rtype[pair[i][j]]:=pair[j][i] */

#define MAXSECTORS 500 /* dimension for a backtrack array */

/********************************
* static VARIABLES             *
********************************/
static int     *indx     = NULL; /* index for moving in the triangle matrices c[] and fMl[]*/
static int     *c        = NULL; /* energy array, given that i-j pair */
static int     *cc       = NULL; /* linear array for calculating canonical structures */
static int     *cc1      = NULL; /*   "     "        */
static int     *f5       = NULL; /* energy of 5' end */
static int     *fML      = NULL; /* multi-loop auxiliary energy array */
static int     *Fmi      = NULL; /* holds row i of fML (avoids jumps in memory) */
static int     *DMLi     = NULL; /* DMLi[j] holds MIN(fML[i,k]+fML[k+1,j])  */
static int     *DMLi1    = NULL; /*             MIN(fML[i+1,k]+fML[k+1,j])  */
static sect    sector[MAXSECTORS]; /* stack of partial structures for backtracking */
static char    *ptype    = NULL; /* precomputed array of pair types */
static short   *S        = NULL; // TCK: sequence encoded as <length>, base0, base1, ..., baseN, base0, \0 with bases in numerical format
static paramT  *P          = NULL;
static bondT   *base_pair2 = NULL; // THIS STORES THE BACKTRACK PAIRS

/********************************
* static FUNCTION DECLARATIONS *
********************************/
static void  backtrack(const char *sequence);
static int   fill_arrays(const char *sequence);

/********************************
* BEGIN OF FUNCTION DEFINITIONS *
********************************/

//#define DBG
#ifdef DBG
unsigned my_size=0;
#endif
static void init_arrays(unsigned int size){
  if(size >= (unsigned int)sqrt((double)INT_MAX))
    nrerror("get_arrays@fold.c: sequence length exceeds addressable range");
  #ifdef DBG
  my_size=size;
  #endif
  c     = (int *) calloc((size*(size+1))/2+2, sizeof(int)); // cost of the outer rule
  fML   = (int *) calloc((size*(size+1))/2+2, sizeof(int)); // cost of multiloop ??
  ptype = (char*) calloc((size*(size+1))/2+2, sizeof(char)); // type of basepair
  f5    = (int *) calloc(size+2, sizeof(int));
  cc    = (int *) calloc(size+2, sizeof(int));
  cc1   = (int *) calloc(size+2, sizeof(int));
  Fmi   = (int *) calloc(size+1, sizeof(int));
  DMLi  = (int *) calloc(size+1, sizeof(int));
  DMLi1 = (int *) calloc(size+1, sizeof(int));
  base_pair2 = (bondT *) calloc(1+size/2, sizeof(bondT));
}

// -----------------------------------------------------------------------------

static void free_arrays(){
  #ifdef DBG
  unsigned int i,j;
  #define PRINT_TAB(X) \
  for (i=1;i<=my_size;++i) { \
    printf("[ C  ] "); \
    for (j=1;j<=my_size;++j) { \
      if (j>i) { \
       int v = X[indx[j]+i]; \
       if (v>=214748364) printf("  .   |"); \
       else printf("%5d |",v); \
      } else printf("      |"); \
    } \
    printf("\n"); \
  }
  //printf("Table c:\n");
  PRINT_TAB(c)
  //printf("Table fML:\n"); PRINT_TAB(fML)
  #endif

  if(indx)  free(indx);
  if(c)     free(c);
  if(fML)   free(fML);
  if(f5)    free(f5);
  if(cc)    free(cc);
  if(cc1)   free(cc1);
  if(ptype) free(ptype);
  if(base_pair2) free(base_pair2);
  if(Fmi)   free(Fmi);
  if(DMLi)  free(DMLi);
  if(DMLi1) free(DMLi1);
  if(P)     free(P);
  if(S)     free(S);
  indx = c = fML = f5 = cc = cc1 = Fmi = DMLi = DMLi1 = NULL;
  ptype       = NULL;
  base_pair2  = NULL;
  P           = NULL;
  S           = NULL;
}

// -----------------------------------------------------------------------------

PUBLIC float fold_par(const char *string, char *structure) {
  unsigned int i,length;
  int energy;
  length = (int) strlen(string);
  if (length<1) nrerror("initialize_fold: argument must be greater 0");
  init_arrays((unsigned) length);

  // TCK: set diagonal offsets in indx[diagonal]
  indx = (int *)calloc(length+1, sizeof(int));
  for (i=1; i<=length; i++) indx[i]=(i*(i-1)) >> 1; // i(i-1)/2

  P = get_scaled_parameters();

  // TCK: encode input sequence in S
  S = (short*) calloc(length+2, sizeof(short));
  for(i=1; i<=length; i++) {
    switch(string[i-1]) {
      case 'A': case 'a': S[i]=1; break;
      case 'C': case 'c': S[i]=2; break;
      case 'G': case 'g': S[i]=3; break;
      case 'U': case 'u': S[i]=4; break;
      default: S[i]=0;
    }
  }
  S[length+1] = S[1];
  S[0] = (short) length;

  // TCK: make pair types
  int pair[5][5]= // TCK: converts a pair of bases to a basepair index
  /* _  A  C  G  U */
  {{ 0, 0, 0, 0, 0},
   { 0, 0, 0, 0, 5},
   { 0, 0, 0, 1, 0},
   { 0, 0, 2, 0, 3},
   { 0, 6, 0, 4, 0}};
  int k,l;
  for (k=1; k<length-TURN; k++) for (l=1; l<=2; l++) { // TURN=3, minimum loop length
    int i=k, j=i+TURN+l; if (j>length) continue;
    int type=pair[S[i]][S[j]], ntype=0, otype=0;
    for (;(i>=1)&&(j<=length);--i,++j,otype=type,type=ntype) {
      if ((i>1)&&(j<length)) ntype = pair[S[i-1]][S[j+1]];
      if ((!otype) && (!ntype)) type = 0; // i..j can only form isolated pairs
      ptype[indx[j]+i] = (char) type;
    }
  }

  energy = fill_arrays(string);
  backtrack(string);

  // TCK: pretty print the backtrack (parenthesis structure)
  int n;
  for (n=0; n<length; ++n) structure[n]='.'; structure[length]='\0';
  for (n=1; n<=base_pair2[0].i; n++) {
    structure[base_pair2[n].i-1] = '(';
    structure[base_pair2[n].j-1] = ')';
  }

  free_arrays();
  return (float) energy/100.;
}

/** fill "c", "fML" and "f5" arrays and return optimal energy */
static int fill_arrays(const char *string) {
  int i, j, k, length, energy;
  int decomp, new_fML;
  int type, type_2;

  length = (int) strlen(string); if (length<=TURN) return 0;

  for (j=1; j<=length; j++) Fmi[j]=DMLi[j]=DMLi1[j]=INF;
  for (j=1; j<=length; j++) for (i=(j>TURN?(j-TURN):1); i<j; i++) { c[indx[j]+i]=fML[indx[j]+i]=INF; }

  for (i=length-TURN-1; i>=1; --i) { /* i,j in [1..length] */
    for (j=i+TURN+1; j<=length; ++j) {
      int p, q, ij;
      ij = indx[j]+i;
      type = ptype[ij];
      energy = INF;

      // TCK: compute c[i,j]
      if (type) { /* we have a pair */
        int new_c=0, stackEnergy=INF;
        /* hairpin ----------------------------------------------*/
        new_c = E_Hairpin(j-i-1, type, S[i+1], S[j-1], string+i-1, P);
        /* check for elementary structures involving more than one closing pair. */
        for (p=i+1; p<=MIN(j-2-TURN,i+MAXLOOP+1);p++) {
          for (q=MAX(j-i+p-MAXLOOP-2,p+1+TURN); q<j; q++) {
            type_2 = ptype[indx[q]+p]; if (type_2==0) continue;
            energy = E_IntLoop(p-i-1, j-q-1, type, rtype[type_2], S[i+1], S[j-1], S[p-1], S[q+1], P);
            new_c = MIN(energy+c[indx[q]+p], new_c);
          }
          if (p==i+1) stackEnergy=energy; /* remember stack energy */
        }
        /* multi-loop decomposition ------------------------*/
        decomp = DMLi1[j-1] + E_MLstem(rtype[type],S[j-1],S[i+1],P);
        c[ij] = cc1[j-1]+stackEnergy;
        cc[j] = MIN(MIN(new_c, decomp + P->p0.MLclosing), c[ij]);
      } /* end >> if (pair) << */
      else c[ij] = INF;

      // TCK: compute fML[i,j]
      new_fML = INF; /* (i,j) + MLstem ? */
      if(type) new_fML = c[ij] + E_MLstem(type, (i==1) ? S[length] : S[i-1], S[j+1], P);

      /* free ends ? -----------------------------------------*
       * we must not just extend 3'/5' end by unpaired nucleotides if dangle_model == 1,
       * this could lead to d5+d3 contributions were mismatch must be taken! */
      new_fML = MIN(new_fML, fML[ij+1]+P->p0.MLbase); // i+1,j
      new_fML = MIN(new_fML, fML[indx[j-1]+i]+P->p0.MLbase); // i,j-1

      /* modular decomposition -------------------------------*/
      decomp=INF;
      for (k=i+1+TURN; k<=j-2-TURN; k++) decomp=MIN(decomp, Fmi[k] /*fML[indx[k]+i]*/ +fML[indx[j]+k+1]);
      DMLi[j] = decomp; /* store for use in ML decompositon */
      new_fML = MIN(new_fML, decomp);
      /* coaxial stacking */
      fML[ij] = Fmi[j] = new_fML; /* substring energy */
    }
    /* rotate the auxilliary arrays */
    int *FF=DMLi1; DMLi1=DMLi; DMLi=FF; FF=cc1; cc1=cc; cc=FF;
    for (j=1; j<=length; ++j) { cc[j]=Fmi[j]=DMLi[j]=INF; }
  }

  /* calculate energies of 5' and 3' fragments */
  f5[TURN+1]= 0;
  /* always use dangles on both sides */
  #define f5_calc(j,Sj1) { f5[j]=f5[j-1]; \
    for (i=j-TURN-1; i>1; --i) { type=ptype[indx[j]+i]; if(type) f5[j]=MIN(f5[j], f5[i-1] + c[indx[j]+i] + E_ExtLoop(type, S[i-1], Sj1, P)); } \
    type=ptype[indx[j]+1]; if(type) f5[j]=MIN(f5[j], c[indx[j]+1]+E_ExtLoop(type,-1,S[j+1],P)); }

  for(j=TURN+2; j<length; ++j) f5_calc(j,S[j+1]);
  f5_calc(length,-1)

  return f5[length];
}

/*
 * Trace back through the "c", "f5" and "fML" arrays to get the
 * base pairing list. No search for equivalent structures is done.
 * This is fast, since only few structure elements are recalculated.
 * Normally s=0. If s>0 then s items have been pushed on sector stack
 */
static void backtrack(const char *string) {
  int   i, j, ij, k, mm3, length, energy, en;
  int   type, type_2, tt;
  int   b=0;
  int   s=0;

  #define PUSH(I,J,ML) { sector[++s].i=(I); sector[s].j = (J); sector[s].ml = (ML); }
  #define PAIR2(I,J) { base_pair2[++b].i=(I); base_pair2[b].j=(J); }

  length = strlen(string);
  if (s==0) PUSH(1,length,0)
  while (s>0) {
    int ml, fij, fi, cij=0, traced, i1, p, q, jj=0;
    int canonical = 1;     /* (i,j) closes a canonical structure */
    i  = sector[s].i;
    j  = sector[s].j;
    ml = sector[s--].ml;   /* ml is a flag indicating if backtracking is to
                              occur in the fML- (1) or in the f-array (0) */
    if (ml==2) { PAIR2(i,j) goto repeat1; }

    if (j < i+TURN+1) continue; /* no more pairs in this interval */
    fij = (ml == 1)? fML[indx[j]+i] : f5[j];
    fi  = (ml == 1)?(fML[indx[j-1]+i]+P->p0.MLbase): f5[j-1];
    if (fij == fi) { PUSH(i,j-1,ml) continue; } /* 3' end is unpaired */

    if (ml == 0) { /* backtrack in f5 */
      mm3 = (j<length) ? S[j+1] : -1;
      for(k=j-TURN-1,traced=0; k>=1; --k) {
        type = ptype[indx[j]+k];
        if(type && fij == E_ExtLoop(type, (k>1) ? S[k-1] : -1, mm3, P) + c[indx[j]+k] + f5[k-1]) { traced=j; jj=k-1; break; }
      }
      if (!traced) { fprintf(stderr, "%s\n", string); nrerror("backtrack failed in f5"); }
      PUSH(1,jj,ml); i=k; j=traced; PAIR2(i,j)
      goto repeat1;
    } else { /* trace back in fML array */
      if (fML[indx[j]+i+1]+P->p0.MLbase == fij) { PUSH(i+1,j,ml); continue; } /* 5' end is unpaired */
      ij  = indx[j]+i;
      tt  = ptype[ij];
      en  = c[ij];
      if(fij == en + E_MLstem(tt, S[i-1], S[j+1], P)) { PAIR2(i,j); goto repeat1; }
      for(k=i+1+TURN; k<=j-2-TURN; k++) if(fij == fML[indx[k]+i]+fML[indx[j]+k+1]) break;
      PUSH(i,k,ml); PUSH(k+1,j,ml);

      if (k>j-2-TURN) nrerror("backtrack failed in fML");
      continue;
    }

  repeat1: /*----- begin of "repeat:" -----*/
    ij = indx[j]+i;
    if (canonical)  cij = c[ij];

    type = ptype[ij];
    if (cij == c[ij]) { /* (i.j) closes canonical structures, thus (i+1.j-1) must be a pair */
      type_2 = rtype[(int)ptype[indx[j-1]+i+1]];
      cij -= P->p0.stack[type][type_2];
      PAIR2(i+1,j-1)
      i++; j--;
      canonical=0;
      goto repeat1;
    }
    canonical = 1;

    if (cij==E_Hairpin(j-i-1, type, S[i+1], S[j-1],string+i-1, P)) continue;

    for (p=i+1; p<=MIN(j-2-TURN,i+MAXLOOP+1); ++p) {
      int minq = j-i+p-MAXLOOP-2;
      if (minq<p+1+TURN) minq = p+1+TURN;
      for (q = j-1; q >= minq; q--) {

        type_2 = ptype[indx[q]+p]; if (type_2==0) continue;
        /* energy = oldLoopEnergy(i, j, p, q, type, type_2); */
        energy = E_IntLoop(p-i-1, j-q-1, type, rtype[type_2], S[i+1], S[j-1], S[p-1], S[q+1], P);

        if (cij == energy+c[indx[q]+p]) {
          PAIR2(p,q); i=p; j=q; goto repeat1;
        }
      }
    }

    /* end of repeat: --------------------------------------------------*/
    /* (i.j) must close a multi-loop */
    tt = rtype[type];
    i1 = i+1;

    en = cij - E_MLstem(tt, S[j-1], S[i+1], P) - P->p0.MLclosing;
    for(k = i+2+TURN; k < j-2-TURN; k++){
      if(en == fML[indx[k]+i+1] + fML[indx[j-1]+k+1]) break;
    }

    if (k<=j-3-TURN) { PUSH(i1,k,1); PUSH(k+1,j,1); } /* found the decomposition */
    else nrerror("backtracking failed in repeat");
  }

  base_pair2[0].i=b; /* save the total number of base pairs */
}
