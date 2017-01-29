#ifndef __librna_impl_h__
#define __librna_impl_h__
#if (!defined(my_len) || !defined(my_seq) || !defined(my_P) || !defined(my_dev))
#error "make sure my_len, my_seq, my_P and my_dev are well defined"
#endif
#ifndef my_P0 // we might want to cache these parameters as they are small
#define my_P0 (my_P->p0)
#endif

// -----------------------------------------------------------------------------
// Candidates for hardcoded values

#ifndef my_LXC  // jacobson_stockmayer
#define my_LXC my_P0.lxc
#endif
#ifndef my_HAIRPIN_MAX // hl_ent
#define my_HAIRPIN_MAX my_P0.hairpin[MAXLOOP]
#endif
#ifndef my_TERM_AU // termau_energy
#define my_TERM_AU my_P0.TerminalAU
#endif
#ifndef my_NINIO0 // il_asym, il_energy
#define my_NINIO0 my_P0.ninio[0]
#endif
#ifndef my_NINIO1 // il_asym
#define my_NINIO1 my_P0.ninio[1]
#endif
#ifndef my_INTERNAL_LOOP5
#define my_INTERNAL_LOOP5 my_P0.internal_loop[5]
#endif
#ifndef my_ML_CLOSING // ml_energy
#define my_ML_CLOSING my_P0.MLclosing
#endif
#ifndef my_ML_INTERN0 // ul_energy
#define my_ML_INTERN0 my_P0.MLintern[0]
#endif
#ifndef my_TEMPERATURE // mk_pf
#define my_TEMPERATURE my_P0.temperature
#endif
#ifndef my_LOG
#define my_LOG log
#endif

// -----------------------------------------------------------------------------
// Header

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>
#include "vienna/vienna.h"

enum base_t { N_BASE, A_BASE, C_BASE, G_BASE, U_BASE, GAP_BASE };
enum iupac_t { N_IUPAC = 0, B_IUPAC = 6, D_IUPAC = 7, H_IUPAC = 8, R_IUPAC = 9, V_IUPAC = 10, Y_IUPAC = 11 };
enum bp_t { N_BP, CG_BP, GC_BP, GU_BP, UG_BP, AU_BP, UA_BP, NO_BP };
typedef /*unsigned*/ int rsize; // denote indices in the sequence

my_dev static paramT *my_P = NULL;
// my_dev static char* my_seq = NULL;
// my_dev static int my_len = 0;

my_dev static int termau_energy(rsize i, rsize j);
my_dev static int hl_energy(rsize i, rsize j);
my_dev static int hl_energy_stem(rsize i, rsize j);
my_dev static int il_energy(rsize i, rsize j, rsize k, rsize l);
my_dev static int bl_energy(rsize bl, rsize i, rsize j, rsize br, rsize Xright);
my_dev static int br_energy(rsize bl, rsize i, rsize j, rsize br, rsize Xleft);
my_dev static int sr_energy(rsize i, rsize j);
my_dev static int sr_pk_energy(char a, char b, char c, char d);
my_dev static int dl_energy(rsize i, rsize j);
my_dev static int dr_energy(rsize i, rsize j);
my_dev static int dli_energy(rsize i, rsize j);
my_dev static int dri_energy(rsize i, rsize j);
my_dev static int ext_mismatch_energy(rsize i, rsize j);
my_dev static int ml_mismatch_energy(rsize i, rsize j);
my_dev static int ml_energy();
my_dev static int ul_energy();
my_dev static int sbase_energy();
my_dev static int ss_energy(rsize i, rsize j);

my_dev static int dl_dangle_dg(enum base_t dangle, enum base_t i, enum base_t j);
my_dev static int dr_dangle_dg(enum base_t i, enum base_t j, enum base_t dangle);

my_dev static double mk_pf(double x);
my_dev static double scale(int x);

my_dev static bool iupac_match(enum base_t base, unsigned char iupac_base);

#define INLINE inline
#define GAPS_DISABLED // input sequence does not contain any gap
#define NOCHECKS // avoid useless tests


// -----------------------------------------------------------------------------
// Implementation
//
// Based on RNA wrapper for Vienna-Tables from Bellman's GAP
//
// Input RNA sequence (my_seq) is encoded in bit encoding: N=0, A=1, C=2, G=3, U=4, GAP=5 (see librna/rnalib.h base_t)
// Basepairs are encoded as follow (librna/rnalib.h bp_t):
//   0=no base pair, 1=CG, 2=GC, 3=GU, 4=UG, 5=AU, 6=UA, 7=NN, N might be a gap.
//   Opposite to N_BP=0, NO_BP is set 0, instead of -INF in the Turner1999 energies.
//

my_dev static inline int bp_index(char x, char y) {
  switch (x) { // faster than lookup in __constant__ for CUDA
    case A_BASE: if (y==U_BASE) return AU_BP; break;
    case C_BASE: if (y==G_BASE) return CG_BP; break;
    case G_BASE: switch (y) { case C_BASE: return GC_BP; case U_BASE: return GU_BP; } break;
    case U_BASE: switch (y) { case G_BASE: return UG_BP; case A_BASE: return UA_BP; } break;
  }
  return NO_BP;
}

#ifdef GAPS_DISABLED // if gaps are forbidden in original sequence, we can avoid loops here
#define noGaps(i,j) 0
#ifdef NOCHECKS
  #define getNext(pos,steps,rightBorder) ((pos)+(steps))
  #define getPrev(pos,steps,leftBorder) ((pos)-(steps))
#else
  my_dev static inline rsize getNext(rsize pos, rsize steps, rsize rightBorder) { int x=pos+steps; return (x>rightBorder) ? rightBorder : x; }
  my_dev static inline rsize getPrev(rsize pos, rsize steps, rsize leftBorder) { int x=pos-steps; return (x<leftBorder) ? leftBorder : x; }
#endif
#else
my_dev static rsize noGaps(rsize i, rsize j) {
  rsize noGaps=0; rsize k; for (k=i;k<=j;++k) if (my_seq[k]==GAP_BASE) ++noGaps;
  return noGaps;
}

my_dev static rsize getNext(rsize pos, rsize steps, rsize rightBorder) { // assert(steps>0);
  rsize nongaps=0, x=pos+1; if (x>rightBorder) return rightBorder;
  do { if (my_seq[x] != GAP_BASE) ++nongaps; } while (nongaps < steps && ++x < rightBorder);
  return x;
}

my_dev static rsize getPrev(rsize pos, rsize steps, rsize leftBorder) { // assert(pos>0 && steps>0);
  rsize nongaps=0, x=pos-1; if (x<=leftBorder) return leftBorder;
  do { if (my_seq[x] != GAP_BASE) ++nongaps; } while (nongaps < steps && --x > leftBorder);
  return x;
}
#endif

#define _next(pos,steps,left) (int)my_seq[getNext(pos,steps,left)]
#define _prev(pos,steps,right) (int)my_seq[getPrev(pos,steps,right)]
#define _bp(i,j) bp_index(my_seq[i],my_seq[j])
#define _bp2(a,b,c, d,e,f) bp_index(my_seq[getPrev(a,b,c)], my_seq[getNext(d,e,f)])

my_dev static inline int jacobson_stockmayer(rsize l) { return (int)(my_LXC*my_LOG((l)*(1.0 / MAXLOOP))); } // approx 10% total time (RNAfold-768)
my_dev static inline int hl_ent(rsize l) { return (l>MAXLOOP) ? my_HAIRPIN_MAX+jacobson_stockmayer(l) : my_P0.hairpin[l]; }
my_dev static inline int hl_stack(rsize i, rsize j) { return my_P0.mismatchH[_bp(i,j)][_next(i,1,j-1)][_prev(j,1,i+1)]; }
my_dev INLINE int termau_energy(rsize i, rsize j) { return ((my_seq[i]==G_BASE && my_seq[j]==C_BASE) || (my_seq[i]==C_BASE && my_seq[j]==G_BASE)) ? 0 : my_TERM_AU; }

my_dev static int strMatch(rsize i, rsize j, const char *str, int strlen, int step) {
  int len = j-i+1; // length of the sequence to match
  const char* p; const char* pmax=str+strlen;
  for (p=str; p<pmax; p+=step) { // p is str candidate
    int n;

#ifdef GAPS_DISABLED
    const char* chr="NACGU";
    for (n=0;n<len;++n) { if (p[n]!=chr[(int)my_seq[i+n]]) break; }
#else
	int k;
    for (n=0,k=0;n<len;++n) { // for all items in [i,j]
      char c=my_seq[i+n];
      if (c==GAP_BASE) continue;
      else if (c==N_BASE) { if (p[k]!='N') break; ++k; }
      else if (c==A_BASE) { if (p[k]!='A') break; ++k; }
      else if (c==C_BASE) { if (p[k]!='C') break; ++k; }
      else if (c==G_BASE) { if (p[k]!='G') break; ++k; }
      else if (c==U_BASE) { if (p[k]!='U') break; ++k; }
      else break;
    }
#endif
    if (n==len) return ((int)(p-str))/step;
  }
  return -1; // not found
}

my_dev int hl_energy(rsize i, rsize j) { // assert(j-i>1);
  rsize size = j-i-1 - noGaps(i+1,j-1);
  if (size < 3) return 600;
  int pos;
  switch (size) {
    case 3: if ((pos=strMatch(i,j,my_P0.Triloops  ,my_P0.TriloopsLen  ,6))!=-1) return my_P0.Triloop_E[pos]; break; // special triloop cases
    case 4: if ((pos=strMatch(i,j,my_P0.Tetraloops,my_P0.TetraloopsLen,7))!=-1) return my_P0.Tetraloop_E[pos]; break; // special tetraloop cases
    case 6: if ((pos=strMatch(i,j,my_P0.Hexaloops ,my_P0.HexaloopsLen ,9))!=-1) return my_P0.Hexaloop_E[pos]; break; //special hexaloop cases
  }
  int entropy = hl_ent(size);
  if (size == 3) return entropy + termau_energy(i,j); //normal hairpins of loop size 3
  else return entropy + hl_stack(i,j); //normal hairpins of loop sizes larger than three (entropy+stack_mismatch)
}

my_dev INLINE int hl_energy_stem(rsize i, rsize j) { rsize size = j-i-1 - noGaps(i+1,j-1); return hl_energy(i,j) - (size >= 4 ? hl_stack(i,j) : 0); }

my_dev static inline int bl_ent(rsize l) { /*assert(l>0);*/ return (l>MAXLOOP) ? my_P0.bulge[MAXLOOP] + jacobson_stockmayer(l) : my_P0.bulge[l]; }

my_dev inline int bl_energy(rsize i, rsize k, rsize l, rsize j, rsize Xright) {
  // assert(j >= 2); // this is of no biological relevance, just to avoid an underflow
  rsize size = l-k+1 - noGaps(k, l);
  if (size==0) return my_P0.stack[_bp(i,j)][_bp(getPrev(j,1,Xright),l+1)];
  if (size==1) return bl_ent(size) + my_P0.stack[_bp(i,j)][_bp(getPrev(j,1,Xright),l+1)];
  if (size>1) return bl_ent(size) + termau_energy(i,j) + termau_energy(getPrev(j,1,Xright), l+1);
  return -1000000; // error
}

my_dev inline int br_energy(rsize i, rsize k, rsize l, rsize j, rsize Xleft) {
  // assert(j >= 1); // this is of no biological relevance, just to avoid an underflow
  rsize size = l-k+1 - noGaps(k, l);
  if (size == 0) return my_P0.stack[_bp(i,j)][_bp(k-1,getNext(i,1,Xleft))];
  if (size == 1) return bl_ent(size) + my_P0.stack[_bp(i,j)][_bp(k-1,getNext(i,1,Xleft))];
  if (size > 1) return bl_ent(size) + termau_energy(i, j) + termau_energy(k-1, getNext(i,1,Xleft));
  return -1000000; // error
}

// Note, basepair and stacking bases are reversed to preserver 5'-3' order
my_dev static inline int il11_energy(rsize i, rsize k, rsize l, rsize j) { return my_P->int11[_bp(i,j)][_bp2(j,2,l, i,2,k)][_next(i,1,k)][_prev(j,1,l)]; }
my_dev static inline int il12_energy(rsize i, rsize k, rsize l, rsize j) { return my_P->int21[_bp(i,j)][_bp2(j,3,l, i,2,k)][_next(i,1,k)][_prev(j,2,l)][_prev(j,1,l)]; }
my_dev static inline int il21_energy(rsize i, rsize k, rsize l, rsize j) { return my_P->int21[_bp2(j,2,l, i,3,k)][_bp(i,j)][_prev(j,1,l)][_next(i,1,k)][_next(i,2,k)]; }
my_dev static inline int il22_energy(rsize i, rsize k, rsize l, rsize j) { return my_P->int22[_bp(i,j)][_bp2(j,3,l, i,3,k)][_next(i,1,k)][_next(i,2,k)][_prev(j,2,l)][_prev(j,1,l)]; }

my_dev static inline int il_ent(rsize l) { // assert(l>1);
  return (l > MAXLOOP) ? my_P0.internal_loop[MAXLOOP] + jacobson_stockmayer(l) : my_P0.internal_loop[l];
}

my_dev static inline int il_stack(rsize i, rsize k, rsize l, rsize j) { return my_P0.mismatchI[_bp(i,j)][_next(i,1,j-1)][_prev(j,1,i+1)] + my_P0.mismatchI[_bp(l,k)][_next(l,1,j-1)][_prev(k,1,i+1)]; }
my_dev static inline int il_asym(rsize sl, rsize sr) { int r = ((int)sl-(int)sr) * my_NINIO0; if (r<0) r=-r; return (r < my_NINIO1) ? r : my_NINIO1; }

my_dev int il_energy(rsize i, rsize k, rsize l, rsize j) {
  rsize sl = k-i-1 - noGaps(i+1, k-1);
  rsize sr = j-l-1 - noGaps(l+1, j-1);

  if (sl == 0) return br_energy(i, l+1, j-1, j, k); //internal loop is a right bulge, because left unpaired region is just a gap
  if (sr == 0) return bl_energy(i, i+1, k-1, j, l); //internal loop is a left bulge, because right unpaired region is just a gap

  #define out_closingBP _bp(i,j)
  #define out_lbase _next(i,1,j-1)
  #define out_rbase _prev(j,1,i+1)
  #define in_closingBP _bp(l,k) // Note, basepair and stacking bases are reversed to preserver 5'-3' order
  #define in_lbase _next(l,1,j-1)
  #define in_rbase _prev(k,1,i+1)

  if (sl == 1) {
    if (sr == 1) return il11_energy(i, k, l, j);
    else if (sr == 2) return il12_energy(i, k, l, j);
    else return il_ent(sl+sr) + il_asym(sl,sr) + my_P0.mismatch1nI[out_closingBP][out_lbase][out_rbase] + my_P0.mismatch1nI[in_closingBP][in_lbase][in_rbase];
  } else if (sl == 2) {
    if (sr == 1) return il21_energy(i, k, l, j);
    else if (sr == 2) return il22_energy(i, k, l, j);
    else if (sr == 3) return my_INTERNAL_LOOP5+my_NINIO0 + my_P0.mismatch23I[out_closingBP][out_lbase][out_rbase] + my_P0.mismatch23I[in_closingBP][in_lbase][in_rbase];
  } else if (sl == 3 && sr == 2) {
    return my_INTERNAL_LOOP5+my_NINIO0 + my_P0.mismatch23I[out_closingBP][out_lbase][out_rbase] + my_P0.mismatch23I[in_closingBP][in_lbase][in_rbase];
  } else if (sr == 1) {
    return il_ent(sl+sr) + il_asym(sl,sr) + my_P0.mismatch1nI[out_closingBP][out_lbase][out_rbase] + my_P0.mismatch1nI[in_closingBP][in_lbase][in_rbase];
  }
  return il_ent(sl+sr) + il_stack(i, k, l, j) + il_asym(sl, sr);

  #undef out_closingBP
  #undef out_lbase
  #undef out_rbase
  #undef in_closingBP
  #undef in_lbase
  #undef in_rbase
}

my_dev INLINE int sr_energy(rsize i, rsize j) { return my_P0.stack[_bp(i,j)][_bp(j-1,i+1)]; }
my_dev INLINE int sr_pk_energy(char a, char b, char c, char d) { return my_P0.stack[bp_index(a,b)][bp_index(d,c)]; }

#ifdef NOCHECKS
  my_dev INLINE int dl_energy(rsize i, rsize j) { return my_P0.dangle5[_bp(i,j)][_prev(i,1,0)]; }
  my_dev INLINE int dr_energy(rsize i, rsize j) { return my_P0.dangle3[_bp(i,j)][_next(j,1,my_len)]; }
  my_dev INLINE int dli_energy(rsize i, rsize j) { return my_P0.dangle3[_bp(j,i)][_next(i,1,j-1)]; }
  my_dev INLINE int dri_energy(rsize i, rsize j) { return my_P0.dangle5[_bp(j,i)][_prev(j,1,i+1)]; }
#else
  my_dev INLINE int dl_energy(rsize i, rsize j) { if (i==0) return 0; int dd = my_P0.dangle5[_bp(i,j)][_prev(i,1,0)]; return (dd>0) ? 0 : dd; } // must be <= 0
  my_dev INLINE int dr_energy(rsize i, rsize j) { if ((j+1)>=my_len) return 0; int dd = my_P0.dangle3[_bp(i,j)][_next(j,1,my_len)]; return (dd>0) ? 0 : dd; } // must be <= 0
  my_dev INLINE int dli_energy(rsize i, rsize j) { int dd = my_P0.dangle3[_bp(j,i)][_next(i,1,j-1)]; return (dd>0) ? 0 : dd; } // must be <= 0
  my_dev INLINE int dri_energy(rsize i, rsize j) { int dd = my_P0.dangle5[_bp(j,i)][_prev(j,1,i+1)]; return (dd>0) ? 0 : dd; } // must be <= 0
#endif

my_dev int ext_mismatch_energy(rsize i, rsize j) {
  if ((i > 0) && ((j+1) < my_len)) return my_P0.mismatchExt[_bp(i,j)][_prev(i,1,0)][_next(j,1,my_len)];
  else if (i > 0) return dl_energy(i,j);
  else if ((j+1) < my_len) return dr_energy(i,j);
  else return 0;
}

my_dev INLINE int ml_mismatch_energy(rsize i, rsize j) { return my_P0.mismatchM[_bp(j,i)][_prev(j,1,i+1)][_next(i,1,j-1)]; } // Note, basepairs and stacking bases are reversed to preserver 5'-3' order
my_dev INLINE int ml_energy() { return my_ML_CLOSING; }
my_dev INLINE int ul_energy() { return my_ML_INTERN0; }
my_dev INLINE int sbase_energy() { return 0; }
my_dev INLINE int ss_energy(rsize i, rsize j) { return 0; }

my_dev INLINE double mk_pf(double x) { return exp((-1.0 * x/100.0) / (GASCONST/1000 * (my_TEMPERATURE + K0))); }

my_dev double scale(int x) {
  double mean_nrg= -0.1843;  // mean energy for random sequences: 184.3*length cal
  double mean_scale = exp (-1.0 * mean_nrg / (GASCONST/1000 * (my_P0.temperature + K0)));
  return (1.0 / pow(mean_scale, x));
}

my_dev INLINE int dl_dangle_dg(enum base_t dangle, enum base_t i, enum base_t j) {
  int dd = my_P0.dangle5[_bp(i,j)][dangle]; return (dd>0) ? 0 : dd;  // must be <= 0
}

my_dev INLINE int dr_dangle_dg(enum base_t i, enum base_t j, enum base_t dangle) {
  int dd = my_P0.dangle3[_bp(i,j)][dangle]; return (dd>0) ? 0 : dd;  // must be <= 0
}

// added by gsauthof, 2012
my_dev static const bool map_base_iupac[5][12] = {
    /*      { N    , A     , C     , G     , U     , _     , B     , D     , H     , R     , V     , Y     }  , */
    /* N */ { true , true  , true  , true  , true  , true  , true  , true  , true  , true  , true  , true  }  ,
    /* A */ { true , true  , false , false , false , false , false , true  , true  , true  , true  , false }  ,
    /* C */ { true , false , true  , false , false , false , true  , false , true  , false , true  , true  }  ,
    /* G */ { true , false , false , true  , false , false , true  , true  , false , true  , true  , false }  ,
    /* U */ { true , false , false , false , true  , false , true  , true  , true  , false , false , true  }  ,
};

my_dev bool iupac_match(enum base_t base, unsigned char iupac_base) {
  // assert(iupac_base<12);
  return map_base_iupac[base][iupac_base];
}

// -----------------------------------------------------------------------------
#undef my_len
#undef my_seq
#undef my_P
#undef my_dev
#endif
