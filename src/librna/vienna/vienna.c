#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "vienna.h"
#include "energy_data.h"

// -----------------------------------------

static void nrerror(const char* msg) { fprintf(stderr,"ERROR: %s.\n",msg); exit(EXIT_FAILURE); }

paramT *get_scaled_parameters() {
  unsigned int i,j,k,l,m,n;
  double tempf;
  paramT *params = (paramT*)calloc(1,sizeof(paramT));
  if (params==NULL) nrerror("Out of memory");

  /* store the model details */
  double temperature = 37.0; /* temperature */
  params->p0.temperature = temperature;
  tempf                  = ((temperature+K0)/Tmeasure);

  #define RESCALE(X,Y) (X)-((X)-(Y))*tempf

  for (i=0; i<31; i++) params->p0.hairpin[i] = RESCALE(hairpindH[i], hairpin37[i]);
  #define MIN2(A,B) ((A) < (B) ? (A) : (B))
  for (i=0; i<=MIN2(30,MAXLOOP); i++) {
    params->p0.bulge[i]          = RESCALE(bulgedH[i], bulge37[i]);
    params->p0.internal_loop[i]  = RESCALE(internal_loopdH[i], internal_loop37[i]);
  }
  params->p0.lxc = lxc37*tempf;
  for (; i<=MAXLOOP; i++) {
    params->p0.bulge[i] = params->p0.bulge[30]+(int)(params->p0.lxc*log((double)(i)/30.));
    params->p0.internal_loop[i] = params->p0.internal_loop[30]+(int)(params->p0.lxc*log((double)(i)/30.));
  }

  params->p0.ninio[0] = RESCALE(niniodH, ninio37);
  params->p0.ninio[1] = MAX_NINIO;

  params->p0.TripleC = RESCALE(TripleCdH, TripleC37);
  params->p0.MultipleCA = RESCALE(MultipleCAdH, MultipleCA37);
  params->p0.MultipleCB = RESCALE(MultipleCBdH, MultipleCB37);

  params->p0.TetraloopsLen=strlen(Tetraloops);
  params->p0.TriloopsLen=strlen(Triloops);
  params->p0.HexaloopsLen=strlen(Hexaloops);
  for (i=0; (i*7)<params->p0.TetraloopsLen; i++) params->p0.Tetraloop_E[i] = RESCALE(TetraloopdH[i],Tetraloop37[i]);
  for (i=0; (i*5)<params->p0.TriloopsLen; i++) params->p0.Triloop_E[i] = RESCALE(TriloopdH[i],Triloop37[i]);
  for (i=0; (i*9)<params->p0.HexaloopsLen; i++) params->p0.Hexaloop_E[i] = RESCALE(HexaloopdH[i],Hexaloop37[i]);

  params->p0.TerminalAU = RESCALE(TerminalAUdH,TerminalAU37);
  params->p0.DuplexInit = RESCALE(DuplexInitdH,DuplexInit37);
  params->p0.MLbase = RESCALE(ML_BASEdH,ML_BASE37);

  for (i=0; i<=NBPAIRS; i++) params->p0.MLintern[i] = RESCALE(ML_interndH,ML_intern37);
  params->p0.MLclosing = RESCALE(ML_closingdH,ML_closing37);

  /* stacks    G(T) = H - [H - G(T0)]*T/T0 */
  for (i=0; i<=NBPAIRS; i++) for (j=0; j<=NBPAIRS; j++) params->p0.stack[i][j] = RESCALE(stackdH[i][j], stack37[i][j]);

  /* mismatches */
  for (i=0; i<=NBPAIRS; i++)
    for (j=0; j<5; j++)
      for (k=0; k<5; k++) {
        int mm;
        params->p0.mismatchI[i][j][k]   = RESCALE(mismatchIdH[i][j][k], mismatchI37[i][j][k]);
        params->p0.mismatchH[i][j][k]   = RESCALE(mismatchHdH[i][j][k], mismatchH37[i][j][k]);
        params->p0.mismatch1nI[i][j][k] = RESCALE(mismatch1nIdH[i][j][k], mismatch1nI37[i][j][k]); /* interior nx1 loops */
        params->p0.mismatch23I[i][j][k] = RESCALE(mismatch23IdH[i][j][k], mismatch23I37[i][j][k]); /* interior 2x3 loops */
        mm = RESCALE(mismatchMdH[i][j][k], mismatchM37[i][j][k]); params->p0.mismatchM[i][j][k] = (mm > 0) ? 0 : mm;
        mm = RESCALE(mismatchExtdH[i][j][k], mismatchExt37[i][j][k]); params->p0.mismatchExt[i][j][k] = (mm > 0) ? 0 : mm;
      }

  /* dangles */
  for (i=0; i<=NBPAIRS; i++)
    for (j=0; j<5; j++) {
      int dd;
      dd = dangle5_dH[i][j] - (dangle5_dH[i][j] - dangle5_37[i][j])*tempf;
      params->p0.dangle5[i][j] = (dd>0) ? 0 : dd;  /* must be <= 0 */
      dd = dangle3_dH[i][j] - (dangle3_dH[i][j] - dangle3_37[i][j])*tempf;
      params->p0.dangle3[i][j] = (dd>0) ? 0 : dd;  /* must be <= 0 */
    }
  for (i=0; i<=NBPAIRS; i++) for (j=0; j<=NBPAIRS; j++) for (k=0; k<5; k++) for (l=0; l<5; l++) {
    /* interior 1x1 loops */
    params->int11[i][j][k][l] = RESCALE(int11_dH[i][j][k][l],int11_37[i][j][k][l]);
    /* interior 2x1 loops */
    for (m=0; m<5; m++) params->int21[i][j][k][l][m] = RESCALE(int21_dH[i][j][k][l][m],int21_37[i][j][k][l][m]);
    /* interior 2x2 loops */
    for (m=0; m<5; m++) for (n=0; n<5; n++) params->int22[i][j][k][l][m][n] = RESCALE(int22_dH[i][j][k][l][m][n],int22_37[i][j][k][l][m][n]);
  }
  strncpy(params->p0.Tetraloops, Tetraloops, 281);
  strncpy(params->p0.Triloops, Triloops, 241);
  strncpy(params->p0.Hexaloops, Hexaloops, 361);
  return params;
}

/* -------------------------------------------------------------------------- */
/*                     READING OF PARAMETER FILE                              */
/* -------------------------------------------------------------------------- */

#define DEF -50
#define NST 0

static FILE *fp;

static char  *get_array1(int *arr, int size);
static void  check_symmetry(void);
static void  update_nst(int array[NBPAIRS+1][NBPAIRS+1][5][5][5][5]);

/**
*** read a X-dimensional array from file
*** \param array  a pointer to the first element in the array
*** \param d (dim)    the size of the array
*** \param s (shift)  the first position the new values will be written in
**/

#define rd_1dim(array,d1,s1) rd_1dim_slice(array,d1,s1,0)
#define rd_2dim(array,d1,d2,s1,s2) rd_2dim_slice(array,d1,d2,s1,s2,0,0)
#define rd_3dim(array,d1,d2,d3,s1,s2,s3) rd_3dim_slice(array,d1,d2,d3,s1,s2,s3,0,0,0)
#define rd_4dim(array,d1,d2,d3,d4,s1,s2,s3,s4) rd_4dim_slice(array,d1,d2,d3,d4,s1,s2,s3,s4,0,0,0,0)
#define rd_5dim(array,d1,d2,d3,d4,d5,s1,s2,s3,s4,s5) rd_5dim_slice(array,d1,d2,d3,d4,d5,s1,s2,s3,s4,s5,0,0,0,0,0)
static void rd_1dim_slice(int *array, int d, int s, int p) { char *cp=get_array1(array+s, d-s-p); if (cp) { fprintf(stderr,"\nrd_1dim: %s\n", cp); exit(EXIT_FAILURE); } }
static void rd_2dim_slice(int *array, int d1, int d2, int s1, int s2, int p1, int p2){
  int i, delta_pre=s1+s2, delta_post=p1+p2; if(delta_pre+delta_post==0) { rd_1dim(array, d1*d2, 0); return; }
  for (i=s1; i<d1-p1; i++) rd_1dim_slice(array + (i*d2), d2, s2, p2);
}
static void rd_3dim_slice(int *array, int d1, int d2, int d3, int s1, int s2, int s3, int p1, int p2, int p3) {
  int i, delta_pre=s1+s2+s3, delta_post=p1+p2+p3; if(delta_pre+delta_post==0) { rd_1dim(array, d1*d2*d3, 0); return; }
  for (i=s1; i<d1-p1; i++) rd_2dim_slice(array+(i*d2*d3), d2,d3,s2,s3,p2,p3);
}
static void rd_4dim_slice(int *array, int d1, int d2, int d3, int d4, int s1, int s2, int s3, int s4, int p1, int p2, int p3, int p4){
  int i, delta_pre=s1+s2+s3+s4, delta_post=p1+p2+p3+p4; if(delta_pre+delta_post==0) { rd_1dim(array, d1*d2*d3*d4, 0); return; }
  for(i=s1; i<d1-p1; i++) rd_3dim_slice(array + (i*d2*d3*d4), d2, d3, d4, s2, s3, s4, p2, p3, p4);
}
static void rd_5dim_slice(int *array, int d1, int d2, int d3, int d4, int d5, int s1, int s2, int s3, int s4, int s5, int p1, int p2, int p3, int p4, int p5){
  int i, delta_pre=s1+s2+s3+s4+s5, delta_post=p1+p2+p3+p4+p5; if(delta_pre+delta_post == 0) { rd_1dim(array, d1*d2*d3*d4*d5, 0); return; }
  for(i=s1; i<d1-p1; i++) rd_4dim_slice(array + (i*d2*d3*d4*d5), d2, d3, d4, d5, s2, s3, s4, s5, p2, p3, p4, p5);
}
static void  rd_6dim_slice(int *array, int d1, int d2, int d3, int d4, int d5, int d6, int s1, int s2, int s3, int s4, int s5, int s6, int p1, int p2, int p3, int p4, int p5, int p6){
  int i, delta_pre=s1+s2+s3+s4+s5+s6, delta_post=p1+p2+p3+p4+p5+p6; if(delta_pre+delta_post==0) { rd_1dim(array, d1*d2*d3*d4*d5*d6, 0); return; }
  for(i=s1; i<d1-p1; i++) rd_5dim_slice(array + (i*d2*d3*d4*d5*d6), d2, d3, d4, d5, d6, s2, s3, s4, s5, s6, p2, p3, p4, p5, p6);
}

static void rd_Tetraloop37(void);
static void rd_Triloop37(void);
static void rd_Hexaloop37(void);

static char *get_line(FILE *fp) { /* reads lines of arbitrary length from fp */
  char s[512], *line, *cp;
  int len=0, size=0, l;
  line=NULL;
  do {
    if (fgets(s, 512, fp)==NULL) break;
    cp = strchr(s, '\n');
    if (cp != NULL) *cp = '\0';
    l = len + (int)strlen(s);
    if (l+1>size) {
      size = (int)((l+1)*1.2);
      if (line==NULL) { line=(char*)malloc(size); if (line) line[0]=0; }
      else line=(char*)realloc(line,size);
      if (line==NULL) nrerror("Realloc failed");
    }
    strcat(line+len, s); len=l;
  } while(cp==NULL);

  return line;
}

/*------------------------------------------------------------*/
void read_parameter_file(const char *filename) {
  char        *line, ident[256];
  int         r;

  if (!(fp=fopen(filename,"r"))) { fprintf(stderr,"Can't open '%s'. Using default parameters instead.\n", filename); return; }
  if (!(line = get_line(fp))) { fprintf(stderr,"Parameter file '%s' is corrupted.\n", filename); fclose(fp); return; }
  if (strncmp(line,"## RNAfold parameter file v2.0",30)!=0) { fprintf(stderr, "Missing v2.0 format header missing in '%s'.\n", filename); }
  free(line);

  while((line=get_line(fp))) {
    r = sscanf(line, "# %255s", ident); if (r!=1) { free(line); continue; }
    #define IDENT(X) (strcmp(ident,X) == 0)
    if      IDENT("stack")                           rd_2dim(&(stack37[0][0]),NBPAIRS+1,NBPAIRS+1,1,1);
    else if IDENT("stack_enthalpies")                rd_2dim(&(stackdH[0][0]),NBPAIRS+1,NBPAIRS+1,1,1);
    else if IDENT("hairpin")                         rd_1dim(&(hairpin37[0]),31,0);
    else if IDENT("hairpin_enthalpies")              rd_1dim(&(hairpindH[0]),31,0);
    else if IDENT("bulge")                           rd_1dim(&(bulge37[0]),31,0);
    else if IDENT("bulge_enthalpies")                rd_1dim(&(bulgedH[0]),31,0);
    else if IDENT("interior")                        rd_1dim(&(internal_loop37[0]),31,0);
    else if IDENT("interior_enthalpies")             rd_1dim(&(internal_loopdH[0]),31,0);
    else if IDENT("mismatch_exterior")               rd_3dim(&(mismatchExt37[0][0][0]),NBPAIRS+1,5,5,1,0,0);
    else if IDENT("mismatch_exterior_enthalpies")    rd_3dim(&(mismatchExtdH[0][0][0]),NBPAIRS+1,5,5,1,0,0);
    else if IDENT("mismatch_hairpin")                rd_3dim(&(mismatchH37[0][0][0]),NBPAIRS+1,5,5,1,0,0);
    else if IDENT("mismatch_hairpin_enthalpies")     rd_3dim(&(mismatchHdH[0][0][0]),NBPAIRS+1,5,5,1,0,0);
    else if IDENT("mismatch_interior")               rd_3dim(&(mismatchI37[0][0][0]),NBPAIRS+1,5,5,1,0,0);
    else if IDENT("mismatch_interior_enthalpies")    rd_3dim(&(mismatchIdH[0][0][0]),NBPAIRS+1,5,5,1,0,0);
    else if IDENT("mismatch_interior_1n")            rd_3dim(&(mismatch1nI37[0][0][0]),NBPAIRS+1,5,5,1,0,0);
    else if IDENT("mismatch_interior_1n_enthalpies") rd_3dim(&(mismatch1nIdH[0][0][0]),NBPAIRS+1,5,5,1,0,0);
    else if IDENT("mismatch_interior_23")            rd_3dim(&(mismatch23I37[0][0][0]),NBPAIRS+1,5,5,1,0,0);
    else if IDENT("mismatch_interior_23_enthalpies") rd_3dim(&(mismatch23IdH[0][0][0]),NBPAIRS+1,5,5,1,0,0);
    else if IDENT("mismatch_multi")                  rd_3dim(&(mismatchM37[0][0][0]),NBPAIRS+1,5,5,1,0,0);
    else if IDENT("mismatch_multi_enthalpies")       rd_3dim(&(mismatchMdH[0][0][0]),NBPAIRS+1,5,5,1,0,0);
    else if IDENT("int11")                           rd_4dim(&(int11_37[0][0][0][0]),NBPAIRS+1,NBPAIRS+1,5,5,1,1,0,0);
    else if IDENT("int11_enthalpies")                rd_4dim(&(int11_dH[0][0][0][0]),NBPAIRS+1,NBPAIRS+1,5,5,1,1,0,0);
    else if IDENT("int21")                           rd_5dim(&(int21_37[0][0][0][0][0]),NBPAIRS+1,NBPAIRS+1,5,5,5,1,1,0,0,0);
    else if IDENT("int21_enthalpies")                rd_5dim(&(int21_dH[0][0][0][0][0]),NBPAIRS+1,NBPAIRS+1,5,5,5,1,1,0,0,0);
    else if IDENT("int22")                         { rd_6dim_slice(&(int22_37[0][0][0][0][0][0]),NBPAIRS+1,NBPAIRS+1,5,5,5,5,1,1,1,1,1,1,1,1,0,0,0,0); update_nst(int22_37); }
    else if IDENT("int22_enthalpies")              { rd_6dim_slice(&(int22_dH[0][0][0][0][0][0]),NBPAIRS+1,NBPAIRS+1,5,5,5,5,1,1,1,1,1,1,1,1,0,0,0,0); update_nst(int22_dH); }
    else if IDENT("dangle5")                         rd_2dim(&(dangle5_37[0][0]),NBPAIRS+1,5,1,0);
    else if IDENT("dangle5_enthalpies")              rd_2dim(&(dangle5_dH[0][0]),NBPAIRS+1,5,1,0);
    else if IDENT("dangle3")                         rd_2dim(&(dangle3_37[0][0]),NBPAIRS+1,5,1,0);
    else if IDENT("dangle3_enthalpies")              rd_2dim(&(dangle3_dH[0][0]),NBPAIRS+1,5,1,0);
    else if IDENT("ML_params")                      { int values[6]; rd_1dim(&values[0],6,0);
                                                      ML_BASE37    = values[0]; ML_BASEdH    = values[1];
                                                      ML_closing37 = values[2]; ML_closingdH = values[3];
                                                      ML_intern37  = values[4]; ML_interndH  = values[5]; }
    else if IDENT("NINIO")                          { int values[3]; rd_1dim(&values[0],3,0); ninio37=values[0]; niniodH=values[1]; MAX_NINIO=values[2]; }
    else if IDENT("Triloops")                        rd_Triloop37();
    else if IDENT("Tetraloops")                      rd_Tetraloop37();
    else if IDENT("Hexaloops")                       rd_Hexaloop37();
    else if IDENT("Misc")                           { int values[4]; rd_1dim(&values[0],4,0); DuplexInit37=values[0]; DuplexInitdH=values[1]; TerminalAU37=values[2]; TerminalAUdH=values[3]; }
    else if IDENT("END")                            { free(line); break; }
    else fprintf(stderr,"Unknown field identifier in '%s'\n",line); /* do nothing but complain */
    free(line);
  }
  fclose(fp);
  check_symmetry();
}

/*------------------------------------------------------------*/

static char *get_array1(int *arr,int size) {
  int    i, p, pos, pp, r, last;
  char  *line, buf[16];

  i = last = 0;
  while(i<size) {
    line = get_line(fp); if (!line) nrerror("unexpected end of file in get_array1");
    //ignore single line /* ... */ comment
    char *cp1, *cp2;
    if ((cp1=strstr(line, "/*"))) {
      cp2 = strstr(cp1, "*/"); if (cp2==NULL) nrerror("unclosed comment in parameter file");
      for (cp2+=2;*cp2!=0;cp2++,cp1++) *cp1=*cp2; *cp1=0; /* can't use strcpy for overlapping strings */
    }
    // end
    pos=0;
    while ((i<size)&&(sscanf(line+pos,"%15s%n", buf, &pp)==1)) {
      pos += pp;
      if (buf[0]=='*') { i++; continue; }
      else if (buf[0]=='x') { /* should only be used for loop parameters */
        if (i==0) nrerror("can't extrapolate first value");
        p = arr[last] + (int) (0.5+ lxc37*log(((double) i)/(double)(last)));
      }
      else if (strcmp(buf,"DEF") == 0) p = DEF;
      else if (strcmp(buf,"INF") == 0) p = INF;
      else if (strcmp(buf,"NST") == 0) p = NST;
      else {
        r=sscanf(buf,"%d", &p);
        if (r!=1) return line+pos;
        last = i;
      }
      arr[i++]=p;
    }
    free(line);
  }
  return NULL;
}

/*------------------------------------------------------------*/
static void rd_Tetraloop37(void) {
  int    i=0, r;
  char   *buf;
  memset(&Tetraloops, 0, 281);
  memset(&Tetraloop37, 0, sizeof(int)*40);
  memset(&TetraloopdH, 0, sizeof(int)*40);
  do {
    buf = get_line(fp); if (buf==NULL) break;
    r = sscanf(buf,"%6s %d %d", &Tetraloops[7*i], &Tetraloop37[i], &TetraloopdH[i]);
    strcat(Tetraloops, " "); free(buf); i++;
  } while((r==3)&&(i<40));
}

/*------------------------------------------------------------*/
static void rd_Hexaloop37(void) {
  int    i=0, r;
  char   *buf;
  memset(&Hexaloops, 0, 361);
  memset(&Hexaloop37, 0, sizeof(int)*40);
  memset(&HexaloopdH, 0, sizeof(int)*40);
  do {
    buf = get_line(fp); if (buf==NULL) break;
    r = sscanf(buf,"%8s %d %d", &Hexaloops[9*i], &Hexaloop37[i], &HexaloopdH[i]);
    strcat(Hexaloops, " "); free(buf); i++;
  } while((r==3)&&(i<40));
}

/*------------------------------------------------------------*/
static void rd_Triloop37(void) {
  int    i=0, r;
  char   *buf;
  memset(&Triloops,   0, 241);
  memset(&Triloop37,  0, sizeof(int)*40);
  memset(&TriloopdH,  0, sizeof(int)*40);
  do {
    buf = get_line(fp); if (buf==NULL) break;
    r = sscanf(buf,"%5s %d %d", &Triloops[6*i], &Triloop37[i], &TriloopdH[i]);
    strcat(Triloops, " "); free(buf); i++;
  } while((r==3)&&(i<40));
}
/*---------------------------------------------------------------*/

static void check_symmetry(void) {
  int i,j,k,l,m,n;

  for (i=0; i<=NBPAIRS; i++) for (j=0; j<=NBPAIRS; j++) {
    if (stack37[i][j] != stack37[j][i]) fprintf(stderr, "WARNING: stacking energies not symmetric\n");
    if (stackdH[i][j] != stackdH[j][i]) fprintf(stderr, "WARNING: stacking enthalpies not symmetric\n");
    /* interior 1x1 loops */
    for (k=0; k<5; k++) for (l=0; l<5; l++) {
      if (int11_37[i][j][k][l] != int11_37[j][i][l][k]) fprintf(stderr, "WARNING: int11 energies not symmetric (%d,%d,%d,%d) (%d vs. %d)\n", i, j, k, l, int11_37[i][j][k][l], int11_37[j][i][l][k]);
      if (int11_dH[i][j][k][l] != int11_dH[j][i][l][k]) fprintf(stderr, "WARNING: int11 enthalpies not symmetric\n");

      /* interior 2x2 loops */
      for (m=0; m<5; m++) for (n=0; n<5; n++) {
        if (int22_37[i][j][k][l][m][n] != int22_37[j][i][m][n][k][l]) fprintf(stderr, "WARNING: int22 energies not symmetric\n");
        if (int22_dH[i][j][k][l][m][n] != int22_dH[j][i][m][n][k][l]) fprintf(stderr, "WARNING: int22 enthalpies not symmetric: %d %d %d %d %d %d\n", i,j,k,l,m,n);
      }
    }
  }
}

/* update nonstandard nucleotide/basepair involved contributions for int22 */
#define MAX2(A,B) if (A<B) A=B;
static void update_nst(int array[NBPAIRS+1][NBPAIRS+1][5][5][5][5]){
  int    i, j, k, l, m, n;
  int max, max2, max3, max4, max5, max6;

  for (i=1; i<NBPAIRS; i++) {
    for (j=1; j<NBPAIRS; j++) {
      for (k=1; k<5; k++) {
        for (l=1; l<5; l++) {
          /* get maxima for one nonstandard nucleotide */
          for (m=1; m<5; m++){
            max = max2 = max3 = max4 = -INF; /* max of {CGAU} */
            for(n=1;n<5;n++){
              MAX2(max,   array[i][j][k][l][m][n]);
              MAX2(max2,  array[i][j][k][l][n][m]);
              MAX2(max3,  array[i][j][k][n][l][m]);
              MAX2(max4,  array[i][j][n][k][l][m]);
            }
            array[i][j][k][l][m][0] = max;
            array[i][j][k][l][0][m] = max2;
            array[i][j][k][0][l][m] = max3;
            array[i][j][0][k][l][m] = max4;
          }
          /* get maxima for two nonstandard nucleotides */
          max = max2 = max3 = max4 = max5 = max6 = -INF; /* max of {CGAU} */
          for (m=1; m<5; m++){
            MAX2(max,   array[i][j][k][l][m][0]);
            MAX2(max2,  array[i][j][k][m][0][l]);
            MAX2(max3,  array[i][j][m][0][k][l]);
            MAX2(max4,  array[i][j][0][k][l][m]);
            MAX2(max5,  array[i][j][0][k][m][l]);
            MAX2(max6,  array[i][j][k][0][l][m]);
          }
          array[i][j][k][l][0][0] = max;
          array[i][j][k][0][0][l] = max2;
          array[i][j][0][0][k][l] = max3;
          array[i][j][k][0][l][0] = max6;
          array[i][j][0][k][0][l] = max5;
          array[i][j][0][k][l][0] = max4;
        }
      }
      /* get maxima for three nonstandard nucleotides */
      for (k=1; k<5; k++){
        max = max2 = max3 = max4 = -INF; /* max of {CGAU} */
        for (l=1; l<5; l++){
          /* should be arbitrary where index l resides in last 3 possible locations */
          MAX2(max,   array[i][j][k][l][0][0]);
          MAX2(max2,  array[i][j][0][k][l][0]);
          MAX2(max3,  array[i][j][0][0][k][l]);
          MAX2(max4,  array[i][j][0][0][l][k]);
        }
        array[i][j][k][0][0][0] = max;
        array[i][j][0][k][0][0] = max2;
        array[i][j][0][0][k][0] = max3;
        array[i][j][0][0][0][k] = max4;
      }
      /* get maxima for 4 nonstandard nucleotides */
      max = -INF; /* max of {CGAU} */
      for (k=1; k<5; k++) MAX2(max,array[i][j][k][0][0][0]);
      array[i][j][0][0][0][0] = max;
    }
  }
  /* now compute contributions for nonstandard base pairs ... */
  /* first, 1 nonstandard bp */
  for (i=1; i<NBPAIRS; i++){
    for (k=0; k<5; k++) for (l=0; l<5; l++) for (m=0; m<5; m++) for(n=0;n<5;n++) {
      max = max2 = -INF;
      for(j=1;j<NBPAIRS;j++) {
        MAX2(max, array[i][j][k][l][m][n]);
        MAX2(max2, array[j][i][k][l][m][n]);
      }
      array[i][NBPAIRS][k][l][m][n] = max;
      array[NBPAIRS][i][k][l][m][n] = max2;
    }
  }
  /* now 2 nst base pairs */
  for (k=0; k<5; k++) for (l=0; l<5; l++) for (m=0; m<5; m++) for(n=0;n<5;n++) {
    max = -INF; for(j=1;j<NBPAIRS;j++) MAX2(max, array[NBPAIRS][j][k][l][m][n]);
    array[NBPAIRS][NBPAIRS][k][l][m][n] = max;
  }
}
