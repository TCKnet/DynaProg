#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <jni.h>

// Definition of the classname, spread over all JNI functions
#define _fun(name) Java_librna_LibRNA_00024_##name
#define _vfun(name) JNIEXPORT void JNICALL Java_librna_LibRNA_00024_##name
#define _ifun(name) JNIEXPORT jint JNICALL Java_librna_LibRNA_00024_##name
#define _p0 JNIEnv *env, jobject obj

// Definition of the JNI interface
#ifdef __cplusplus
extern "C" {
#endif
_vfun(setParams)(_p0, jstring);
_vfun(setSequence)(_p0, jstring);
_vfun(clear)(_p0);
JNIEXPORT jstring JNICALL _fun(getConsts)(_p0);

_ifun(termau_1energy)(_p0, jint, jint);
_ifun(hl_1energy)(_p0, jint, jint);
_ifun(hl_1energy_1stem)(_p0, jint, jint);
_ifun(il_1energy)(_p0, jint, jint, jint, jint);
_ifun(bl_1energy)(_p0, jint, jint, jint, jint, jint);
_ifun(br_1energy)(_p0, jint, jint, jint, jint, jint);
_ifun(sr_1energy)(_p0, jint, jint);
_ifun(sr_1pk_1energy)(_p0, jbyte, jbyte, jbyte, jbyte);
_ifun(dl_1energy)(_p0, jint, jint);
_ifun(dr_1energy)(_p0, jint, jint);
_ifun(dli_1energy)(_p0, jint, jint);
_ifun(dri_1energy)(_p0, jint, jint);
_ifun(ext_1mismatch_1energy)(_p0, jint, jint);
_ifun(ml_1mismatch_1energy)(_p0, jint, jint);
_ifun(ml_1energy)(_p0);
_ifun(ul_1energy)(_p0);
_ifun(sbase_1energy)(_p0);
_ifun(ss_1energy)(_p0, jint, jint);
_ifun(dl_dangle_dg)(_p0, jbyte, jbyte, jbyte);
_ifun(dr_dangle_dg)(_p0, jbyte, jbyte, jbyte);

JNIEXPORT jdouble JNICALL _fun(pk_pf)(_p0, jdouble);
JNIEXPORT jdouble JNICALL _fun(scale)(_p0, jint);
JNIEXPORT jboolean JNICALL _fun(iupac_match)(_p0, jbyte, jbyte);
#ifdef __cplusplus
}
#endif

static char* c_seq = NULL;
static int c_len = 0;

#define my_len c_len
#define my_seq c_seq
#define my_P c_P
#define my_dev
#include "librna_impl.h"

void _fun(setParams)(_p0, jstring file) {
  const char *str = (*env)->GetStringUTFChars(env, file, 0);
  if (str) read_parameter_file(str);
  (*env)->ReleaseStringUTFChars(env, file, str);
}

void _fun(setSequence)(_p0, jstring sequence) {
  c_P=get_scaled_parameters();
  const char *str = (*env)->GetStringUTFChars(env, sequence, 0);
  c_len=strlen(str); if (c_seq) free(c_seq); c_seq=malloc((c_len+1)*sizeof(char));
  if (c_seq==NULL) { fprintf(stderr,"Sequence memory allocation error.\n"); exit(EXIT_FAILURE); }
  strncpy(c_seq,str,c_len);
  /*
  size_t i;
  for (i=0;i<c_len;++i) switch(str[i]) {
    case 'a': c_seq[i]=1; break;
    case 'c': c_seq[i]=2; break;
    case 'g': c_seq[i]=3; break;
    case 'u': c_seq[i]=4; break;
    default:
      fprintf(stderr,"Bad character '%c' (%d) in the provided sequence.\n",str[i],str[i]);
      exit(EXIT_FAILURE);
  }
  c_seq[c_len]=0;
  */
  (*env)->ReleaseStringUTFChars(env, sequence, str);
}

jstring _fun(getConsts)(_p0) {
  c_P=get_scaled_parameters();
  char buf[2048];
  snprintf(buf,2048,
    "#define my_LOG logf\n"
    "#define my_LXC %d.%04d\n"
    "#define my_HAIRPIN_MAX %d\n"
    "#define my_TERM_AU %d\n"
    "#define my_NINIO0 %d\n"
    "#define my_NINIO1 %d\n"
    "#define my_INTERNA_LOOP5 %d\n"
    "#define my_ML_CLOSING %d\n"
    "#define my_ML_INTERN0 %d\n"
    "#define my_TEMPERATURE %d.%d\n",
      (int)c_P->p0.lxc,(int)(c_P->p0.lxc*10000)%10000,
      c_P->p0.hairpin[MAXLOOP],
      c_P->p0.TerminalAU,
      c_P->p0.ninio[0],
      c_P->p0.ninio[1],
      c_P->p0.internal_loop[5],
      c_P->p0.MLclosing,
      c_P->p0.MLintern[0],
      (int)c_P->p0.temperature,(int)(c_P->p0.temperature*10000)%10000
    );
  free(c_P);
  return (jstring)(*env)->NewStringUTF(env,buf);
}

void _fun(clear)(_p0) { if (c_P) { free(c_P); c_P=NULL; } if (c_seq) { free(c_seq); c_seq=NULL; } }
jint _fun(termau_1energy)(_p0, jint i, jint j) { return termau_energy(i,j); }
jint _fun(hl_1energy)(_p0, jint i, jint j) { return hl_energy(i,j); }
jint _fun(hl_1energy_1stem)(_p0, jint i, jint j) { return hl_energy_stem(i,j); }
jint _fun(il_1energy)(_p0, jint i, jint k, jint l, jint j) { return il_energy(i,k,l,j); }
jint _fun(bl_1energy)(_p0, jint i, jint k, jint l, jint j, jint Xright) { return bl_energy(i,k,l,j,Xright); }
jint _fun(br_1energy)(_p0, jint i, jint k, jint l, jint j, jint Xleft) { return br_energy(i,k,l,j,Xleft); }
jint _fun(sr_1energy)(_p0, jint i, jint j) { return sr_energy(i,j); }
jint _fun(sr_1pk_1energy)(_p0, jbyte a, jbyte b, jbyte c, jbyte d) { return sr_pk_energy(a,b,c,d); }
jint _fun(dl_1energy)(_p0, jint i, jint j) { return dl_energy(i,j); }
jint _fun(dr_1energy)(_p0, jint i, jint j) { return dr_energy(i,j); }
jint _fun(dli_1energy)(_p0, jint i, jint j) { return dli_energy(i,j); }
jint _fun(dri_1energy)(_p0, jint i, jint j) { return dri_energy(i,j); }
jint _fun(ext_1mismatch_1energy)(_p0, jint i, jint j) { return ext_mismatch_energy(i,j); }
jint _fun(ml_1mismatch_1energy)(_p0, jint i, jint j) { return ml_mismatch_energy(i,j); }
jint _fun(ml_1energy)(_p0) { return ml_energy(); }
jint _fun(ul_1energy)(_p0) { return ul_energy(); }
jint _fun(sbase_1energy)(_p0) { return sbase_energy(); }
jint _fun(ss_1energy)(_p0, jint i, jint j) { return ss_energy(i,j); }
jint _fun(dl_dangle_dg)(_p0, jbyte dangle, jbyte i, jbyte j) { return dl_dangle_dg(dangle,i,j); }
jint _fun(dr_dangle_dg)(_p0, jbyte i, jbyte j, jbyte dangle) { return dl_dangle_dg(i,j,dangle); }

jdouble _fun(mk_pf)(_p0, jdouble x) { return mk_pf(x); }
jdouble _fun(scale)(_p0, jint x) { return scale(x); }
jboolean _fun(iupac_match)(_p0, jbyte base, jbyte iupac_base) { return iupac_match(base,iupac_base); };
