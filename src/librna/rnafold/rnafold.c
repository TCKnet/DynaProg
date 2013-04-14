/* Interface for MFE and Partition function folding of single linear RNA molecules. */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include <unistd.h>
#include <string.h>
#include "fold.h"

#define VRNA_INPUT_ERROR                  1U
#define VRNA_INPUT_QUIT                   2U
#define VRNA_INPUT_FASTA_HEADER           8U
#define VRNA_INPUT_SEQUENCE               16U

#define space(S) calloc(1,(S))

void nrerror(const char message[]);

static char *inbuf = NULL;

/*-------------------------------------------------------------------------*/

void nrerror(const char message[]) { fprintf(stderr, "ERROR: %s\n", message); exit(EXIT_FAILURE); }

static void *xrealloc(void *p, unsigned size) {
  if (p == 0) return space(size);
  p = (void *) realloc(p, size);
  if (p == NULL) {
    fprintf(stderr,"xrealloc: requested size: %d\n", size);
    nrerror("xrealloc allocation failure -> no memory");
  }
  return p;
}

/*------------------------------------------------------------------------*/

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
      line = (char *) xrealloc(line, size*sizeof(char));
    }
    strcat(line+len, s);
    len=l;
  } while(cp==NULL);

  return line;
}

static unsigned int get_multi_input_line(char **string, unsigned int option) {
  char  *line;
  int   i, l;
  int   state = 0;
  int   str_length = 0;

  line = (inbuf) ? inbuf : get_line(stdin);
  inbuf = NULL;
  do {
    /* read lines until informative data appears or report an error if anything goes wrong */
    if(!line) return VRNA_INPUT_ERROR;
    l = (int)strlen(line);

    /* eliminate whitespaces at the end of the line read */
    for(i = l-1; i >= 0; i--){
      if      (line[i] == ' ')  continue;
      else if (line[i] == '\t') continue;
      else                      break;
    }
    line[(i >= 0) ? (i+1) : 0] = '\0';

    l           = (int)strlen(line);
    str_length  = (*string) ? (int) strlen(*string) : 0;

    switch(*line) {
      case  '@':    /* user abort */
                    if(state) inbuf = line;
                    else      free(line);
                    return (state==1) ? VRNA_INPUT_SEQUENCE : VRNA_INPUT_QUIT;
      case  '\0': break; /* empty line */
      case  '#': case  '%': case  ';': case  '/': case  '*': case ' ': break; /* comments */
      case  '>':    /* fasta header */
                    if(state) inbuf   = line;
                    else      *string = line;
                    return (state==1) ? VRNA_INPUT_SEQUENCE : VRNA_INPUT_FASTA_HEADER;
      case  '<': case  '.': case  '|': case  '(': case ')': case '[': case ']': case '{': case '}': case ',':
                    /* seems to be a structure or a constraint */
                    /* either we concatenate this line to one that we read previously */
                    if(option & VRNA_INPUT_FASTA_HEADER){
                      if(state == 1) { inbuf = line; return VRNA_INPUT_SEQUENCE; }
                      else {
                        *string = (char *)xrealloc(*string, sizeof(char) * (str_length + l + 1));
                        strcpy(*string + str_length, line); state = 2;
                      }
                    }
      default:      if(option & VRNA_INPUT_FASTA_HEADER) { /* are we already in sequence mode? */
                        *string = (char *)xrealloc(*string, sizeof(char) * (str_length + l + 1));
                        strcpy(*string + str_length, line); state = 1;
                    } else { *string = line; return VRNA_INPUT_SEQUENCE; }
    }
    free(line);
    line = get_line(stdin);
  } while(line);
  return (state==1) ? VRNA_INPUT_SEQUENCE : VRNA_INPUT_ERROR;
}

/*--------------------------------------------------------------------------*/

int main(int argc, char *argv[]){
  char          *rec_sequence, *structure;
  double        min_en;

  rec_sequence = structure = NULL;

  /* initialization */
  if (argc>1) {
    read_parameter_file(argv[1]);
    // printf("Params: '%s'\n",argv[1]);
  }

  /* main loop: continue until end of file */
  while (!(get_multi_input_line(&rec_sequence, 0) & (VRNA_INPUT_ERROR | VRNA_INPUT_QUIT) )) {
    /* init everything according to the data we've read */
    int length  = (int)strlen(rec_sequence);
    structure = (char *)space(sizeof(char) *(length+1));
    /* actual computations */
    min_en = fold_par(rec_sequence, structure);
    printf("%s\n%s (%6.2f)\n", rec_sequence, structure, min_en);
    fflush(stdout);
    /* clean up */
    free(rec_sequence); free(structure);
    rec_sequence = structure = NULL;
  }
  return EXIT_SUCCESS;
}
