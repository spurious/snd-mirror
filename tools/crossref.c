/* gcc -o crossref crossref.c -g3 */

#include <ctype.h>
#include <stddef.h>
#include <math.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include <stdbool.h>


char **names;
char **hnames;
char **files;
char **headers;
int **counts;
char ***lines;
#define MAX_LINES 16
int *voids;
int *results;
int *procs;

int names_size=0,names_ctr=0;
int files_size=0,files_ctr=0;
int headers_size=0,headers_ctr=0;
int snd_xen_c = -1;
int snd_noxen_c = -1;
int snd_nogui_c = -1;

char *copy_string (char *str)
{
  char *newstr = NULL;
  if (str)
    {
      newstr = (char *)calloc(strlen(str)+1,sizeof(char));
      strcpy(newstr,str);
    }
  return(newstr);
}

void add_header(char *name)
{
  headers[headers_ctr++] = copy_string(name);
  if (headers_ctr == headers_size) fprintf(stderr,"oops headers");
}

int add_name(char *name, char *hdr)
{
  int i;
  if (name == NULL) return(-1);
  if ((isdigit(name[0])) || (strlen(name) == 1)) return(-1);
  if ((strlen(name) > 2) && ((strncmp(name, "SCM", 3) == 0) ||
			     (strncmp(name, "scm", 3) == 0) ||
			     (strncmp(name, "rb_", 3) == 0) ||
			     (strncmp(name, "gh_", 3) == 0) ||
			     (strncmp(name, "GDK", 3) == 0) ||
			     (strncmp(name, "XK_", 3) == 0) ||
			     (strncmp(name, "XEN", 3) == 0) ||
			     (strncmp(name, "xen", 3) == 0)))

    return(-1);
  for (i=0;i<names_ctr;i++) if (strcmp(names[i],name) == 0) return(-1);
  hnames[names_ctr] = hdr;
  names[names_ctr++] = name;
  if (names_ctr == names_size) fprintf(stderr,"oops names");
  return(names_ctr - 1);
}

void add_file(char *name)
{
  if (strcmp(name,"snd-xen.c") == 0) snd_xen_c = files_ctr;
  if (strcmp(name,"snd-nogui.c") == 0) snd_nogui_c = files_ctr;
  files[files_ctr++] = copy_string(name);
  if (files_ctr == files_size) fprintf(stderr,"oops files");
}

static int add_count(char *name, int curfile)
{
  int i;
  for (i=0;i<names_ctr;i++)
    if (strcmp(names[i],name) == 0)
      {
	if (counts[i] == NULL) counts[i] = (int *)calloc(files_size,sizeof(int));
	counts[i][curfile] += 1;
	return(i);
      }
  return(-1);
}

#define MAX_CHARS 1048576
static char *get_call(char *input, int input_loc, int curname_len, char *curname, int chars)
{
  int start = 0, end = 0, i;
  for (i = input_loc - curname_len; i >= 0; i--)
    if (input[i] == '\n')
      {
	start = i+1;
	break;
      }
  if (start == 0)
    fprintf(stderr, "%s at %d found no begin cr\n", curname, input_loc);
  for (i = input_loc; i < chars; i++)
    {
      if (input[i] == ')')
	{
	  end = i+1;
	  break;
	}
      if (input[i] == '\n')
	{
	  end = i;
	  break;
	}
    }
  if (end == 0)
    fprintf(stderr,"%s at %d found no end\n", curname, input_loc);
  if ((start != 0) && (end > start))
    {
      char *value;
      int n, k;
      value = (char *)calloc(end - start + 1, sizeof(char));
      for (n = start, k = 0; n < end; n++, k++)
	value[k] = input[n];
      return(value);
    }
  return(NULL);
}

static int get_result(char *input, int input_loc, int curname_len)
{
  int i;
  for (i = input_loc - curname_len; i >= 0; i--)
    {
      if ((input[i] == '=') || (input[i] == '(') || (input[i] == ',')) return(1);
      if ((input[i] == '+') || (input[i] == '-') || (input[i] == '*') || (input[i] == '/')) return(1);
      if ((input[i] == '&') || (input[i] == '|') || (input[i] == '^') || (input[i] == '!') || (input[i] == '?')) return(1);
      if ((input[i] == ';') || (input[i] == '{')) return(0);
      if ((input[i] == ')') && (input[i-1] != '*')) return(0);
    }
  return(1);
}

typedef struct {
  char *name, *hname;
  int i, calls, v, results, proc;
} qdata;

static int greater_compare(const void *a, const void *b)
{
  qdata *d1 = *(qdata **)a;
  qdata *d2 = *(qdata **)b;
  if (d1->calls > d2->calls) 
    return(1); 
  else 
    {
      if (d1->calls == d2->calls) 
	return(0); 
      else return(-1);
    }
}

int main(int argc, char **argv)
{
  int i,j,fd,curfile,chars,k,in_comment=0,in_white = 0, calls=0, in_parens=0, in_quotes=0, in_define=0, in_curly=0;
  int maxc[8192],maxf[8192],maxg[8192], mcalls[8192];
  qdata **qs;
  char input[MAX_CHARS];
  char curname[128];
  FILE *FD;
  
  names_size = 8192;
  names = (char **)calloc(names_size,sizeof(char *));
  hnames = (char **)calloc(names_size,sizeof(char *));
  voids = (int *)calloc(names_size,sizeof(int));
  files_size = 128;
  files = (char **)calloc(files_size,sizeof(char *));
  headers_size = 32;
  headers = (char **)calloc(headers_size,sizeof(char *));
  counts = (int **)calloc(names_size,sizeof(int *));
  lines = (char ***)calloc(names_size, sizeof(char **));
  results = (int *)calloc(names_size, sizeof(int));
  procs = (int *)calloc(names_size, sizeof(int));

  add_header("sndlib.h");
  add_header("clm.h");
  add_header("vct.h");
  add_header("sndlib2xen.h");
  add_header("clm2xen.h");
  add_header("snd.h");
#if 0
  add_header("snd-strings.h");
  add_header("sndlib-strings.h");
  add_header("clm-strings.h");
#endif
  add_header("snd-0.h");
  add_header("snd-1.h");
  add_header("snd-x0.h");
  add_header("snd-x1.h");
  add_header("snd-g0.h");
  add_header("snd-g1.h");
  add_header("snd-nogui0.h");
  add_header("snd-nogui1.h");
  add_header("snd-rec.h");
  add_header("xen.h");
  add_header("config.h.in");
  add_header("libclm.def");
  add_header("snd-menu.h");
  add_header("snd-file.h");

  /* add_file("xen.h"); */
  /* add_file("snd.h"); */

  add_file("headers.c");
  add_file("audio.c");
  add_file("io.c");
  add_file("sound.c");
  add_file("clm.c");
  add_file("vct.c");
  add_file("cmus.c");
  add_file("sndlib2xen.c");
  add_file("clm2xen.c");
  add_file("midi.c");
  add_file("snd-io.c");
  add_file("snd-utils.c");
  add_file("snd-error.c");
  add_file("snd-completion.c");
  add_file("snd-menu.c");
  add_file("snd-axis.c");
  add_file("snd-data.c");
  add_file("snd-draw.c");
  add_file("snd-fft.c");
  add_file("snd-marks.c");
  add_file("snd-file.c");
  add_file("snd-edits.c");
  add_file("snd-chn.c");
  add_file("snd-sig.c");
  add_file("snd-kbd.c");
  add_file("snd-dac.c");
  add_file("snd-region.c");
  add_file("snd-run.c");
  add_file("snd-select.c");
  add_file("snd-find.c");
  add_file("snd-snd.c");
  add_file("snd-help.c");
  add_file("snd-main.c");
  add_file("snd-print.c");
  add_file("snd-trans.c");
  add_file("snd-mix.c");
  add_file("snd.c");
  add_file("snd-rec.c");
  add_file("snd-env.c");
  add_file("snd-xen.c");
  add_file("snd-ladspa.c");
  add_file("snd-xutils.c");
  add_file("snd-xhelp.c");
  add_file("snd-xfind.c");
  add_file("snd-xmenu.c");
  add_file("snd-xdraw.c");
  add_file("snd-xlistener.c");
  add_file("snd-listener.c");
  add_file("snd-xchn.c");
  add_file("snd-xsnd.c");
  add_file("snd-xdac.c");
  add_file("snd-xregion.c");
  add_file("snd-xdrop.c");
  add_file("snd-xmain.c");
  add_file("snd-xmix.c");
  add_file("snd-xrec.c");
  add_file("snd-xenv.c");
  add_file("snd-gxutils.c");
  add_file("snd-gxbitmaps.c");
  add_file("snd-gxcolormaps.c");
  add_file("snd-xfft.c");
  add_file("snd-xprint.c");
  add_file("snd-xfile.c");
  add_file("snd-xxen.c");
  add_file("snd-gutils.c");
  add_file("snd-ghelp.c");
  add_file("snd-gfind.c");
  add_file("snd-gmenu.c");
  add_file("snd-gdraw.c");
  add_file("snd-glistener.c");
  add_file("snd-gchn.c");
  add_file("snd-gsnd.c");
  add_file("snd-gdac.c");
  add_file("snd-gregion.c");
  add_file("snd-gdrop.c");
  add_file("snd-gmain.c");
  add_file("snd-gmix.c");
  add_file("snd-grec.c");
  add_file("snd-genv.c");
  add_file("snd-gxutils.c");
  add_file("snd-gfft.c");
  add_file("snd-gprint.c");
  add_file("snd-gfile.c");
  add_file("snd-gxen.c");
  add_file("snd-nogui.c");
  add_file("xen.c");
  add_file("xm.c");
  add_file("gl.c");
  add_file("xg.c");
  add_file("xm-ruby.c");
  add_file("gl-ruby.c");
  add_file("xg-ruby.c");

  /*
  add_file("cmus.c");
  add_file("sc.c");
  */
  add_file("ffi.lisp");
  add_file("sndlib2clm.lisp");

  for (i=0;i<headers_ctr;i++)
    {
      k = 0;
      in_quotes = 0;
      in_white = 0;
      in_parens = 0;
      in_comment = 0;
      fd = open(headers[i],O_RDONLY,0);
      do 
	{
	  chars = read(fd,input,MAX_CHARS);
	  for (j = 0; j < chars; j++)
	    {
	      if ((in_comment == 0) && (in_define == 0) && (in_curly == 0))
		{
		  if ((isalpha(input[j])) || (isdigit(input[j])) || (input[j] == '_'))
		    {
		      in_white = 0;
		      curname[k++] = input[j];
		    }
		  else
		    {
		      in_white = 1;
		      curname[k] = 0;
		      if ((k > 0) && (in_parens == 0) && (in_quotes == 0))
			{
			  int loc;
			  loc = add_name(copy_string(curname), headers[i]);
			  if (loc >= 0)
			    {
			      int start, n, maybe_proc = 1;
			      for (n = 0; n < k; n++)
				if (isupper(curname[n]))
				  {
				    maybe_proc = 0;
				    break;
				  }
			      if ((maybe_proc) && ((input[j] == '(') || ((input[j] == ' ') && (input[j+1] == '('))))
				procs[loc] = maybe_proc;
			      else procs[loc] = 0;
			      start = j - strlen(curname) - 6;
			      if (start >= 0)
				{
				  int m;
				  for (m = 0; m < 3; m++)
				    if (strncmp((char *)(input + start + m), "void", 4) == 0)
				      {
					voids[loc] = 1;
					break;
				      }
				}
			    }
			}
		      /* else if (k > 0) fprintf(stderr,"drop %s %d %d\n",curname,in_parens, in_quotes); */
		      k = 0;
		      if ((input[j] == '/') && (input[j+1] == '*'))
			in_comment = 1;
		      else 
			{
			  if (input[j] == '#')
			    in_define = 1;
			  else
			    {
			      if (input[j] == '{')
				in_curly = 1;
			      else
				{
				  if (input[j] == '(') in_parens++;
				  if (input[j] == ')') in_parens--;
				  if (input[j] == '"')
				    {
				      if (in_quotes == 1)
					in_quotes = 0;
				      else in_quotes = 1;
				    }
				}
			    }
			}
		    }
		}
	      else
		{
		  if ((input[j] == '*') && (input[j+1] == '/'))
		    in_comment = 0;
		  else 
		    {
		      if (input[j] == '\n')
			in_define = 0;
		      else
			{
			  if (input[j] == '}')
			    in_curly = 2;
			  else
			    {
			      if (input[j] == ';')
				in_curly = 0;
			    }
			}
		    }
		}
	    }
	}
      while (chars == MAX_CHARS);
      close(fd);
    }
  fprintf(stderr,"%d names ",names_ctr);
  k=0;
  in_comment=0;
  in_white=0;
  for (i=0;i<files_ctr;i++)
    {
      k = 0;
      fd = open(files[i],O_RDONLY,0);
      do 
	{
	  chars = read(fd,input,MAX_CHARS);
	  for (j=0;j<chars;j++)
	    {
	      if (in_comment == 0)
		{
		  if ((isalpha(input[j])) || (isdigit(input[j])) || (input[j] == '_'))
		    {
		      curname[k++] = input[j];
		    }
		  else
		    {
		      if ((input[j] == '/') && (input[j+1] == '*'))
			in_comment = 1;
		      if (k>0)
			{
			  curname[k] = 0;
			  if (k<128)
			    {
			      int loc;
			      loc = add_count(curname,i);
			      if (loc >= 0)
				{
				  if (procs[loc])
				    results[loc] += get_result(input, j, k);
				  if (lines[loc] == NULL)
				    {
				      lines[loc] = (char **)calloc(MAX_LINES, sizeof(char *));
				      lines[loc][0] = get_call(input, j, k, curname, chars);
				    }
				  else
				    {
				      int m;
				      for (m = 0; m < MAX_LINES; m++)
					if (lines[loc][m] == NULL)
					  {
					    lines[loc][m] = get_call(input, j, k, curname, chars);
					    break;
					  }
				    }
				}
			    }
			  k=0;
			}
		    }
		}
	      else
		{
		  if ((input[j] == '*') && (input[j+1] == '/'))
		    in_comment = 0;
		}
	    }
	}
      while (chars == MAX_CHARS);
      close(fd);
    }
  FD = fopen("xref.data","w");
  for (i=0;i<names_ctr;i++)
    {
      maxc[i] = 0;
      maxf[i] = 0;
      maxg[i] = 0;
      for (j=0;j<files_ctr;j++)
	if ((counts[i]) && (counts[i][j] > 0)) 
	  {
	    maxc[i] += counts[i][j]; 
	    maxf[i]++;
	    if ((j == snd_xen_c) || (j == snd_nogui_c)) maxg[i]++;
	  }
    }
  for (i=0;i<names_ctr;i++)
    {
      calls = 0;
      if (counts[i])
	for (j=0;j<files_ctr;j++)
	  calls += counts[i][j];
      mcalls[i]=calls;
    }
  qs = (qdata **)calloc(8192, sizeof(qdata *));
  for (i = 0; i < names_ctr; i++)
    {
      qdata *q;
      q = calloc(1, sizeof(qdata));
      qs[i] = q;
      q->i = i;
      q->v = voids[i];
      q->name = names[i];
      q->hname = hnames[i];
      q->calls = mcalls[i];
      q->results = results[i];
      q->proc = procs[i];
    }
  qsort((void *)qs, names_ctr, sizeof(qdata *), greater_compare);
  for (i=0; i< names_ctr; i++)
    {
      bool menu_case = false, file_case = false, rec_case = false;
      int menu_count = 0, file_count = 0, rec_count = 0;
      int nfiles;
      nfiles = 0;
      fprintf(FD, "\n\n%s: %d [%s]", qs[i]->name, qs[i]->calls, qs[i]->hname);
      if (qs[i]->v) 
	{
	  fprintf(FD, " (void)");
	}
      else
	{
	  if ((qs[i]->results == 0) && (qs[i]->proc > 0) && (qs[i]->calls > 0) &&
	      (strncmp(qs[i]->name, "set_", 4) != 0) &&
	      (strncmp(qs[i]->name, "in_set_", 7) != 0))
	    fprintf(FD, " (not void but result not used?)");
	}
      menu_case = (strcmp(qs[i]->hname, "snd-menu.h") != 0);
      file_case = (strcmp(qs[i]->hname, "snd-file.h") != 0);
      rec_case = (strcmp(qs[i]->hname, "snd-rec.h") != 0);
      menu_count =0;
      file_count = 0;
      rec_count = 0;
      for (j=0;j<files_ctr;j++)
	{
	  if ((counts[qs[i]->i]) && (counts[qs[i]->i][j] > 0))
	    {
	      if (menu_case)
		{
		  if ((strcmp(files[j], "snd-menu.c") != 0) &&
		      (strcmp(files[j], "snd-xmenu.c") != 0) &&
		      (strcmp(files[j], "snd-gmenu.c") != 0))
		    {
		      if (strcmp(files[j], "snd-nogui.c") != 0)
			menu_case = false;
		    }
		  else menu_count++;
		}
	      if (file_case)
		{
		  if ((strcmp(files[j], "snd-file.c") != 0) &&
		      (strcmp(files[j], "snd-xfile.c") != 0) &&
		      (strcmp(files[j], "snd-gfile.c") != 0))
		    {
		      if (strcmp(files[j], "snd-nogui.c") != 0)
			file_case = false;
		    }
		  else file_count++;
		}
	      if (rec_case)
		{
		  if ((strcmp(files[j], "snd-rec.c") != 0) &&
		      (strcmp(files[j], "snd-xrec.c") != 0) &&
		      (strcmp(files[j], "snd-grec.c") != 0))
		    {
		      if (strcmp(files[j], "snd-nogui.c") != 0)
			rec_case = false;
		    }
		  else rec_count++;
		}

	      fprintf(FD,"\n    %s: %d",files[j],counts[qs[i]->i][j]);
	      nfiles++;
	    }
	}
      if ((menu_case) && (menu_count > 0)) fprintf(FD, "\n->SND-MENU.H\n");
      if ((file_case) && (file_count > 0)) fprintf(FD, "\n->SND-FILE.H\n");
      if ((rec_case) && (rec_count > 0)) fprintf(FD, "\n->SND-REC.H\n");
      {
	int m;
	if ((nfiles > 0) && (lines[qs[i]->i]))
	  {
	    fprintf(FD, "\n");
	    for (m = 0; m < MAX_LINES; m++)
	      {
		if (lines[qs[i]->i][m] == NULL) break;
		fprintf(FD, "\n        %s", lines[qs[i]->i][m]);
	      }
	  }
      }
      if (nfiles < 2) 
	fprintf(FD, "\n----------------------------------------");
      else fprintf(FD, "\n----------------");
    }
  fclose(FD);
}
