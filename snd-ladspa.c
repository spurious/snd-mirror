/*****************************************************************************/

/* Snd LADSPA Support - Copyright 2000 Richard W.E. Furse. */

/*****************************************************************************/

#include "snd.h"

#if HAVE_LADSPA

#include <dlfcn.h>
#include <ladspa.h>
#include <dirent.h>

/* CHANGES:
 *
 * bil: 1-Aug-03  added direct struct readers for LADSPA_Descriptor
 * bil: 6-Jan-03  use FREE, not free.
 * bil: 21-Nov-02 better checks for C-g interrupt.
 * bil: 2-May-02  use off_t for sample number.
 * bil: 14-Dec-01 various C++ cleanups.
 * bil: 28-Nov-01 input chans need not equal output chans now.
 * bil: 15-Oct-01 added some error returns (rather than snd_error).  multichannel plugin support.
 * bil: 20-Sep-01 changed location of pfInputBuffer to avoid glomming up the stack with a huge array.
 */

#define NO_SUCH_PLUGIN XEN_ERROR_TYPE("no-such-plugin")
#define PLUGIN_ERROR   XEN_ERROR_TYPE("plugin-error")


/*****************************************************************************/

/* FIXME: Repository is not threadsafe. */
/* FIXME: Memory checking is non-existent. */

/*****************************************************************************/

typedef struct {
  char *m_pcPackedFilename;
  const char *m_pcLabel;
  const LADSPA_Descriptor *m_psDescriptor;
  void *m_pvPluginHandle;
} LADSPAPluginInfo;

/*****************************************************************************/

static char g_bLADSPAInitialised = 0;
static LADSPAPluginInfo ** g_psLADSPARepository;
static long g_lLADSPARepositoryCapacity;
static long g_lLADSPARepositoryCount;

#define LADSPA_REPOSITORY_CAPACITY_STEP 100

/*****************************************************************************/

static int lInputCount, lOutputCount;

static void isLADSPAPluginSupported(const LADSPA_Descriptor *psDescriptor) {
  unsigned int lIndex;
  LADSPA_PortDescriptor iPortDescriptor;
  lInputCount = lOutputCount = 0;
  for (lIndex = 0; lIndex < psDescriptor->PortCount; lIndex++) {
    iPortDescriptor = psDescriptor->PortDescriptors[lIndex];
    if (LADSPA_IS_PORT_AUDIO(iPortDescriptor)) {
      if (LADSPA_IS_PORT_INPUT(iPortDescriptor))
	lInputCount++;
      else
	lOutputCount++;
    }
  }
}

/*****************************************************************************/

/* Assumes repository initialised, returns NULL if not found. */
static const LADSPA_Descriptor *findLADSPADescriptor(const char *pcPackedFilename, const char *pcLabel) {

  /* FIXME: Could be using hashtables, binary chops etc. Instead we simply scan the table. */

  long lIndex;
  LADSPAPluginInfo *psInfo;
  for (lIndex = 0; lIndex < g_lLADSPARepositoryCount; lIndex++) {
    psInfo = g_psLADSPARepository[lIndex];
    if (strcmp(pcLabel, psInfo->m_pcLabel) == 0
	&& strcmp(pcPackedFilename, psInfo->m_pcPackedFilename) == 0)
      return psInfo->m_psDescriptor;
  }

  return NULL;
}

/*****************************************************************************/

/* Allocate a new string. The string will contain a library filename,
   stripped of path and .so (if present) */
static char *packLADSPAFilename(const char * pcFilename) {

  const char *pcStart, * pcEnd;
  char *pcPackedFilename;

  /* Move start past last /, move pcEnd to end. */
  pcStart = pcFilename;
  for (pcEnd = pcStart; *pcEnd != '\0'; pcEnd++)
    if (*pcEnd == '/')
      pcStart = pcEnd + 1;
  if (pcEnd - pcStart > 3)
    if (strcmp(".so", pcEnd - 3) == 0)
      pcEnd -= 3;

  pcPackedFilename = (char *)MALLOC(pcEnd - pcStart + 1);
  memcpy(pcPackedFilename, pcStart, pcEnd - pcStart);
  pcPackedFilename[pcEnd - pcStart] = '\0';

  return pcPackedFilename;
}

/*****************************************************************************/

static void unloadLADSPA() {

  long lIndex;
  LADSPAPluginInfo *pvPluginHandle = NULL;
  LADSPAPluginInfo *psInfo = NULL;
  if (g_lLADSPARepositoryCount > 0)
    pvPluginHandle = (LADSPAPluginInfo *)(g_psLADSPARepository[0]->m_pvPluginHandle);
  pvPluginHandle++;
  for (lIndex = 0; lIndex < g_lLADSPARepositoryCount; lIndex++) {
    psInfo = g_psLADSPARepository[lIndex];
    FREE(psInfo->m_pcPackedFilename);
    /* Don't free Label or Descriptor - this memory is owned by the
       relevant plugin library. */
    if (pvPluginHandle != psInfo->m_pvPluginHandle) {
      pvPluginHandle = (LADSPAPluginInfo *)(psInfo->m_pvPluginHandle);
      dlclose(pvPluginHandle);
    }
    FREE(psInfo);
  }

  FREE(g_psLADSPARepository);
  g_bLADSPAInitialised = 0;
}

/*****************************************************************************/

/* Called only from within loadLADSPA->loadLADSPADirectory. */
static void loadLADSPALibrary(void *pvPluginHandle,
			      char *pcFilename,
			      LADSPA_Descriptor_Function fDescriptorFunction) {

  LADSPAPluginInfo **psOldRepository, *psInfo;
  long lNewCapacity, lIndex;
  const LADSPA_Descriptor *psDescriptor;

  for (lIndex = 0;
       (psDescriptor = fDescriptorFunction(lIndex)) != NULL;
       lIndex++)
    {
      if (g_lLADSPARepositoryCount == g_lLADSPARepositoryCapacity) {
	psOldRepository = g_psLADSPARepository;
	lNewCapacity = (g_lLADSPARepositoryCapacity
			+ LADSPA_REPOSITORY_CAPACITY_STEP);
	g_psLADSPARepository = (LADSPAPluginInfo **)MALLOC(lNewCapacity * sizeof(LADSPAPluginInfo *));
	memcpy(g_psLADSPARepository,
	       psOldRepository,
	       sizeof(LADSPAPluginInfo *) * g_lLADSPARepositoryCount);
	g_lLADSPARepositoryCapacity = lNewCapacity;
	FREE(psOldRepository);
      }
      psInfo
	= g_psLADSPARepository[g_lLADSPARepositoryCount++]
	= (LADSPAPluginInfo *)MALLOC(sizeof(LADSPAPluginInfo));
      psInfo->m_pcPackedFilename = packLADSPAFilename(pcFilename);
      psInfo->m_pcLabel = psDescriptor->Label;
      psInfo->m_psDescriptor = psDescriptor;
      psInfo->m_pvPluginHandle = pvPluginHandle;
    }
}

/*****************************************************************************/

/* Search just the one directory. Called only from within
   loadLADSPA. */
static void loadLADSPADirectory(const char *pcDirectory) {

  char *pcFilename = NULL;
  DIR *psDirectory;
  LADSPA_Descriptor_Function fDescriptorFunction;
  long lDirLength;
  long iNeedSlash;
  struct dirent *psDirectoryEntry;
  void *pvPluginHandle;

  lDirLength = strlen(pcDirectory);
  if (!lDirLength)
    return;
  if (pcDirectory[lDirLength - 1] == '/')
    iNeedSlash = 0;
  else
    iNeedSlash = 1;

  psDirectory = opendir(pcDirectory);
  if (!psDirectory)
    return;

  while (1) {

    psDirectoryEntry = readdir(psDirectory);
    if (!psDirectoryEntry) {
      closedir(psDirectory);
      return;
    }

    pcFilename = (char *)MALLOC(lDirLength
				+ strlen(psDirectoryEntry->d_name)
				+ 1 + iNeedSlash);
    strcpy(pcFilename, pcDirectory);
    if (iNeedSlash)
      strcat(pcFilename, "/");
    strcat(pcFilename, psDirectoryEntry->d_name);

    pvPluginHandle = dlopen(pcFilename, RTLD_LAZY);
    if (pvPluginHandle) {
      /* This is a file and the file is a shared library! */

      dlerror();
      fDescriptorFunction
	= (LADSPA_Descriptor_Function)dlsym(pvPluginHandle,
					    "ladspa_descriptor");
      if (dlerror() == NULL && fDescriptorFunction) {
	loadLADSPALibrary(pvPluginHandle, pcFilename, fDescriptorFunction);
      }
      else {
	/* It was a library, but not a LADSPA one. Unload it. */
	/* bil: this is not safe! Could be legit already-loaded library. */
	/* dlclose(pcFilename); */
      }
    }
    if (pcFilename) FREE(pcFilename);
    pcFilename = NULL;
  }
}

/*****************************************************************************/

static void loadLADSPA() {

  char *pcBuffer = NULL;
  const char *pcEnd;
  const char *pcLADSPAPath;
  const char *pcStart;

  g_bLADSPAInitialised = 1;
  g_psLADSPARepository = (LADSPAPluginInfo **)MALLOC(sizeof(LADSPAPluginInfo *)
				* LADSPA_REPOSITORY_CAPACITY_STEP);
  g_lLADSPARepositoryCapacity = LADSPA_REPOSITORY_CAPACITY_STEP;
  g_lLADSPARepositoryCount = 0;

  pcLADSPAPath = ladspa_dir(ss);
  if (!pcLADSPAPath)
    {
      pcLADSPAPath = getenv("LADSPA_PATH");
      if (pcLADSPAPath == NULL) {
	snd_warning(_("Warning: You have not set ladspa-dir or the environment variable LADSPA_PATH."));
	return;
      }
    }

  pcStart = pcLADSPAPath;
  while (*pcStart != '\0') {
    pcEnd = pcStart;
    while (*pcEnd != ':' && *pcEnd != '\0')
      pcEnd++;

    pcBuffer = (char *)MALLOC(1 + pcEnd - pcStart);
    if (pcEnd > pcStart)
      strncpy(pcBuffer, pcStart, pcEnd - pcStart);
    pcBuffer[pcEnd - pcStart] = '\0';

    loadLADSPADirectory(pcBuffer);

    pcStart = pcEnd;
    if (*pcStart == ':')
      pcStart++;

    if (pcBuffer) FREE(pcBuffer);
    pcBuffer = NULL;
  }

  /* FIXME: It might be nice to qsort the data in the repository by
   *  filename+label at this point to provide organised results from
   *  list-ladspa.
   */
}

/*****************************************************************************/

#define S_init_ladspa "init-ladspa"

static XEN g_init_ladspa() {

#define H_init_ladspa "(" S_init_ladspa "): reinitialise LADSPA. This is not \
normally necessary as LADSPA automatically initialises itself, however \
it can be useful when the plugins on the system have changed."

  if (g_bLADSPAInitialised)
    unloadLADSPA();

  loadLADSPA();

  return(XEN_FALSE);
}

/*****************************************************************************/

#define S_list_ladspa "list-ladspa"

static XEN g_list_ladspa() {

#define H_list_ladspa "(" S_list_ladspa "): return a list of lists containing \
information of the LADSPA plugins currently available. For each plugin a \
list containing the plugin-file and plugin-label is included."

  long lIndex;
  XEN xenList, xenPluginList;
  LADSPAPluginInfo *psInfo;

  if (!g_bLADSPAInitialised)
    loadLADSPA();

  xenList = XEN_EMPTY_LIST;

  for (lIndex = g_lLADSPARepositoryCount - 1; lIndex >= 0; lIndex--) {
    psInfo = g_psLADSPARepository[lIndex];
    xenPluginList = XEN_CONS(C_TO_XEN_STRING(psInfo->m_pcPackedFilename),
			     XEN_CONS(C_TO_XEN_STRING((char *)psInfo->m_pcLabel),
				      XEN_EMPTY_LIST));
    xenList = XEN_CONS(xenPluginList, xenList);
  }

  return xenList;
}

/*****************************************************************************/

#define S_analyse_ladspa "analyse-ladspa"

static XEN g_analyse_ladspa(XEN ladspa_plugin_filename,
			    XEN ladspa_plugin_label) {

#define H_analyse_ladspa "(" S_analyse_ladspa " library plugin): return a list of information about \
a LADSPA plugin. The plugin is identified by library and plugin. \
The items are: plugin-name, plugin-maker, \
plugin-copyright, plugin-parameter-list. The plugin-port-list contains a \
list of information for each parameter available. The first item in this \
list is the name of the port. Other hint information may follow this to help \
a user interface edit the parameter in a useful way."

  long lIndex;
  const LADSPA_Descriptor *psDescriptor;
  char *pcFilename, *pcLabel, *pcTmp;
  XEN xenList, xenPortData;
  LADSPA_PortRangeHintDescriptor iHint;

  if (!g_bLADSPAInitialised)
    loadLADSPA();

  XEN_ASSERT_TYPE(XEN_STRING_P(ladspa_plugin_filename),
                  ladspa_plugin_filename,
	          XEN_ARG_1,
	          S_analyse_ladspa, "a string");
  XEN_ASSERT_TYPE(XEN_STRING_P(ladspa_plugin_label),
	          ladspa_plugin_label,
	          XEN_ARG_2,
	          S_analyse_ladspa, "a string");

  /* Plugin. */
  pcTmp = XEN_TO_C_STRING(ladspa_plugin_filename);
  pcLabel = XEN_TO_C_STRING(ladspa_plugin_label);
  pcFilename = packLADSPAFilename(pcTmp);
  psDescriptor = findLADSPADescriptor(pcFilename, pcLabel);
  FREE(pcFilename);

  if (!psDescriptor) {
    XEN_ERROR(NO_SUCH_PLUGIN,
	      XEN_LIST_3(C_TO_XEN_STRING(S_analyse_ladspa),
			 C_TO_XEN_STRING("plugin file: ~A, plugin label: ~A"),
                         XEN_LIST_2(ladspa_plugin_filename,
				    ladspa_plugin_label)));
    return(XEN_FALSE);
  }

  xenList = XEN_EMPTY_LIST;
  for (lIndex = psDescriptor->PortCount - 1; lIndex >= 0; lIndex--)
    if (LADSPA_IS_PORT_CONTROL(psDescriptor->PortDescriptors[lIndex])
	&& LADSPA_IS_PORT_INPUT(psDescriptor->PortDescriptors[lIndex])) {

      iHint = psDescriptor->PortRangeHints[lIndex].HintDescriptor;

      xenPortData = XEN_EMPTY_LIST;
      if (LADSPA_IS_HINT_TOGGLED(iHint))
	xenPortData = XEN_CONS(C_TO_XEN_STRING("toggle"), xenPortData);
      if (LADSPA_IS_HINT_LOGARITHMIC(iHint))
	xenPortData = XEN_CONS(C_TO_XEN_STRING("logarithmic"), xenPortData);
      if (LADSPA_IS_HINT_INTEGER(iHint))
	xenPortData = XEN_CONS(C_TO_XEN_STRING("integer"), xenPortData);
      if (LADSPA_IS_HINT_SAMPLE_RATE(iHint))
	xenPortData = XEN_CONS(C_TO_XEN_STRING("sample_rate"), xenPortData);
      if (LADSPA_IS_HINT_BOUNDED_ABOVE(iHint))
	xenPortData = XEN_CONS(C_TO_XEN_STRING("maximum"),
			       XEN_CONS(C_TO_XEN_DOUBLE(psDescriptor->PortRangeHints[lIndex].UpperBound),
					xenPortData));
      if (LADSPA_IS_HINT_BOUNDED_BELOW(iHint))
	xenPortData = XEN_CONS(C_TO_XEN_STRING("minimum"),
			       XEN_CONS(C_TO_XEN_DOUBLE(psDescriptor->PortRangeHints[lIndex].LowerBound),
					xenPortData));
      xenPortData = XEN_CONS(C_TO_XEN_STRING((char *)psDescriptor->PortNames[lIndex]),
			     xenPortData);
      xenList = XEN_CONS(xenPortData, xenList);
    }

  xenList = XEN_CONS(C_TO_XEN_STRING((char *)psDescriptor->Name),
		     XEN_CONS(C_TO_XEN_STRING((char *)psDescriptor->Maker),
			      XEN_CONS(C_TO_XEN_STRING((char *)psDescriptor->Copyright),
				       XEN_CONS(xenList, XEN_EMPTY_LIST))));
  return xenList;
}

/*****************************************************************************/

/* FIXME: We could improve this function to receive a list of plugin configurations for chain processing. */

#define S_apply_ladspa "apply-ladspa"

static XEN g_apply_ladspa(XEN reader,
			  XEN ladspa_plugin_configuration,
			  XEN samples,
			  XEN origin)
{
#define H_apply_ladspa "(" S_apply_ladspa " reader (list library plugin pars) dur origin): apply a LADSPA plugin to process a \
sound. The parameters are soundfile-reader, a ladspa-plugin-configuration, \
the number of samples to process, and an `origin' for edit lists. The \
ladspa-plugin-configuration is a list containing the plugin-file and \
plugin-label for the LADSPA plugin, as provided by list-ladspa, followed \
by any arguments. The reader argument can also be a list of readers. \
Information about about parameters can be acquired using analyse-ladspa."

  const LADSPA_Descriptor *psDescriptor;
  char *pcFilename, *pcLabel, *pcTmp;
  LADSPA_Handle *psHandle;
  unsigned long lSampleRate, lPortIndex, lBlockSize, lSampleIndex;
  off_t lAt;
  unsigned long lParameterCount;
  XEN xenParameters;
  LADSPA_PortDescriptor iPortDescriptor;
  LADSPA_Data *pfControls = NULL;
  chan_info *cp, *ncp;
  snd_info *sp;
  char *ofile, *msg;
  int i, j, ofd, datumb, err = 0, inchans = 1, readers = 1, outchans = 1;
  off_t num;
  snd_fd **sf;
  file_info *hdr;
  XEN errmsg;
  mus_sample_t **data;
  LADSPA_Data **pfInputBuffer = NULL;
  LADSPA_Data **pfOutputBuffer = NULL;

  if (!g_bLADSPAInitialised)
    loadLADSPA();

  /* First parameter should be a file reader or list thereod. */
  XEN_ASSERT_TYPE(sf_p(reader) || XEN_LIST_P(reader),
		  reader,
		  XEN_ARG_1,
		  S_apply_ladspa, "a sample-reader or a list of readers");
  if (XEN_LIST_P(reader)) readers = XEN_LIST_LENGTH(reader);

  /* Second parameter should be a list of two strings, then any number
     (inc 0) of numbers. */
  if ((XEN_LIST_LENGTH(ladspa_plugin_configuration) < 2) ||
      (!(XEN_STRING_P(XEN_CAR(ladspa_plugin_configuration)))) ||
      (!(XEN_STRING_P(XEN_CADR(ladspa_plugin_configuration)))))
    XEN_ASSERT_TYPE(0, ladspa_plugin_configuration, XEN_ARG_2, S_apply_ladspa, "a list of 2 or more strings");

  /* Third parameter is the number of samples to process. */
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samples),
		  samples,
		  XEN_ARG_3,
		  S_apply_ladspa, "a number");
  /* The fourth parameter is a tag to identify the edit. */
  XEN_ASSERT_TYPE(XEN_STRING_P(origin),
		  origin,
		  XEN_ARG_4,
		  S_apply_ladspa, "a string");

  /* Plugin. */
  pcTmp = XEN_TO_C_STRING(XEN_CAR(ladspa_plugin_configuration));
  pcLabel = XEN_TO_C_STRING(XEN_CADR(ladspa_plugin_configuration));
  pcFilename = packLADSPAFilename(pcTmp);
  psDescriptor = findLADSPADescriptor(pcFilename, pcLabel);
  FREE(pcFilename);

  if (!psDescriptor)
    XEN_ERROR(NO_SUCH_PLUGIN,
	      XEN_LIST_2(C_TO_XEN_STRING(S_apply_ladspa),
			 ladspa_plugin_configuration));

  isLADSPAPluginSupported(psDescriptor);
  inchans = lInputCount;
  outchans = lOutputCount;
  if ((inchans == 0) || (outchans == 0))
    XEN_ERROR(PLUGIN_ERROR,
	      XEN_LIST_3(C_TO_XEN_STRING(S_apply_ladspa),
			 ladspa_plugin_configuration,
			 C_TO_XEN_STRING(_("Snd plugins must have at least 1 input and output"))));

  if (inchans != readers)
    {
      msg = mus_format(_("Ladspa %s inputs (%d) != sample-readers (%d)"), pcLabel, readers, inchans);
      errmsg = C_TO_XEN_STRING(msg);
      FREE(msg);
      XEN_ERROR(PLUGIN_ERROR,
		XEN_LIST_3(C_TO_XEN_STRING(S_apply_ladspa),
			   ladspa_plugin_configuration,
			   errmsg));
    }

  lParameterCount = 0;
  for (lPortIndex = 0; lPortIndex < psDescriptor->PortCount; lPortIndex++)
    if (LADSPA_IS_PORT_CONTROL(psDescriptor->PortDescriptors[lPortIndex])
	&& LADSPA_IS_PORT_INPUT(psDescriptor->PortDescriptors[lPortIndex]))
      lParameterCount++;
  msg = mus_format("a list of 2 strings + %d parameters", (int)lParameterCount);
  XEN_ASSERT_TYPE(XEN_LIST_LENGTH(ladspa_plugin_configuration) == (int)(2 + lParameterCount),
		  ladspa_plugin_configuration,
		  XEN_ARG_2,
		  S_apply_ladspa, 
		  msg);
  FREE(msg);
  pfControls = (LADSPA_Data *)MALLOC(psDescriptor->PortCount * sizeof(LADSPA_Data));

  /* Get parameters. */
  xenParameters = XEN_CDR(XEN_CDR(ladspa_plugin_configuration));
  for (lPortIndex = 0; lPortIndex < psDescriptor->PortCount; lPortIndex++) 
    {
    iPortDescriptor = psDescriptor->PortDescriptors[lPortIndex];
    if (LADSPA_IS_PORT_CONTROL(iPortDescriptor)
	&& LADSPA_IS_PORT_INPUT(iPortDescriptor)) {
      /* FIXME: uninformative error. */
      XEN_ASSERT_TYPE(XEN_NUMBER_P(XEN_CAR(xenParameters)),
		      ladspa_plugin_configuration,
		      XEN_ARG_2,
		      S_apply_ladspa, "a number");
      pfControls[lPortIndex] = (LADSPA_Data)XEN_TO_C_DOUBLE(XEN_CAR(xenParameters));
      xenParameters = XEN_CDR(xenParameters);
    }
  }

  sf = (snd_fd **)CALLOC(readers, sizeof(snd_fd *));

  /* Get sample count. */
  num = XEN_TO_C_OFF_T(samples);

  /* Local version of sound descriptor. */
  if (XEN_LIST_P(reader))
    {
      for (i = 0; i < readers; i++)
	sf[i] = get_sf(XEN_LIST_REF(reader, i));
    }
  else sf[0] = get_sf(reader);

  /* Channel info structure. */
  cp = sf[0]->cp;

  /* Sound information. */
  sp = (cp->sound);

  lSampleRate = (unsigned long)(sp->hdr->srate);
  psHandle = (LADSPA_Handle *)psDescriptor->instantiate(psDescriptor, lSampleRate);
  if (!psHandle)
    XEN_ERROR(PLUGIN_ERROR,
	      XEN_LIST_3(C_TO_XEN_STRING(S_apply_ladspa),
			 ladspa_plugin_configuration,
			 C_TO_XEN_STRING("plugin did not instantiate")));

  /* this code added 20-Sep-01 */
  pfInputBuffer = (LADSPA_Data **)CALLOC(inchans, sizeof(LADSPA_Data *));
  for (i = 0; i < inchans; i++)
    pfInputBuffer[i] = (LADSPA_Data *)CALLOC(MAX_BUFFER_SIZE, sizeof(LADSPA_Data));
  pfOutputBuffer = (LADSPA_Data **)CALLOC(outchans, sizeof(LADSPA_Data *));
  for (i = 0; i < outchans; i++)
    pfOutputBuffer[i] = (LADSPA_Data *)CALLOC(MAX_BUFFER_SIZE, sizeof(LADSPA_Data));

  data = (mus_sample_t **)CALLOC(outchans, sizeof(mus_sample_t *));
  for (i = 0; i < outchans; i++)
    data[i] = (mus_sample_t *)CALLOC(MAX_BUFFER_SIZE, sizeof(mus_sample_t));

  /* Connect input and output control ports. */
  {
    int inc = 0, outc = 0;
    for (lPortIndex = 0; lPortIndex < psDescriptor->PortCount; lPortIndex++) {
      if (LADSPA_IS_PORT_CONTROL(psDescriptor->PortDescriptors[lPortIndex])) {
	psDescriptor->connect_port(psHandle,
				   lPortIndex,
				   pfControls + lPortIndex);
	/* (Output control data is quietly lost.) */
      }
      else /* AUDIO */ {
	if (LADSPA_IS_PORT_INPUT(psDescriptor->PortDescriptors[lPortIndex]))
	  psDescriptor->connect_port(psHandle,
				     lPortIndex,
				     pfInputBuffer[inc++]);
	else
	  psDescriptor->connect_port(psHandle,
				     lPortIndex,
				     pfOutputBuffer[outc++]);
      }
    }
  }

  /* Temporary file name. */
  ofile = snd_tempnam();

  /* Create initial header for output file, stealing info from input
     file. */
  hdr = make_temp_header(ofile,
			 SND_SRATE(sp),
			 outchans,
			 num,
			 XEN_TO_C_STRING(origin));

  /* Open the output file, using the header we've been working on. */
  ofd = open_temp_file(ofile, outchans, hdr);
  if (ofd == -1)
    XEN_ERROR(CANNOT_SAVE,
	      XEN_LIST_3(C_TO_XEN_STRING(S_apply_ladspa),
			 C_TO_XEN_STRING(ofile),
			 C_TO_XEN_STRING(strerror(errno))));

  /* Tidy up header. */
  datumb = mus_bytes_per_sample(hdr->format);

  if (psDescriptor->activate)
    psDescriptor->activate(psHandle);

  lAt = 0;
  ss->stopped_explicitly = false;
  while (lAt < num) {

    /* Decide how much audio to process this frame. */
    lBlockSize = num - lAt;
    if (lBlockSize > MAX_BUFFER_SIZE)
      lBlockSize = MAX_BUFFER_SIZE;

    /* Prepare the input data. */
    for (i = 0; i < readers; i++)
      for (lSampleIndex = 0; lSampleIndex < lBlockSize; lSampleIndex++) {
	pfInputBuffer[i][lSampleIndex] = read_sample_to_float(sf[i]);
      }

    /* Run the plugin. */
    psDescriptor->run(psHandle, lBlockSize);

    /* Prepare the output data. */
    for (i = 0; i < outchans; i++)
      for (lSampleIndex = 0; lSampleIndex < lBlockSize; lSampleIndex++)
	data[i][lSampleIndex] = MUS_FLOAT_TO_SAMPLE(pfOutputBuffer[i][lSampleIndex]);

    /* Send the output data to the outside world. */
    err = mus_file_write(ofd,
			 0,
			 lBlockSize - 1,
			 outchans,
			 data);
    if (err == -1)
      break;
    if (ss->stopped_explicitly)
      break;

    lAt += lBlockSize;
  }

  if (psDescriptor->deactivate)
    psDescriptor->deactivate(psHandle);

  psDescriptor->cleanup(psHandle);

  close_temp_file(ofd,
		  hdr,
		  num * datumb * outchans,
		  sp);

  /* Discard tmp header. */
  hdr = free_file_info(hdr);
  if (!(ss->stopped_explicitly))
    {
      for (i = 0, j = 0; i < outchans; i++)
	{
	  ncp = sf[j]->cp;
	  file_change_samples(sf[j]->initial_samp,
			      num,
			      ofile,
			      ncp,
			      i,
			      (outchans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
			      LOCK_MIXES,
			      XEN_TO_C_STRING(origin),
			      ncp->edit_ctr);
	  update_graph(ncp);
	  j++;
	  if (j >= inchans) j = 0;
	}
    }
  else 
    {
      report_in_minibuffer(sp, _(S_apply_ladspa " interrupted"));
      ss->stopped_explicitly = false;
    }
  if (ofile) FREE(ofile);
  for (i = 0; i < inchans; i++)
    FREE(pfInputBuffer[i]);
  /* sf[i] is directly from scheme, so it will presumably handle reader gc */
  for (i = 0; i < outchans; i++)
    {
      FREE(pfOutputBuffer[i]);
      FREE(data[i]);
    }
  if (sf) FREE(sf);
  if (pfControls) FREE(pfControls);
  FREE(data);
  FREE(pfInputBuffer);
  FREE(pfOutputBuffer);
  return(XEN_FALSE);
}

#if HAVE_EXTENSION_LANGUAGE
#if HAVE_GUILE
  #define FIELD_PREFIX "."
#else
  #define FIELD_PREFIX "R"
#endif

#if HAVE_GUILE
#if HAVE_SCM_C_DEFINE
  #define DEFINE_INTEGER(Name) scm_c_define(#Name, C_TO_XEN_INT(Name))
#else
  #define DEFINE_INTEGER(Name) gh_define(#Name, C_TO_XEN_INT(Name))
#endif
#else
  #define DEFINE_INTEGER(Name) rb_define_global_const(#Name, C_TO_XEN_INT(Name))
#endif

#define DEFINE_READER(Name, Value) XEN_DEFINE_PROCEDURE(FIELD_PREFIX #Name, Value, 1, 0, 0, NULL)

#define C_TO_XEN_Ladspa_Descriptor(Value) \
  ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("Ladspa-Descriptor"), C_TO_XEN_ULONG((unsigned long)Value)) : XEN_FALSE)
#define XEN_TO_C_Ladspa_Descriptor(Value) ((LADSPA_Descriptor *)(XEN_TO_C_ULONG(XEN_CADR(Value))))
#define XEN_Ladspa_Descriptor_P(Value) (XEN_LIST_P(Value) && (XEN_LIST_LENGTH(Value) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                            (strcmp("Ladspa-Descriptor", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))
  
#define C_TO_XEN_Ladspa_Handle(Value) \
  ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("Ladspa-Handle"), C_TO_XEN_ULONG((unsigned long)Value)) : XEN_FALSE)
#define XEN_TO_C_Ladspa_Handle(Value) ((LADSPA_Handle *)(XEN_TO_C_ULONG(XEN_CADR(Value))))
#define XEN_Ladspa_Handle_P(Value) (XEN_LIST_P(Value) && (XEN_LIST_LENGTH(Value) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                           (strcmp("Ladspa-Handle", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))
  
#define S_ladspa_descriptor "ladspa-descriptor"

static XEN g_ladspa_descriptor(XEN ladspa_plugin_filename, XEN ladspa_plugin_label)
{
  #define H_ladspa_descriptor "(" S_ladspa_descriptor " library plugin): return the descriptor \
associated with the given plugin."
  const LADSPA_Descriptor *psDescriptor;
  char *pcFilename, *pcLabel, *pcTmp;
  if (!g_bLADSPAInitialised) loadLADSPA();
  XEN_ASSERT_TYPE(XEN_STRING_P(ladspa_plugin_filename), ladspa_plugin_filename, XEN_ARG_1, S_ladspa_descriptor, "a string");
  XEN_ASSERT_TYPE(XEN_STRING_P(ladspa_plugin_label), ladspa_plugin_label, XEN_ARG_2, S_ladspa_descriptor, "a string");
  pcTmp = XEN_TO_C_STRING(ladspa_plugin_filename);
  pcLabel = XEN_TO_C_STRING(ladspa_plugin_label);
  pcFilename = packLADSPAFilename(pcTmp);
  psDescriptor = findLADSPADescriptor(pcFilename, pcLabel);
  FREE(pcFilename);
  if (!psDescriptor) return(XEN_FALSE);
  return(C_TO_XEN_Ladspa_Descriptor(psDescriptor));
}

static XEN g_ladspa_Label(XEN ptr)
{
  XEN_ASSERT_TYPE(XEN_Ladspa_Descriptor_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "Label", "Ladspa descriptor");
  return(C_TO_XEN_STRING((XEN_TO_C_Ladspa_Descriptor(ptr))->Label));
}

static XEN g_ladspa_Name(XEN ptr)
{
  XEN_ASSERT_TYPE(XEN_Ladspa_Descriptor_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "Name", "Ladspa descriptor");
  return(C_TO_XEN_STRING((XEN_TO_C_Ladspa_Descriptor(ptr))->Name));
}

static XEN g_ladspa_Copyright(XEN ptr)
{
  XEN_ASSERT_TYPE(XEN_Ladspa_Descriptor_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "Copyright", "Ladspa descriptor");
  return(C_TO_XEN_STRING((XEN_TO_C_Ladspa_Descriptor(ptr))->Copyright));
}

static XEN g_ladspa_Maker(XEN ptr)
{
  XEN_ASSERT_TYPE(XEN_Ladspa_Descriptor_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "Maker", "Ladspa descriptor");
  return(C_TO_XEN_STRING((XEN_TO_C_Ladspa_Descriptor(ptr))->Maker));
}

static XEN g_ladspa_Properties(XEN ptr)
{
  XEN_ASSERT_TYPE(XEN_Ladspa_Descriptor_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "Properties", "Ladspa descriptor");
  return(C_TO_XEN_INT((XEN_TO_C_Ladspa_Descriptor(ptr))->Properties));
}

static XEN g_ladspa_UniqueID(XEN ptr)
{
  XEN_ASSERT_TYPE(XEN_Ladspa_Descriptor_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "UniqueID", "Ladspa descriptor");
  return(C_TO_XEN_ULONG((XEN_TO_C_Ladspa_Descriptor(ptr))->UniqueID));
}

static XEN g_ladspa_PortCount(XEN ptr)
{
  XEN_ASSERT_TYPE(XEN_Ladspa_Descriptor_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "PortCount", "Ladspa descriptor");
  return(C_TO_XEN_ULONG((XEN_TO_C_Ladspa_Descriptor(ptr))->PortCount));
}

static XEN g_ladspa_PortDescriptors(XEN ptr)
{
  LADSPA_Descriptor *descriptor;
  int i, len;
  XEN lst = XEN_EMPTY_LIST;
  XEN_ASSERT_TYPE(XEN_Ladspa_Descriptor_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "PortDescriptors", "Ladspa descriptor");
  descriptor = XEN_TO_C_Ladspa_Descriptor(ptr);
  len = descriptor->PortCount;
  for (i = len - 1; i >= 0; i--)
    lst = XEN_CONS(C_TO_XEN_INT(descriptor->PortDescriptors[i]), lst);
  return(lst);
}

static XEN g_ladspa_PortRangeHints(XEN ptr)
{
  LADSPA_Descriptor *descriptor;
  int i, len;
  XEN lst = XEN_EMPTY_LIST;
  XEN_ASSERT_TYPE(XEN_Ladspa_Descriptor_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "PortRangeHints", "Ladspa descriptor");
  descriptor = XEN_TO_C_Ladspa_Descriptor(ptr);
  len = descriptor->PortCount;
  for (i = len - 1; i >= 0; i--)
    lst = XEN_CONS(XEN_LIST_3(C_TO_XEN_INT(descriptor->PortRangeHints[i].HintDescriptor),
			      C_TO_XEN_DOUBLE(descriptor->PortRangeHints[i].LowerBound),
			      C_TO_XEN_DOUBLE(descriptor->PortRangeHints[i].UpperBound)),
		   lst);
  return(lst);
}

static XEN g_ladspa_PortNames(XEN ptr)
{
  LADSPA_Descriptor *descriptor;
  int i, len;
  XEN lst = XEN_EMPTY_LIST;
  XEN_ASSERT_TYPE(XEN_Ladspa_Descriptor_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "PortNames", "Ladspa descriptor");
  descriptor = XEN_TO_C_Ladspa_Descriptor(ptr);
  len = descriptor->PortCount;
  for (i = len - 1; i >= 0; i--)
    lst = XEN_CONS(C_TO_XEN_STRING(descriptor->PortNames[i]), lst);
  return(lst);
}


#define S_ladspa_instantiate "ladspa-instantiate"
static XEN g_ladspa_instantiate(XEN ptr, XEN srate)
{
  const LADSPA_Descriptor *descriptor;
  LADSPA_Handle handle;
  XEN_ASSERT_TYPE(XEN_Ladspa_Descriptor_P(ptr), ptr, XEN_ARG_1, S_ladspa_instantiate, "Ladspa descriptor");
  XEN_ASSERT_TYPE(XEN_ULONG_P(srate), srate, XEN_ARG_2, S_ladspa_instantiate, "int");
  descriptor = XEN_TO_C_Ladspa_Descriptor(ptr);
  handle = descriptor->instantiate(descriptor, XEN_TO_C_ULONG(srate));
  return(C_TO_XEN_Ladspa_Handle(handle));
}

#define S_ladspa_activate "ladspa-activate"
static XEN g_ladspa_activate(XEN desc, XEN ptr)
{
  const LADSPA_Descriptor *descriptor;
  XEN_ASSERT_TYPE(XEN_Ladspa_Descriptor_P(desc), desc, XEN_ARG_1, S_ladspa_activate, "Ladspa descriptor");
  XEN_ASSERT_TYPE(XEN_Ladspa_Handle_P(ptr), ptr, XEN_ARG_2, S_ladspa_activate, "Ladspa handle");
  descriptor = XEN_TO_C_Ladspa_Descriptor(desc);
  if (descriptor->activate) descriptor->activate(XEN_TO_C_Ladspa_Handle(ptr));
  return(XEN_FALSE);
}

#define S_ladspa_deactivate "ladspa-deactivate"
static XEN g_ladspa_deactivate(XEN desc, XEN ptr)
{
  const LADSPA_Descriptor *descriptor;
  XEN_ASSERT_TYPE(XEN_Ladspa_Descriptor_P(desc), desc, XEN_ARG_1, S_ladspa_deactivate, "Ladspa descriptor");
  XEN_ASSERT_TYPE(XEN_Ladspa_Handle_P(ptr), ptr, XEN_ARG_2, S_ladspa_deactivate, "Ladspa handle");
  descriptor = XEN_TO_C_Ladspa_Descriptor(desc);
  if (descriptor->deactivate) descriptor->deactivate(XEN_TO_C_Ladspa_Handle(ptr));
  return(XEN_FALSE);
}

#define S_ladspa_cleanup "ladspa-cleanup"
static XEN g_ladspa_cleanup(XEN desc, XEN ptr)
{
  const LADSPA_Descriptor *descriptor;
  XEN_ASSERT_TYPE(XEN_Ladspa_Descriptor_P(desc), desc, XEN_ARG_1, S_ladspa_cleanup, "Ladspa descriptor");
  XEN_ASSERT_TYPE(XEN_Ladspa_Handle_P(ptr), ptr, XEN_ARG_2, S_ladspa_cleanup, "Ladspa handle");
  descriptor = XEN_TO_C_Ladspa_Descriptor(desc);
  if (descriptor->cleanup) descriptor->cleanup(XEN_TO_C_Ladspa_Handle(ptr));
  return(XEN_FALSE);
}

#define S_ladspa_run "ladspa-run"
static XEN g_ladspa_run(XEN desc, XEN ptr, XEN count)
{
  const LADSPA_Descriptor *descriptor;
  XEN_ASSERT_TYPE(XEN_Ladspa_Descriptor_P(desc), desc, XEN_ARG_1, S_ladspa_run, "Ladspa descriptor");
  XEN_ASSERT_TYPE(XEN_Ladspa_Handle_P(ptr), ptr, XEN_ARG_2, S_ladspa_run, "Ladspa handle");
  XEN_ASSERT_TYPE(XEN_ULONG_P(count), count, XEN_ARG_3, S_ladspa_run, "unsigned long");
  descriptor = XEN_TO_C_Ladspa_Descriptor(desc);
  if (descriptor->run) descriptor->run(XEN_TO_C_Ladspa_Handle(ptr), XEN_TO_C_ULONG(count));
  return(XEN_FALSE);
}

#define S_ladspa_run_adding "ladspa-run-adding"
static XEN g_ladspa_run_adding(XEN desc, XEN ptr, XEN count)
{
  const LADSPA_Descriptor *descriptor;
  XEN_ASSERT_TYPE(XEN_Ladspa_Descriptor_P(desc), desc, XEN_ARG_1, S_ladspa_run_adding, "Ladspa descriptor");
  XEN_ASSERT_TYPE(XEN_Ladspa_Handle_P(ptr), ptr, XEN_ARG_2, S_ladspa_run_adding, "Ladspa handle");
  XEN_ASSERT_TYPE(XEN_ULONG_P(count), count, XEN_ARG_3, S_ladspa_run_adding, "unsigned long");
  descriptor = XEN_TO_C_Ladspa_Descriptor(desc);
  if (descriptor->run_adding) descriptor->run_adding(XEN_TO_C_Ladspa_Handle(ptr), XEN_TO_C_ULONG(count));
  return(XEN_FALSE);
}

#define S_ladspa_set_run_adding_gain "ladspa-set-run-adding-gain"
static XEN g_ladspa_set_run_adding_gain(XEN desc, XEN ptr, XEN gain)
{
  const LADSPA_Descriptor *descriptor;
  XEN_ASSERT_TYPE(XEN_Ladspa_Descriptor_P(desc), desc, XEN_ARG_1, S_ladspa_set_run_adding_gain, "Ladspa descriptor");
  XEN_ASSERT_TYPE(XEN_Ladspa_Handle_P(ptr), ptr, XEN_ARG_2, S_ladspa_set_run_adding_gain, "Ladspa handle");
  XEN_ASSERT_TYPE(XEN_DOUBLE_P(gain), gain, XEN_ARG_3, S_ladspa_set_run_adding_gain, "float");
  descriptor = XEN_TO_C_Ladspa_Descriptor(desc);
  if (descriptor->set_run_adding_gain) descriptor->set_run_adding_gain(XEN_TO_C_Ladspa_Handle(ptr), (LADSPA_Data)(XEN_TO_C_DOUBLE(gain)));
  return(XEN_FALSE);
}

#define S_ladspa_connect_port "ladspa-connect-port"
static XEN g_ladspa_connect_port(XEN desc, XEN ptr, XEN port, XEN data)
{
  const LADSPA_Descriptor *descriptor;
  vct *samples;
  XEN_ASSERT_TYPE(XEN_Ladspa_Descriptor_P(desc), desc, XEN_ARG_1, S_ladspa_connect_port, "Ladspa descriptor");
  XEN_ASSERT_TYPE(XEN_Ladspa_Handle_P(ptr), ptr, XEN_ARG_2, S_ladspa_connect_port, "Ladspa handle");
  XEN_ASSERT_TYPE(XEN_ULONG_P(port), port, XEN_ARG_3, S_ladspa_connect_port, "unsigned long");
  XEN_ASSERT_TYPE(VCT_P(data), data, XEN_ARG_4, S_ladspa_connect_port, "vct");
  descriptor = XEN_TO_C_Ladspa_Descriptor(desc);
  samples = TO_VCT(data);
  if (descriptor->connect_port) 
    descriptor->connect_port(XEN_TO_C_Ladspa_Handle(ptr),
			     XEN_TO_C_ULONG(port),
			     samples->data);
  /* if --with-doubles, samples->data is a double array */
  return(XEN_FALSE);
}



#ifdef XEN_ARGIFY_1
XEN_NARGIFY_2(g_analyse_ladspa_w, g_analyse_ladspa)
XEN_NARGIFY_2(g_ladspa_descriptor_w, g_ladspa_descriptor)
XEN_NARGIFY_4(g_apply_ladspa_w, g_apply_ladspa)
XEN_NARGIFY_0(g_init_ladspa_w, g_init_ladspa)
XEN_NARGIFY_0(g_list_ladspa_w, g_list_ladspa)
XEN_NARGIFY_1(g_ladspa_Label_w, g_ladspa_Label)
XEN_NARGIFY_1(g_ladspa_Name_w, g_ladspa_Name)
XEN_NARGIFY_1(g_ladspa_Copyright_w, g_ladspa_Copyright)
XEN_NARGIFY_1(g_ladspa_Maker_w, g_ladspa_Maker)
XEN_NARGIFY_1(g_ladspa_Properties_w, g_ladspa_Properties)
XEN_NARGIFY_1(g_ladspa_UniqueID_w, g_ladspa_UniqueID)
XEN_NARGIFY_1(g_ladspa_PortNames_w, g_ladspa_PortNames)
XEN_NARGIFY_1(g_ladspa_PortDescriptors_w, g_ladspa_PortDescriptors)
XEN_NARGIFY_1(g_ladspa_PortRangeHints_w, g_ladspa_PortRangeHints)
XEN_NARGIFY_1(g_ladspa_PortCount_w, g_ladspa_PortCount)
XEN_NARGIFY_2(g_ladspa_instantiate_w, g_ladspa_instantiate)
XEN_NARGIFY_2(g_ladspa_activate_w, g_ladspa_activate)
XEN_NARGIFY_2(g_ladspa_deactivate_w, g_ladspa_deactivate)
XEN_NARGIFY_2(g_ladspa_cleanup_w, g_ladspa_cleanup)
XEN_NARGIFY_3(g_ladspa_run_w, g_ladspa_run)
XEN_NARGIFY_3(g_ladspa_run_adding_w, g_ladspa_run_adding)
XEN_NARGIFY_3(g_ladspa_set_run_adding_gain_w, g_ladspa_set_run_adding_gain)
XEN_NARGIFY_4(g_ladspa_connect_port_w, g_ladspa_connect_port)
#else
#define g_analyse_ladspa_w g_analyse_ladspa
#define g_ladspa_descriptor_w g_ladspa_descriptor
#define g_apply_ladspa_w g_apply_ladspa
#define g_init_ladspa_w g_init_ladspa
#define g_list_ladspa_w g_list_ladspa
#define g_ladspa_Label_w g_ladspa_Label
#define g_ladspa_Name_w g_ladspa_Name
#define g_ladspa_Copyright_w g_ladspa_Copyright
#define g_ladspa_Maker_w g_ladspa_Maker
#define g_ladspa_Properties_w g_ladspa_Properties
#define g_ladspa_UniqueID_w g_ladspa_UniqueID
#define g_ladspa_PortNames_w g_ladspa_PortNames
#define g_ladspa_PortDescriptors_w g_ladspa_PortDescriptors
#define g_ladspa_PortRangeHints_w g_ladspa_PortRangeHints
#define g_ladspa_PortCount_w g_ladspa_PortCount
#define g_ladspa_instantiate_w g_ladspa_instantiate
#define g_ladspa_activate_w g_ladspa_activate
#define g_ladspa_deactivate_w g_ladspa_deactivate
#define g_ladspa_cleanup_w g_ladspa_cleanup
#define g_ladspa_run_w g_ladspa_run
#define g_ladspa_run_adding_w g_ladspa_run_adding
#define g_ladspa_set_run_adding_gain_w g_ladspa_set_run_adding_gain
#define g_ladspa_connect_port_w g_ladspa_connect_port
#endif

void g_ladspa_to_snd(void);
void g_ladspa_to_snd(void)
{
  XEN_DEFINE_PROCEDURE(S_analyse_ladspa, g_analyse_ladspa_w, 2, 0, 0, H_analyse_ladspa);
  XEN_DEFINE_PROCEDURE(S_apply_ladspa, g_apply_ladspa_w, 4, 0, 0, H_apply_ladspa);
  XEN_DEFINE_PROCEDURE(S_init_ladspa, g_init_ladspa_w, 0, 0, 0, H_init_ladspa);
  XEN_DEFINE_PROCEDURE(S_list_ladspa, g_list_ladspa_w, 0, 0, 0, H_list_ladspa);

  DEFINE_INTEGER(LADSPA_PROPERTY_REALTIME);
  DEFINE_INTEGER(LADSPA_PROPERTY_INPLACE_BROKEN);
  DEFINE_INTEGER(LADSPA_PROPERTY_HARD_RT_CAPABLE);

  DEFINE_INTEGER(LADSPA_PORT_INPUT);
  DEFINE_INTEGER(LADSPA_PORT_OUTPUT);
  DEFINE_INTEGER(LADSPA_PORT_CONTROL);
  DEFINE_INTEGER(LADSPA_PORT_AUDIO);

  DEFINE_INTEGER(LADSPA_HINT_BOUNDED_BELOW);
  DEFINE_INTEGER(LADSPA_HINT_BOUNDED_ABOVE);
  DEFINE_INTEGER(LADSPA_HINT_TOGGLED);
  DEFINE_INTEGER(LADSPA_HINT_SAMPLE_RATE);
  DEFINE_INTEGER(LADSPA_HINT_LOGARITHMIC);
  DEFINE_INTEGER(LADSPA_HINT_INTEGER);
#ifdef LADSPA_HINT_DEFAULT_MASK
  DEFINE_INTEGER(LADSPA_HINT_DEFAULT_MASK);
  DEFINE_INTEGER(LADSPA_HINT_DEFAULT_NONE);
  DEFINE_INTEGER(LADSPA_HINT_DEFAULT_MINIMUM);
  DEFINE_INTEGER(LADSPA_HINT_DEFAULT_LOW);
  DEFINE_INTEGER(LADSPA_HINT_DEFAULT_MIDDLE);
  DEFINE_INTEGER(LADSPA_HINT_DEFAULT_HIGH);
  DEFINE_INTEGER(LADSPA_HINT_DEFAULT_MAXIMUM);
  DEFINE_INTEGER(LADSPA_HINT_DEFAULT_0);
  DEFINE_INTEGER(LADSPA_HINT_DEFAULT_1);
  DEFINE_INTEGER(LADSPA_HINT_DEFAULT_100);
  DEFINE_INTEGER(LADSPA_HINT_DEFAULT_440);
#endif

  XEN_DEFINE_PROCEDURE(S_ladspa_descriptor, g_ladspa_descriptor_w, 2, 0, 0, H_ladspa_descriptor);
  DEFINE_READER(Label, g_ladspa_Label_w);
  DEFINE_READER(Name, g_ladspa_Name_w);
  DEFINE_READER(Copyright, g_ladspa_Copyright_w);
  DEFINE_READER(Maker, g_ladspa_Maker_w);
  DEFINE_READER(Properties, g_ladspa_Properties_w);
  DEFINE_READER(UniqueID, g_ladspa_UniqueID_w);
  DEFINE_READER(PortNames, g_ladspa_PortNames_w);
  DEFINE_READER(PortDescriptors, g_ladspa_PortDescriptors_w);
  DEFINE_READER(PortRangeHints, g_ladspa_PortRangeHints_w); 
  DEFINE_READER(PortCount, g_ladspa_PortCount_w);
 
  XEN_DEFINE_PROCEDURE(S_ladspa_instantiate, g_ladspa_instantiate_w, 2, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(S_ladspa_activate, g_ladspa_activate_w, 2, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(S_ladspa_deactivate, g_ladspa_deactivate_w, 2, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(S_ladspa_cleanup, g_ladspa_cleanup_w, 2, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(S_ladspa_run, g_ladspa_run_w, 3, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(S_ladspa_run_adding, g_ladspa_run_adding_w, 3, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(S_ladspa_set_run_adding_gain, g_ladspa_set_run_adding_gain_w, 3, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(S_ladspa_connect_port, g_ladspa_connect_port_w, 4, 0, 0, NULL);

  XEN_YES_WE_HAVE("snd-ladspa");
}

#endif
#endif

