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
 * bil: 28-Nov-01 input chans need not equal output chans now.
 * bil: 15-Oct-01 added some error returns (rather than snd_error).
 *       multichannel plugin support.
 * bil: 20-Sep-01 changed location of pfInputBuffer to avoid glomming up the
 *       stack with a huge array.  Also added c++ code (previous code
 *       was illegal in c++).
 */



/*****************************************************************************/

/* FIXME: Repository is not threadsafe. Does this matter in Snd? (not currently -- Bill) */

/* FIXME: Memory checking is non-existent. */

/*****************************************************************************/

typedef struct {
  char * m_pcPackedFilename;
  const char * m_pcLabel;
  const LADSPA_Descriptor * m_psDescriptor;
  void * m_pvPluginHandle;
} LADSPAPluginInfo;

/*****************************************************************************/

char g_bLADSPAInitialised = 0;
LADSPAPluginInfo ** g_psLADSPARepository;
long g_lLADSPARepositoryCapacity;
long g_lLADSPARepositoryCount;

#define LADSPA_REPOSITORY_CAPACITY_STEP 100

/*****************************************************************************/

static int lInputCount, lOutputCount;

static void isLADSPAPluginSupported(const LADSPA_Descriptor * psDescriptor) {
  int lIndex;
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
static const LADSPA_Descriptor *findLADSPADescriptor(const char * pcPackedFilename, const char * pcLabel) {

  /* FIXME: Could be using hashtables, binary chops etc. Instead we simply scan the table. */

  long lIndex;
  LADSPAPluginInfo * psInfo;

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
static char * packLADSPAFilename(const char * pcFilename) {

  const char * pcStart, * pcEnd;
  char * pcPackedFilename;

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
  LADSPAPluginInfo *pvPluginHandle;
  LADSPAPluginInfo * psInfo;
  if (g_lLADSPARepositoryCount > 0)
    pvPluginHandle = (LADSPAPluginInfo *)(g_psLADSPARepository[0]->m_pvPluginHandle);
  pvPluginHandle++;
  for (lIndex = 0; lIndex < g_lLADSPARepositoryCount; lIndex++) {
    psInfo = g_psLADSPARepository[lIndex];
    free(psInfo->m_pcPackedFilename);
    /* Don't free Label or Descriptor - this memory is owned by the
       relevant plugin library. */
    if (pvPluginHandle != psInfo->m_pvPluginHandle) {
      pvPluginHandle = (LADSPAPluginInfo *)(psInfo->m_pvPluginHandle);
      dlclose(pvPluginHandle);
    }
    free(psInfo);
  }

  free(g_psLADSPARepository);
  g_bLADSPAInitialised = 0;
}

/*****************************************************************************/

/* Called only from within loadLADSPA->loadLADSPADirectory. */
static void loadLADSPALibrary(void * pvPluginHandle,
			      char * pcFilename,
			      LADSPA_Descriptor_Function fDescriptorFunction) {

  LADSPAPluginInfo ** psOldRepository, * psInfo;
  long lNewCapacity, lIndex;
  const LADSPA_Descriptor * psDescriptor;

  for (lIndex = 0;
       (psDescriptor = fDescriptorFunction(lIndex)) != NULL;
       lIndex++)
    {
      if (g_lLADSPARepositoryCount == g_lLADSPARepositoryCapacity) {
	psOldRepository = g_psLADSPARepository;
	lNewCapacity = (g_lLADSPARepositoryCapacity
			+ LADSPA_REPOSITORY_CAPACITY_STEP);
	g_psLADSPARepository = (LADSPAPluginInfo **)MALLOC(sizeof(LADSPAPluginInfo *) * lNewCapacity);
	memcpy(g_psLADSPARepository,
	       psOldRepository,
	       sizeof(LADSPAPluginInfo *) * g_lLADSPARepositoryCount);
	g_lLADSPARepositoryCapacity = lNewCapacity;
	free(psOldRepository);
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
static void loadLADSPADirectory(const char * pcDirectory) {

  char * pcFilename = NULL;
  DIR * psDirectory;
  LADSPA_Descriptor_Function fDescriptorFunction;
  long lDirLength;
  long iNeedSlash;
  struct dirent * psDirectoryEntry;
  void * pvPluginHandle;

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

  char * pcBuffer = NULL;
  const char * pcEnd;
  const char * pcLADSPAPath;
  const char * pcStart;

  g_bLADSPAInitialised = 1;
  g_psLADSPARepository = (LADSPAPluginInfo **)MALLOC(sizeof(LADSPAPluginInfo *)
				* LADSPA_REPOSITORY_CAPACITY_STEP);
  g_lLADSPARepositoryCapacity = LADSPA_REPOSITORY_CAPACITY_STEP;
  g_lLADSPARepositoryCount = 0;

  pcLADSPAPath = ladspa_dir(get_global_state());
  if (!pcLADSPAPath)
    {
      pcLADSPAPath = getenv("LADSPA_PATH");
      if (!pcLADSPAPath) {
	snd_warning("Warning: You have not set " S_ladspa_dir " or the environment variable LADSPA_PATH.");
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

#define H_init_ladspa "(" S_init_ladspa ") reinitialises LADSPA. This is not \
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

#define H_list_ladspa "(" S_list_ladspa ") returns a list of lists containing \
information of the LADSPA plugins currently available. For each plugin a \
list containing the plugin-file and plugin-label is included."

  long lIndex;
  XEN xenList; XEN xenPluginList;
  LADSPAPluginInfo * psInfo;

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

#define H_analyse_ladspa "(" S_analyse_ladspa " library plugin) returns a list of information about \
a LADSPA plugin. The plugin is identified by library and plugin. \
The items are: plugin-name, plugin-maker, \
plugin-copyright, plugin-parameter-list. The plugin-port-list contains a \
list of information for each parameter available. The first item in this \
list is the name of the port. Other hint information may follow this to help \
a user interface edit the parameter in a useful way."

  long lIndex;
  const LADSPA_Descriptor * psDescriptor;
  char * pcFilename, * pcLabel, * pcTmp;
  XEN xenList; XEN xenPortData;
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
                         ladspa_plugin_filename,
                         ladspa_plugin_label));
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

snd_fd *get_sf(XEN obj);
int sf_p(XEN obj);

/* FIXME: We could improve this function to receive a list of plugin configurations for chain processing. */

#define S_apply_ladspa "apply-ladspa"

static XEN g_apply_ladspa(XEN reader,
			  XEN ladspa_plugin_configuration,
			  XEN samples,
			  XEN origin)
{
#define H_apply_ladspa "(" S_apply_ladspa " reader (list library plugin pars) dur edname) applies a LADSPA plugin to process a \
sound. The parameters are soundfile-reader, a ladspa-plugin-configuration, \
the number of samples to process, and an `origin' for edit lists. The \
ladspa-plugin-configuration is a list containing the plugin-file and \
plugin-label for the LADSPA plugin, as provided by list-ladspa, followed \
by any arguments. The reader argument can also be a list of readers. \
Information about about parameters can be acquired using analyse-ladspa."

  const LADSPA_Descriptor * psDescriptor;
  char * pcFilename, * pcLabel, * pcTmp;
  LADSPA_Handle * psHandle;
  unsigned long lSampleRate, lPortIndex, lAt, lBlockSize, lSampleIndex;
  unsigned long lParameterCount;
  XEN xenParameters;
  LADSPA_PortDescriptor iPortDescriptor;

  LADSPA_Data *pfControls = NULL;
  chan_info *cp, *ncp;
  snd_info *sp;
  char *ofile, *msg;
  int num, i, j, ofd, datumb, err = 0, inchans = 1, readers = 1, outchans = 1;
  snd_fd **sf;
  file_info *hdr;
  snd_state *state;
  XEN errmsg;
  MUS_SAMPLE_TYPE **data;
  LADSPA_Data **pfInputBuffer = NULL;
  LADSPA_Data **pfOutputBuffer = NULL;

  state = get_global_state();
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
			 C_TO_XEN_STRING("Snd plugins must have at least 1 input and output")));

  if (inchans != readers)
    {
      msg = mus_format("%s inputs (%d) != sample-readers (%d)", pcLabel, readers, inchans);
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
  XEN_ASSERT_TYPE(XEN_LIST_LENGTH(ladspa_plugin_configuration) == 2 + lParameterCount,
		  ladspa_plugin_configuration,
		  XEN_ARG_2,
		  S_apply_ladspa, mus_format("a list of 2 strings + %d parameters", (int)lParameterCount));
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

  sf = (snd_fd **)CALLOC(readers, sizeof(snd_fd));

  /* Get sample count. */
  num = XEN_TO_C_INT(samples);

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
  psHandle = (void **)psDescriptor->instantiate(psDescriptor, lSampleRate);
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

  data = (MUS_SAMPLE_TYPE **)CALLOC(outchans, sizeof(MUS_SAMPLE_TYPE *));
  for (i = 0; i < outchans; i++)
    data[i] = (MUS_SAMPLE_TYPE *)CALLOC(MAX_BUFFER_SIZE, sizeof(MUS_SAMPLE_TYPE));

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
  ofile = snd_tempnam(state);

  /* Create initial header for output file, stealing info from input
     file. */
  hdr = make_temp_header(ofile,
			 SND_SRATE(sp),
			 outchans,
			 num,
			 XEN_TO_C_STRING(origin));

  /* Open the output file, using the header we've been working on. */
  ofd = open_temp_file(ofile, outchans, hdr, state);
  if (ofd == -1)
    XEN_ERROR(CANNOT_SAVE,
	      XEN_LIST_3(C_TO_XEN_STRING(S_apply_ladspa),
			 C_TO_XEN_STRING(ofile),
			 C_TO_XEN_STRING(strerror(errno))));

  /* Tidy up header. */
  datumb = mus_data_format_to_bytes_per_sample(hdr->format);

  if (psDescriptor->activate)
    psDescriptor->activate(psHandle);

  lAt = 0;
  while (lAt < num) {

    /* Decide how much audio to process this frame. */
    lBlockSize = num - lAt;
    if (lBlockSize > MAX_BUFFER_SIZE)
      lBlockSize = MAX_BUFFER_SIZE;

    /* Prepare the input data. */
    for (i = 0; i < readers; i++)
      for (lSampleIndex = 0; lSampleIndex < lBlockSize; lSampleIndex++) {
	pfInputBuffer[i][lSampleIndex] = next_sample_to_float(sf[i]);
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
    if (state->stopped_explicitly)
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
      update_graph(ncp, NULL);
      j++;
      if (j >= inchans) j = 0;
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

/*****************************************************************************/
#ifdef XEN_ARGIFY_1
XEN_NARGIFY_2(g_analyse_ladspa_w, g_analyse_ladspa)
XEN_NARGIFY_4(g_apply_ladspa_w, g_apply_ladspa)
XEN_NARGIFY_0(g_init_ladspa_w, g_init_ladspa)
XEN_NARGIFY_0(g_list_ladspa_w, g_list_ladspa)
#else
#define g_analyse_ladspa_w g_analyse_ladspa
#define g_apply_ladspa_w g_apply_ladspa
#define g_init_ladspa_w g_init_ladspa
#define g_list_ladspa_w g_list_ladspa
#endif

void g_ladspa_to_snd(void);
void g_ladspa_to_snd(void)
{
  XEN_DEFINE_PROCEDURE(S_analyse_ladspa, g_analyse_ladspa_w, 2, 0, 0, H_analyse_ladspa);
  XEN_DEFINE_PROCEDURE(S_apply_ladspa, g_apply_ladspa_w, 4, 0, 0, H_apply_ladspa);
  XEN_DEFINE_PROCEDURE(S_init_ladspa, g_init_ladspa_w, 0, 0, 0, H_init_ladspa);
  XEN_DEFINE_PROCEDURE(S_list_ladspa, g_list_ladspa_w, 0, 0, 0, H_list_ladspa);
  XEN_YES_WE_HAVE("snd-ladspa");
}

/*****************************************************************************/

#endif
