/*****************************************************************************/

/* Snd LADSPA Support - Copyright 2000 Richard W.E. Furse. */

/*****************************************************************************/

#include "snd.h"

#if HAVE_LADSPA

#include <dlfcn.h>
#include <ladspa.h>
#include <dirent.h>

/*****************************************************************************/

//FIXME: Repository is not threadsafe. Does this matter in Snd?

//FIXME: Memory checking is non-existent.

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

/* Snd currently does not support all LADSPA plugins. */
char
isLADSPAPluginSupported(const LADSPA_Descriptor * psDescriptor) {

  long lInputCount, lOutputCount, lIndex;
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

  /* Snd currently only supports mono plugins. */
  return (lInputCount == 1 && lOutputCount == 1);
}

/*****************************************************************************/

/* Assumes repository initialised, returns NULL if not found. */
const LADSPA_Descriptor *
findLADSPADescriptor(const char * pcPackedFilename, const char * pcLabel) {

  //FIXME: Could be using hashtables, binary chops etc. Instead we
  //simply scan the table.

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
char * packLADSPAFilename(const char * pcFilename) {

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

  pcPackedFilename = MALLOC(pcEnd - pcStart + 1);
  memcpy(pcPackedFilename, pcStart, pcEnd - pcStart);
  pcPackedFilename[pcEnd - pcStart] = '\0';

  return pcPackedFilename;
}

/*****************************************************************************/

static void unloadLADSPA() {

  long lIndex;
  void * pvPluginHandle;
  LADSPAPluginInfo * psInfo;

  if (g_lLADSPARepositoryCount > 0)
    pvPluginHandle = g_psLADSPARepository[0]->m_pvPluginHandle + 1;

  for (lIndex = 0; lIndex < g_lLADSPARepositoryCount; lIndex++) {
    psInfo = g_psLADSPARepository[lIndex];
    free(psInfo->m_pcPackedFilename);
    /* Don't free Label or Descriptor - this memory is owned by the
       relevant plugin library. */
    if (pvPluginHandle != psInfo->m_pvPluginHandle) {
      pvPluginHandle = psInfo->m_pvPluginHandle;
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
    if (isLADSPAPluginSupported(psDescriptor)) {
      if (g_lLADSPARepositoryCount == g_lLADSPARepositoryCapacity) {
	psOldRepository = g_psLADSPARepository;
	lNewCapacity = (g_lLADSPARepositoryCapacity
			+ LADSPA_REPOSITORY_CAPACITY_STEP);
	g_psLADSPARepository = MALLOC(sizeof(LADSPAPluginInfo *) * lNewCapacity);
	memcpy(g_psLADSPARepository,
	       psOldRepository,
	       sizeof(LADSPAPluginInfo *) * g_lLADSPARepositoryCount);
	g_lLADSPARepositoryCapacity = lNewCapacity;
	free(psOldRepository);
      }
      psInfo
	= g_psLADSPARepository[g_lLADSPARepositoryCount++]
	= MALLOC(sizeof(LADSPAPluginInfo));
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

  char * pcFilename;
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

    pcFilename = MALLOC(lDirLength
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
	dlclose(pcFilename);
      }
    }
  }
}

/*****************************************************************************/

static void loadLADSPA() {

  char * pcBuffer;
  const char * pcEnd;
  const char * pcLADSPAPath;
  const char * pcStart;

  g_bLADSPAInitialised = 1;
  g_psLADSPARepository = MALLOC(sizeof(LADSPAPluginInfo *)
				* LADSPA_REPOSITORY_CAPACITY_STEP);
  g_lLADSPARepositoryCapacity = LADSPA_REPOSITORY_CAPACITY_STEP;
  g_lLADSPARepositoryCount = 0;

  pcLADSPAPath = getenv("LADSPA_PATH");
  if (!pcLADSPAPath) {
    snd_warning(
	    "Warning: You do not have a LADSPA_PATH "
	    "environment variable set.\n");
    return;
  }

  pcStart = pcLADSPAPath;
  while (*pcStart != '\0') {
    pcEnd = pcStart;
    while (*pcEnd != ':' && *pcEnd != '\0')
      pcEnd++;

    pcBuffer = MALLOC(1 + pcEnd - pcStart);
    if (pcEnd > pcStart)
      strncpy(pcBuffer, pcStart, pcEnd - pcStart);
    pcBuffer[pcEnd - pcStart] = '\0';

    loadLADSPADirectory(pcBuffer);

    pcStart = pcEnd;
    if (*pcStart == ':')
      pcStart++;
  }

  //FIXME: It might be nice to qsort the data in the repository by
  //filename+label at this point to provide organised results from
  //list-ladspa.
}

/*****************************************************************************/

#define S_init_ladspa "init-ladspa"

static SCM g_init_ladspa() {

#define H_init_ladspa "This function reinitialises LADSPA. This is not \
normally necessary as LADSPA automatically initialises itself, however \
it can be useful when the plugins on the system have changed."

  if (g_bLADSPAInitialised)
    unloadLADSPA();

  loadLADSPA();

  return(SCM_BOOL_F);
}

/*****************************************************************************/

#define S_list_ladspa "list-ladspa"

static SCM g_list_ladspa() {

#define H_list_ladspa "This function returns a list of lists containing \
information of the LADSPA plugins currently available. For each plugin a \
list containing the plugin-file and plugin-label is included."

  long lIndex;
  SCM scmList, scmPluginList;
  LADSPAPluginInfo * psInfo;

  if (!g_bLADSPAInitialised)
    loadLADSPA();

  scmList = SCM_EOL;

  for (lIndex = g_lLADSPARepositoryCount - 1; lIndex >= 0; lIndex--) {
    psInfo = g_psLADSPARepository[lIndex];
    scmPluginList = CONS(TO_SCM_STRING(psInfo->m_pcPackedFilename),
			    CONS(TO_SCM_STRING((char *)psInfo->m_pcLabel),
				    SCM_EOL));
    scmList = CONS(scmPluginList, scmList);
  }

  return scmList;
}

/*****************************************************************************/

#define S_analyse_ladspa "analyse-ladspa"

static SCM g_analyse_ladspa(SCM ladspa_plugin_filename,
			    SCM ladspa_plugin_label) {

#define H_analyse_ladspa "This function returns a list of information about \
a LADSPA plugin. The plugin is identified by plugin-file and plugin-label \
(see list-ladspa). The items are: plugin-name, plugin-maker, \
plugin-copyright, plugin-parameter-list. The plugin-port-list contains a \
list of information for each parameter available. The first item in this \
list is the name of the port. Other hint information may follow this to help \
a user interface edit the parameter in a useful way. Note that only mono \
LADSPA plugins are supported by Snd at this time."

  long lIndex;
  const LADSPA_Descriptor * psDescriptor;
  char * pcFilename, * pcLabel, * pcTmp;
  SCM scmList, scmPortData;
  LADSPA_PortRangeHintDescriptor iHint;

  if (!g_bLADSPAInitialised)
    loadLADSPA();

  ASSERT_TYPE(STRING_P(ladspa_plugin_filename),
	     ladspa_plugin_filename,
	     ARG1,
	     S_analyse_ladspa, "a string");
  ASSERT_TYPE(STRING_P(ladspa_plugin_label),
	     ladspa_plugin_label,
	     ARG2,
	     S_analyse_ladspa, "a string");

  /* Plugin. */
  pcTmp = TO_NEW_C_STRING(ladspa_plugin_filename);
  pcLabel = TO_NEW_C_STRING(ladspa_plugin_label);
  pcFilename = packLADSPAFilename(pcTmp);
  free(pcTmp);

  psDescriptor = findLADSPADescriptor(pcFilename, pcLabel);
  free(pcFilename);
  free(pcLabel);

  if (!psDescriptor) {
    snd_error("Plugin unknown.\n"); /* or we could return UNKNOWN_PLUGIN */
    return(SCM_BOOL_F);
  }

  scmList = SCM_EOL;
  for (lIndex = psDescriptor->PortCount - 1; lIndex >= 0; lIndex--)
    if (LADSPA_IS_PORT_CONTROL(psDescriptor->PortDescriptors[lIndex])
	&& LADSPA_IS_PORT_INPUT(psDescriptor->PortDescriptors[lIndex])) {

      iHint = psDescriptor->PortRangeHints[lIndex].HintDescriptor;

      scmPortData = SCM_EOL;
      if (LADSPA_IS_HINT_TOGGLED(iHint))
	scmPortData = CONS(TO_SCM_STRING("toggle"), scmPortData);
      if (LADSPA_IS_HINT_LOGARITHMIC(iHint))
	scmPortData = CONS(TO_SCM_STRING("logarithmic"), scmPortData);
      if (LADSPA_IS_HINT_INTEGER(iHint))
	scmPortData = CONS(TO_SCM_STRING("integer"), scmPortData);
      if (LADSPA_IS_HINT_SAMPLE_RATE(iHint))
	scmPortData = CONS(TO_SCM_STRING("sample_rate"), scmPortData);
      if (LADSPA_IS_HINT_BOUNDED_ABOVE(iHint))
	scmPortData = CONS(TO_SCM_STRING("maximum"),
			 
     CONS(TO_SCM_DOUBLE(psDescriptor->PortRangeHints[lIndex].UpperBound),
				      scmPortData));
      if (LADSPA_IS_HINT_BOUNDED_BELOW(iHint))
	scmPortData = CONS(TO_SCM_STRING("minimum"),
			      CONS(TO_SCM_DOUBLE(psDescriptor->PortRangeHints[lIndex].LowerBound),
				      scmPortData));
      scmPortData = CONS(TO_SCM_STRING((char *)psDescriptor->PortNames[lIndex]),
			    scmPortData);
      scmList = CONS(scmPortData, scmList);
    }

  scmList = CONS(TO_SCM_STRING((char *)psDescriptor->Name),
		    CONS(TO_SCM_STRING((char *)psDescriptor->Maker),
			    CONS(TO_SCM_STRING((char *)psDescriptor->Copyright),
				    CONS(scmList, SCM_EOL))));
  return scmList;
}

/*****************************************************************************/

snd_fd *get_sf(SCM obj);

//FIXME: We could improve this function to receive a list of plugin
//configurations for chain processing. Also, is multiple channel
//support possible?

#define S_apply_ladspa "apply-ladspa"

static SCM g_apply_ladspa(SCM reader,
			  SCM ladspa_plugin_configuration,
			  SCM samples,
			  SCM origin)
{
#define H_apply_ladspa "This function applies a LADSPA plugin to process a \
sound. The parameters are soundfile-reader, a ladspa-plugin-configuration, \
the number of samples to process, and an `origin' for edit lists. The \
ladspa-plugin-configuration is a list containing the plugin-file and \
plugin-label for the LADSPA plugin (as provided by list-ladspa) followed \
by any arguments. (Information about about parameters can be acquired using analyse-ladspa.)"

  const LADSPA_Descriptor * psDescriptor;
  char * pcFilename, * pcLabel, * pcTmp;
  LADSPA_Handle * psHandle;
  unsigned long lSampleRate, lPortIndex, lAt, lBlockSize, lSampleIndex;
  unsigned long lParameterCount;
  SCM scmParameters;
  LADSPA_PortDescriptor iPortDescriptor;

  LADSPA_Data * pfControls;
  LADSPA_Data pfInputBuffer[MAX_BUFFER_SIZE];
  LADSPA_Data * pfOutputBuffer;

#if (!SNDLIB_USE_FLOATS)
  LADSPA_Data pfBuffer2[MAX_BUFFER_SIZE];
#endif

  chan_info *cp;
  snd_info *sp;
  char *ofile;
  int num, i, j = 0, ofd, datumb, err = 0;
  snd_fd *sf;
  file_info *hdr;
  snd_state *state;
  MUS_SAMPLE_TYPE **data;
  MUS_SAMPLE_TYPE *idata;
#if (!SNDLIB_USE_FLOATS)
  MUS_SAMPLE_TYPE val;
#endif

  state = get_global_state();
  if (!g_bLADSPAInitialised)
    loadLADSPA();

  /* First parameter should be a file reader. */
  ASSERT_TYPE(sf_p(reader),
	     reader,
	     ARG1,
	     S_apply_ladspa, "a sample-reader");
  /* Second parameter should be a list of two strings, then any number
     (inc 0) of numbers. */
  //FIXME: uninformative error.
  ASSERT_TYPE(LIST_LENGTH(ladspa_plugin_configuration) >= 2,
	     ladspa_plugin_configuration,
	     ARG2,
	     S_apply_ladspa, "a list");
  //FIXME: uninformative error.
  ASSERT_TYPE(STRING_P(CAR(ladspa_plugin_configuration)),
	     ladspa_plugin_configuration,
	     ARG2,
	     S_apply_ladspa, "a string");
  //FIXME: uninformative error.
  ASSERT_TYPE(STRING_P(CAR(CDR(ladspa_plugin_configuration))),
	     ladspa_plugin_configuration,
	     ARG2,
	     S_apply_ladspa, "a string");

  /* Third parameter is the number of samples to process. */
  ASSERT_TYPE(NUMBER_P(samples),
	     samples,
	     ARG3,
	     S_apply_ladspa, "a number");
  /* The fourth parameter is a tag to identify the edit. */
  ASSERT_TYPE(STRING_P(origin),
	     origin,
	     ARG4,
	     S_apply_ladspa, "a string");

  /* Get sample count. */
  num = TO_C_INT(samples);

  /* Local version of sound descriptor. */
  sf = get_sf(reader);

  /* Channel info structure. */
  cp = sf->cp;

  /* Sound information. */
  sp = (cp->sound);

  /* Plugin. */
  pcTmp = TO_NEW_C_STRING(CAR(ladspa_plugin_configuration));
  pcLabel = TO_NEW_C_STRING(CAR(CDR(ladspa_plugin_configuration)));
  pcFilename = packLADSPAFilename(pcTmp);
  free(pcTmp);

  psDescriptor = findLADSPADescriptor(pcFilename, pcLabel);
  free(pcFilename);
  free(pcLabel);

  if (!psDescriptor) {
    snd_error("Plugin unknown.\n");
    //FIXME: How to report?
    return(SCM_BOOL_F);
  }

  //FIXME: uninformative errors.
  lParameterCount = 0;
  for (lPortIndex = 0; lPortIndex < psDescriptor->PortCount; lPortIndex++)
    if (LADSPA_IS_PORT_CONTROL(psDescriptor->PortDescriptors[lPortIndex])
	&& LADSPA_IS_PORT_INPUT(psDescriptor->PortDescriptors[lPortIndex]))
      lParameterCount++;
  ASSERT_TYPE(LIST_LENGTH(ladspa_plugin_configuration) == 2 + lParameterCount,
	     ladspa_plugin_configuration,
	     ARG2,
	     S_apply_ladspa, "a list");
  pfControls = MALLOC(psDescriptor->PortCount * sizeof(LADSPA_Data));

  /* Get parameters. */
  scmParameters = CDR(CDR(ladspa_plugin_configuration));
  for (lPortIndex = 0; lPortIndex < psDescriptor->PortCount; lPortIndex++) 
{
    iPortDescriptor = psDescriptor->PortDescriptors[lPortIndex];
    if (LADSPA_IS_PORT_CONTROL(iPortDescriptor)
	&& LADSPA_IS_PORT_INPUT(iPortDescriptor)) {
      //FIXME: uninformative error.
      ASSERT_TYPE(NUMBER_P(CAR(scmParameters)),
		 ladspa_plugin_configuration,
		 ARG2,
		 S_apply_ladspa, "a number");
      pfControls[lPortIndex]
	= (LADSPA_Data)TO_C_DOUBLE(CAR(scmParameters));
      scmParameters = CDR(scmParameters);
    }
  }

  lSampleRate = (unsigned long)(sp->hdr->srate);
  psHandle = psDescriptor->instantiate(psDescriptor, lSampleRate);
  if (!psHandle) {
    snd_error("Plugin did not instantiate.\n");
    //FIXME: How to report?
    return(SCM_BOOL_F);
  }

  /* Allocate buffer to work with (data[0] is an audio buffer). */
  data = (MUS_SAMPLE_TYPE **)CALLOC(1, sizeof(MUS_SAMPLE_TYPE *));
  data[0] = (MUS_SAMPLE_TYPE *)CALLOC(MAX_BUFFER_SIZE, sizeof(MUS_SAMPLE_TYPE));

#if SNDLIB_USE_FLOATS
  pfOutputBuffer = data[0];
#else
  if (LADSPA_IS_INPLACE_BROKEN(psDescriptor->Properties))
    pfOutputBuffer = pfBuffer2;
  else
    pfOutputBuffer = pfInputBuffer;
#endif

  /* Connect input and output control ports. */
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
				   pfInputBuffer);
      else
	psDescriptor->connect_port(psHandle,
				   lPortIndex,
				   pfOutputBuffer);
    }
  }

  /* Temporary file name. */
  ofile = snd_tempnam(state);

  /* Create initial header for output file, stealing info from input
     file. */
  hdr = make_temp_header(ofile,
			 SND_SRATE(sp),
			 1,
			 num,
			 TO_C_STRING(origin));

  /* Open the output file, using the header we've been working on. */
  ofd = open_temp_file(ofile, 1, hdr, state);

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
    for (lSampleIndex = 0; lSampleIndex < lBlockSize; lSampleIndex++) {
     pfInputBuffer[lSampleIndex] = next_sample_to_float(sf);
    }

    /* Run the plugin. */
    psDescriptor->run(psHandle, lBlockSize);

#if SNDLIB_USE_FLOATS
    /* Data was written direct to data[0]. */
#else
    /* Prepare the output data. */
    for (lSampleIndex = 0; lSampleIndex < lBlockSize; lSampleIndex++)
      data[0][lSampleIndex]
	= MUS_FLOAT_TO_SAMPLE(pfOutputBuffer[lSampleIndex]);
#endif

    /* Send the output data to the outside world. */
    err = mus_file_write(ofd,
			 0,
			 lBlockSize - 1,
			 1,
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
		  num * datumb,
		  sp);

  /* Discard tmp header. */
  hdr = free_file_info(hdr);

  file_change_samples(sf->initial_samp,
		      num,
		      ofile,
		      cp,
		      0,
		      DELETE_ME, LOCK_MIXES,
		      TO_NEW_C_STRING(origin));

  update_graph(cp, NULL); /* is this needed? */

  if (ofile) FREE(ofile);
  FREE(data[0]);
  FREE(data);

  return(SCM_BOOL_F);
}

/*****************************************************************************/
void g_ladspa_to_snd(SCM local_doc);
void g_ladspa_to_snd(SCM local_doc)
{
  DEFINE_PROC(S_analyse_ladspa, g_analyse_ladspa, 2, 0, 0, H_analyse_ladspa);
  DEFINE_PROC(S_apply_ladspa, g_apply_ladspa, 4, 0, 0, H_apply_ladspa);
  DEFINE_PROC(S_init_ladspa, g_init_ladspa, 0, 0, 0, H_init_ladspa);
  DEFINE_PROC(S_list_ladspa, g_list_ladspa, 0, 0, 0, H_list_ladspa);
  YES_WE_HAVE("snd-ladspa");
}

/*****************************************************************************/

#endif
