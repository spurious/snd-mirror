/* this is so ugly I can't bear to include it in xm.c
 */

#if HAVE_XP
  XEN_NARGIFY_2(gxm_XpStartPage_w, gxm_XpStartPage)
  XEN_NARGIFY_1(gxm_XpEndPage_w, gxm_XpEndPage)
  XEN_NARGIFY_2(gxm_XpCancelPage_w, gxm_XpCancelPage)
  XEN_NARGIFY_2(gxm_XpStartJob_w, gxm_XpStartJob)
  XEN_NARGIFY_1(gxm_XpEndJob_w, gxm_XpEndJob)
  XEN_NARGIFY_2(gxm_XpCancelJob_w, gxm_XpCancelJob)
  XEN_NARGIFY_2(gxm_XpStartDoc_w, gxm_XpStartDoc)
  XEN_NARGIFY_1(gxm_XpEndDoc_w, gxm_XpEndDoc)
  XEN_NARGIFY_2(gxm_XpCancelDoc_w, gxm_XpCancelDoc)
  XEN_NARGIFY_1(gxm_XpRehashPrinterList_w, gxm_XpRehashPrinterList)
  XEN_NARGIFY_2(gxm_XpCreateContext_w, gxm_XpCreateContext)
  XEN_NARGIFY_2(gxm_XpSetContext_w, gxm_XpSetContext)
  XEN_NARGIFY_1(gxm_XpGetContext_w, gxm_XpGetContext)
  XEN_NARGIFY_2(gxm_XpDestroyContext_w, gxm_XpDestroyContext)
  XEN_NARGIFY_0(gxm_XpGetLocaleNetString_w, gxm_XpGetLocaleNetString)
  XEN_NARGIFY_6(gxm_XpNotifyPdm_w, gxm_XpNotifyPdm)
  XEN_NARGIFY_2(gxm_XpSendAuth_w, gxm_XpSendAuth)
  XEN_NARGIFY_2(gxm_XpGetImageResolution_w, gxm_XpGetImageResolution)
  XEN_NARGIFY_3(gxm_XpGetAttributes_w, gxm_XpGetAttributes)
  XEN_NARGIFY_5(gxm_XpSetAttributes_w, gxm_XpSetAttributes)
  XEN_NARGIFY_4(gxm_XpGetOneAttribute_w, gxm_XpGetOneAttribute)
  XEN_NARGIFY_2(gxm_XpGetScreenOfContext_w, gxm_XpGetScreenOfContext)
  XEN_NARGIFY_1(gxm_XpFreePrinterList_w, gxm_XpFreePrinterList)
  XEN_NARGIFY_1(gxm_XpQueryVersion_w, gxm_XpQueryVersion)
  XEN_NARGIFY_1(gxm_XpQueryExtension_w, gxm_XpQueryExtension)
  XEN_NARGIFY_1(gxm_XpQueryScreens_w, gxm_XpQueryScreens)
  XEN_NARGIFY_5(gxm_XpGetPdmStartParams_w, gxm_XpGetPdmStartParams)
  XEN_NARGIFY_2(gxm_XpGetAuthParams_w, gxm_XpGetAuthParams)
  XEN_NARGIFY_4(gxm_XpSendOneTicket_w, gxm_XpSendOneTicket)
  XEN_NARGIFY_2(gxm_XpGetPageDimensions_w, gxm_XpGetPageDimensions)
  XEN_NARGIFY_4(gxm_XpSetImageResolution_w, gxm_XpSetImageResolution)
  XEN_NARGIFY_2(gxm_XpGetPrinterList_w, gxm_XpGetPrinterList)
  XEN_NARGIFY_3(gxm_XpSelectInput_w, gxm_XpSelectInput)
  XEN_NARGIFY_3(gxm_XpInputSelected_w, gxm_XpInputSelected)
  XEN_NARGIFY_6(gxm_XpPutDocumentData_w, gxm_XpPutDocumentData)
  XEN_NARGIFY_5(gxm_XpGetDocumentData_w, gxm_XpGetDocumentData)
#endif
#if (!XM_DISABLE_DEPRECATED)
  XEN_NARGIFY_1(gxm_XtWarning_w, gxm_XtWarning)
  XEN_NARGIFY_2(gxm_XtAppWarning_w, gxm_XtAppWarning)
  XEN_NARGIFY_1(gxm_XtSetWarningMsgHandler_w, gxm_XtSetWarningMsgHandler)
  XEN_NARGIFY_6(gxm_XtInitialize_w, gxm_XtInitialize)
  XEN_NARGIFY_1(gxm_XtSetWarningHandler_w, gxm_XtSetWarningHandler)
  XEN_NARGIFY_1(gxm_XtError_w, gxm_XtError)
  XEN_NARGIFY_6(gxm_XtErrorMsg_w, gxm_XtErrorMsg)
  XEN_NARGIFY_6(gxm_XtWarningMsg_w, gxm_XtWarningMsg)
  XEN_NARGIFY_1(gxm_XtSetErrorMsgHandler_w, gxm_XtSetErrorMsgHandler)
  XEN_NARGIFY_1(gxm_XtSetErrorHandler_w, gxm_XtSetErrorHandler)
  XEN_ARGIFY_4(gxm_XtCreateApplicationShell_w, gxm_XtCreateApplicationShell)
  XEN_NARGIFY_1(gxm_XtSetSelectionTimeout_w, gxm_XtSetSelectionTimeout)
  XEN_NARGIFY_0(gxm_XtGetSelectionTimeout_w, gxm_XtGetSelectionTimeout)
  XEN_NARGIFY_1(gxm_XtAddActions_w, gxm_XtAddActions)
  XEN_ARGIFY_4(gxm_XtAddInput_w, gxm_XtAddInput)
  XEN_ARGIFY_2(gxm_XtAddWorkProc_w, gxm_XtAddWorkProc)
  XEN_ARGIFY_3(gxm_XtAddTimeOut_w, gxm_XtAddTimeOut)
  XEN_NARGIFY_0(gxm_XtMainLoop_w, gxm_XtMainLoop)
  XEN_NARGIFY_1(gxm_XtProcessEvent_w, gxm_XtProcessEvent)
  XEN_NARGIFY_0(gxm_XtPending_w, gxm_XtPending)
  XEN_NARGIFY_0(gxm_XtNextEvent_w, gxm_XtNextEvent)
  XEN_NARGIFY_0(gxm_XtPeekEvent_w, gxm_XtPeekEvent)
#endif
#if HAVE_MOTIF
  XEN_NARGIFY_3(gxm_XtSetArg_w, gxm_XtSetArg)
  XEN_NARGIFY_2(gxm_XtManageChildren_w, gxm_XtManageChildren)
  XEN_NARGIFY_1(gxm_XtManageChild_w, gxm_XtManageChild)
  XEN_NARGIFY_2(gxm_XtUnmanageChildren_w, gxm_XtUnmanageChildren)
  XEN_NARGIFY_1(gxm_XtUnmanageChild_w, gxm_XtUnmanageChild)
  XEN_NARGIFY_1(gxm_XtDispatchEvent_w, gxm_XtDispatchEvent)
  XEN_NARGIFY_2(gxm_XtCallAcceptFocus_w, gxm_XtCallAcceptFocus)
  XEN_NARGIFY_1(gxm_XtAppPeekEvent_w, gxm_XtAppPeekEvent)
#if MOTIF_2
  XEN_NARGIFY_2(gxm_XtIsSubclass_w, gxm_XtIsSubclass)
#endif
  XEN_NARGIFY_1(gxm_XtIsObject_w, gxm_XtIsObject)
  XEN_NARGIFY_1(gxm_XtIsManaged_w, gxm_XtIsManaged)
  XEN_NARGIFY_1(gxm_XtIsRealized_w, gxm_XtIsRealized)
  XEN_NARGIFY_1(gxm_XtIsSensitive_w, gxm_XtIsSensitive)
  XEN_NARGIFY_6(gxm_XtOwnSelection_w, gxm_XtOwnSelection)
  XEN_NARGIFY_8(gxm_XtOwnSelectionIncremental_w, gxm_XtOwnSelectionIncremental)
  XEN_NARGIFY_3(gxm_XtMakeResizeRequest_w, gxm_XtMakeResizeRequest)
  XEN_NARGIFY_3(gxm_XtTranslateCoords_w, gxm_XtTranslateCoords)
  XEN_NARGIFY_2(gxm_XtKeysymToKeycodeList_w, gxm_XtKeysymToKeycodeList)
  XEN_NARGIFY_1(gxm_XtParseTranslationTable_w, gxm_XtParseTranslationTable)
  XEN_NARGIFY_1(gxm_XtParseAcceleratorTable_w, gxm_XtParseAcceleratorTable)
  XEN_NARGIFY_2(gxm_XtOverrideTranslations_w, gxm_XtOverrideTranslations)
  XEN_NARGIFY_2(gxm_XtAugmentTranslations_w, gxm_XtAugmentTranslations)
  XEN_NARGIFY_2(gxm_XtInstallAccelerators_w, gxm_XtInstallAccelerators)
  XEN_NARGIFY_2(gxm_XtInstallAllAccelerators_w, gxm_XtInstallAllAccelerators)
  XEN_NARGIFY_1(gxm_XtUninstallTranslations_w, gxm_XtUninstallTranslations)
  XEN_NARGIFY_2(gxm_XtAppAddActions_w, gxm_XtAppAddActions)
  XEN_ARGIFY_3(gxm_XtAppAddActionHook_w, gxm_XtAppAddActionHook)
  XEN_NARGIFY_1(gxm_XtRemoveActionHook_w, gxm_XtRemoveActionHook)
  XEN_NARGIFY_1(gxm_XtGetActionList_w, gxm_XtGetActionList)
  XEN_NARGIFY_5(gxm_XtCallActionProc_w, gxm_XtCallActionProc)
  XEN_NARGIFY_5(gxm_XtRegisterGrabAction_w, gxm_XtRegisterGrabAction)
  XEN_NARGIFY_2(gxm_XtSetMultiClickTime_w, gxm_XtSetMultiClickTime)
  XEN_NARGIFY_1(gxm_XtGetMultiClickTime_w, gxm_XtGetMultiClickTime)
  XEN_NARGIFY_1(gxm_XtGetActionKeysym_w, gxm_XtGetActionKeysym)
  XEN_NARGIFY_3(gxm_XtTranslateKeycode_w, gxm_XtTranslateKeycode)
  XEN_NARGIFY_3(gxm_XtTranslateKey_w, gxm_XtTranslateKey)
  XEN_NARGIFY_2(gxm_XtSetKeyTranslator_w, gxm_XtSetKeyTranslator)
  XEN_NARGIFY_4(gxm_XtRegisterCaseConverter_w, gxm_XtRegisterCaseConverter)
  XEN_NARGIFY_2(gxm_XtConvertCase_w, gxm_XtConvertCase)
  XEN_ARGIFY_5(gxm_XtAddEventHandler_w, gxm_XtAddEventHandler)
  XEN_NARGIFY_5(gxm_XtRemoveEventHandler_w, gxm_XtRemoveEventHandler)
  XEN_NARGIFY_5(gxm_XtAddRawEventHandler_w, gxm_XtAddRawEventHandler)
  XEN_NARGIFY_5(gxm_XtRemoveRawEventHandler_w, gxm_XtRemoveRawEventHandler)
  XEN_NARGIFY_6(gxm_XtInsertEventHandler_w, gxm_XtInsertEventHandler)
  XEN_NARGIFY_6(gxm_XtInsertRawEventHandler_w, gxm_XtInsertRawEventHandler)
  XEN_NARGIFY_2(gxm_XtDispatchEventToWidget_w, gxm_XtDispatchEventToWidget)
  XEN_NARGIFY_1(gxm_XtBuildEventMask_w, gxm_XtBuildEventMask)
  XEN_NARGIFY_3(gxm_XtAddGrab_w, gxm_XtAddGrab)
  XEN_NARGIFY_1(gxm_XtRemoveGrab_w, gxm_XtRemoveGrab)
  XEN_NARGIFY_2(gxm_XtAppProcessEvent_w, gxm_XtAppProcessEvent)
  XEN_NARGIFY_1(gxm_XtAppMainLoop_w, gxm_XtAppMainLoop)
  XEN_NARGIFY_2(gxm_XtAddExposureToRegion_w, gxm_XtAddExposureToRegion)
  XEN_NARGIFY_2(gxm_XtSetKeyboardFocus_w, gxm_XtSetKeyboardFocus)
  XEN_NARGIFY_1(gxm_XtGetKeyboardFocusWidget_w, gxm_XtGetKeyboardFocusWidget)
  XEN_NARGIFY_1(gxm_XtLastEventProcessed_w, gxm_XtLastEventProcessed)
  XEN_NARGIFY_1(gxm_XtLastTimestampProcessed_w, gxm_XtLastTimestampProcessed)
  XEN_ARGIFY_4(gxm_XtAppAddTimeOut_w, gxm_XtAppAddTimeOut)
  XEN_NARGIFY_1(gxm_XtRemoveTimeOut_w, gxm_XtRemoveTimeOut)
  XEN_ARGIFY_5(gxm_XtAppAddInput_w, gxm_XtAppAddInput)
  XEN_NARGIFY_1(gxm_XtRemoveInput_w, gxm_XtRemoveInput)
  XEN_NARGIFY_1(gxm_XtAppNextEvent_w, gxm_XtAppNextEvent)
  XEN_NARGIFY_1(gxm_XtAppPending_w, gxm_XtAppPending)
  XEN_NARGIFY_1(gxm_XtRealizeWidget_w, gxm_XtRealizeWidget)
  XEN_NARGIFY_1(gxm_XtUnrealizeWidget_w, gxm_XtUnrealizeWidget)
  XEN_NARGIFY_1(gxm_XtDestroyWidget_w, gxm_XtDestroyWidget)
  XEN_NARGIFY_2(gxm_XtSetSensitive_w, gxm_XtSetSensitive)
  XEN_NARGIFY_2(gxm_XtNameToWidget_w, gxm_XtNameToWidget)
  XEN_NARGIFY_2(gxm_XtWindowToWidget_w, gxm_XtWindowToWidget)
  XEN_NARGIFY_4(gxm_XtMergeArgLists_w, gxm_XtMergeArgLists)
  XEN_NARGIFY_2(gxm_XtVaCreateArgsList_w, gxm_XtVaCreateArgsList)
  XEN_NARGIFY_1(gxm_XtDisplay_w, gxm_XtDisplay)
  XEN_NARGIFY_1(gxm_XtDisplayOfObject_w, gxm_XtDisplayOfObject)
  XEN_NARGIFY_1(gxm_XtScreen_w, gxm_XtScreen)
  XEN_NARGIFY_1(gxm_XtScreenOfObject_w, gxm_XtScreenOfObject)
  XEN_NARGIFY_1(gxm_XtWindow_w, gxm_XtWindow)
  XEN_NARGIFY_1(gxm_XtWindowOfObject_w, gxm_XtWindowOfObject)
  XEN_NARGIFY_1(gxm_XtName_w, gxm_XtName)
  XEN_NARGIFY_1(gxm_XtSuperclass_w, gxm_XtSuperclass)
  XEN_NARGIFY_1(gxm_XtClass_w, gxm_XtClass)
  XEN_NARGIFY_1(gxm_XtParent_w, gxm_XtParent)
  XEN_ARGIFY_4(gxm_XtAddCallback_w, gxm_XtAddCallback)
  XEN_NARGIFY_3(gxm_XtRemoveCallback_w, gxm_XtRemoveCallback)
  XEN_NARGIFY_3(gxm_XtAddCallbacks_w, gxm_XtAddCallbacks)
  XEN_NARGIFY_3(gxm_XtRemoveCallbacks_w, gxm_XtRemoveCallbacks)
  XEN_NARGIFY_2(gxm_XtRemoveAllCallbacks_w, gxm_XtRemoveAllCallbacks)
  XEN_NARGIFY_3(gxm_XtCallCallbacks_w, gxm_XtCallCallbacks)
  XEN_NARGIFY_3(gxm_XtCallCallbackList_w, gxm_XtCallCallbackList)
  XEN_NARGIFY_2(gxm_XtHasCallbacks_w, gxm_XtHasCallbacks)
  XEN_ARGIFY_5(gxm_XtCreatePopupShell_w, gxm_XtCreatePopupShell)
  XEN_NARGIFY_4(gxm_XtVaCreatePopupShell_w, gxm_XtVaCreatePopupShell)
  XEN_NARGIFY_2(gxm_XtPopup_w, gxm_XtPopup)
  XEN_NARGIFY_1(gxm_XtPopupSpringLoaded_w, gxm_XtPopupSpringLoaded)
  XEN_NARGIFY_3(gxm_XtCallbackNone_w, gxm_XtCallbackNone)
  XEN_NARGIFY_3(gxm_XtCallbackNonexclusive_w, gxm_XtCallbackNonexclusive)
  XEN_NARGIFY_3(gxm_XtCallbackExclusive_w, gxm_XtCallbackExclusive)
  XEN_NARGIFY_1(gxm_XtPopdown_w, gxm_XtPopdown)
  XEN_NARGIFY_3(gxm_XtCallbackPopdown_w, gxm_XtCallbackPopdown)
  XEN_ARGIFY_5(gxm_XtCreateWidget_w, gxm_XtCreateWidget)
  XEN_ARGIFY_5(gxm_XtCreateManagedWidget_w, gxm_XtCreateManagedWidget)
  XEN_NARGIFY_4(gxm_XtVaCreateWidget_w, gxm_XtVaCreateWidget)
  XEN_NARGIFY_4(gxm_XtVaCreateManagedWidget_w, gxm_XtVaCreateManagedWidget)
  XEN_ARGIFY_6(gxm_XtAppCreateShell_w, gxm_XtAppCreateShell)
  XEN_NARGIFY_5(gxm_XtVaAppCreateShell_w, gxm_XtVaAppCreateShell)
  XEN_NARGIFY_0(gxm_XtToolkitInitialize_w, gxm_XtToolkitInitialize)
  XEN_NARGIFY_3(gxm_XtSetLanguageProc_w, gxm_XtSetLanguageProc)
  XEN_NARGIFY_8(gxm_XtDisplayInitialize_w, gxm_XtDisplayInitialize)
  XEN_NARGIFY_9(gxm_XtOpenApplication_w, gxm_XtOpenApplication)
  XEN_NARGIFY_8(gxm_XtVaOpenApplication_w, gxm_XtVaOpenApplication)
  XEN_NARGIFY_8(gxm_XtAppInitialize_w, gxm_XtAppInitialize)
  XEN_NARGIFY_7(gxm_XtVaAppInitialize_w, gxm_XtVaAppInitialize)
  XEN_NARGIFY_8(gxm_XtOpenDisplay_w, gxm_XtOpenDisplay)
  XEN_NARGIFY_0(gxm_XtCreateApplicationContext_w, gxm_XtCreateApplicationContext)
  XEN_NARGIFY_1(gxm_XtDestroyApplicationContext_w, gxm_XtDestroyApplicationContext)
  XEN_NARGIFY_1(gxm_XtInitializeWidgetClass_w, gxm_XtInitializeWidgetClass)
  XEN_NARGIFY_1(gxm_XtWidgetToApplicationContext_w, gxm_XtWidgetToApplicationContext)
  XEN_NARGIFY_1(gxm_XtDisplayToApplicationContext_w, gxm_XtDisplayToApplicationContext)
  XEN_NARGIFY_1(gxm_XtCloseDisplay_w, gxm_XtCloseDisplay)
  XEN_ARGIFY_3(gxm_XtSetValues_w, gxm_XtSetValues)
  XEN_NARGIFY_2(gxm_XtVaSetValues_w, gxm_XtVaSetValues)
  XEN_ARGIFY_3(gxm_XtGetValues_w, gxm_XtGetValues)
  XEN_NARGIFY_2(gxm_XtVaGetValues_w, gxm_XtVaGetValues)
  XEN_NARGIFY_2(gxm_XtAppSetErrorMsgHandler_w, gxm_XtAppSetErrorMsgHandler)
  XEN_NARGIFY_2(gxm_XtAppSetWarningMsgHandler_w, gxm_XtAppSetWarningMsgHandler)
  XEN_NARGIFY_7(gxm_XtAppErrorMsg_w, gxm_XtAppErrorMsg)
  XEN_NARGIFY_7(gxm_XtAppWarningMsg_w, gxm_XtAppWarningMsg)
  XEN_NARGIFY_2(gxm_XtAppSetErrorHandler_w, gxm_XtAppSetErrorHandler)
  XEN_NARGIFY_2(gxm_XtAppSetWarningHandler_w, gxm_XtAppSetWarningHandler)
  XEN_NARGIFY_2(gxm_XtAppError_w, gxm_XtAppError)
  XEN_NARGIFY_1(gxm_XtMalloc_w, gxm_XtMalloc)
  XEN_NARGIFY_2(gxm_XtCalloc_w, gxm_XtCalloc)
  XEN_NARGIFY_2(gxm_XtRealloc_w, gxm_XtRealloc)
  XEN_NARGIFY_1(gxm_XtFree_w, gxm_XtFree)
  XEN_ARGIFY_3(gxm_XtAppAddWorkProc_w, gxm_XtAppAddWorkProc)
  XEN_NARGIFY_1(gxm_XtRemoveWorkProc_w, gxm_XtRemoveWorkProc)
  XEN_NARGIFY_3(gxm_XtGetGC_w, gxm_XtGetGC)
  XEN_NARGIFY_6(gxm_XtAllocateGC_w, gxm_XtAllocateGC)
  XEN_NARGIFY_1(gxm_XtDestroyGC_w, gxm_XtDestroyGC)
  XEN_NARGIFY_2(gxm_XtReleaseGC_w, gxm_XtReleaseGC)
  XEN_NARGIFY_3(gxm_XtSetWMColormapWindows_w, gxm_XtSetWMColormapWindows)
  XEN_NARGIFY_4(gxm_XtFindFile_w, gxm_XtFindFile)
  XEN_NARGIFY_8(gxm_XtResolvePathname_w, gxm_XtResolvePathname)
  XEN_NARGIFY_3(gxm_XtDisownSelection_w, gxm_XtDisownSelection)
  XEN_NARGIFY_6(gxm_XtGetSelectionValue_w, gxm_XtGetSelectionValue)
  XEN_NARGIFY_7(gxm_XtGetSelectionValues_w, gxm_XtGetSelectionValues)
  XEN_NARGIFY_2(gxm_XtAppSetSelectionTimeout_w, gxm_XtAppSetSelectionTimeout)
  XEN_NARGIFY_1(gxm_XtAppGetSelectionTimeout_w, gxm_XtAppGetSelectionTimeout)
  XEN_NARGIFY_3(gxm_XtGetSelectionRequest_w, gxm_XtGetSelectionRequest)
  XEN_NARGIFY_6(gxm_XtGetSelectionValueIncremental_w, gxm_XtGetSelectionValueIncremental)
  XEN_NARGIFY_7(gxm_XtGetSelectionValuesIncremental_w, gxm_XtGetSelectionValuesIncremental)
  XEN_NARGIFY_2(gxm_XtCreateSelectionRequest_w, gxm_XtCreateSelectionRequest)
  XEN_NARGIFY_3(gxm_XtSendSelectionRequest_w, gxm_XtSendSelectionRequest)
  XEN_NARGIFY_2(gxm_XtCancelSelectionRequest_w, gxm_XtCancelSelectionRequest)
  XEN_NARGIFY_6(gxm_XtGrabKey_w, gxm_XtGrabKey)
  XEN_NARGIFY_3(gxm_XtUngrabKey_w, gxm_XtUngrabKey)
  XEN_NARGIFY_5(gxm_XtGrabKeyboard_w, gxm_XtGrabKeyboard)
  XEN_NARGIFY_2(gxm_XtUngrabKeyboard_w, gxm_XtUngrabKeyboard)
  XEN_NARGIFY_9(gxm_XtGrabButton_w, gxm_XtGrabButton)
  XEN_NARGIFY_3(gxm_XtUngrabButton_w, gxm_XtUngrabButton)
  XEN_NARGIFY_8(gxm_XtGrabPointer_w, gxm_XtGrabPointer)
  XEN_NARGIFY_2(gxm_XtUngrabPointer_w, gxm_XtUngrabPointer)
  XEN_NARGIFY_1(gxm_XtGetApplicationNameAndClass_w, gxm_XtGetApplicationNameAndClass)
  XEN_NARGIFY_1(gxm_XtGetDisplays_w, gxm_XtGetDisplays)
  XEN_NARGIFY_0(gxm_XtToolkitThreadInitialize_w, gxm_XtToolkitThreadInitialize)
  XEN_NARGIFY_1(gxm_XtAppLock_w, gxm_XtAppLock)
  XEN_NARGIFY_1(gxm_XtAppUnlock_w, gxm_XtAppUnlock)
  XEN_NARGIFY_1(gxm_XtIsRectObj_w, gxm_XtIsRectObj)
  XEN_NARGIFY_1(gxm_XtIsWidget_w, gxm_XtIsWidget)
  XEN_NARGIFY_1(gxm_XtIsComposite_w, gxm_XtIsComposite)
  XEN_NARGIFY_1(gxm_XtIsConstraint_w, gxm_XtIsConstraint)
  XEN_NARGIFY_1(gxm_XtIsShell_w, gxm_XtIsShell)
  XEN_NARGIFY_1(gxm_XtIsOverrideShell_w, gxm_XtIsOverrideShell)
  XEN_NARGIFY_1(gxm_XtIsWMShell_w, gxm_XtIsWMShell)
  XEN_NARGIFY_1(gxm_XtIsVendorShell_w, gxm_XtIsVendorShell)
  XEN_NARGIFY_1(gxm_XtIsTransientShell_w, gxm_XtIsTransientShell)
  XEN_NARGIFY_1(gxm_XtIsTopLevelShell_w, gxm_XtIsTopLevelShell)
  XEN_NARGIFY_1(gxm_XtIsApplicationShell_w, gxm_XtIsApplicationShell)
  XEN_NARGIFY_1(gxm_XtIsSessionShell_w, gxm_XtIsSessionShell)
  XEN_NARGIFY_1(gxm_XtMapWidget_w, gxm_XtMapWidget)
  XEN_NARGIFY_1(gxm_XtUnmapWidget_w, gxm_XtUnmapWidget)
  XEN_NARGIFY_1(gxm_XtAppContext_w, gxm_XtAppContext)
#endif
  XEN_NARGIFY_2(gxm_XLoadQueryFont_w, gxm_XLoadQueryFont)
  XEN_NARGIFY_2(gxm_XQueryFont_w, gxm_XQueryFont)
  XEN_NARGIFY_4(gxm_XGetMotionEvents_w, gxm_XGetMotionEvents)
  XEN_NARGIFY_3(gxm_XDeleteModifiermapEntry_w, gxm_XDeleteModifiermapEntry)
  XEN_NARGIFY_1(gxm_XGetModifierMapping_w, gxm_XGetModifierMapping)
  XEN_NARGIFY_3(gxm_XInsertModifiermapEntry_w, gxm_XInsertModifiermapEntry)
  XEN_NARGIFY_1(gxm_XNewModifiermap_w, gxm_XNewModifiermap)
  XEN_VARGIFY(gxm_XCreateImage_w, gxm_XCreateImage)
  XEN_NARGIFY_8(gxm_XGetImage_w, gxm_XGetImage)
  XEN_VARGIFY(gxm_XGetSubImage_w, gxm_XGetSubImage)
  XEN_NARGIFY_1(gxm_XOpenDisplay_w, gxm_XOpenDisplay)
  XEN_NARGIFY_1(gxm_XFetchBytes_w, gxm_XFetchBytes)
  XEN_NARGIFY_2(gxm_XFetchBuffer_w, gxm_XFetchBuffer)
  XEN_NARGIFY_2(gxm_XGetAtomName_w, gxm_XGetAtomName)
  XEN_NARGIFY_1(gxm_XDisplayName_w, gxm_XDisplayName)
  XEN_NARGIFY_1(gxm_XKeysymToString_w, gxm_XKeysymToString)
  XEN_NARGIFY_2(gxm_XSynchronize_w, gxm_XSynchronize)
  XEN_NARGIFY_2(gxm_XSetAfterFunction_w, gxm_XSetAfterFunction)
  XEN_NARGIFY_3(gxm_XInternAtom_w, gxm_XInternAtom)
  XEN_NARGIFY_2(gxm_XCopyColormapAndFree_w, gxm_XCopyColormapAndFree)
  XEN_NARGIFY_4(gxm_XCreateColormap_w, gxm_XCreateColormap)
  XEN_NARGIFY_7(gxm_XCreatePixmapCursor_w, gxm_XCreatePixmapCursor)
  XEN_NARGIFY_7(gxm_XCreateGlyphCursor_w, gxm_XCreateGlyphCursor)
  XEN_NARGIFY_2(gxm_XCreateFontCursor_w, gxm_XCreateFontCursor)
  XEN_NARGIFY_2(gxm_XLoadFont_w, gxm_XLoadFont)
  XEN_NARGIFY_4(gxm_XCreateGC_w, gxm_XCreateGC)
  XEN_NARGIFY_2(gxm_XFlushGC_w, gxm_XFlushGC)
  XEN_NARGIFY_5(gxm_XCreatePixmap_w, gxm_XCreatePixmap)
  XEN_NARGIFY_5(gxm_XCreateBitmapFromData_w, gxm_XCreateBitmapFromData)
  XEN_NARGIFY_8(gxm_XCreatePixmapFromBitmapData_w, gxm_XCreatePixmapFromBitmapData)
  XEN_NARGIFY_9(gxm_XCreateSimpleWindow_w, gxm_XCreateSimpleWindow)
  XEN_NARGIFY_2(gxm_XGetSelectionOwner_w, gxm_XGetSelectionOwner)
  XEN_VARGIFY(gxm_XCreateWindow_w, gxm_XCreateWindow)
  XEN_NARGIFY_2(gxm_XListInstalledColormaps_w, gxm_XListInstalledColormaps)
  XEN_NARGIFY_3(gxm_XListFonts_w, gxm_XListFonts)
  XEN_NARGIFY_3(gxm_XListFontsWithInfo_w, gxm_XListFontsWithInfo)
  XEN_NARGIFY_1(gxm_XGetFontPath_w, gxm_XGetFontPath)
  XEN_NARGIFY_1(gxm_XListExtensions_w, gxm_XListExtensions)
  XEN_NARGIFY_2(gxm_XListProperties_w, gxm_XListProperties)
  XEN_NARGIFY_3(gxm_XKeycodeToKeysym_w, gxm_XKeycodeToKeysym)
  XEN_NARGIFY_2(gxm_XLookupKeysym_w, gxm_XLookupKeysym)
  XEN_NARGIFY_3(gxm_XGetKeyboardMapping_w, gxm_XGetKeyboardMapping)
  XEN_NARGIFY_1(gxm_XStringToKeysym_w, gxm_XStringToKeysym)
  XEN_NARGIFY_1(gxm_XMaxRequestSize_w, gxm_XMaxRequestSize)
  XEN_NARGIFY_1(gxm_XExtendedMaxRequestSize_w, gxm_XExtendedMaxRequestSize)
  XEN_NARGIFY_1(gxm_XDisplayMotionBufferSize_w, gxm_XDisplayMotionBufferSize)
  XEN_NARGIFY_1(gxm_XVisualIDFromVisual_w, gxm_XVisualIDFromVisual)
  XEN_NARGIFY_0(gxm_XInitThreads_w, gxm_XInitThreads)
  XEN_NARGIFY_1(gxm_XLockDisplay_w, gxm_XLockDisplay)
  XEN_NARGIFY_1(gxm_XUnlockDisplay_w, gxm_XUnlockDisplay)
  XEN_NARGIFY_2(gxm_XRootWindow_w, gxm_XRootWindow)
  XEN_NARGIFY_1(gxm_XDefaultRootWindow_w, gxm_XDefaultRootWindow)
  XEN_NARGIFY_1(gxm_XRootWindowOfScreen_w, gxm_XRootWindowOfScreen)
  XEN_NARGIFY_2(gxm_XDefaultVisual_w, gxm_XDefaultVisual)
  XEN_NARGIFY_1(gxm_XDefaultVisualOfScreen_w, gxm_XDefaultVisualOfScreen)
  XEN_NARGIFY_2(gxm_XDefaultGC_w, gxm_XDefaultGC)
  XEN_NARGIFY_1(gxm_XDefaultGCOfScreen_w, gxm_XDefaultGCOfScreen)
  XEN_NARGIFY_2(gxm_XBlackPixel_w, gxm_XBlackPixel)
  XEN_NARGIFY_2(gxm_XWhitePixel_w, gxm_XWhitePixel)
  XEN_NARGIFY_0(gxm_XAllPlanes_w, gxm_XAllPlanes)
  XEN_NARGIFY_1(gxm_XBlackPixelOfScreen_w, gxm_XBlackPixelOfScreen)
  XEN_NARGIFY_1(gxm_XWhitePixelOfScreen_w, gxm_XWhitePixelOfScreen)
  XEN_NARGIFY_1(gxm_XNextRequest_w, gxm_XNextRequest)
  XEN_NARGIFY_1(gxm_XLastKnownRequestProcessed_w, gxm_XLastKnownRequestProcessed)
  XEN_NARGIFY_1(gxm_XServerVendor_w, gxm_XServerVendor)
  XEN_NARGIFY_1(gxm_XDisplayString_w, gxm_XDisplayString)
  XEN_NARGIFY_2(gxm_XDefaultColormap_w, gxm_XDefaultColormap)
  XEN_NARGIFY_1(gxm_XDefaultColormapOfScreen_w, gxm_XDefaultColormapOfScreen)
  XEN_NARGIFY_1(gxm_XDisplayOfScreen_w, gxm_XDisplayOfScreen)
  XEN_NARGIFY_2(gxm_XScreenOfDisplay_w, gxm_XScreenOfDisplay)
  XEN_NARGIFY_1(gxm_XDefaultScreenOfDisplay_w, gxm_XDefaultScreenOfDisplay)
  XEN_NARGIFY_1(gxm_XEventMaskOfScreen_w, gxm_XEventMaskOfScreen)
  XEN_NARGIFY_1(gxm_XScreenNumberOfScreen_w, gxm_XScreenNumberOfScreen)
  XEN_NARGIFY_1(gxm_XSetErrorHandler_w, gxm_XSetErrorHandler)
  XEN_NARGIFY_1(gxm_XSetIOErrorHandler_w, gxm_XSetIOErrorHandler)
  XEN_NARGIFY_1(gxm_XListPixmapFormats_w, gxm_XListPixmapFormats)
  XEN_NARGIFY_2(gxm_XListDepths_w, gxm_XListDepths)
  XEN_NARGIFY_5(gxm_XReconfigureWMWindow_w, gxm_XReconfigureWMWindow)
  XEN_NARGIFY_2(gxm_XGetWMProtocols_w, gxm_XGetWMProtocols)
  XEN_NARGIFY_4(gxm_XSetWMProtocols_w, gxm_XSetWMProtocols)
  XEN_NARGIFY_3(gxm_XIconifyWindow_w, gxm_XIconifyWindow)
  XEN_NARGIFY_3(gxm_XWithdrawWindow_w, gxm_XWithdrawWindow)
  XEN_NARGIFY_2(gxm_XGetCommand_w, gxm_XGetCommand)
  XEN_NARGIFY_2(gxm_XGetWMColormapWindows_w, gxm_XGetWMColormapWindows)
  XEN_NARGIFY_4(gxm_XSetWMColormapWindows_w, gxm_XSetWMColormapWindows)
  XEN_NARGIFY_1(gxm_XFreeStringList_w, gxm_XFreeStringList)
  XEN_NARGIFY_3(gxm_XSetTransientForHint_w, gxm_XSetTransientForHint)
  XEN_NARGIFY_1(gxm_XActivateScreenSaver_w, gxm_XActivateScreenSaver)
  XEN_NARGIFY_3(gxm_XAllocColor_w, gxm_XAllocColor)
  XEN_NARGIFY_5(gxm_XAllocColorCells_w, gxm_XAllocColorCells)
  XEN_VARGIFY(gxm_XAllocColorPlanes_w, gxm_XAllocColorPlanes)
  XEN_NARGIFY_5(gxm_XAllocNamedColor_w, gxm_XAllocNamedColor)
  XEN_NARGIFY_3(gxm_XAllowEvents_w, gxm_XAllowEvents)
  XEN_NARGIFY_1(gxm_XAutoRepeatOff_w, gxm_XAutoRepeatOff)
  XEN_NARGIFY_1(gxm_XAutoRepeatOn_w, gxm_XAutoRepeatOn)
  XEN_NARGIFY_2(gxm_XBell_w, gxm_XBell)
  XEN_NARGIFY_1(gxm_XBitmapBitOrder_w, gxm_XBitmapBitOrder)
  XEN_NARGIFY_1(gxm_XBitmapPad_w, gxm_XBitmapPad)
  XEN_NARGIFY_1(gxm_XBitmapUnit_w, gxm_XBitmapUnit)
  XEN_NARGIFY_1(gxm_XCellsOfScreen_w, gxm_XCellsOfScreen)
  XEN_NARGIFY_4(gxm_XChangeActivePointerGrab_w, gxm_XChangeActivePointerGrab)
  XEN_NARGIFY_4(gxm_XChangeGC_w, gxm_XChangeGC)
  XEN_NARGIFY_3(gxm_XChangeKeyboardControl_w, gxm_XChangeKeyboardControl)
  XEN_NARGIFY_5(gxm_XChangeKeyboardMapping_w, gxm_XChangeKeyboardMapping)
  XEN_NARGIFY_6(gxm_XChangePointerControl_w, gxm_XChangePointerControl)
  XEN_NARGIFY_8(gxm_XChangeProperty_w, gxm_XChangeProperty)
  XEN_NARGIFY_4(gxm_XChangeWindowAttributes_w, gxm_XChangeWindowAttributes)
  XEN_NARGIFY_3(gxm_XCheckIfEvent_w, gxm_XCheckIfEvent)
  XEN_NARGIFY_3(gxm_XCheckMaskEvent_w, gxm_XCheckMaskEvent)
  XEN_NARGIFY_2(gxm_XCheckTypedEvent_w, gxm_XCheckTypedEvent)
  XEN_NARGIFY_3(gxm_XCheckTypedWindowEvent_w, gxm_XCheckTypedWindowEvent)
  XEN_NARGIFY_3(gxm_XCheckWindowEvent_w, gxm_XCheckWindowEvent)
  XEN_NARGIFY_3(gxm_XCirculateSubwindows_w, gxm_XCirculateSubwindows)
  XEN_NARGIFY_2(gxm_XCirculateSubwindowsDown_w, gxm_XCirculateSubwindowsDown)
  XEN_NARGIFY_2(gxm_XCirculateSubwindowsUp_w, gxm_XCirculateSubwindowsUp)
  XEN_NARGIFY_7(gxm_XClearArea_w, gxm_XClearArea)
  XEN_NARGIFY_2(gxm_XClearWindow_w, gxm_XClearWindow)
  XEN_NARGIFY_1(gxm_XCloseDisplay_w, gxm_XCloseDisplay)
  XEN_NARGIFY_4(gxm_XConfigureWindow_w, gxm_XConfigureWindow)
  XEN_NARGIFY_1(gxm_XConnectionNumber_w, gxm_XConnectionNumber)
  XEN_NARGIFY_6(gxm_XConvertSelection_w, gxm_XConvertSelection)
  XEN_VARGIFY(gxm_XCopyArea_w, gxm_XCopyArea)
  XEN_NARGIFY_4(gxm_XCopyGC_w, gxm_XCopyGC)
  XEN_VARGIFY(gxm_XCopyPlane_w, gxm_XCopyPlane)
  XEN_NARGIFY_2(gxm_XDefaultDepth_w, gxm_XDefaultDepth)
  XEN_NARGIFY_1(gxm_XDefaultDepthOfScreen_w, gxm_XDefaultDepthOfScreen)
  XEN_NARGIFY_1(gxm_XDefaultScreen_w, gxm_XDefaultScreen)
  XEN_NARGIFY_3(gxm_XDefineCursor_w, gxm_XDefineCursor)
  XEN_NARGIFY_3(gxm_XDeleteProperty_w, gxm_XDeleteProperty)
  XEN_NARGIFY_2(gxm_XDestroyWindow_w, gxm_XDestroyWindow)
  XEN_NARGIFY_2(gxm_XDestroySubwindows_w, gxm_XDestroySubwindows)
  XEN_NARGIFY_1(gxm_XDoesBackingStore_w, gxm_XDoesBackingStore)
  XEN_NARGIFY_1(gxm_XDoesSaveUnders_w, gxm_XDoesSaveUnders)
  XEN_NARGIFY_1(gxm_XDisableAccessControl_w, gxm_XDisableAccessControl)
  XEN_NARGIFY_2(gxm_XDisplayCells_w, gxm_XDisplayCells)
  XEN_NARGIFY_2(gxm_XDisplayHeight_w, gxm_XDisplayHeight)
  XEN_NARGIFY_2(gxm_XDisplayHeightMM_w, gxm_XDisplayHeightMM)
  XEN_NARGIFY_1(gxm_XDisplayKeycodes_w, gxm_XDisplayKeycodes)
  XEN_NARGIFY_2(gxm_XDisplayPlanes_w, gxm_XDisplayPlanes)
  XEN_NARGIFY_2(gxm_XDisplayWidth_w, gxm_XDisplayWidth)
  XEN_NARGIFY_2(gxm_XDisplayWidthMM_w, gxm_XDisplayWidthMM)
  XEN_NARGIFY_9(gxm_XDrawArc_w, gxm_XDrawArc)
  XEN_NARGIFY_5(gxm_XDrawArcs_w, gxm_XDrawArcs)
  XEN_NARGIFY_7(gxm_XDrawImageString_w, gxm_XDrawImageString)
  XEN_NARGIFY_7(gxm_XDrawLine_w, gxm_XDrawLine)
  XEN_NARGIFY_6(gxm_XDrawLines_w, gxm_XDrawLines)
  XEN_NARGIFY_6(gxm_XDrawLinesDirect_w, gxm_XDrawLinesDirect)
  XEN_NARGIFY_1(gxm_FreeXPoints_w, gxm_FreeXPoints)
  XEN_NARGIFY_4(gxm_MoveXPoints_w, gxm_MoveXPoints)
  XEN_NARGIFY_1(gxm_Vector2XPoints_w, gxm_Vector2XPoints)
  XEN_NARGIFY_5(gxm_XDrawPoint_w, gxm_XDrawPoint)
  XEN_NARGIFY_6(gxm_XDrawPoints_w, gxm_XDrawPoints)
  XEN_NARGIFY_7(gxm_XDrawRectangle_w, gxm_XDrawRectangle)
  XEN_NARGIFY_5(gxm_XDrawRectangles_w, gxm_XDrawRectangles)
  XEN_NARGIFY_5(gxm_XDrawSegments_w, gxm_XDrawSegments)
  XEN_NARGIFY_7(gxm_XDrawString_w, gxm_XDrawString)
  XEN_NARGIFY_7(gxm_XDrawText_w, gxm_XDrawText)
  XEN_NARGIFY_1(gxm_XEnableAccessControl_w, gxm_XEnableAccessControl)
  XEN_NARGIFY_2(gxm_XEventsQueued_w, gxm_XEventsQueued)
  XEN_NARGIFY_2(gxm_XFetchName_w, gxm_XFetchName)
  XEN_NARGIFY_9(gxm_XFillArc_w, gxm_XFillArc)
  XEN_NARGIFY_5(gxm_XFillArcs_w, gxm_XFillArcs)
  XEN_NARGIFY_7(gxm_XFillPolygon_w, gxm_XFillPolygon)
  XEN_NARGIFY_7(gxm_XFillRectangle_w, gxm_XFillRectangle)
  XEN_NARGIFY_5(gxm_XFillRectangles_w, gxm_XFillRectangles)
  XEN_NARGIFY_1(gxm_XFlush_w, gxm_XFlush)
  XEN_NARGIFY_2(gxm_XForceScreenSaver_w, gxm_XForceScreenSaver)
  XEN_NARGIFY_1(gxm_XFree_w, gxm_XFree)
  XEN_NARGIFY_2(gxm_XFreeColormap_w, gxm_XFreeColormap)
  XEN_NARGIFY_5(gxm_XFreeColors_w, gxm_XFreeColors)
  XEN_NARGIFY_2(gxm_XFreeCursor_w, gxm_XFreeCursor)
  XEN_NARGIFY_1(gxm_XFreeExtensionList_w, gxm_XFreeExtensionList)
  XEN_NARGIFY_2(gxm_XFreeFont_w, gxm_XFreeFont)
  XEN_NARGIFY_3(gxm_XFreeFontInfo_w, gxm_XFreeFontInfo)
  XEN_NARGIFY_1(gxm_XFreeFontNames_w, gxm_XFreeFontNames)
  XEN_NARGIFY_1(gxm_XFreeFontPath_w, gxm_XFreeFontPath)
  XEN_NARGIFY_2(gxm_XFreeGC_w, gxm_XFreeGC)
  XEN_NARGIFY_1(gxm_XFreeModifiermap_w, gxm_XFreeModifiermap)
  XEN_NARGIFY_2(gxm_XFreePixmap_w, gxm_XFreePixmap)
  XEN_VARGIFY(gxm_XGeometry_w, gxm_XGeometry)
  XEN_NARGIFY_4(gxm_XGetErrorText_w, gxm_XGetErrorText)
  XEN_NARGIFY_2(gxm_XGetFontProperty_w, gxm_XGetFontProperty)
  XEN_NARGIFY_3(gxm_XGetGCValues_w, gxm_XGetGCValues)
  XEN_NARGIFY_0(gxm_XGCValues_w, gxm_XGCValues)
  XEN_ARGIFY_1(gxm_XEvent_w, gxm_XEvent)
  XEN_NARGIFY_2(gxm_XGetGeometry_w, gxm_XGetGeometry)
  XEN_NARGIFY_2(gxm_XGetIconName_w, gxm_XGetIconName)
  XEN_NARGIFY_1(gxm_XGetInputFocus_w, gxm_XGetInputFocus)
  XEN_NARGIFY_1(gxm_XGetKeyboardControl_w, gxm_XGetKeyboardControl)
  XEN_NARGIFY_1(gxm_XGetPointerControl_w, gxm_XGetPointerControl)
  XEN_NARGIFY_3(gxm_XGetPointerMapping_w, gxm_XGetPointerMapping)
  XEN_NARGIFY_1(gxm_XGetScreenSaver_w, gxm_XGetScreenSaver)
  XEN_NARGIFY_2(gxm_XGetTransientForHint_w, gxm_XGetTransientForHint)
  XEN_VARGIFY(gxm_XGetWindowProperty_w, gxm_XGetWindowProperty)
  XEN_NARGIFY_2(gxm_XGetWindowAttributes_w, gxm_XGetWindowAttributes)
  XEN_ARGIFY_1(gxm_XGrabButton_w, gxm_XGrabButton)
  XEN_NARGIFY_7(gxm_XGrabKey_w, gxm_XGrabKey)
  XEN_NARGIFY_6(gxm_XGrabKeyboard_w, gxm_XGrabKeyboard)
  XEN_NARGIFY_9(gxm_XGrabPointer_w, gxm_XGrabPointer)
  XEN_NARGIFY_1(gxm_XGrabServer_w, gxm_XGrabServer)
  XEN_NARGIFY_1(gxm_XHeightMMOfScreen_w, gxm_XHeightMMOfScreen)
  XEN_NARGIFY_1(gxm_XHeightOfScreen_w, gxm_XHeightOfScreen)
  XEN_NARGIFY_3(gxm_XIfEvent_w, gxm_XIfEvent)
  XEN_NARGIFY_1(gxm_XImageByteOrder_w, gxm_XImageByteOrder)
  XEN_NARGIFY_2(gxm_XInstallColormap_w, gxm_XInstallColormap)
  XEN_NARGIFY_2(gxm_XKeysymToKeycode_w, gxm_XKeysymToKeycode)
  XEN_NARGIFY_2(gxm_XKillClient_w, gxm_XKillClient)
  XEN_NARGIFY_5(gxm_XLookupColor_w, gxm_XLookupColor)
  XEN_NARGIFY_2(gxm_XLowerWindow_w, gxm_XLowerWindow)
  XEN_NARGIFY_2(gxm_XMapRaised_w, gxm_XMapRaised)
  XEN_NARGIFY_2(gxm_XMapSubwindows_w, gxm_XMapSubwindows)
  XEN_NARGIFY_2(gxm_XMapWindow_w, gxm_XMapWindow)
  XEN_NARGIFY_2(gxm_XMaskEvent_w, gxm_XMaskEvent)
  XEN_NARGIFY_1(gxm_XMaxCmapsOfScreen_w, gxm_XMaxCmapsOfScreen)
  XEN_NARGIFY_1(gxm_XMinCmapsOfScreen_w, gxm_XMinCmapsOfScreen)
  XEN_NARGIFY_6(gxm_XMoveResizeWindow_w, gxm_XMoveResizeWindow)
  XEN_NARGIFY_4(gxm_XMoveWindow_w, gxm_XMoveWindow)
  XEN_NARGIFY_1(gxm_XNextEvent_w, gxm_XNextEvent)
  XEN_NARGIFY_1(gxm_XNoOp_w, gxm_XNoOp)
  XEN_NARGIFY_4(gxm_XParseColor_w, gxm_XParseColor)
  XEN_NARGIFY_1(gxm_XParseGeometry_w, gxm_XParseGeometry)
  XEN_NARGIFY_1(gxm_XPeekEvent_w, gxm_XPeekEvent)
  XEN_NARGIFY_3(gxm_XPeekIfEvent_w, gxm_XPeekIfEvent)
  XEN_NARGIFY_1(gxm_XPending_w, gxm_XPending)
  XEN_NARGIFY_1(gxm_XPlanesOfScreen_w, gxm_XPlanesOfScreen)
  XEN_NARGIFY_1(gxm_XProtocolRevision_w, gxm_XProtocolRevision)
  XEN_NARGIFY_1(gxm_XProtocolVersion_w, gxm_XProtocolVersion)
  XEN_NARGIFY_2(gxm_XPutBackEvent_w, gxm_XPutBackEvent)
  XEN_VARGIFY(gxm_XPutImage_w, gxm_XPutImage)
  XEN_NARGIFY_1(gxm_XQLength_w, gxm_XQLength)
  XEN_NARGIFY_4(gxm_XQueryBestCursor_w, gxm_XQueryBestCursor)
  XEN_NARGIFY_5(gxm_XQueryBestSize_w, gxm_XQueryBestSize)
  XEN_NARGIFY_4(gxm_XQueryBestStipple_w, gxm_XQueryBestStipple)
  XEN_NARGIFY_4(gxm_XQueryBestTile_w, gxm_XQueryBestTile)
  XEN_NARGIFY_3(gxm_XQueryColor_w, gxm_XQueryColor)
  XEN_NARGIFY_4(gxm_XQueryColors_w, gxm_XQueryColors)
  XEN_NARGIFY_2(gxm_XQueryExtension_w, gxm_XQueryExtension)
  XEN_NARGIFY_1(gxm_XQueryKeymap_w, gxm_XQueryKeymap)
  XEN_NARGIFY_2(gxm_XQueryPointer_w, gxm_XQueryPointer)
  XEN_NARGIFY_3(gxm_XQueryTextExtents_w, gxm_XQueryTextExtents)
  XEN_NARGIFY_2(gxm_XQueryTree_w, gxm_XQueryTree)
  XEN_NARGIFY_2(gxm_XRaiseWindow_w, gxm_XRaiseWindow)
  XEN_NARGIFY_3(gxm_XReadBitmapFile_w, gxm_XReadBitmapFile)
  XEN_NARGIFY_1(gxm_XReadBitmapFileData_w, gxm_XReadBitmapFileData)
  XEN_NARGIFY_6(gxm_XRebindKeysym_w, gxm_XRebindKeysym)
  XEN_NARGIFY_4(gxm_XRecolorCursor_w, gxm_XRecolorCursor)
  XEN_NARGIFY_1(gxm_XRefreshKeyboardMapping_w, gxm_XRefreshKeyboardMapping)
  XEN_NARGIFY_5(gxm_XReparentWindow_w, gxm_XReparentWindow)
  XEN_NARGIFY_1(gxm_XResetScreenSaver_w, gxm_XResetScreenSaver)
  XEN_NARGIFY_4(gxm_XResizeWindow_w, gxm_XResizeWindow)
  XEN_NARGIFY_3(gxm_XRestackWindows_w, gxm_XRestackWindows)
  XEN_NARGIFY_2(gxm_XRotateBuffers_w, gxm_XRotateBuffers)
  XEN_NARGIFY_5(gxm_XRotateWindowProperties_w, gxm_XRotateWindowProperties)
  XEN_NARGIFY_1(gxm_XScreenCount_w, gxm_XScreenCount)
  XEN_NARGIFY_3(gxm_XSelectInput_w, gxm_XSelectInput)
  XEN_NARGIFY_5(gxm_XSendEvent_w, gxm_XSendEvent)
  XEN_NARGIFY_2(gxm_XSetAccessControl_w, gxm_XSetAccessControl)
  XEN_NARGIFY_3(gxm_XSetArcMode_w, gxm_XSetArcMode)
  XEN_NARGIFY_3(gxm_XSetBackground_w, gxm_XSetBackground)
  XEN_NARGIFY_3(gxm_XSetClipMask_w, gxm_XSetClipMask)
  XEN_NARGIFY_4(gxm_XSetClipOrigin_w, gxm_XSetClipOrigin)
  XEN_NARGIFY_7(gxm_XSetClipRectangles_w, gxm_XSetClipRectangles)
  XEN_NARGIFY_2(gxm_XSetCloseDownMode_w, gxm_XSetCloseDownMode)
  XEN_NARGIFY_4(gxm_XSetCommand_w, gxm_XSetCommand)
  XEN_NARGIFY_5(gxm_XSetDashes_w, gxm_XSetDashes)
  XEN_NARGIFY_3(gxm_XSetFillRule_w, gxm_XSetFillRule)
  XEN_NARGIFY_3(gxm_XSetFillStyle_w, gxm_XSetFillStyle)
  XEN_NARGIFY_3(gxm_XSetFont_w, gxm_XSetFont)
  XEN_NARGIFY_3(gxm_XSetFontPath_w, gxm_XSetFontPath)
  XEN_NARGIFY_3(gxm_XSetForeground_w, gxm_XSetForeground)
  XEN_NARGIFY_3(gxm_XSetFunction_w, gxm_XSetFunction)
  XEN_NARGIFY_3(gxm_XSetGraphicsExposures_w, gxm_XSetGraphicsExposures)
  XEN_NARGIFY_3(gxm_XSetIconName_w, gxm_XSetIconName)
  XEN_NARGIFY_4(gxm_XSetInputFocus_w, gxm_XSetInputFocus)
  XEN_NARGIFY_6(gxm_XSetLineAttributes_w, gxm_XSetLineAttributes)
  XEN_NARGIFY_2(gxm_XSetModifierMapping_w, gxm_XSetModifierMapping)
  XEN_NARGIFY_3(gxm_XSetPlaneMask_w, gxm_XSetPlaneMask)
  XEN_NARGIFY_3(gxm_XSetPointerMapping_w, gxm_XSetPointerMapping)
  XEN_NARGIFY_5(gxm_XSetScreenSaver_w, gxm_XSetScreenSaver)
  XEN_NARGIFY_4(gxm_XSetSelectionOwner_w, gxm_XSetSelectionOwner)
  XEN_NARGIFY_6(gxm_XSetState_w, gxm_XSetState)
  XEN_NARGIFY_3(gxm_XSetStipple_w, gxm_XSetStipple)
  XEN_NARGIFY_3(gxm_XSetSubwindowMode_w, gxm_XSetSubwindowMode)
  XEN_NARGIFY_4(gxm_XSetTSOrigin_w, gxm_XSetTSOrigin)
  XEN_NARGIFY_3(gxm_XSetTile_w, gxm_XSetTile)
  XEN_NARGIFY_3(gxm_XSetWindowBackground_w, gxm_XSetWindowBackground)
  XEN_NARGIFY_3(gxm_XSetWindowBackgroundPixmap_w, gxm_XSetWindowBackgroundPixmap)
  XEN_NARGIFY_3(gxm_XSetWindowBorder_w, gxm_XSetWindowBorder)
  XEN_NARGIFY_3(gxm_XSetWindowBorderPixmap_w, gxm_XSetWindowBorderPixmap)
  XEN_NARGIFY_3(gxm_XSetWindowBorderWidth_w, gxm_XSetWindowBorderWidth)
  XEN_NARGIFY_3(gxm_XSetWindowColormap_w, gxm_XSetWindowColormap)
  XEN_NARGIFY_4(gxm_XStoreBuffer_w, gxm_XStoreBuffer)
  XEN_NARGIFY_3(gxm_XStoreBytes_w, gxm_XStoreBytes)
  XEN_NARGIFY_3(gxm_XStoreColor_w, gxm_XStoreColor)
  XEN_NARGIFY_4(gxm_XStoreColors_w, gxm_XStoreColors)
  XEN_NARGIFY_3(gxm_XStoreName_w, gxm_XStoreName)
  XEN_NARGIFY_5(gxm_XStoreNamedColor_w, gxm_XStoreNamedColor)
  XEN_NARGIFY_2(gxm_XSync_w, gxm_XSync)
  XEN_NARGIFY_3(gxm_XTextExtents_w, gxm_XTextExtents)
  XEN_NARGIFY_3(gxm_XTextWidth_w, gxm_XTextWidth)
  XEN_NARGIFY_5(gxm_XTranslateCoordinates_w, gxm_XTranslateCoordinates)
  XEN_NARGIFY_2(gxm_XUndefineCursor_w, gxm_XUndefineCursor)
  XEN_NARGIFY_4(gxm_XUngrabButton_w, gxm_XUngrabButton)
  XEN_NARGIFY_4(gxm_XUngrabKey_w, gxm_XUngrabKey)
  XEN_NARGIFY_2(gxm_XUngrabKeyboard_w, gxm_XUngrabKeyboard)
  XEN_NARGIFY_2(gxm_XUngrabPointer_w, gxm_XUngrabPointer)
  XEN_NARGIFY_1(gxm_XUngrabServer_w, gxm_XUngrabServer)
  XEN_NARGIFY_2(gxm_XUninstallColormap_w, gxm_XUninstallColormap)
  XEN_NARGIFY_2(gxm_XUnloadFont_w, gxm_XUnloadFont)
  XEN_NARGIFY_2(gxm_XUnmapSubwindows_w, gxm_XUnmapSubwindows)
  XEN_NARGIFY_2(gxm_XUnmapWindow_w, gxm_XUnmapWindow)
  XEN_NARGIFY_1(gxm_XVendorRelease_w, gxm_XVendorRelease)
  XEN_NARGIFY_9(gxm_XWarpPointer_w, gxm_XWarpPointer)
  XEN_NARGIFY_1(gxm_XWidthMMOfScreen_w, gxm_XWidthMMOfScreen)
  XEN_NARGIFY_1(gxm_XWidthOfScreen_w, gxm_XWidthOfScreen)
  XEN_NARGIFY_3(gxm_XWindowEvent_w, gxm_XWindowEvent)
  XEN_NARGIFY_7(gxm_XWriteBitmapFile_w, gxm_XWriteBitmapFile)
  XEN_NARGIFY_0(gxm_XSupportsLocale_w, gxm_XSupportsLocale)
  XEN_NARGIFY_1(gxm_XSetLocaleModifiers_w, gxm_XSetLocaleModifiers)
  XEN_NARGIFY_2(gxm_XCreateFontSet_w, gxm_XCreateFontSet)
  XEN_NARGIFY_2(gxm_XFreeFontSet_w, gxm_XFreeFontSet)
  XEN_NARGIFY_1(gxm_XFontsOfFontSet_w, gxm_XFontsOfFontSet)
  XEN_NARGIFY_1(gxm_XBaseFontNameListOfFontSet_w, gxm_XBaseFontNameListOfFontSet)
  XEN_NARGIFY_1(gxm_XLocaleOfFontSet_w, gxm_XLocaleOfFontSet)
  XEN_NARGIFY_1(gxm_XContextDependentDrawing_w, gxm_XContextDependentDrawing)
  XEN_NARGIFY_1(gxm_XDirectionalDependentDrawing_w, gxm_XDirectionalDependentDrawing)
  XEN_NARGIFY_1(gxm_XContextualDrawing_w, gxm_XContextualDrawing)
  XEN_NARGIFY_2(gxm_XFilterEvent_w, gxm_XFilterEvent)
  XEN_NARGIFY_0(gxm_XAllocIconSize_w, gxm_XAllocIconSize)
  XEN_NARGIFY_0(gxm_XAllocStandardColormap_w, gxm_XAllocStandardColormap)
  XEN_NARGIFY_0(gxm_XAllocWMHints_w, gxm_XAllocWMHints)
  XEN_NARGIFY_1(gxm_XClipBox_w, gxm_XClipBox)
  XEN_NARGIFY_0(gxm_XCreateRegion_w, gxm_XCreateRegion)
  XEN_NARGIFY_0(gxm_XDefaultString_w, gxm_XDefaultString)
  XEN_NARGIFY_3(gxm_XDeleteContext_w, gxm_XDeleteContext)
  XEN_NARGIFY_1(gxm_XDestroyRegion_w, gxm_XDestroyRegion)
  XEN_NARGIFY_1(gxm_XEmptyRegion_w, gxm_XEmptyRegion)
  XEN_NARGIFY_2(gxm_XEqualRegion_w, gxm_XEqualRegion)
  XEN_NARGIFY_3(gxm_XFindContext_w, gxm_XFindContext)
  XEN_NARGIFY_2(gxm_XGetIconSizes_w, gxm_XGetIconSizes)
  XEN_NARGIFY_3(gxm_XGetRGBColormaps_w, gxm_XGetRGBColormaps)
  XEN_NARGIFY_3(gxm_XGetStandardColormap_w, gxm_XGetStandardColormap)
  XEN_NARGIFY_3(gxm_XGetVisualInfo_w, gxm_XGetVisualInfo)
  XEN_NARGIFY_2(gxm_XGetWMHints_w, gxm_XGetWMHints)
  XEN_NARGIFY_3(gxm_XIntersectRegion_w, gxm_XIntersectRegion)
  XEN_NARGIFY_1(gxm_XConvertCase_w, gxm_XConvertCase)
  XEN_NARGIFY_1(gxm_XLookupString_w, gxm_XLookupString)
  XEN_NARGIFY_4(gxm_XMatchVisualInfo_w, gxm_XMatchVisualInfo)
  XEN_NARGIFY_3(gxm_XOffsetRegion_w, gxm_XOffsetRegion)
  XEN_NARGIFY_3(gxm_XPointInRegion_w, gxm_XPointInRegion)
  XEN_NARGIFY_3(gxm_XPolygonRegion_w, gxm_XPolygonRegion)
  XEN_NARGIFY_5(gxm_XRectInRegion_w, gxm_XRectInRegion)
  XEN_NARGIFY_4(gxm_XSaveContext_w, gxm_XSaveContext)
  XEN_NARGIFY_0(gxm_XUniqueContext_w, gxm_XUniqueContext)
  XEN_NARGIFY_5(gxm_XSetRGBColormaps_w, gxm_XSetRGBColormaps)
  XEN_NARGIFY_3(gxm_XSetWMHints_w, gxm_XSetWMHints)
  XEN_NARGIFY_3(gxm_XSetRegion_w, gxm_XSetRegion)
  XEN_NARGIFY_4(gxm_XSetStandardColormap_w, gxm_XSetStandardColormap)
  XEN_NARGIFY_3(gxm_XShrinkRegion_w, gxm_XShrinkRegion)
  XEN_NARGIFY_3(gxm_XSubtractRegion_w, gxm_XSubtractRegion)
  XEN_NARGIFY_3(gxm_XUnionRectWithRegion_w, gxm_XUnionRectWithRegion)
  XEN_NARGIFY_3(gxm_XUnionRegion_w, gxm_XUnionRegion)
  XEN_NARGIFY_3(gxm_XXorRegion_w, gxm_XXorRegion)

  XEN_NARGIFY_1(gxm_DefaultScreen_w, gxm_DefaultScreen)
  XEN_NARGIFY_1(gxm_DefaultRootWindow_w, gxm_DefaultRootWindow)
  XEN_NARGIFY_1(gxm_QLength_w, gxm_QLength)
  XEN_NARGIFY_1(gxm_ScreenCount_w, gxm_ScreenCount)
  XEN_NARGIFY_1(gxm_ServerVendor_w, gxm_ServerVendor)
  XEN_NARGIFY_1(gxm_ProtocolVersion_w, gxm_ProtocolVersion)
  XEN_NARGIFY_1(gxm_ProtocolRevision_w, gxm_ProtocolRevision)
  XEN_NARGIFY_1(gxm_VendorRelease_w, gxm_VendorRelease)
  XEN_NARGIFY_1(gxm_DisplayString_w, gxm_DisplayString)
  XEN_NARGIFY_1(gxm_BitmapUnit_w, gxm_BitmapUnit)
  XEN_NARGIFY_1(gxm_BitmapBitOrder_w, gxm_BitmapBitOrder)
  XEN_NARGIFY_1(gxm_BitmapPad_w, gxm_BitmapPad)
  XEN_NARGIFY_1(gxm_ImageByteOrder_w, gxm_ImageByteOrder)
  XEN_NARGIFY_1(gxm_NextRequest_w, gxm_NextRequest)
  XEN_NARGIFY_1(gxm_LastKnownRequestProcessed_w, gxm_LastKnownRequestProcessed)
  XEN_NARGIFY_1(gxm_DefaultScreenOfDisplay_w, gxm_DefaultScreenOfDisplay)
  XEN_NARGIFY_1(gxm_DisplayOfScreen_w, gxm_DisplayOfScreen)
  XEN_NARGIFY_1(gxm_RootWindowOfScreen_w, gxm_RootWindowOfScreen)
  XEN_NARGIFY_1(gxm_BlackPixelOfScreen_w, gxm_BlackPixelOfScreen)
  XEN_NARGIFY_1(gxm_WhitePixelOfScreen_w, gxm_WhitePixelOfScreen)
  XEN_NARGIFY_1(gxm_DefaultColormapOfScreen_w, gxm_DefaultColormapOfScreen)
  XEN_NARGIFY_1(gxm_DefaultDepthOfScreen_w, gxm_DefaultDepthOfScreen)
  XEN_NARGIFY_1(gxm_DefaultGCOfScreen_w, gxm_DefaultGCOfScreen)
  XEN_NARGIFY_1(gxm_DefaultVisualOfScreen_w, gxm_DefaultVisualOfScreen)
  XEN_NARGIFY_1(gxm_WidthOfScreen_w, gxm_WidthOfScreen)
  XEN_NARGIFY_1(gxm_HeightOfScreen_w, gxm_HeightOfScreen)
  XEN_NARGIFY_1(gxm_WidthMMOfScreen_w, gxm_WidthMMOfScreen)
  XEN_NARGIFY_1(gxm_HeightMMOfScreen_w, gxm_HeightMMOfScreen)
  XEN_NARGIFY_1(gxm_PlanesOfScreen_w, gxm_PlanesOfScreen)
  XEN_NARGIFY_1(gxm_CellsOfScreen_w, gxm_CellsOfScreen)
  XEN_NARGIFY_1(gxm_MinCmapsOfScreen_w, gxm_MinCmapsOfScreen)
  XEN_NARGIFY_1(gxm_MaxCmapsOfScreen_w, gxm_MaxCmapsOfScreen)
  XEN_NARGIFY_1(gxm_DoesSaveUnders_w, gxm_DoesSaveUnders)
  XEN_NARGIFY_1(gxm_DoesBackingStore_w, gxm_DoesBackingStore)
  XEN_NARGIFY_1(gxm_EventMaskOfScreen_w, gxm_EventMaskOfScreen)
  XEN_NARGIFY_2(gxm_RootWindow_w, gxm_RootWindow)
  XEN_NARGIFY_2(gxm_DefaultVisual_w, gxm_DefaultVisual)
  XEN_NARGIFY_2(gxm_DefaultGC_w, gxm_DefaultGC)
  XEN_NARGIFY_2(gxm_BlackPixel_w, gxm_BlackPixel)
  XEN_NARGIFY_2(gxm_WhitePixel_w, gxm_WhitePixel)
  XEN_NARGIFY_2(gxm_DisplayWidth_w, gxm_DisplayWidth)
  XEN_NARGIFY_2(gxm_DisplayHeight_w, gxm_DisplayHeight)
  XEN_NARGIFY_2(gxm_DisplayWidthMM_w, gxm_DisplayWidthMM)
  XEN_NARGIFY_2(gxm_DisplayHeightMM_w, gxm_DisplayHeightMM)
  XEN_NARGIFY_2(gxm_DisplayPlanes_w, gxm_DisplayPlanes)
  XEN_NARGIFY_2(gxm_DisplayCells_w, gxm_DisplayCells)
  XEN_NARGIFY_2(gxm_DefaultColormap_w, gxm_DefaultColormap)
  XEN_NARGIFY_2(gxm_ScreenOfDisplay_w, gxm_ScreenOfDisplay)
  XEN_NARGIFY_2(gxm_DefaultDepth_w, gxm_DefaultDepth)

  XEN_NARGIFY_1(gxm_IsKeypadKey_w, gxm_IsKeypadKey)
  XEN_NARGIFY_1(gxm_IsPrivateKeypadKey_w, gxm_IsPrivateKeypadKey)
  XEN_NARGIFY_1(gxm_IsCursorKey_w, gxm_IsCursorKey)
  XEN_NARGIFY_1(gxm_IsPFKey_w, gxm_IsPFKey)
  XEN_NARGIFY_1(gxm_IsFunctionKey_w, gxm_IsFunctionKey)
  XEN_NARGIFY_1(gxm_IsMiscFunctionKey_w, gxm_IsMiscFunctionKey)
  XEN_NARGIFY_1(gxm_IsModifierKey_w, gxm_IsModifierKey)

#if HAVE_MOTIF
  XEN_ARGIFY_4(gxm_XmCreateMessageBox_w, gxm_XmCreateMessageBox)
  XEN_ARGIFY_4(gxm_XmCreateMessageDialog_w, gxm_XmCreateMessageDialog)
  XEN_ARGIFY_4(gxm_XmCreateErrorDialog_w, gxm_XmCreateErrorDialog)
  XEN_ARGIFY_4(gxm_XmCreateInformationDialog_w, gxm_XmCreateInformationDialog)
  XEN_ARGIFY_4(gxm_XmCreateQuestionDialog_w, gxm_XmCreateQuestionDialog)
  XEN_ARGIFY_4(gxm_XmCreateWarningDialog_w, gxm_XmCreateWarningDialog)
  XEN_ARGIFY_4(gxm_XmCreateWorkingDialog_w, gxm_XmCreateWorkingDialog)
  XEN_ARGIFY_4(gxm_XmCreateTemplateDialog_w, gxm_XmCreateTemplateDialog)
  XEN_NARGIFY_2(gxm_XmMessageBoxGetChild_w, gxm_XmMessageBoxGetChild)
  XEN_ARGIFY_4(gxm_XmCreateArrowButtonGadget_w, gxm_XmCreateArrowButtonGadget)
  XEN_ARGIFY_4(gxm_XmCreateArrowButton_w, gxm_XmCreateArrowButton)
#if MOTIF_2
  XEN_ARGIFY_4(gxm_XmCreateNotebook_w, gxm_XmCreateNotebook)
  XEN_NARGIFY_2(gxm_XmNotebookGetPageInfo_w, gxm_XmNotebookGetPageInfo)
#if HAVE_XP
  XEN_ARGIFY_5(gxm_XmPrintSetup_w, gxm_XmPrintSetup)
  XEN_NARGIFY_4(gxm_XmPrintToFile_w, gxm_XmPrintToFile)
  XEN_NARGIFY_2(gxm_XmPrintPopupPDM_w, gxm_XmPrintPopupPDM)
  XEN_NARGIFY_1(gxm_XmRedisplayWidget_w, gxm_XmRedisplayWidget)
#endif
  XEN_NARGIFY_5(gxm_XmTransferSetParameters_w, gxm_XmTransferSetParameters)
  XEN_NARGIFY_2(gxm_XmTransferDone_w, gxm_XmTransferDone)
  XEN_NARGIFY_5(gxm_XmTransferValue_w, gxm_XmTransferValue)
  XEN_NARGIFY_1(gxm_XmTransferStartRequest_w, gxm_XmTransferStartRequest)
  XEN_NARGIFY_2(gxm_XmTransferSendRequest_w, gxm_XmTransferSendRequest)
  XEN_ARGIFY_4(gxm_XmCreateComboBox_w, gxm_XmCreateComboBox)
  XEN_ARGIFY_4(gxm_XmCreateDropDownComboBox_w, gxm_XmCreateDropDownComboBox)
  XEN_ARGIFY_4(gxm_XmCreateDropDownList_w, gxm_XmCreateDropDownList)
  XEN_NARGIFY_4(gxm_XmComboBoxAddItem_w, gxm_XmComboBoxAddItem)
  XEN_NARGIFY_2(gxm_XmComboBoxDeletePos_w, gxm_XmComboBoxDeletePos)
  XEN_NARGIFY_2(gxm_XmComboBoxSelectItem_w, gxm_XmComboBoxSelectItem)
  XEN_NARGIFY_2(gxm_XmComboBoxSetItem_w, gxm_XmComboBoxSetItem)
  XEN_NARGIFY_1(gxm_XmComboBoxUpdate_w, gxm_XmComboBoxUpdate)
  XEN_ARGIFY_4(gxm_XmCreateContainer_w, gxm_XmCreateContainer)
  XEN_NARGIFY_2(gxm_XmContainerGetItemChildren_w, gxm_XmContainerGetItemChildren)
  XEN_NARGIFY_1(gxm_XmContainerRelayout_w, gxm_XmContainerRelayout)
  XEN_NARGIFY_3(gxm_XmContainerReorder_w, gxm_XmContainerReorder)
  XEN_NARGIFY_2(gxm_XmContainerCut_w, gxm_XmContainerCut)
  XEN_NARGIFY_2(gxm_XmContainerCopy_w, gxm_XmContainerCopy)
  XEN_NARGIFY_1(gxm_XmContainerPaste_w, gxm_XmContainerPaste)
  XEN_NARGIFY_2(gxm_XmContainerCopyLink_w, gxm_XmContainerCopyLink)
  XEN_NARGIFY_1(gxm_XmContainerPasteLink_w, gxm_XmContainerPasteLink)
  XEN_ARGIFY_4(gxm_XmCreateSpinBox_w, gxm_XmCreateSpinBox)
  XEN_NARGIFY_1(gxm_XmSpinBoxValidatePosition_w, gxm_XmSpinBoxValidatePosition)
  XEN_ARGIFY_4(gxm_XmCreateSimpleSpinBox_w, gxm_XmCreateSimpleSpinBox)
  XEN_NARGIFY_3(gxm_XmSimpleSpinBoxAddItem_w, gxm_XmSimpleSpinBoxAddItem)
  XEN_NARGIFY_2(gxm_XmSimpleSpinBoxDeletePos_w, gxm_XmSimpleSpinBoxDeletePos)
  XEN_NARGIFY_2(gxm_XmSimpleSpinBoxSetItem_w, gxm_XmSimpleSpinBoxSetItem)
  XEN_NARGIFY_1(gxm_XmDropSiteRegistered_w, gxm_XmDropSiteRegistered)
  XEN_NARGIFY_2(gxm_XmTextFieldCopyLink_w, gxm_XmTextFieldCopyLink)
  XEN_NARGIFY_1(gxm_XmTextFieldPasteLink_w, gxm_XmTextFieldPasteLink)
  XEN_NARGIFY_1(gxm_XmTextGetCenterline_w, gxm_XmTextGetCenterline)
  XEN_NARGIFY_3(gxm_XmToggleButtonGadgetSetValue_w, gxm_XmToggleButtonGadgetSetValue)
  XEN_ARGIFY_4(gxm_XmCreateIconGadget_w, gxm_XmCreateIconGadget)
  XEN_ARGIFY_4(gxm_XmCreateIconHeader_w, gxm_XmCreateIconHeader)
  XEN_NARGIFY_3(gxm_XmObjectAtPoint_w, gxm_XmObjectAtPoint)
  XEN_NARGIFY_4(gxm_XmConvertStringToUnits_w, gxm_XmConvertStringToUnits)
  XEN_ARGIFY_4(gxm_XmCreateGrabShell_w, gxm_XmCreateGrabShell)
  XEN_NARGIFY_3(gxm_XmToggleButtonSetValue_w, gxm_XmToggleButtonSetValue)
  XEN_NARGIFY_1(gxm_XmTextPasteLink_w, gxm_XmTextPasteLink)
  XEN_NARGIFY_2(gxm_XmTextCopyLink_w, gxm_XmTextCopyLink)
  XEN_NARGIFY_7(gxm_XmScaleSetTicks_w, gxm_XmScaleSetTicks)
#endif
  XEN_NARGIFY_3(gxm_XmInternAtom_w, gxm_XmInternAtom)
  XEN_NARGIFY_2(gxm_XmGetAtomName_w, gxm_XmGetAtomName)
  XEN_ARGIFY_4(gxm_XmCreatePanedWindow_w, gxm_XmCreatePanedWindow)
  XEN_ARGIFY_4(gxm_XmCreateBulletinBoard_w, gxm_XmCreateBulletinBoard)
  XEN_ARGIFY_4(gxm_XmCreateBulletinBoardDialog_w, gxm_XmCreateBulletinBoardDialog)
  XEN_ARGIFY_4(gxm_XmCreateCascadeButtonGadget_w, gxm_XmCreateCascadeButtonGadget)
  XEN_NARGIFY_2(gxm_XmCascadeButtonGadgetHighlight_w, gxm_XmCascadeButtonGadgetHighlight)
  XEN_NARGIFY_4(gxm_XmAddProtocols_w, gxm_XmAddProtocols)
  XEN_NARGIFY_4(gxm_XmRemoveProtocols_w, gxm_XmRemoveProtocols)
  XEN_NARGIFY_5(gxm_XmAddProtocolCallback_w, gxm_XmAddProtocolCallback)
  XEN_NARGIFY_5(gxm_XmRemoveProtocolCallback_w, gxm_XmRemoveProtocolCallback)
  XEN_NARGIFY_3(gxm_XmActivateProtocol_w, gxm_XmActivateProtocol)
  XEN_NARGIFY_3(gxm_XmDeactivateProtocol_w, gxm_XmDeactivateProtocol)
  XEN_NARGIFY_7(gxm_XmSetProtocolHooks_w, gxm_XmSetProtocolHooks)
  XEN_ARGIFY_4(gxm_XmCreateCascadeButton_w, gxm_XmCreateCascadeButton)
  XEN_NARGIFY_2(gxm_XmCascadeButtonHighlight_w, gxm_XmCascadeButtonHighlight)
  XEN_ARGIFY_4(gxm_XmCreatePushButtonGadget_w, gxm_XmCreatePushButtonGadget)
  XEN_ARGIFY_4(gxm_XmCreatePushButton_w, gxm_XmCreatePushButton)
  XEN_ARGIFY_4(gxm_XmCreateCommand_w, gxm_XmCreateCommand)
  XEN_NARGIFY_2(gxm_XmCommandGetChild_w, gxm_XmCommandGetChild)
  XEN_NARGIFY_2(gxm_XmCommandSetValue_w, gxm_XmCommandSetValue)
  XEN_NARGIFY_2(gxm_XmCommandAppendValue_w, gxm_XmCommandAppendValue)
  XEN_NARGIFY_2(gxm_XmCommandError_w, gxm_XmCommandError)
  XEN_ARGIFY_4(gxm_XmCreateCommandDialog_w, gxm_XmCreateCommandDialog)
  XEN_NARGIFY_2(gxm_XmMenuPosition_w, gxm_XmMenuPosition)
  XEN_ARGIFY_4(gxm_XmCreateRowColumn_w, gxm_XmCreateRowColumn)
  XEN_ARGIFY_4(gxm_XmCreateWorkArea_w, gxm_XmCreateWorkArea)
  XEN_ARGIFY_4(gxm_XmCreateRadioBox_w, gxm_XmCreateRadioBox)
  XEN_ARGIFY_4(gxm_XmCreateOptionMenu_w, gxm_XmCreateOptionMenu)
  XEN_NARGIFY_1(gxm_XmOptionLabelGadget_w, gxm_XmOptionLabelGadget)
  XEN_NARGIFY_1(gxm_XmOptionButtonGadget_w, gxm_XmOptionButtonGadget)
  XEN_ARGIFY_4(gxm_XmCreateMenuBar_w, gxm_XmCreateMenuBar)
  XEN_ARGIFY_4(gxm_XmCreatePopupMenu_w, gxm_XmCreatePopupMenu)
  XEN_ARGIFY_4(gxm_XmCreatePulldownMenu_w, gxm_XmCreatePulldownMenu)
  XEN_NARGIFY_1(gxm_XmGetPostedFromWidget_w, gxm_XmGetPostedFromWidget)
  XEN_NARGIFY_1(gxm_XmGetTearOffControl_w, gxm_XmGetTearOffControl)
  XEN_NARGIFY_2(gxm_XmAddToPostFromList_w, gxm_XmAddToPostFromList)
  XEN_NARGIFY_2(gxm_XmRemoveFromPostFromList_w, gxm_XmRemoveFromPostFromList)
  XEN_NARGIFY_2(gxm_XmScaleSetValue_w, gxm_XmScaleSetValue)
  XEN_NARGIFY_1(gxm_XmScaleGetValue_w, gxm_XmScaleGetValue)
  XEN_ARGIFY_4(gxm_XmCreateScale_w, gxm_XmCreateScale)
  XEN_NARGIFY_6(gxm_XmClipboardStartCopy_w, gxm_XmClipboardStartCopy)
  XEN_NARGIFY_7(gxm_XmClipboardCopy_w, gxm_XmClipboardCopy)
  XEN_NARGIFY_3(gxm_XmClipboardEndCopy_w, gxm_XmClipboardEndCopy)
  XEN_NARGIFY_3(gxm_XmClipboardCancelCopy_w, gxm_XmClipboardCancelCopy)
  XEN_NARGIFY_3(gxm_XmClipboardWithdrawFormat_w, gxm_XmClipboardWithdrawFormat)
  XEN_NARGIFY_6(gxm_XmClipboardCopyByName_w, gxm_XmClipboardCopyByName)
  XEN_NARGIFY_2(gxm_XmClipboardUndoCopy_w, gxm_XmClipboardUndoCopy)
  XEN_NARGIFY_2(gxm_XmClipboardLock_w, gxm_XmClipboardLock)
  XEN_NARGIFY_3(gxm_XmClipboardUnlock_w, gxm_XmClipboardUnlock)
  XEN_NARGIFY_3(gxm_XmClipboardStartRetrieve_w, gxm_XmClipboardStartRetrieve)
  XEN_NARGIFY_2(gxm_XmClipboardEndRetrieve_w, gxm_XmClipboardEndRetrieve)
  XEN_NARGIFY_4(gxm_XmClipboardRetrieve_w, gxm_XmClipboardRetrieve)
  XEN_NARGIFY_2(gxm_XmClipboardInquireCount_w, gxm_XmClipboardInquireCount)
  XEN_NARGIFY_4(gxm_XmClipboardInquireFormat_w, gxm_XmClipboardInquireFormat)
  XEN_NARGIFY_3(gxm_XmClipboardInquireLength_w, gxm_XmClipboardInquireLength)
  XEN_NARGIFY_3(gxm_XmClipboardInquirePendingItems_w, gxm_XmClipboardInquirePendingItems)
  XEN_NARGIFY_3(gxm_XmClipboardRegisterFormat_w, gxm_XmClipboardRegisterFormat)
  XEN_NARGIFY_1(gxm_XmGetXmScreen_w, gxm_XmGetXmScreen)
  XEN_ARGIFY_4(gxm_XmCreateScrollBar_w, gxm_XmCreateScrollBar)
  XEN_NARGIFY_1(gxm_XmScrollBarGetValues_w, gxm_XmScrollBarGetValues)
  XEN_NARGIFY_6(gxm_XmScrollBarSetValues_w, gxm_XmScrollBarSetValues)
  XEN_ARGIFY_4(gxm_XmCreateDialogShell_w, gxm_XmCreateDialogShell)
  XEN_ARGIFY_4(gxm_XmCreateScrolledWindow_w, gxm_XmCreateScrolledWindow)
  XEN_NARGIFY_4(gxm_XmScrollVisible_w, gxm_XmScrollVisible)
  XEN_NARGIFY_2(gxm_XmGetDragContext_w, gxm_XmGetDragContext)
  XEN_NARGIFY_1(gxm_XmGetXmDisplay_w, gxm_XmGetXmDisplay)
  XEN_NARGIFY_2(gxm_XmSelectionBoxGetChild_w, gxm_XmSelectionBoxGetChild)
  XEN_ARGIFY_4(gxm_XmCreateSelectionBox_w, gxm_XmCreateSelectionBox)
  XEN_ARGIFY_4(gxm_XmCreateSelectionDialog_w, gxm_XmCreateSelectionDialog)
  XEN_ARGIFY_4(gxm_XmCreatePromptDialog_w, gxm_XmCreatePromptDialog)
  XEN_ARGIFY_4(gxm_XmDragStart_w, gxm_XmDragStart)
  XEN_NARGIFY_1(gxm_XmDragCancel_w, gxm_XmDragCancel)
  XEN_NARGIFY_5(gxm_XmTargetsAreCompatible_w, gxm_XmTargetsAreCompatible)
  XEN_ARGIFY_4(gxm_XmCreateSeparatorGadget_w, gxm_XmCreateSeparatorGadget)
  XEN_ARGIFY_4(gxm_XmCreateDragIcon_w, gxm_XmCreateDragIcon)
  XEN_ARGIFY_4(gxm_XmCreateSeparator_w, gxm_XmCreateSeparator)
  XEN_ARGIFY_4(gxm_XmCreateDrawingArea_w, gxm_XmCreateDrawingArea)
  XEN_ARGIFY_4(gxm_XmCreateDrawnButton_w, gxm_XmCreateDrawnButton)
  XEN_ARGIFY_3(gxm_XmDropSiteRegister_w, gxm_XmDropSiteRegister)
  XEN_NARGIFY_1(gxm_XmDropSiteUnregister_w, gxm_XmDropSiteUnregister)
  XEN_NARGIFY_1(gxm_XmDropSiteStartUpdate_w, gxm_XmDropSiteStartUpdate)
  XEN_ARGIFY_3(gxm_XmDropSiteUpdate_w, gxm_XmDropSiteUpdate)
  XEN_NARGIFY_1(gxm_XmDropSiteEndUpdate_w, gxm_XmDropSiteEndUpdate)
  XEN_ARGIFY_3(gxm_XmDropSiteRetrieve_w, gxm_XmDropSiteRetrieve)
  XEN_NARGIFY_1(gxm_XmDropSiteQueryStackingOrder_w, gxm_XmDropSiteQueryStackingOrder)
  XEN_NARGIFY_3(gxm_XmDropSiteConfigureStackingOrder_w, gxm_XmDropSiteConfigureStackingOrder)
  XEN_ARGIFY_3(gxm_XmDropTransferStart_w, gxm_XmDropTransferStart)
  XEN_NARGIFY_2(gxm_XmDropTransferAdd_w, gxm_XmDropTransferAdd)
  XEN_NARGIFY_1(gxm_XmTextFieldGetString_w, gxm_XmTextFieldGetString)
  XEN_NARGIFY_3(gxm_XmTextFieldGetSubstring_w, gxm_XmTextFieldGetSubstring)
  XEN_NARGIFY_1(gxm_XmTextFieldGetLastPosition_w, gxm_XmTextFieldGetLastPosition)
  XEN_NARGIFY_2(gxm_XmTextFieldSetString_w, gxm_XmTextFieldSetString)
  XEN_NARGIFY_4(gxm_XmTextFieldReplace_w, gxm_XmTextFieldReplace)
  XEN_NARGIFY_3(gxm_XmTextFieldInsert_w, gxm_XmTextFieldInsert)
  XEN_NARGIFY_2(gxm_XmTextFieldSetAddMode_w, gxm_XmTextFieldSetAddMode)
  XEN_NARGIFY_1(gxm_XmTextFieldGetAddMode_w, gxm_XmTextFieldGetAddMode)
  XEN_NARGIFY_1(gxm_XmTextFieldGetEditable_w, gxm_XmTextFieldGetEditable)
  XEN_NARGIFY_2(gxm_XmTextFieldSetEditable_w, gxm_XmTextFieldSetEditable)
  XEN_NARGIFY_1(gxm_XmTextFieldGetMaxLength_w, gxm_XmTextFieldGetMaxLength)
  XEN_NARGIFY_2(gxm_XmTextFieldSetMaxLength_w, gxm_XmTextFieldSetMaxLength)
  XEN_NARGIFY_1(gxm_XmTextFieldGetCursorPosition_w, gxm_XmTextFieldGetCursorPosition)
  XEN_NARGIFY_1(gxm_XmTextFieldGetInsertionPosition_w, gxm_XmTextFieldGetInsertionPosition)
  XEN_NARGIFY_2(gxm_XmTextFieldSetCursorPosition_w, gxm_XmTextFieldSetCursorPosition)
  XEN_NARGIFY_2(gxm_XmTextFieldSetInsertionPosition_w, gxm_XmTextFieldSetInsertionPosition)
  XEN_NARGIFY_1(gxm_XmTextFieldGetSelectionPosition_w, gxm_XmTextFieldGetSelectionPosition)
  XEN_NARGIFY_1(gxm_XmTextFieldGetSelection_w, gxm_XmTextFieldGetSelection)
  XEN_NARGIFY_1(gxm_XmTextFieldRemove_w, gxm_XmTextFieldRemove)
  XEN_NARGIFY_2(gxm_XmTextFieldCopy_w, gxm_XmTextFieldCopy)
  XEN_NARGIFY_2(gxm_XmTextFieldCut_w, gxm_XmTextFieldCut)
  XEN_NARGIFY_1(gxm_XmTextFieldPaste_w, gxm_XmTextFieldPaste)
  XEN_NARGIFY_2(gxm_XmTextFieldClearSelection_w, gxm_XmTextFieldClearSelection)
  XEN_NARGIFY_4(gxm_XmTextFieldSetSelection_w, gxm_XmTextFieldSetSelection)
  XEN_NARGIFY_3(gxm_XmTextFieldXYToPos_w, gxm_XmTextFieldXYToPos)
  XEN_NARGIFY_2(gxm_XmTextFieldPosToXY_w, gxm_XmTextFieldPosToXY)
  XEN_NARGIFY_2(gxm_XmTextFieldShowPosition_w, gxm_XmTextFieldShowPosition)
  XEN_NARGIFY_4(gxm_XmTextFieldSetHighlight_w, gxm_XmTextFieldSetHighlight)
  XEN_NARGIFY_1(gxm_XmTextFieldGetBaseline_w, gxm_XmTextFieldGetBaseline)
  XEN_ARGIFY_4(gxm_XmCreateTextField_w, gxm_XmCreateTextField)
  XEN_NARGIFY_2(gxm_XmFileSelectionBoxGetChild_w, gxm_XmFileSelectionBoxGetChild)
  XEN_NARGIFY_2(gxm_XmFileSelectionDoSearch_w, gxm_XmFileSelectionDoSearch)
  XEN_ARGIFY_4(gxm_XmCreateFileSelectionBox_w, gxm_XmCreateFileSelectionBox)
  XEN_ARGIFY_4(gxm_XmCreateFileSelectionDialog_w, gxm_XmCreateFileSelectionDialog)
  XEN_NARGIFY_4(gxm_XmTextSetHighlight_w, gxm_XmTextSetHighlight)
  XEN_ARGIFY_4(gxm_XmCreateScrolledText_w, gxm_XmCreateScrolledText)
  XEN_ARGIFY_4(gxm_XmCreateText_w, gxm_XmCreateText)
  XEN_NARGIFY_3(gxm_XmTextGetSubstring_w, gxm_XmTextGetSubstring)
  XEN_NARGIFY_1(gxm_XmTextGetString_w, gxm_XmTextGetString)
  XEN_NARGIFY_1(gxm_XmTextGetLastPosition_w, gxm_XmTextGetLastPosition)
  XEN_NARGIFY_2(gxm_XmTextSetString_w, gxm_XmTextSetString)
  XEN_NARGIFY_4(gxm_XmTextReplace_w, gxm_XmTextReplace)
  XEN_NARGIFY_3(gxm_XmTextInsert_w, gxm_XmTextInsert)
  XEN_NARGIFY_2(gxm_XmTextSetAddMode_w, gxm_XmTextSetAddMode)
  XEN_NARGIFY_1(gxm_XmTextGetAddMode_w, gxm_XmTextGetAddMode)
  XEN_NARGIFY_1(gxm_XmTextGetEditable_w, gxm_XmTextGetEditable)
  XEN_NARGIFY_2(gxm_XmTextSetEditable_w, gxm_XmTextSetEditable)
  XEN_NARGIFY_1(gxm_XmTextGetMaxLength_w, gxm_XmTextGetMaxLength)
  XEN_NARGIFY_2(gxm_XmTextSetMaxLength_w, gxm_XmTextSetMaxLength)
  XEN_NARGIFY_1(gxm_XmTextGetTopCharacter_w, gxm_XmTextGetTopCharacter)
  XEN_NARGIFY_2(gxm_XmTextSetTopCharacter_w, gxm_XmTextSetTopCharacter)
  XEN_NARGIFY_1(gxm_XmTextGetCursorPosition_w, gxm_XmTextGetCursorPosition)
  XEN_NARGIFY_1(gxm_XmTextGetInsertionPosition_w, gxm_XmTextGetInsertionPosition)
  XEN_NARGIFY_2(gxm_XmTextSetInsertionPosition_w, gxm_XmTextSetInsertionPosition)
  XEN_NARGIFY_2(gxm_XmTextSetCursorPosition_w, gxm_XmTextSetCursorPosition)
  XEN_NARGIFY_1(gxm_XmTextRemove_w, gxm_XmTextRemove)
  XEN_NARGIFY_2(gxm_XmTextCopy_w, gxm_XmTextCopy)
  XEN_NARGIFY_2(gxm_XmTextCut_w, gxm_XmTextCut)
  XEN_NARGIFY_1(gxm_XmTextPaste_w, gxm_XmTextPaste)
  XEN_NARGIFY_1(gxm_XmTextGetSelection_w, gxm_XmTextGetSelection)
  XEN_NARGIFY_4(gxm_XmTextSetSelection_w, gxm_XmTextSetSelection)
  XEN_NARGIFY_2(gxm_XmTextClearSelection_w, gxm_XmTextClearSelection)
  XEN_NARGIFY_1(gxm_XmTextGetSelectionPosition_w, gxm_XmTextGetSelectionPosition)
  XEN_NARGIFY_3(gxm_XmTextXYToPos_w, gxm_XmTextXYToPos)
  XEN_NARGIFY_2(gxm_XmTextPosToXY_w, gxm_XmTextPosToXY)
  XEN_NARGIFY_1(gxm_XmTextGetSource_w, gxm_XmTextGetSource)
  XEN_NARGIFY_4(gxm_XmTextSetSource_w, gxm_XmTextSetSource)
  XEN_NARGIFY_2(gxm_XmTextShowPosition_w, gxm_XmTextShowPosition)
  XEN_NARGIFY_2(gxm_XmTextScroll_w, gxm_XmTextScroll)
  XEN_NARGIFY_1(gxm_XmTextGetBaseline_w, gxm_XmTextGetBaseline)
  XEN_NARGIFY_1(gxm_XmTextDisableRedisplay_w, gxm_XmTextDisableRedisplay)
  XEN_NARGIFY_1(gxm_XmTextEnableRedisplay_w, gxm_XmTextEnableRedisplay)
  XEN_NARGIFY_4(gxm_XmTextFindString_w, gxm_XmTextFindString)
  XEN_ARGIFY_4(gxm_XmCreateForm_w, gxm_XmCreateForm)
  XEN_ARGIFY_4(gxm_XmCreateFormDialog_w, gxm_XmCreateFormDialog)
  XEN_ARGIFY_4(gxm_XmCreateFrame_w, gxm_XmCreateFrame)
  XEN_NARGIFY_1(gxm_XmToggleButtonGadgetGetState_w, gxm_XmToggleButtonGadgetGetState)
  XEN_NARGIFY_3(gxm_XmToggleButtonGadgetSetState_w, gxm_XmToggleButtonGadgetSetState)
  XEN_ARGIFY_4(gxm_XmCreateToggleButtonGadget_w, gxm_XmCreateToggleButtonGadget)
  XEN_NARGIFY_1(gxm_XmToggleButtonGetState_w, gxm_XmToggleButtonGetState)
  XEN_NARGIFY_3(gxm_XmToggleButtonSetState_w, gxm_XmToggleButtonSetState)
  XEN_ARGIFY_4(gxm_XmCreateToggleButton_w, gxm_XmCreateToggleButton)
  XEN_ARGIFY_4(gxm_XmCreateLabelGadget_w, gxm_XmCreateLabelGadget)
  XEN_ARGIFY_4(gxm_XmCreateLabel_w, gxm_XmCreateLabel)
  XEN_NARGIFY_1(gxm_XmIsMotifWMRunning_w, gxm_XmIsMotifWMRunning)
  XEN_NARGIFY_3(gxm_XmListAddItem_w, gxm_XmListAddItem)
  XEN_NARGIFY_4(gxm_XmListAddItems_w, gxm_XmListAddItems)
  XEN_NARGIFY_4(gxm_XmListAddItemsUnselected_w, gxm_XmListAddItemsUnselected)
  XEN_NARGIFY_3(gxm_XmListAddItemUnselected_w, gxm_XmListAddItemUnselected)
  XEN_NARGIFY_2(gxm_XmListDeleteItem_w, gxm_XmListDeleteItem)
  XEN_NARGIFY_3(gxm_XmListDeleteItems_w, gxm_XmListDeleteItems)
  XEN_NARGIFY_3(gxm_XmListDeletePositions_w, gxm_XmListDeletePositions)
  XEN_NARGIFY_2(gxm_XmListDeletePos_w, gxm_XmListDeletePos)
  XEN_NARGIFY_3(gxm_XmListDeleteItemsPos_w, gxm_XmListDeleteItemsPos)
  XEN_NARGIFY_1(gxm_XmListDeleteAllItems_w, gxm_XmListDeleteAllItems)
  XEN_NARGIFY_4(gxm_XmListReplaceItems_w, gxm_XmListReplaceItems)
  XEN_NARGIFY_4(gxm_XmListReplaceItemsPos_w, gxm_XmListReplaceItemsPos)
  XEN_NARGIFY_4(gxm_XmListReplaceItemsUnselected_w, gxm_XmListReplaceItemsUnselected)
  XEN_NARGIFY_4(gxm_XmListReplaceItemsPosUnselected_w, gxm_XmListReplaceItemsPosUnselected)
  XEN_NARGIFY_4(gxm_XmListReplacePositions_w, gxm_XmListReplacePositions)
  XEN_NARGIFY_3(gxm_XmListSelectItem_w, gxm_XmListSelectItem)
  XEN_NARGIFY_3(gxm_XmListSelectPos_w, gxm_XmListSelectPos)
  XEN_NARGIFY_2(gxm_XmListDeselectItem_w, gxm_XmListDeselectItem)
  XEN_NARGIFY_2(gxm_XmListDeselectPos_w, gxm_XmListDeselectPos)
  XEN_NARGIFY_1(gxm_XmListDeselectAllItems_w, gxm_XmListDeselectAllItems)
  XEN_NARGIFY_2(gxm_XmListSetPos_w, gxm_XmListSetPos)
  XEN_NARGIFY_2(gxm_XmListSetBottomPos_w, gxm_XmListSetBottomPos)
  XEN_NARGIFY_2(gxm_XmListSetItem_w, gxm_XmListSetItem)
  XEN_NARGIFY_2(gxm_XmListSetBottomItem_w, gxm_XmListSetBottomItem)
  XEN_NARGIFY_2(gxm_XmListSetAddMode_w, gxm_XmListSetAddMode)
  XEN_NARGIFY_2(gxm_XmListItemExists_w, gxm_XmListItemExists)
  XEN_NARGIFY_2(gxm_XmListItemPos_w, gxm_XmListItemPos)
  XEN_NARGIFY_1(gxm_XmListGetKbdItemPos_w, gxm_XmListGetKbdItemPos)
  XEN_NARGIFY_2(gxm_XmListSetKbdItemPos_w, gxm_XmListSetKbdItemPos)
  XEN_NARGIFY_2(gxm_XmListYToPos_w, gxm_XmListYToPos)
  XEN_NARGIFY_2(gxm_XmListPosToBounds_w, gxm_XmListPosToBounds)
  XEN_NARGIFY_2(gxm_XmListGetMatchPos_w, gxm_XmListGetMatchPos)
  XEN_NARGIFY_2(gxm_XmListSetHorizPos_w, gxm_XmListSetHorizPos)
  XEN_NARGIFY_1(gxm_XmListUpdateSelectedList_w, gxm_XmListUpdateSelectedList)
  XEN_NARGIFY_2(gxm_XmListPosSelected_w, gxm_XmListPosSelected)
  XEN_ARGIFY_4(gxm_XmCreateList_w, gxm_XmCreateList)
  XEN_ARGIFY_4(gxm_XmCreateScrolledList_w, gxm_XmCreateScrolledList)
  XEN_NARGIFY_3(gxm_XmTranslateKey_w, gxm_XmTranslateKey)
  XEN_ARGIFY_4(gxm_XmCreateMainWindow_w, gxm_XmCreateMainWindow)
  XEN_NARGIFY_2(gxm_XmInstallImage_w, gxm_XmInstallImage)
  XEN_NARGIFY_1(gxm_XmUninstallImage_w, gxm_XmUninstallImage)
  XEN_NARGIFY_4(gxm_XmGetPixmap_w, gxm_XmGetPixmap)
  XEN_NARGIFY_5(gxm_XmGetPixmapByDepth_w, gxm_XmGetPixmapByDepth)
  XEN_NARGIFY_2(gxm_XmDestroyPixmap_w, gxm_XmDestroyPixmap)
  XEN_NARGIFY_1(gxm_XmUpdateDisplay_w, gxm_XmUpdateDisplay)
  XEN_NARGIFY_1(gxm_XmWidgetGetBaselines_w, gxm_XmWidgetGetBaselines)
  XEN_NARGIFY_2(gxm_XmRegisterSegmentEncoding_w, gxm_XmRegisterSegmentEncoding)
  XEN_NARGIFY_1(gxm_XmMapSegmentEncoding_w, gxm_XmMapSegmentEncoding)
  XEN_NARGIFY_1(gxm_XmCvtCTToXmString_w, gxm_XmCvtCTToXmString)
  XEN_NARGIFY_1(gxm_XmCvtXmStringToCT_w, gxm_XmCvtXmStringToCT)
  XEN_NARGIFY_5(gxm_XmConvertUnits_w, gxm_XmConvertUnits)
  XEN_ARGIFY_4(gxm_XmCreateSimpleMenuBar_w, gxm_XmCreateSimpleMenuBar)
  XEN_ARGIFY_4(gxm_XmCreateSimplePopupMenu_w, gxm_XmCreateSimplePopupMenu)
  XEN_ARGIFY_4(gxm_XmCreateSimplePulldownMenu_w, gxm_XmCreateSimplePulldownMenu)
  XEN_ARGIFY_4(gxm_XmCreateSimpleOptionMenu_w, gxm_XmCreateSimpleOptionMenu)
  XEN_ARGIFY_4(gxm_XmCreateSimpleRadioBox_w, gxm_XmCreateSimpleRadioBox)
  XEN_ARGIFY_4(gxm_XmCreateSimpleCheckBox_w, gxm_XmCreateSimpleCheckBox)
  XEN_NARGIFY_3(gxm_XmVaCreateSimpleMenuBar_w, gxm_XmVaCreateSimpleMenuBar)
  XEN_NARGIFY_4(gxm_XmVaCreateSimplePopupMenu_w, gxm_XmVaCreateSimplePopupMenu)
  XEN_NARGIFY_5(gxm_XmVaCreateSimplePulldownMenu_w, gxm_XmVaCreateSimplePulldownMenu)
  XEN_NARGIFY_7(gxm_XmVaCreateSimpleOptionMenu_w, gxm_XmVaCreateSimpleOptionMenu)
  XEN_NARGIFY_5(gxm_XmVaCreateSimpleRadioBox_w, gxm_XmVaCreateSimpleRadioBox)
  XEN_NARGIFY_4(gxm_XmVaCreateSimpleCheckBox_w, gxm_XmVaCreateSimpleCheckBox)
  XEN_NARGIFY_3(gxm_XmTrackingEvent_w, gxm_XmTrackingEvent)
  XEN_NARGIFY_1(gxm_XmSetColorCalculation_w, gxm_XmSetColorCalculation)
  XEN_NARGIFY_0(gxm_XmGetColorCalculation_w, gxm_XmGetColorCalculation)
  XEN_NARGIFY_3(gxm_XmGetColors_w, gxm_XmGetColors)
  XEN_NARGIFY_2(gxm_XmChangeColor_w, gxm_XmChangeColor)
  XEN_NARGIFY_2(gxm_XmStringCreate_w, gxm_XmStringCreate)
  XEN_NARGIFY_1(gxm_XmStringCreateLocalized_w, gxm_XmStringCreateLocalized)
  XEN_NARGIFY_1(gxm_XmStringDirectionCreate_w, gxm_XmStringDirectionCreate)
  XEN_NARGIFY_0(gxm_XmStringSeparatorCreate_w, gxm_XmStringSeparatorCreate)
  XEN_NARGIFY_1(gxm_XmStringInitContext_w, gxm_XmStringInitContext)
  XEN_NARGIFY_1(gxm_XmStringFreeContext_w, gxm_XmStringFreeContext)
#if MOTIF_2
  XEN_NARGIFY_2(gxm_XmStringConcatAndFree_w, gxm_XmStringConcatAndFree)
  XEN_NARGIFY_1(gxm_XmStringIsVoid_w, gxm_XmStringIsVoid)
  XEN_NARGIFY_1(gxm_XmCvtXmStringToByteStream_w, gxm_XmCvtXmStringToByteStream)
  XEN_NARGIFY_1(gxm_XmCvtByteStreamToXmString_w, gxm_XmCvtByteStreamToXmString)
  XEN_NARGIFY_1(gxm_XmStringByteStreamLength_w, gxm_XmStringByteStreamLength)
  XEN_NARGIFY_1(gxm_XmStringPeekNextTriple_w, gxm_XmStringPeekNextTriple)
  XEN_NARGIFY_1(gxm_XmStringGetNextTriple_w, gxm_XmStringGetNextTriple)
  XEN_NARGIFY_3(gxm_XmStringComponentCreate_w, gxm_XmStringComponentCreate)
  XEN_NARGIFY_7(gxm_XmStringUnparse_w, gxm_XmStringUnparse)
  XEN_NARGIFY_7(gxm_XmStringParseText_w, gxm_XmStringParseText)
  XEN_NARGIFY_2(gxm_XmStringToXmStringTable_w, gxm_XmStringToXmStringTable)
  XEN_NARGIFY_3(gxm_XmStringTableToXmString_w, gxm_XmStringTableToXmString)
  XEN_NARGIFY_8(gxm_XmStringTableUnparse_w, gxm_XmStringTableUnparse)
  XEN_NARGIFY_7(gxm_XmStringTableParseStringArray_w, gxm_XmStringTableParseStringArray)
  XEN_NARGIFY_1(gxm_XmDirectionToStringDirection_w, gxm_XmDirectionToStringDirection)
  XEN_NARGIFY_1(gxm_XmStringDirectionToDirection_w, gxm_XmStringDirectionToDirection)
  XEN_NARGIFY_4(gxm_XmStringGenerate_w, gxm_XmStringGenerate)
  XEN_NARGIFY_2(gxm_XmStringPutRendition_w, gxm_XmStringPutRendition)
  XEN_ARGIFY_2(gxm_XmParseMappingCreate_w, gxm_XmParseMappingCreate)
  XEN_ARGIFY_3(gxm_XmParseMappingSetValues_w, gxm_XmParseMappingSetValues)
  XEN_ARGIFY_3(gxm_XmParseMappingGetValues_w, gxm_XmParseMappingGetValues)
  XEN_NARGIFY_1(gxm_XmParseMappingFree_w, gxm_XmParseMappingFree)
  XEN_NARGIFY_2(gxm_XmParseTableFree_w, gxm_XmParseTableFree)
  XEN_NARGIFY_5(gxm_XmStringTableProposeTablist_w, gxm_XmStringTableProposeTablist)
  XEN_NARGIFY_2(gxm_XmTabSetValue_w, gxm_XmTabSetValue)
  XEN_NARGIFY_1(gxm_XmTabGetValues_w, gxm_XmTabGetValues)
  XEN_NARGIFY_1(gxm_XmTabFree_w, gxm_XmTabFree)
  XEN_NARGIFY_5(gxm_XmTabCreate_w, gxm_XmTabCreate)
  XEN_NARGIFY_1(gxm_XmTabListTabCount_w, gxm_XmTabListTabCount)
  XEN_NARGIFY_3(gxm_XmTabListRemoveTabs_w, gxm_XmTabListRemoveTabs)
  XEN_NARGIFY_4(gxm_XmTabListReplacePositions_w, gxm_XmTabListReplacePositions)
  XEN_NARGIFY_2(gxm_XmTabListGetTab_w, gxm_XmTabListGetTab)
  XEN_NARGIFY_3(gxm_XmTabListCopy_w, gxm_XmTabListCopy)
  XEN_NARGIFY_4(gxm_XmTabListInsertTabs_w, gxm_XmTabListInsertTabs)
  XEN_NARGIFY_3(gxm_XmRenderTableCvtFromProp_w, gxm_XmRenderTableCvtFromProp)
  XEN_NARGIFY_2(gxm_XmRenderTableCvtToProp_w, gxm_XmRenderTableCvtToProp)
  XEN_ARGIFY_3(gxm_XmRenditionUpdate_w, gxm_XmRenditionUpdate)
  XEN_ARGIFY_3(gxm_XmRenditionRetrieve_w, gxm_XmRenditionRetrieve)
  XEN_NARGIFY_1(gxm_XmRenditionFree_w, gxm_XmRenditionFree)
  XEN_ARGIFY_4(gxm_XmRenditionCreate_w, gxm_XmRenditionCreate)
  XEN_ARGIFY_3(gxm_XmRenderTableGetRenditions_w, gxm_XmRenderTableGetRenditions)
  XEN_NARGIFY_2(gxm_XmRenderTableGetRendition_w, gxm_XmRenderTableGetRendition)
  XEN_NARGIFY_1(gxm_XmRenderTableGetTags_w, gxm_XmRenderTableGetTags)
  XEN_NARGIFY_1(gxm_XmRenderTableFree_w, gxm_XmRenderTableFree)
  XEN_ARGIFY_3(gxm_XmRenderTableCopy_w, gxm_XmRenderTableCopy)
  XEN_ARGIFY_3(gxm_XmRenderTableRemoveRenditions_w, gxm_XmRenderTableRemoveRenditions)
  XEN_NARGIFY_4(gxm_XmRenderTableAddRenditions_w, gxm_XmRenderTableAddRenditions)
#endif
  XEN_NARGIFY_2(gxm_XmStringConcat_w, gxm_XmStringConcat)
  XEN_NARGIFY_1(gxm_XmStringCopy_w, gxm_XmStringCopy)
  XEN_NARGIFY_2(gxm_XmStringCompare_w, gxm_XmStringCompare)
  XEN_NARGIFY_1(gxm_XmStringEmpty_w, gxm_XmStringEmpty)
  XEN_NARGIFY_2(gxm_XmStringHasSubstring_w, gxm_XmStringHasSubstring)
  XEN_NARGIFY_1(gxm_XmStringFree_w, gxm_XmStringFree)
  XEN_NARGIFY_2(gxm_XmStringBaseline_w, gxm_XmStringBaseline)
  XEN_NARGIFY_2(gxm_XmStringWidth_w, gxm_XmStringWidth)
  XEN_NARGIFY_2(gxm_XmStringHeight_w, gxm_XmStringHeight)
  XEN_NARGIFY_2(gxm_XmStringExtent_w, gxm_XmStringExtent)
  XEN_NARGIFY_1(gxm_XmStringLineCount_w, gxm_XmStringLineCount)
  XEN_VARGIFY(gxm_XmStringDraw_w, gxm_XmStringDraw)
  XEN_VARGIFY(gxm_XmStringDrawImage_w, gxm_XmStringDrawImage)
  XEN_VARGIFY(gxm_XmStringDrawUnderline_w, gxm_XmStringDrawUnderline)
  XEN_NARGIFY_1(gxm_XmGetDestination_w, gxm_XmGetDestination)
  XEN_NARGIFY_1(gxm_XmIsTraversable_w, gxm_XmIsTraversable)
  XEN_NARGIFY_1(gxm_XmGetVisibility_w, gxm_XmGetVisibility)
  XEN_NARGIFY_1(gxm_XmGetTabGroup_w, gxm_XmGetTabGroup)
  XEN_NARGIFY_1(gxm_XmGetFocusWidget_w, gxm_XmGetFocusWidget)
  XEN_NARGIFY_2(gxm_XmProcessTraversal_w, gxm_XmProcessTraversal)
  XEN_ARGIFY_4(gxm_XmCreateMenuShell_w, gxm_XmCreateMenuShell)

  XEN_NARGIFY_1(gxm_XmIsMessageBox_w, gxm_XmIsMessageBox)
  XEN_NARGIFY_1(gxm_XmIsArrowButtonGadget_w, gxm_XmIsArrowButtonGadget)
  XEN_NARGIFY_1(gxm_XmIsArrowButton_w, gxm_XmIsArrowButton)
#if MOTIF_2
  XEN_NARGIFY_1(gxm_XmIsNotebook_w, gxm_XmIsNotebook)
#if HAVE_XP
  XEN_NARGIFY_1(gxm_XmIsPrintShell_w, gxm_XmIsPrintShell)
#endif
  XEN_NARGIFY_1(gxm_XmIsComboBox_w, gxm_XmIsComboBox)
  XEN_NARGIFY_1(gxm_XmIsContainer_w, gxm_XmIsContainer)
  XEN_NARGIFY_1(gxm_XmIsGrabShell_w, gxm_XmIsGrabShell)
  XEN_NARGIFY_1(gxm_XmIsIconGadget_w, gxm_XmIsIconGadget)
  XEN_NARGIFY_1(gxm_XmIsIconHeader_w, gxm_XmIsIconHeader)
#endif
  XEN_NARGIFY_1(gxm_XmIsPanedWindow_w, gxm_XmIsPanedWindow)
  XEN_NARGIFY_1(gxm_XmIsBulletinBoard_w, gxm_XmIsBulletinBoard)
  XEN_NARGIFY_1(gxm_XmIsPrimitive_w, gxm_XmIsPrimitive)
  XEN_NARGIFY_1(gxm_XmIsCascadeButtonGadget_w, gxm_XmIsCascadeButtonGadget)
  XEN_NARGIFY_1(gxm_XmIsCascadeButton_w, gxm_XmIsCascadeButton)
  XEN_NARGIFY_1(gxm_XmIsPushButtonGadget_w, gxm_XmIsPushButtonGadget)
  XEN_NARGIFY_1(gxm_XmIsPushButton_w, gxm_XmIsPushButton)
  XEN_NARGIFY_1(gxm_XmIsCommand_w, gxm_XmIsCommand)
  XEN_NARGIFY_1(gxm_XmIsRowColumn_w, gxm_XmIsRowColumn)
  XEN_NARGIFY_1(gxm_XmIsScale_w, gxm_XmIsScale)
  XEN_NARGIFY_1(gxm_XmIsScreen_w, gxm_XmIsScreen)
  XEN_NARGIFY_1(gxm_XmIsScrollBar_w, gxm_XmIsScrollBar)
  XEN_NARGIFY_1(gxm_XmIsDialogShell_w, gxm_XmIsDialogShell)
  XEN_NARGIFY_1(gxm_XmIsScrolledWindow_w, gxm_XmIsScrolledWindow)
  XEN_NARGIFY_1(gxm_XmIsDisplay_w, gxm_XmIsDisplay)
  XEN_NARGIFY_1(gxm_XmIsSelectionBox_w, gxm_XmIsSelectionBox)
  XEN_NARGIFY_1(gxm_XmIsDragContext_w, gxm_XmIsDragContext)
  XEN_NARGIFY_1(gxm_XmIsSeparatorGadget_w, gxm_XmIsSeparatorGadget)
  XEN_NARGIFY_1(gxm_XmIsDragIconObjectClass_w, gxm_XmIsDragIconObjectClass)
  XEN_NARGIFY_1(gxm_XmIsSeparator_w, gxm_XmIsSeparator)
  XEN_NARGIFY_1(gxm_XmIsDrawingArea_w, gxm_XmIsDrawingArea)
  XEN_NARGIFY_1(gxm_XmIsDrawnButton_w, gxm_XmIsDrawnButton)
  XEN_NARGIFY_1(gxm_XmIsDropSiteManager_w, gxm_XmIsDropSiteManager)
  XEN_NARGIFY_1(gxm_XmIsDropTransfer_w, gxm_XmIsDropTransfer)
  XEN_NARGIFY_1(gxm_XmIsTextField_w, gxm_XmIsTextField)
  XEN_NARGIFY_1(gxm_XmIsFileSelectionBox_w, gxm_XmIsFileSelectionBox)
  XEN_NARGIFY_1(gxm_XmIsText_w, gxm_XmIsText)
  XEN_NARGIFY_1(gxm_XmIsForm_w, gxm_XmIsForm)
  XEN_NARGIFY_1(gxm_XmIsFrame_w, gxm_XmIsFrame)
  XEN_NARGIFY_1(gxm_XmIsGadget_w, gxm_XmIsGadget)
  XEN_NARGIFY_1(gxm_XmIsToggleButtonGadget_w, gxm_XmIsToggleButtonGadget)
  XEN_NARGIFY_1(gxm_XmIsToggleButton_w, gxm_XmIsToggleButton)
  XEN_NARGIFY_1(gxm_XmIsLabelGadget_w, gxm_XmIsLabelGadget)
  XEN_NARGIFY_1(gxm_XmIsLabel_w, gxm_XmIsLabel)
  XEN_NARGIFY_1(gxm_XmIsVendorShell_w, gxm_XmIsVendorShell)
  XEN_NARGIFY_1(gxm_XmIsList_w, gxm_XmIsList)
  XEN_NARGIFY_1(gxm_XmIsMainWindow_w, gxm_XmIsMainWindow)
  XEN_NARGIFY_1(gxm_XmIsManager_w, gxm_XmIsManager)
  XEN_NARGIFY_1(gxm_XmIsMenuShell_w, gxm_XmIsMenuShell)

#if (!XM_DISABLE_DEPRECATED)
  XEN_NARGIFY_1(gxm_XmStringLength_w, gxm_XmStringLength)
  XEN_NARGIFY_2(gxm_XmStringByteCompare_w, gxm_XmStringByteCompare)
  XEN_NARGIFY_4(gxm_XmScrolledWindowSetAreas_w, gxm_XmScrolledWindowSetAreas)
  XEN_NARGIFY_4(gxm_XmFontListEntryCreate_r_w, gxm_XmFontListEntryCreate_r)
  XEN_NARGIFY_3(gxm_XmFontListCreate_r_w, gxm_XmFontListCreate_r)
  XEN_NARGIFY_1(gxm_XmFontListEntryFree_w, gxm_XmFontListEntryFree)
  XEN_NARGIFY_2(gxm_XmFontListEntryGetFont_w, gxm_XmFontListEntryGetFont)
  XEN_NARGIFY_1(gxm_XmFontListEntryGetTag_w, gxm_XmFontListEntryGetTag)
  XEN_NARGIFY_2(gxm_XmFontListAppendEntry_w, gxm_XmFontListAppendEntry)
  XEN_NARGIFY_1(gxm_XmFontListNextEntry_w, gxm_XmFontListNextEntry)
  XEN_NARGIFY_2(gxm_XmFontListRemoveEntry_w, gxm_XmFontListRemoveEntry)
  XEN_NARGIFY_4(gxm_XmFontListEntryLoad_w, gxm_XmFontListEntryLoad)
  XEN_NARGIFY_2(gxm_XmFontListCreate_w, gxm_XmFontListCreate)
  XEN_NARGIFY_1(gxm_XmFontListFree_w, gxm_XmFontListFree)
  XEN_NARGIFY_3(gxm_XmFontListAdd_w, gxm_XmFontListAdd)
  XEN_NARGIFY_1(gxm_XmFontListCopy_w, gxm_XmFontListCopy)
  XEN_NARGIFY_1(gxm_XmFontListInitFontContext_w, gxm_XmFontListInitFontContext)
  XEN_NARGIFY_1(gxm_XmFontListGetNextFont_w, gxm_XmFontListGetNextFont)
  XEN_NARGIFY_1(gxm_XmFontListFreeFontContext_w, gxm_XmFontListFreeFontContext)
  XEN_NARGIFY_1(gxm_XmStringGetNextComponent_w, gxm_XmStringGetNextComponent)
  XEN_NARGIFY_1(gxm_XmStringPeekNextComponent_w, gxm_XmStringPeekNextComponent)
  XEN_NARGIFY_1(gxm_XmStringGetNextSegment_w, gxm_XmStringGetNextSegment)
  XEN_NARGIFY_2(gxm_XmStringGetLtoR_w, gxm_XmStringGetLtoR)
  XEN_NARGIFY_3(gxm_XmFontListEntryCreate_w, gxm_XmFontListEntryCreate)
  XEN_NARGIFY_1(gxm_XmStringCreateSimple_w, gxm_XmStringCreateSimple)
  XEN_NARGIFY_4(gxm_XmStringSegmentCreate_w, gxm_XmStringSegmentCreate)
  XEN_NARGIFY_2(gxm_XmStringCreateLtoR_w, gxm_XmStringCreateLtoR)
  XEN_NARGIFY_1(gxm_XmListGetSelectedPos_w, gxm_XmListGetSelectedPos)
  XEN_NARGIFY_2(gxm_XmStringNCopy_w, gxm_XmStringNCopy)
  XEN_NARGIFY_3(gxm_XmStringNConcat_w, gxm_XmStringNConcat)
  XEN_NARGIFY_6(gxm_XmMainWindowSetAreas_w, gxm_XmMainWindowSetAreas)
  XEN_NARGIFY_1(gxm_XmMainWindowSep1_w, gxm_XmMainWindowSep1)
  XEN_NARGIFY_1(gxm_XmMainWindowSep2_w, gxm_XmMainWindowSep2)
  XEN_NARGIFY_1(gxm_XmMainWindowSep3_w, gxm_XmMainWindowSep3)
  XEN_NARGIFY_3(gxm_XmSetFontUnits_w, gxm_XmSetFontUnits)
  XEN_NARGIFY_2(gxm_XmSetFontUnit_w, gxm_XmSetFontUnit)
  XEN_NARGIFY_2(gxm_XmSetMenuCursor_w, gxm_XmSetMenuCursor)
  XEN_NARGIFY_1(gxm_XmGetMenuCursor_w, gxm_XmGetMenuCursor)
  XEN_NARGIFY_3(gxm_XmTrackingLocate_w, gxm_XmTrackingLocate)
  XEN_NARGIFY_1(gxm_XmAddTabGroup_w, gxm_XmAddTabGroup)
  XEN_NARGIFY_1(gxm_XmRemoveTabGroup_w, gxm_XmRemoveTabGroup)

  XEN_NARGIFY_1(XEN_XmFontList_p_w, XEN_XmFontList_p)
  XEN_NARGIFY_1(XEN_XmFontContext_p_w, XEN_XmFontContext_p)
  XEN_NARGIFY_1(XEN_XmFontListEntry_p_w, XEN_XmFontListEntry_p)
#endif

#endif

#if HAVE_XPM
  XEN_NARGIFY_4(gxm_XpmCreatePixmapFromData_w, gxm_XpmCreatePixmapFromData)
  XEN_NARGIFY_4(gxm_XpmCreateDataFromPixmap_w, gxm_XpmCreateDataFromPixmap)
  XEN_NARGIFY_4(gxm_XpmReadFileToPixmap_w, gxm_XpmReadFileToPixmap)
  XEN_NARGIFY_5(gxm_XpmWriteFileFromPixmap_w, gxm_XpmWriteFileFromPixmap)
  XEN_NARGIFY_4(gxm_XpmCreatePixmapFromBuffer_w, gxm_XpmCreatePixmapFromBuffer)
  XEN_NARGIFY_4(gxm_XpmCreateBufferFromImage_w, gxm_XpmCreateBufferFromImage)
  XEN_NARGIFY_4(gxm_XpmCreateBufferFromPixmap_w, gxm_XpmCreateBufferFromPixmap)
  XEN_NARGIFY_4(gxm_XpmCreatePixmapFromXpmImage_w, gxm_XpmCreatePixmapFromXpmImage)
  XEN_NARGIFY_5(gxm_XpmCreateXpmImageFromPixmap_w, gxm_XpmCreateXpmImageFromPixmap)
#endif
  XEN_NARGIFY_3(gxm_XGetPixel_w, gxm_XGetPixel)
  XEN_NARGIFY_1(gxm_XDestroyImage_w, gxm_XDestroyImage)
  XEN_NARGIFY_4(gxm_XPutPixel_w, gxm_XPutPixel)
  XEN_NARGIFY_5(gxm_XSubImage_w, gxm_XSubImage)
  XEN_NARGIFY_2(gxm_XAddPixel_w, gxm_XAddPixel)

#if HAVE_MOTIF
  XEN_NARGIFY_1(gxm_Pixel_w, gxm_Pixel)
  XEN_NARGIFY_1(gxm_GC_w, gxm_GC)
  XEN_NARGIFY_1(gxm_Widget_w, gxm_Widget)
  XEN_NARGIFY_1(XEN_XtAppContext_p_w, XEN_XtAppContext_p)
  XEN_NARGIFY_1(XEN_XtRequestId_p_w, XEN_XtRequestId_p)
  XEN_NARGIFY_1(XEN_XtWorkProcId_p_w, XEN_XtWorkProcId_p)
  XEN_NARGIFY_1(XEN_XtInputId_p_w, XEN_XtInputId_p)
  XEN_NARGIFY_1(XEN_XtIntervalId_p_w, XEN_XtIntervalId_p)
#endif
  XEN_NARGIFY_1(XEN_Screen_p_w, XEN_Screen_p)
  XEN_NARGIFY_1(XEN_XEvent_p_w, XEN_XEvent_p)
  XEN_NARGIFY_1(XEN_XRectangle_p_w, XEN_XRectangle_p)
  XEN_NARGIFY_1(XEN_XArc_p_w, XEN_XArc_p)
  XEN_NARGIFY_1(XEN_XPoint_p_w, XEN_XPoint_p)
  XEN_NARGIFY_1(XEN_XSegment_p_w, XEN_XSegment_p)
  XEN_NARGIFY_1(XEN_XColor_p_w, XEN_XColor_p)
  XEN_NARGIFY_1(XEN_Atom_p_w, XEN_Atom_p)
  XEN_NARGIFY_1(XEN_Colormap_p_w, XEN_Colormap_p)
  XEN_NARGIFY_1(XEN_XModifierKeymap_p_w, XEN_XModifierKeymap_p)
  XEN_NARGIFY_1(XEN_Depth_p_w, XEN_Depth_p)
  XEN_NARGIFY_1(XEN_Display_p_w, XEN_Display_p)
  XEN_NARGIFY_1(XEN_Font_p_w, XEN_Font_p)
  XEN_NARGIFY_1(XEN_GC_p_w, XEN_GC_p)
  XEN_NARGIFY_1(XEN_KeySym_p_w, XEN_KeySym_p)
  XEN_NARGIFY_1(XEN_Pixel_p_w, XEN_Pixel_p)
  XEN_NARGIFY_1(XEN_Pixmap_p_w, XEN_Pixmap_p)
  XEN_NARGIFY_1(XEN_Region_p_w, XEN_Region_p)
  XEN_NARGIFY_1(XEN_Time_p_w, XEN_Time_p)
  XEN_NARGIFY_1(XEN_Visual_p_w, XEN_Visual_p)
  XEN_NARGIFY_1(XEN_Window_p_w, XEN_Window_p)
  XEN_NARGIFY_1(XEN_Widget_p_w, XEN_Widget_p)
  XEN_NARGIFY_1(XEN_XmStringContext_p_w, XEN_XmStringContext_p)
  XEN_NARGIFY_1(XEN_XFontProp_p_w, XEN_XFontProp_p)
  XEN_NARGIFY_1(XEN_XFontSet_p_w, XEN_XFontSet_p)
  XEN_NARGIFY_1(XEN_XFontStruct_p_w, XEN_XFontStruct_p)
  XEN_NARGIFY_1(XEN_XGCValues_p_w, XEN_XGCValues_p)
  XEN_NARGIFY_1(XEN_XImage_p_w, XEN_XImage_p)
  XEN_NARGIFY_1(XEN_XVisualInfo_p_w, XEN_XVisualInfo_p)
  XEN_NARGIFY_1(XEN_XWMHints_p_w, XEN_XWMHints_p)
  XEN_NARGIFY_1(XEN_XWindowAttributes_p_w, XEN_XWindowAttributes_p)
  XEN_NARGIFY_1(XEN_XWindowChanges_p_w, XEN_XWindowChanges_p)
  XEN_NARGIFY_1(XEN_KeyCode_p_w, XEN_KeyCode_p)
  XEN_NARGIFY_1(XEN_XContext_p_w, XEN_XContext_p)
  XEN_NARGIFY_1(XEN_XCharStruct_p_w, XEN_XCharStruct_p)
  XEN_NARGIFY_1(XEN_XTextItem_p_w, XEN_XTextItem_p)
  XEN_NARGIFY_1(XEN_XStandardColormap_p_w, XEN_XStandardColormap_p)
  XEN_NARGIFY_1(XEN_Substitution_p_w, XEN_Substitution_p)
  XEN_NARGIFY_1(XEN_Cursor_p_w, XEN_Cursor_p)
#if HAVE_XP
  XEN_NARGIFY_1(XEN_XPContext_p_w, XEN_XPContext_p)
#endif
#if HAVE_MOTIF
  XEN_NARGIFY_1(XEN_WidgetClass_p_w, XEN_WidgetClass_p)
  XEN_NARGIFY_1(XEN_XmString_p_w, XEN_XmString_p)
#if MOTIF_2
  XEN_NARGIFY_1(XEN_XmParseTable_p_w, XEN_XmParseTable_p)
  XEN_NARGIFY_1(XEN_XmTab_p_w, XEN_XmTab_p)
  XEN_NARGIFY_1(XEN_XmRendition_p_w, XEN_XmRendition_p)
  XEN_NARGIFY_1(XEN_XmRenderTable_p_w, XEN_XmRenderTable_p)
  XEN_NARGIFY_1(XEN_XmTabList_p_w, XEN_XmTabList_p)
  XEN_NARGIFY_1(XEN_XmParseMapping_p_w, XEN_XmParseMapping_p)
#endif
  XEN_NARGIFY_1(XEN_XmTextSource_p_w, XEN_XmTextSource_p)
#endif
#if HAVE_XPM
  XEN_NARGIFY_1(XEN_XpmAttributes_p_w, XEN_XpmAttributes_p)
  XEN_NARGIFY_1(XEN_XpmImage_p_w, XEN_XpmImage_p)
  XEN_NARGIFY_1(XEN_XpmColorSymbol_p_w, XEN_XpmColorSymbol_p)
#endif

static void define_procedures(void)
{
  xm_gc_table = XEN_MAKE_VECTOR(1, XEN_FALSE);
  XEN_PROTECT_FROM_GC(xm_gc_table);
  xm_protected_size = 512;
  xm_protected = XEN_MAKE_VECTOR(xm_protected_size, XEN_FALSE);
  XEN_VECTOR_SET(xm_gc_table, 0, xm_protected);

#if HAVE_XP
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpStartPage" XM_POSTFIX, gxm_XpStartPage_w, 2, 0, 0, H_XpStartPage);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpEndPage" XM_POSTFIX, gxm_XpEndPage_w, 1, 0, 0, H_XpEndPage);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpCancelPage" XM_POSTFIX, gxm_XpCancelPage_w, 2, 0, 0, H_XpCancelPage);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpStartJob" XM_POSTFIX, gxm_XpStartJob_w, 2, 0, 0, H_XpStartJob);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpEndJob" XM_POSTFIX, gxm_XpEndJob_w, 1, 0, 0, H_XpEndJob);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpCancelJob" XM_POSTFIX, gxm_XpCancelJob_w, 2, 0, 0, H_XpCancelJob);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpStartDoc" XM_POSTFIX, gxm_XpStartDoc_w, 2, 0, 0, H_XpStartDoc);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpEndDoc" XM_POSTFIX, gxm_XpEndDoc_w, 1, 0, 0, H_XpEndDoc);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpCancelDoc" XM_POSTFIX, gxm_XpCancelDoc_w, 2, 0, 0, H_XpCancelDoc);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpRehashPrinterList" XM_POSTFIX, gxm_XpRehashPrinterList_w, 1, 0, 0, H_XpRehashPrinterList);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpCreateContext" XM_POSTFIX, gxm_XpCreateContext_w, 2, 0, 0, H_XpCreateContext);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpSetContext" XM_POSTFIX, gxm_XpSetContext_w, 2, 0, 0, H_XpSetContext);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpGetContext" XM_POSTFIX, gxm_XpGetContext_w, 1, 0, 0, H_XpGetContext);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpDestroyContext" XM_POSTFIX, gxm_XpDestroyContext_w, 2, 0, 0, H_XpDestroyContext);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpGetLocaleNetString" XM_POSTFIX, gxm_XpGetLocaleNetString_w, 0, 0, 0, H_XpGetLocaleNetString);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpNotifyPdm" XM_POSTFIX, gxm_XpNotifyPdm_w, 6, 0, 0, H_XpNotifyPdm);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpSendAuth" XM_POSTFIX, gxm_XpSendAuth_w, 2, 0, 0, H_XpSendAuth);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpGetImageResolution" XM_POSTFIX, gxm_XpGetImageResolution_w, 2, 0, 0, H_XpGetImageResolution);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpGetAttributes" XM_POSTFIX, gxm_XpGetAttributes_w, 3, 0, 0, H_XpGetAttributes);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpSetAttributes" XM_POSTFIX, gxm_XpSetAttributes_w, 5, 0, 0, H_XpSetAttributes);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpGetOneAttribute" XM_POSTFIX, gxm_XpGetOneAttribute_w, 4, 0, 0, H_XpGetOneAttribute);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpGetScreenOfContext" XM_POSTFIX, gxm_XpGetScreenOfContext_w, 2, 0, 0, H_XpGetScreenOfContext);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpFreePrinterList" XM_POSTFIX, gxm_XpFreePrinterList_w, 1, 0, 0, H_XpFreePrinterList);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpQueryVersion" XM_POSTFIX, gxm_XpQueryVersion_w, 1, 0, 0, H_XpQueryVersion);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpQueryExtension" XM_POSTFIX, gxm_XpQueryExtension_w, 1, 0, 0, H_XpQueryExtension);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpQueryScreens" XM_POSTFIX, gxm_XpQueryScreens_w, 1, 0, 0, H_XpQueryScreens);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpGetPdmStartParams" XM_POSTFIX, gxm_XpGetPdmStartParams_w, 5, 0, 0, H_XpGetPdmStartParams);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpGetAuthParams" XM_POSTFIX, gxm_XpGetAuthParams_w, 2, 0, 0, H_XpGetAuthParams);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpSendOneTicket" XM_POSTFIX, gxm_XpSendOneTicket_w, 4, 0, 0, H_XpSendOneTicket);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpGetPageDimensions" XM_POSTFIX, gxm_XpGetPageDimensions_w, 2, 0, 0, H_XpGetPageDimensions);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpSetImageResolution" XM_POSTFIX, gxm_XpSetImageResolution_w, 4, 0, 0, H_XpSetImageResolution);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpGetPrinterList" XM_POSTFIX, gxm_XpGetPrinterList_w, 2, 0, 0, H_XpGetPrinterList);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpSelectInput" XM_POSTFIX, gxm_XpSelectInput_w, 3, 0, 0, H_XpSelectInput);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpInputSelected" XM_POSTFIX, gxm_XpInputSelected_w, 3, 0, 0, H_XpInputSelected);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpPutDocumentData" XM_POSTFIX, gxm_XpPutDocumentData_w, 6, 0, 0, H_XpPutDocumentData);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpGetDocumentData" XM_POSTFIX, gxm_XpGetDocumentData_w, 5, 0, 0, H_XpGetDocumentData);
#endif
#if HAVE_MOTIF
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtSetArg" XM_POSTFIX, gxm_XtSetArg_w, 3, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtManageChildren" XM_POSTFIX, gxm_XtManageChildren_w, 2, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtManageChild" XM_POSTFIX, gxm_XtManageChild_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtUnmanageChildren" XM_POSTFIX, gxm_XtUnmanageChildren_w, 2, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtUnmanageChild" XM_POSTFIX, gxm_XtUnmanageChild_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtDispatchEvent" XM_POSTFIX, gxm_XtDispatchEvent_w, 1, 0, 0, H_XtDispatchEvent);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtCallAcceptFocus" XM_POSTFIX, gxm_XtCallAcceptFocus_w, 2, 0, 0, H_XtCallAcceptFocus);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAppPeekEvent" XM_POSTFIX, gxm_XtAppPeekEvent_w, 1, 0, 0, H_XtAppPeekEvent);
#if MOTIF_2
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtIsSubclass" XM_POSTFIX, gxm_XtIsSubclass_w, 2, 0, 0, H_XtIsSubclass);
#endif
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtIsObject" XM_POSTFIX, gxm_XtIsObject_w, 1, 0, 0, H_XtIsObject);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtIsManaged" XM_POSTFIX, gxm_XtIsManaged_w, 1, 0, 0, H_XtIsManaged);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtIsRealized" XM_POSTFIX, gxm_XtIsRealized_w, 1, 0, 0, H_XtIsRealized);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtIsSensitive" XM_POSTFIX, gxm_XtIsSensitive_w, 1, 0, 0, H_XtIsSensitive);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtOwnSelection" XM_POSTFIX, gxm_XtOwnSelection_w, 6, 0, 0, H_XtOwnSelection);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtOwnSelectionIncremental" XM_POSTFIX, gxm_XtOwnSelectionIncremental_w, 8, 0, 0, H_XtOwnSelectionIncremental);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtMakeResizeRequest" XM_POSTFIX, gxm_XtMakeResizeRequest_w, 3, 0, 0, H_XtMakeResizeRequest);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtTranslateCoords" XM_POSTFIX, gxm_XtTranslateCoords_w, 3, 0, 0, H_XtTranslateCoords);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtKeysymToKeycodeList" XM_POSTFIX, gxm_XtKeysymToKeycodeList_w, 2, 0, 0, H_XtKeysymToKeycodeList);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtParseTranslationTable" XM_POSTFIX, gxm_XtParseTranslationTable_w, 1, 0, 0, H_XtParseTranslationTable);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtParseAcceleratorTable" XM_POSTFIX, gxm_XtParseAcceleratorTable_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtOverrideTranslations" XM_POSTFIX, gxm_XtOverrideTranslations_w, 2, 0, 0, H_XtOverrideTranslations);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAugmentTranslations" XM_POSTFIX, gxm_XtAugmentTranslations_w, 2, 0, 0, H_XtAugmentTranslations);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtInstallAccelerators" XM_POSTFIX, gxm_XtInstallAccelerators_w, 2, 0, 0, H_XtInstallAccelerators);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtInstallAllAccelerators" XM_POSTFIX, gxm_XtInstallAllAccelerators_w, 2, 0, 0, H_XtInstallAllAccelerators);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtUninstallTranslations" XM_POSTFIX, gxm_XtUninstallTranslations_w, 1, 0, 0, H_XtUninstallTranslations);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAppAddActions" XM_POSTFIX, gxm_XtAppAddActions_w, 2, 0, 0, H_XtAppAddActions);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAppAddActionHook" XM_POSTFIX, gxm_XtAppAddActionHook_w, 2, 1, 0, H_XtAppAddActionHook);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtRemoveActionHook" XM_POSTFIX, gxm_XtRemoveActionHook_w, 1, 0, 0, H_XtRemoveActionHook);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtGetActionList" XM_POSTFIX, gxm_XtGetActionList_w, 1, 0, 0, H_XtGetActionList);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtCallActionProc" XM_POSTFIX, gxm_XtCallActionProc_w, 5, 0, 0, H_XtCallActionProc);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtRegisterGrabAction" XM_POSTFIX, gxm_XtRegisterGrabAction_w, 5, 0, 0, H_XtRegisterGrabAction);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtSetMultiClickTime" XM_POSTFIX, gxm_XtSetMultiClickTime_w, 2, 0, 0, H_XtSetMultiClickTime);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtGetMultiClickTime" XM_POSTFIX, gxm_XtGetMultiClickTime_w, 1, 0, 0, H_XtGetMultiClickTime);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtGetActionKeysym" XM_POSTFIX, gxm_XtGetActionKeysym_w, 1, 0, 0, H_XtGetActionKeysym);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtTranslateKeycode" XM_POSTFIX, gxm_XtTranslateKeycode_w, 3, 0, 0, H_XtTranslateKeycode);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtTranslateKey" XM_POSTFIX, gxm_XtTranslateKey_w, 3, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtSetKeyTranslator" XM_POSTFIX, gxm_XtSetKeyTranslator_w, 2, 0, 0, H_XtSetKeyTranslator);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtRegisterCaseConverter" XM_POSTFIX, gxm_XtRegisterCaseConverter_w, 4, 0, 0, H_XtRegisterCaseConverter);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtConvertCase" XM_POSTFIX, gxm_XtConvertCase_w, 2, 0, 0, H_XtConvertCase);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAddEventHandler" XM_POSTFIX, gxm_XtAddEventHandler_w, 4, 1, 0, H_XtAddEventHandler);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtRemoveEventHandler" XM_POSTFIX, gxm_XtRemoveEventHandler_w, 5, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAddRawEventHandler" XM_POSTFIX, gxm_XtAddRawEventHandler_w, 5, 0, 0, H_XtAddRawEventHandler);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtRemoveRawEventHandler" XM_POSTFIX, gxm_XtRemoveRawEventHandler_w, 5, 0, 0, H_XtRemoveRawEventHandler);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtInsertEventHandler" XM_POSTFIX, gxm_XtInsertEventHandler_w, 6, 0, 0, H_XtInsertEventHandler);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtInsertRawEventHandler" XM_POSTFIX, gxm_XtInsertRawEventHandler_w, 6, 0, 0, H_XtInsertRawEventHandler);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtDispatchEventToWidget" XM_POSTFIX, gxm_XtDispatchEventToWidget_w, 2, 0, 0, H_XtDispatchEventToWidget);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtBuildEventMask" XM_POSTFIX, gxm_XtBuildEventMask_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAddGrab" XM_POSTFIX, gxm_XtAddGrab_w, 3, 0, 0, H_XtAddGrab);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtRemoveGrab" XM_POSTFIX, gxm_XtRemoveGrab_w, 1, 0, 0, H_XtRemoveGrab);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAppProcessEvent" XM_POSTFIX, gxm_XtAppProcessEvent_w, 2, 0, 0, H_XtAppProcessEvent);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAppMainLoop" XM_POSTFIX, gxm_XtAppMainLoop_w, 1, 0, 0, H_XtAppMainLoop);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAddExposureToRegion" XM_POSTFIX, gxm_XtAddExposureToRegion_w, 2, 0, 0, H_XtAddExposureToRegion);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtSetKeyboardFocus" XM_POSTFIX, gxm_XtSetKeyboardFocus_w, 2, 0, 0, H_XtSetKeyboardFocus);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtGetKeyboardFocusWidget" XM_POSTFIX, gxm_XtGetKeyboardFocusWidget_w, 1, 0, 0, H_XtGetKeyboardFocusWidget);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtLastEventProcessed" XM_POSTFIX, gxm_XtLastEventProcessed_w, 1, 0, 0, H_XtLastEventProcessed);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtLastTimestampProcessed" XM_POSTFIX, gxm_XtLastTimestampProcessed_w, 1, 0, 0, H_XtLastTimestampProcessed);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAppAddTimeOut" XM_POSTFIX, gxm_XtAppAddTimeOut_w, 3, 1, 0, H_XtAppAddTimeOut);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtRemoveTimeOut" XM_POSTFIX, gxm_XtRemoveTimeOut_w, 1, 0, 0, H_XtRemoveTimeOut);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAppAddInput" XM_POSTFIX, gxm_XtAppAddInput_w, 4, 1, 0, H_XtAppAddInput);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtRemoveInput" XM_POSTFIX, gxm_XtRemoveInput_w, 1, 0, 0, H_XtRemoveInput);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAppNextEvent" XM_POSTFIX, gxm_XtAppNextEvent_w, 1, 0, 0, H_XtAppNextEvent);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAppPending" XM_POSTFIX, gxm_XtAppPending_w, 1, 0, 0, H_XtAppPending);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtRealizeWidget" XM_POSTFIX, gxm_XtRealizeWidget_w, 1, 0, 0, H_XtRealizeWidget);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtUnrealizeWidget" XM_POSTFIX, gxm_XtUnrealizeWidget_w, 1, 0, 0, H_XtUnrealizeWidget);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtDestroyWidget" XM_POSTFIX, gxm_XtDestroyWidget_w, 1, 0, 0, H_XtDestroyWidget);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtSetSensitive" XM_POSTFIX, gxm_XtSetSensitive_w, 2, 0, 0, H_XtSetSensitive);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtNameToWidget" XM_POSTFIX, gxm_XtNameToWidget_w, 2, 0, 0, H_XtNameToWidget);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtWindowToWidget" XM_POSTFIX, gxm_XtWindowToWidget_w, 2, 0, 0, H_XtWindowToWidget);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtMergeArgLists" XM_POSTFIX, gxm_XtMergeArgLists_w, 4, 0, 0, H_XtMergeArgLists);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtVaCreateArgsList" XM_POSTFIX, gxm_XtVaCreateArgsList_w, 2, 0, 0, H_XtVaCreateArgsList);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtDisplay" XM_POSTFIX, gxm_XtDisplay_w, 1, 0, 0, H_XtDisplay);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtDisplayOfObject" XM_POSTFIX, gxm_XtDisplayOfObject_w, 1, 0, 0, H_XtDisplayOfObject);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtScreen" XM_POSTFIX, gxm_XtScreen_w, 1, 0, 0, H_XtScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtScreenOfObject" XM_POSTFIX, gxm_XtScreenOfObject_w, 1, 0, 0, H_XtScreenOfObject);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtWindow" XM_POSTFIX, gxm_XtWindow_w, 1, 0, 0, H_XtWindow);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtWindowOfObject" XM_POSTFIX, gxm_XtWindowOfObject_w, 1, 0, 0, H_XtWindowOfObject);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtName" XM_POSTFIX, gxm_XtName_w, 1, 0, 0, H_XtName);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtSuperclass" XM_POSTFIX, gxm_XtSuperclass_w, 1, 0, 0, H_XtSuperclass);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtClass" XM_POSTFIX, gxm_XtClass_w, 1, 0, 0, H_XtClass);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtParent" XM_POSTFIX, gxm_XtParent_w, 1, 0, 0, H_XtParent);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAddCallback" XM_POSTFIX, gxm_XtAddCallback_w, 3, 1, 0, H_XtAddCallback);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtRemoveCallback" XM_POSTFIX, gxm_XtRemoveCallback_w, 3, 0, 0, H_XtRemoveCallback);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAddCallbacks" XM_POSTFIX, gxm_XtAddCallbacks_w, 3, 0, 0, H_XtAddCallbacks);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtRemoveCallbacks" XM_POSTFIX, gxm_XtRemoveCallbacks_w, 3, 0, 0, H_XtRemoveCallbacks);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtRemoveAllCallbacks" XM_POSTFIX, gxm_XtRemoveAllCallbacks_w, 2, 0, 0, H_XtRemoveAllCallbacks);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtCallCallbacks" XM_POSTFIX, gxm_XtCallCallbacks_w, 3, 0, 0, H_XtCallCallbacks);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtCallCallbackList" XM_POSTFIX, gxm_XtCallCallbackList_w, 3, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtHasCallbacks" XM_POSTFIX, gxm_XtHasCallbacks_w, 2, 0, 0, H_XtHasCallbacks);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtCreatePopupShell" XM_POSTFIX, gxm_XtCreatePopupShell_w, 4, 1, 0, H_XtCreatePopupShell);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtVaCreatePopupShell" XM_POSTFIX, gxm_XtVaCreatePopupShell_w, 4, 0, 0, H_XtVaCreatePopupShell);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtPopup" XM_POSTFIX, gxm_XtPopup_w, 2, 0, 0, H_XtPopup);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtPopupSpringLoaded" XM_POSTFIX, gxm_XtPopupSpringLoaded_w, 1, 0, 0, H_XtPopupSpringLoaded);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtCallbackNone" XM_POSTFIX, gxm_XtCallbackNone_w, 3, 0, 0, H_XtCallbackNone);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtCallbackNonexclusive" XM_POSTFIX, gxm_XtCallbackNonexclusive_w, 3, 0, 0, H_XtCallbackNonexclusive);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtCallbackExclusive" XM_POSTFIX, gxm_XtCallbackExclusive_w, 3, 0, 0, H_XtCallbackExclusive);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtPopdown" XM_POSTFIX, gxm_XtPopdown_w, 1, 0, 0, H_XtPopdown);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtCallbackPopdown" XM_POSTFIX, gxm_XtCallbackPopdown_w, 3, 0, 0, H_XtCallbackPopdown);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtCreateWidget" XM_POSTFIX, gxm_XtCreateWidget_w, 4, 1, 0, H_XtCreateWidget);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtCreateManagedWidget" XM_POSTFIX, gxm_XtCreateManagedWidget_w, 4, 1, 0, H_XtCreateManagedWidget);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtVaCreateWidget" XM_POSTFIX, gxm_XtVaCreateWidget_w, 4, 0, 0, H_XtVaCreateWidget);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtVaCreateManagedWidget" XM_POSTFIX, gxm_XtVaCreateManagedWidget_w, 4, 0, 0, H_XtVaCreateManagedWidget);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAppCreateShell" XM_POSTFIX, gxm_XtAppCreateShell_w, 5, 1, 0, H_XtAppCreateShell);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtVaAppCreateShell" XM_POSTFIX, gxm_XtVaAppCreateShell_w, 5, 0, 0, H_XtVaAppCreateShell);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtToolkitInitialize" XM_POSTFIX, gxm_XtToolkitInitialize_w, 0, 0, 0, H_XtToolkitInitialize);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtSetLanguageProc" XM_POSTFIX, gxm_XtSetLanguageProc_w, 3, 0, 0, H_XtSetLanguageProc);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtDisplayInitialize" XM_POSTFIX, gxm_XtDisplayInitialize_w, 8, 0, 0, H_XtDisplayInitialize);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtOpenApplication" XM_POSTFIX, gxm_XtOpenApplication_w, 9, 0, 0, H_XtOpenApplication);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtVaOpenApplication" XM_POSTFIX, gxm_XtVaOpenApplication_w, 8, 0, 0, H_XtVaOpenApplication);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAppInitialize" XM_POSTFIX, gxm_XtAppInitialize_w, 8, 0, 0, H_XtAppInitialize);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtVaAppInitialize" XM_POSTFIX, gxm_XtVaAppInitialize_w, 7, 0, 0, H_XtVaAppInitialize);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtOpenDisplay" XM_POSTFIX, gxm_XtOpenDisplay_w, 8, 0, 0, H_XtOpenDisplay);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtCreateApplicationContext" XM_POSTFIX, gxm_XtCreateApplicationContext_w, 0, 0, 0, H_XtCreateApplicationContext);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtDestroyApplicationContext" XM_POSTFIX, gxm_XtDestroyApplicationContext_w, 1, 0, 0, H_XtDestroyApplicationContext);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtInitializeWidgetClass" XM_POSTFIX, gxm_XtInitializeWidgetClass_w, 1, 0, 0, H_XtInitializeWidgetClass);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtWidgetToApplicationContext" XM_POSTFIX, gxm_XtWidgetToApplicationContext_w, 1, 0, 0, H_XtWidgetToApplicationContext);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtDisplayToApplicationContext" XM_POSTFIX, gxm_XtDisplayToApplicationContext_w, 1, 0, 0, H_XtDisplayToApplicationContext);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtCloseDisplay" XM_POSTFIX, gxm_XtCloseDisplay_w, 1, 0, 0, H_XtCloseDisplay);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtSetValues" XM_POSTFIX, gxm_XtSetValues_w, 2, 1, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtVaSetValues" XM_POSTFIX, gxm_XtVaSetValues_w, 2, 0, 0, H_XtVaSetValues);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtGetValues" XM_POSTFIX, gxm_XtGetValues_w, 2, 1, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtVaGetValues" XM_POSTFIX, gxm_XtVaGetValues_w, 2, 0, 0, H_XtVaGetValues);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAppSetErrorMsgHandler" XM_POSTFIX, gxm_XtAppSetErrorMsgHandler_w, 2, 0, 0, H_XtAppSetErrorMsgHandler);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAppSetWarningMsgHandler" XM_POSTFIX, gxm_XtAppSetWarningMsgHandler_w, 2, 0, 0, H_XtAppSetWarningMsgHandler);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAppErrorMsg" XM_POSTFIX, gxm_XtAppErrorMsg_w, 7, 0, 0, H_XtAppErrorMsg);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAppWarningMsg" XM_POSTFIX, gxm_XtAppWarningMsg_w, 7, 0, 0, H_XtAppWarningMsg);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAppSetErrorHandler" XM_POSTFIX, gxm_XtAppSetErrorHandler_w, 2, 0, 0, H_XtAppSetErrorHandler);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAppSetWarningHandler" XM_POSTFIX, gxm_XtAppSetWarningHandler_w, 2, 0, 0, H_XtAppSetWarningHandler);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAppError" XM_POSTFIX, gxm_XtAppError_w, 2, 0, 0, H_XtAppError);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtMalloc" XM_POSTFIX, gxm_XtMalloc_w, 1, 0, 0, H_XtMalloc);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtCalloc" XM_POSTFIX, gxm_XtCalloc_w, 2, 0, 0, H_XtCalloc);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtRealloc" XM_POSTFIX, gxm_XtRealloc_w, 2, 0, 0, H_XtRealloc);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtFree" XM_POSTFIX, gxm_XtFree_w, 1, 0, 0, H_XtFree);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAppAddWorkProc" XM_POSTFIX, gxm_XtAppAddWorkProc_w, 2, 1, 0, H_XtAppAddWorkProc);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtRemoveWorkProc" XM_POSTFIX, gxm_XtRemoveWorkProc_w, 1, 0, 0, H_XtRemoveWorkProc);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtGetGC" XM_POSTFIX, gxm_XtGetGC_w, 3, 0, 0, H_XtGetGC);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAllocateGC" XM_POSTFIX, gxm_XtAllocateGC_w, 6, 0, 0, H_XtAllocateGC);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtDestroyGC" XM_POSTFIX, gxm_XtDestroyGC_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtReleaseGC" XM_POSTFIX, gxm_XtReleaseGC_w, 2, 0, 0, H_XtReleaseGC);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtSetWMColormapWindows" XM_POSTFIX, gxm_XtSetWMColormapWindows_w, 3, 0, 0, H_XtSetWMColormapWindows);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtFindFile" XM_POSTFIX, gxm_XtFindFile_w, 4, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtResolvePathname" XM_POSTFIX, gxm_XtResolvePathname_w, 8, 0, 0, H_XtResolvePathname);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtDisownSelection" XM_POSTFIX, gxm_XtDisownSelection_w, 3, 0, 0, H_XtDisownSelection);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtGetSelectionValue" XM_POSTFIX, gxm_XtGetSelectionValue_w, 6, 0, 0, H_XtGetSelectionValue);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtGetSelectionValues" XM_POSTFIX, gxm_XtGetSelectionValues_w, 7, 0, 0, H_XtGetSelectionValues);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAppSetSelectionTimeout" XM_POSTFIX, gxm_XtAppSetSelectionTimeout_w, 2, 0, 0, H_XtAppSetSelectionTimeout);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAppGetSelectionTimeout" XM_POSTFIX, gxm_XtAppGetSelectionTimeout_w, 1, 0, 0, H_XtAppGetSelectionTimeout);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtGetSelectionRequest" XM_POSTFIX, gxm_XtGetSelectionRequest_w, 3, 0, 0, H_XtGetSelectionRequest);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtGetSelectionValueIncremental" XM_POSTFIX, gxm_XtGetSelectionValueIncremental_w, 6, 0, 0, H_XtGetSelectionValueIncremental);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtGetSelectionValuesIncremental" XM_POSTFIX, gxm_XtGetSelectionValuesIncremental_w, 7, 0, 0, H_XtGetSelectionValuesIncremental);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtCreateSelectionRequest" XM_POSTFIX, gxm_XtCreateSelectionRequest_w, 2, 0, 0, H_XtCreateSelectionRequest);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtSendSelectionRequest" XM_POSTFIX, gxm_XtSendSelectionRequest_w, 3, 0, 0, H_XtSendSelectionRequest);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtCancelSelectionRequest" XM_POSTFIX, gxm_XtCancelSelectionRequest_w, 2, 0, 0, H_XtCancelSelectionRequest);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtGrabKey" XM_POSTFIX, gxm_XtGrabKey_w, 6, 0, 0, H_XtGrabKey);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtUngrabKey" XM_POSTFIX, gxm_XtUngrabKey_w, 3, 0, 0, H_XtUngrabKey);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtGrabKeyboard" XM_POSTFIX, gxm_XtGrabKeyboard_w, 5, 0, 0, H_XtGrabKeyboard);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtUngrabKeyboard" XM_POSTFIX, gxm_XtUngrabKeyboard_w, 2, 0, 0, H_XtUngrabKeyboard);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtGrabButton" XM_POSTFIX, gxm_XtGrabButton_w, 9, 0, 0, H_XtGrabButton);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtUngrabButton" XM_POSTFIX, gxm_XtUngrabButton_w, 3, 0, 0, H_XtUngrabButton);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtGrabPointer" XM_POSTFIX, gxm_XtGrabPointer_w, 8, 0, 0, H_XtGrabPointer);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtUngrabPointer" XM_POSTFIX, gxm_XtUngrabPointer_w, 2, 0, 0, H_XtUngrabPointer);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtGetApplicationNameAndClass" XM_POSTFIX, gxm_XtGetApplicationNameAndClass_w, 1, 0, 0, H_XtGetApplicationNameAndClass);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtGetDisplays" XM_POSTFIX, gxm_XtGetDisplays_w, 1, 0, 0, H_XtGetDisplays);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtToolkitThreadInitialize" XM_POSTFIX, gxm_XtToolkitThreadInitialize_w, 0, 0, 0, H_XtToolkitThreadInitialize);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAppLock" XM_POSTFIX, gxm_XtAppLock_w, 1, 0, 0, H_XtAppLock);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAppUnlock" XM_POSTFIX, gxm_XtAppUnlock_w, 1, 0, 0, H_XtAppUnlock);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtIsRectObj" XM_POSTFIX, gxm_XtIsRectObj_w, 1, 0, 0, H_XtIsRectObj);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtIsWidget" XM_POSTFIX, gxm_XtIsWidget_w, 1, 0, 0, H_XtIsWidget);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtIsComposite" XM_POSTFIX, gxm_XtIsComposite_w, 1, 0, 0, H_XtIsComposite);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtIsConstraint" XM_POSTFIX, gxm_XtIsConstraint_w, 1, 0, 0, H_XtIsConstraint);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtIsShell" XM_POSTFIX, gxm_XtIsShell_w, 1, 0, 0, H_XtIsShell);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtIsOverrideShell" XM_POSTFIX, gxm_XtIsOverrideShell_w, 1, 0, 0, H_XtIsOverrideShell);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtIsWMShell" XM_POSTFIX, gxm_XtIsWMShell_w, 1, 0, 0, H_XtIsWMShell);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtIsVendorShell" XM_POSTFIX, gxm_XtIsVendorShell_w, 1, 0, 0, H_XtIsVendorShell);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtIsTransientShell" XM_POSTFIX, gxm_XtIsTransientShell_w, 1, 0, 0, H_XtIsTransientShell);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtIsTopLevelShell" XM_POSTFIX, gxm_XtIsTopLevelShell_w, 1, 0, 0, H_XtIsTopLevelShell);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtIsApplicationShell" XM_POSTFIX, gxm_XtIsApplicationShell_w, 1, 0, 0, H_XtIsApplicationShell);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtIsSessionShell" XM_POSTFIX, gxm_XtIsSessionShell_w, 1, 0, 0, H_XtIsSessionShell);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtMapWidget" XM_POSTFIX, gxm_XtMapWidget_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtUnmapWidget" XM_POSTFIX, gxm_XtUnmapWidget_w, 1, 0, 0, H_XtUnmapWidget);
   XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAppContext" XM_POSTFIX, gxm_XtAppContext_w, 1, 0, 0, NULL);
#endif
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XUniqueContext" XM_POSTFIX, gxm_XUniqueContext_w, 0, 0, 0, H_XUniqueContext);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XLoadQueryFont" XM_POSTFIX, gxm_XLoadQueryFont_w, 2, 0, 0, H_XLoadQueryFont);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XQueryFont" XM_POSTFIX, gxm_XQueryFont_w, 2, 0, 0, H_XQueryFont);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetMotionEvents" XM_POSTFIX, gxm_XGetMotionEvents_w, 4, 0, 0, H_XGetMotionEvents);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDeleteModifiermapEntry" XM_POSTFIX, gxm_XDeleteModifiermapEntry_w, 3, 0, 0, H_XDeleteModifiermapEntry);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetModifierMapping" XM_POSTFIX, gxm_XGetModifierMapping_w, 1, 0, 0, H_XGetModifierMapping);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XInsertModifiermapEntry" XM_POSTFIX, gxm_XInsertModifiermapEntry_w, 3, 0, 0, H_XInsertModifiermapEntry);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XNewModifiermap" XM_POSTFIX, gxm_XNewModifiermap_w, 1, 0, 0, H_XNewModifiermap);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCreateImage" XM_POSTFIX, gxm_XCreateImage_w, 0, 0, 1, H_XCreateImage);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetImage" XM_POSTFIX, gxm_XGetImage_w, 8, 0, 0, H_XGetImage);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetSubImage" XM_POSTFIX, gxm_XGetSubImage_w, 0, 0, 1, H_XGetSubImage);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XOpenDisplay" XM_POSTFIX, gxm_XOpenDisplay_w, 1, 0, 0, H_XOpenDisplay);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFetchBytes" XM_POSTFIX, gxm_XFetchBytes_w, 1, 0, 0, H_XFetchBytes);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFetchBuffer" XM_POSTFIX, gxm_XFetchBuffer_w, 2, 0, 0, H_XFetchBuffer);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetAtomName" XM_POSTFIX, gxm_XGetAtomName_w, 2, 0, 0, H_XGetAtomName);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDisplayName" XM_POSTFIX, gxm_XDisplayName_w, 1, 0, 0, H_XDisplayName);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XKeysymToString" XM_POSTFIX, gxm_XKeysymToString_w, 1, 0, 0, H_XKeysymToString);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSynchronize" XM_POSTFIX, gxm_XSynchronize_w, 2, 0, 0, H_XSynchronize);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetAfterFunction" XM_POSTFIX, gxm_XSetAfterFunction_w, 2, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XInternAtom" XM_POSTFIX, gxm_XInternAtom_w, 3, 0, 0, H_XInternAtom);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCopyColormapAndFree" XM_POSTFIX, gxm_XCopyColormapAndFree_w, 2, 0, 0, H_XCopyColormapAndFree);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCreateColormap" XM_POSTFIX, gxm_XCreateColormap_w, 4, 0, 0, H_XCreateColormap);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCreatePixmapCursor" XM_POSTFIX, gxm_XCreatePixmapCursor_w, 7, 0, 0, H_XCreatePixmapCursor);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCreateGlyphCursor" XM_POSTFIX, gxm_XCreateGlyphCursor_w, 7, 0, 0, H_XCreateGlyphCursor);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCreateFontCursor" XM_POSTFIX, gxm_XCreateFontCursor_w, 2, 0, 0, H_XCreateFontCursor);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XLoadFont" XM_POSTFIX, gxm_XLoadFont_w, 2, 0, 0, H_XLoadFont);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCreateGC" XM_POSTFIX, gxm_XCreateGC_w, 4, 0, 0, H_XCreateGC);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFlushGC" XM_POSTFIX, gxm_XFlushGC_w, 2, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCreatePixmap" XM_POSTFIX, gxm_XCreatePixmap_w, 5, 0, 0, H_XCreatePixmap);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCreateBitmapFromData" XM_POSTFIX, gxm_XCreateBitmapFromData_w, 5, 0, 0, H_XCreateBitmapFromData);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCreatePixmapFromBitmapData" XM_POSTFIX, gxm_XCreatePixmapFromBitmapData_w, 8, 0, 0, H_XCreatePixmapFromBitmapData);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCreateSimpleWindow" XM_POSTFIX, gxm_XCreateSimpleWindow_w, 9, 0, 0, H_XCreateSimpleWindow);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetSelectionOwner" XM_POSTFIX, gxm_XGetSelectionOwner_w, 2, 0, 0, H_XGetSelectionOwner);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCreateWindow" XM_POSTFIX, gxm_XCreateWindow_w, 0, 0, 1, H_XCreateWindow);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XListInstalledColormaps" XM_POSTFIX, gxm_XListInstalledColormaps_w, 2, 0, 0, H_XListInstalledColormaps);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XListFonts" XM_POSTFIX, gxm_XListFonts_w, 3, 0, 0, H_XListFonts);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XListFontsWithInfo" XM_POSTFIX, gxm_XListFontsWithInfo_w, 3, 0, 0, H_XListFontsWithInfo);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetFontPath" XM_POSTFIX, gxm_XGetFontPath_w, 1, 0, 0, H_XGetFontPath);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XListExtensions" XM_POSTFIX, gxm_XListExtensions_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XListProperties" XM_POSTFIX, gxm_XListProperties_w, 2, 0, 0, H_XListProperties);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XKeycodeToKeysym" XM_POSTFIX, gxm_XKeycodeToKeysym_w, 3, 0, 0, H_XKeycodeToKeysym);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XLookupKeysym" XM_POSTFIX, gxm_XLookupKeysym_w, 2, 0, 0, H_XLookupKeysym);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetKeyboardMapping" XM_POSTFIX, gxm_XGetKeyboardMapping_w, 3, 0, 0, H_XGetKeyboardMapping);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XStringToKeysym" XM_POSTFIX, gxm_XStringToKeysym_w, 1, 0, 0, H_XStringToKeysym);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XMaxRequestSize" XM_POSTFIX, gxm_XMaxRequestSize_w, 1, 0, 0, H_XMaxRequestSize);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XExtendedMaxRequestSize" XM_POSTFIX, gxm_XExtendedMaxRequestSize_w, 1, 0, 0, H_XExtendedMaxRequestSize);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDisplayMotionBufferSize" XM_POSTFIX, gxm_XDisplayMotionBufferSize_w, 1, 0, 0, H_XDisplayMotionBufferSize);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XVisualIDFromVisual" XM_POSTFIX, gxm_XVisualIDFromVisual_w, 1, 0, 0, H_XVisualIDFromVisual);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XInitThreads" XM_POSTFIX, gxm_XInitThreads_w, 0, 0, 0, H_XInitThreads);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XLockDisplay" XM_POSTFIX, gxm_XLockDisplay_w, 1, 0, 0, H_XLockDisplay);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XUnlockDisplay" XM_POSTFIX, gxm_XUnlockDisplay_w, 1, 0, 0, H_XUnlockDisplay);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XRootWindow" XM_POSTFIX, gxm_XRootWindow_w, 2, 0, 0, H_RootWindow);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDefaultRootWindow" XM_POSTFIX, gxm_XDefaultRootWindow_w, 1, 0, 0, H_DefaultRootWindow);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XRootWindowOfScreen" XM_POSTFIX, gxm_XRootWindowOfScreen_w, 1, 0, 0, H_RootWindowOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDefaultVisual" XM_POSTFIX, gxm_XDefaultVisual_w, 2, 0, 0, H_DefaultVisual);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDefaultVisualOfScreen" XM_POSTFIX, gxm_XDefaultVisualOfScreen_w, 1, 0, 0, H_DefaultVisualOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDefaultGC" XM_POSTFIX, gxm_XDefaultGC_w, 2, 0, 0, H_DefaultGC);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDefaultGCOfScreen" XM_POSTFIX, gxm_XDefaultGCOfScreen_w, 1, 0, 0, H_DefaultGCOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XBlackPixel" XM_POSTFIX, gxm_XBlackPixel_w, 2, 0, 0, H_BlackPixel);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XWhitePixel" XM_POSTFIX, gxm_XWhitePixel_w, 2, 0, 0, H_WhitePixel);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XAllPlanes" XM_POSTFIX, gxm_XAllPlanes_w, 0, 0, 0, H_AllPlanes);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XBlackPixelOfScreen" XM_POSTFIX, gxm_XBlackPixelOfScreen_w, 1, 0, 0, H_BlackPixelOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XWhitePixelOfScreen" XM_POSTFIX, gxm_XWhitePixelOfScreen_w, 1, 0, 0, H_WhitePixelOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XNextRequest" XM_POSTFIX, gxm_XNextRequest_w, 1, 0, 0, H_NextRequest);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XLastKnownRequestProcessed" XM_POSTFIX, gxm_XLastKnownRequestProcessed_w, 1, 0, 0, H_LastKnownRequestProcessed);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XServerVendor" XM_POSTFIX, gxm_XServerVendor_w, 1, 0, 0, H_ServerVendor);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDisplayString" XM_POSTFIX, gxm_XDisplayString_w, 1, 0, 0, H_DisplayString);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDefaultColormap" XM_POSTFIX, gxm_XDefaultColormap_w, 2, 0, 0, H_DefaultColormap);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDefaultColormapOfScreen" XM_POSTFIX, gxm_XDefaultColormapOfScreen_w, 1, 0, 0, H_DefaultColormapOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDisplayOfScreen" XM_POSTFIX, gxm_XDisplayOfScreen_w, 1, 0, 0, H_DisplayOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XScreenOfDisplay" XM_POSTFIX, gxm_XScreenOfDisplay_w, 2, 0, 0, H_ScreenOfDisplay);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDefaultScreenOfDisplay" XM_POSTFIX, gxm_XDefaultScreenOfDisplay_w, 1, 0, 0, H_DefaultScreenOfDisplay);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XEventMaskOfScreen" XM_POSTFIX, gxm_XEventMaskOfScreen_w, 1, 0, 0, H_EventMaskOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XScreenNumberOfScreen" XM_POSTFIX, gxm_XScreenNumberOfScreen_w, 1, 0, 0, H_XScreenNumberOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetErrorHandler" XM_POSTFIX, gxm_XSetErrorHandler_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetIOErrorHandler" XM_POSTFIX, gxm_XSetIOErrorHandler_w, 1, 0, 0, H_XSetIOErrorHandler);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XListPixmapFormats" XM_POSTFIX, gxm_XListPixmapFormats_w, 1, 0, 0, H_XListPixmapFormats);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XListDepths" XM_POSTFIX, gxm_XListDepths_w, 2, 0, 0, H_XListDepths);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XReconfigureWMWindow" XM_POSTFIX, gxm_XReconfigureWMWindow_w, 5, 0, 0, H_XReconfigureWMWindow);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetWMProtocols" XM_POSTFIX, gxm_XGetWMProtocols_w, 2, 0, 0, H_XGetWMProtocols);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetWMProtocols" XM_POSTFIX, gxm_XSetWMProtocols_w, 4, 0, 0, H_XSetWMProtocols);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XIconifyWindow" XM_POSTFIX, gxm_XIconifyWindow_w, 3, 0, 0, H_XIconifyWindow);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XWithdrawWindow" XM_POSTFIX, gxm_XWithdrawWindow_w, 3, 0, 0, H_XWithdrawWindow);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetCommand" XM_POSTFIX, gxm_XGetCommand_w, 2, 0, 0, H_XGetCommand);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetWMColormapWindows" XM_POSTFIX, gxm_XGetWMColormapWindows_w, 2, 0, 0, H_XGetWMColormapWindows);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetWMColormapWindows" XM_POSTFIX, gxm_XSetWMColormapWindows_w, 4, 0, 0, H_XSetWMColormapWindows);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFreeStringList" XM_POSTFIX, gxm_XFreeStringList_w, 1, 0, 0, H_XFreeStringList);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetTransientForHint" XM_POSTFIX, gxm_XSetTransientForHint_w, 3, 0, 0, H_XSetTransientForHint);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XActivateScreenSaver" XM_POSTFIX, gxm_XActivateScreenSaver_w, 1, 0, 0, H_XActivateScreenSaver);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XAllocColor" XM_POSTFIX, gxm_XAllocColor_w, 3, 0, 0, H_XAllocColor);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XAllocColorCells" XM_POSTFIX, gxm_XAllocColorCells_w, 5, 0, 0, H_XAllocColorCells);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XAllocColorPlanes" XM_POSTFIX, gxm_XAllocColorPlanes_w, 0, 0, 1, H_XAllocColorPlanes);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XAllocNamedColor" XM_POSTFIX, gxm_XAllocNamedColor_w, 5, 0, 0, H_XAllocNamedColor);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XAllowEvents" XM_POSTFIX, gxm_XAllowEvents_w, 3, 0, 0, H_XAllowEvents);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XAutoRepeatOff" XM_POSTFIX, gxm_XAutoRepeatOff_w, 1, 0, 0, H_XAutoRepeatOff);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XAutoRepeatOn" XM_POSTFIX, gxm_XAutoRepeatOn_w, 1, 0, 0, H_XAutoRepeatOn);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XBell" XM_POSTFIX, gxm_XBell_w, 2, 0, 0, H_XBell);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XBitmapBitOrder" XM_POSTFIX, gxm_XBitmapBitOrder_w, 1, 0, 0, H_BitmapBitOrder);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XBitmapPad" XM_POSTFIX, gxm_XBitmapPad_w, 1, 0, 0, H_BitmapPad);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XBitmapUnit" XM_POSTFIX, gxm_XBitmapUnit_w, 1, 0, 0, H_BitmapUnit);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCellsOfScreen" XM_POSTFIX, gxm_XCellsOfScreen_w, 1, 0, 0, H_CellsOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XChangeActivePointerGrab" XM_POSTFIX, gxm_XChangeActivePointerGrab_w, 4, 0, 0, H_XChangeActivePointerGrab);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XChangeGC" XM_POSTFIX, gxm_XChangeGC_w, 4, 0, 0, H_XChangeGC);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XChangeKeyboardControl" XM_POSTFIX, gxm_XChangeKeyboardControl_w, 3, 0, 0, H_XChangeKeyboardControl);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XChangeKeyboardMapping" XM_POSTFIX, gxm_XChangeKeyboardMapping_w, 5, 0, 0, H_XChangeKeyboardMapping);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XChangePointerControl" XM_POSTFIX, gxm_XChangePointerControl_w, 6, 0, 0, H_XChangePointerControl);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XChangeProperty" XM_POSTFIX, gxm_XChangeProperty_w, 7, 1, 0, H_XChangeProperty);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XChangeWindowAttributes" XM_POSTFIX, gxm_XChangeWindowAttributes_w, 4, 0, 0, H_XChangeWindowAttributes);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCheckIfEvent" XM_POSTFIX, gxm_XCheckIfEvent_w, 3, 0, 0, H_XCheckIfEvent);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCheckMaskEvent" XM_POSTFIX, gxm_XCheckMaskEvent_w, 3, 0, 0, H_XCheckMaskEvent);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCheckTypedEvent" XM_POSTFIX, gxm_XCheckTypedEvent_w, 2, 0, 0, H_XCheckTypedEvent);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCheckTypedWindowEvent" XM_POSTFIX, gxm_XCheckTypedWindowEvent_w, 3, 0, 0, H_XCheckTypedWindowEvent);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCheckWindowEvent" XM_POSTFIX, gxm_XCheckWindowEvent_w, 3, 0, 0, H_XCheckWindowEvent);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCirculateSubwindows" XM_POSTFIX, gxm_XCirculateSubwindows_w, 3, 0, 0, H_XCirculateSubwindows);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCirculateSubwindowsDown" XM_POSTFIX, gxm_XCirculateSubwindowsDown_w, 2, 0, 0, H_XCirculateSubwindowsDown);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCirculateSubwindowsUp" XM_POSTFIX, gxm_XCirculateSubwindowsUp_w, 2, 0, 0, H_XCirculateSubwindowsUp);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XClearArea" XM_POSTFIX, gxm_XClearArea_w, 7, 0, 0, H_XClearArea);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XClearWindow" XM_POSTFIX, gxm_XClearWindow_w, 2, 0, 0, H_XClearWindow);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCloseDisplay" XM_POSTFIX, gxm_XCloseDisplay_w, 1, 0, 0, H_XCloseDisplay);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XConfigureWindow" XM_POSTFIX, gxm_XConfigureWindow_w, 4, 0, 0, H_XConfigureWindow);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XConnectionNumber" XM_POSTFIX, gxm_XConnectionNumber_w, 1, 0, 0, H_XConnectionNumber);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XConvertSelection" XM_POSTFIX, gxm_XConvertSelection_w, 6, 0, 0, H_XConvertSelection);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCopyArea" XM_POSTFIX, gxm_XCopyArea_w, 0, 0, 1, H_XCopyArea);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCopyGC" XM_POSTFIX, gxm_XCopyGC_w, 4, 0, 0, H_XCopyGC);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCopyPlane" XM_POSTFIX, gxm_XCopyPlane_w, 0, 0, 1, H_XCopyPlane);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDefaultDepth" XM_POSTFIX, gxm_XDefaultDepth_w, 2, 0, 0, H_DefaultDepth);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDefaultDepthOfScreen" XM_POSTFIX, gxm_XDefaultDepthOfScreen_w, 1, 0, 0, H_DefaultDepthOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDefaultScreen" XM_POSTFIX, gxm_XDefaultScreen_w, 1, 0, 0, H_DefaultScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDefineCursor" XM_POSTFIX, gxm_XDefineCursor_w, 3, 0, 0, H_XDefineCursor);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDeleteProperty" XM_POSTFIX, gxm_XDeleteProperty_w, 3, 0, 0, H_XDeleteProperty);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDestroyWindow" XM_POSTFIX, gxm_XDestroyWindow_w, 2, 0, 0, H_XDestroyWindow);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDestroySubwindows" XM_POSTFIX, gxm_XDestroySubwindows_w, 2, 0, 0, H_XDestroySubwindows);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDoesBackingStore" XM_POSTFIX, gxm_XDoesBackingStore_w, 1, 0, 0, H_XDoesBackingStore);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDoesSaveUnders" XM_POSTFIX, gxm_XDoesSaveUnders_w, 1, 0, 0, H_XDoesSaveUnders);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDisableAccessControl" XM_POSTFIX, gxm_XDisableAccessControl_w, 1, 0, 0, H_XDisableAccessControl);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDisplayCells" XM_POSTFIX, gxm_XDisplayCells_w, 2, 0, 0, H_XDisplayCells);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDisplayHeight" XM_POSTFIX, gxm_XDisplayHeight_w, 2, 0, 0, H_XDisplayHeight);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDisplayHeightMM" XM_POSTFIX, gxm_XDisplayHeightMM_w, 2, 0, 0, H_XDisplayHeightMM);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDisplayKeycodes" XM_POSTFIX, gxm_XDisplayKeycodes_w, 1, 0, 0, H_XDisplayKeycodes);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDisplayPlanes" XM_POSTFIX, gxm_XDisplayPlanes_w, 2, 0, 0, H_XDisplayPlanes);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDisplayWidth" XM_POSTFIX, gxm_XDisplayWidth_w, 2, 0, 0, H_XDisplayWidth);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDisplayWidthMM" XM_POSTFIX, gxm_XDisplayWidthMM_w, 2, 0, 0, H_XDisplayWidthMM);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDrawArc" XM_POSTFIX, gxm_XDrawArc_w, 9, 0, 0, H_XDrawArc);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDrawArcs" XM_POSTFIX, gxm_XDrawArcs_w, 5, 0, 0, H_XDrawArcs);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDrawImageString" XM_POSTFIX, gxm_XDrawImageString_w, 7, 0, 0, H_XDrawImageString);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDrawLine" XM_POSTFIX, gxm_XDrawLine_w, 7, 0, 0, H_XDrawLine);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDrawLines" XM_POSTFIX, gxm_XDrawLines_w, 6, 0, 0, H_XDrawLines);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDrawLinesDirect" XM_POSTFIX, gxm_XDrawLinesDirect_w, 6, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "freeXPoints" XM_POSTFIX, gxm_FreeXPoints_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "moveXPoints" XM_POSTFIX, gxm_MoveXPoints_w, 4, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "vector->XPoints" XM_POSTFIX, gxm_Vector2XPoints_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDrawPoint" XM_POSTFIX, gxm_XDrawPoint_w, 5, 0, 0, H_XDrawPoint);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDrawPoints" XM_POSTFIX, gxm_XDrawPoints_w, 6, 0, 0, H_XDrawPoints);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDrawRectangle" XM_POSTFIX, gxm_XDrawRectangle_w, 7, 0, 0, H_XDrawRectangle);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDrawRectangles" XM_POSTFIX, gxm_XDrawRectangles_w, 5, 0, 0, H_XDrawRectangles);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDrawSegments" XM_POSTFIX, gxm_XDrawSegments_w, 5, 0, 0, H_XDrawSegments);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDrawString" XM_POSTFIX, gxm_XDrawString_w, 7, 0, 0, H_XDrawString);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDrawText" XM_POSTFIX, gxm_XDrawText_w, 7, 0, 0, H_XDrawText);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XEnableAccessControl" XM_POSTFIX, gxm_XEnableAccessControl_w, 1, 0, 0, H_XEnableAccessControl);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XEventsQueued" XM_POSTFIX, gxm_XEventsQueued_w, 2, 0, 0, H_XEventsQueued);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFetchName" XM_POSTFIX, gxm_XFetchName_w, 2, 0, 0, H_XFetchName);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFillArc" XM_POSTFIX, gxm_XFillArc_w, 9, 0, 0, H_XFillArc);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFillArcs" XM_POSTFIX, gxm_XFillArcs_w, 5, 0, 0, H_XFillArcs);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFillPolygon" XM_POSTFIX, gxm_XFillPolygon_w, 7, 0, 0, H_XFillPolygon);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFillRectangle" XM_POSTFIX, gxm_XFillRectangle_w, 7, 0, 0, H_XFillRectangle);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFillRectangles" XM_POSTFIX, gxm_XFillRectangles_w, 5, 0, 0, H_XFillRectangles);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFlush" XM_POSTFIX, gxm_XFlush_w, 1, 0, 0, H_XFlush);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XForceScreenSaver" XM_POSTFIX, gxm_XForceScreenSaver_w, 2, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFree" XM_POSTFIX, gxm_XFree_w, 1, 0, 0, H_XFree);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFreeColormap" XM_POSTFIX, gxm_XFreeColormap_w, 2, 0, 0, H_XFreeColormap);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFreeColors" XM_POSTFIX, gxm_XFreeColors_w, 5, 0, 0, H_XFreeColors);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFreeCursor" XM_POSTFIX, gxm_XFreeCursor_w, 2, 0, 0, H_XFreeCursor);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFreeExtensionList" XM_POSTFIX, gxm_XFreeExtensionList_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFreeFont" XM_POSTFIX, gxm_XFreeFont_w, 2, 0, 0, H_XFreeFont);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFreeFontInfo" XM_POSTFIX, gxm_XFreeFontInfo_w, 3, 0, 0, H_XFreeFontInfo);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFreeFontNames" XM_POSTFIX, gxm_XFreeFontNames_w, 1, 0, 0, H_XFreeFontNames);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFreeFontPath" XM_POSTFIX, gxm_XFreeFontPath_w, 1, 0, 0, H_XFreeFontPath);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFreeGC" XM_POSTFIX, gxm_XFreeGC_w, 2, 0, 0, H_XFreeGC);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFreeModifiermap" XM_POSTFIX, gxm_XFreeModifiermap_w, 1, 0, 0, H_XFreeModifiermap);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFreePixmap" XM_POSTFIX, gxm_XFreePixmap_w, 2, 0, 0, H_XFreePixmap);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGeometry" XM_POSTFIX, gxm_XGeometry_w, 0, 0, 1, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetErrorText" XM_POSTFIX, gxm_XGetErrorText_w, 4, 0, 0, H_XGetErrorText);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetFontProperty" XM_POSTFIX, gxm_XGetFontProperty_w, 2, 0, 0, H_XGetFontProperty);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetGCValues" XM_POSTFIX, gxm_XGetGCValues_w, 3, 0, 0, H_XGetGCValues);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGCValues" XM_POSTFIX, gxm_XGCValues_w, 0, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XEvent" XM_POSTFIX, gxm_XEvent_w, 0, 1, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetGeometry" XM_POSTFIX, gxm_XGetGeometry_w, 2, 0, 0, H_XGetGeometry);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetIconName" XM_POSTFIX, gxm_XGetIconName_w, 2, 0, 0, H_XGetIconName);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetInputFocus" XM_POSTFIX, gxm_XGetInputFocus_w, 1, 0, 0, H_XGetInputFocus);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetKeyboardControl" XM_POSTFIX, gxm_XGetKeyboardControl_w, 1, 0, 0, H_XGetKeyboardControl);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetPointerControl" XM_POSTFIX, gxm_XGetPointerControl_w, 1, 0, 0, H_XGetPointerControl);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetPointerMapping" XM_POSTFIX, gxm_XGetPointerMapping_w, 3, 0, 0, H_XGetPointerMapping);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetScreenSaver" XM_POSTFIX, gxm_XGetScreenSaver_w, 1, 0, 0, H_XGetScreenSaver);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetTransientForHint" XM_POSTFIX, gxm_XGetTransientForHint_w, 2, 0, 0, H_XGetTransientForHint);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetWindowProperty" XM_POSTFIX, gxm_XGetWindowProperty_w, 0, 0, 1, H_XGetWindowProperty);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetWindowAttributes" XM_POSTFIX, gxm_XGetWindowAttributes_w, 2, 0, 0, H_XGetWindowAttributes);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGrabButton" XM_POSTFIX, gxm_XGrabButton_w, 0, 0, 1, H_XGrabButton);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGrabKey" XM_POSTFIX, gxm_XGrabKey_w, 7, 0, 0, H_XGrabKey);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGrabKeyboard" XM_POSTFIX, gxm_XGrabKeyboard_w, 6, 0, 0, H_XGrabKeyboard);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGrabPointer" XM_POSTFIX, gxm_XGrabPointer_w, 9, 0, 0, H_XGrabPointer);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGrabServer" XM_POSTFIX, gxm_XGrabServer_w, 1, 0, 0, H_XGrabServer);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XHeightMMOfScreen" XM_POSTFIX, gxm_XHeightMMOfScreen_w, 1, 0, 0, H_HeightMMOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XHeightOfScreen" XM_POSTFIX, gxm_XHeightOfScreen_w, 1, 0, 0, H_HeightOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XIfEvent" XM_POSTFIX, gxm_XIfEvent_w, 3, 0, 0, H_XIfEvent);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XImageByteOrder" XM_POSTFIX, gxm_XImageByteOrder_w, 1, 0, 0, H_ImageByteOrder);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XInstallColormap" XM_POSTFIX, gxm_XInstallColormap_w, 2, 0, 0, H_XInstallColormap);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XKeysymToKeycode" XM_POSTFIX, gxm_XKeysymToKeycode_w, 2, 0, 0, H_XKeysymToKeycode);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XKillClient" XM_POSTFIX, gxm_XKillClient_w, 2, 0, 0, H_XKillClient);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XLookupColor" XM_POSTFIX, gxm_XLookupColor_w, 5, 0, 0, H_XLookupColor);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XLowerWindow" XM_POSTFIX, gxm_XLowerWindow_w, 2, 0, 0, H_XLowerWindow);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XMapRaised" XM_POSTFIX, gxm_XMapRaised_w, 2, 0, 0, H_XMapRaised);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XMapSubwindows" XM_POSTFIX, gxm_XMapSubwindows_w, 2, 0, 0, H_XMapSubwindows);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XMapWindow" XM_POSTFIX, gxm_XMapWindow_w, 2, 0, 0, H_XMapWindow);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XMaskEvent" XM_POSTFIX, gxm_XMaskEvent_w, 2, 0, 0, H_XMaskEvent);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XMaxCmapsOfScreen" XM_POSTFIX, gxm_XMaxCmapsOfScreen_w, 1, 0, 0, H_MaxCmapsOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XMinCmapsOfScreen" XM_POSTFIX, gxm_XMinCmapsOfScreen_w, 1, 0, 0, H_MinCmapsOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XMoveResizeWindow" XM_POSTFIX, gxm_XMoveResizeWindow_w, 6, 0, 0, H_XMoveResizeWindow);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XMoveWindow" XM_POSTFIX, gxm_XMoveWindow_w, 4, 0, 0, H_XMoveWindow);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XNextEvent" XM_POSTFIX, gxm_XNextEvent_w, 1, 0, 0, H_XNextEvent);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XNoOp" XM_POSTFIX, gxm_XNoOp_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XParseColor" XM_POSTFIX, gxm_XParseColor_w, 4, 0, 0, H_XParseColor);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XParseGeometry" XM_POSTFIX, gxm_XParseGeometry_w, 1, 0, 0, H_XParseGeometry);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XPeekEvent" XM_POSTFIX, gxm_XPeekEvent_w, 1, 0, 0, H_XPeekEvent);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XPeekIfEvent" XM_POSTFIX, gxm_XPeekIfEvent_w, 3, 0, 0, H_XPeekIfEvent);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XPending" XM_POSTFIX, gxm_XPending_w, 1, 0, 0, H_XPending);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XPlanesOfScreen" XM_POSTFIX, gxm_XPlanesOfScreen_w, 1, 0, 0, H_PlanesOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XProtocolRevision" XM_POSTFIX, gxm_XProtocolRevision_w, 1, 0, 0, H_ProtocolRevision);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XProtocolVersion" XM_POSTFIX, gxm_XProtocolVersion_w, 1, 0, 0, H_ProtocolVersion);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XPutBackEvent" XM_POSTFIX, gxm_XPutBackEvent_w, 2, 0, 0, H_XPutBackEvent);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XPutImage" XM_POSTFIX, gxm_XPutImage_w, 0, 0, 1, H_XPutImage);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XQLength" XM_POSTFIX, gxm_XQLength_w, 1, 0, 0, H_QLength);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XQueryBestCursor" XM_POSTFIX, gxm_XQueryBestCursor_w, 4, 0, 0, H_XQueryBestCursor);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XQueryBestSize" XM_POSTFIX, gxm_XQueryBestSize_w, 5, 0, 0, H_XQueryBestSize);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XQueryBestStipple" XM_POSTFIX, gxm_XQueryBestStipple_w, 4, 0, 0, H_XQueryBestStipple);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XQueryBestTile" XM_POSTFIX, gxm_XQueryBestTile_w, 4, 0, 0, H_XQueryBestTile);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XQueryColor" XM_POSTFIX, gxm_XQueryColor_w, 3, 0, 0, H_XQueryColor);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XQueryColors" XM_POSTFIX, gxm_XQueryColors_w, 4, 0, 0, H_XQueryColors);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XQueryExtension" XM_POSTFIX, gxm_XQueryExtension_w, 2, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XQueryKeymap" XM_POSTFIX, gxm_XQueryKeymap_w, 1, 0, 0, H_XQueryKeymap);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XQueryPointer" XM_POSTFIX, gxm_XQueryPointer_w, 2, 0, 0, H_XQueryPointer);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XQueryTextExtents" XM_POSTFIX, gxm_XQueryTextExtents_w, 3, 0, 0, H_XQueryTextExtents);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XQueryTree" XM_POSTFIX, gxm_XQueryTree_w, 2, 0, 0, H_XQueryTree);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XRaiseWindow" XM_POSTFIX, gxm_XRaiseWindow_w, 2, 0, 0, H_XRaiseWindow);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XReadBitmapFile" XM_POSTFIX, gxm_XReadBitmapFile_w, 3, 0, 0, H_XReadBitmapFile);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XReadBitmapFileData" XM_POSTFIX, gxm_XReadBitmapFileData_w, 1, 0, 0, H_XReadBitmapFileData);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XRebindKeysym" XM_POSTFIX, gxm_XRebindKeysym_w, 6, 0, 0, H_XRebindKeysym);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XRecolorCursor" XM_POSTFIX, gxm_XRecolorCursor_w, 4, 0, 0, H_XRecolorCursor);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XRefreshKeyboardMapping" XM_POSTFIX, gxm_XRefreshKeyboardMapping_w, 1, 0, 0, H_XRefreshKeyboardMapping);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XReparentWindow" XM_POSTFIX, gxm_XReparentWindow_w, 5, 0, 0, H_XReparentWindow);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XResetScreenSaver" XM_POSTFIX, gxm_XResetScreenSaver_w, 1, 0, 0, H_XResetScreenSaver);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XResizeWindow" XM_POSTFIX, gxm_XResizeWindow_w, 4, 0, 0, H_XResizeWindow);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XRestackWindows" XM_POSTFIX, gxm_XRestackWindows_w, 3, 0, 0, H_XRestackWindows);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XRotateBuffers" XM_POSTFIX, gxm_XRotateBuffers_w, 2, 0, 0, H_XRotateBuffers);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XRotateWindowProperties" XM_POSTFIX, gxm_XRotateWindowProperties_w, 5, 0, 0, H_XRotateWindowProperties);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XScreenCount" XM_POSTFIX, gxm_XScreenCount_w, 1, 0, 0, H_ScreenCount);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSelectInput" XM_POSTFIX, gxm_XSelectInput_w, 3, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSendEvent" XM_POSTFIX, gxm_XSendEvent_w, 5, 0, 0, H_XSendEvent);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetAccessControl" XM_POSTFIX, gxm_XSetAccessControl_w, 2, 0, 0, H_XSetAccessControl);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetArcMode" XM_POSTFIX, gxm_XSetArcMode_w, 3, 0, 0, H_XSetArcMode);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetBackground" XM_POSTFIX, gxm_XSetBackground_w, 3, 0, 0, H_XSetBackground);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetClipMask" XM_POSTFIX, gxm_XSetClipMask_w, 3, 0, 0, H_XSetClipMask);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetClipOrigin" XM_POSTFIX, gxm_XSetClipOrigin_w, 4, 0, 0, H_XSetClipOrigin);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetClipRectangles" XM_POSTFIX, gxm_XSetClipRectangles_w, 7, 0, 0, H_XSetClipRectangles);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetCloseDownMode" XM_POSTFIX, gxm_XSetCloseDownMode_w, 2, 0, 0, H_XSetCloseDownMode);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetCommand" XM_POSTFIX, gxm_XSetCommand_w, 4, 0, 0, H_XSetCommand);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetDashes" XM_POSTFIX, gxm_XSetDashes_w, 5, 0, 0, H_XSetDashes);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetFillRule" XM_POSTFIX, gxm_XSetFillRule_w, 3, 0, 0, H_XSetFillRule);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetFillStyle" XM_POSTFIX, gxm_XSetFillStyle_w, 3, 0, 0, H_XSetFillStyle);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetFont" XM_POSTFIX, gxm_XSetFont_w, 3, 0, 0, H_XSetFont);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetFontPath" XM_POSTFIX, gxm_XSetFontPath_w, 3, 0, 0, H_XSetFontPath);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetForeground" XM_POSTFIX, gxm_XSetForeground_w, 3, 0, 0, H_XSetForeground);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetFunction" XM_POSTFIX, gxm_XSetFunction_w, 3, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetGraphicsExposures" XM_POSTFIX, gxm_XSetGraphicsExposures_w, 3, 0, 0, H_XSetGraphicsExposures);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetIconName" XM_POSTFIX, gxm_XSetIconName_w, 3, 0, 0, H_XSetIconName);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetInputFocus" XM_POSTFIX, gxm_XSetInputFocus_w, 4, 0, 0, H_XSetInputFocus);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetLineAttributes" XM_POSTFIX, gxm_XSetLineAttributes_w, 6, 0, 0, H_XSetLineAttributes);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetModifierMapping" XM_POSTFIX, gxm_XSetModifierMapping_w, 2, 0, 0, H_XSetModifierMapping);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetPlaneMask" XM_POSTFIX, gxm_XSetPlaneMask_w, 3, 0, 0, H_XSetPlaneMask);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetPointerMapping" XM_POSTFIX, gxm_XSetPointerMapping_w, 2, 1, 0, H_XSetPointerMapping);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetScreenSaver" XM_POSTFIX, gxm_XSetScreenSaver_w, 5, 0, 0, H_XSetScreenSaver);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetSelectionOwner" XM_POSTFIX, gxm_XSetSelectionOwner_w, 4, 0, 0, H_XSetSelectionOwner);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetState" XM_POSTFIX, gxm_XSetState_w, 6, 0, 0, H_XSetState);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetStipple" XM_POSTFIX, gxm_XSetStipple_w, 3, 0, 0, H_XSetStipple);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetSubwindowMode" XM_POSTFIX, gxm_XSetSubwindowMode_w, 3, 0, 0, H_XSetSubwindowMode);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetTSOrigin" XM_POSTFIX, gxm_XSetTSOrigin_w, 4, 0, 0, H_XSetTSOrigin);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetTile" XM_POSTFIX, gxm_XSetTile_w, 3, 0, 0, H_XSetTile);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetWindowBackground" XM_POSTFIX, gxm_XSetWindowBackground_w, 3, 0, 0, H_XSetWindowBackground);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetWindowBackgroundPixmap" XM_POSTFIX, gxm_XSetWindowBackgroundPixmap_w, 3, 0, 0, H_XSetWindowBackgroundPixmap);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetWindowBorder" XM_POSTFIX, gxm_XSetWindowBorder_w, 3, 0, 0, H_XSetWindowBorder);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetWindowBorderPixmap" XM_POSTFIX, gxm_XSetWindowBorderPixmap_w, 3, 0, 0, H_XSetWindowBorderPixmap);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetWindowBorderWidth" XM_POSTFIX, gxm_XSetWindowBorderWidth_w, 3, 0, 0, H_XSetWindowBorderWidth);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetWindowColormap" XM_POSTFIX, gxm_XSetWindowColormap_w, 3, 0, 0, H_XSetWindowColormap);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XStoreBuffer" XM_POSTFIX, gxm_XStoreBuffer_w, 4, 0, 0, H_XStoreBuffer);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XStoreBytes" XM_POSTFIX, gxm_XStoreBytes_w, 3, 0, 0, H_XStoreBytes);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XStoreColor" XM_POSTFIX, gxm_XStoreColor_w, 3, 0, 0, H_XStoreColor);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XStoreColors" XM_POSTFIX, gxm_XStoreColors_w, 4, 0, 0, H_XStoreColors);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XStoreName" XM_POSTFIX, gxm_XStoreName_w, 3, 0, 0, H_XStoreName);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XStoreNamedColor" XM_POSTFIX, gxm_XStoreNamedColor_w, 5, 0, 0, H_XStoreNamedColor);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSync" XM_POSTFIX, gxm_XSync_w, 2, 0, 0, H_XSync);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XTextExtents" XM_POSTFIX, gxm_XTextExtents_w, 3, 0, 0, H_XTextExtents);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XTextWidth" XM_POSTFIX, gxm_XTextWidth_w, 3, 0, 0, H_XTextWidth);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XTranslateCoordinates" XM_POSTFIX, gxm_XTranslateCoordinates_w, 5, 0, 0, H_XTranslateCoordinates);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XUndefineCursor" XM_POSTFIX, gxm_XUndefineCursor_w, 2, 0, 0, H_XUndefineCursor);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XUngrabButton" XM_POSTFIX, gxm_XUngrabButton_w, 4, 0, 0, H_XUngrabButton);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XUngrabKey" XM_POSTFIX, gxm_XUngrabKey_w, 4, 0, 0, H_XUngrabKey);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XUngrabKeyboard" XM_POSTFIX, gxm_XUngrabKeyboard_w, 2, 0, 0, H_XUngrabKeyboard);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XUngrabPointer" XM_POSTFIX, gxm_XUngrabPointer_w, 2, 0, 0, H_XUngrabPointer);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XUngrabServer" XM_POSTFIX, gxm_XUngrabServer_w, 1, 0, 0, H_XUngrabServer);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XUninstallColormap" XM_POSTFIX, gxm_XUninstallColormap_w, 2, 0, 0, H_XUninstallColormap);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XUnloadFont" XM_POSTFIX, gxm_XUnloadFont_w, 2, 0, 0, H_XUnloadFont);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XUnmapSubwindows" XM_POSTFIX, gxm_XUnmapSubwindows_w, 2, 0, 0, H_XUnmapSubwindows);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XUnmapWindow" XM_POSTFIX, gxm_XUnmapWindow_w, 2, 0, 0, H_XUnmapWindow);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XVendorRelease" XM_POSTFIX, gxm_XVendorRelease_w, 1, 0, 0, H_VendorRelease);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XWarpPointer" XM_POSTFIX, gxm_XWarpPointer_w, 9, 0, 0, H_XWarpPointer);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XWidthMMOfScreen" XM_POSTFIX, gxm_XWidthMMOfScreen_w, 1, 0, 0, H_WidthMMOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XWidthOfScreen" XM_POSTFIX, gxm_XWidthOfScreen_w, 1, 0, 0, H_WidthOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XWindowEvent" XM_POSTFIX, gxm_XWindowEvent_w, 3, 0, 0, H_XWindowEvent);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XWriteBitmapFile" XM_POSTFIX, gxm_XWriteBitmapFile_w, 7, 0, 0, H_XWriteBitmapFile);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSupportsLocale" XM_POSTFIX, gxm_XSupportsLocale_w, 0, 0, 0, H_XSupportsLocale);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetLocaleModifiers" XM_POSTFIX, gxm_XSetLocaleModifiers_w, 1, 0, 0, H_XSetLocaleModifiers);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCreateFontSet" XM_POSTFIX, gxm_XCreateFontSet_w, 2, 0, 0, H_XCreateFontSet);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFreeFontSet" XM_POSTFIX, gxm_XFreeFontSet_w, 2, 0, 0, H_XFreeFontSet);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFontsOfFontSet" XM_POSTFIX, gxm_XFontsOfFontSet_w, 1, 0, 0, H_XFontsOfFontSet);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XBaseFontNameListOfFontSet" XM_POSTFIX, gxm_XBaseFontNameListOfFontSet_w, 1, 0, 0, H_XBaseFontNameListOfFontSet);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XLocaleOfFontSet" XM_POSTFIX, gxm_XLocaleOfFontSet_w, 1, 0, 0, H_XLocaleOfFontSet);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XContextDependentDrawing" XM_POSTFIX, gxm_XContextDependentDrawing_w, 1, 0, 0, H_XContextDependentDrawing);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDirectionalDependentDrawing" XM_POSTFIX, gxm_XDirectionalDependentDrawing_w, 1, 0, 0, H_XDirectionalDependentDrawing);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XContextualDrawing" XM_POSTFIX, gxm_XContextualDrawing_w, 1, 0, 0, H_XContextualDrawing);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFilterEvent" XM_POSTFIX, gxm_XFilterEvent_w, 2, 0, 0, H_XFilterEvent);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XAllocIconSize" XM_POSTFIX, gxm_XAllocIconSize_w, 0, 0, 0, H_XAllocIconSize);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XAllocStandardColormap" XM_POSTFIX, gxm_XAllocStandardColormap_w, 0, 0, 0, H_XAllocStandardColormap);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XAllocWMHints" XM_POSTFIX, gxm_XAllocWMHints_w, 0, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XClipBox" XM_POSTFIX, gxm_XClipBox_w, 1, 0, 0, H_XClipBox);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCreateRegion" XM_POSTFIX, gxm_XCreateRegion_w, 0, 0, 0, H_XCreateRegion);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDefaultString" XM_POSTFIX, gxm_XDefaultString_w, 0, 0, 0, H_XDefaultString);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDeleteContext" XM_POSTFIX, gxm_XDeleteContext_w, 3, 0, 0, H_XDeleteContext);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDestroyRegion" XM_POSTFIX, gxm_XDestroyRegion_w, 1, 0, 0, H_XDestroyRegion);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XEmptyRegion" XM_POSTFIX, gxm_XEmptyRegion_w, 1, 0, 0, H_XEmptyRegion);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XEqualRegion" XM_POSTFIX, gxm_XEqualRegion_w, 2, 0, 0, H_XEqualRegion);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFindContext" XM_POSTFIX, gxm_XFindContext_w, 3, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetIconSizes" XM_POSTFIX, gxm_XGetIconSizes_w, 2, 0, 0, H_XGetIconSizes);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetRGBColormaps" XM_POSTFIX, gxm_XGetRGBColormaps_w, 3, 0, 0, H_XGetRGBColormaps);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetStandardColormap" XM_POSTFIX, gxm_XGetStandardColormap_w, 3, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetVisualInfo" XM_POSTFIX, gxm_XGetVisualInfo_w, 3, 0, 0, H_XGetVisualInfo);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetWMHints" XM_POSTFIX, gxm_XGetWMHints_w, 2, 0, 0, H_XGetWMHints);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XIntersectRegion" XM_POSTFIX, gxm_XIntersectRegion_w, 3, 0, 0, H_XIntersectRegion);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XConvertCase" XM_POSTFIX, gxm_XConvertCase_w, 1, 0, 0, H_XConvertCase);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XLookupString" XM_POSTFIX, gxm_XLookupString_w, 1, 0, 0, H_XLookupString);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XMatchVisualInfo" XM_POSTFIX, gxm_XMatchVisualInfo_w, 4, 0, 0, H_XMatchVisualInfo);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XOffsetRegion" XM_POSTFIX, gxm_XOffsetRegion_w, 3, 0, 0, H_XOffsetRegion);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XPointInRegion" XM_POSTFIX, gxm_XPointInRegion_w, 3, 0, 0, H_XPointInRegion);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XPolygonRegion" XM_POSTFIX, gxm_XPolygonRegion_w, 3, 0, 0, H_XPolygonRegion);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XRectInRegion" XM_POSTFIX, gxm_XRectInRegion_w, 5, 0, 0, H_XRectInRegion);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSaveContext" XM_POSTFIX, gxm_XSaveContext_w, 4, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetRGBColormaps" XM_POSTFIX, gxm_XSetRGBColormaps_w, 5, 0, 0, H_XSetRGBColormaps);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetWMHints" XM_POSTFIX, gxm_XSetWMHints_w, 3, 0, 0, H_XSetWMHints);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetRegion" XM_POSTFIX, gxm_XSetRegion_w, 3, 0, 0, H_XSetRegion);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSetStandardColormap" XM_POSTFIX, gxm_XSetStandardColormap_w, 4, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XShrinkRegion" XM_POSTFIX, gxm_XShrinkRegion_w, 3, 0, 0, H_XShrinkRegion);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSubtractRegion" XM_POSTFIX, gxm_XSubtractRegion_w, 3, 0, 0, H_XSubtractRegion);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XUnionRectWithRegion" XM_POSTFIX, gxm_XUnionRectWithRegion_w, 3, 0, 0, H_XUnionRectWithRegion);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XUnionRegion" XM_POSTFIX, gxm_XUnionRegion_w, 3, 0, 0, H_XUnionRegion);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XXorRegion" XM_POSTFIX, gxm_XXorRegion_w, 3, 0, 0, H_XXorRegion);

  XEN_DEFINE_PROCEDURE(XM_PREFIX "DefaultScreen" XM_POSTFIX, gxm_DefaultScreen_w, 1, 0, 0, H_DefaultScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "DefaultRootWindow" XM_POSTFIX, gxm_DefaultRootWindow_w, 1, 0, 0, H_DefaultRootWindow);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "QLength" XM_POSTFIX, gxm_QLength_w, 1, 0, 0, H_QLength);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "ScreenCount" XM_POSTFIX, gxm_ScreenCount_w, 1, 0, 0, H_ScreenCount);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "ServerVendor" XM_POSTFIX, gxm_ServerVendor_w, 1, 0, 0, H_ServerVendor);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "ProtocolVersion" XM_POSTFIX, gxm_ProtocolVersion_w, 1, 0, 0, H_ProtocolVersion);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "ProtocolRevision" XM_POSTFIX, gxm_ProtocolRevision_w, 1, 0, 0, H_ProtocolRevision);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "VendorRelease" XM_POSTFIX, gxm_VendorRelease_w, 1, 0, 0, H_VendorRelease);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "DisplayString" XM_POSTFIX, gxm_DisplayString_w, 1, 0, 0, H_DisplayString);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "BitmapUnit" XM_POSTFIX, gxm_BitmapUnit_w, 1, 0, 0, H_BitmapUnit);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "BitmapBitOrder" XM_POSTFIX, gxm_BitmapBitOrder_w, 1, 0, 0, H_BitmapBitOrder);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "BitmapPad" XM_POSTFIX, gxm_BitmapPad_w, 1, 0, 0, H_BitmapPad);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "ImageByteOrder" XM_POSTFIX, gxm_ImageByteOrder_w, 1, 0, 0, H_ImageByteOrder);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "NextRequest" XM_POSTFIX, gxm_NextRequest_w, 1, 0, 0, H_NextRequest);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "LastKnownRequestProcessed" XM_POSTFIX, gxm_LastKnownRequestProcessed_w, 1, 0, 0, H_LastKnownRequestProcessed);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "DefaultScreenOfDisplay" XM_POSTFIX, gxm_DefaultScreenOfDisplay_w, 1, 0, 0, H_DefaultScreenOfDisplay);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "DisplayOfScreen" XM_POSTFIX, gxm_DisplayOfScreen_w, 1, 0, 0, H_DisplayOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "RootWindowOfScreen" XM_POSTFIX, gxm_RootWindowOfScreen_w, 1, 0, 0, H_RootWindowOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "BlackPixelOfScreen" XM_POSTFIX, gxm_BlackPixelOfScreen_w, 1, 0, 0, H_BlackPixelOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "WhitePixelOfScreen" XM_POSTFIX, gxm_WhitePixelOfScreen_w, 1, 0, 0, H_WhitePixelOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "DefaultColormapOfScreen" XM_POSTFIX, gxm_DefaultColormapOfScreen_w, 1, 0, 0, H_DefaultColormapOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "DefaultDepthOfScreen" XM_POSTFIX, gxm_DefaultDepthOfScreen_w, 1, 0, 0, H_DefaultDepthOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "DefaultGCOfScreen" XM_POSTFIX, gxm_DefaultGCOfScreen_w, 1, 0, 0, H_DefaultGCOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "DefaultVisualOfScreen" XM_POSTFIX, gxm_DefaultVisualOfScreen_w, 1, 0, 0, H_DefaultVisualOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "WidthOfScreen" XM_POSTFIX, gxm_WidthOfScreen_w, 1, 0, 0, H_WidthOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "HeightOfScreen" XM_POSTFIX, gxm_HeightOfScreen_w, 1, 0, 0, H_HeightOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "WidthMMOfScreen" XM_POSTFIX, gxm_WidthMMOfScreen_w, 1, 0, 0, H_WidthMMOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "HeightMMOfScreen" XM_POSTFIX, gxm_HeightMMOfScreen_w, 1, 0, 0, H_HeightMMOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "PlanesOfScreen" XM_POSTFIX, gxm_PlanesOfScreen_w, 1, 0, 0, H_PlanesOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "CellsOfScreen" XM_POSTFIX, gxm_CellsOfScreen_w, 1, 0, 0, H_CellsOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "MinCmapsOfScreen" XM_POSTFIX, gxm_MinCmapsOfScreen_w, 1, 0, 0, H_MinCmapsOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "MaxCmapsOfScreen" XM_POSTFIX, gxm_MaxCmapsOfScreen_w, 1, 0, 0, H_MaxCmapsOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "DoesSaveUnders" XM_POSTFIX, gxm_DoesSaveUnders_w, 1, 0, 0, H_DoesSaveUnders);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "DoesBackingStore" XM_POSTFIX, gxm_DoesBackingStore_w, 1, 0, 0, H_DoesBackingStore);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "EventMaskOfScreen" XM_POSTFIX, gxm_EventMaskOfScreen_w, 1, 0, 0, H_EventMaskOfScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "RootWindow" XM_POSTFIX, gxm_RootWindow_w, 2, 0, 0, H_RootWindow);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "DefaultVisual" XM_POSTFIX, gxm_DefaultVisual_w, 2, 0, 0, H_DefaultVisual);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "DefaultGC" XM_POSTFIX, gxm_DefaultGC_w, 2, 0, 0, H_DefaultGC);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "BlackPixel" XM_POSTFIX, gxm_BlackPixel_w, 2, 0, 0, H_BlackPixel);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "WhitePixel" XM_POSTFIX, gxm_WhitePixel_w, 2, 0, 0, H_WhitePixel);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "DisplayWidth" XM_POSTFIX, gxm_DisplayWidth_w, 2, 0, 0, H_DisplayWidth);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "DisplayHeight" XM_POSTFIX, gxm_DisplayHeight_w, 2, 0, 0, H_DisplayHeight);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "DisplayWidthMM" XM_POSTFIX, gxm_DisplayWidthMM_w, 2, 0, 0, H_DisplayWidthMM);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "DisplayHeightMM" XM_POSTFIX, gxm_DisplayHeightMM_w, 2, 0, 0, H_DisplayHeightMM);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "DisplayPlanes" XM_POSTFIX, gxm_DisplayPlanes_w, 2, 0, 0, H_DisplayPlanes);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "DisplayCells" XM_POSTFIX, gxm_DisplayCells_w, 2, 0, 0, H_DisplayCells);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "DefaultColormap" XM_POSTFIX, gxm_DefaultColormap_w, 2, 0, 0, H_DefaultColormap);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "ScreenOfDisplay" XM_POSTFIX, gxm_ScreenOfDisplay_w, 2, 0, 0, H_ScreenOfDisplay);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "DefaultDepth" XM_POSTFIX, gxm_DefaultDepth_w, 2, 0, 0, H_DefaultDepth);

  XEN_DEFINE_PROCEDURE(XM_PREFIX "IsKeypadKey" XM_POSTFIX, gxm_IsKeypadKey_w, 1, 0, 0, H_IsKeypadKey);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "IsPrivateKeypadKey" XM_POSTFIX, gxm_IsPrivateKeypadKey_w, 1, 0, 0, H_IsPrivateKeypadKey);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "IsCursorKey" XM_POSTFIX, gxm_IsCursorKey_w, 1, 0, 0, H_IsCursorKey);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "IsPFKey" XM_POSTFIX, gxm_IsPFKey_w, 1, 0, 0, H_IsPFKey);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "IsFunctionKey" XM_POSTFIX, gxm_IsFunctionKey_w, 1, 0, 0, H_IsFunctionKey);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "IsMiscFunctionKey" XM_POSTFIX, gxm_IsMiscFunctionKey_w, 1, 0, 0, H_IsMiscFunctionKey);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "IsModifierKey" XM_POSTFIX, gxm_IsModifierKey_w, 1, 0, 0, H_IsModifierKey);

#if HAVE_MOTIF
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateMessageBox" XM_POSTFIX, gxm_XmCreateMessageBox_w, 3, 1, 0, H_XmCreateMessageBox);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateMessageDialog" XM_POSTFIX, gxm_XmCreateMessageDialog_w, 3, 1, 0, H_XmCreateMessageDialog);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateErrorDialog" XM_POSTFIX, gxm_XmCreateErrorDialog_w, 3, 1, 0, H_XmCreateErrorDialog);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateInformationDialog" XM_POSTFIX, gxm_XmCreateInformationDialog_w, 3, 1, 0, H_XmCreateInformationDialog);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateQuestionDialog" XM_POSTFIX, gxm_XmCreateQuestionDialog_w, 3, 1, 0, H_XmCreateQuestionDialog);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateWarningDialog" XM_POSTFIX, gxm_XmCreateWarningDialog_w, 3, 1, 0, H_XmCreateWarningDialog);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateWorkingDialog" XM_POSTFIX, gxm_XmCreateWorkingDialog_w, 3, 1, 0, H_XmCreateWorkingDialog);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateTemplateDialog" XM_POSTFIX, gxm_XmCreateTemplateDialog_w, 3, 1, 0, H_XmCreateTemplateDialog);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmMessageBoxGetChild" XM_POSTFIX, gxm_XmMessageBoxGetChild_w, 2, 0, 0, H_XmMessageBoxGetChild);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateArrowButtonGadget" XM_POSTFIX, gxm_XmCreateArrowButtonGadget_w, 3, 1, 0, H_XmCreateArrowButtonGadget);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateArrowButton" XM_POSTFIX, gxm_XmCreateArrowButton_w, 3, 1, 0, H_XmCreateArrowButton);
#if MOTIF_2
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateNotebook" XM_POSTFIX, gxm_XmCreateNotebook_w, 3, 1, 0, H_XmCreateNotebook);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmNotebookGetPageInfo" XM_POSTFIX, gxm_XmNotebookGetPageInfo_w, 2, 0, 0, H_XmNotebookGetPageInfo);
#if HAVE_XP
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmPrintSetup" XM_POSTFIX, gxm_XmPrintSetup_w, 4, 1, 0, H_XmPrintSetup);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmPrintToFile" XM_POSTFIX, gxm_XmPrintToFile_w, 4, 0, 0, H_XmPrintToFile);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmPrintPopupPDM" XM_POSTFIX, gxm_XmPrintPopupPDM_w, 2, 0, 0, H_XmPrintPopupPDM);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmRedisplayWidget" XM_POSTFIX, gxm_XmRedisplayWidget_w, 1, 0, 0, H_XmRedisplayWidget);
#endif
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTransferSetParameters" XM_POSTFIX, gxm_XmTransferSetParameters_w, 5, 0, 0, H_XmTransferSetParameters);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTransferDone" XM_POSTFIX, gxm_XmTransferDone_w, 2, 0, 0, H_XmTransferDone);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTransferValue" XM_POSTFIX, gxm_XmTransferValue_w, 5, 0, 0, H_XmTransferValue);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTransferStartRequest" XM_POSTFIX, gxm_XmTransferStartRequest_w, 1, 0, 0, H_XmTransferStartRequest);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTransferSendRequest" XM_POSTFIX, gxm_XmTransferSendRequest_w, 2, 0, 0, H_XmTransferSendRequest);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateComboBox" XM_POSTFIX, gxm_XmCreateComboBox_w, 3, 1, 0, H_XmCreateComboBox);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateDropDownComboBox" XM_POSTFIX, gxm_XmCreateDropDownComboBox_w, 3, 1, 0, H_XmCreateDropDownComboBox);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateDropDownList" XM_POSTFIX, gxm_XmCreateDropDownList_w, 3, 1, 0, H_XmCreateDropDownList);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmComboBoxAddItem" XM_POSTFIX, gxm_XmComboBoxAddItem_w, 4, 0, 0, H_XmComboBoxAddItem);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmComboBoxDeletePos" XM_POSTFIX, gxm_XmComboBoxDeletePos_w, 2, 0, 0, H_XmComboBoxDeletePos);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmComboBoxSelectItem" XM_POSTFIX, gxm_XmComboBoxSelectItem_w, 2, 0, 0, H_XmComboBoxSelectItem);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmComboBoxSetItem" XM_POSTFIX, gxm_XmComboBoxSetItem_w, 2, 0, 0, H_XmComboBoxSetItem);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmComboBoxUpdate" XM_POSTFIX, gxm_XmComboBoxUpdate_w, 1, 0, 0, H_XmComboBoxUpdate);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateContainer" XM_POSTFIX, gxm_XmCreateContainer_w, 3, 1, 0, H_XmCreateContainer);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmContainerGetItemChildren" XM_POSTFIX, gxm_XmContainerGetItemChildren_w, 2, 0, 0, H_XmContainerGetItemChildren);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmContainerRelayout" XM_POSTFIX, gxm_XmContainerRelayout_w, 1, 0, 0, H_XmContainerRelayout);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmContainerReorder" XM_POSTFIX, gxm_XmContainerReorder_w, 3, 0, 0, H_XmContainerReorder);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmContainerCut" XM_POSTFIX, gxm_XmContainerCut_w, 2, 0, 0, H_XmContainerCut);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmContainerCopy" XM_POSTFIX, gxm_XmContainerCopy_w, 2, 0, 0, H_XmContainerCopy);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmContainerPaste" XM_POSTFIX, gxm_XmContainerPaste_w, 1, 0, 0, H_XmContainerPaste);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmContainerCopyLink" XM_POSTFIX, gxm_XmContainerCopyLink_w, 2, 0, 0, H_XmContainerCopyLink);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmContainerPasteLink" XM_POSTFIX, gxm_XmContainerPasteLink_w, 1, 0, 0, H_XmContainerPasteLink);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateSpinBox" XM_POSTFIX, gxm_XmCreateSpinBox_w, 3, 1, 0, H_XmCreateSpinBox);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmSpinBoxValidatePosition" XM_POSTFIX, gxm_XmSpinBoxValidatePosition_w, 1, 0, 0, H_XmSpinBoxValidatePosition);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateSimpleSpinBox" XM_POSTFIX, gxm_XmCreateSimpleSpinBox_w, 3, 1, 0, H_XmCreateSimpleSpinBox);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmSimpleSpinBoxAddItem" XM_POSTFIX, gxm_XmSimpleSpinBoxAddItem_w, 3, 0, 0, H_XmSimpleSpinBoxAddItem);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmSimpleSpinBoxDeletePos" XM_POSTFIX, gxm_XmSimpleSpinBoxDeletePos_w, 2, 0, 0, H_XmSimpleSpinBoxDeletePos);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmSimpleSpinBoxSetItem" XM_POSTFIX, gxm_XmSimpleSpinBoxSetItem_w, 2, 0, 0, H_XmSimpleSpinBoxSetItem);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmDropSiteRegistered" XM_POSTFIX, gxm_XmDropSiteRegistered_w, 1, 0, 0, H_XmDropSiteRegistered);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldCopyLink" XM_POSTFIX, gxm_XmTextFieldCopyLink_w, 2, 0, 0, H_XmTextFieldCopyLink);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldPasteLink" XM_POSTFIX, gxm_XmTextFieldPasteLink_w, 1, 0, 0, H_XmTextFieldPasteLink);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextGetCenterline" XM_POSTFIX, gxm_XmTextGetCenterline_w, 1, 0, 0, H_XmTextGetCenterline);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmToggleButtonGadgetSetValue" XM_POSTFIX, gxm_XmToggleButtonGadgetSetValue_w, 3, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateIconGadget" XM_POSTFIX, gxm_XmCreateIconGadget_w, 3, 1, 0, H_XmCreateIconGadget);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateIconHeader" XM_POSTFIX, gxm_XmCreateIconHeader_w, 3, 1, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmObjectAtPoint" XM_POSTFIX, gxm_XmObjectAtPoint_w, 3, 0, 0, H_XmObjectAtPoint);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmConvertStringToUnits" XM_POSTFIX, gxm_XmConvertStringToUnits_w, 4, 0, 0, H_XmConvertStringToUnits);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateGrabShell" XM_POSTFIX, gxm_XmCreateGrabShell_w, 3, 1, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmToggleButtonSetValue" XM_POSTFIX, gxm_XmToggleButtonSetValue_w, 3, 0, 0, H_XmToggleButtonSetValue);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextPasteLink" XM_POSTFIX, gxm_XmTextPasteLink_w, 1, 0, 0, H_XmTextPasteLink);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextCopyLink" XM_POSTFIX, gxm_XmTextCopyLink_w, 2, 0, 0, H_XmTextCopyLink);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmScaleSetTicks" XM_POSTFIX, gxm_XmScaleSetTicks_w, 7, 0, 0, H_XmScaleSetTicks);
#endif
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmInternAtom" XM_POSTFIX, gxm_XmInternAtom_w, 3, 0, 0, H_XmInternAtom);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmGetAtomName" XM_POSTFIX, gxm_XmGetAtomName_w, 2, 0, 0, H_XmGetAtomName);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreatePanedWindow" XM_POSTFIX, gxm_XmCreatePanedWindow_w, 3, 1, 0, H_XmCreatePanedWindow);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateBulletinBoard" XM_POSTFIX, gxm_XmCreateBulletinBoard_w, 3, 1, 0, H_XmCreateBulletinBoard);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateBulletinBoardDialog" XM_POSTFIX, gxm_XmCreateBulletinBoardDialog_w, 3, 1, 0, H_XmCreateBulletinBoardDialog);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateCascadeButtonGadget" XM_POSTFIX, gxm_XmCreateCascadeButtonGadget_w, 3, 1, 0, H_XmCreateCascadeButtonGadget);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCascadeButtonGadgetHighlight" XM_POSTFIX, gxm_XmCascadeButtonGadgetHighlight_w, 2, 0, 0, H_XmCascadeButtonGadgetHighlight);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmAddProtocols" XM_POSTFIX, gxm_XmAddProtocols_w, 4, 0, 0, H_XmAddProtocols);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmRemoveProtocols" XM_POSTFIX, gxm_XmRemoveProtocols_w, 4, 0, 0, H_XmRemoveProtocols);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmAddProtocolCallback" XM_POSTFIX, gxm_XmAddProtocolCallback_w, 5, 0, 0, H_XmAddProtocolCallback);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmRemoveProtocolCallback" XM_POSTFIX, gxm_XmRemoveProtocolCallback_w, 5, 0, 0, H_XmRemoveProtocolCallback);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmActivateProtocol" XM_POSTFIX, gxm_XmActivateProtocol_w, 3, 0, 0, H_XmActivateProtocol);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmDeactivateProtocol" XM_POSTFIX, gxm_XmDeactivateProtocol_w, 3, 0, 0, H_XmDeactivateProtocol);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmSetProtocolHooks" XM_POSTFIX, gxm_XmSetProtocolHooks_w, 7, 0, 0, H_XmSetProtocolHooks);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateCascadeButton" XM_POSTFIX, gxm_XmCreateCascadeButton_w, 3, 1, 0, H_XmCreateCascadeButton);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCascadeButtonHighlight" XM_POSTFIX, gxm_XmCascadeButtonHighlight_w, 2, 0, 0, H_XmCascadeButtonHighlight);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreatePushButtonGadget" XM_POSTFIX, gxm_XmCreatePushButtonGadget_w, 3, 1, 0, H_XmCreatePushButtonGadget);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreatePushButton" XM_POSTFIX, gxm_XmCreatePushButton_w, 3, 1, 0, H_XmCreatePushButton);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateCommand" XM_POSTFIX, gxm_XmCreateCommand_w, 3, 1, 0, H_XmCreateCommand);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCommandGetChild" XM_POSTFIX, gxm_XmCommandGetChild_w, 2, 0, 0, H_XmCommandGetChild);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCommandSetValue" XM_POSTFIX, gxm_XmCommandSetValue_w, 2, 0, 0, H_XmCommandSetValue);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCommandAppendValue" XM_POSTFIX, gxm_XmCommandAppendValue_w, 2, 0, 0, H_XmCommandAppendValue);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCommandError" XM_POSTFIX, gxm_XmCommandError_w, 2, 0, 0, H_XmCommandError);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateCommandDialog" XM_POSTFIX, gxm_XmCreateCommandDialog_w, 3, 1, 0, H_XmCreateCommandDialog);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmMenuPosition" XM_POSTFIX, gxm_XmMenuPosition_w, 2, 0, 0, H_XmMenuPosition);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateRowColumn" XM_POSTFIX, gxm_XmCreateRowColumn_w, 3, 1, 0, H_XmCreateRowColumn);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateWorkArea" XM_POSTFIX, gxm_XmCreateWorkArea_w, 3, 1, 0, H_XmCreateWorkArea);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateRadioBox" XM_POSTFIX, gxm_XmCreateRadioBox_w, 3, 1, 0, H_XmCreateRadioBox);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateOptionMenu" XM_POSTFIX, gxm_XmCreateOptionMenu_w, 3, 1, 0, H_XmCreateOptionMenu);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmOptionLabelGadget" XM_POSTFIX, gxm_XmOptionLabelGadget_w, 1, 0, 0, H_XmOptionLabelGadget);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmOptionButtonGadget" XM_POSTFIX, gxm_XmOptionButtonGadget_w, 1, 0, 0, H_XmOptionButtonGadget);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateMenuBar" XM_POSTFIX, gxm_XmCreateMenuBar_w, 3, 1, 0, H_XmCreateMenuBar);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreatePopupMenu" XM_POSTFIX, gxm_XmCreatePopupMenu_w, 3, 1, 0, H_XmCreatePopupMenu);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreatePulldownMenu" XM_POSTFIX, gxm_XmCreatePulldownMenu_w, 3, 1, 0, H_XmCreatePulldownMenu);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmGetPostedFromWidget" XM_POSTFIX, gxm_XmGetPostedFromWidget_w, 1, 0, 0, H_XmGetPostedFromWidget);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmGetTearOffControl" XM_POSTFIX, gxm_XmGetTearOffControl_w, 1, 0, 0, H_XmGetTearOffControl);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmAddToPostFromList" XM_POSTFIX, gxm_XmAddToPostFromList_w, 2, 0, 0, H_XmAddToPostFromList);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmRemoveFromPostFromList" XM_POSTFIX, gxm_XmRemoveFromPostFromList_w, 2, 0, 0, H_XmRemoveFromPostFromList);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmScaleSetValue" XM_POSTFIX, gxm_XmScaleSetValue_w, 2, 0, 0, H_XmScaleSetValue);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmScaleGetValue" XM_POSTFIX, gxm_XmScaleGetValue_w, 1, 0, 0, H_XmScaleGetValue);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateScale" XM_POSTFIX, gxm_XmCreateScale_w, 3, 1, 0, H_XmCreateScale);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmClipboardStartCopy" XM_POSTFIX, gxm_XmClipboardStartCopy_w, 6, 0, 0, H_XmClipboardStartCopy);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmClipboardCopy" XM_POSTFIX, gxm_XmClipboardCopy_w, 7, 0, 0, H_XmClipboardCopy);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmClipboardEndCopy" XM_POSTFIX, gxm_XmClipboardEndCopy_w, 3, 0, 0, H_XmClipboardEndCopy);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmClipboardCancelCopy" XM_POSTFIX, gxm_XmClipboardCancelCopy_w, 3, 0, 0, H_XmClipboardCancelCopy);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmClipboardWithdrawFormat" XM_POSTFIX, gxm_XmClipboardWithdrawFormat_w, 3, 0, 0, H_XmClipboardWithdrawFormat);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmClipboardCopyByName" XM_POSTFIX, gxm_XmClipboardCopyByName_w, 6, 0, 0, H_XmClipboardCopyByName);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmClipboardUndoCopy" XM_POSTFIX, gxm_XmClipboardUndoCopy_w, 2, 0, 0, H_XmClipboardUndoCopy);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmClipboardLock" XM_POSTFIX, gxm_XmClipboardLock_w, 2, 0, 0, H_XmClipboardLock);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmClipboardUnlock" XM_POSTFIX, gxm_XmClipboardUnlock_w, 3, 0, 0, H_XmClipboardUnlock);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmClipboardStartRetrieve" XM_POSTFIX, gxm_XmClipboardStartRetrieve_w, 3, 0, 0, H_XmClipboardStartRetrieve);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmClipboardEndRetrieve" XM_POSTFIX, gxm_XmClipboardEndRetrieve_w, 2, 0, 0, H_XmClipboardEndRetrieve);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmClipboardRetrieve" XM_POSTFIX, gxm_XmClipboardRetrieve_w, 4, 0, 0, H_XmClipboardRetrieve);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmClipboardInquireCount" XM_POSTFIX, gxm_XmClipboardInquireCount_w, 2, 0, 0, H_XmClipboardInquireCount);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmClipboardInquireFormat" XM_POSTFIX, gxm_XmClipboardInquireFormat_w, 4, 0, 0, H_XmClipboardInquireFormat);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmClipboardInquireLength" XM_POSTFIX, gxm_XmClipboardInquireLength_w, 3, 0, 0, H_XmClipboardInquireLength);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmClipboardInquirePendingItems" XM_POSTFIX, gxm_XmClipboardInquirePendingItems_w, 3, 0, 0, H_XmClipboardInquirePendingItems);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmClipboardRegisterFormat" XM_POSTFIX, gxm_XmClipboardRegisterFormat_w, 3, 0, 0, H_XmClipboardRegisterFormat);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmGetXmScreen" XM_POSTFIX, gxm_XmGetXmScreen_w, 1, 0, 0, H_XmGetXmScreen);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateScrollBar" XM_POSTFIX, gxm_XmCreateScrollBar_w, 3, 1, 0, H_XmCreateScrollBar);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmScrollBarGetValues" XM_POSTFIX, gxm_XmScrollBarGetValues_w, 1, 0, 0, H_XmScrollBarGetValues);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmScrollBarSetValues" XM_POSTFIX, gxm_XmScrollBarSetValues_w, 6, 0, 0, H_XmScrollBarSetValues);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateDialogShell" XM_POSTFIX, gxm_XmCreateDialogShell_w, 3, 1, 0, H_XmCreateDialogShell);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateScrolledWindow" XM_POSTFIX, gxm_XmCreateScrolledWindow_w, 3, 1, 0, H_XmCreateScrolledWindow);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmScrollVisible" XM_POSTFIX, gxm_XmScrollVisible_w, 4, 0, 0, H_XmScrollVisible);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmGetDragContext" XM_POSTFIX, gxm_XmGetDragContext_w, 2, 0, 0, H_XmGetDragContext);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmGetXmDisplay" XM_POSTFIX, gxm_XmGetXmDisplay_w, 1, 0, 0, H_XmGetXmDisplay);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmSelectionBoxGetChild" XM_POSTFIX, gxm_XmSelectionBoxGetChild_w, 2, 0, 0, H_XmSelectionBoxGetChild);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateSelectionBox" XM_POSTFIX, gxm_XmCreateSelectionBox_w, 3, 1, 0, H_XmCreateSelectionBox);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateSelectionDialog" XM_POSTFIX, gxm_XmCreateSelectionDialog_w, 3, 1, 0, H_XmCreateSelectionDialog);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreatePromptDialog" XM_POSTFIX, gxm_XmCreatePromptDialog_w, 3, 1, 0, H_XmCreatePromptDialog);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmDragStart" XM_POSTFIX, gxm_XmDragStart_w, 3, 1, 0, H_XmDragStart);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmDragCancel" XM_POSTFIX, gxm_XmDragCancel_w, 1, 0, 0, H_XmDragCancel);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTargetsAreCompatible" XM_POSTFIX, gxm_XmTargetsAreCompatible_w, 5, 0, 0, H_XmTargetsAreCompatible);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateSeparatorGadget" XM_POSTFIX, gxm_XmCreateSeparatorGadget_w, 3, 1, 0, H_XmCreateSeparatorGadget);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateDragIcon" XM_POSTFIX, gxm_XmCreateDragIcon_w, 3, 1, 0, H_XmCreateDragIcon);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateSeparator" XM_POSTFIX, gxm_XmCreateSeparator_w, 3, 1, 0, H_XmCreateSeparator);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateDrawingArea" XM_POSTFIX, gxm_XmCreateDrawingArea_w, 3, 1, 0, H_XmCreateDrawingArea);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateDrawnButton" XM_POSTFIX, gxm_XmCreateDrawnButton_w, 3, 1, 0, H_XmCreateDrawnButton);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmDropSiteRegister" XM_POSTFIX, gxm_XmDropSiteRegister_w, 2, 1, 0, H_XmDropSiteRegister);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmDropSiteUnregister" XM_POSTFIX, gxm_XmDropSiteUnregister_w, 1, 0, 0, H_XmDropSiteUnregister);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmDropSiteStartUpdate" XM_POSTFIX, gxm_XmDropSiteStartUpdate_w, 1, 0, 0, H_XmDropSiteStartUpdate);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmDropSiteUpdate" XM_POSTFIX, gxm_XmDropSiteUpdate_w, 2, 1, 0, H_XmDropSiteUpdate);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmDropSiteEndUpdate" XM_POSTFIX, gxm_XmDropSiteEndUpdate_w, 1, 0, 0, H_XmDropSiteEndUpdate);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmDropSiteRetrieve" XM_POSTFIX, gxm_XmDropSiteRetrieve_w, 2, 1, 0, H_XmDropSiteRetrieve);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmDropSiteQueryStackingOrder" XM_POSTFIX, gxm_XmDropSiteQueryStackingOrder_w, 1, 0, 0, H_XmDropSiteQueryStackingOrder);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmDropSiteConfigureStackingOrder" XM_POSTFIX, gxm_XmDropSiteConfigureStackingOrder_w, 3, 0, 0, H_XmDropSiteConfigureStackingOrder);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmDropTransferStart" XM_POSTFIX, gxm_XmDropTransferStart_w, 2, 1, 0, H_XmDropTransferStart);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmDropTransferAdd" XM_POSTFIX, gxm_XmDropTransferAdd_w, 2, 0, 0, H_XmDropTransferAdd);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldGetString" XM_POSTFIX, gxm_XmTextFieldGetString_w, 1, 0, 0, H_XmTextFieldGetString);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldGetSubstring" XM_POSTFIX, gxm_XmTextFieldGetSubstring_w, 3, 0, 0, H_XmTextFieldGetSubstring);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldGetLastPosition" XM_POSTFIX, gxm_XmTextFieldGetLastPosition_w, 1, 0, 0, H_XmTextFieldGetLastPosition);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldSetString" XM_POSTFIX, gxm_XmTextFieldSetString_w, 2, 0, 0, H_XmTextFieldSetString);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldReplace" XM_POSTFIX, gxm_XmTextFieldReplace_w, 4, 0, 0, H_XmTextFieldReplace);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldInsert" XM_POSTFIX, gxm_XmTextFieldInsert_w, 3, 0, 0, H_XmTextFieldInsert);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldSetAddMode" XM_POSTFIX, gxm_XmTextFieldSetAddMode_w, 2, 0, 0, H_XmTextFieldSetAddMode);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldGetAddMode" XM_POSTFIX, gxm_XmTextFieldGetAddMode_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldGetEditable" XM_POSTFIX, gxm_XmTextFieldGetEditable_w, 1, 0, 0, H_XmTextFieldGetEditable);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldSetEditable" XM_POSTFIX, gxm_XmTextFieldSetEditable_w, 2, 0, 0, H_XmTextFieldSetEditable);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldGetMaxLength" XM_POSTFIX, gxm_XmTextFieldGetMaxLength_w, 1, 0, 0, H_XmTextFieldGetMaxLength);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldSetMaxLength" XM_POSTFIX, gxm_XmTextFieldSetMaxLength_w, 2, 0, 0, H_XmTextFieldSetMaxLength);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldGetCursorPosition" XM_POSTFIX, gxm_XmTextFieldGetCursorPosition_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldGetInsertionPosition" XM_POSTFIX, gxm_XmTextFieldGetInsertionPosition_w, 1, 0, 0, H_XmTextFieldGetInsertionPosition);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldSetCursorPosition" XM_POSTFIX, gxm_XmTextFieldSetCursorPosition_w, 2, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldSetInsertionPosition" XM_POSTFIX, gxm_XmTextFieldSetInsertionPosition_w, 2, 0, 0, H_XmTextFieldSetInsertionPosition);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldGetSelectionPosition" XM_POSTFIX, gxm_XmTextFieldGetSelectionPosition_w, 1, 0, 0, H_XmTextFieldGetSelectionPosition);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldGetSelection" XM_POSTFIX, gxm_XmTextFieldGetSelection_w, 1, 0, 0, H_XmTextFieldGetSelection);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldRemove" XM_POSTFIX, gxm_XmTextFieldRemove_w, 1, 0, 0, H_XmTextFieldRemove);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldCopy" XM_POSTFIX, gxm_XmTextFieldCopy_w, 2, 0, 0, H_XmTextFieldCopy);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldCut" XM_POSTFIX, gxm_XmTextFieldCut_w, 2, 0, 0, H_XmTextFieldCut);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldPaste" XM_POSTFIX, gxm_XmTextFieldPaste_w, 1, 0, 0, H_XmTextFieldPaste);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldClearSelection" XM_POSTFIX, gxm_XmTextFieldClearSelection_w, 2, 0, 0, H_XmTextFieldClearSelection);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldSetSelection" XM_POSTFIX, gxm_XmTextFieldSetSelection_w, 4, 0, 0, H_XmTextFieldSetSelection);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldXYToPos" XM_POSTFIX, gxm_XmTextFieldXYToPos_w, 3, 0, 0, H_XmTextFieldXYToPos);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldPosToXY" XM_POSTFIX, gxm_XmTextFieldPosToXY_w, 2, 0, 0, H_XmTextFieldPosToXY);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldShowPosition" XM_POSTFIX, gxm_XmTextFieldShowPosition_w, 2, 0, 0, H_XmTextFieldShowPosition);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldSetHighlight" XM_POSTFIX, gxm_XmTextFieldSetHighlight_w, 4, 0, 0, H_XmTextFieldSetHighlight);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFieldGetBaseline" XM_POSTFIX, gxm_XmTextFieldGetBaseline_w, 1, 0, 0, H_XmTextFieldGetBaseline);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateTextField" XM_POSTFIX, gxm_XmCreateTextField_w, 3, 1, 0, H_XmCreateTextField);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmFileSelectionBoxGetChild" XM_POSTFIX, gxm_XmFileSelectionBoxGetChild_w, 2, 0, 0, H_XmFileSelectionBoxGetChild);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmFileSelectionDoSearch" XM_POSTFIX, gxm_XmFileSelectionDoSearch_w, 2, 0, 0, H_XmFileSelectionDoSearch);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateFileSelectionBox" XM_POSTFIX, gxm_XmCreateFileSelectionBox_w, 3, 1, 0, H_XmCreateFileSelectionBox);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateFileSelectionDialog" XM_POSTFIX, gxm_XmCreateFileSelectionDialog_w, 3, 1, 0, H_XmCreateFileSelectionDialog);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextSetHighlight" XM_POSTFIX, gxm_XmTextSetHighlight_w, 4, 0, 0, H_XmTextSetHighlight);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateScrolledText" XM_POSTFIX, gxm_XmCreateScrolledText_w, 3, 1, 0, H_XmCreateScrolledText);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateText" XM_POSTFIX, gxm_XmCreateText_w, 3, 1, 0, H_XmCreateText);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextGetSubstring" XM_POSTFIX, gxm_XmTextGetSubstring_w, 3, 0, 0, H_XmTextGetSubstring);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextGetString" XM_POSTFIX, gxm_XmTextGetString_w, 1, 0, 0, H_XmTextGetString);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextGetLastPosition" XM_POSTFIX, gxm_XmTextGetLastPosition_w, 1, 0, 0, H_XmTextGetLastPosition);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextSetString" XM_POSTFIX, gxm_XmTextSetString_w, 2, 0, 0, H_XmTextSetString);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextReplace" XM_POSTFIX, gxm_XmTextReplace_w, 4, 0, 0, H_XmTextReplace);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextInsert" XM_POSTFIX, gxm_XmTextInsert_w, 3, 0, 0, H_XmTextInsert);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextSetAddMode" XM_POSTFIX, gxm_XmTextSetAddMode_w, 2, 0, 0, H_XmTextSetAddMode);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextGetAddMode" XM_POSTFIX, gxm_XmTextGetAddMode_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextGetEditable" XM_POSTFIX, gxm_XmTextGetEditable_w, 1, 0, 0, H_XmTextGetEditable);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextSetEditable" XM_POSTFIX, gxm_XmTextSetEditable_w, 2, 0, 0, H_XmTextSetEditable);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextGetMaxLength" XM_POSTFIX, gxm_XmTextGetMaxLength_w, 1, 0, 0, H_XmTextGetMaxLength);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextSetMaxLength" XM_POSTFIX, gxm_XmTextSetMaxLength_w, 2, 0, 0, H_XmTextSetMaxLength);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextGetTopCharacter" XM_POSTFIX, gxm_XmTextGetTopCharacter_w, 1, 0, 0, H_XmTextGetTopCharacter);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextSetTopCharacter" XM_POSTFIX, gxm_XmTextSetTopCharacter_w, 2, 0, 0, H_XmTextSetTopCharacter);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextGetCursorPosition" XM_POSTFIX, gxm_XmTextGetCursorPosition_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextGetInsertionPosition" XM_POSTFIX, gxm_XmTextGetInsertionPosition_w, 1, 0, 0, H_XmTextGetInsertionPosition);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextSetInsertionPosition" XM_POSTFIX, gxm_XmTextSetInsertionPosition_w, 2, 0, 0, H_XmTextSetInsertionPosition);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextSetCursorPosition" XM_POSTFIX, gxm_XmTextSetCursorPosition_w, 2, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextRemove" XM_POSTFIX, gxm_XmTextRemove_w, 1, 0, 0, H_XmTextRemove);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextCopy" XM_POSTFIX, gxm_XmTextCopy_w, 2, 0, 0, H_XmTextCopy);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextCut" XM_POSTFIX, gxm_XmTextCut_w, 2, 0, 0, H_XmTextCut);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextPaste" XM_POSTFIX, gxm_XmTextPaste_w, 1, 0, 0, H_XmTextPaste);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextGetSelection" XM_POSTFIX, gxm_XmTextGetSelection_w, 1, 0, 0, H_XmTextGetSelection);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextSetSelection" XM_POSTFIX, gxm_XmTextSetSelection_w, 4, 0, 0, H_XmTextSetSelection);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextClearSelection" XM_POSTFIX, gxm_XmTextClearSelection_w, 2, 0, 0, H_XmTextClearSelection);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextGetSelectionPosition" XM_POSTFIX, gxm_XmTextGetSelectionPosition_w, 1, 0, 0, H_XmTextGetSelectionPosition);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextXYToPos" XM_POSTFIX, gxm_XmTextXYToPos_w, 3, 0, 0, H_XmTextXYToPos);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextPosToXY" XM_POSTFIX, gxm_XmTextPosToXY_w, 2, 0, 0, H_XmTextPosToXY);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextGetSource" XM_POSTFIX, gxm_XmTextGetSource_w, 1, 0, 0, H_XmTextGetSource);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextSetSource" XM_POSTFIX, gxm_XmTextSetSource_w, 4, 0, 0, H_XmTextSetSource);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextShowPosition" XM_POSTFIX, gxm_XmTextShowPosition_w, 2, 0, 0, H_XmTextShowPosition);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextScroll" XM_POSTFIX, gxm_XmTextScroll_w, 2, 0, 0, H_XmTextScroll);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextGetBaseline" XM_POSTFIX, gxm_XmTextGetBaseline_w, 1, 0, 0, H_XmTextGetBaseline);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextDisableRedisplay" XM_POSTFIX, gxm_XmTextDisableRedisplay_w, 1, 0, 0, H_XmTextDisableRedisplay);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextEnableRedisplay" XM_POSTFIX, gxm_XmTextEnableRedisplay_w, 1, 0, 0, H_XmTextEnableRedisplay);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextFindString" XM_POSTFIX, gxm_XmTextFindString_w, 4, 0, 0, H_XmTextFindString);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateForm" XM_POSTFIX, gxm_XmCreateForm_w, 3, 1, 0, H_XmCreateForm);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateFormDialog" XM_POSTFIX, gxm_XmCreateFormDialog_w, 3, 1, 0, H_XmCreateFormDialog);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateFrame" XM_POSTFIX, gxm_XmCreateFrame_w, 3, 1, 0, H_XmCreateFrame);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmToggleButtonGadgetGetState" XM_POSTFIX, gxm_XmToggleButtonGadgetGetState_w, 1, 0, 0, H_XmToggleButtonGadgetGetState);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmToggleButtonGadgetSetState" XM_POSTFIX, gxm_XmToggleButtonGadgetSetState_w, 3, 0, 0, H_XmToggleButtonGadgetSetState);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateToggleButtonGadget" XM_POSTFIX, gxm_XmCreateToggleButtonGadget_w, 3, 1, 0, H_XmCreateToggleButtonGadget);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmToggleButtonGetState" XM_POSTFIX, gxm_XmToggleButtonGetState_w, 1, 0, 0, H_XmToggleButtonGetState);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmToggleButtonSetState" XM_POSTFIX, gxm_XmToggleButtonSetState_w, 3, 0, 0, H_XmToggleButtonSetState);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateToggleButton" XM_POSTFIX, gxm_XmCreateToggleButton_w, 3, 1, 0, H_XmCreateToggleButton);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateLabelGadget" XM_POSTFIX, gxm_XmCreateLabelGadget_w, 3, 1, 0, H_XmCreateLabelGadget);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateLabel" XM_POSTFIX, gxm_XmCreateLabel_w, 3, 1, 0, H_XmCreateLabel);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsMotifWMRunning" XM_POSTFIX, gxm_XmIsMotifWMRunning_w, 1, 0, 0, H_XmIsMotifWMRunning);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListAddItem" XM_POSTFIX, gxm_XmListAddItem_w, 3, 0, 0, H_XmListAddItem);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListAddItems" XM_POSTFIX, gxm_XmListAddItems_w, 4, 0, 0, H_XmListAddItems);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListAddItemsUnselected" XM_POSTFIX, gxm_XmListAddItemsUnselected_w, 4, 0, 0, H_XmListAddItemsUnselected);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListAddItemUnselected" XM_POSTFIX, gxm_XmListAddItemUnselected_w, 3, 0, 0, H_XmListAddItemUnselected);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListDeleteItem" XM_POSTFIX, gxm_XmListDeleteItem_w, 2, 0, 0, H_XmListDeleteItem);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListDeleteItems" XM_POSTFIX, gxm_XmListDeleteItems_w, 3, 0, 0, H_XmListDeleteItems);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListDeletePositions" XM_POSTFIX, gxm_XmListDeletePositions_w, 3, 0, 0, H_XmListDeletePositions);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListDeletePos" XM_POSTFIX, gxm_XmListDeletePos_w, 2, 0, 0, H_XmListDeletePos);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListDeleteItemsPos" XM_POSTFIX, gxm_XmListDeleteItemsPos_w, 3, 0, 0, H_XmListDeleteItemsPos);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListDeleteAllItems" XM_POSTFIX, gxm_XmListDeleteAllItems_w, 1, 0, 0, H_XmListDeleteAllItems);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListReplaceItems" XM_POSTFIX, gxm_XmListReplaceItems_w, 4, 0, 0, H_XmListReplaceItems);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListReplaceItemsPos" XM_POSTFIX, gxm_XmListReplaceItemsPos_w, 4, 0, 0, H_XmListReplaceItemsPos);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListReplaceItemsUnselected" XM_POSTFIX, gxm_XmListReplaceItemsUnselected_w, 4, 0, 0, H_XmListReplaceItemsUnselected);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListReplaceItemsPosUnselected" XM_POSTFIX, gxm_XmListReplaceItemsPosUnselected_w, 4, 0, 0, H_XmListReplaceItemsPosUnselected);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListReplacePositions" XM_POSTFIX, gxm_XmListReplacePositions_w, 4, 0, 0, H_XmListReplacePositions);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListSelectItem" XM_POSTFIX, gxm_XmListSelectItem_w, 3, 0, 0, H_XmListSelectItem);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListSelectPos" XM_POSTFIX, gxm_XmListSelectPos_w, 3, 0, 0, H_XmListSelectPos);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListDeselectItem" XM_POSTFIX, gxm_XmListDeselectItem_w, 2, 0, 0, H_XmListDeselectItem);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListDeselectPos" XM_POSTFIX, gxm_XmListDeselectPos_w, 2, 0, 0, H_XmListDeselectPos);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListDeselectAllItems" XM_POSTFIX, gxm_XmListDeselectAllItems_w, 1, 0, 0, H_XmListDeselectAllItems);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListSetPos" XM_POSTFIX, gxm_XmListSetPos_w, 2, 0, 0, H_XmListSetPos);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListSetBottomPos" XM_POSTFIX, gxm_XmListSetBottomPos_w, 2, 0, 0, H_XmListSetBottomPos);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListSetItem" XM_POSTFIX, gxm_XmListSetItem_w, 2, 0, 0, H_XmListSetItem);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListSetBottomItem" XM_POSTFIX, gxm_XmListSetBottomItem_w, 2, 0, 0, H_XmListSetBottomItem);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListSetAddMode" XM_POSTFIX, gxm_XmListSetAddMode_w, 2, 0, 0, H_XmListSetAddMode);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListItemExists" XM_POSTFIX, gxm_XmListItemExists_w, 2, 0, 0, H_XmListItemExists);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListItemPos" XM_POSTFIX, gxm_XmListItemPos_w, 2, 0, 0, H_XmListItemPos);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListGetKbdItemPos" XM_POSTFIX, gxm_XmListGetKbdItemPos_w, 1, 0, 0, H_XmListGetKbdItemPos);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListSetKbdItemPos" XM_POSTFIX, gxm_XmListSetKbdItemPos_w, 2, 0, 0, H_XmListSetKbdItemPos);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListYToPos" XM_POSTFIX, gxm_XmListYToPos_w, 2, 0, 0, H_XmListYToPos);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListPosToBounds" XM_POSTFIX, gxm_XmListPosToBounds_w, 2, 0, 0, H_XmListPosToBounds);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListGetMatchPos" XM_POSTFIX, gxm_XmListGetMatchPos_w, 2, 0, 0, H_XmListGetMatchPos);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListSetHorizPos" XM_POSTFIX, gxm_XmListSetHorizPos_w, 2, 0, 0, H_XmListSetHorizPos);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListUpdateSelectedList" XM_POSTFIX, gxm_XmListUpdateSelectedList_w, 1, 0, 0, H_XmListUpdateSelectedList);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListPosSelected" XM_POSTFIX, gxm_XmListPosSelected_w, 2, 0, 0, H_XmListPosSelected);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateList" XM_POSTFIX, gxm_XmCreateList_w, 3, 1, 0, H_XmCreateList);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateScrolledList" XM_POSTFIX, gxm_XmCreateScrolledList_w, 3, 1, 0, H_XmCreateScrolledList);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTranslateKey" XM_POSTFIX, gxm_XmTranslateKey_w, 3, 0, 0, H_XmTranslateKey);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateMainWindow" XM_POSTFIX, gxm_XmCreateMainWindow_w, 3, 1, 0, H_XmCreateMainWindow);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmInstallImage" XM_POSTFIX, gxm_XmInstallImage_w, 2, 0, 0, H_XmInstallImage);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmUninstallImage" XM_POSTFIX, gxm_XmUninstallImage_w, 1, 0, 0, H_XmUninstallImage);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmGetPixmap" XM_POSTFIX, gxm_XmGetPixmap_w, 4, 0, 0, H_XmGetPixmap);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmGetPixmapByDepth" XM_POSTFIX, gxm_XmGetPixmapByDepth_w, 5, 0, 0, H_XmGetPixmapByDepth);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmDestroyPixmap" XM_POSTFIX, gxm_XmDestroyPixmap_w, 2, 0, 0, H_XmDestroyPixmap);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmUpdateDisplay" XM_POSTFIX, gxm_XmUpdateDisplay_w, 1, 0, 0, H_XmUpdateDisplay);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmWidgetGetBaselines" XM_POSTFIX, gxm_XmWidgetGetBaselines_w, 1, 0, 0, H_XmWidgetGetBaselines);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmRegisterSegmentEncoding" XM_POSTFIX, gxm_XmRegisterSegmentEncoding_w, 2, 0, 0, H_XmRegisterSegmentEncoding);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmMapSegmentEncoding" XM_POSTFIX, gxm_XmMapSegmentEncoding_w, 1, 0, 0, H_XmMapSegmentEncoding);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCvtCTToXmString" XM_POSTFIX, gxm_XmCvtCTToXmString_w, 1, 0, 0, H_XmCvtCTToXmString);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCvtXmStringToCT" XM_POSTFIX, gxm_XmCvtXmStringToCT_w, 1, 0, 0, H_XmCvtXmStringToCT);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmConvertUnits" XM_POSTFIX, gxm_XmConvertUnits_w, 5, 0, 0, H_XmConvertUnits);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateSimpleMenuBar" XM_POSTFIX, gxm_XmCreateSimpleMenuBar_w, 3, 1, 0, H_XmCreateSimpleMenuBar);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateSimplePopupMenu" XM_POSTFIX, gxm_XmCreateSimplePopupMenu_w, 3, 1, 0, H_XmCreateSimplePopupMenu);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateSimplePulldownMenu" XM_POSTFIX, gxm_XmCreateSimplePulldownMenu_w, 3, 1, 0, H_XmCreateSimplePulldownMenu);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateSimpleOptionMenu" XM_POSTFIX, gxm_XmCreateSimpleOptionMenu_w, 3, 1, 0, H_XmCreateSimpleOptionMenu);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateSimpleRadioBox" XM_POSTFIX, gxm_XmCreateSimpleRadioBox_w, 3, 1, 0, H_XmCreateSimpleRadioBox);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateSimpleCheckBox" XM_POSTFIX, gxm_XmCreateSimpleCheckBox_w, 3, 1, 0, H_XmCreateSimpleCheckBox);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmVaCreateSimpleMenuBar" XM_POSTFIX, gxm_XmVaCreateSimpleMenuBar_w, 3, 0, 0, H_XmVaCreateSimpleMenuBar);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmVaCreateSimplePopupMenu" XM_POSTFIX, gxm_XmVaCreateSimplePopupMenu_w, 4, 0, 0, H_XmVaCreateSimplePopupMenu);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmVaCreateSimplePulldownMenu" XM_POSTFIX, gxm_XmVaCreateSimplePulldownMenu_w, 5, 0, 0, H_XmVaCreateSimplePulldownMenu);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmVaCreateSimpleOptionMenu" XM_POSTFIX, gxm_XmVaCreateSimpleOptionMenu_w, 7, 0, 0, H_XmVaCreateSimpleOptionMenu);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmVaCreateSimpleRadioBox" XM_POSTFIX, gxm_XmVaCreateSimpleRadioBox_w, 5, 0, 0, H_XmVaCreateSimpleRadioBox);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmVaCreateSimpleCheckBox" XM_POSTFIX, gxm_XmVaCreateSimpleCheckBox_w, 4, 0, 0, H_XmVaCreateSimpleCheckBox);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTrackingEvent" XM_POSTFIX, gxm_XmTrackingEvent_w, 3, 0, 0, H_XmTrackingEvent);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmSetColorCalculation" XM_POSTFIX, gxm_XmSetColorCalculation_w, 1, 0, 0, H_XmSetColorCalculation);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmGetColorCalculation" XM_POSTFIX, gxm_XmGetColorCalculation_w, 0, 0, 0, H_XmGetColorCalculation);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmGetColors" XM_POSTFIX, gxm_XmGetColors_w, 3, 0, 0, H_XmGetColors);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmChangeColor" XM_POSTFIX, gxm_XmChangeColor_w, 2, 0, 0, H_XmChangeColor);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringCreate" XM_POSTFIX, gxm_XmStringCreate_w, 2, 0, 0, H_XmStringCreate);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringCreateLocalized" XM_POSTFIX, gxm_XmStringCreateLocalized_w, 1, 0, 0, H_XmStringCreateLocalized);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringDirectionCreate" XM_POSTFIX, gxm_XmStringDirectionCreate_w, 1, 0, 0, H_XmStringDirectionCreate);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringSeparatorCreate" XM_POSTFIX, gxm_XmStringSeparatorCreate_w, 0, 0, 0, H_XmStringSeparatorCreate);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringInitContext" XM_POSTFIX, gxm_XmStringInitContext_w, 1, 0, 0, H_XmStringInitContext);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringFreeContext" XM_POSTFIX, gxm_XmStringFreeContext_w, 1, 0, 0, H_XmStringFreeContext);
#if MOTIF_2
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringConcatAndFree" XM_POSTFIX, gxm_XmStringConcatAndFree_w, 2, 0, 0, H_XmStringConcatAndFree);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringIsVoid" XM_POSTFIX, gxm_XmStringIsVoid_w, 1, 0, 0, H_XmStringIsVoid);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCvtXmStringToByteStream" XM_POSTFIX, gxm_XmCvtXmStringToByteStream_w, 1, 0, 0, H_XmCvtXmStringToByteStream);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCvtByteStreamToXmString" XM_POSTFIX, gxm_XmCvtByteStreamToXmString_w, 1, 0, 0, H_XmCvtByteStreamToXmString);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringByteStreamLength" XM_POSTFIX, gxm_XmStringByteStreamLength_w, 1, 0, 0, H_XmStringByteStreamLength);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringPeekNextTriple" XM_POSTFIX, gxm_XmStringPeekNextTriple_w, 1, 0, 0, H_XmStringPeekNextTriple);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringGetNextTriple" XM_POSTFIX, gxm_XmStringGetNextTriple_w, 1, 0, 0, H_XmStringGetNextTriple);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringComponentCreate" XM_POSTFIX, gxm_XmStringComponentCreate_w, 3, 0, 0, H_XmStringComponentCreate);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringUnparse" XM_POSTFIX, gxm_XmStringUnparse_w, 7, 0, 0, H_XmStringUnparse);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringParseText" XM_POSTFIX, gxm_XmStringParseText_w, 7, 0, 0, H_XmStringParseText);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringToXmStringTable" XM_POSTFIX, gxm_XmStringToXmStringTable_w, 2, 0, 0, H_XmStringToXmStringTable);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringTableToXmString" XM_POSTFIX, gxm_XmStringTableToXmString_w, 3, 0, 0, H_XmStringTableToXmString);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringTableUnparse" XM_POSTFIX, gxm_XmStringTableUnparse_w, 8, 0, 0, H_XmStringTableUnparse);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringTableParseStringArray" XM_POSTFIX, gxm_XmStringTableParseStringArray_w, 7, 0, 0, H_XmStringTableParseStringArray);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmDirectionToStringDirection" XM_POSTFIX, gxm_XmDirectionToStringDirection_w, 1, 0, 0, H_XmDirectionToStringDirection);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringDirectionToDirection" XM_POSTFIX, gxm_XmStringDirectionToDirection_w, 1, 0, 0, H_XmStringDirectionToDirection);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringGenerate" XM_POSTFIX, gxm_XmStringGenerate_w, 4, 0, 0, H_XmStringGenerate);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringPutRendition" XM_POSTFIX, gxm_XmStringPutRendition_w, 2, 0, 0, H_XmStringPutRendition);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmParseMappingCreate" XM_POSTFIX, gxm_XmParseMappingCreate_w, 1, 1, 0, H_XmParseMappingCreate);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmParseMappingSetValues" XM_POSTFIX, gxm_XmParseMappingSetValues_w, 2, 1, 0, H_XmParseMappingSetValues);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmParseMappingGetValues" XM_POSTFIX, gxm_XmParseMappingGetValues_w, 2, 1, 0, H_XmParseMappingGetValues);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmParseMappingFree" XM_POSTFIX, gxm_XmParseMappingFree_w, 1, 0, 0, H_XmParseMappingFree);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmParseTableFree" XM_POSTFIX, gxm_XmParseTableFree_w, 2, 0, 0, H_XmParseTableFree);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringTableProposeTablist" XM_POSTFIX, gxm_XmStringTableProposeTablist_w, 5, 0, 0, H_XmStringTableProposeTablist);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTabSetValue" XM_POSTFIX, gxm_XmTabSetValue_w, 2, 0, 0, H_XmTabSetValue);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTabGetValues" XM_POSTFIX, gxm_XmTabGetValues_w, 1, 0, 0, H_XmTabGetValues);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTabFree" XM_POSTFIX, gxm_XmTabFree_w, 1, 0, 0, H_XmTabFree);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTabCreate" XM_POSTFIX, gxm_XmTabCreate_w, 5, 0, 0, H_XmTabCreate);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTabListTabCount" XM_POSTFIX, gxm_XmTabListTabCount_w, 1, 0, 0, H_XmTabListTabCount);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTabListRemoveTabs" XM_POSTFIX, gxm_XmTabListRemoveTabs_w, 3, 0, 0, H_XmTabListRemoveTabs);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTabListReplacePositions" XM_POSTFIX, gxm_XmTabListReplacePositions_w, 4, 0, 0, H_XmTabListReplacePositions);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTabListGetTab" XM_POSTFIX, gxm_XmTabListGetTab_w, 2, 0, 0, H_XmTabListGetTab);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTabListCopy" XM_POSTFIX, gxm_XmTabListCopy_w, 3, 0, 0, H_XmTabListCopy);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTabListInsertTabs" XM_POSTFIX, gxm_XmTabListInsertTabs_w, 4, 0, 0, H_XmTabListInsertTabs);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmRenderTableCvtFromProp" XM_POSTFIX, gxm_XmRenderTableCvtFromProp_w, 3, 0, 0, H_XmRenderTableCvtFromProp);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmRenderTableCvtToProp" XM_POSTFIX, gxm_XmRenderTableCvtToProp_w, 2, 0, 0, H_XmRenderTableCvtToProp);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmRenditionUpdate" XM_POSTFIX, gxm_XmRenditionUpdate_w, 2, 1, 0, H_XmRenditionUpdate);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmRenditionRetrieve" XM_POSTFIX, gxm_XmRenditionRetrieve_w, 2, 1, 0, H_XmRenditionRetrieve);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmRenditionFree" XM_POSTFIX, gxm_XmRenditionFree_w, 1, 0, 0, H_XmRenditionFree);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmRenditionCreate" XM_POSTFIX, gxm_XmRenditionCreate_w, 3, 1, 0, H_XmRenditionCreate);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmRenderTableGetRenditions" XM_POSTFIX, gxm_XmRenderTableGetRenditions_w, 0, 3, 0, H_XmRenderTableGetRenditions);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmRenderTableGetRendition" XM_POSTFIX, gxm_XmRenderTableGetRendition_w, 2, 0, 0, H_XmRenderTableGetRendition);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmRenderTableGetTags" XM_POSTFIX, gxm_XmRenderTableGetTags_w, 1, 0, 0, H_XmRenderTableGetTags);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmRenderTableFree" XM_POSTFIX, gxm_XmRenderTableFree_w, 1, 0, 0, H_XmRenderTableFree);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmRenderTableCopy" XM_POSTFIX, gxm_XmRenderTableCopy_w, 0, 3, 0, H_XmRenderTableCopy);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmRenderTableRemoveRenditions" XM_POSTFIX, gxm_XmRenderTableRemoveRenditions_w, 0, 3, 0, H_XmRenderTableRemoveRenditions);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmRenderTableAddRenditions" XM_POSTFIX, gxm_XmRenderTableAddRenditions_w, 4, 0, 0, H_XmRenderTableAddRenditions);
#endif
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringConcat" XM_POSTFIX, gxm_XmStringConcat_w, 2, 0, 0, H_XmStringConcat);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringCopy" XM_POSTFIX, gxm_XmStringCopy_w, 1, 0, 0, H_XmStringCopy);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringCompare" XM_POSTFIX, gxm_XmStringCompare_w, 2, 0, 0, H_XmStringCompare);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringEmpty" XM_POSTFIX, gxm_XmStringEmpty_w, 1, 0, 0, H_XmStringEmpty);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringHasSubstring" XM_POSTFIX, gxm_XmStringHasSubstring_w, 2, 0, 0, H_XmStringHasSubstring);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringFree" XM_POSTFIX, gxm_XmStringFree_w, 1, 0, 0, H_XmStringFree);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringBaseline" XM_POSTFIX, gxm_XmStringBaseline_w, 2, 0, 0, H_XmStringBaseline);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringWidth" XM_POSTFIX, gxm_XmStringWidth_w, 2, 0, 0, H_XmStringWidth);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringHeight" XM_POSTFIX, gxm_XmStringHeight_w, 2, 0, 0, H_XmStringHeight);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringExtent" XM_POSTFIX, gxm_XmStringExtent_w, 2, 0, 0, H_XmStringExtent);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringLineCount" XM_POSTFIX, gxm_XmStringLineCount_w, 1, 0, 0, H_XmStringLineCount);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringDraw" XM_POSTFIX, gxm_XmStringDraw_w, 0, 0, 1, H_XmStringDraw);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringDrawImage" XM_POSTFIX, gxm_XmStringDrawImage_w, 0, 0, 1, H_XmStringDrawImage);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringDrawUnderline" XM_POSTFIX, gxm_XmStringDrawUnderline_w, 0, 0, 1, H_XmStringDrawUnderline);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmGetDestination" XM_POSTFIX, gxm_XmGetDestination_w, 1, 0, 0, H_XmGetDestination);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsTraversable" XM_POSTFIX, gxm_XmIsTraversable_w, 1, 0, 0, H_XmIsTraversable);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmGetVisibility" XM_POSTFIX, gxm_XmGetVisibility_w, 1, 0, 0, H_XmGetVisibility);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmGetTabGroup" XM_POSTFIX, gxm_XmGetTabGroup_w, 1, 0, 0, H_XmGetTabGroup);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmGetFocusWidget" XM_POSTFIX, gxm_XmGetFocusWidget_w, 1, 0, 0, H_XmGetFocusWidget);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmProcessTraversal" XM_POSTFIX, gxm_XmProcessTraversal_w, 2, 0, 0, H_XmProcessTraversal);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCreateMenuShell" XM_POSTFIX, gxm_XmCreateMenuShell_w, 3, 1, 0, H_XmCreateMenuShell);

  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsMessageBox" XM_POSTFIX, gxm_XmIsMessageBox_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsArrowButtonGadget" XM_POSTFIX, gxm_XmIsArrowButtonGadget_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsArrowButton" XM_POSTFIX, gxm_XmIsArrowButton_w, 1, 0, 0, NULL);
#if MOTIF_2
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsNotebook" XM_POSTFIX, gxm_XmIsNotebook_w, 1, 0, 0, NULL);
#if HAVE_XP
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsPrintShell" XM_POSTFIX, gxm_XmIsPrintShell_w, 1, 0, 0, NULL);
#endif
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsComboBox" XM_POSTFIX, gxm_XmIsComboBox_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsContainer" XM_POSTFIX, gxm_XmIsContainer_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsGrabShell" XM_POSTFIX, gxm_XmIsGrabShell_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsIconGadget" XM_POSTFIX, gxm_XmIsIconGadget_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsIconHeader" XM_POSTFIX, gxm_XmIsIconHeader_w, 1, 0, 0, NULL);
#endif
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsPanedWindow" XM_POSTFIX, gxm_XmIsPanedWindow_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsBulletinBoard" XM_POSTFIX, gxm_XmIsBulletinBoard_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsPrimitive" XM_POSTFIX, gxm_XmIsPrimitive_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsCascadeButtonGadget" XM_POSTFIX, gxm_XmIsCascadeButtonGadget_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsCascadeButton" XM_POSTFIX, gxm_XmIsCascadeButton_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsPushButtonGadget" XM_POSTFIX, gxm_XmIsPushButtonGadget_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsPushButton" XM_POSTFIX, gxm_XmIsPushButton_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsCommand" XM_POSTFIX, gxm_XmIsCommand_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsRowColumn" XM_POSTFIX, gxm_XmIsRowColumn_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsScale" XM_POSTFIX, gxm_XmIsScale_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsScreen" XM_POSTFIX, gxm_XmIsScreen_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsScrollBar" XM_POSTFIX, gxm_XmIsScrollBar_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsDialogShell" XM_POSTFIX, gxm_XmIsDialogShell_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsScrolledWindow" XM_POSTFIX, gxm_XmIsScrolledWindow_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsDisplay" XM_POSTFIX, gxm_XmIsDisplay_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsSelectionBox" XM_POSTFIX, gxm_XmIsSelectionBox_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsDragContext" XM_POSTFIX, gxm_XmIsDragContext_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsSeparatorGadget" XM_POSTFIX, gxm_XmIsSeparatorGadget_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsDragIconObjectClass" XM_POSTFIX, gxm_XmIsDragIconObjectClass_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsSeparator" XM_POSTFIX, gxm_XmIsSeparator_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsDrawingArea" XM_POSTFIX, gxm_XmIsDrawingArea_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsDrawnButton" XM_POSTFIX, gxm_XmIsDrawnButton_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsDropSiteManager" XM_POSTFIX, gxm_XmIsDropSiteManager_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsDropTransfer" XM_POSTFIX, gxm_XmIsDropTransfer_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsTextField" XM_POSTFIX, gxm_XmIsTextField_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsFileSelectionBox" XM_POSTFIX, gxm_XmIsFileSelectionBox_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsText" XM_POSTFIX, gxm_XmIsText_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsForm" XM_POSTFIX, gxm_XmIsForm_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsFrame" XM_POSTFIX, gxm_XmIsFrame_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsGadget" XM_POSTFIX, gxm_XmIsGadget_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsToggleButtonGadget" XM_POSTFIX, gxm_XmIsToggleButtonGadget_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsToggleButton" XM_POSTFIX, gxm_XmIsToggleButton_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsLabelGadget" XM_POSTFIX, gxm_XmIsLabelGadget_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsLabel" XM_POSTFIX, gxm_XmIsLabel_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsVendorShell" XM_POSTFIX, gxm_XmIsVendorShell_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsList" XM_POSTFIX, gxm_XmIsList_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsMainWindow" XM_POSTFIX, gxm_XmIsMainWindow_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsManager" XM_POSTFIX, gxm_XmIsManager_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIsMenuShell" XM_POSTFIX, gxm_XmIsMenuShell_w, 1, 0, 0, NULL);
#endif

#if (!XM_DISABLE_DEPRECATED)
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtWarning" XM_POSTFIX, gxm_XtWarning_w, 1, 0, 0, H_XtWarning);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAppWarning" XM_POSTFIX, gxm_XtAppWarning_w, 2, 0, 0, H_XtAppWarning);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtSetWarningMsgHandler" XM_POSTFIX, gxm_XtSetWarningMsgHandler_w, 1, 0, 0, H_XtSetWarningMsgHandler);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtSetWarningHandler" XM_POSTFIX, gxm_XtSetWarningHandler_w, 1, 0, 0, H_XtSetWarningHandler);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtWarningMsg" XM_POSTFIX, gxm_XtWarningMsg_w, 6, 0, 0, H_XtWarningMsg);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtErrorMsg" XM_POSTFIX, gxm_XtErrorMsg_w, 6, 0, 0, H_XtErrorMsg);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtError" XM_POSTFIX, gxm_XtError_w, 1, 0, 0, H_XtError);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtSetErrorMsgHandler" XM_POSTFIX, gxm_XtSetErrorMsgHandler_w, 1, 0, 0, H_XtSetErrorMsgHandler);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtSetErrorHandler" XM_POSTFIX, gxm_XtSetErrorHandler_w, 1, 0, 0, H_XtSetErrorHandler);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtInitialize" XM_POSTFIX, gxm_XtInitialize_w, 6, 0, 0, H_XtInitialize);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtCreateApplicationShell" XM_POSTFIX, gxm_XtCreateApplicationShell_w, 3, 1, 0, H_XtCreateApplicationShell);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtSetSelectionTimeout" XM_POSTFIX, gxm_XtSetSelectionTimeout_w, 1, 0, 0, H_XtSetSelectionTimeout);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtGetSelectionTimeout" XM_POSTFIX, gxm_XtGetSelectionTimeout_w, 0, 0, 0, H_XtGetSelectionTimeout);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAddActions" XM_POSTFIX, gxm_XtAddActions_w, 1, 0, 0, H_XtAddActions);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAddInput" XM_POSTFIX, gxm_XtAddInput_w, 3, 1, 0, H_XtAddInput);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAddWorkProc" XM_POSTFIX, gxm_XtAddWorkProc_w, 1, 1, 0, H_XtAddWorkProc);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAddTimeOut" XM_POSTFIX, gxm_XtAddTimeOut_w, 2, 1, 0, H_XtAddTimeOut);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtMainLoop" XM_POSTFIX, gxm_XtMainLoop_w, 0, 0, 0, H_XtMainLoop);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtProcessEvent" XM_POSTFIX, gxm_XtProcessEvent_w, 1, 0, 0, H_XtProcessEvent);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtPending" XM_POSTFIX, gxm_XtPending_w, 0, 0, 0, H_XtPending);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtNextEvent" XM_POSTFIX, gxm_XtNextEvent_w, 0, 0, 0, H_XtNextEvent);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtPeekEvent" XM_POSTFIX, gxm_XtPeekEvent_w, 0, 0, 0, H_XtPeekEvent);

  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmFontListEntryCreate" XM_POSTFIX, gxm_XmFontListEntryCreate_w, 3, 0, 0, H_XmFontListEntryCreate);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmFontListEntryCreate_r" XM_POSTFIX, gxm_XmFontListEntryCreate_r_w, 4, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmFontListEntryFree" XM_POSTFIX, gxm_XmFontListEntryFree_w, 1, 0, 0, H_XmFontListEntryFree);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmFontListEntryGetFont" XM_POSTFIX, gxm_XmFontListEntryGetFont_w, 2, 0, 0, H_XmFontListEntryGetFont);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmFontListEntryGetTag" XM_POSTFIX, gxm_XmFontListEntryGetTag_w, 1, 0, 0, H_XmFontListEntryGetTag);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmFontListAppendEntry" XM_POSTFIX, gxm_XmFontListAppendEntry_w, 2, 0, 0, H_XmFontListAppendEntry);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmFontListNextEntry" XM_POSTFIX, gxm_XmFontListNextEntry_w, 1, 0, 0, H_XmFontListNextEntry);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmFontListRemoveEntry" XM_POSTFIX, gxm_XmFontListRemoveEntry_w, 2, 0, 0, H_XmFontListRemoveEntry);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmFontListEntryLoad" XM_POSTFIX, gxm_XmFontListEntryLoad_w, 4, 0, 0, H_XmFontListEntryLoad);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmFontListFree" XM_POSTFIX, gxm_XmFontListFree_w, 1, 0, 0, H_XmFontListFree);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmFontListCopy" XM_POSTFIX, gxm_XmFontListCopy_w, 1, 0, 0, H_XmFontListCopy);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmFontListInitFontContext" XM_POSTFIX, gxm_XmFontListInitFontContext_w, 1, 0, 0, H_XmFontListInitFontContext);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmFontListFreeFontContext" XM_POSTFIX, gxm_XmFontListFreeFontContext_w, 1, 0, 0, H_XmFontListFreeFontContext);

  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmFontContext?" XM_POSTFIX, XEN_XmFontContext_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmFontListEntry?" XM_POSTFIX, XEN_XmFontListEntry_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmFontList?" XM_POSTFIX, XEN_XmFontList_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmFontListCreate_r" XM_POSTFIX, gxm_XmFontListCreate_r_w, 3, 0, 0, H_XmFontListCreate_r);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmFontListCreate" XM_POSTFIX, gxm_XmFontListCreate_w, 2, 0, 0, H_XmFontListCreate);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTrackingLocate" XM_POSTFIX, gxm_XmTrackingLocate_w, 3, 0, 0, H_XmTrackingLocate);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmFontListGetNextFont" XM_POSTFIX, gxm_XmFontListGetNextFont_w, 1, 0, 0, H_XmFontListGetNextFont);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringByteCompare" XM_POSTFIX, gxm_XmStringByteCompare_w, 2, 0, 0, H_XmStringByteCompare);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringCreateLtoR" XM_POSTFIX, gxm_XmStringCreateLtoR_w, 2, 0, 0, H_XmStringCreateLtoR);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringCreateSimple" XM_POSTFIX, gxm_XmStringCreateSimple_w, 1, 0, 0, H_XmStringCreateSimple);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringGetLtoR" XM_POSTFIX, gxm_XmStringGetLtoR_w, 2, 0, 0, H_XmStringGetLtoR);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringGetNextSegment" XM_POSTFIX, gxm_XmStringGetNextSegment_w, 1, 0, 0, H_XmStringGetNextSegment);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringSegmentCreate" XM_POSTFIX, gxm_XmStringSegmentCreate_w, 4, 0, 0, H_XmStringSegmentCreate);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringPeekNextComponent" XM_POSTFIX, gxm_XmStringPeekNextComponent_w, 1, 0, 0, H_XmStringPeekNextComponent);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringGetNextComponent" XM_POSTFIX, gxm_XmStringGetNextComponent_w, 1, 0, 0, H_XmStringGetNextComponent);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmRemoveTabGroup" XM_POSTFIX, gxm_XmRemoveTabGroup_w, 1, 0, 0, H_XmRemoveTabGroup);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmFontListAdd" XM_POSTFIX, gxm_XmFontListAdd_w, 3, 0, 0, H_XmFontListAdd);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringLength" XM_POSTFIX, gxm_XmStringLength_w, 1, 0, 0, H_XmStringLength);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringNConcat" XM_POSTFIX, gxm_XmStringNConcat_w, 3, 0, 0, H_XmStringNConcat);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringNCopy" XM_POSTFIX, gxm_XmStringNCopy_w, 2, 0, 0, H_XmStringNCopy);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmScrolledWindowSetAreas" XM_POSTFIX, gxm_XmScrolledWindowSetAreas_w, 4, 0, 0, H_XmScrolledWindowSetAreas);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmSetFontUnits" XM_POSTFIX, gxm_XmSetFontUnits_w, 3, 0, 0, H_XmSetFontUnits);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmSetFontUnit" XM_POSTFIX, gxm_XmSetFontUnit_w, 2, 0, 0, H_XmSetFontUnit);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmGetMenuCursor" XM_POSTFIX, gxm_XmGetMenuCursor_w, 1, 0, 0, H_XmGetMenuCursor);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmSetMenuCursor" XM_POSTFIX, gxm_XmSetMenuCursor_w, 2, 0, 0, H_XmSetMenuCursor);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmListGetSelectedPos" XM_POSTFIX, gxm_XmListGetSelectedPos_w, 1, 0, 0, H_XmListGetSelectedPos);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmMainWindowSetAreas" XM_POSTFIX, gxm_XmMainWindowSetAreas_w, 6, 0, 0, H_XmMainWindowSetAreas);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmMainWindowSep1" XM_POSTFIX, gxm_XmMainWindowSep1_w, 1, 0, 0, H_XmMainWindowSep1);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmMainWindowSep2" XM_POSTFIX, gxm_XmMainWindowSep2_w, 1, 0, 0, H_XmMainWindowSep2);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmMainWindowSep3" XM_POSTFIX, gxm_XmMainWindowSep3_w, 1, 0, 0, H_XmMainWindowSep3);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmAddTabGroup" XM_POSTFIX, gxm_XmAddTabGroup_w, 1, 0, 0, H_XmAddTabGroup);
#endif

#if HAVE_XPM
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpmCreatePixmapFromData" XM_POSTFIX, gxm_XpmCreatePixmapFromData_w, 4, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpmCreateDataFromPixmap" XM_POSTFIX, gxm_XpmCreateDataFromPixmap_w, 4, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpmReadFileToPixmap" XM_POSTFIX, gxm_XpmReadFileToPixmap_w, 4, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpmReadPixmapFile" XM_POSTFIX, gxm_XpmReadFileToPixmap_w, 4, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpmWriteFileFromPixmap" XM_POSTFIX, gxm_XpmWriteFileFromPixmap_w, 5, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpmWritePixmapFile" XM_POSTFIX, gxm_XpmWriteFileFromPixmap_w, 5, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpmCreatePixmapFromBuffer" XM_POSTFIX, gxm_XpmCreatePixmapFromBuffer_w, 4, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpmCreateBufferFromImage" XM_POSTFIX, gxm_XpmCreateBufferFromImage_w, 4, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpmCreateBufferFromPixmap" XM_POSTFIX, gxm_XpmCreateBufferFromPixmap_w, 4, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpmCreatePixmapFromXpmImage" XM_POSTFIX, gxm_XpmCreatePixmapFromXpmImage_w, 4, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpmCreateXpmImageFromPixmap" XM_POSTFIX, gxm_XpmCreateXpmImageFromPixmap_w, 5, 0, 0, NULL);
#endif
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGetPixel" XM_POSTFIX, gxm_XGetPixel_w, 3, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XDestroyImage" XM_POSTFIX, gxm_XDestroyImage_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XPutPixel" XM_POSTFIX, gxm_XPutPixel_w, 4, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSubImage" XM_POSTFIX, gxm_XSubImage_w, 5, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XAddPixel" XM_POSTFIX, gxm_XAddPixel_w, 2, 0, 0, NULL);


  /*  ADD: XtAppContext? XtRequestId? XtWorkProcId? XtInputId? XtIntervalId? Screen? XEvent? XRectangle? XArc?
      ADD: XPoint? XSegment? XColor? XmTab? Atom? Colormap? Depth? Display? Drawable? Font? GC? KeySym? Pixel? Pixmap? Region?
      ADD: Time? Visual? Window? XFontProp? XFontStruct? XGCValues? XImage? XVisualInfo? XWMHints? XWindowAttributes? XWindowChanges?
      ADD: KeyCode? XContext? Substitution? XmString? XmToggleButton? XmDrawingArea?
      ADD: XmPushButton? XmTextField? XmFileSelectionBox? XmText? XmFrame? XmLabel? XmList? XmArrowButton? XmScrollBar? XmCommand?
      ADD: XmScale? XmRowColumn? XmNotebook? XmPrintShell? XmComboBox? XmContainer? XmIconHeader? XmGrabShell? XmPanedWindow? XmScrolledWindow?
      ADD: XmCascadeButton? XmForm? XmBulletinBoard? XmScreen? XmDialogShell? XmDisplay? XmSelectionBox? XmDragContext? XmDragIconObjectClass?
      ADD: XmSeparator? XmDropSiteManager? XmDropTransfer? XmVendorShell? XmMainWindow? XmMessageBox? XmManager? XmMenuShell? XmIconGadget?
      ADD: XmLabelGadget? XmPushButtonGadget? XmSeparatorGadget? XmArrowButtonGadget? XmCascadeButtonGadget? XmToggleButtonGadget? XmDrawnButton?
      ADD: XmPrimitive? XmTabList? XmParseMapping? XmFontList? XmFontListEntry? XmTextSource? XmStringContext?
      ADD: XStandardColormap? WidgetClass? Widget? XTextItem? XCharStruct? XmParseTable? XmFontContext? XFontSet?
      ADD: XpmAttributes? XpmImage? XmRendition? XmRenderTable? XModifierKeymap? XPContext?
  */
#if HAVE_MOTIF
  XEN_DEFINE_PROCEDURE(XM_PREFIX "Pixel" XM_POSTFIX, gxm_Pixel_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "GC" XM_POSTFIX, gxm_GC_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "Widget" XM_POSTFIX, gxm_Widget_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtAppContext?" XM_POSTFIX, XEN_XtAppContext_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtRequestId?" XM_POSTFIX, XEN_XtRequestId_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtWorkProcId?" XM_POSTFIX, XEN_XtWorkProcId_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtInputId?" XM_POSTFIX, XEN_XtInputId_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XtIntervalId?" XM_POSTFIX, XEN_XtIntervalId_p_w, 1, 0, 0, NULL);
#endif
  XEN_DEFINE_PROCEDURE(XM_PREFIX "Screen?" XM_POSTFIX, XEN_Screen_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XEvent?" XM_POSTFIX, XEN_XEvent_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XRectangle?" XM_POSTFIX, XEN_XRectangle_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XArc?" XM_POSTFIX, XEN_XArc_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XPoint?" XM_POSTFIX, XEN_XPoint_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSegment?" XM_POSTFIX, XEN_XSegment_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XColor?" XM_POSTFIX, XEN_XColor_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "Atom?" XM_POSTFIX, XEN_Atom_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "Colormap?" XM_POSTFIX, XEN_Colormap_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XModifierKeymap?" XM_POSTFIX, XEN_XModifierKeymap_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "Depth?" XM_POSTFIX, XEN_Depth_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "Display?" XM_POSTFIX, XEN_Display_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "Drawable?" XM_POSTFIX, XEN_Window_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "Font?" XM_POSTFIX, XEN_Font_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "GC?" XM_POSTFIX, XEN_GC_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "KeySym?" XM_POSTFIX, XEN_KeySym_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "Pixel?" XM_POSTFIX, XEN_Pixel_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "Pixmap?" XM_POSTFIX, XEN_Pixmap_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "Region?" XM_POSTFIX, XEN_Region_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "Time?" XM_POSTFIX, XEN_Time_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "Visual?" XM_POSTFIX, XEN_Visual_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "Window?" XM_POSTFIX, XEN_Window_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "Widget?" XM_POSTFIX, XEN_Widget_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmStringContext?" XM_POSTFIX, XEN_XmStringContext_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFontProp?" XM_POSTFIX, XEN_XFontProp_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFontSet?" XM_POSTFIX, XEN_XFontSet_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XFontStruct?" XM_POSTFIX, XEN_XFontStruct_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XGCValues?" XM_POSTFIX, XEN_XGCValues_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XImage?" XM_POSTFIX, XEN_XImage_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XVisualInfo?" XM_POSTFIX, XEN_XVisualInfo_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XWMHints?" XM_POSTFIX, XEN_XWMHints_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XWindowAttributes?" XM_POSTFIX, XEN_XWindowAttributes_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XWindowChanges?" XM_POSTFIX, XEN_XWindowChanges_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "KeyCode?" XM_POSTFIX, XEN_KeyCode_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XContext?" XM_POSTFIX, XEN_XContext_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XCharStruct?" XM_POSTFIX, XEN_XCharStruct_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XTextItem?" XM_POSTFIX, XEN_XTextItem_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XStandardColormap?" XM_POSTFIX, XEN_XStandardColormap_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "Substitution?" XM_POSTFIX, XEN_Substitution_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "Cursor?" XM_POSTFIX, XEN_Cursor_p_w, 1, 0, 0, NULL);
#if HAVE_XP
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XPContext?" XM_POSTFIX, XEN_XPContext_p_w, 1, 0, 0, NULL);
#endif
#if HAVE_MOTIF
  XEN_DEFINE_PROCEDURE(XM_PREFIX "WidgetClass?" XM_POSTFIX, XEN_WidgetClass_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmString?" XM_POSTFIX, XEN_XmString_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmToggleButton?" XM_POSTFIX, gxm_XmIsToggleButton_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmDrawingArea?" XM_POSTFIX, gxm_XmIsDrawingArea_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmPushButton?" XM_POSTFIX, gxm_XmIsPushButton_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextField?" XM_POSTFIX, gxm_XmIsTextField_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmFileSelectionBox?" XM_POSTFIX, gxm_XmIsFileSelectionBox_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmText?" XM_POSTFIX, gxm_XmIsText_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmFrame?" XM_POSTFIX, gxm_XmIsFrame_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmLabel?" XM_POSTFIX, gxm_XmIsLabel_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmList?" XM_POSTFIX, gxm_XmIsList_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmArrowButton?" XM_POSTFIX, gxm_XmIsArrowButton_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmScrollBar?" XM_POSTFIX, gxm_XmIsScrollBar_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCommand?" XM_POSTFIX, gxm_XmIsCommand_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmScale?" XM_POSTFIX, gxm_XmIsScale_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmRowColumn?" XM_POSTFIX, gxm_XmIsRowColumn_w, 1, 0, 0, NULL);
#if MOTIF_2
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmParseTable?" XM_POSTFIX, XEN_XmParseTable_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTab?" XM_POSTFIX, XEN_XmTab_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmNotebook?" XM_POSTFIX, gxm_XmIsNotebook_w, 1, 0, 0, NULL);
#if HAVE_XP
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmPrintShell?" XM_POSTFIX, gxm_XmIsPrintShell_w, 1, 0, 0, NULL);
#endif
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmComboBox?" XM_POSTFIX, gxm_XmIsComboBox_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmContainer?" XM_POSTFIX, gxm_XmIsContainer_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIconHeader?" XM_POSTFIX, gxm_XmIsIconHeader_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmGrabShell?" XM_POSTFIX, gxm_XmIsGrabShell_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmRendition?" XM_POSTFIX, XEN_XmRendition_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmRenderTable?" XM_POSTFIX, XEN_XmRenderTable_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmIconGadget?" XM_POSTFIX, gxm_XmIsIconGadget_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTabList?" XM_POSTFIX, XEN_XmTabList_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmParseMapping?" XM_POSTFIX, XEN_XmParseMapping_p_w, 1, 0, 0, NULL);
#endif
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmPanedWindow?" XM_POSTFIX, gxm_XmIsPanedWindow_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmScrolledWindow?" XM_POSTFIX, gxm_XmIsScrolledWindow_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCascadeButton?" XM_POSTFIX, gxm_XmIsCascadeButton_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmForm?" XM_POSTFIX, gxm_XmIsForm_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmBulletinBoard?" XM_POSTFIX, gxm_XmIsBulletinBoard_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmScreen?" XM_POSTFIX, gxm_XmIsScreen_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmDialogShell?" XM_POSTFIX, gxm_XmIsDialogShell_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmDisplay?" XM_POSTFIX, gxm_XmIsDisplay_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmSelectionBox?" XM_POSTFIX, gxm_XmIsSelectionBox_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmDragContext?" XM_POSTFIX, gxm_XmIsDragContext_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmDragIconObjectClass?" XM_POSTFIX, gxm_XmIsDragIconObjectClass_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmSeparator?" XM_POSTFIX, gxm_XmIsSeparator_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmDropSiteManager?" XM_POSTFIX, gxm_XmIsDropSiteManager_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmDropTransfer?" XM_POSTFIX, gxm_XmIsDropTransfer_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmVendorShell?" XM_POSTFIX, gxm_XmIsVendorShell_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmMainWindow?" XM_POSTFIX, gxm_XmIsMainWindow_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmMessageBox?" XM_POSTFIX, gxm_XmIsMessageBox_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmManager?" XM_POSTFIX, gxm_XmIsManager_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmMenuShell?" XM_POSTFIX, gxm_XmIsMenuShell_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmLabelGadget?" XM_POSTFIX, gxm_XmIsLabelGadget_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmPushButtonGadget?" XM_POSTFIX, gxm_XmIsPushButtonGadget_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmSeparatorGadget?" XM_POSTFIX, gxm_XmIsSeparatorGadget_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmArrowButtonGadget?" XM_POSTFIX, gxm_XmIsArrowButtonGadget_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmCascadeButtonGadget?" XM_POSTFIX, gxm_XmIsCascadeButtonGadget_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmToggleButtonGadget?" XM_POSTFIX, gxm_XmIsToggleButtonGadget_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmDrawnButton?" XM_POSTFIX, gxm_XmIsDrawnButton_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmPrimitive?" XM_POSTFIX, gxm_XmIsPrimitive_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XmTextSource?" XM_POSTFIX, XEN_XmTextSource_p_w, 1, 0, 0, NULL);
#endif
#if HAVE_XPM
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpmAttributes?" XM_POSTFIX, XEN_XpmAttributes_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpmImage?" XM_POSTFIX, XEN_XpmImage_p_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpmColorSymbol?" XM_POSTFIX, XEN_XpmColorSymbol_p_w, 1, 0, 0, NULL);
#endif
}


  XEN_NARGIFY_4(gxm_XSegment_w, gxm_XSegment)
  XEN_NARGIFY_4(gxm_XRectangle_w, gxm_XRectangle)
  XEN_NARGIFY_1(gxm_to_s_w, gxm_to_s)
  XEN_ARGIFY_6(gxm_XColor_w, gxm_XColor)
  XEN_NARGIFY_6(gxm_XArc_w, gxm_XArc)
  XEN_NARGIFY_2(gxm_XPoint_w, gxm_XPoint)
  XEN_NARGIFY_4(gxm_XTextItem_w, gxm_XTextItem)
  XEN_NARGIFY_1(gxm_pixel_w, gxm_pixel)
  XEN_NARGIFY_2(gxm_set_pixel_w, gxm_set_pixel)
  XEN_NARGIFY_1(gxm_red_w, gxm_red)
  XEN_NARGIFY_2(gxm_set_red_w, gxm_set_red)
  XEN_NARGIFY_1(gxm_green_w, gxm_green)
  XEN_NARGIFY_2(gxm_set_green_w, gxm_set_green)
  XEN_NARGIFY_1(gxm_blue_w, gxm_blue)
  XEN_NARGIFY_2(gxm_set_blue_w, gxm_set_blue)
  XEN_NARGIFY_1(gxm_flags_w, gxm_flags)
  XEN_NARGIFY_2(gxm_set_flags_w, gxm_set_flags)
  XEN_NARGIFY_1(gxm_pad_w, gxm_pad)
  XEN_NARGIFY_2(gxm_set_pad_w, gxm_set_pad)
  XEN_NARGIFY_1(gxm_x_w, gxm_x)
  XEN_NARGIFY_2(gxm_set_x_w, gxm_set_x)
  XEN_NARGIFY_1(gxm_y_w, gxm_y)
  XEN_NARGIFY_2(gxm_set_y_w, gxm_set_y)
  XEN_NARGIFY_1(gxm_width_w, gxm_width)
  XEN_NARGIFY_2(gxm_set_width_w, gxm_set_width)
  XEN_NARGIFY_1(gxm_height_w, gxm_height)
  XEN_NARGIFY_2(gxm_set_height_w, gxm_set_height)
  XEN_NARGIFY_1(gxm_angle1_w, gxm_angle1)
  XEN_NARGIFY_2(gxm_set_angle1_w, gxm_set_angle1)
  XEN_NARGIFY_1(gxm_angle2_w, gxm_angle2)
  XEN_NARGIFY_2(gxm_set_angle2_w, gxm_set_angle2)
  XEN_NARGIFY_1(gxm_x1_w, gxm_x1)
  XEN_NARGIFY_2(gxm_set_x1_w, gxm_set_x1)
  XEN_NARGIFY_1(gxm_y1_w, gxm_y1)
  XEN_NARGIFY_2(gxm_set_y1_w, gxm_set_y1)
  XEN_NARGIFY_1(gxm_x2_w, gxm_x2)
  XEN_NARGIFY_2(gxm_set_x2_w, gxm_set_x2)
  XEN_NARGIFY_1(gxm_y2_w, gxm_y2)
  XEN_NARGIFY_2(gxm_set_y2_w, gxm_set_y2)
  XEN_NARGIFY_1(gxm_dashes_w, gxm_dashes)
  XEN_NARGIFY_2(gxm_set_dashes_w, gxm_set_dashes)
  XEN_NARGIFY_1(gxm_dash_offset_w, gxm_dash_offset)
  XEN_NARGIFY_2(gxm_set_dash_offset_w, gxm_set_dash_offset)
  XEN_NARGIFY_1(gxm_clip_mask_w, gxm_clip_mask)
  XEN_NARGIFY_2(gxm_set_clip_mask_w, gxm_set_clip_mask)
  XEN_NARGIFY_1(gxm_clip_y_origin_w, gxm_clip_y_origin)
  XEN_NARGIFY_2(gxm_set_clip_y_origin_w, gxm_set_clip_y_origin)
  XEN_NARGIFY_1(gxm_clip_x_origin_w, gxm_clip_x_origin)
  XEN_NARGIFY_2(gxm_set_clip_x_origin_w, gxm_set_clip_x_origin)
  XEN_NARGIFY_1(gxm_graphics_exposures_w, gxm_graphics_exposures)
  XEN_NARGIFY_2(gxm_set_graphics_exposures_w, gxm_set_graphics_exposures)
  XEN_NARGIFY_1(gxm_subwindow_mode_w, gxm_subwindow_mode)
  XEN_NARGIFY_2(gxm_set_subwindow_mode_w, gxm_set_subwindow_mode)
  XEN_NARGIFY_1(gxm_font_w, gxm_font)
  XEN_NARGIFY_2(gxm_set_font_w, gxm_set_font)
  XEN_NARGIFY_1(gxm_ts_y_origin_w, gxm_ts_y_origin)
  XEN_NARGIFY_2(gxm_set_ts_y_origin_w, gxm_set_ts_y_origin)
  XEN_NARGIFY_1(gxm_ts_x_origin_w, gxm_ts_x_origin)
  XEN_NARGIFY_2(gxm_set_ts_x_origin_w, gxm_set_ts_x_origin)
  XEN_NARGIFY_1(gxm_stipple_w, gxm_stipple)
  XEN_NARGIFY_2(gxm_set_stipple_w, gxm_set_stipple)
  XEN_NARGIFY_1(gxm_tile_w, gxm_tile)
  XEN_NARGIFY_2(gxm_set_tile_w, gxm_set_tile)
  XEN_NARGIFY_1(gxm_arc_mode_w, gxm_arc_mode)
  XEN_NARGIFY_2(gxm_set_arc_mode_w, gxm_set_arc_mode)
  XEN_NARGIFY_1(gxm_fill_rule_w, gxm_fill_rule)
  XEN_NARGIFY_2(gxm_set_fill_rule_w, gxm_set_fill_rule)
  XEN_NARGIFY_1(gxm_fill_style_w, gxm_fill_style)
  XEN_NARGIFY_2(gxm_set_fill_style_w, gxm_set_fill_style)
  XEN_NARGIFY_1(gxm_join_style_w, gxm_join_style)
  XEN_NARGIFY_2(gxm_set_join_style_w, gxm_set_join_style)
  XEN_NARGIFY_1(gxm_cap_style_w, gxm_cap_style)
  XEN_NARGIFY_2(gxm_set_cap_style_w, gxm_set_cap_style)
  XEN_NARGIFY_1(gxm_line_style_w, gxm_line_style)
  XEN_NARGIFY_2(gxm_set_line_style_w, gxm_set_line_style)
  XEN_NARGIFY_1(gxm_line_width_w, gxm_line_width)
  XEN_NARGIFY_2(gxm_set_line_width_w, gxm_set_line_width)
  XEN_NARGIFY_1(gxm_background_w, gxm_background)
  XEN_NARGIFY_2(gxm_set_background_w, gxm_set_background)
  XEN_NARGIFY_1(gxm_foreground_w, gxm_foreground)
  XEN_NARGIFY_2(gxm_set_foreground_w, gxm_set_foreground)
  XEN_NARGIFY_1(gxm_plane_mask_w, gxm_plane_mask)
  XEN_NARGIFY_2(gxm_set_plane_mask_w, gxm_set_plane_mask)
  XEN_NARGIFY_1(gxm_function_w, gxm_function)
  XEN_NARGIFY_2(gxm_set_function_w, gxm_set_function)
  XEN_NARGIFY_1(gxm_delta_w, gxm_delta)
  XEN_NARGIFY_2(gxm_set_delta_w, gxm_set_delta)
  XEN_NARGIFY_1(gxm_nchars_w, gxm_nchars)
  XEN_NARGIFY_2(gxm_set_nchars_w, gxm_set_nchars)
  XEN_NARGIFY_1(gxm_chars_w, gxm_chars)
  XEN_NARGIFY_2(gxm_set_chars_w, gxm_set_chars)
  XEN_NARGIFY_1(gxm_name_w, gxm_name)
  XEN_NARGIFY_2(gxm_set_name_w, gxm_set_name)
  XEN_NARGIFY_1(gxm_depth_w, gxm_depth)
  XEN_NARGIFY_2(gxm_set_depth_w, gxm_set_depth)
  XEN_NARGIFY_1(gxm_visual_w, gxm_visual)
  XEN_NARGIFY_2(gxm_set_visual_w, gxm_set_visual)

  XEN_NARGIFY_1(gxm_display_w, gxm_display)
  XEN_NARGIFY_1(gxm_root_w, gxm_root)
  XEN_NARGIFY_1(gxm_mwidth_w, gxm_mwidth)
  XEN_NARGIFY_1(gxm_mheight_w, gxm_mheight)
  XEN_NARGIFY_1(gxm_ndepths_w, gxm_ndepths)
  XEN_NARGIFY_1(gxm_depths_w, gxm_depths)
  XEN_NARGIFY_1(gxm_root_depth_w, gxm_root_depth)
  XEN_NARGIFY_1(gxm_root_visual_w, gxm_root_visual)
  XEN_NARGIFY_1(gxm_default_gc_w, gxm_default_gc)
  XEN_NARGIFY_1(gxm_cmap_w, gxm_cmap)
  XEN_NARGIFY_1(gxm_white_pixel_w, gxm_white_pixel)
  XEN_NARGIFY_1(gxm_black_pixel_w, gxm_black_pixel)
  XEN_NARGIFY_1(gxm_max_maps_w, gxm_max_maps)
  XEN_NARGIFY_1(gxm_min_maps_w, gxm_min_maps)
  XEN_NARGIFY_1(gxm_backing_store_w, gxm_backing_store)
  XEN_NARGIFY_1(gxm_save_unders_w, gxm_save_unders)
  XEN_NARGIFY_1(gxm_root_input_mask_w, gxm_root_input_mask)
  XEN_NARGIFY_1(gxm_type_w, gxm_type)
  XEN_NARGIFY_1(gxm_serial_w, gxm_serial)
  XEN_NARGIFY_1(gxm_send_event_w, gxm_send_event)
  XEN_NARGIFY_1(gxm_window_w, gxm_window)
  XEN_NARGIFY_1(gxm_subwindow_w, gxm_subwindow)
  XEN_NARGIFY_1(gxm_time_w, gxm_time)
  XEN_NARGIFY_1(gxm_x_root_w, gxm_x_root)
  XEN_NARGIFY_1(gxm_y_root_w, gxm_y_root)
  XEN_NARGIFY_1(gxm_state_w, gxm_state)
  XEN_NARGIFY_1(gxm_keycode_w, gxm_keycode)
  XEN_NARGIFY_1(gxm_same_screen_w, gxm_same_screen)
  XEN_NARGIFY_1(gxm_button_w, gxm_button)
  XEN_NARGIFY_1(gxm_is_hint_w, gxm_is_hint)
  XEN_NARGIFY_1(gxm_mode_w, gxm_mode)
  XEN_NARGIFY_1(gxm_detail_w, gxm_detail)
  XEN_NARGIFY_1(gxm_focus_w, gxm_focus)
  XEN_NARGIFY_1(gxm_key_vector_w, gxm_key_vector)
  XEN_NARGIFY_1(gxm_count_w, gxm_count)
  XEN_NARGIFY_1(gxm_drawable_w, gxm_drawable)
  XEN_NARGIFY_1(gxm_major_code_w, gxm_major_code)
  XEN_NARGIFY_1(gxm_minor_code_w, gxm_minor_code)
  XEN_NARGIFY_1(gxm_parent_w, gxm_parent)
  XEN_NARGIFY_1(gxm_border_width_w, gxm_border_width)
  XEN_NARGIFY_1(gxm_override_redirect_w, gxm_override_redirect)
  XEN_NARGIFY_1(gxm_event_w, gxm_event)
  XEN_NARGIFY_1(gxm_from_configure_w, gxm_from_configure)
  XEN_NARGIFY_1(gxm_above_w, gxm_above)
  XEN_NARGIFY_1(gxm_value_mask_w, gxm_value_mask)
  XEN_NARGIFY_1(gxm_place_w, gxm_place)
  XEN_NARGIFY_1(gxm_atom_w, gxm_atom)
  XEN_NARGIFY_1(gxm_selection_w, gxm_selection)
  XEN_NARGIFY_1(gxm_owner_w, gxm_owner)
  XEN_NARGIFY_1(gxm_requestor_w, gxm_requestor)
  XEN_NARGIFY_1(gxm_target_w, gxm_target)
  XEN_NARGIFY_1(gxm_property_w, gxm_property)
  XEN_NARGIFY_1(gxm_new_w, gxm_new)
  XEN_NARGIFY_1(gxm_message_type_w, gxm_message_type)
  XEN_NARGIFY_1(gxm_format_w, gxm_format)
  XEN_NARGIFY_1(gxm_request_w, gxm_request)
  XEN_NARGIFY_1(gxm_first_keycode_w, gxm_first_keycode)
  XEN_NARGIFY_1(gxm_resourceid_w, gxm_resourceid)
  XEN_NARGIFY_1(gxm_error_code_w, gxm_error_code)
  XEN_NARGIFY_1(gxm_request_code_w, gxm_request_code)
  XEN_NARGIFY_1(gxm_lbearing_w, gxm_lbearing)
  XEN_NARGIFY_1(gxm_rbearing_w, gxm_rbearing)
  XEN_NARGIFY_1(gxm_ascent_w, gxm_ascent)
  XEN_NARGIFY_1(gxm_descent_w, gxm_descent)
  XEN_NARGIFY_1(gxm_attributes_w, gxm_attributes)
  XEN_NARGIFY_1(gxm_card32_w, gxm_card32)
  XEN_NARGIFY_1(gxm_fid_w, gxm_fid)
  XEN_NARGIFY_1(gxm_properties_w, gxm_properties)
  XEN_NARGIFY_1(gxm_min_bounds_w, gxm_min_bounds)
  XEN_NARGIFY_1(gxm_max_bounds_w, gxm_max_bounds)
  XEN_NARGIFY_1(gxm_per_char_w, gxm_per_char)
  XEN_NARGIFY_1(gxm_input_w, gxm_input)
  XEN_NARGIFY_1(gxm_initial_state_w, gxm_initial_state)
  XEN_NARGIFY_1(gxm_icon_pixmap_w, gxm_icon_pixmap)
  XEN_NARGIFY_1(gxm_icon_window_w, gxm_icon_window)
  XEN_NARGIFY_1(gxm_icon_x_w, gxm_icon_x)
  XEN_NARGIFY_1(gxm_icon_y_w, gxm_icon_y)
  XEN_NARGIFY_1(gxm_icon_mask_w, gxm_icon_mask)
  XEN_NARGIFY_1(gxm_window_group_w, gxm_window_group)
  XEN_NARGIFY_1(gxm_visualid_w, gxm_visualid)
  XEN_NARGIFY_1(gxm_class_w, gxm_class)
  XEN_NARGIFY_1(gxm_red_mask_w, gxm_red_mask)
  XEN_NARGIFY_1(gxm_green_mask_w, gxm_green_mask)
  XEN_NARGIFY_1(gxm_blue_mask_w, gxm_blue_mask)
  XEN_NARGIFY_1(gxm_bits_per_rgb_w, gxm_bits_per_rgb)
  XEN_NARGIFY_1(gxm_map_entries_w, gxm_map_entries)
  XEN_NARGIFY_1(gxm_colormap_size_w, gxm_colormap_size)
  XEN_NARGIFY_1(gxm_nvisuals_w, gxm_nvisuals)
  XEN_NARGIFY_1(gxm_visuals_w, gxm_visuals)
  XEN_NARGIFY_1(gxm_bits_per_pixel_w, gxm_bits_per_pixel)
  XEN_NARGIFY_1(gxm_background_pixmap_w, gxm_background_pixmap)
  XEN_NARGIFY_1(gxm_background_pixel_w, gxm_background_pixel)
  XEN_NARGIFY_1(gxm_border_pixmap_w, gxm_border_pixmap)
  XEN_NARGIFY_1(gxm_border_pixel_w, gxm_border_pixel)
  XEN_NARGIFY_1(gxm_bit_gravity_w, gxm_bit_gravity)
  XEN_NARGIFY_1(gxm_win_gravity_w, gxm_win_gravity)
  XEN_NARGIFY_1(gxm_backing_planes_w, gxm_backing_planes)
  XEN_NARGIFY_1(gxm_backing_pixel_w, gxm_backing_pixel)
  XEN_NARGIFY_1(gxm_save_under_w, gxm_save_under)
  XEN_NARGIFY_1(gxm_event_mask_w, gxm_event_mask)
  XEN_NARGIFY_1(gxm_do_not_propagate_mask_w, gxm_do_not_propagate_mask)
  XEN_NARGIFY_1(gxm_cursor_w, gxm_cursor)
  XEN_NARGIFY_1(gxm_map_installed_w, gxm_map_installed)
  XEN_NARGIFY_1(gxm_map_state_w, gxm_map_state)
  XEN_NARGIFY_1(gxm_all_event_masks_w, gxm_all_event_masks)
  XEN_NARGIFY_1(gxm_your_event_mask_w, gxm_your_event_mask)
  XEN_NARGIFY_1(gxm_screen_w, gxm_screen)
  XEN_NARGIFY_1(gxm_xoffset_w, gxm_xoffset)
  XEN_NARGIFY_1(gxm_byte_order_w, gxm_byte_order)
  XEN_NARGIFY_1(gxm_bitmap_unit_w, gxm_bitmap_unit)
  XEN_NARGIFY_1(gxm_bitmap_bit_order_w, gxm_bitmap_bit_order)
  XEN_NARGIFY_1(gxm_bitmap_pad_w, gxm_bitmap_pad)
  XEN_NARGIFY_1(gxm_bytes_per_line_w, gxm_bytes_per_line)
  XEN_NARGIFY_1(gxm_obdata_w, gxm_obdata)
  XEN_NARGIFY_1(gxm_sibling_w, gxm_sibling)
  XEN_NARGIFY_1(gxm_stack_mode_w, gxm_stack_mode)
 
  XEN_NARGIFY_1(gxm_red_max_w, gxm_red_max)
  XEN_NARGIFY_1(gxm_red_mult_w, gxm_red_mult)
  XEN_NARGIFY_1(gxm_green_max_w, gxm_green_max)
  XEN_NARGIFY_1(gxm_green_mult_w, gxm_green_mult)
  XEN_NARGIFY_1(gxm_blue_max_w, gxm_blue_max)
  XEN_NARGIFY_1(gxm_blue_mult_w, gxm_blue_mult)
  XEN_NARGIFY_1(gxm_base_pixel_w, gxm_base_pixel)
  XEN_NARGIFY_1(gxm_killid_w, gxm_killid)
  XEN_NARGIFY_1(gxm_data_w, gxm_data)

  XEN_NARGIFY_2(gxm_set_request_code_w, gxm_set_request_code)
  XEN_NARGIFY_2(gxm_set_error_code_w, gxm_set_error_code)
  XEN_NARGIFY_2(gxm_set_first_keycode_w, gxm_set_first_keycode)
  XEN_NARGIFY_2(gxm_set_request_w, gxm_set_request)
  XEN_NARGIFY_2(gxm_set_resourceid_w, gxm_set_resourceid)
  XEN_NARGIFY_2(gxm_set_format_w, gxm_set_format)
  XEN_NARGIFY_2(gxm_set_message_type_w, gxm_set_message_type)
  XEN_NARGIFY_2(gxm_set_new_w, gxm_set_new)
  XEN_NARGIFY_2(gxm_set_property_w, gxm_set_property)
  XEN_NARGIFY_2(gxm_set_display_w, gxm_set_display)
  XEN_NARGIFY_2(gxm_set_target_w, gxm_set_target)
  XEN_NARGIFY_2(gxm_set_requestor_w, gxm_set_requestor)
  XEN_NARGIFY_2(gxm_set_owner_w, gxm_set_owner)
  XEN_NARGIFY_2(gxm_set_selection_w, gxm_set_selection)
  XEN_NARGIFY_2(gxm_set_atom_w, gxm_set_atom)
  XEN_NARGIFY_2(gxm_set_place_w, gxm_set_place)
  XEN_NARGIFY_2(gxm_set_value_mask_w, gxm_set_value_mask)
  XEN_NARGIFY_2(gxm_set_above_w, gxm_set_above)
  XEN_NARGIFY_2(gxm_set_from_configure_w, gxm_set_from_configure)
  XEN_NARGIFY_2(gxm_set_event_w, gxm_set_event)
  XEN_NARGIFY_2(gxm_set_override_redirect_w, gxm_set_override_redirect)
  XEN_NARGIFY_2(gxm_set_border_width_w, gxm_set_border_width)
  XEN_NARGIFY_2(gxm_set_parent_w, gxm_set_parent)
  XEN_NARGIFY_2(gxm_set_minor_code_w, gxm_set_minor_code)
  XEN_NARGIFY_2(gxm_set_major_code_w, gxm_set_major_code)
  XEN_NARGIFY_2(gxm_set_drawable_w, gxm_set_drawable)
  XEN_NARGIFY_2(gxm_set_count_w, gxm_set_count)
  XEN_NARGIFY_2(gxm_set_key_vector_w, gxm_set_key_vector)
  XEN_NARGIFY_2(gxm_set_focus_w, gxm_set_focus)
  XEN_NARGIFY_2(gxm_set_detail_w, gxm_set_detail)
  XEN_NARGIFY_2(gxm_set_mode_w, gxm_set_mode)
  XEN_NARGIFY_2(gxm_set_is_hint_w, gxm_set_is_hint)
  XEN_NARGIFY_2(gxm_set_button_w, gxm_set_button)
  XEN_NARGIFY_2(gxm_set_same_screen_w, gxm_set_same_screen)
  XEN_NARGIFY_2(gxm_set_keycode_w, gxm_set_keycode)
  XEN_NARGIFY_2(gxm_set_state_w, gxm_set_state)
  XEN_NARGIFY_2(gxm_set_y_root_w, gxm_set_y_root)
  XEN_NARGIFY_2(gxm_set_x_root_w, gxm_set_x_root)
  XEN_NARGIFY_2(gxm_set_root_w, gxm_set_root)
  XEN_NARGIFY_2(gxm_set_time_w, gxm_set_time)
  XEN_NARGIFY_2(gxm_set_subwindow_w, gxm_set_subwindow)
  XEN_NARGIFY_2(gxm_set_window_w, gxm_set_window)
  XEN_NARGIFY_2(gxm_set_send_event_w, gxm_set_send_event)
  XEN_NARGIFY_2(gxm_set_serial_w, gxm_set_serial)
  XEN_NARGIFY_2(gxm_set_type_w, gxm_set_type)

  XEN_NARGIFY_1(gxm_min_height_w, gxm_min_height)
  XEN_NARGIFY_1(gxm_max_height_w, gxm_max_height)
  XEN_NARGIFY_1(gxm_min_width_w, gxm_min_width)
  XEN_NARGIFY_1(gxm_max_width_w, gxm_max_width)
  XEN_NARGIFY_1(gxm_height_inc_w, gxm_height_inc)
  XEN_NARGIFY_1(gxm_width_inc_w, gxm_width_inc)

#if HAVE_MOTIF
#if MOTIF_2
  XEN_NARGIFY_1(gxm_page_number_w, gxm_page_number)
  XEN_NARGIFY_1(gxm_page_widget_w, gxm_page_widget)
  XEN_NARGIFY_1(gxm_status_area_widget_w, gxm_status_area_widget)
  XEN_NARGIFY_1(gxm_major_tab_widget_w, gxm_major_tab_widget)
  XEN_NARGIFY_1(gxm_minor_tab_widget_w, gxm_minor_tab_widget)
  XEN_NARGIFY_1(gxm_source_data_w, gxm_source_data)
  XEN_NARGIFY_1(gxm_location_data_w, gxm_location_data)
  XEN_NARGIFY_1(gxm_parm_w, gxm_parm)
  XEN_NARGIFY_1(gxm_parm_format_w, gxm_parm_format)
  XEN_NARGIFY_1(gxm_parm_length_w, gxm_parm_length)
  XEN_NARGIFY_1(gxm_parm_type_w, gxm_parm_type)
  XEN_NARGIFY_1(gxm_transfer_id_w, gxm_transfer_id)
  XEN_NARGIFY_1(gxm_destination_data_w, gxm_destination_data)
  XEN_NARGIFY_1(gxm_remaining_w, gxm_remaining)
  XEN_NARGIFY_1(gxm_item_or_text_w, gxm_item_or_text)
  XEN_NARGIFY_1(gxm_auto_selection_type_w, gxm_auto_selection_type)
  XEN_NARGIFY_1(gxm_new_outline_state_w, gxm_new_outline_state)
  XEN_NARGIFY_1(gxm_prev_page_number_w, gxm_prev_page_number)
  XEN_NARGIFY_1(gxm_prev_page_widget_w, gxm_prev_page_widget)
  XEN_NARGIFY_1(gxm_rendition_w, gxm_rendition)
  XEN_NARGIFY_1(gxm_render_table_w, gxm_render_table)
#if HAVE_XP
  XEN_NARGIFY_1(gxm_last_page_w, gxm_last_page)
#endif
  XEN_NARGIFY_1(gxm_crossed_boundary_w, gxm_crossed_boundary)
  XEN_NARGIFY_1(gxm_client_data_w, gxm_client_data)
  XEN_NARGIFY_1(gxm_status_w, gxm_status)
  XEN_NARGIFY_1(gxm_font_name_w, gxm_font_name)
  XEN_NARGIFY_1(gxm_tag_w, gxm_tag)
  XEN_NARGIFY_1(gxm_traversal_destination_w, gxm_traversal_destination)
  XEN_NARGIFY_1(gxm_dragProtocolStyle_w, gxm_dragProtocolStyle)
  XEN_NARGIFY_1(gxm_direction_w, gxm_direction)
#endif
  XEN_NARGIFY_1(gxm_reason_w, gxm_reason)
  XEN_NARGIFY_1(gxm_timeStamp_w, gxm_timeStamp)
  XEN_NARGIFY_1(gxm_operation_w, gxm_operation )
  XEN_NARGIFY_2(gxm_set_operation_w, gxm_set_operation)
  XEN_NARGIFY_1(gxm_operations_w, gxm_operations)
  XEN_NARGIFY_1(gxm_dropSiteStatus_w, gxm_dropSiteStatus )
  XEN_NARGIFY_2(gxm_set_dropSiteStatus_w, gxm_set_dropSiteStatus)
  XEN_NARGIFY_1(gxm_dropAction_w, gxm_dropAction)
  XEN_NARGIFY_1(gxm_iccHandle_w, gxm_iccHandle)
  XEN_NARGIFY_1(gxm_completionStatus_w, gxm_completionStatus)
  XEN_NARGIFY_1(gxm_dragContext_w, gxm_dragContext)
  XEN_NARGIFY_1(gxm_animate_w, gxm_animate)
  XEN_NARGIFY_1(gxm_length_w, gxm_length)
  XEN_NARGIFY_1(gxm_click_count_w, gxm_click_count)
  XEN_NARGIFY_1(gxm_widget_w, gxm_widget)
  XEN_NARGIFY_1(gxm_item_position_w, gxm_item_position)
  XEN_NARGIFY_1(gxm_callbackstruct_w, gxm_callbackstruct)
  XEN_NARGIFY_1(gxm_set_w, gxm_set)
  XEN_NARGIFY_1(gxm_item_w, gxm_item)
  XEN_NARGIFY_1(gxm_item_length_w, gxm_item_length)
  XEN_NARGIFY_1(gxm_selected_items_w, gxm_selected_items)
  XEN_NARGIFY_1(gxm_selected_item_count_w, gxm_selected_item_count)
  XEN_NARGIFY_1(gxm_selected_item_positions_w, gxm_selected_item_positions)
  XEN_NARGIFY_1(gxm_selection_type_w, gxm_selection_type)
  XEN_NARGIFY_1(gxm_mask_w, gxm_mask)
  XEN_NARGIFY_1(gxm_mask_length_w, gxm_mask_length)
  XEN_NARGIFY_1(gxm_dir_w, gxm_dir)
  XEN_NARGIFY_1(gxm_dir_length_w, gxm_dir_length)
  XEN_NARGIFY_1(gxm_pattern_w, gxm_pattern)
  XEN_NARGIFY_1(gxm_pattern_length_w, gxm_pattern_length)
#if MOTIF_2
  XEN_NARGIFY_1(gxm_position_w, gxm_position)
#endif
  XEN_NARGIFY_1(gxm_currInsert_w, gxm_currInsert)
  XEN_NARGIFY_1(gxm_newInsert_w, gxm_newInsert)
  XEN_NARGIFY_1(gxm_startPos_w, gxm_startPos)
  XEN_NARGIFY_1(gxm_endPos_w, gxm_endPos)
  XEN_NARGIFY_1(gxm_text_w, gxm_text)
  XEN_NARGIFY_1(gxm_value_w, gxm_value)
  XEN_NARGIFY_2(gxm_set_value_w, gxm_set_value)
  XEN_NARGIFY_1(gxm_doit_w, gxm_doit)
  XEN_NARGIFY_2(gxm_set_doit_w, gxm_set_doit)
  XEN_NARGIFY_1(gxm_colormap_w, gxm_colormap)
  XEN_NARGIFY_2(gxm_set_colormap_w, gxm_set_colormap)
#if MOTIF_2
  XEN_NARGIFY_1(gxm_menuToPost_w, gxm_menuToPost)
  XEN_NARGIFY_2(gxm_set_menuToPost_w, gxm_set_menuToPost)
  XEN_NARGIFY_1(gxm_postIt_w, gxm_postIt)
  XEN_NARGIFY_2(gxm_set_postIt_w, gxm_set_postIt)
#endif

#if HAVE_XPM
  XEN_NARGIFY_1(gxm_valuemask_w, gxm_valuemask)
  XEN_NARGIFY_2(gxm_set_valuemask_w, gxm_set_valuemask)
  XEN_NARGIFY_1(gxm_ncolors_w, gxm_ncolors)
  XEN_NARGIFY_2(gxm_set_ncolors_w, gxm_set_ncolors)
  XEN_NARGIFY_1(gxm_cpp_w, gxm_cpp)
  XEN_NARGIFY_2(gxm_set_cpp_w, gxm_set_cpp)
  XEN_NARGIFY_1(gxm_numsymbols_w, gxm_numsymbols)
  XEN_NARGIFY_2(gxm_set_numsymbols_w, gxm_set_numsymbols)
  XEN_NARGIFY_1(gxm_colorsymbols_w, gxm_colorsymbols)
  XEN_NARGIFY_2(gxm_set_colorsymbols_w, gxm_set_colorsymbols)
  XEN_NARGIFY_1(gxm_npixels_w, gxm_npixels)
  XEN_NARGIFY_2(gxm_set_npixels_w, gxm_set_npixels)
  XEN_NARGIFY_1(gxm_y_hotspot_w, gxm_y_hotspot)
  XEN_NARGIFY_2(gxm_set_y_hotspot_w, gxm_set_y_hotspot)
  XEN_NARGIFY_1(gxm_x_hotspot_w, gxm_x_hotspot)
  XEN_NARGIFY_2(gxm_set_x_hotspot_w, gxm_set_x_hotspot)

  XEN_NARGIFY_5(gxm_XpmImage_w, gxm_XpmImage)
  XEN_NARGIFY_3(gxm_XpmColorSymbol_w, gxm_XpmColorSymbol)
  XEN_NARGIFY_0(gxm_XpmAttributes_w, gxm_XpmAttributes)
#endif

#endif


static void define_structs(void)
{
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "pixel" XM_POSTFIX, gxm_pixel_w, "", 
				   XM_PREFIX "set_pixel" XM_POSTFIX, gxm_set_pixel_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "red" XM_POSTFIX, gxm_red_w, "", 
				   XM_PREFIX "set_red" XM_POSTFIX, gxm_set_red_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "green" XM_POSTFIX, gxm_green_w, "", 
				   XM_PREFIX "set_green" XM_POSTFIX, gxm_set_green_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "blue" XM_POSTFIX, gxm_blue_w, "", 
				   XM_PREFIX "set_blue" XM_POSTFIX, gxm_set_blue_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "flags" XM_POSTFIX, gxm_flags_w, "", 
				   XM_PREFIX "set_flags" XM_POSTFIX, gxm_set_flags_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "pad" XM_POSTFIX, gxm_pad_w, "", 
				   XM_PREFIX "set_pad" XM_POSTFIX, gxm_set_pad_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XColor" XM_POSTFIX, gxm_XColor_w, 0, 6, 0, NULL);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "x" XM_POSTFIX, gxm_x_w, "", 
				   XM_PREFIX "set_x" XM_POSTFIX, gxm_set_x_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "y" XM_POSTFIX, gxm_y_w, "", 
				   XM_PREFIX "set_y" XM_POSTFIX, gxm_set_y_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "width" XM_POSTFIX, gxm_width_w, "", 
				   XM_PREFIX "set_width" XM_POSTFIX, gxm_set_width_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "height" XM_POSTFIX, gxm_height_w, "", 
				   XM_PREFIX "set_height" XM_POSTFIX, gxm_set_height_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "angle1" XM_POSTFIX, gxm_angle1_w, "", 
				   XM_PREFIX "set_angle1" XM_POSTFIX, gxm_set_angle1_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "angle2" XM_POSTFIX, gxm_angle2_w, "", 
				   XM_PREFIX "set_angle2" XM_POSTFIX, gxm_set_angle2_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XArc" XM_POSTFIX, gxm_XArc_w, 6, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XPoint" XM_POSTFIX, gxm_XPoint_w, 2, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "x1" XM_POSTFIX, gxm_x1_w, "", 
				   XM_PREFIX "set_x1" XM_POSTFIX, gxm_set_x1_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "y1" XM_POSTFIX, gxm_y1_w, "", 
				   XM_PREFIX "set_y1" XM_POSTFIX, gxm_set_y1_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "x2" XM_POSTFIX, gxm_x2_w, "", 
				   XM_PREFIX "set_x2" XM_POSTFIX, gxm_set_x2_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "y2" XM_POSTFIX, gxm_y2_w, "", 
				   XM_PREFIX "set_y2" XM_POSTFIX, gxm_set_y2_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XSegment" XM_POSTFIX, gxm_XSegment_w, 4, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XRectangle" XM_POSTFIX, gxm_XRectangle_w, 4, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "->string" XM_POSTFIX, gxm_to_s_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "dashes" XM_POSTFIX, gxm_dashes_w, "",  
				   XM_PREFIX "set_dashes" XM_POSTFIX, gxm_set_dashes_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "dash_offset" XM_POSTFIX, gxm_dash_offset_w, "", 
				   XM_PREFIX "set_dash_offset" XM_POSTFIX, gxm_set_dash_offset_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "clip_mask" XM_POSTFIX, gxm_clip_mask_w, "", 
				   XM_PREFIX "set_clip_mask" XM_POSTFIX, gxm_set_clip_mask_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "clip_y_origin" XM_POSTFIX, gxm_clip_y_origin_w, "", 
				   XM_PREFIX "set_clip_y_origin" XM_POSTFIX, gxm_set_clip_y_origin_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "clip_x_origin" XM_POSTFIX, gxm_clip_x_origin_w, "", 
				   XM_PREFIX "set_clip_x_origin" XM_POSTFIX, gxm_set_clip_x_origin_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "graphics_exposures" XM_POSTFIX, gxm_graphics_exposures_w, "", 
				   XM_PREFIX "set_graphics_exposures" XM_POSTFIX, gxm_set_graphics_exposures_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "subwindow_mode" XM_POSTFIX, gxm_subwindow_mode_w, "", 
				   XM_PREFIX "set_subwindow_mode" XM_POSTFIX, gxm_set_subwindow_mode_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "font" XM_POSTFIX, gxm_font_w, "",  
				   XM_PREFIX "set_font" XM_POSTFIX, gxm_set_font_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "ts_y_origin" XM_POSTFIX, gxm_ts_y_origin_w, "", 
				   XM_PREFIX "set_ts_y_origin" XM_POSTFIX, gxm_set_ts_y_origin_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "ts_x_origin" XM_POSTFIX, gxm_ts_x_origin_w, "", 
				   XM_PREFIX "set_ts_x_origin" XM_POSTFIX, gxm_set_ts_x_origin_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "stipple" XM_POSTFIX, gxm_stipple_w, "", 
				   XM_PREFIX "set_stipple" XM_POSTFIX, gxm_set_stipple_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "tile" XM_POSTFIX, gxm_tile_w, "", 
				   XM_PREFIX "set_tile" XM_POSTFIX, gxm_set_tile_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "arc_mode" XM_POSTFIX, gxm_arc_mode_w, "", 
				   XM_PREFIX "set_arc_mode" XM_POSTFIX, gxm_set_arc_mode_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "fill_rule" XM_POSTFIX, gxm_fill_rule_w, "", 
				   XM_PREFIX "set_fill_rule" XM_POSTFIX, gxm_set_fill_rule_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "fill_style" XM_POSTFIX, gxm_fill_style_w, "", 
				   XM_PREFIX "set_fill_style" XM_POSTFIX, gxm_set_fill_style_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "join_style" XM_POSTFIX, gxm_join_style_w, "", 
				   XM_PREFIX "set_join_style" XM_POSTFIX, gxm_set_join_style_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "cap_style" XM_POSTFIX, gxm_cap_style_w, "", 
				   XM_PREFIX "set_cap_style" XM_POSTFIX, gxm_set_cap_style_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "line_style" XM_POSTFIX, gxm_line_style_w, "", 
				   XM_PREFIX "set_line_style" XM_POSTFIX, gxm_set_line_style_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "line_width" XM_POSTFIX, gxm_line_width_w, "", 
				   XM_PREFIX "set_line_width" XM_POSTFIX, gxm_set_line_width_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "background" XM_POSTFIX, gxm_background_w, "", 
				   XM_PREFIX "set_background" XM_POSTFIX, gxm_set_background_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "foreground" XM_POSTFIX, gxm_foreground_w, "", 
				   XM_PREFIX "set_foreground" XM_POSTFIX, gxm_set_foreground_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "plane_mask" XM_POSTFIX, gxm_plane_mask_w, "", 
				   XM_PREFIX "set_plane_mask" XM_POSTFIX, gxm_set_plane_mask_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "function" XM_POSTFIX, gxm_function_w, "", 
				   XM_PREFIX "set_function" XM_POSTFIX, gxm_set_function_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "delta" XM_POSTFIX, gxm_delta_w, "", 
				   XM_PREFIX "set_delta" XM_POSTFIX, gxm_set_delta_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "nchars" XM_POSTFIX, gxm_nchars_w, "", 
				   XM_PREFIX "set_nchars" XM_POSTFIX, gxm_set_nchars_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "chars" XM_POSTFIX, gxm_chars_w, "", 
				   XM_PREFIX "set_chars" XM_POSTFIX, gxm_set_chars_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XTextItem" XM_POSTFIX, gxm_XTextItem_w, 4, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "name" XM_POSTFIX, gxm_name_w, "", 
				   XM_PREFIX "set_name" XM_POSTFIX, gxm_set_name_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "depth" XM_POSTFIX, gxm_depth_w, "", 
				   XM_PREFIX "set_depth" XM_POSTFIX, gxm_set_depth_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "visual" XM_POSTFIX, gxm_visual_w, "", 
				   XM_PREFIX "set_visual" XM_POSTFIX, gxm_set_visual_w,  1, 0, 2, 0);

  XEN_DEFINE_PROCEDURE(XM_PREFIX "mwidth" XM_POSTFIX, gxm_mwidth_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "mheight" XM_POSTFIX, gxm_mheight_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "ndepths" XM_POSTFIX, gxm_ndepths_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "depths" XM_POSTFIX, gxm_depths_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "root_depth" XM_POSTFIX, gxm_root_depth_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "root_visual" XM_POSTFIX, gxm_root_visual_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "default_gc" XM_POSTFIX, gxm_default_gc_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "cmap" XM_POSTFIX, gxm_cmap_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "white_pixel" XM_POSTFIX, gxm_white_pixel_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "black_pixel" XM_POSTFIX, gxm_black_pixel_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "max_maps" XM_POSTFIX, gxm_max_maps_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "min_maps" XM_POSTFIX, gxm_min_maps_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "backing_store" XM_POSTFIX, gxm_backing_store_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "save_unders" XM_POSTFIX, gxm_save_unders_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "root_input_mask" XM_POSTFIX, gxm_root_input_mask_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "lbearing" XM_POSTFIX, gxm_lbearing_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "rbearing" XM_POSTFIX, gxm_rbearing_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "ascent" XM_POSTFIX, gxm_ascent_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "descent" XM_POSTFIX, gxm_descent_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "attributes" XM_POSTFIX, gxm_attributes_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "card32" XM_POSTFIX, gxm_card32_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "fid" XM_POSTFIX, gxm_fid_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "properties" XM_POSTFIX, gxm_properties_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "min_bounds" XM_POSTFIX, gxm_min_bounds_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "max_bounds" XM_POSTFIX, gxm_max_bounds_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "per_char" XM_POSTFIX, gxm_per_char_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "input" XM_POSTFIX, gxm_input_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "initial_state" XM_POSTFIX, gxm_initial_state_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "icon_pixmap" XM_POSTFIX, gxm_icon_pixmap_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "icon_window" XM_POSTFIX, gxm_icon_window_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "icon_x" XM_POSTFIX, gxm_icon_x_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "icon_y" XM_POSTFIX, gxm_icon_y_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "icon_mask" XM_POSTFIX, gxm_icon_mask_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "window_group" XM_POSTFIX, gxm_window_group_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "visualid" XM_POSTFIX, gxm_visualid_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "class" XM_POSTFIX, gxm_class_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "red_mask" XM_POSTFIX, gxm_red_mask_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "green_mask" XM_POSTFIX, gxm_green_mask_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "blue_mask" XM_POSTFIX, gxm_blue_mask_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "bits_per_rgb" XM_POSTFIX, gxm_bits_per_rgb_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "map_entries" XM_POSTFIX, gxm_map_entries_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "colormap_size" XM_POSTFIX, gxm_colormap_size_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "nvisuals" XM_POSTFIX, gxm_nvisuals_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "visuals" XM_POSTFIX, gxm_visuals_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "bits_per_pixel" XM_POSTFIX, gxm_bits_per_pixel_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "background_pixmap" XM_POSTFIX, gxm_background_pixmap_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "background_pixel" XM_POSTFIX, gxm_background_pixel_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "border_pixmap" XM_POSTFIX, gxm_border_pixmap_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "border_pixel" XM_POSTFIX, gxm_border_pixel_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "bit_gravity" XM_POSTFIX, gxm_bit_gravity_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "win_gravity" XM_POSTFIX, gxm_win_gravity_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "backing_planes" XM_POSTFIX, gxm_backing_planes_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "backing_pixel" XM_POSTFIX, gxm_backing_pixel_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "save_under" XM_POSTFIX, gxm_save_under_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "event_mask" XM_POSTFIX, gxm_event_mask_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "do_not_propagate_mask" XM_POSTFIX, gxm_do_not_propagate_mask_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "cursor" XM_POSTFIX, gxm_cursor_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "map_installed" XM_POSTFIX, gxm_map_installed_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "map_state" XM_POSTFIX, gxm_map_state_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "all_event_masks" XM_POSTFIX, gxm_all_event_masks_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "your_event_mask" XM_POSTFIX, gxm_your_event_mask_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "screen" XM_POSTFIX, gxm_screen_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "xoffset" XM_POSTFIX, gxm_xoffset_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "byte_order" XM_POSTFIX, gxm_byte_order_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "bitmap_unit" XM_POSTFIX, gxm_bitmap_unit_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "bitmap_bit_order" XM_POSTFIX, gxm_bitmap_bit_order_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "bitmap_pad" XM_POSTFIX, gxm_bitmap_pad_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "bytes_per_line" XM_POSTFIX, gxm_bytes_per_line_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "obdata" XM_POSTFIX, gxm_obdata_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "sibling" XM_POSTFIX, gxm_sibling_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "stack_mode" XM_POSTFIX, gxm_stack_mode_w, 1, 0, 0, NULL);
 
  XEN_DEFINE_PROCEDURE(XM_PREFIX "red_max" XM_POSTFIX, gxm_red_max_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "red_mult" XM_POSTFIX, gxm_red_mult_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "green_max" XM_POSTFIX, gxm_green_max_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "green_mult" XM_POSTFIX, gxm_green_mult_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "blue_max" XM_POSTFIX, gxm_blue_max_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "blue_mult" XM_POSTFIX, gxm_blue_mult_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "base_pixel" XM_POSTFIX, gxm_base_pixel_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "killid" XM_POSTFIX, gxm_killid_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "data" XM_POSTFIX, gxm_data_w, 1, 0, 0, NULL);

  XEN_DEFINE_PROCEDURE(XM_PREFIX "min_height" XM_POSTFIX, gxm_min_height_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "max_height" XM_POSTFIX, gxm_max_height_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "min_width" XM_POSTFIX, gxm_min_width_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "max_width" XM_POSTFIX, gxm_max_width_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "height_inc" XM_POSTFIX, gxm_height_inc_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "width_inc" XM_POSTFIX, gxm_width_inc_w, 1, 0, 0, NULL);

#if HAVE_MOTIF
#if MOTIF_2
  XEN_DEFINE_PROCEDURE(XM_PREFIX "page_number" XM_POSTFIX, gxm_page_number_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "page_widget" XM_POSTFIX, gxm_page_widget_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "status_area_widget" XM_POSTFIX, gxm_status_area_widget_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "major_tab_widget" XM_POSTFIX, gxm_major_tab_widget_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "minor_tab_widget" XM_POSTFIX, gxm_minor_tab_widget_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "source_data" XM_POSTFIX, gxm_source_data_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "location_data" XM_POSTFIX, gxm_location_data_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "parm" XM_POSTFIX, gxm_parm_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "parm_format" XM_POSTFIX, gxm_parm_format_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "parm_length" XM_POSTFIX, gxm_parm_length_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "parm_type" XM_POSTFIX, gxm_parm_type_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "transfer_id" XM_POSTFIX, gxm_transfer_id_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "destination_data" XM_POSTFIX, gxm_destination_data_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "remaining" XM_POSTFIX, gxm_remaining_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "item_or_text" XM_POSTFIX, gxm_item_or_text_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "auto_selection_type" XM_POSTFIX, gxm_auto_selection_type_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "new_outline_state" XM_POSTFIX, gxm_new_outline_state_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "prev_page_number" XM_POSTFIX, gxm_prev_page_number_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "prev_page_widget" XM_POSTFIX, gxm_prev_page_widget_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "rendition" XM_POSTFIX, gxm_rendition_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "render_table" XM_POSTFIX, gxm_render_table_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "last_page" XM_POSTFIX, gxm_last_page_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "crossed_boundary" XM_POSTFIX, gxm_crossed_boundary_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "client_data" XM_POSTFIX, gxm_client_data_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "status" XM_POSTFIX, gxm_status_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "font_name" XM_POSTFIX, gxm_font_name_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "tag" XM_POSTFIX, gxm_tag_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "traversal_destination" XM_POSTFIX, gxm_traversal_destination_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "dragProtocolStyle" XM_POSTFIX, gxm_dragProtocolStyle_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "direction" XM_POSTFIX, gxm_direction_w, 1, 0, 0, NULL);
#endif
  XEN_DEFINE_PROCEDURE(XM_PREFIX "reason" XM_POSTFIX, gxm_reason_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "timeStamp" XM_POSTFIX, gxm_timeStamp_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "operation" XM_POSTFIX, gxm_operation_w, "", 
				   XM_PREFIX "set_operation" XM_POSTFIX, gxm_set_operation_w, 1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "operations" XM_POSTFIX, gxm_operations_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "dropSiteStatus" XM_POSTFIX, gxm_dropSiteStatus_w, "",
				   XM_PREFIX "set_dropSiteStatus" XM_POSTFIX, gxm_set_dropSiteStatus_w, 1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "dropAction" XM_POSTFIX, gxm_dropAction_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "iccHandle" XM_POSTFIX, gxm_iccHandle_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "completionStatus" XM_POSTFIX, gxm_completionStatus_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "dragContext" XM_POSTFIX, gxm_dragContext_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "animate" XM_POSTFIX, gxm_animate_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "length" XM_POSTFIX, gxm_length_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "click_count" XM_POSTFIX, gxm_click_count_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "widget" XM_POSTFIX, gxm_widget_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "item_position" XM_POSTFIX, gxm_item_position_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "callbackstruct" XM_POSTFIX, gxm_callbackstruct_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "set" XM_POSTFIX, gxm_set_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "item" XM_POSTFIX, gxm_item_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "item_length" XM_POSTFIX, gxm_item_length_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "selected_items" XM_POSTFIX, gxm_selected_items_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "selected_item_count" XM_POSTFIX, gxm_selected_item_count_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "selected_item_positions" XM_POSTFIX, gxm_selected_item_positions_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "selection_type" XM_POSTFIX, gxm_selection_type_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "mask" XM_POSTFIX, gxm_mask_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "mask_length" XM_POSTFIX, gxm_mask_length_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "dir" XM_POSTFIX, gxm_dir_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "dir_length" XM_POSTFIX, gxm_dir_length_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "pattern" XM_POSTFIX, gxm_pattern_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "pattern_length" XM_POSTFIX, gxm_pattern_length_w, 1, 0, 0, NULL);
#if MOTIF_2
  XEN_DEFINE_PROCEDURE(XM_PREFIX "position" XM_POSTFIX, gxm_position_w, 1, 0, 0, NULL);
#endif
  XEN_DEFINE_PROCEDURE(XM_PREFIX "currInsert" XM_POSTFIX, gxm_currInsert_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "newInsert" XM_POSTFIX, gxm_newInsert_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "startPos" XM_POSTFIX, gxm_startPos_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "endPos" XM_POSTFIX, gxm_endPos_w, 1, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "text" XM_POSTFIX, gxm_text_w, 1, 0, 0, NULL);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "request_code" XM_POSTFIX, gxm_request_code_w, "", 
				   XM_PREFIX "set_request_code" XM_POSTFIX, gxm_set_request_code_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "error_code" XM_POSTFIX, gxm_error_code_w, "", 
				   XM_PREFIX "set_error_code" XM_POSTFIX, gxm_set_error_code_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "first_keycode" XM_POSTFIX, gxm_first_keycode_w, "", 
				   XM_PREFIX "set_first_keycode" XM_POSTFIX, gxm_set_first_keycode_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "request" XM_POSTFIX, gxm_request_w, "", 
				   XM_PREFIX "set_request" XM_POSTFIX, gxm_set_request_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "resourceid" XM_POSTFIX, gxm_resourceid_w, "", 
				   XM_PREFIX "set_resourceid" XM_POSTFIX, gxm_set_resourceid_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "format" XM_POSTFIX, gxm_format_w, "", 
				   XM_PREFIX "set_format" XM_POSTFIX, gxm_set_format_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "message_type" XM_POSTFIX, gxm_message_type_w, "", 
				   XM_PREFIX "set_message_type" XM_POSTFIX, gxm_set_message_type_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "new" XM_POSTFIX, gxm_new_w, "", 
				   XM_PREFIX "set_new" XM_POSTFIX, gxm_set_new_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "property" XM_POSTFIX, gxm_property_w, "", 
				   XM_PREFIX "set_property" XM_POSTFIX, gxm_set_property_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "display" XM_POSTFIX, gxm_display_w, "", 
				   XM_PREFIX "set_display" XM_POSTFIX, gxm_set_display_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "target" XM_POSTFIX, gxm_target_w, "", 
				   XM_PREFIX "set_target" XM_POSTFIX, gxm_set_target_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "requestor" XM_POSTFIX, gxm_requestor_w, "", 
				   XM_PREFIX "set_requestor" XM_POSTFIX, gxm_set_requestor_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "owner" XM_POSTFIX, gxm_owner_w, "", 
				   XM_PREFIX "set_owner" XM_POSTFIX, gxm_set_owner_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "selection" XM_POSTFIX, gxm_selection_w, "", 
				   XM_PREFIX "set_selection" XM_POSTFIX, gxm_set_selection_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "atom" XM_POSTFIX, gxm_atom_w, "", 
				   XM_PREFIX "set_atom" XM_POSTFIX, gxm_set_atom_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "place" XM_POSTFIX, gxm_place_w, "", 
				   XM_PREFIX "set_place" XM_POSTFIX, gxm_set_place_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "value_mask" XM_POSTFIX, gxm_value_mask_w, "", 
				   XM_PREFIX "set_value_mask" XM_POSTFIX, gxm_set_value_mask_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "above" XM_POSTFIX, gxm_above_w, "", 
				   XM_PREFIX "set_above" XM_POSTFIX, gxm_set_above_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "from_configure" XM_POSTFIX, gxm_from_configure_w, "", 
				   XM_PREFIX "set_from_configure" XM_POSTFIX, gxm_set_from_configure_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "event" XM_POSTFIX, gxm_event_w, "", 
				   XM_PREFIX "set_event" XM_POSTFIX, gxm_set_event_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "override_redirect" XM_POSTFIX, gxm_override_redirect_w, "", 
				   XM_PREFIX "set_override_redirect" XM_POSTFIX, gxm_set_override_redirect_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "border_width" XM_POSTFIX, gxm_border_width_w, "", 
				   XM_PREFIX "set_border_width" XM_POSTFIX, gxm_set_border_width_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "parent" XM_POSTFIX, gxm_parent_w, "", 
				   XM_PREFIX "set_parent" XM_POSTFIX, gxm_set_parent_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "minor_code" XM_POSTFIX, gxm_minor_code_w, "", 
				   XM_PREFIX "set_minor_code" XM_POSTFIX, gxm_set_minor_code_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "major_code" XM_POSTFIX, gxm_major_code_w, "", 
				   XM_PREFIX "set_major_code" XM_POSTFIX, gxm_set_major_code_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "drawable" XM_POSTFIX, gxm_drawable_w, "", 
				   XM_PREFIX "set_drawable" XM_POSTFIX, gxm_set_drawable_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "count" XM_POSTFIX, gxm_count_w, "", 
				   XM_PREFIX "set_count" XM_POSTFIX, gxm_set_count_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "key_vector" XM_POSTFIX, gxm_key_vector_w, "", 
				   XM_PREFIX "set_key_vector" XM_POSTFIX, gxm_set_key_vector_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "focus" XM_POSTFIX, gxm_focus_w, "", 
				   XM_PREFIX "set_focus" XM_POSTFIX, gxm_set_focus_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "detail" XM_POSTFIX, gxm_detail_w, "", 
				   XM_PREFIX "set_detail" XM_POSTFIX, gxm_set_detail_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "mode" XM_POSTFIX, gxm_mode_w, "", 
				   XM_PREFIX "set_mode" XM_POSTFIX, gxm_set_mode_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "is_hint" XM_POSTFIX, gxm_is_hint_w, "", 
				   XM_PREFIX "set_is_hint" XM_POSTFIX, gxm_set_is_hint_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "button" XM_POSTFIX, gxm_button_w, "", 
				   XM_PREFIX "set_button" XM_POSTFIX, gxm_set_button_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "same_screen" XM_POSTFIX, gxm_same_screen_w, "", 
				   XM_PREFIX "set_same_screen" XM_POSTFIX, gxm_set_same_screen_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "keycode" XM_POSTFIX, gxm_keycode_w, "", 
				   XM_PREFIX "set_keycode" XM_POSTFIX, gxm_set_keycode_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "state" XM_POSTFIX, gxm_state_w, "", 
				   XM_PREFIX "set_state" XM_POSTFIX, gxm_set_state_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "y_root" XM_POSTFIX, gxm_y_root_w, "", 
				   XM_PREFIX "set_y_root" XM_POSTFIX, gxm_set_y_root_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "x_root" XM_POSTFIX, gxm_x_root_w, "", 
				   XM_PREFIX "set_x_root" XM_POSTFIX, gxm_set_x_root_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "root" XM_POSTFIX, gxm_root_w, "", 
				   XM_PREFIX "set_root" XM_POSTFIX, gxm_set_root_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "time" XM_POSTFIX, gxm_time_w, "", 
				   XM_PREFIX "set_time" XM_POSTFIX, gxm_set_time_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "subwindow" XM_POSTFIX, gxm_subwindow_w, "", 
				   XM_PREFIX "set_subwindow" XM_POSTFIX, gxm_set_subwindow_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "window" XM_POSTFIX, gxm_window_w, "", 
				   XM_PREFIX "set_window" XM_POSTFIX, gxm_set_window_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "send_event" XM_POSTFIX, gxm_send_event_w, "", 
				   XM_PREFIX "set_send_event" XM_POSTFIX, gxm_set_send_event_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "serial" XM_POSTFIX, gxm_serial_w, "", 
				   XM_PREFIX "set_serial" XM_POSTFIX, gxm_set_serial_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "type" XM_POSTFIX, gxm_type_w, "", 
				   XM_PREFIX "set_type" XM_POSTFIX, gxm_set_type_w,  1, 0, 2, 0); 


  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "value" XM_POSTFIX, gxm_value_w, "", 
				   XM_PREFIX "set_value" XM_POSTFIX, gxm_set_value_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "doit" XM_POSTFIX, gxm_doit_w, "", 
				   XM_PREFIX "set_doit" XM_POSTFIX, gxm_set_doit_w,  1, 0, 2, 0); 
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "colormap" XM_POSTFIX, gxm_colormap_w, "", 
				   XM_PREFIX "set_colormap" XM_POSTFIX, gxm_set_colormap_w, 1, 0, 2, 0);
#if MOTIF_2
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "menuToPost" XM_POSTFIX, gxm_menuToPost_w, "", 
				   XM_PREFIX "set_menuToPost", gxm_set_menuToPost_w, 1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "postIt" XM_POSTFIX, gxm_postIt_w, "", 
				   XM_PREFIX "set_postIt" XM_POSTFIX, gxm_set_postIt_w, 1, 0, 2, 0);
#endif

#if HAVE_XPM
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "valuemask" XM_POSTFIX, gxm_valuemask_w, "", 
				   XM_PREFIX "set_valuemask" XM_POSTFIX, gxm_set_valuemask_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "ncolors" XM_POSTFIX, gxm_ncolors_w, "", 
				   XM_PREFIX "set_ncolors" XM_POSTFIX, gxm_set_ncolors_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "cpp" XM_POSTFIX, gxm_cpp_w, "", 
				   XM_PREFIX "set_cpp" XM_POSTFIX, gxm_set_cpp_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpmImage" XM_POSTFIX, gxm_XpmImage_w, 5, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "numsymbols" XM_POSTFIX, gxm_numsymbols_w, "", 
				   XM_PREFIX "set_numsymbols" XM_POSTFIX, gxm_set_numsymbols_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "colorsymbols" XM_POSTFIX, gxm_colorsymbols_w, "", 
				   XM_PREFIX "set_colorsymbols" XM_POSTFIX, gxm_set_colorsymbols_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "npixels" XM_POSTFIX, gxm_npixels_w, "", 
				   XM_PREFIX "set_npixels" XM_POSTFIX, gxm_set_npixels_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "y_hotspot" XM_POSTFIX, gxm_y_hotspot_w, "", 
				   XM_PREFIX "set_y_hotspot" XM_POSTFIX, gxm_set_y_hotspot_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_PREFIX "x_hotspot" XM_POSTFIX, gxm_x_hotspot_w, "", 
				   XM_PREFIX "set_x_hotspot" XM_POSTFIX, gxm_set_x_hotspot_w,  1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpmColorSymbol" XM_POSTFIX, gxm_XpmColorSymbol_w, 3, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE(XM_PREFIX "XpmAttributes" XM_POSTFIX, gxm_XpmAttributes_w, 0, 0, 0, NULL);
#endif

#endif

}

