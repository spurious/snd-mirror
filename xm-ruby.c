/* this is so ugly I can't bear to include it in xm.c
 */

#if HAVE_XM_XP
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
#if HAVE_MOTIF
#if (!XM_DISABLE_DEPRECATED)
  XEN_NARGIFY_1(gxm_XtWarning_w, gxm_XtWarning)
  XEN_NARGIFY_2(gxm_XtAppWarning_w, gxm_XtAppWarning)
  XEN_NARGIFY_1(gxm_XtSetWarningMsgHandler_w, gxm_XtSetWarningMsgHandler)
  XEN_NARGIFY_4(gxm_XtInitialize_w, gxm_XtInitialize)
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
  XEN_NARGIFY_3(gxm_XtSetArg_w, gxm_XtSetArg)
  XEN_ARGIFY_2(gxm_XtManageChildren_w, gxm_XtManageChildren)
  XEN_NARGIFY_1(gxm_XtManageChild_w, gxm_XtManageChild)
  XEN_ARGIFY_2(gxm_XtUnmanageChildren_w, gxm_XtUnmanageChildren)
  XEN_NARGIFY_1(gxm_XtUnmanageChild_w, gxm_XtUnmanageChild)
  XEN_NARGIFY_1(gxm_XtDispatchEvent_w, gxm_XtDispatchEvent)
  XEN_NARGIFY_2(gxm_XtCallAcceptFocus_w, gxm_XtCallAcceptFocus)
  XEN_NARGIFY_1(gxm_XtAppPeekEvent_w, gxm_XtAppPeekEvent)
#if MOTIF_2
  XEN_NARGIFY_2(gxm_XtIsSubclass_w, gxm_XtIsSubclass)
#endif
  XEN_NARGIFY_2(gxm_XtAppSetFallbackResources_w, gxm_XtAppSetFallbackResources)
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
  XEN_ARGIFY_5(gxm_XtCallActionProc_w, gxm_XtCallActionProc)
  XEN_NARGIFY_5(gxm_XtRegisterGrabAction_w, gxm_XtRegisterGrabAction)
  XEN_NARGIFY_2(gxm_XtSetMultiClickTime_w, gxm_XtSetMultiClickTime)
  XEN_NARGIFY_1(gxm_XtGetMultiClickTime_w, gxm_XtGetMultiClickTime)
  XEN_NARGIFY_1(gxm_XtGetResourceList_w, gxm_XtGetResourceList)
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
  XEN_NARGIFY_6(gxm_XtDisplayInitialize_w, gxm_XtDisplayInitialize)
  XEN_ARGIFY_6(gxm_XtOpenApplication_w, gxm_XtOpenApplication)
  XEN_ARGIFY_6(gxm_XtVaOpenApplication_w, gxm_XtVaOpenApplication)
  XEN_ARGIFY_5(gxm_XtAppInitialize_w, gxm_XtAppInitialize)
  XEN_ARGIFY_5(gxm_XtVaAppInitialize_w, gxm_XtVaAppInitialize)
  XEN_NARGIFY_6(gxm_XtOpenDisplay_w, gxm_XtOpenDisplay)
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
  XEN_ARGIFY_8(gxm_XChangeProperty_w, gxm_XChangeProperty)
  XEN_NARGIFY_4(gxm_XChangeWindowAttributes_w, gxm_XChangeWindowAttributes)
  XEN_NARGIFY_3(gxm_XCheckIfEvent_w, gxm_XCheckIfEvent)
  XEN_NARGIFY_2(gxm_XCheckMaskEvent_w, gxm_XCheckMaskEvent)
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
  XEN_NARGIFY_1(gxm_Vector2XPoints_w, gxm_Vector2XPoints)
  XEN_NARGIFY_5(gxm_XDrawPoint_w, gxm_XDrawPoint)
  XEN_NARGIFY_6(gxm_XDrawPoints_w, gxm_XDrawPoints)
  XEN_NARGIFY_7(gxm_XDrawRectangle_w, gxm_XDrawRectangle)
  XEN_NARGIFY_5(gxm_XDrawRectangles_w, gxm_XDrawRectangles)
  XEN_NARGIFY_5(gxm_XDrawSegments_w, gxm_XDrawSegments)
  XEN_NARGIFY_7(gxm_XDrawString_w, gxm_XDrawString)
  XEN_ARGIFY_7(gxm_XDrawText_w, gxm_XDrawText)
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
  XEN_VARGIFY(gxm_XGrabButton_w, gxm_XGrabButton)
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
  XEN_ARGIFY_4(gxm_XQueryColors_w, gxm_XQueryColors)
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
  XEN_ARGIFY_3(gxm_XSetPointerMapping_w, gxm_XSetPointerMapping)
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
#if (!XM_DISABLE_DEPRECATED)
  XEN_NARGIFY_3(gxm_XGetStandardColormap_w, gxm_XGetStandardColormap)
#endif
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
#if (!XM_DISABLE_DEPRECATED)
  XEN_NARGIFY_4(gxm_XSetStandardColormap_w, gxm_XSetStandardColormap)
  XEN_NARGIFY_8(gxm_XSetStandardProperties_w, gxm_XSetStandardProperties)
#endif
  XEN_NARGIFY_8(gxm_XSetWMProperties_w, gxm_XSetWMProperties)
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

  XEN_NARGIFY_1(XEN_XButtonEvent_p_w, XEN_XButtonEvent_p)
  XEN_NARGIFY_1(XEN_XCirculateEvent_p_w, XEN_XCirculateEvent_p)
  XEN_NARGIFY_1(XEN_XCirculateRequestEvent_p_w, XEN_XCirculateRequestEvent_p)
  XEN_NARGIFY_1(XEN_XClientMessageEvent_p_w, XEN_XClientMessageEvent_p)
  XEN_NARGIFY_1(XEN_XColormapEvent_p_w, XEN_XColormapEvent_p)
  XEN_NARGIFY_1(XEN_XConfigureEvent_p_w, XEN_XConfigureEvent_p)
  XEN_NARGIFY_1(XEN_XConfigureRequestEvent_p_w, XEN_XConfigureRequestEvent_p)
  XEN_NARGIFY_1(XEN_XCreateWindowEvent_p_w, XEN_XCreateWindowEvent_p)
  XEN_NARGIFY_1(XEN_XCrossingEvent_p_w, XEN_XCrossingEvent_p)
  XEN_NARGIFY_1(XEN_XDestroyWindowEvent_p_w, XEN_XDestroyWindowEvent_p)
  XEN_NARGIFY_1(XEN_XErrorEvent_p_w, XEN_XErrorEvent_p)
  XEN_NARGIFY_1(XEN_XExposeEvent_p_w, XEN_XExposeEvent_p)
  XEN_NARGIFY_1(XEN_XFocusChangeEvent_p_w, XEN_XFocusChangeEvent_p)
  XEN_NARGIFY_1(XEN_XGraphicsExposeEvent_p_w, XEN_XGraphicsExposeEvent_p)
  XEN_NARGIFY_1(XEN_XGravityEvent_p_w, XEN_XGravityEvent_p)
  XEN_NARGIFY_1(XEN_XKeyEvent_p_w, XEN_XKeyEvent_p)
  XEN_NARGIFY_1(XEN_XKeymapEvent_p_w, XEN_XKeymapEvent_p)
  XEN_NARGIFY_1(XEN_XMapEvent_p_w, XEN_XMapEvent_p)
  XEN_NARGIFY_1(XEN_XMapRequestEvent_p_w, XEN_XMapRequestEvent_p)
  XEN_NARGIFY_1(XEN_XMappingEvent_p_w, XEN_XMappingEvent_p)
  XEN_NARGIFY_1(XEN_XMotionEvent_p_w, XEN_XMotionEvent_p)
  XEN_NARGIFY_1(XEN_XNoExposeEvent_p_w, XEN_XNoExposeEvent_p)
  XEN_NARGIFY_1(XEN_XPropertyEvent_p_w, XEN_XPropertyEvent_p)
  XEN_NARGIFY_1(XEN_XReparentEvent_p_w, XEN_XReparentEvent_p)
  XEN_NARGIFY_1(XEN_XResizeRequestEvent_p_w, XEN_XResizeRequestEvent_p)
  XEN_NARGIFY_1(XEN_XSelectionClearEvent_p_w, XEN_XSelectionClearEvent_p)
  XEN_NARGIFY_1(XEN_XSelectionEvent_p_w, XEN_XSelectionEvent_p)
  XEN_NARGIFY_1(XEN_XSelectionRequestEvent_p_w, XEN_XSelectionRequestEvent_p)
  XEN_NARGIFY_1(XEN_XSetWindowAttributes_p_w, XEN_XSetWindowAttributes_p)
  XEN_NARGIFY_1(XEN_XUnmapEvent_p_w, XEN_XUnmapEvent_p)
  XEN_NARGIFY_1(XEN_XVisibilityEvent_p_w, XEN_XVisibilityEvent_p)
  XEN_NARGIFY_1(XEN_XIconSize_p_w, XEN_XIconSize_p)

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
#if HAVE_XM_XP
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
#if (!XM_DISABLE_DEPRECATED)
  XEN_NARGIFY_2(gxm_XmAddToPostFromList_w, gxm_XmAddToPostFromList)
  XEN_NARGIFY_2(gxm_XmRemoveFromPostFromList_w, gxm_XmRemoveFromPostFromList)
#endif
  XEN_NARGIFY_2(gxm_XmScaleSetValue_w, gxm_XmScaleSetValue)
  XEN_NARGIFY_1(gxm_XmScaleGetValue_w, gxm_XmScaleGetValue)
  XEN_ARGIFY_4(gxm_XmCreateScale_w, gxm_XmCreateScale)
#ifndef LESSTIF_VERSION
  XEN_NARGIFY_5(gxm_XmClipboardBeginCopy_w, gxm_XmClipboardBeginCopy)
#endif
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
#if MOTIF_2
#if HAVE_XmToolTipGetLabel
  XEN_NARGIFY_1(gxm_XmToolTipGetLabel_w, gxm_XmToolTipGetLabel)
#endif
#ifndef LESSTIF_VERSION
  XEN_NARGIFY_1(gxm_XmGetXmScreen_w, gxm_XmGetXmScreen)
#endif
#endif
#if HAVE_XmCreateFontSelector
    XEN_ARGIFY_4(gxm_XmCreateFontSelector_w, gxm_XmCreateFontSelector)
#endif
#if HAVE_XmCreateColorSelector
    XEN_ARGIFY_4(gxm_XmCreateColorSelector_w, gxm_XmCreateColorSelector)
#endif
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
#if MOTIF_2
  XEN_NARGIFY_2(gxm_XmTextFieldSetAddMode_w, gxm_XmTextFieldSetAddMode)
  XEN_NARGIFY_1(gxm_XmTextFieldGetAddMode_w, gxm_XmTextFieldGetAddMode)
#endif
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
#if MOTIF_2
  XEN_NARGIFY_2(gxm_XmTextSetAddMode_w, gxm_XmTextSetAddMode)
  XEN_NARGIFY_1(gxm_XmTextGetAddMode_w, gxm_XmTextGetAddMode)
#endif
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
  XEN_ARGIFY_2(gxm_XmParseTableFree_w, gxm_XmParseTableFree)
  XEN_NARGIFY_5(gxm_XmStringTableProposeTablist_w, gxm_XmStringTableProposeTablist)
  XEN_NARGIFY_2(gxm_XmTabSetValue_w, gxm_XmTabSetValue)
  XEN_NARGIFY_1(gxm_XmTabGetValues_w, gxm_XmTabGetValues)
  XEN_NARGIFY_1(gxm_XmTabFree_w, gxm_XmTabFree)
  XEN_NARGIFY_1(gxm_XmTabListFree_w, gxm_XmTabListFree)
  XEN_NARGIFY_5(gxm_XmTabCreate_w, gxm_XmTabCreate)
  XEN_NARGIFY_1(gxm_XmTabListTabCount_w, gxm_XmTabListTabCount)
  XEN_ARGIFY_3(gxm_XmTabListRemoveTabs_w, gxm_XmTabListRemoveTabs)
  XEN_ARGIFY_4(gxm_XmTabListReplacePositions_w, gxm_XmTabListReplacePositions)
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
  XEN_NARGIFY_1(gxm_XmCvtXmStringToByteStream_w, gxm_XmCvtXmStringToByteStream)
  XEN_NARGIFY_1(gxm_XmCvtByteStreamToXmString_w, gxm_XmCvtByteStreamToXmString)
  XEN_NARGIFY_1(gxm_XmStringByteStreamLength_w, gxm_XmStringByteStreamLength)

  XEN_NARGIFY_1(gxm_XmIsNotebook_w, gxm_XmIsNotebook)
#if HAVE_XM_XP
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
#if MOTIF_2
  XEN_NARGIFY_1(gxm_XmIsScreen_w, gxm_XmIsScreen)
#endif
  XEN_NARGIFY_1(gxm_XmIsScrollBar_w, gxm_XmIsScrollBar)
  XEN_NARGIFY_1(gxm_XmIsDialogShell_w, gxm_XmIsDialogShell)
  XEN_NARGIFY_1(gxm_XmIsScrolledWindow_w, gxm_XmIsScrolledWindow)
  XEN_NARGIFY_1(gxm_XmIsDisplay_w, gxm_XmIsDisplay)
  XEN_NARGIFY_1(gxm_XmIsSelectionBox_w, gxm_XmIsSelectionBox)
  XEN_NARGIFY_1(gxm_XmIsDragContext_w, gxm_XmIsDragContext)
  XEN_NARGIFY_1(gxm_XmIsSeparatorGadget_w, gxm_XmIsSeparatorGadget)
#if MOTIF_2
  XEN_NARGIFY_1(gxm_XmIsDragIconObjectClass_w, gxm_XmIsDragIconObjectClass)
#endif
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
  XEN_NARGIFY_1(gxm_XmListGetSelectedPos_w, gxm_XmListGetSelectedPos)
  XEN_NARGIFY_1(gxm_XmWidgetGetDisplayRect_w, gxm_XmWidgetGetDisplayRect)

#if (!XM_DISABLE_DEPRECATED)
  XEN_NARGIFY_1(gxm_XmStringLength_w, gxm_XmStringLength)
  XEN_NARGIFY_2(gxm_XmStringByteCompare_w, gxm_XmStringByteCompare)
  XEN_NARGIFY_4(gxm_XmScrolledWindowSetAreas_w, gxm_XmScrolledWindowSetAreas)
  XEN_NARGIFY_1(gxm_XmFontListEntryFree_w, gxm_XmFontListEntryFree)
  XEN_NARGIFY_1(gxm_XmFontListEntryGetFont_w, gxm_XmFontListEntryGetFont)
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

  XEN_NARGIFY_1(XEN_XmFontList_p_w, XEN_XmFontList_p)
  XEN_NARGIFY_1(XEN_XmFontContext_p_w, XEN_XmFontContext_p)
  XEN_NARGIFY_1(XEN_XmFontListEntry_p_w, XEN_XmFontListEntry_p)
#endif

#endif

#if HAVE_XPM
  XEN_NARGIFY_4(gxm_XpmCreatePixmapFromData_w, gxm_XpmCreatePixmapFromData)
  XEN_NARGIFY_4(gxm_XpmCreateDataFromPixmap_w, gxm_XpmCreateDataFromPixmap)
  XEN_NARGIFY_4(gxm_XpmReadFileToPixmap_w, gxm_XpmReadFileToPixmap)
  XEN_NARGIFY_1(gxm_XpmReadFileToXpmImage_w, gxm_XpmReadFileToXpmImage)
  XEN_NARGIFY_5(gxm_XpmWriteFileFromPixmap_w, gxm_XpmWriteFileFromPixmap)
  XEN_NARGIFY_4(gxm_XpmCreatePixmapFromXpmImage_w, gxm_XpmCreatePixmapFromXpmImage)
  XEN_NARGIFY_4(gxm_XpmCreateXpmImageFromPixmap_w, gxm_XpmCreateXpmImageFromPixmap)
#if HAVE_XPM_GET_ERROR_STRING
  XEN_NARGIFY_1(gxm_XpmGetErrorString_w, gxm_XpmGetErrorString)
#endif
#endif
  XEN_NARGIFY_3(gxm_XGetPixel_w, gxm_XGetPixel)
  XEN_NARGIFY_1(gxm_XDestroyImage_w, gxm_XDestroyImage)
  XEN_NARGIFY_4(gxm_XPutPixel_w, gxm_XPutPixel)
  XEN_NARGIFY_5(gxm_XSubImage_w, gxm_XSubImage)
  XEN_NARGIFY_2(gxm_XAddPixel_w, gxm_XAddPixel)

#if HAVE_MOTIF
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
#if HAVE_MOTIF
  XEN_NARGIFY_1(XEN_Widget_p_w, XEN_Widget_p)
  XEN_NARGIFY_1(XEN_XmStringContext_p_w, XEN_XmStringContext_p)
#endif
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
  XEN_NARGIFY_1(XEN_Cursor_p_w, XEN_Cursor_p)
#if HAVE_XM_XP
  XEN_NARGIFY_1(XEN_XPContext_p_w, XEN_XPContext_p)
#endif
#if HAVE_MOTIF
  XEN_NARGIFY_1(XEN_WidgetClass_p_w, XEN_WidgetClass_p)
  XEN_NARGIFY_1(XEN_XmString_p_w, XEN_XmString_p)
#if MOTIF_2
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
#if WITH_EDITRES
  XEN_NARGIFY_4(gxm_XEditResCheckMessages_w, gxm_XEditResCheckMessages)
#endif

#if HAVE_XSHAPEQUERYEXTENSION
  XEN_NARGIFY_1(gxm_XShapeQueryExtension_w, gxm_XShapeQueryExtension)
  XEN_NARGIFY_1(gxm_XShapeQueryVersion_w, gxm_XShapeQueryVersion)
  XEN_NARGIFY_2(gxm_XShapeQueryExtents_w, gxm_XShapeQueryExtents)
  XEN_NARGIFY_3(gxm_XShapeGetRectangles_w, gxm_XShapeGetRectangles)
  XEN_NARGIFY_5(gxm_XShapeOffsetShape_w, gxm_XShapeOffsetShape)
  XEN_NARGIFY_7(gxm_XShapeCombineRegion_w, gxm_XShapeCombineRegion)
  XEN_NARGIFY_7(gxm_XShapeCombineMask_w, gxm_XShapeCombineMask)
  XEN_NARGIFY_8(gxm_XShapeCombineShape_w, gxm_XShapeCombineShape)
  XEN_NARGIFY_9(gxm_XShapeCombineRectangles_w, gxm_XShapeCombineRectangles)
#endif

static void define_procedures(void)
{
  #define XM_DEFINE_PROCEDURE(Name, Value, A1, A2, A3, Help) XEN_DEFINE_PROCEDURE(XM_PREFIX #Name XM_POSTFIX, Value, A1, A2, A3, Help)

  xm_gc_table = XEN_MAKE_VECTOR(1, XEN_FALSE);
  XEN_PROTECT_FROM_GC(xm_gc_table);
  xm_protected_size = 512;
  xm_protected = XEN_MAKE_VECTOR(xm_protected_size, XEN_FALSE);
  XEN_VECTOR_SET(xm_gc_table, 0, xm_protected);

#if HAVE_XM_XP
  XM_DEFINE_PROCEDURE(XpStartPage, gxm_XpStartPage_w, 2, 0, 0, H_XpStartPage);
  XM_DEFINE_PROCEDURE(XpEndPage, gxm_XpEndPage_w, 1, 0, 0, H_XpEndPage);
  XM_DEFINE_PROCEDURE(XpCancelPage, gxm_XpCancelPage_w, 2, 0, 0, H_XpCancelPage);
  XM_DEFINE_PROCEDURE(XpStartJob, gxm_XpStartJob_w, 2, 0, 0, H_XpStartJob);
  XM_DEFINE_PROCEDURE(XpEndJob, gxm_XpEndJob_w, 1, 0, 0, H_XpEndJob);
  XM_DEFINE_PROCEDURE(XpCancelJob, gxm_XpCancelJob_w, 2, 0, 0, H_XpCancelJob);
  XM_DEFINE_PROCEDURE(XpStartDoc, gxm_XpStartDoc_w, 2, 0, 0, H_XpStartDoc);
  XM_DEFINE_PROCEDURE(XpEndDoc, gxm_XpEndDoc_w, 1, 0, 0, H_XpEndDoc);
  XM_DEFINE_PROCEDURE(XpCancelDoc, gxm_XpCancelDoc_w, 2, 0, 0, H_XpCancelDoc);
  XM_DEFINE_PROCEDURE(XpRehashPrinterList, gxm_XpRehashPrinterList_w, 1, 0, 0, H_XpRehashPrinterList);
  XM_DEFINE_PROCEDURE(XpCreateContext, gxm_XpCreateContext_w, 2, 0, 0, H_XpCreateContext);
  XM_DEFINE_PROCEDURE(XpSetContext, gxm_XpSetContext_w, 2, 0, 0, H_XpSetContext);
  XM_DEFINE_PROCEDURE(XpGetContext, gxm_XpGetContext_w, 1, 0, 0, H_XpGetContext);
  XM_DEFINE_PROCEDURE(XpDestroyContext, gxm_XpDestroyContext_w, 2, 0, 0, H_XpDestroyContext);
  XM_DEFINE_PROCEDURE(XpGetLocaleNetString, gxm_XpGetLocaleNetString_w, 0, 0, 0, H_XpGetLocaleNetString);
  XM_DEFINE_PROCEDURE(XpNotifyPdm, gxm_XpNotifyPdm_w, 6, 0, 0, H_XpNotifyPdm);
  XM_DEFINE_PROCEDURE(XpSendAuth, gxm_XpSendAuth_w, 2, 0, 0, H_XpSendAuth);
  XM_DEFINE_PROCEDURE(XpGetImageResolution, gxm_XpGetImageResolution_w, 2, 0, 0, H_XpGetImageResolution);
  XM_DEFINE_PROCEDURE(XpGetAttributes, gxm_XpGetAttributes_w, 3, 0, 0, H_XpGetAttributes);
  XM_DEFINE_PROCEDURE(XpSetAttributes, gxm_XpSetAttributes_w, 5, 0, 0, H_XpSetAttributes);
  XM_DEFINE_PROCEDURE(XpGetOneAttribute, gxm_XpGetOneAttribute_w, 4, 0, 0, H_XpGetOneAttribute);
  XM_DEFINE_PROCEDURE(XpGetScreenOfContext, gxm_XpGetScreenOfContext_w, 2, 0, 0, H_XpGetScreenOfContext);
  XM_DEFINE_PROCEDURE(XpFreePrinterList, gxm_XpFreePrinterList_w, 1, 0, 0, H_XpFreePrinterList);
  XM_DEFINE_PROCEDURE(XpQueryVersion, gxm_XpQueryVersion_w, 1, 0, 0, H_XpQueryVersion);
  XM_DEFINE_PROCEDURE(XpQueryExtension, gxm_XpQueryExtension_w, 1, 0, 0, H_XpQueryExtension);
  XM_DEFINE_PROCEDURE(XpQueryScreens, gxm_XpQueryScreens_w, 1, 0, 0, H_XpQueryScreens);
  XM_DEFINE_PROCEDURE(XpGetPdmStartParams, gxm_XpGetPdmStartParams_w, 5, 0, 0, H_XpGetPdmStartParams);
  XM_DEFINE_PROCEDURE(XpGetAuthParams, gxm_XpGetAuthParams_w, 2, 0, 0, H_XpGetAuthParams);
  XM_DEFINE_PROCEDURE(XpSendOneTicket, gxm_XpSendOneTicket_w, 4, 0, 0, H_XpSendOneTicket);
  XM_DEFINE_PROCEDURE(XpGetPageDimensions, gxm_XpGetPageDimensions_w, 2, 0, 0, H_XpGetPageDimensions);
  XM_DEFINE_PROCEDURE(XpSetImageResolution, gxm_XpSetImageResolution_w, 4, 0, 0, H_XpSetImageResolution);
  XM_DEFINE_PROCEDURE(XpGetPrinterList, gxm_XpGetPrinterList_w, 2, 0, 0, H_XpGetPrinterList);
  XM_DEFINE_PROCEDURE(XpSelectInput, gxm_XpSelectInput_w, 3, 0, 0, H_XpSelectInput);
  XM_DEFINE_PROCEDURE(XpInputSelected, gxm_XpInputSelected_w, 3, 0, 0, H_XpInputSelected);
  XM_DEFINE_PROCEDURE(XpPutDocumentData, gxm_XpPutDocumentData_w, 6, 0, 0, H_XpPutDocumentData);
  XM_DEFINE_PROCEDURE(XpGetDocumentData, gxm_XpGetDocumentData_w, 5, 0, 0, H_XpGetDocumentData);
#endif
#if HAVE_MOTIF
  XM_DEFINE_PROCEDURE(XtSetArg, gxm_XtSetArg_w, 3, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XtManageChildren, gxm_XtManageChildren_w, 1, 1, 0, NULL);
  XM_DEFINE_PROCEDURE(XtManageChild, gxm_XtManageChild_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XtUnmanageChildren, gxm_XtUnmanageChildren_w, 1, 1, 0, NULL);
  XM_DEFINE_PROCEDURE(XtUnmanageChild, gxm_XtUnmanageChild_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XtDispatchEvent, gxm_XtDispatchEvent_w, 1, 0, 0, H_XtDispatchEvent);
  XM_DEFINE_PROCEDURE(XtCallAcceptFocus, gxm_XtCallAcceptFocus_w, 2, 0, 0, H_XtCallAcceptFocus);
  XM_DEFINE_PROCEDURE(XtAppPeekEvent, gxm_XtAppPeekEvent_w, 1, 0, 0, H_XtAppPeekEvent);
#if MOTIF_2
  XM_DEFINE_PROCEDURE(XtIsSubclass, gxm_XtIsSubclass_w, 2, 0, 0, H_XtIsSubclass);
#endif
  XM_DEFINE_PROCEDURE(XtIsObject, gxm_XtIsObject_w, 1, 0, 0, H_XtIsObject);
  XM_DEFINE_PROCEDURE(XtIsManaged, gxm_XtIsManaged_w, 1, 0, 0, H_XtIsManaged);
  XM_DEFINE_PROCEDURE(XtIsRealized, gxm_XtIsRealized_w, 1, 0, 0, H_XtIsRealized);
  XM_DEFINE_PROCEDURE(XtIsSensitive, gxm_XtIsSensitive_w, 1, 0, 0, H_XtIsSensitive);
  XM_DEFINE_PROCEDURE(XtOwnSelection, gxm_XtOwnSelection_w, 6, 0, 0, H_XtOwnSelection);
  XM_DEFINE_PROCEDURE(XtOwnSelectionIncremental, gxm_XtOwnSelectionIncremental_w, 8, 0, 0, H_XtOwnSelectionIncremental);
  XM_DEFINE_PROCEDURE(XtMakeResizeRequest, gxm_XtMakeResizeRequest_w, 3, 0, 0, H_XtMakeResizeRequest);
  XM_DEFINE_PROCEDURE(XtTranslateCoords, gxm_XtTranslateCoords_w, 3, 0, 0, H_XtTranslateCoords);
  XM_DEFINE_PROCEDURE(XtKeysymToKeycodeList, gxm_XtKeysymToKeycodeList_w, 2, 0, 0, H_XtKeysymToKeycodeList);
  XM_DEFINE_PROCEDURE(XtParseTranslationTable, gxm_XtParseTranslationTable_w, 1, 0, 0, H_XtParseTranslationTable);
  XM_DEFINE_PROCEDURE(XtParseAcceleratorTable, gxm_XtParseAcceleratorTable_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XtOverrideTranslations, gxm_XtOverrideTranslations_w, 2, 0, 0, H_XtOverrideTranslations);
  XM_DEFINE_PROCEDURE(XtAugmentTranslations, gxm_XtAugmentTranslations_w, 2, 0, 0, H_XtAugmentTranslations);
  XM_DEFINE_PROCEDURE(XtInstallAccelerators, gxm_XtInstallAccelerators_w, 2, 0, 0, H_XtInstallAccelerators);
  XM_DEFINE_PROCEDURE(XtInstallAllAccelerators, gxm_XtInstallAllAccelerators_w, 2, 0, 0, H_XtInstallAllAccelerators);
  XM_DEFINE_PROCEDURE(XtUninstallTranslations, gxm_XtUninstallTranslations_w, 1, 0, 0, H_XtUninstallTranslations);
  XM_DEFINE_PROCEDURE(XtAppAddActions, gxm_XtAppAddActions_w, 2, 0, 0, H_XtAppAddActions);
  XM_DEFINE_PROCEDURE(XtAppAddActionHook, gxm_XtAppAddActionHook_w, 2, 1, 0, H_XtAppAddActionHook);
  XM_DEFINE_PROCEDURE(XtRemoveActionHook, gxm_XtRemoveActionHook_w, 1, 0, 0, H_XtRemoveActionHook);
  XM_DEFINE_PROCEDURE(XtGetActionList, gxm_XtGetActionList_w, 1, 0, 0, H_XtGetActionList);
  XM_DEFINE_PROCEDURE(XtCallActionProc, gxm_XtCallActionProc_w, 4, 1, 0, H_XtCallActionProc);
  XM_DEFINE_PROCEDURE(XtRegisterGrabAction, gxm_XtRegisterGrabAction_w, 5, 0, 0, H_XtRegisterGrabAction);
  XM_DEFINE_PROCEDURE(XtSetMultiClickTime, gxm_XtSetMultiClickTime_w, 2, 0, 0, H_XtSetMultiClickTime);
  XM_DEFINE_PROCEDURE(XtGetMultiClickTime, gxm_XtGetMultiClickTime_w, 1, 0, 0, H_XtGetMultiClickTime);
  XM_DEFINE_PROCEDURE(XtGetResourceList, gxm_XtGetResourceList_w, 1, 0, 0, H_XtGetResourceList);
  XM_DEFINE_PROCEDURE(XtGetActionKeysym, gxm_XtGetActionKeysym_w, 1, 0, 0, H_XtGetActionKeysym);
  XM_DEFINE_PROCEDURE(XtTranslateKeycode, gxm_XtTranslateKeycode_w, 3, 0, 0, H_XtTranslateKeycode);
  XM_DEFINE_PROCEDURE(XtTranslateKey, gxm_XtTranslateKey_w, 3, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XtSetKeyTranslator, gxm_XtSetKeyTranslator_w, 2, 0, 0, H_XtSetKeyTranslator);
  XM_DEFINE_PROCEDURE(XtRegisterCaseConverter, gxm_XtRegisterCaseConverter_w, 4, 0, 0, H_XtRegisterCaseConverter);
  XM_DEFINE_PROCEDURE(XtConvertCase, gxm_XtConvertCase_w, 2, 0, 0, H_XtConvertCase);
  XM_DEFINE_PROCEDURE(XtAddEventHandler, gxm_XtAddEventHandler_w, 4, 1, 0, H_XtAddEventHandler);
  XM_DEFINE_PROCEDURE(XtRemoveEventHandler, gxm_XtRemoveEventHandler_w, 5, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XtAddRawEventHandler, gxm_XtAddRawEventHandler_w, 5, 0, 0, H_XtAddRawEventHandler);
  XM_DEFINE_PROCEDURE(XtRemoveRawEventHandler, gxm_XtRemoveRawEventHandler_w, 5, 0, 0, H_XtRemoveRawEventHandler);
  XM_DEFINE_PROCEDURE(XtInsertEventHandler, gxm_XtInsertEventHandler_w, 6, 0, 0, H_XtInsertEventHandler);
  XM_DEFINE_PROCEDURE(XtInsertRawEventHandler, gxm_XtInsertRawEventHandler_w, 6, 0, 0, H_XtInsertRawEventHandler);
  XM_DEFINE_PROCEDURE(XtDispatchEventToWidget, gxm_XtDispatchEventToWidget_w, 2, 0, 0, H_XtDispatchEventToWidget);
  XM_DEFINE_PROCEDURE(XtBuildEventMask, gxm_XtBuildEventMask_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XtAddGrab, gxm_XtAddGrab_w, 3, 0, 0, H_XtAddGrab);
  XM_DEFINE_PROCEDURE(XtRemoveGrab, gxm_XtRemoveGrab_w, 1, 0, 0, H_XtRemoveGrab);
  XM_DEFINE_PROCEDURE(XtAppProcessEvent, gxm_XtAppProcessEvent_w, 2, 0, 0, H_XtAppProcessEvent);
  XM_DEFINE_PROCEDURE(XtAppMainLoop, gxm_XtAppMainLoop_w, 1, 0, 0, H_XtAppMainLoop);
  XM_DEFINE_PROCEDURE(XtAddExposureToRegion, gxm_XtAddExposureToRegion_w, 2, 0, 0, H_XtAddExposureToRegion);
  XM_DEFINE_PROCEDURE(XtSetKeyboardFocus, gxm_XtSetKeyboardFocus_w, 2, 0, 0, H_XtSetKeyboardFocus);
  XM_DEFINE_PROCEDURE(XtGetKeyboardFocusWidget, gxm_XtGetKeyboardFocusWidget_w, 1, 0, 0, H_XtGetKeyboardFocusWidget);
  XM_DEFINE_PROCEDURE(XtLastEventProcessed, gxm_XtLastEventProcessed_w, 1, 0, 0, H_XtLastEventProcessed);
  XM_DEFINE_PROCEDURE(XtLastTimestampProcessed, gxm_XtLastTimestampProcessed_w, 1, 0, 0, H_XtLastTimestampProcessed);
  XM_DEFINE_PROCEDURE(XtAppAddTimeOut, gxm_XtAppAddTimeOut_w, 3, 1, 0, H_XtAppAddTimeOut);
  XM_DEFINE_PROCEDURE(XtRemoveTimeOut, gxm_XtRemoveTimeOut_w, 1, 0, 0, H_XtRemoveTimeOut);
  XM_DEFINE_PROCEDURE(XtAppAddInput, gxm_XtAppAddInput_w, 4, 1, 0, H_XtAppAddInput);
  XM_DEFINE_PROCEDURE(XtRemoveInput, gxm_XtRemoveInput_w, 1, 0, 0, H_XtRemoveInput);
  XM_DEFINE_PROCEDURE(XtAppNextEvent, gxm_XtAppNextEvent_w, 1, 0, 0, H_XtAppNextEvent);
  XM_DEFINE_PROCEDURE(XtAppPending, gxm_XtAppPending_w, 1, 0, 0, H_XtAppPending);
  XM_DEFINE_PROCEDURE(XtRealizeWidget, gxm_XtRealizeWidget_w, 1, 0, 0, H_XtRealizeWidget);
  XM_DEFINE_PROCEDURE(XtUnrealizeWidget, gxm_XtUnrealizeWidget_w, 1, 0, 0, H_XtUnrealizeWidget);
  XM_DEFINE_PROCEDURE(XtDestroyWidget, gxm_XtDestroyWidget_w, 1, 0, 0, H_XtDestroyWidget);
  XM_DEFINE_PROCEDURE(XtSetSensitive, gxm_XtSetSensitive_w, 2, 0, 0, H_XtSetSensitive);
  XM_DEFINE_PROCEDURE(XtNameToWidget, gxm_XtNameToWidget_w, 2, 0, 0, H_XtNameToWidget);
  XM_DEFINE_PROCEDURE(XtWindowToWidget, gxm_XtWindowToWidget_w, 2, 0, 0, H_XtWindowToWidget);
  XM_DEFINE_PROCEDURE(XtMergeArgLists, gxm_XtMergeArgLists_w, 4, 0, 0, H_XtMergeArgLists);
  XM_DEFINE_PROCEDURE(XtVaCreateArgsList, gxm_XtVaCreateArgsList_w, 2, 0, 0, H_XtVaCreateArgsList);
  XM_DEFINE_PROCEDURE(XtDisplay, gxm_XtDisplay_w, 1, 0, 0, H_XtDisplay);
  XM_DEFINE_PROCEDURE(XtDisplayOfObject, gxm_XtDisplayOfObject_w, 1, 0, 0, H_XtDisplayOfObject);
  XM_DEFINE_PROCEDURE(XtScreen, gxm_XtScreen_w, 1, 0, 0, H_XtScreen);
  XM_DEFINE_PROCEDURE(XtScreenOfObject, gxm_XtScreenOfObject_w, 1, 0, 0, H_XtScreenOfObject);
  XM_DEFINE_PROCEDURE(XtWindow, gxm_XtWindow_w, 1, 0, 0, H_XtWindow);
  XM_DEFINE_PROCEDURE(XtWindowOfObject, gxm_XtWindowOfObject_w, 1, 0, 0, H_XtWindowOfObject);
  XM_DEFINE_PROCEDURE(XtName, gxm_XtName_w, 1, 0, 0, H_XtName);
  XM_DEFINE_PROCEDURE(XtSuperclass, gxm_XtSuperclass_w, 1, 0, 0, H_XtSuperclass);
  XM_DEFINE_PROCEDURE(XtClass, gxm_XtClass_w, 1, 0, 0, H_XtClass);
  XM_DEFINE_PROCEDURE(XtParent, gxm_XtParent_w, 1, 0, 0, H_XtParent);
  XM_DEFINE_PROCEDURE(XtAddCallback, gxm_XtAddCallback_w, 3, 1, 0, H_XtAddCallback);
  XM_DEFINE_PROCEDURE(XtRemoveCallback, gxm_XtRemoveCallback_w, 3, 0, 0, H_XtRemoveCallback);
  XM_DEFINE_PROCEDURE(XtAddCallbacks, gxm_XtAddCallbacks_w, 3, 0, 0, H_XtAddCallbacks);
  XM_DEFINE_PROCEDURE(XtRemoveCallbacks, gxm_XtRemoveCallbacks_w, 3, 0, 0, H_XtRemoveCallbacks);
  XM_DEFINE_PROCEDURE(XtRemoveAllCallbacks, gxm_XtRemoveAllCallbacks_w, 2, 0, 0, H_XtRemoveAllCallbacks);
  XM_DEFINE_PROCEDURE(XtCallCallbacks, gxm_XtCallCallbacks_w, 3, 0, 0, H_XtCallCallbacks);
  XM_DEFINE_PROCEDURE(XtHasCallbacks, gxm_XtHasCallbacks_w, 2, 0, 0, H_XtHasCallbacks);
  XM_DEFINE_PROCEDURE(XtCreatePopupShell, gxm_XtCreatePopupShell_w, 4, 1, 0, H_XtCreatePopupShell);
  XM_DEFINE_PROCEDURE(XtVaCreatePopupShell, gxm_XtVaCreatePopupShell_w, 4, 0, 0, H_XtVaCreatePopupShell);
  XM_DEFINE_PROCEDURE(XtPopup, gxm_XtPopup_w, 2, 0, 0, H_XtPopup);
  XM_DEFINE_PROCEDURE(XtPopupSpringLoaded, gxm_XtPopupSpringLoaded_w, 1, 0, 0, H_XtPopupSpringLoaded);
  XM_DEFINE_PROCEDURE(XtCallbackNone, gxm_XtCallbackNone_w, 3, 0, 0, H_XtCallbackNone);
  XM_DEFINE_PROCEDURE(XtCallbackNonexclusive, gxm_XtCallbackNonexclusive_w, 3, 0, 0, H_XtCallbackNonexclusive);
  XM_DEFINE_PROCEDURE(XtCallbackExclusive, gxm_XtCallbackExclusive_w, 3, 0, 0, H_XtCallbackExclusive);
  XM_DEFINE_PROCEDURE(XtPopdown, gxm_XtPopdown_w, 1, 0, 0, H_XtPopdown);
  XM_DEFINE_PROCEDURE(XtCallbackPopdown, gxm_XtCallbackPopdown_w, 3, 0, 0, H_XtCallbackPopdown);
  XM_DEFINE_PROCEDURE(XtCreateWidget, gxm_XtCreateWidget_w, 4, 1, 0, H_XtCreateWidget);
  XM_DEFINE_PROCEDURE(XtCreateManagedWidget, gxm_XtCreateManagedWidget_w, 4, 1, 0, H_XtCreateManagedWidget);
  XM_DEFINE_PROCEDURE(XtVaCreateWidget, gxm_XtVaCreateWidget_w, 4, 0, 0, H_XtVaCreateWidget);
  XM_DEFINE_PROCEDURE(XtVaCreateManagedWidget, gxm_XtVaCreateManagedWidget_w, 4, 0, 0, H_XtVaCreateManagedWidget);
  XM_DEFINE_PROCEDURE(XtAppCreateShell, gxm_XtAppCreateShell_w, 5, 1, 0, H_XtAppCreateShell);
  XM_DEFINE_PROCEDURE(XtVaAppCreateShell, gxm_XtVaAppCreateShell_w, 5, 0, 0, H_XtVaAppCreateShell);
  XM_DEFINE_PROCEDURE(XtToolkitInitialize, gxm_XtToolkitInitialize_w, 0, 0, 0, H_XtToolkitInitialize);
  XM_DEFINE_PROCEDURE(XtSetLanguageProc, gxm_XtSetLanguageProc_w, 3, 0, 0, H_XtSetLanguageProc);
  XM_DEFINE_PROCEDURE(XtDisplayInitialize, gxm_XtDisplayInitialize_w, 6, 0, 0, H_XtDisplayInitialize);
  XM_DEFINE_PROCEDURE(XtOpenApplication, gxm_XtOpenApplication_w, 5, 1, 0, H_XtOpenApplication);
  XM_DEFINE_PROCEDURE(XtVaOpenApplication, gxm_XtVaOpenApplication_w, 5, 1, 0, H_XtVaOpenApplication);
  XM_DEFINE_PROCEDURE(XtAppInitialize, gxm_XtAppInitialize_w, 4, 1, 0, H_XtAppInitialize);
  XM_DEFINE_PROCEDURE(XtVaAppInitialize, gxm_XtVaAppInitialize_w, 4, 1, 0, H_XtVaAppInitialize);
  XM_DEFINE_PROCEDURE(XtOpenDisplay, gxm_XtOpenDisplay_w, 6, 0, 0, H_XtOpenDisplay);
  XM_DEFINE_PROCEDURE(XtCreateApplicationContext, gxm_XtCreateApplicationContext_w, 0, 0, 0, H_XtCreateApplicationContext);
  XM_DEFINE_PROCEDURE(XtDestroyApplicationContext, gxm_XtDestroyApplicationContext_w, 1, 0, 0, H_XtDestroyApplicationContext);
  XM_DEFINE_PROCEDURE(XtAppSetFallbackResources, gxm_XtAppSetFallbackResources_w, 2, 0, 0, H_XtAppSetFallbackResources);
  XM_DEFINE_PROCEDURE(XtInitializeWidgetClass, gxm_XtInitializeWidgetClass_w, 1, 0, 0, H_XtInitializeWidgetClass);
  XM_DEFINE_PROCEDURE(XtWidgetToApplicationContext, gxm_XtWidgetToApplicationContext_w, 1, 0, 0, H_XtWidgetToApplicationContext);
  XM_DEFINE_PROCEDURE(XtDisplayToApplicationContext, gxm_XtDisplayToApplicationContext_w, 1, 0, 0, H_XtDisplayToApplicationContext);
  XM_DEFINE_PROCEDURE(XtCloseDisplay, gxm_XtCloseDisplay_w, 1, 0, 0, H_XtCloseDisplay);
  XM_DEFINE_PROCEDURE(XtSetValues, gxm_XtSetValues_w, 2, 1, 0, NULL);
  XM_DEFINE_PROCEDURE(XtVaSetValues, gxm_XtVaSetValues_w, 2, 0, 0, H_XtVaSetValues);
  XM_DEFINE_PROCEDURE(XtGetValues, gxm_XtGetValues_w, 2, 1, 0, NULL);
  XM_DEFINE_PROCEDURE(XtVaGetValues, gxm_XtVaGetValues_w, 2, 0, 0, H_XtVaGetValues);
  XM_DEFINE_PROCEDURE(XtAppSetErrorMsgHandler, gxm_XtAppSetErrorMsgHandler_w, 2, 0, 0, H_XtAppSetErrorMsgHandler);
  XM_DEFINE_PROCEDURE(XtAppSetWarningMsgHandler, gxm_XtAppSetWarningMsgHandler_w, 2, 0, 0, H_XtAppSetWarningMsgHandler);
  XM_DEFINE_PROCEDURE(XtAppErrorMsg, gxm_XtAppErrorMsg_w, 7, 0, 0, H_XtAppErrorMsg);
  XM_DEFINE_PROCEDURE(XtAppWarningMsg, gxm_XtAppWarningMsg_w, 7, 0, 0, H_XtAppWarningMsg);
  XM_DEFINE_PROCEDURE(XtAppSetErrorHandler, gxm_XtAppSetErrorHandler_w, 2, 0, 0, H_XtAppSetErrorHandler);
  XM_DEFINE_PROCEDURE(XtAppSetWarningHandler, gxm_XtAppSetWarningHandler_w, 2, 0, 0, H_XtAppSetWarningHandler);
  XM_DEFINE_PROCEDURE(XtAppError, gxm_XtAppError_w, 2, 0, 0, H_XtAppError);
  XM_DEFINE_PROCEDURE(XtMalloc, gxm_XtMalloc_w, 1, 0, 0, H_XtMalloc);
  XM_DEFINE_PROCEDURE(XtCalloc, gxm_XtCalloc_w, 2, 0, 0, H_XtCalloc);
  XM_DEFINE_PROCEDURE(XtRealloc, gxm_XtRealloc_w, 2, 0, 0, H_XtRealloc);
  XM_DEFINE_PROCEDURE(XtFree, gxm_XtFree_w, 1, 0, 0, H_XtFree);
  XM_DEFINE_PROCEDURE(XtAppAddWorkProc, gxm_XtAppAddWorkProc_w, 2, 1, 0, H_XtAppAddWorkProc);
  XM_DEFINE_PROCEDURE(XtRemoveWorkProc, gxm_XtRemoveWorkProc_w, 1, 0, 0, H_XtRemoveWorkProc);
  XM_DEFINE_PROCEDURE(XtGetGC, gxm_XtGetGC_w, 3, 0, 0, H_XtGetGC);
  XM_DEFINE_PROCEDURE(XtAllocateGC, gxm_XtAllocateGC_w, 6, 0, 0, H_XtAllocateGC);
  XM_DEFINE_PROCEDURE(XtDestroyGC, gxm_XtDestroyGC_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XtReleaseGC, gxm_XtReleaseGC_w, 2, 0, 0, H_XtReleaseGC);
  XM_DEFINE_PROCEDURE(XtFindFile, gxm_XtFindFile_w, 4, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XtResolvePathname, gxm_XtResolvePathname_w, 8, 0, 0, H_XtResolvePathname);
  XM_DEFINE_PROCEDURE(XtDisownSelection, gxm_XtDisownSelection_w, 3, 0, 0, H_XtDisownSelection);
  XM_DEFINE_PROCEDURE(XtGetSelectionValue, gxm_XtGetSelectionValue_w, 6, 0, 0, H_XtGetSelectionValue);
  XM_DEFINE_PROCEDURE(XtGetSelectionValues, gxm_XtGetSelectionValues_w, 7, 0, 0, H_XtGetSelectionValues);
  XM_DEFINE_PROCEDURE(XtAppSetSelectionTimeout, gxm_XtAppSetSelectionTimeout_w, 2, 0, 0, H_XtAppSetSelectionTimeout);
  XM_DEFINE_PROCEDURE(XtAppGetSelectionTimeout, gxm_XtAppGetSelectionTimeout_w, 1, 0, 0, H_XtAppGetSelectionTimeout);
  XM_DEFINE_PROCEDURE(XtGetSelectionRequest, gxm_XtGetSelectionRequest_w, 3, 0, 0, H_XtGetSelectionRequest);
  XM_DEFINE_PROCEDURE(XtGetSelectionValueIncremental, gxm_XtGetSelectionValueIncremental_w, 6, 0, 0, H_XtGetSelectionValueIncremental);
  XM_DEFINE_PROCEDURE(XtGetSelectionValuesIncremental, gxm_XtGetSelectionValuesIncremental_w, 7, 0, 0, H_XtGetSelectionValuesIncremental);
  XM_DEFINE_PROCEDURE(XtCreateSelectionRequest, gxm_XtCreateSelectionRequest_w, 2, 0, 0, H_XtCreateSelectionRequest);
  XM_DEFINE_PROCEDURE(XtSendSelectionRequest, gxm_XtSendSelectionRequest_w, 3, 0, 0, H_XtSendSelectionRequest);
  XM_DEFINE_PROCEDURE(XtCancelSelectionRequest, gxm_XtCancelSelectionRequest_w, 2, 0, 0, H_XtCancelSelectionRequest);
  XM_DEFINE_PROCEDURE(XtGrabKey, gxm_XtGrabKey_w, 6, 0, 0, H_XtGrabKey);
  XM_DEFINE_PROCEDURE(XtUngrabKey, gxm_XtUngrabKey_w, 3, 0, 0, H_XtUngrabKey);
  XM_DEFINE_PROCEDURE(XtGrabKeyboard, gxm_XtGrabKeyboard_w, 5, 0, 0, H_XtGrabKeyboard);
  XM_DEFINE_PROCEDURE(XtUngrabKeyboard, gxm_XtUngrabKeyboard_w, 2, 0, 0, H_XtUngrabKeyboard);
  XM_DEFINE_PROCEDURE(XtGrabButton, gxm_XtGrabButton_w, 9, 0, 0, H_XtGrabButton);
  XM_DEFINE_PROCEDURE(XtUngrabButton, gxm_XtUngrabButton_w, 3, 0, 0, H_XtUngrabButton);
  XM_DEFINE_PROCEDURE(XtGrabPointer, gxm_XtGrabPointer_w, 8, 0, 0, H_XtGrabPointer);
  XM_DEFINE_PROCEDURE(XtUngrabPointer, gxm_XtUngrabPointer_w, 2, 0, 0, H_XtUngrabPointer);
  XM_DEFINE_PROCEDURE(XtGetApplicationNameAndClass, gxm_XtGetApplicationNameAndClass_w, 1, 0, 0, H_XtGetApplicationNameAndClass);
  XM_DEFINE_PROCEDURE(XtGetDisplays, gxm_XtGetDisplays_w, 1, 0, 0, H_XtGetDisplays);
  XM_DEFINE_PROCEDURE(XtToolkitThreadInitialize, gxm_XtToolkitThreadInitialize_w, 0, 0, 0, H_XtToolkitThreadInitialize);
  XM_DEFINE_PROCEDURE(XtAppLock, gxm_XtAppLock_w, 1, 0, 0, H_XtAppLock);
  XM_DEFINE_PROCEDURE(XtAppUnlock, gxm_XtAppUnlock_w, 1, 0, 0, H_XtAppUnlock);
  XM_DEFINE_PROCEDURE(XtIsRectObj, gxm_XtIsRectObj_w, 1, 0, 0, H_XtIsRectObj);
  XM_DEFINE_PROCEDURE(XtIsWidget, gxm_XtIsWidget_w, 1, 0, 0, H_XtIsWidget);
  XM_DEFINE_PROCEDURE(XtIsComposite, gxm_XtIsComposite_w, 1, 0, 0, H_XtIsComposite);
  XM_DEFINE_PROCEDURE(XtIsConstraint, gxm_XtIsConstraint_w, 1, 0, 0, H_XtIsConstraint);
  XM_DEFINE_PROCEDURE(XtIsShell, gxm_XtIsShell_w, 1, 0, 0, H_XtIsShell);
  XM_DEFINE_PROCEDURE(XtIsOverrideShell, gxm_XtIsOverrideShell_w, 1, 0, 0, H_XtIsOverrideShell);
  XM_DEFINE_PROCEDURE(XtIsWMShell, gxm_XtIsWMShell_w, 1, 0, 0, H_XtIsWMShell);
  XM_DEFINE_PROCEDURE(XtIsVendorShell, gxm_XtIsVendorShell_w, 1, 0, 0, H_XtIsVendorShell);
  XM_DEFINE_PROCEDURE(XtIsTransientShell, gxm_XtIsTransientShell_w, 1, 0, 0, H_XtIsTransientShell);
  XM_DEFINE_PROCEDURE(XtIsTopLevelShell, gxm_XtIsTopLevelShell_w, 1, 0, 0, H_XtIsTopLevelShell);
  XM_DEFINE_PROCEDURE(XtIsApplicationShell, gxm_XtIsApplicationShell_w, 1, 0, 0, H_XtIsApplicationShell);
  XM_DEFINE_PROCEDURE(XtIsSessionShell, gxm_XtIsSessionShell_w, 1, 0, 0, H_XtIsSessionShell);
  XM_DEFINE_PROCEDURE(XtMapWidget, gxm_XtMapWidget_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XtUnmapWidget, gxm_XtUnmapWidget_w, 1, 0, 0, H_XtUnmapWidget);
#endif
  XM_DEFINE_PROCEDURE(XUniqueContext, gxm_XUniqueContext_w, 0, 0, 0, H_XUniqueContext);
  XM_DEFINE_PROCEDURE(XLoadQueryFont, gxm_XLoadQueryFont_w, 2, 0, 0, H_XLoadQueryFont);
  XM_DEFINE_PROCEDURE(XQueryFont, gxm_XQueryFont_w, 2, 0, 0, H_XQueryFont);
  XM_DEFINE_PROCEDURE(XGetMotionEvents, gxm_XGetMotionEvents_w, 4, 0, 0, H_XGetMotionEvents);
  XM_DEFINE_PROCEDURE(XDeleteModifiermapEntry, gxm_XDeleteModifiermapEntry_w, 3, 0, 0, H_XDeleteModifiermapEntry);
  XM_DEFINE_PROCEDURE(XGetModifierMapping, gxm_XGetModifierMapping_w, 1, 0, 0, H_XGetModifierMapping);
  XM_DEFINE_PROCEDURE(XInsertModifiermapEntry, gxm_XInsertModifiermapEntry_w, 3, 0, 0, H_XInsertModifiermapEntry);
  XM_DEFINE_PROCEDURE(XNewModifiermap, gxm_XNewModifiermap_w, 1, 0, 0, H_XNewModifiermap);
  XM_DEFINE_PROCEDURE(XCreateImage, gxm_XCreateImage_w, 0, 0, 1, H_XCreateImage);
  XM_DEFINE_PROCEDURE(XGetImage, gxm_XGetImage_w, 8, 0, 0, H_XGetImage);
  XM_DEFINE_PROCEDURE(XGetSubImage, gxm_XGetSubImage_w, 0, 0, 1, H_XGetSubImage);
  XM_DEFINE_PROCEDURE(XOpenDisplay, gxm_XOpenDisplay_w, 1, 0, 0, H_XOpenDisplay);
  XM_DEFINE_PROCEDURE(XFetchBytes, gxm_XFetchBytes_w, 1, 0, 0, H_XFetchBytes);
  XM_DEFINE_PROCEDURE(XFetchBuffer, gxm_XFetchBuffer_w, 2, 0, 0, H_XFetchBuffer);
  XM_DEFINE_PROCEDURE(XGetAtomName, gxm_XGetAtomName_w, 2, 0, 0, H_XGetAtomName);
  XM_DEFINE_PROCEDURE(XDisplayName, gxm_XDisplayName_w, 1, 0, 0, H_XDisplayName);
  XM_DEFINE_PROCEDURE(XKeysymToString, gxm_XKeysymToString_w, 1, 0, 0, H_XKeysymToString);
  XM_DEFINE_PROCEDURE(XSynchronize, gxm_XSynchronize_w, 2, 0, 0, H_XSynchronize);
  XM_DEFINE_PROCEDURE(XSetAfterFunction, gxm_XSetAfterFunction_w, 2, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XInternAtom, gxm_XInternAtom_w, 3, 0, 0, H_XInternAtom);
  XM_DEFINE_PROCEDURE(XCopyColormapAndFree, gxm_XCopyColormapAndFree_w, 2, 0, 0, H_XCopyColormapAndFree);
  XM_DEFINE_PROCEDURE(XCreateColormap, gxm_XCreateColormap_w, 4, 0, 0, H_XCreateColormap);
  XM_DEFINE_PROCEDURE(XCreatePixmapCursor, gxm_XCreatePixmapCursor_w, 7, 0, 0, H_XCreatePixmapCursor);
  XM_DEFINE_PROCEDURE(XCreateGlyphCursor, gxm_XCreateGlyphCursor_w, 7, 0, 0, H_XCreateGlyphCursor);
  XM_DEFINE_PROCEDURE(XCreateFontCursor, gxm_XCreateFontCursor_w, 2, 0, 0, H_XCreateFontCursor);
  XM_DEFINE_PROCEDURE(XLoadFont, gxm_XLoadFont_w, 2, 0, 0, H_XLoadFont);
  XM_DEFINE_PROCEDURE(XCreateGC, gxm_XCreateGC_w, 4, 0, 0, H_XCreateGC);
  XM_DEFINE_PROCEDURE(XFlushGC, gxm_XFlushGC_w, 2, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XCreatePixmap, gxm_XCreatePixmap_w, 5, 0, 0, H_XCreatePixmap);
  XM_DEFINE_PROCEDURE(XCreateBitmapFromData, gxm_XCreateBitmapFromData_w, 5, 0, 0, H_XCreateBitmapFromData);
  XM_DEFINE_PROCEDURE(XCreatePixmapFromBitmapData, gxm_XCreatePixmapFromBitmapData_w, 8, 0, 0, H_XCreatePixmapFromBitmapData);
  XM_DEFINE_PROCEDURE(XCreateSimpleWindow, gxm_XCreateSimpleWindow_w, 9, 0, 0, H_XCreateSimpleWindow);
  XM_DEFINE_PROCEDURE(XGetSelectionOwner, gxm_XGetSelectionOwner_w, 2, 0, 0, H_XGetSelectionOwner);
  XM_DEFINE_PROCEDURE(XCreateWindow, gxm_XCreateWindow_w, 0, 0, 1, H_XCreateWindow);
  XM_DEFINE_PROCEDURE(XListInstalledColormaps, gxm_XListInstalledColormaps_w, 2, 0, 0, H_XListInstalledColormaps);
  XM_DEFINE_PROCEDURE(XListFonts, gxm_XListFonts_w, 3, 0, 0, H_XListFonts);
  XM_DEFINE_PROCEDURE(XListFontsWithInfo, gxm_XListFontsWithInfo_w, 3, 0, 0, H_XListFontsWithInfo);
  XM_DEFINE_PROCEDURE(XGetFontPath, gxm_XGetFontPath_w, 1, 0, 0, H_XGetFontPath);
  XM_DEFINE_PROCEDURE(XListExtensions, gxm_XListExtensions_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XListProperties, gxm_XListProperties_w, 2, 0, 0, H_XListProperties);
  XM_DEFINE_PROCEDURE(XKeycodeToKeysym, gxm_XKeycodeToKeysym_w, 3, 0, 0, H_XKeycodeToKeysym);
  XM_DEFINE_PROCEDURE(XLookupKeysym, gxm_XLookupKeysym_w, 2, 0, 0, H_XLookupKeysym);
  XM_DEFINE_PROCEDURE(XGetKeyboardMapping, gxm_XGetKeyboardMapping_w, 3, 0, 0, H_XGetKeyboardMapping);
  XM_DEFINE_PROCEDURE(XStringToKeysym, gxm_XStringToKeysym_w, 1, 0, 0, H_XStringToKeysym);
  XM_DEFINE_PROCEDURE(XMaxRequestSize, gxm_XMaxRequestSize_w, 1, 0, 0, H_XMaxRequestSize);
  XM_DEFINE_PROCEDURE(XExtendedMaxRequestSize, gxm_XExtendedMaxRequestSize_w, 1, 0, 0, H_XExtendedMaxRequestSize);
  XM_DEFINE_PROCEDURE(XDisplayMotionBufferSize, gxm_XDisplayMotionBufferSize_w, 1, 0, 0, H_XDisplayMotionBufferSize);
  XM_DEFINE_PROCEDURE(XVisualIDFromVisual, gxm_XVisualIDFromVisual_w, 1, 0, 0, H_XVisualIDFromVisual);
  XM_DEFINE_PROCEDURE(XRootWindow, gxm_XRootWindow_w, 2, 0, 0, H_RootWindow);
  XM_DEFINE_PROCEDURE(XDefaultRootWindow, gxm_XDefaultRootWindow_w, 1, 0, 0, H_DefaultRootWindow);
  XM_DEFINE_PROCEDURE(XRootWindowOfScreen, gxm_XRootWindowOfScreen_w, 1, 0, 0, H_RootWindowOfScreen);
  XM_DEFINE_PROCEDURE(XDefaultVisual, gxm_XDefaultVisual_w, 2, 0, 0, H_DefaultVisual);
  XM_DEFINE_PROCEDURE(XDefaultVisualOfScreen, gxm_XDefaultVisualOfScreen_w, 1, 0, 0, H_DefaultVisualOfScreen);
  XM_DEFINE_PROCEDURE(XDefaultGC, gxm_XDefaultGC_w, 2, 0, 0, H_DefaultGC);
  XM_DEFINE_PROCEDURE(XDefaultGCOfScreen, gxm_XDefaultGCOfScreen_w, 1, 0, 0, H_DefaultGCOfScreen);
  XM_DEFINE_PROCEDURE(XBlackPixel, gxm_XBlackPixel_w, 2, 0, 0, H_BlackPixel);
  XM_DEFINE_PROCEDURE(XWhitePixel, gxm_XWhitePixel_w, 2, 0, 0, H_WhitePixel);
  XM_DEFINE_PROCEDURE(XAllPlanes, gxm_XAllPlanes_w, 0, 0, 0, H_AllPlanes);
  XM_DEFINE_PROCEDURE(XBlackPixelOfScreen, gxm_XBlackPixelOfScreen_w, 1, 0, 0, H_BlackPixelOfScreen);
  XM_DEFINE_PROCEDURE(XWhitePixelOfScreen, gxm_XWhitePixelOfScreen_w, 1, 0, 0, H_WhitePixelOfScreen);
  XM_DEFINE_PROCEDURE(XNextRequest, gxm_XNextRequest_w, 1, 0, 0, H_NextRequest);
  XM_DEFINE_PROCEDURE(XLastKnownRequestProcessed, gxm_XLastKnownRequestProcessed_w, 1, 0, 0, H_LastKnownRequestProcessed);
  XM_DEFINE_PROCEDURE(XServerVendor, gxm_XServerVendor_w, 1, 0, 0, H_ServerVendor);
  XM_DEFINE_PROCEDURE(XDisplayString, gxm_XDisplayString_w, 1, 0, 0, H_DisplayString);
  XM_DEFINE_PROCEDURE(XDefaultColormap, gxm_XDefaultColormap_w, 2, 0, 0, H_DefaultColormap);
  XM_DEFINE_PROCEDURE(XDefaultColormapOfScreen, gxm_XDefaultColormapOfScreen_w, 1, 0, 0, H_DefaultColormapOfScreen);
  XM_DEFINE_PROCEDURE(XDisplayOfScreen, gxm_XDisplayOfScreen_w, 1, 0, 0, H_DisplayOfScreen);
  XM_DEFINE_PROCEDURE(XScreenOfDisplay, gxm_XScreenOfDisplay_w, 2, 0, 0, H_ScreenOfDisplay);
  XM_DEFINE_PROCEDURE(XDefaultScreenOfDisplay, gxm_XDefaultScreenOfDisplay_w, 1, 0, 0, H_DefaultScreenOfDisplay);
  XM_DEFINE_PROCEDURE(XEventMaskOfScreen, gxm_XEventMaskOfScreen_w, 1, 0, 0, H_EventMaskOfScreen);
  XM_DEFINE_PROCEDURE(XScreenNumberOfScreen, gxm_XScreenNumberOfScreen_w, 1, 0, 0, H_XScreenNumberOfScreen);
  XM_DEFINE_PROCEDURE(XSetErrorHandler, gxm_XSetErrorHandler_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XSetIOErrorHandler, gxm_XSetIOErrorHandler_w, 1, 0, 0, H_XSetIOErrorHandler);
  XM_DEFINE_PROCEDURE(XListPixmapFormats, gxm_XListPixmapFormats_w, 1, 0, 0, H_XListPixmapFormats);
  XM_DEFINE_PROCEDURE(XListDepths, gxm_XListDepths_w, 2, 0, 0, H_XListDepths);
  XM_DEFINE_PROCEDURE(XReconfigureWMWindow, gxm_XReconfigureWMWindow_w, 5, 0, 0, H_XReconfigureWMWindow);
  XM_DEFINE_PROCEDURE(XGetWMProtocols, gxm_XGetWMProtocols_w, 2, 0, 0, H_XGetWMProtocols);
  XM_DEFINE_PROCEDURE(XSetWMProtocols, gxm_XSetWMProtocols_w, 4, 0, 0, H_XSetWMProtocols);
  XM_DEFINE_PROCEDURE(XIconifyWindow, gxm_XIconifyWindow_w, 3, 0, 0, H_XIconifyWindow);
  XM_DEFINE_PROCEDURE(XWithdrawWindow, gxm_XWithdrawWindow_w, 3, 0, 0, H_XWithdrawWindow);
  XM_DEFINE_PROCEDURE(XGetCommand, gxm_XGetCommand_w, 2, 0, 0, H_XGetCommand);
  XM_DEFINE_PROCEDURE(XGetWMColormapWindows, gxm_XGetWMColormapWindows_w, 2, 0, 0, H_XGetWMColormapWindows);
  XM_DEFINE_PROCEDURE(XSetWMColormapWindows, gxm_XSetWMColormapWindows_w, 4, 0, 0, H_XSetWMColormapWindows);
  XM_DEFINE_PROCEDURE(XSetTransientForHint, gxm_XSetTransientForHint_w, 3, 0, 0, H_XSetTransientForHint);
  XM_DEFINE_PROCEDURE(XActivateScreenSaver, gxm_XActivateScreenSaver_w, 1, 0, 0, H_XActivateScreenSaver);
  XM_DEFINE_PROCEDURE(XAllocColor, gxm_XAllocColor_w, 3, 0, 0, H_XAllocColor);
  XM_DEFINE_PROCEDURE(XAllocColorCells, gxm_XAllocColorCells_w, 5, 0, 0, H_XAllocColorCells);
  XM_DEFINE_PROCEDURE(XAllocColorPlanes, gxm_XAllocColorPlanes_w, 0, 0, 1, H_XAllocColorPlanes);
  XM_DEFINE_PROCEDURE(XAllocNamedColor, gxm_XAllocNamedColor_w, 5, 0, 0, H_XAllocNamedColor);
  XM_DEFINE_PROCEDURE(XAllowEvents, gxm_XAllowEvents_w, 3, 0, 0, H_XAllowEvents);
  XM_DEFINE_PROCEDURE(XAutoRepeatOff, gxm_XAutoRepeatOff_w, 1, 0, 0, H_XAutoRepeatOff);
  XM_DEFINE_PROCEDURE(XAutoRepeatOn, gxm_XAutoRepeatOn_w, 1, 0, 0, H_XAutoRepeatOn);
  XM_DEFINE_PROCEDURE(XBell, gxm_XBell_w, 2, 0, 0, H_XBell);
  XM_DEFINE_PROCEDURE(XBitmapBitOrder, gxm_XBitmapBitOrder_w, 1, 0, 0, H_BitmapBitOrder);
  XM_DEFINE_PROCEDURE(XBitmapPad, gxm_XBitmapPad_w, 1, 0, 0, H_BitmapPad);
  XM_DEFINE_PROCEDURE(XBitmapUnit, gxm_XBitmapUnit_w, 1, 0, 0, H_BitmapUnit);
  XM_DEFINE_PROCEDURE(XCellsOfScreen, gxm_XCellsOfScreen_w, 1, 0, 0, H_CellsOfScreen);
  XM_DEFINE_PROCEDURE(XChangeActivePointerGrab, gxm_XChangeActivePointerGrab_w, 4, 0, 0, H_XChangeActivePointerGrab);
  XM_DEFINE_PROCEDURE(XChangeGC, gxm_XChangeGC_w, 4, 0, 0, H_XChangeGC);
  XM_DEFINE_PROCEDURE(XChangeKeyboardControl, gxm_XChangeKeyboardControl_w, 3, 0, 0, H_XChangeKeyboardControl);
  XM_DEFINE_PROCEDURE(XChangeKeyboardMapping, gxm_XChangeKeyboardMapping_w, 5, 0, 0, H_XChangeKeyboardMapping);
  XM_DEFINE_PROCEDURE(XChangePointerControl, gxm_XChangePointerControl_w, 6, 0, 0, H_XChangePointerControl);
  XM_DEFINE_PROCEDURE(XChangeProperty, gxm_XChangeProperty_w, 7, 1, 0, H_XChangeProperty);
  XM_DEFINE_PROCEDURE(XChangeWindowAttributes, gxm_XChangeWindowAttributes_w, 4, 0, 0, H_XChangeWindowAttributes);
  XM_DEFINE_PROCEDURE(XCheckIfEvent, gxm_XCheckIfEvent_w, 3, 0, 0, H_XCheckIfEvent);
  XM_DEFINE_PROCEDURE(XCheckMaskEvent, gxm_XCheckMaskEvent_w, 2, 0, 0, H_XCheckMaskEvent);
  XM_DEFINE_PROCEDURE(XCheckTypedEvent, gxm_XCheckTypedEvent_w, 2, 0, 0, H_XCheckTypedEvent);
  XM_DEFINE_PROCEDURE(XCheckTypedWindowEvent, gxm_XCheckTypedWindowEvent_w, 3, 0, 0, H_XCheckTypedWindowEvent);
  XM_DEFINE_PROCEDURE(XCheckWindowEvent, gxm_XCheckWindowEvent_w, 3, 0, 0, H_XCheckWindowEvent);
  XM_DEFINE_PROCEDURE(XCirculateSubwindows, gxm_XCirculateSubwindows_w, 3, 0, 0, H_XCirculateSubwindows);
  XM_DEFINE_PROCEDURE(XCirculateSubwindowsDown, gxm_XCirculateSubwindowsDown_w, 2, 0, 0, H_XCirculateSubwindowsDown);
  XM_DEFINE_PROCEDURE(XCirculateSubwindowsUp, gxm_XCirculateSubwindowsUp_w, 2, 0, 0, H_XCirculateSubwindowsUp);
  XM_DEFINE_PROCEDURE(XClearArea, gxm_XClearArea_w, 7, 0, 0, H_XClearArea);
  XM_DEFINE_PROCEDURE(XClearWindow, gxm_XClearWindow_w, 2, 0, 0, H_XClearWindow);
  XM_DEFINE_PROCEDURE(XCloseDisplay, gxm_XCloseDisplay_w, 1, 0, 0, H_XCloseDisplay);
  XM_DEFINE_PROCEDURE(XConfigureWindow, gxm_XConfigureWindow_w, 4, 0, 0, H_XConfigureWindow);
  XM_DEFINE_PROCEDURE(XConnectionNumber, gxm_XConnectionNumber_w, 1, 0, 0, H_XConnectionNumber);
  XM_DEFINE_PROCEDURE(XConvertSelection, gxm_XConvertSelection_w, 6, 0, 0, H_XConvertSelection);
  XM_DEFINE_PROCEDURE(XCopyArea, gxm_XCopyArea_w, 0, 0, 1, H_XCopyArea);
  XM_DEFINE_PROCEDURE(XCopyGC, gxm_XCopyGC_w, 4, 0, 0, H_XCopyGC);
  XM_DEFINE_PROCEDURE(XCopyPlane, gxm_XCopyPlane_w, 0, 0, 1, H_XCopyPlane);
  XM_DEFINE_PROCEDURE(XDefaultDepth, gxm_XDefaultDepth_w, 2, 0, 0, H_DefaultDepth);
  XM_DEFINE_PROCEDURE(XDefaultDepthOfScreen, gxm_XDefaultDepthOfScreen_w, 1, 0, 0, H_DefaultDepthOfScreen);
  XM_DEFINE_PROCEDURE(XDefaultScreen, gxm_XDefaultScreen_w, 1, 0, 0, H_DefaultScreen);
  XM_DEFINE_PROCEDURE(XDefineCursor, gxm_XDefineCursor_w, 3, 0, 0, H_XDefineCursor);
  XM_DEFINE_PROCEDURE(XDeleteProperty, gxm_XDeleteProperty_w, 3, 0, 0, H_XDeleteProperty);
  XM_DEFINE_PROCEDURE(XDestroyWindow, gxm_XDestroyWindow_w, 2, 0, 0, H_XDestroyWindow);
  XM_DEFINE_PROCEDURE(XDestroySubwindows, gxm_XDestroySubwindows_w, 2, 0, 0, H_XDestroySubwindows);
  XM_DEFINE_PROCEDURE(XDoesBackingStore, gxm_XDoesBackingStore_w, 1, 0, 0, H_XDoesBackingStore);
  XM_DEFINE_PROCEDURE(XDoesSaveUnders, gxm_XDoesSaveUnders_w, 1, 0, 0, H_XDoesSaveUnders);
  XM_DEFINE_PROCEDURE(XDisableAccessControl, gxm_XDisableAccessControl_w, 1, 0, 0, H_XDisableAccessControl);
  XM_DEFINE_PROCEDURE(XDisplayCells, gxm_XDisplayCells_w, 2, 0, 0, H_XDisplayCells);
  XM_DEFINE_PROCEDURE(XDisplayHeight, gxm_XDisplayHeight_w, 2, 0, 0, H_XDisplayHeight);
  XM_DEFINE_PROCEDURE(XDisplayHeightMM, gxm_XDisplayHeightMM_w, 2, 0, 0, H_XDisplayHeightMM);
  XM_DEFINE_PROCEDURE(XDisplayKeycodes, gxm_XDisplayKeycodes_w, 1, 0, 0, H_XDisplayKeycodes);
  XM_DEFINE_PROCEDURE(XDisplayPlanes, gxm_XDisplayPlanes_w, 2, 0, 0, H_XDisplayPlanes);
  XM_DEFINE_PROCEDURE(XDisplayWidth, gxm_XDisplayWidth_w, 2, 0, 0, H_XDisplayWidth);
  XM_DEFINE_PROCEDURE(XDisplayWidthMM, gxm_XDisplayWidthMM_w, 2, 0, 0, H_XDisplayWidthMM);
  XM_DEFINE_PROCEDURE(XDrawArc, gxm_XDrawArc_w, 9, 0, 0, H_XDrawArc);
  XM_DEFINE_PROCEDURE(XDrawArcs, gxm_XDrawArcs_w, 5, 0, 0, H_XDrawArcs);
  XM_DEFINE_PROCEDURE(XDrawImageString, gxm_XDrawImageString_w, 7, 0, 0, H_XDrawImageString);
  XM_DEFINE_PROCEDURE(XDrawLine, gxm_XDrawLine_w, 7, 0, 0, H_XDrawLine);
  XM_DEFINE_PROCEDURE(XDrawLines, gxm_XDrawLines_w, 6, 0, 0, H_XDrawLines);
  XM_DEFINE_PROCEDURE(XDrawLinesDirect, gxm_XDrawLinesDirect_w, 6, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(freeXPoints, gxm_FreeXPoints_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(vector->XPoints, gxm_Vector2XPoints_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XDrawPoint, gxm_XDrawPoint_w, 5, 0, 0, H_XDrawPoint);
  XM_DEFINE_PROCEDURE(XDrawPoints, gxm_XDrawPoints_w, 6, 0, 0, H_XDrawPoints);
  XM_DEFINE_PROCEDURE(XDrawRectangle, gxm_XDrawRectangle_w, 7, 0, 0, H_XDrawRectangle);
  XM_DEFINE_PROCEDURE(XDrawRectangles, gxm_XDrawRectangles_w, 5, 0, 0, H_XDrawRectangles);
  XM_DEFINE_PROCEDURE(XDrawSegments, gxm_XDrawSegments_w, 5, 0, 0, H_XDrawSegments);
  XM_DEFINE_PROCEDURE(XDrawString, gxm_XDrawString_w, 7, 0, 0, H_XDrawString);
  XM_DEFINE_PROCEDURE(XDrawText, gxm_XDrawText_w, 6, 1, 0, H_XDrawText);
  XM_DEFINE_PROCEDURE(XEnableAccessControl, gxm_XEnableAccessControl_w, 1, 0, 0, H_XEnableAccessControl);
  XM_DEFINE_PROCEDURE(XEventsQueued, gxm_XEventsQueued_w, 2, 0, 0, H_XEventsQueued);
  XM_DEFINE_PROCEDURE(XFetchName, gxm_XFetchName_w, 2, 0, 0, H_XFetchName);
  XM_DEFINE_PROCEDURE(XFillArc, gxm_XFillArc_w, 9, 0, 0, H_XFillArc);
  XM_DEFINE_PROCEDURE(XFillArcs, gxm_XFillArcs_w, 5, 0, 0, H_XFillArcs);
  XM_DEFINE_PROCEDURE(XFillPolygon, gxm_XFillPolygon_w, 7, 0, 0, H_XFillPolygon);
  XM_DEFINE_PROCEDURE(XFillRectangle, gxm_XFillRectangle_w, 7, 0, 0, H_XFillRectangle);
  XM_DEFINE_PROCEDURE(XFillRectangles, gxm_XFillRectangles_w, 5, 0, 0, H_XFillRectangles);
  XM_DEFINE_PROCEDURE(XFlush, gxm_XFlush_w, 1, 0, 0, H_XFlush);
  XM_DEFINE_PROCEDURE(XForceScreenSaver, gxm_XForceScreenSaver_w, 2, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XFree, gxm_XFree_w, 1, 0, 0, H_XFree);
  XM_DEFINE_PROCEDURE(XFreeColormap, gxm_XFreeColormap_w, 2, 0, 0, H_XFreeColormap);
  XM_DEFINE_PROCEDURE(XFreeColors, gxm_XFreeColors_w, 5, 0, 0, H_XFreeColors);
  XM_DEFINE_PROCEDURE(XFreeCursor, gxm_XFreeCursor_w, 2, 0, 0, H_XFreeCursor);
  XM_DEFINE_PROCEDURE(XFreeExtensionList, gxm_XFreeExtensionList_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XFreeFont, gxm_XFreeFont_w, 2, 0, 0, H_XFreeFont);
  XM_DEFINE_PROCEDURE(XFreeFontInfo, gxm_XFreeFontInfo_w, 3, 0, 0, H_XFreeFontInfo);
  XM_DEFINE_PROCEDURE(XFreeFontNames, gxm_XFreeFontNames_w, 1, 0, 0, H_XFreeFontNames);
  XM_DEFINE_PROCEDURE(XFreeFontPath, gxm_XFreeFontPath_w, 1, 0, 0, H_XFreeFontPath);
  XM_DEFINE_PROCEDURE(XFreeGC, gxm_XFreeGC_w, 2, 0, 0, H_XFreeGC);
  XM_DEFINE_PROCEDURE(XFreeModifiermap, gxm_XFreeModifiermap_w, 1, 0, 0, H_XFreeModifiermap);
  XM_DEFINE_PROCEDURE(XFreePixmap, gxm_XFreePixmap_w, 2, 0, 0, H_XFreePixmap);
  XM_DEFINE_PROCEDURE(XGeometry, gxm_XGeometry_w, 0, 0, 1, NULL);
  XM_DEFINE_PROCEDURE(XGetErrorText, gxm_XGetErrorText_w, 4, 0, 0, H_XGetErrorText);
  XM_DEFINE_PROCEDURE(XGetFontProperty, gxm_XGetFontProperty_w, 2, 0, 0, H_XGetFontProperty);
  XM_DEFINE_PROCEDURE(XGetGCValues, gxm_XGetGCValues_w, 3, 0, 0, H_XGetGCValues);
  XM_DEFINE_PROCEDURE(XGCValues, gxm_XGCValues_w, 0, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XEvent, gxm_XEvent_w, 0, 1, 0, NULL);
  XM_DEFINE_PROCEDURE(XGetGeometry, gxm_XGetGeometry_w, 2, 0, 0, H_XGetGeometry);
  XM_DEFINE_PROCEDURE(XGetIconName, gxm_XGetIconName_w, 2, 0, 0, H_XGetIconName);
  XM_DEFINE_PROCEDURE(XGetInputFocus, gxm_XGetInputFocus_w, 1, 0, 0, H_XGetInputFocus);
  XM_DEFINE_PROCEDURE(XGetKeyboardControl, gxm_XGetKeyboardControl_w, 1, 0, 0, H_XGetKeyboardControl);
  XM_DEFINE_PROCEDURE(XGetPointerControl, gxm_XGetPointerControl_w, 1, 0, 0, H_XGetPointerControl);
  XM_DEFINE_PROCEDURE(XGetPointerMapping, gxm_XGetPointerMapping_w, 3, 0, 0, H_XGetPointerMapping);
  XM_DEFINE_PROCEDURE(XGetScreenSaver, gxm_XGetScreenSaver_w, 1, 0, 0, H_XGetScreenSaver);
  XM_DEFINE_PROCEDURE(XGetTransientForHint, gxm_XGetTransientForHint_w, 2, 0, 0, H_XGetTransientForHint);
  XM_DEFINE_PROCEDURE(XGetWindowProperty, gxm_XGetWindowProperty_w, 0, 0, 1, H_XGetWindowProperty);
  XM_DEFINE_PROCEDURE(XGetWindowAttributes, gxm_XGetWindowAttributes_w, 2, 0, 0, H_XGetWindowAttributes);
  XM_DEFINE_PROCEDURE(XGrabButton, gxm_XGrabButton_w, 0, 0, 1, H_XGrabButton);
  XM_DEFINE_PROCEDURE(XGrabKey, gxm_XGrabKey_w, 7, 0, 0, H_XGrabKey);
  XM_DEFINE_PROCEDURE(XGrabKeyboard, gxm_XGrabKeyboard_w, 6, 0, 0, H_XGrabKeyboard);
  XM_DEFINE_PROCEDURE(XGrabPointer, gxm_XGrabPointer_w, 9, 0, 0, H_XGrabPointer);
  XM_DEFINE_PROCEDURE(XGrabServer, gxm_XGrabServer_w, 1, 0, 0, H_XGrabServer);
  XM_DEFINE_PROCEDURE(XHeightMMOfScreen, gxm_XHeightMMOfScreen_w, 1, 0, 0, H_HeightMMOfScreen);
  XM_DEFINE_PROCEDURE(XHeightOfScreen, gxm_XHeightOfScreen_w, 1, 0, 0, H_HeightOfScreen);
  XM_DEFINE_PROCEDURE(XIfEvent, gxm_XIfEvent_w, 3, 0, 0, H_XIfEvent);
  XM_DEFINE_PROCEDURE(XImageByteOrder, gxm_XImageByteOrder_w, 1, 0, 0, H_ImageByteOrder);
  XM_DEFINE_PROCEDURE(XInstallColormap, gxm_XInstallColormap_w, 2, 0, 0, H_XInstallColormap);
  XM_DEFINE_PROCEDURE(XKeysymToKeycode, gxm_XKeysymToKeycode_w, 2, 0, 0, H_XKeysymToKeycode);
  XM_DEFINE_PROCEDURE(XKillClient, gxm_XKillClient_w, 2, 0, 0, H_XKillClient);
  XM_DEFINE_PROCEDURE(XLookupColor, gxm_XLookupColor_w, 5, 0, 0, H_XLookupColor);
  XM_DEFINE_PROCEDURE(XLowerWindow, gxm_XLowerWindow_w, 2, 0, 0, H_XLowerWindow);
  XM_DEFINE_PROCEDURE(XMapRaised, gxm_XMapRaised_w, 2, 0, 0, H_XMapRaised);
  XM_DEFINE_PROCEDURE(XMapSubwindows, gxm_XMapSubwindows_w, 2, 0, 0, H_XMapSubwindows);
  XM_DEFINE_PROCEDURE(XMapWindow, gxm_XMapWindow_w, 2, 0, 0, H_XMapWindow);
  XM_DEFINE_PROCEDURE(XMaskEvent, gxm_XMaskEvent_w, 2, 0, 0, H_XMaskEvent);
  XM_DEFINE_PROCEDURE(XMaxCmapsOfScreen, gxm_XMaxCmapsOfScreen_w, 1, 0, 0, H_MaxCmapsOfScreen);
  XM_DEFINE_PROCEDURE(XMinCmapsOfScreen, gxm_XMinCmapsOfScreen_w, 1, 0, 0, H_MinCmapsOfScreen);
  XM_DEFINE_PROCEDURE(XMoveResizeWindow, gxm_XMoveResizeWindow_w, 6, 0, 0, H_XMoveResizeWindow);
  XM_DEFINE_PROCEDURE(XMoveWindow, gxm_XMoveWindow_w, 4, 0, 0, H_XMoveWindow);
  XM_DEFINE_PROCEDURE(XNextEvent, gxm_XNextEvent_w, 1, 0, 0, H_XNextEvent);
  XM_DEFINE_PROCEDURE(XNoOp, gxm_XNoOp_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XParseColor, gxm_XParseColor_w, 4, 0, 0, H_XParseColor);
  XM_DEFINE_PROCEDURE(XParseGeometry, gxm_XParseGeometry_w, 1, 0, 0, H_XParseGeometry);
  XM_DEFINE_PROCEDURE(XPeekEvent, gxm_XPeekEvent_w, 1, 0, 0, H_XPeekEvent);
  XM_DEFINE_PROCEDURE(XPeekIfEvent, gxm_XPeekIfEvent_w, 3, 0, 0, H_XPeekIfEvent);
  XM_DEFINE_PROCEDURE(XPending, gxm_XPending_w, 1, 0, 0, H_XPending);
  XM_DEFINE_PROCEDURE(XPlanesOfScreen, gxm_XPlanesOfScreen_w, 1, 0, 0, H_PlanesOfScreen);
  XM_DEFINE_PROCEDURE(XProtocolRevision, gxm_XProtocolRevision_w, 1, 0, 0, H_ProtocolRevision);
  XM_DEFINE_PROCEDURE(XProtocolVersion, gxm_XProtocolVersion_w, 1, 0, 0, H_ProtocolVersion);
  XM_DEFINE_PROCEDURE(XPutBackEvent, gxm_XPutBackEvent_w, 2, 0, 0, H_XPutBackEvent);
  XM_DEFINE_PROCEDURE(XPutImage, gxm_XPutImage_w, 0, 0, 1, H_XPutImage);
  XM_DEFINE_PROCEDURE(XQLength, gxm_XQLength_w, 1, 0, 0, H_QLength);
  XM_DEFINE_PROCEDURE(XQueryBestCursor, gxm_XQueryBestCursor_w, 4, 0, 0, H_XQueryBestCursor);
  XM_DEFINE_PROCEDURE(XQueryBestSize, gxm_XQueryBestSize_w, 5, 0, 0, H_XQueryBestSize);
  XM_DEFINE_PROCEDURE(XQueryBestStipple, gxm_XQueryBestStipple_w, 4, 0, 0, H_XQueryBestStipple);
  XM_DEFINE_PROCEDURE(XQueryBestTile, gxm_XQueryBestTile_w, 4, 0, 0, H_XQueryBestTile);
  XM_DEFINE_PROCEDURE(XQueryColor, gxm_XQueryColor_w, 3, 0, 0, H_XQueryColor);
  XM_DEFINE_PROCEDURE(XQueryColors, gxm_XQueryColors_w, 3, 1, 0, H_XQueryColors);
  XM_DEFINE_PROCEDURE(XQueryExtension, gxm_XQueryExtension_w, 2, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XQueryKeymap, gxm_XQueryKeymap_w, 1, 0, 0, H_XQueryKeymap);
  XM_DEFINE_PROCEDURE(XQueryPointer, gxm_XQueryPointer_w, 2, 0, 0, H_XQueryPointer);
  XM_DEFINE_PROCEDURE(XQueryTextExtents, gxm_XQueryTextExtents_w, 3, 0, 0, H_XQueryTextExtents);
  XM_DEFINE_PROCEDURE(XQueryTree, gxm_XQueryTree_w, 2, 0, 0, H_XQueryTree);
  XM_DEFINE_PROCEDURE(XRaiseWindow, gxm_XRaiseWindow_w, 2, 0, 0, H_XRaiseWindow);
  XM_DEFINE_PROCEDURE(XReadBitmapFile, gxm_XReadBitmapFile_w, 3, 0, 0, H_XReadBitmapFile);
  XM_DEFINE_PROCEDURE(XReadBitmapFileData, gxm_XReadBitmapFileData_w, 1, 0, 0, H_XReadBitmapFileData);
  XM_DEFINE_PROCEDURE(XRebindKeysym, gxm_XRebindKeysym_w, 6, 0, 0, H_XRebindKeysym);
  XM_DEFINE_PROCEDURE(XRecolorCursor, gxm_XRecolorCursor_w, 4, 0, 0, H_XRecolorCursor);
  XM_DEFINE_PROCEDURE(XRefreshKeyboardMapping, gxm_XRefreshKeyboardMapping_w, 1, 0, 0, H_XRefreshKeyboardMapping);
  XM_DEFINE_PROCEDURE(XReparentWindow, gxm_XReparentWindow_w, 5, 0, 0, H_XReparentWindow);
  XM_DEFINE_PROCEDURE(XResetScreenSaver, gxm_XResetScreenSaver_w, 1, 0, 0, H_XResetScreenSaver);
  XM_DEFINE_PROCEDURE(XResizeWindow, gxm_XResizeWindow_w, 4, 0, 0, H_XResizeWindow);
  XM_DEFINE_PROCEDURE(XRestackWindows, gxm_XRestackWindows_w, 3, 0, 0, H_XRestackWindows);
  XM_DEFINE_PROCEDURE(XRotateBuffers, gxm_XRotateBuffers_w, 2, 0, 0, H_XRotateBuffers);
  XM_DEFINE_PROCEDURE(XRotateWindowProperties, gxm_XRotateWindowProperties_w, 5, 0, 0, H_XRotateWindowProperties);
  XM_DEFINE_PROCEDURE(XScreenCount, gxm_XScreenCount_w, 1, 0, 0, H_ScreenCount);
  XM_DEFINE_PROCEDURE(XSelectInput, gxm_XSelectInput_w, 3, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XSendEvent, gxm_XSendEvent_w, 5, 0, 0, H_XSendEvent);
  XM_DEFINE_PROCEDURE(XSetAccessControl, gxm_XSetAccessControl_w, 2, 0, 0, H_XSetAccessControl);
  XM_DEFINE_PROCEDURE(XSetArcMode, gxm_XSetArcMode_w, 3, 0, 0, H_XSetArcMode);
  XM_DEFINE_PROCEDURE(XSetBackground, gxm_XSetBackground_w, 3, 0, 0, H_XSetBackground);
  XM_DEFINE_PROCEDURE(XSetClipMask, gxm_XSetClipMask_w, 3, 0, 0, H_XSetClipMask);
  XM_DEFINE_PROCEDURE(XSetClipOrigin, gxm_XSetClipOrigin_w, 4, 0, 0, H_XSetClipOrigin);
  XM_DEFINE_PROCEDURE(XSetClipRectangles, gxm_XSetClipRectangles_w, 7, 0, 0, H_XSetClipRectangles);
  XM_DEFINE_PROCEDURE(XSetCloseDownMode, gxm_XSetCloseDownMode_w, 2, 0, 0, H_XSetCloseDownMode);
  XM_DEFINE_PROCEDURE(XSetCommand, gxm_XSetCommand_w, 4, 0, 0, H_XSetCommand);
  XM_DEFINE_PROCEDURE(XSetDashes, gxm_XSetDashes_w, 5, 0, 0, H_XSetDashes);
  XM_DEFINE_PROCEDURE(XSetFillRule, gxm_XSetFillRule_w, 3, 0, 0, H_XSetFillRule);
  XM_DEFINE_PROCEDURE(XSetFillStyle, gxm_XSetFillStyle_w, 3, 0, 0, H_XSetFillStyle);
  XM_DEFINE_PROCEDURE(XSetFont, gxm_XSetFont_w, 3, 0, 0, H_XSetFont);
  XM_DEFINE_PROCEDURE(XSetFontPath, gxm_XSetFontPath_w, 3, 0, 0, H_XSetFontPath);
  XM_DEFINE_PROCEDURE(XSetForeground, gxm_XSetForeground_w, 3, 0, 0, H_XSetForeground);
  XM_DEFINE_PROCEDURE(XSetFunction, gxm_XSetFunction_w, 3, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XSetGraphicsExposures, gxm_XSetGraphicsExposures_w, 3, 0, 0, H_XSetGraphicsExposures);
  XM_DEFINE_PROCEDURE(XSetIconName, gxm_XSetIconName_w, 3, 0, 0, H_XSetIconName);
  XM_DEFINE_PROCEDURE(XSetInputFocus, gxm_XSetInputFocus_w, 4, 0, 0, H_XSetInputFocus);
  XM_DEFINE_PROCEDURE(XSetLineAttributes, gxm_XSetLineAttributes_w, 6, 0, 0, H_XSetLineAttributes);
  XM_DEFINE_PROCEDURE(XSetModifierMapping, gxm_XSetModifierMapping_w, 2, 0, 0, H_XSetModifierMapping);
  XM_DEFINE_PROCEDURE(XSetPlaneMask, gxm_XSetPlaneMask_w, 3, 0, 0, H_XSetPlaneMask);
  XM_DEFINE_PROCEDURE(XSetPointerMapping, gxm_XSetPointerMapping_w, 2, 1, 0, H_XSetPointerMapping);
  XM_DEFINE_PROCEDURE(XSetScreenSaver, gxm_XSetScreenSaver_w, 5, 0, 0, H_XSetScreenSaver);
  XM_DEFINE_PROCEDURE(XSetSelectionOwner, gxm_XSetSelectionOwner_w, 4, 0, 0, H_XSetSelectionOwner);
  XM_DEFINE_PROCEDURE(XSetState, gxm_XSetState_w, 6, 0, 0, H_XSetState);
  XM_DEFINE_PROCEDURE(XSetStipple, gxm_XSetStipple_w, 3, 0, 0, H_XSetStipple);
  XM_DEFINE_PROCEDURE(XSetSubwindowMode, gxm_XSetSubwindowMode_w, 3, 0, 0, H_XSetSubwindowMode);
  XM_DEFINE_PROCEDURE(XSetTSOrigin, gxm_XSetTSOrigin_w, 4, 0, 0, H_XSetTSOrigin);
  XM_DEFINE_PROCEDURE(XSetTile, gxm_XSetTile_w, 3, 0, 0, H_XSetTile);
  XM_DEFINE_PROCEDURE(XSetWindowBackground, gxm_XSetWindowBackground_w, 3, 0, 0, H_XSetWindowBackground);
  XM_DEFINE_PROCEDURE(XSetWindowBackgroundPixmap, gxm_XSetWindowBackgroundPixmap_w, 3, 0, 0, H_XSetWindowBackgroundPixmap);
  XM_DEFINE_PROCEDURE(XSetWindowBorder, gxm_XSetWindowBorder_w, 3, 0, 0, H_XSetWindowBorder);
  XM_DEFINE_PROCEDURE(XSetWindowBorderPixmap, gxm_XSetWindowBorderPixmap_w, 3, 0, 0, H_XSetWindowBorderPixmap);
  XM_DEFINE_PROCEDURE(XSetWindowBorderWidth, gxm_XSetWindowBorderWidth_w, 3, 0, 0, H_XSetWindowBorderWidth);
  XM_DEFINE_PROCEDURE(XSetWindowColormap, gxm_XSetWindowColormap_w, 3, 0, 0, H_XSetWindowColormap);
  XM_DEFINE_PROCEDURE(XStoreBuffer, gxm_XStoreBuffer_w, 4, 0, 0, H_XStoreBuffer);
  XM_DEFINE_PROCEDURE(XStoreBytes, gxm_XStoreBytes_w, 3, 0, 0, H_XStoreBytes);
  XM_DEFINE_PROCEDURE(XStoreColor, gxm_XStoreColor_w, 3, 0, 0, H_XStoreColor);
  XM_DEFINE_PROCEDURE(XStoreColors, gxm_XStoreColors_w, 4, 0, 0, H_XStoreColors);
  XM_DEFINE_PROCEDURE(XStoreName, gxm_XStoreName_w, 3, 0, 0, H_XStoreName);
  XM_DEFINE_PROCEDURE(XStoreNamedColor, gxm_XStoreNamedColor_w, 5, 0, 0, H_XStoreNamedColor);
  XM_DEFINE_PROCEDURE(XSync, gxm_XSync_w, 2, 0, 0, H_XSync);
  XM_DEFINE_PROCEDURE(XTextExtents, gxm_XTextExtents_w, 3, 0, 0, H_XTextExtents);
  XM_DEFINE_PROCEDURE(XTextWidth, gxm_XTextWidth_w, 3, 0, 0, H_XTextWidth);
  XM_DEFINE_PROCEDURE(XTranslateCoordinates, gxm_XTranslateCoordinates_w, 5, 0, 0, H_XTranslateCoordinates);
  XM_DEFINE_PROCEDURE(XUndefineCursor, gxm_XUndefineCursor_w, 2, 0, 0, H_XUndefineCursor);
  XM_DEFINE_PROCEDURE(XUngrabButton, gxm_XUngrabButton_w, 4, 0, 0, H_XUngrabButton);
  XM_DEFINE_PROCEDURE(XUngrabKey, gxm_XUngrabKey_w, 4, 0, 0, H_XUngrabKey);
  XM_DEFINE_PROCEDURE(XUngrabKeyboard, gxm_XUngrabKeyboard_w, 2, 0, 0, H_XUngrabKeyboard);
  XM_DEFINE_PROCEDURE(XUngrabPointer, gxm_XUngrabPointer_w, 2, 0, 0, H_XUngrabPointer);
  XM_DEFINE_PROCEDURE(XUngrabServer, gxm_XUngrabServer_w, 1, 0, 0, H_XUngrabServer);
  XM_DEFINE_PROCEDURE(XUninstallColormap, gxm_XUninstallColormap_w, 2, 0, 0, H_XUninstallColormap);
  XM_DEFINE_PROCEDURE(XUnloadFont, gxm_XUnloadFont_w, 2, 0, 0, H_XUnloadFont);
  XM_DEFINE_PROCEDURE(XUnmapSubwindows, gxm_XUnmapSubwindows_w, 2, 0, 0, H_XUnmapSubwindows);
  XM_DEFINE_PROCEDURE(XUnmapWindow, gxm_XUnmapWindow_w, 2, 0, 0, H_XUnmapWindow);
  XM_DEFINE_PROCEDURE(XVendorRelease, gxm_XVendorRelease_w, 1, 0, 0, H_VendorRelease);
  XM_DEFINE_PROCEDURE(XWarpPointer, gxm_XWarpPointer_w, 9, 0, 0, H_XWarpPointer);
  XM_DEFINE_PROCEDURE(XWidthMMOfScreen, gxm_XWidthMMOfScreen_w, 1, 0, 0, H_WidthMMOfScreen);
  XM_DEFINE_PROCEDURE(XWidthOfScreen, gxm_XWidthOfScreen_w, 1, 0, 0, H_WidthOfScreen);
  XM_DEFINE_PROCEDURE(XWindowEvent, gxm_XWindowEvent_w, 3, 0, 0, H_XWindowEvent);
  XM_DEFINE_PROCEDURE(XWriteBitmapFile, gxm_XWriteBitmapFile_w, 7, 0, 0, H_XWriteBitmapFile);
  XM_DEFINE_PROCEDURE(XSupportsLocale, gxm_XSupportsLocale_w, 0, 0, 0, H_XSupportsLocale);
  XM_DEFINE_PROCEDURE(XSetLocaleModifiers, gxm_XSetLocaleModifiers_w, 1, 0, 0, H_XSetLocaleModifiers);
  XM_DEFINE_PROCEDURE(XCreateFontSet, gxm_XCreateFontSet_w, 2, 0, 0, H_XCreateFontSet);
  XM_DEFINE_PROCEDURE(XFreeFontSet, gxm_XFreeFontSet_w, 2, 0, 0, H_XFreeFontSet);
  XM_DEFINE_PROCEDURE(XFontsOfFontSet, gxm_XFontsOfFontSet_w, 1, 0, 0, H_XFontsOfFontSet);
  XM_DEFINE_PROCEDURE(XBaseFontNameListOfFontSet, gxm_XBaseFontNameListOfFontSet_w, 1, 0, 0, H_XBaseFontNameListOfFontSet);
  XM_DEFINE_PROCEDURE(XLocaleOfFontSet, gxm_XLocaleOfFontSet_w, 1, 0, 0, H_XLocaleOfFontSet);
  XM_DEFINE_PROCEDURE(XContextDependentDrawing, gxm_XContextDependentDrawing_w, 1, 0, 0, H_XContextDependentDrawing);
  XM_DEFINE_PROCEDURE(XDirectionalDependentDrawing, gxm_XDirectionalDependentDrawing_w, 1, 0, 0, H_XDirectionalDependentDrawing);
  XM_DEFINE_PROCEDURE(XContextualDrawing, gxm_XContextualDrawing_w, 1, 0, 0, H_XContextualDrawing);
  XM_DEFINE_PROCEDURE(XFilterEvent, gxm_XFilterEvent_w, 2, 0, 0, H_XFilterEvent);
  XM_DEFINE_PROCEDURE(XAllocIconSize, gxm_XAllocIconSize_w, 0, 0, 0, H_XAllocIconSize);
  XM_DEFINE_PROCEDURE(XAllocStandardColormap, gxm_XAllocStandardColormap_w, 0, 0, 0, H_XAllocStandardColormap);
  XM_DEFINE_PROCEDURE(XAllocWMHints, gxm_XAllocWMHints_w, 0, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XClipBox, gxm_XClipBox_w, 1, 0, 0, H_XClipBox);
  XM_DEFINE_PROCEDURE(XCreateRegion, gxm_XCreateRegion_w, 0, 0, 0, H_XCreateRegion);
  XM_DEFINE_PROCEDURE(XDefaultString, gxm_XDefaultString_w, 0, 0, 0, H_XDefaultString);
  XM_DEFINE_PROCEDURE(XDeleteContext, gxm_XDeleteContext_w, 3, 0, 0, H_XDeleteContext);
  XM_DEFINE_PROCEDURE(XDestroyRegion, gxm_XDestroyRegion_w, 1, 0, 0, H_XDestroyRegion);
  XM_DEFINE_PROCEDURE(XEmptyRegion, gxm_XEmptyRegion_w, 1, 0, 0, H_XEmptyRegion);
  XM_DEFINE_PROCEDURE(XEqualRegion, gxm_XEqualRegion_w, 2, 0, 0, H_XEqualRegion);
  XM_DEFINE_PROCEDURE(XFindContext, gxm_XFindContext_w, 3, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XGetIconSizes, gxm_XGetIconSizes_w, 2, 0, 0, H_XGetIconSizes);
  XM_DEFINE_PROCEDURE(XGetRGBColormaps, gxm_XGetRGBColormaps_w, 3, 0, 0, H_XGetRGBColormaps);
#if (!XM_DISABLE_DEPRECATED)
  XM_DEFINE_PROCEDURE(XGetStandardColormap, gxm_XGetStandardColormap_w, 3, 0, 0, NULL);
#endif
  XM_DEFINE_PROCEDURE(XGetVisualInfo, gxm_XGetVisualInfo_w, 3, 0, 0, H_XGetVisualInfo);
  XM_DEFINE_PROCEDURE(XGetWMHints, gxm_XGetWMHints_w, 2, 0, 0, H_XGetWMHints);
  XM_DEFINE_PROCEDURE(XIntersectRegion, gxm_XIntersectRegion_w, 3, 0, 0, H_XIntersectRegion);
  XM_DEFINE_PROCEDURE(XConvertCase, gxm_XConvertCase_w, 1, 0, 0, H_XConvertCase);
  XM_DEFINE_PROCEDURE(XLookupString, gxm_XLookupString_w, 1, 0, 0, H_XLookupString);
  XM_DEFINE_PROCEDURE(XMatchVisualInfo, gxm_XMatchVisualInfo_w, 4, 0, 0, H_XMatchVisualInfo);
  XM_DEFINE_PROCEDURE(XOffsetRegion, gxm_XOffsetRegion_w, 3, 0, 0, H_XOffsetRegion);
  XM_DEFINE_PROCEDURE(XPointInRegion, gxm_XPointInRegion_w, 3, 0, 0, H_XPointInRegion);
  XM_DEFINE_PROCEDURE(XPolygonRegion, gxm_XPolygonRegion_w, 3, 0, 0, H_XPolygonRegion);
  XM_DEFINE_PROCEDURE(XRectInRegion, gxm_XRectInRegion_w, 5, 0, 0, H_XRectInRegion);
  XM_DEFINE_PROCEDURE(XSaveContext, gxm_XSaveContext_w, 4, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XSetRGBColormaps, gxm_XSetRGBColormaps_w, 5, 0, 0, H_XSetRGBColormaps);
  XM_DEFINE_PROCEDURE(XSetWMHints, gxm_XSetWMHints_w, 3, 0, 0, H_XSetWMHints);
  XM_DEFINE_PROCEDURE(XSetRegion, gxm_XSetRegion_w, 3, 0, 0, H_XSetRegion);
#if (!XM_DISABLE_DEPRECATED)
  XM_DEFINE_PROCEDURE(XSetStandardColormap, gxm_XSetStandardColormap_w, 4, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XSetStandardProperties, gxm_XSetStandardProperties_w, 8, 0, 0, H_XSetStandardProperties);
#endif
  XM_DEFINE_PROCEDURE(XSetWMProperties, gxm_XSetWMProperties_w, 8, 0, 0, H_XSetWMProperties);
  XM_DEFINE_PROCEDURE(XShrinkRegion, gxm_XShrinkRegion_w, 3, 0, 0, H_XShrinkRegion);
  XM_DEFINE_PROCEDURE(XSubtractRegion, gxm_XSubtractRegion_w, 3, 0, 0, H_XSubtractRegion);
  XM_DEFINE_PROCEDURE(XUnionRectWithRegion, gxm_XUnionRectWithRegion_w, 3, 0, 0, H_XUnionRectWithRegion);
  XM_DEFINE_PROCEDURE(XUnionRegion, gxm_XUnionRegion_w, 3, 0, 0, H_XUnionRegion);
  XM_DEFINE_PROCEDURE(XXorRegion, gxm_XXorRegion_w, 3, 0, 0, H_XXorRegion);

  XM_DEFINE_PROCEDURE(DefaultScreen, gxm_DefaultScreen_w, 1, 0, 0, H_DefaultScreen);
  XM_DEFINE_PROCEDURE(DefaultRootWindow, gxm_DefaultRootWindow_w, 1, 0, 0, H_DefaultRootWindow);
  XM_DEFINE_PROCEDURE(QLength, gxm_QLength_w, 1, 0, 0, H_QLength);
  XM_DEFINE_PROCEDURE(ScreenCount, gxm_ScreenCount_w, 1, 0, 0, H_ScreenCount);
  XM_DEFINE_PROCEDURE(ServerVendor, gxm_ServerVendor_w, 1, 0, 0, H_ServerVendor);
  XM_DEFINE_PROCEDURE(ProtocolVersion, gxm_ProtocolVersion_w, 1, 0, 0, H_ProtocolVersion);
  XM_DEFINE_PROCEDURE(ProtocolRevision, gxm_ProtocolRevision_w, 1, 0, 0, H_ProtocolRevision);
  XM_DEFINE_PROCEDURE(VendorRelease, gxm_VendorRelease_w, 1, 0, 0, H_VendorRelease);
  XM_DEFINE_PROCEDURE(DisplayString, gxm_DisplayString_w, 1, 0, 0, H_DisplayString);
  XM_DEFINE_PROCEDURE(BitmapUnit, gxm_BitmapUnit_w, 1, 0, 0, H_BitmapUnit);
  XM_DEFINE_PROCEDURE(BitmapBitOrder, gxm_BitmapBitOrder_w, 1, 0, 0, H_BitmapBitOrder);
  XM_DEFINE_PROCEDURE(BitmapPad, gxm_BitmapPad_w, 1, 0, 0, H_BitmapPad);
  XM_DEFINE_PROCEDURE(ImageByteOrder, gxm_ImageByteOrder_w, 1, 0, 0, H_ImageByteOrder);
  XM_DEFINE_PROCEDURE(NextRequest, gxm_NextRequest_w, 1, 0, 0, H_NextRequest);
  XM_DEFINE_PROCEDURE(LastKnownRequestProcessed, gxm_LastKnownRequestProcessed_w, 1, 0, 0, H_LastKnownRequestProcessed);
  XM_DEFINE_PROCEDURE(DefaultScreenOfDisplay, gxm_DefaultScreenOfDisplay_w, 1, 0, 0, H_DefaultScreenOfDisplay);
  XM_DEFINE_PROCEDURE(DisplayOfScreen, gxm_DisplayOfScreen_w, 1, 0, 0, H_DisplayOfScreen);
  XM_DEFINE_PROCEDURE(RootWindowOfScreen, gxm_RootWindowOfScreen_w, 1, 0, 0, H_RootWindowOfScreen);
  XM_DEFINE_PROCEDURE(BlackPixelOfScreen, gxm_BlackPixelOfScreen_w, 1, 0, 0, H_BlackPixelOfScreen);
  XM_DEFINE_PROCEDURE(WhitePixelOfScreen, gxm_WhitePixelOfScreen_w, 1, 0, 0, H_WhitePixelOfScreen);
  XM_DEFINE_PROCEDURE(DefaultColormapOfScreen, gxm_DefaultColormapOfScreen_w, 1, 0, 0, H_DefaultColormapOfScreen);
  XM_DEFINE_PROCEDURE(DefaultDepthOfScreen, gxm_DefaultDepthOfScreen_w, 1, 0, 0, H_DefaultDepthOfScreen);
  XM_DEFINE_PROCEDURE(DefaultGCOfScreen, gxm_DefaultGCOfScreen_w, 1, 0, 0, H_DefaultGCOfScreen);
  XM_DEFINE_PROCEDURE(DefaultVisualOfScreen, gxm_DefaultVisualOfScreen_w, 1, 0, 0, H_DefaultVisualOfScreen);
  XM_DEFINE_PROCEDURE(WidthOfScreen, gxm_WidthOfScreen_w, 1, 0, 0, H_WidthOfScreen);
  XM_DEFINE_PROCEDURE(HeightOfScreen, gxm_HeightOfScreen_w, 1, 0, 0, H_HeightOfScreen);
  XM_DEFINE_PROCEDURE(WidthMMOfScreen, gxm_WidthMMOfScreen_w, 1, 0, 0, H_WidthMMOfScreen);
  XM_DEFINE_PROCEDURE(HeightMMOfScreen, gxm_HeightMMOfScreen_w, 1, 0, 0, H_HeightMMOfScreen);
  XM_DEFINE_PROCEDURE(PlanesOfScreen, gxm_PlanesOfScreen_w, 1, 0, 0, H_PlanesOfScreen);
  XM_DEFINE_PROCEDURE(CellsOfScreen, gxm_CellsOfScreen_w, 1, 0, 0, H_CellsOfScreen);
  XM_DEFINE_PROCEDURE(MinCmapsOfScreen, gxm_MinCmapsOfScreen_w, 1, 0, 0, H_MinCmapsOfScreen);
  XM_DEFINE_PROCEDURE(MaxCmapsOfScreen, gxm_MaxCmapsOfScreen_w, 1, 0, 0, H_MaxCmapsOfScreen);
  XM_DEFINE_PROCEDURE(DoesSaveUnders, gxm_DoesSaveUnders_w, 1, 0, 0, H_DoesSaveUnders);
  XM_DEFINE_PROCEDURE(DoesBackingStore, gxm_DoesBackingStore_w, 1, 0, 0, H_DoesBackingStore);
  XM_DEFINE_PROCEDURE(EventMaskOfScreen, gxm_EventMaskOfScreen_w, 1, 0, 0, H_EventMaskOfScreen);
  XM_DEFINE_PROCEDURE(RootWindow, gxm_RootWindow_w, 2, 0, 0, H_RootWindow);
  XM_DEFINE_PROCEDURE(DefaultVisual, gxm_DefaultVisual_w, 2, 0, 0, H_DefaultVisual);
  XM_DEFINE_PROCEDURE(DefaultGC, gxm_DefaultGC_w, 2, 0, 0, H_DefaultGC);
  XM_DEFINE_PROCEDURE(BlackPixel, gxm_BlackPixel_w, 2, 0, 0, H_BlackPixel);
  XM_DEFINE_PROCEDURE(WhitePixel, gxm_WhitePixel_w, 2, 0, 0, H_WhitePixel);
  XM_DEFINE_PROCEDURE(DisplayWidth, gxm_DisplayWidth_w, 2, 0, 0, H_DisplayWidth);
  XM_DEFINE_PROCEDURE(DisplayHeight, gxm_DisplayHeight_w, 2, 0, 0, H_DisplayHeight);
  XM_DEFINE_PROCEDURE(DisplayWidthMM, gxm_DisplayWidthMM_w, 2, 0, 0, H_DisplayWidthMM);
  XM_DEFINE_PROCEDURE(DisplayHeightMM, gxm_DisplayHeightMM_w, 2, 0, 0, H_DisplayHeightMM);
  XM_DEFINE_PROCEDURE(DisplayPlanes, gxm_DisplayPlanes_w, 2, 0, 0, H_DisplayPlanes);
  XM_DEFINE_PROCEDURE(DisplayCells, gxm_DisplayCells_w, 2, 0, 0, H_DisplayCells);
  XM_DEFINE_PROCEDURE(DefaultColormap, gxm_DefaultColormap_w, 2, 0, 0, H_DefaultColormap);
  XM_DEFINE_PROCEDURE(ScreenOfDisplay, gxm_ScreenOfDisplay_w, 2, 0, 0, H_ScreenOfDisplay);
  XM_DEFINE_PROCEDURE(DefaultDepth, gxm_DefaultDepth_w, 2, 0, 0, H_DefaultDepth);

  XM_DEFINE_PROCEDURE(IsKeypadKey, gxm_IsKeypadKey_w, 1, 0, 0, H_IsKeypadKey);
  XM_DEFINE_PROCEDURE(IsPrivateKeypadKey, gxm_IsPrivateKeypadKey_w, 1, 0, 0, H_IsPrivateKeypadKey);
  XM_DEFINE_PROCEDURE(IsCursorKey, gxm_IsCursorKey_w, 1, 0, 0, H_IsCursorKey);
  XM_DEFINE_PROCEDURE(IsPFKey, gxm_IsPFKey_w, 1, 0, 0, H_IsPFKey);
  XM_DEFINE_PROCEDURE(IsFunctionKey, gxm_IsFunctionKey_w, 1, 0, 0, H_IsFunctionKey);
  XM_DEFINE_PROCEDURE(IsMiscFunctionKey, gxm_IsMiscFunctionKey_w, 1, 0, 0, H_IsMiscFunctionKey);
  XM_DEFINE_PROCEDURE(IsModifierKey, gxm_IsModifierKey_w, 1, 0, 0, H_IsModifierKey);

  XM_DEFINE_PROCEDURE(XButtonEvent?, XEN_XButtonEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XCirculateEvent?, XEN_XCirculateEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XCirculateRequestEvent?, XEN_XCirculateRequestEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XClientMessageEvent?, XEN_XClientMessageEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XColormapEvent?, XEN_XColormapEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XConfigureEvent?, XEN_XConfigureEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XConfigureRequestEvent?, XEN_XConfigureRequestEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XCreateWindowEvent?, XEN_XCreateWindowEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XCrossingEvent?, XEN_XCrossingEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XDestroyWindowEvent?, XEN_XDestroyWindowEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XErrorEvent?, XEN_XErrorEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XExposeEvent?, XEN_XExposeEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XFocusChangeEvent?, XEN_XFocusChangeEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XGraphicsExposeEvent?, XEN_XGraphicsExposeEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XGravityEvent?, XEN_XGravityEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XKeyEvent?, XEN_XKeyEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XKeymapEvent?, XEN_XKeymapEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XMapEvent?, XEN_XMapEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XMapRequestEvent?, XEN_XMapRequestEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XMappingEvent?, XEN_XMappingEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XMotionEvent?, XEN_XMotionEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XNoExposeEvent?, XEN_XNoExposeEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XPropertyEvent?, XEN_XPropertyEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XReparentEvent?, XEN_XReparentEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XResizeRequestEvent?, XEN_XResizeRequestEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XSelectionClearEvent?, XEN_XSelectionClearEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XSelectionEvent?, XEN_XSelectionEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XSelectionRequestEvent?, XEN_XSelectionRequestEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XSetWindowAttributes?, XEN_XSetWindowAttributes_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XUnmapEvent?, XEN_XUnmapEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XVisibilityEvent?, XEN_XVisibilityEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XIconSize?, XEN_XIconSize_p_w, 1, 0, 0, NULL);

#if HAVE_MOTIF
  XM_DEFINE_PROCEDURE(XmCreateMessageBox, gxm_XmCreateMessageBox_w, 3, 1, 0, H_XmCreateMessageBox);
  XM_DEFINE_PROCEDURE(XmCreateMessageDialog, gxm_XmCreateMessageDialog_w, 3, 1, 0, H_XmCreateMessageDialog);
  XM_DEFINE_PROCEDURE(XmCreateErrorDialog, gxm_XmCreateErrorDialog_w, 3, 1, 0, H_XmCreateErrorDialog);
  XM_DEFINE_PROCEDURE(XmCreateInformationDialog, gxm_XmCreateInformationDialog_w, 3, 1, 0, H_XmCreateInformationDialog);
  XM_DEFINE_PROCEDURE(XmCreateQuestionDialog, gxm_XmCreateQuestionDialog_w, 3, 1, 0, H_XmCreateQuestionDialog);
  XM_DEFINE_PROCEDURE(XmCreateWarningDialog, gxm_XmCreateWarningDialog_w, 3, 1, 0, H_XmCreateWarningDialog);
  XM_DEFINE_PROCEDURE(XmCreateWorkingDialog, gxm_XmCreateWorkingDialog_w, 3, 1, 0, H_XmCreateWorkingDialog);
  XM_DEFINE_PROCEDURE(XmCreateTemplateDialog, gxm_XmCreateTemplateDialog_w, 3, 1, 0, H_XmCreateTemplateDialog);
  XM_DEFINE_PROCEDURE(XmMessageBoxGetChild, gxm_XmMessageBoxGetChild_w, 2, 0, 0, H_XmMessageBoxGetChild);
  XM_DEFINE_PROCEDURE(XmCreateArrowButtonGadget, gxm_XmCreateArrowButtonGadget_w, 3, 1, 0, H_XmCreateArrowButtonGadget);
  XM_DEFINE_PROCEDURE(XmCreateArrowButton, gxm_XmCreateArrowButton_w, 3, 1, 0, H_XmCreateArrowButton);
#if HAVE_XmCreateFontSelector
  XM_DEFINE_PROCEDURE(XmCreateFontSelector, gxm_XmCreateFontSelector_w, 3, 1, 0, H_XmCreateFontSelector);
#endif
#if HAVE_XmCreateColorSelector
  XM_DEFINE_PROCEDURE(XmCreateColorSelector, gxm_XmCreateColorSelector_w, 3, 1, 0, H_XmCreateColorSelector);
#endif
#if MOTIF_2
#if HAVE_XmToolTipGetLabel
  XM_DEFINE_PROCEDURE(XmToolTipGetLabel, gxm_XmToolTipGetLabel_w, 1, 0, 0, H_XmToolTipGetLabel);
#endif
  XM_DEFINE_PROCEDURE(XmCreateNotebook, gxm_XmCreateNotebook_w, 3, 1, 0, H_XmCreateNotebook);
  XM_DEFINE_PROCEDURE(XmNotebookGetPageInfo, gxm_XmNotebookGetPageInfo_w, 2, 0, 0, H_XmNotebookGetPageInfo);
#if HAVE_XM_XP
  XM_DEFINE_PROCEDURE(XmPrintSetup, gxm_XmPrintSetup_w, 4, 1, 0, H_XmPrintSetup);
  XM_DEFINE_PROCEDURE(XmPrintToFile, gxm_XmPrintToFile_w, 4, 0, 0, H_XmPrintToFile);
  XM_DEFINE_PROCEDURE(XmPrintPopupPDM, gxm_XmPrintPopupPDM_w, 2, 0, 0, H_XmPrintPopupPDM);
  XM_DEFINE_PROCEDURE(XmRedisplayWidget, gxm_XmRedisplayWidget_w, 1, 0, 0, H_XmRedisplayWidget);
#endif
  XM_DEFINE_PROCEDURE(XmTransferSetParameters, gxm_XmTransferSetParameters_w, 5, 0, 0, H_XmTransferSetParameters);
  XM_DEFINE_PROCEDURE(XmTransferDone, gxm_XmTransferDone_w, 2, 0, 0, H_XmTransferDone);
  XM_DEFINE_PROCEDURE(XmTransferValue, gxm_XmTransferValue_w, 5, 0, 0, H_XmTransferValue);
  XM_DEFINE_PROCEDURE(XmTransferStartRequest, gxm_XmTransferStartRequest_w, 1, 0, 0, H_XmTransferStartRequest);
  XM_DEFINE_PROCEDURE(XmTransferSendRequest, gxm_XmTransferSendRequest_w, 2, 0, 0, H_XmTransferSendRequest);
  XM_DEFINE_PROCEDURE(XmCreateComboBox, gxm_XmCreateComboBox_w, 3, 1, 0, H_XmCreateComboBox);
  XM_DEFINE_PROCEDURE(XmCreateDropDownComboBox, gxm_XmCreateDropDownComboBox_w, 3, 1, 0, H_XmCreateDropDownComboBox);
  XM_DEFINE_PROCEDURE(XmCreateDropDownList, gxm_XmCreateDropDownList_w, 3, 1, 0, H_XmCreateDropDownList);
  XM_DEFINE_PROCEDURE(XmComboBoxAddItem, gxm_XmComboBoxAddItem_w, 4, 0, 0, H_XmComboBoxAddItem);
  XM_DEFINE_PROCEDURE(XmComboBoxDeletePos, gxm_XmComboBoxDeletePos_w, 2, 0, 0, H_XmComboBoxDeletePos);
  XM_DEFINE_PROCEDURE(XmComboBoxSelectItem, gxm_XmComboBoxSelectItem_w, 2, 0, 0, H_XmComboBoxSelectItem);
  XM_DEFINE_PROCEDURE(XmComboBoxSetItem, gxm_XmComboBoxSetItem_w, 2, 0, 0, H_XmComboBoxSetItem);
  XM_DEFINE_PROCEDURE(XmComboBoxUpdate, gxm_XmComboBoxUpdate_w, 1, 0, 0, H_XmComboBoxUpdate);
  XM_DEFINE_PROCEDURE(XmCreateContainer, gxm_XmCreateContainer_w, 3, 1, 0, H_XmCreateContainer);
  XM_DEFINE_PROCEDURE(XmContainerGetItemChildren, gxm_XmContainerGetItemChildren_w, 2, 0, 0, H_XmContainerGetItemChildren);
  XM_DEFINE_PROCEDURE(XmContainerRelayout, gxm_XmContainerRelayout_w, 1, 0, 0, H_XmContainerRelayout);
  XM_DEFINE_PROCEDURE(XmContainerReorder, gxm_XmContainerReorder_w, 3, 0, 0, H_XmContainerReorder);
  XM_DEFINE_PROCEDURE(XmContainerCut, gxm_XmContainerCut_w, 2, 0, 0, H_XmContainerCut);
  XM_DEFINE_PROCEDURE(XmContainerCopy, gxm_XmContainerCopy_w, 2, 0, 0, H_XmContainerCopy);
  XM_DEFINE_PROCEDURE(XmContainerPaste, gxm_XmContainerPaste_w, 1, 0, 0, H_XmContainerPaste);
  XM_DEFINE_PROCEDURE(XmContainerCopyLink, gxm_XmContainerCopyLink_w, 2, 0, 0, H_XmContainerCopyLink);
  XM_DEFINE_PROCEDURE(XmContainerPasteLink, gxm_XmContainerPasteLink_w, 1, 0, 0, H_XmContainerPasteLink);
  XM_DEFINE_PROCEDURE(XmCreateSpinBox, gxm_XmCreateSpinBox_w, 3, 1, 0, H_XmCreateSpinBox);
  XM_DEFINE_PROCEDURE(XmSpinBoxValidatePosition, gxm_XmSpinBoxValidatePosition_w, 1, 0, 0, H_XmSpinBoxValidatePosition);
  XM_DEFINE_PROCEDURE(XmCreateSimpleSpinBox, gxm_XmCreateSimpleSpinBox_w, 3, 1, 0, H_XmCreateSimpleSpinBox);
  XM_DEFINE_PROCEDURE(XmSimpleSpinBoxAddItem, gxm_XmSimpleSpinBoxAddItem_w, 3, 0, 0, H_XmSimpleSpinBoxAddItem);
  XM_DEFINE_PROCEDURE(XmSimpleSpinBoxDeletePos, gxm_XmSimpleSpinBoxDeletePos_w, 2, 0, 0, H_XmSimpleSpinBoxDeletePos);
  XM_DEFINE_PROCEDURE(XmSimpleSpinBoxSetItem, gxm_XmSimpleSpinBoxSetItem_w, 2, 0, 0, H_XmSimpleSpinBoxSetItem);
  XM_DEFINE_PROCEDURE(XmDropSiteRegistered, gxm_XmDropSiteRegistered_w, 1, 0, 0, H_XmDropSiteRegistered);
  XM_DEFINE_PROCEDURE(XmTextFieldCopyLink, gxm_XmTextFieldCopyLink_w, 2, 0, 0, H_XmTextFieldCopyLink);
  XM_DEFINE_PROCEDURE(XmTextFieldPasteLink, gxm_XmTextFieldPasteLink_w, 1, 0, 0, H_XmTextFieldPasteLink);
  XM_DEFINE_PROCEDURE(XmTextGetCenterline, gxm_XmTextGetCenterline_w, 1, 0, 0, H_XmTextGetCenterline);
  XM_DEFINE_PROCEDURE(XmToggleButtonGadgetSetValue, gxm_XmToggleButtonGadgetSetValue_w, 3, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmCreateIconGadget, gxm_XmCreateIconGadget_w, 3, 1, 0, H_XmCreateIconGadget);
  XM_DEFINE_PROCEDURE(XmCreateIconHeader, gxm_XmCreateIconHeader_w, 3, 1, 0, NULL);
  XM_DEFINE_PROCEDURE(XmObjectAtPoint, gxm_XmObjectAtPoint_w, 3, 0, 0, H_XmObjectAtPoint);
  XM_DEFINE_PROCEDURE(XmConvertStringToUnits, gxm_XmConvertStringToUnits_w, 4, 0, 0, H_XmConvertStringToUnits);
  XM_DEFINE_PROCEDURE(XmCreateGrabShell, gxm_XmCreateGrabShell_w, 3, 1, 0, NULL);
  XM_DEFINE_PROCEDURE(XmToggleButtonSetValue, gxm_XmToggleButtonSetValue_w, 3, 0, 0, H_XmToggleButtonSetValue);
  XM_DEFINE_PROCEDURE(XmTextPasteLink, gxm_XmTextPasteLink_w, 1, 0, 0, H_XmTextPasteLink);
  XM_DEFINE_PROCEDURE(XmTextCopyLink, gxm_XmTextCopyLink_w, 2, 0, 0, H_XmTextCopyLink);
  XM_DEFINE_PROCEDURE(XmScaleSetTicks, gxm_XmScaleSetTicks_w, 7, 0, 0, H_XmScaleSetTicks);
#endif
  XM_DEFINE_PROCEDURE(XmInternAtom, gxm_XmInternAtom_w, 3, 0, 0, H_XmInternAtom);
  XM_DEFINE_PROCEDURE(XmGetAtomName, gxm_XmGetAtomName_w, 2, 0, 0, H_XmGetAtomName);
  XM_DEFINE_PROCEDURE(XmCreatePanedWindow, gxm_XmCreatePanedWindow_w, 3, 1, 0, H_XmCreatePanedWindow);
  XM_DEFINE_PROCEDURE(XmCreateBulletinBoard, gxm_XmCreateBulletinBoard_w, 3, 1, 0, H_XmCreateBulletinBoard);
  XM_DEFINE_PROCEDURE(XmCreateBulletinBoardDialog, gxm_XmCreateBulletinBoardDialog_w, 3, 1, 0, H_XmCreateBulletinBoardDialog);
  XM_DEFINE_PROCEDURE(XmCreateCascadeButtonGadget, gxm_XmCreateCascadeButtonGadget_w, 3, 1, 0, H_XmCreateCascadeButtonGadget);
  XM_DEFINE_PROCEDURE(XmCascadeButtonGadgetHighlight, gxm_XmCascadeButtonGadgetHighlight_w, 2, 0, 0, H_XmCascadeButtonGadgetHighlight);
  XM_DEFINE_PROCEDURE(XmAddProtocols, gxm_XmAddProtocols_w, 4, 0, 0, H_XmAddProtocols);
  XM_DEFINE_PROCEDURE(XmRemoveProtocols, gxm_XmRemoveProtocols_w, 4, 0, 0, H_XmRemoveProtocols);
  XM_DEFINE_PROCEDURE(XmAddProtocolCallback, gxm_XmAddProtocolCallback_w, 5, 0, 0, H_XmAddProtocolCallback);
  XM_DEFINE_PROCEDURE(XmRemoveProtocolCallback, gxm_XmRemoveProtocolCallback_w, 5, 0, 0, H_XmRemoveProtocolCallback);
  XM_DEFINE_PROCEDURE(XmActivateProtocol, gxm_XmActivateProtocol_w, 3, 0, 0, H_XmActivateProtocol);
  XM_DEFINE_PROCEDURE(XmDeactivateProtocol, gxm_XmDeactivateProtocol_w, 3, 0, 0, H_XmDeactivateProtocol);
  XM_DEFINE_PROCEDURE(XmSetProtocolHooks, gxm_XmSetProtocolHooks_w, 7, 0, 0, H_XmSetProtocolHooks);
  XM_DEFINE_PROCEDURE(XmCreateCascadeButton, gxm_XmCreateCascadeButton_w, 3, 1, 0, H_XmCreateCascadeButton);
  XM_DEFINE_PROCEDURE(XmCascadeButtonHighlight, gxm_XmCascadeButtonHighlight_w, 2, 0, 0, H_XmCascadeButtonHighlight);
  XM_DEFINE_PROCEDURE(XmCreatePushButtonGadget, gxm_XmCreatePushButtonGadget_w, 3, 1, 0, H_XmCreatePushButtonGadget);
  XM_DEFINE_PROCEDURE(XmCreatePushButton, gxm_XmCreatePushButton_w, 3, 1, 0, H_XmCreatePushButton);
  XM_DEFINE_PROCEDURE(XmCreateCommand, gxm_XmCreateCommand_w, 3, 1, 0, H_XmCreateCommand);
  XM_DEFINE_PROCEDURE(XmCommandGetChild, gxm_XmCommandGetChild_w, 2, 0, 0, H_XmCommandGetChild);
  XM_DEFINE_PROCEDURE(XmCommandSetValue, gxm_XmCommandSetValue_w, 2, 0, 0, H_XmCommandSetValue);
  XM_DEFINE_PROCEDURE(XmCommandAppendValue, gxm_XmCommandAppendValue_w, 2, 0, 0, H_XmCommandAppendValue);
  XM_DEFINE_PROCEDURE(XmCommandError, gxm_XmCommandError_w, 2, 0, 0, H_XmCommandError);
  XM_DEFINE_PROCEDURE(XmCreateCommandDialog, gxm_XmCreateCommandDialog_w, 3, 1, 0, H_XmCreateCommandDialog);
  XM_DEFINE_PROCEDURE(XmMenuPosition, gxm_XmMenuPosition_w, 2, 0, 0, H_XmMenuPosition);
  XM_DEFINE_PROCEDURE(XmCreateRowColumn, gxm_XmCreateRowColumn_w, 3, 1, 0, H_XmCreateRowColumn);
  XM_DEFINE_PROCEDURE(XmCreateWorkArea, gxm_XmCreateWorkArea_w, 3, 1, 0, H_XmCreateWorkArea);
  XM_DEFINE_PROCEDURE(XmCreateRadioBox, gxm_XmCreateRadioBox_w, 3, 1, 0, H_XmCreateRadioBox);
  XM_DEFINE_PROCEDURE(XmCreateOptionMenu, gxm_XmCreateOptionMenu_w, 3, 1, 0, H_XmCreateOptionMenu);
  XM_DEFINE_PROCEDURE(XmOptionLabelGadget, gxm_XmOptionLabelGadget_w, 1, 0, 0, H_XmOptionLabelGadget);
  XM_DEFINE_PROCEDURE(XmOptionButtonGadget, gxm_XmOptionButtonGadget_w, 1, 0, 0, H_XmOptionButtonGadget);
  XM_DEFINE_PROCEDURE(XmCreateMenuBar, gxm_XmCreateMenuBar_w, 3, 1, 0, H_XmCreateMenuBar);
  XM_DEFINE_PROCEDURE(XmCreatePopupMenu, gxm_XmCreatePopupMenu_w, 3, 1, 0, H_XmCreatePopupMenu);
  XM_DEFINE_PROCEDURE(XmCreatePulldownMenu, gxm_XmCreatePulldownMenu_w, 3, 1, 0, H_XmCreatePulldownMenu);
  XM_DEFINE_PROCEDURE(XmGetPostedFromWidget, gxm_XmGetPostedFromWidget_w, 1, 0, 0, H_XmGetPostedFromWidget);
  XM_DEFINE_PROCEDURE(XmGetTearOffControl, gxm_XmGetTearOffControl_w, 1, 0, 0, H_XmGetTearOffControl);
#if (!XM_DISABLE_DEPRECATED)
  XM_DEFINE_PROCEDURE(XmAddToPostFromList, gxm_XmAddToPostFromList_w, 2, 0, 0, H_XmAddToPostFromList);
  XM_DEFINE_PROCEDURE(XmRemoveFromPostFromList, gxm_XmRemoveFromPostFromList_w, 2, 0, 0, H_XmRemoveFromPostFromList);
#endif
  XM_DEFINE_PROCEDURE(XmScaleSetValue, gxm_XmScaleSetValue_w, 2, 0, 0, H_XmScaleSetValue);
  XM_DEFINE_PROCEDURE(XmScaleGetValue, gxm_XmScaleGetValue_w, 1, 0, 0, H_XmScaleGetValue);
  XM_DEFINE_PROCEDURE(XmCreateScale, gxm_XmCreateScale_w, 3, 1, 0, H_XmCreateScale);
#ifndef LESSTIF_VERSION
  XM_DEFINE_PROCEDURE(XmClipboardBeginCopy, gxm_XmClipboardBeginCopy_w, 5, 0, 0, H_XmClipboardBeginCopy);
#endif
  XM_DEFINE_PROCEDURE(XmWidgetGetDisplayRect, gxm_XmWidgetGetDisplayRect_w, 1, 0, 0, H_XmWidgetGetDisplayRect);
  XM_DEFINE_PROCEDURE(XmClipboardStartCopy, gxm_XmClipboardStartCopy_w, 6, 0, 0, H_XmClipboardStartCopy);
  XM_DEFINE_PROCEDURE(XmClipboardCopy, gxm_XmClipboardCopy_w, 7, 0, 0, H_XmClipboardCopy);
  XM_DEFINE_PROCEDURE(XmClipboardEndCopy, gxm_XmClipboardEndCopy_w, 3, 0, 0, H_XmClipboardEndCopy);
  XM_DEFINE_PROCEDURE(XmClipboardCancelCopy, gxm_XmClipboardCancelCopy_w, 3, 0, 0, H_XmClipboardCancelCopy);
  XM_DEFINE_PROCEDURE(XmClipboardWithdrawFormat, gxm_XmClipboardWithdrawFormat_w, 3, 0, 0, H_XmClipboardWithdrawFormat);
  XM_DEFINE_PROCEDURE(XmClipboardCopyByName, gxm_XmClipboardCopyByName_w, 6, 0, 0, H_XmClipboardCopyByName);
  XM_DEFINE_PROCEDURE(XmClipboardUndoCopy, gxm_XmClipboardUndoCopy_w, 2, 0, 0, H_XmClipboardUndoCopy);
  XM_DEFINE_PROCEDURE(XmClipboardLock, gxm_XmClipboardLock_w, 2, 0, 0, H_XmClipboardLock);
  XM_DEFINE_PROCEDURE(XmClipboardUnlock, gxm_XmClipboardUnlock_w, 3, 0, 0, H_XmClipboardUnlock);
  XM_DEFINE_PROCEDURE(XmClipboardStartRetrieve, gxm_XmClipboardStartRetrieve_w, 3, 0, 0, H_XmClipboardStartRetrieve);
  XM_DEFINE_PROCEDURE(XmClipboardEndRetrieve, gxm_XmClipboardEndRetrieve_w, 2, 0, 0, H_XmClipboardEndRetrieve);
  XM_DEFINE_PROCEDURE(XmClipboardRetrieve, gxm_XmClipboardRetrieve_w, 4, 0, 0, H_XmClipboardRetrieve);
  XM_DEFINE_PROCEDURE(XmClipboardInquireCount, gxm_XmClipboardInquireCount_w, 2, 0, 0, H_XmClipboardInquireCount);
  XM_DEFINE_PROCEDURE(XmClipboardInquireFormat, gxm_XmClipboardInquireFormat_w, 4, 0, 0, H_XmClipboardInquireFormat);
  XM_DEFINE_PROCEDURE(XmClipboardInquireLength, gxm_XmClipboardInquireLength_w, 3, 0, 0, H_XmClipboardInquireLength);
  XM_DEFINE_PROCEDURE(XmClipboardInquirePendingItems, gxm_XmClipboardInquirePendingItems_w, 3, 0, 0, H_XmClipboardInquirePendingItems);
  XM_DEFINE_PROCEDURE(XmClipboardRegisterFormat, gxm_XmClipboardRegisterFormat_w, 3, 0, 0, H_XmClipboardRegisterFormat);
#ifndef LESSTIF_VERSION
  XM_DEFINE_PROCEDURE(XmGetXmScreen, gxm_XmGetXmScreen_w, 1, 0, 0, H_XmGetXmScreen);
#endif
  XM_DEFINE_PROCEDURE(XmCreateScrollBar, gxm_XmCreateScrollBar_w, 3, 1, 0, H_XmCreateScrollBar);
  XM_DEFINE_PROCEDURE(XmScrollBarGetValues, gxm_XmScrollBarGetValues_w, 1, 0, 0, H_XmScrollBarGetValues);
  XM_DEFINE_PROCEDURE(XmScrollBarSetValues, gxm_XmScrollBarSetValues_w, 6, 0, 0, H_XmScrollBarSetValues);
  XM_DEFINE_PROCEDURE(XmCreateDialogShell, gxm_XmCreateDialogShell_w, 3, 1, 0, H_XmCreateDialogShell);
  XM_DEFINE_PROCEDURE(XmCreateScrolledWindow, gxm_XmCreateScrolledWindow_w, 3, 1, 0, H_XmCreateScrolledWindow);
  XM_DEFINE_PROCEDURE(XmScrollVisible, gxm_XmScrollVisible_w, 4, 0, 0, H_XmScrollVisible);
  XM_DEFINE_PROCEDURE(XmGetDragContext, gxm_XmGetDragContext_w, 2, 0, 0, H_XmGetDragContext);
  XM_DEFINE_PROCEDURE(XmGetXmDisplay, gxm_XmGetXmDisplay_w, 1, 0, 0, H_XmGetXmDisplay);
  XM_DEFINE_PROCEDURE(XmSelectionBoxGetChild, gxm_XmSelectionBoxGetChild_w, 2, 0, 0, H_XmSelectionBoxGetChild);
  XM_DEFINE_PROCEDURE(XmCreateSelectionBox, gxm_XmCreateSelectionBox_w, 3, 1, 0, H_XmCreateSelectionBox);
  XM_DEFINE_PROCEDURE(XmCreateSelectionDialog, gxm_XmCreateSelectionDialog_w, 3, 1, 0, H_XmCreateSelectionDialog);
  XM_DEFINE_PROCEDURE(XmCreatePromptDialog, gxm_XmCreatePromptDialog_w, 3, 1, 0, H_XmCreatePromptDialog);
  XM_DEFINE_PROCEDURE(XmDragStart, gxm_XmDragStart_w, 3, 1, 0, H_XmDragStart);
  XM_DEFINE_PROCEDURE(XmDragCancel, gxm_XmDragCancel_w, 1, 0, 0, H_XmDragCancel);
  XM_DEFINE_PROCEDURE(XmTargetsAreCompatible, gxm_XmTargetsAreCompatible_w, 5, 0, 0, H_XmTargetsAreCompatible);
  XM_DEFINE_PROCEDURE(XmCreateSeparatorGadget, gxm_XmCreateSeparatorGadget_w, 3, 1, 0, H_XmCreateSeparatorGadget);
  XM_DEFINE_PROCEDURE(XmCreateDragIcon, gxm_XmCreateDragIcon_w, 3, 1, 0, H_XmCreateDragIcon);
  XM_DEFINE_PROCEDURE(XmCreateSeparator, gxm_XmCreateSeparator_w, 3, 1, 0, H_XmCreateSeparator);
  XM_DEFINE_PROCEDURE(XmCreateDrawingArea, gxm_XmCreateDrawingArea_w, 3, 1, 0, H_XmCreateDrawingArea);
  XM_DEFINE_PROCEDURE(XmCreateDrawnButton, gxm_XmCreateDrawnButton_w, 3, 1, 0, H_XmCreateDrawnButton);
  XM_DEFINE_PROCEDURE(XmDropSiteRegister, gxm_XmDropSiteRegister_w, 2, 1, 0, H_XmDropSiteRegister);
  XM_DEFINE_PROCEDURE(XmDropSiteUnregister, gxm_XmDropSiteUnregister_w, 1, 0, 0, H_XmDropSiteUnregister);
  XM_DEFINE_PROCEDURE(XmDropSiteStartUpdate, gxm_XmDropSiteStartUpdate_w, 1, 0, 0, H_XmDropSiteStartUpdate);
  XM_DEFINE_PROCEDURE(XmDropSiteUpdate, gxm_XmDropSiteUpdate_w, 2, 1, 0, H_XmDropSiteUpdate);
  XM_DEFINE_PROCEDURE(XmDropSiteEndUpdate, gxm_XmDropSiteEndUpdate_w, 1, 0, 0, H_XmDropSiteEndUpdate);
  XM_DEFINE_PROCEDURE(XmDropSiteRetrieve, gxm_XmDropSiteRetrieve_w, 2, 1, 0, H_XmDropSiteRetrieve);
  XM_DEFINE_PROCEDURE(XmDropSiteQueryStackingOrder, gxm_XmDropSiteQueryStackingOrder_w, 1, 0, 0, H_XmDropSiteQueryStackingOrder);
  XM_DEFINE_PROCEDURE(XmDropSiteConfigureStackingOrder, gxm_XmDropSiteConfigureStackingOrder_w, 3, 0, 0, H_XmDropSiteConfigureStackingOrder);
  XM_DEFINE_PROCEDURE(XmDropTransferStart, gxm_XmDropTransferStart_w, 2, 1, 0, H_XmDropTransferStart);
  XM_DEFINE_PROCEDURE(XmDropTransferAdd, gxm_XmDropTransferAdd_w, 2, 0, 0, H_XmDropTransferAdd);
  XM_DEFINE_PROCEDURE(XmTextFieldGetString, gxm_XmTextFieldGetString_w, 1, 0, 0, H_XmTextFieldGetString);
  XM_DEFINE_PROCEDURE(XmTextFieldGetSubstring, gxm_XmTextFieldGetSubstring_w, 3, 0, 0, H_XmTextFieldGetSubstring);
  XM_DEFINE_PROCEDURE(XmTextFieldGetLastPosition, gxm_XmTextFieldGetLastPosition_w, 1, 0, 0, H_XmTextFieldGetLastPosition);
  XM_DEFINE_PROCEDURE(XmTextFieldSetString, gxm_XmTextFieldSetString_w, 2, 0, 0, H_XmTextFieldSetString);
  XM_DEFINE_PROCEDURE(XmTextFieldReplace, gxm_XmTextFieldReplace_w, 4, 0, 0, H_XmTextFieldReplace);
  XM_DEFINE_PROCEDURE(XmTextFieldInsert, gxm_XmTextFieldInsert_w, 3, 0, 0, H_XmTextFieldInsert);
#if MOTIF_2
  XM_DEFINE_PROCEDURE(XmTextFieldSetAddMode, gxm_XmTextFieldSetAddMode_w, 2, 0, 0, H_XmTextFieldSetAddMode);
  XM_DEFINE_PROCEDURE(XmTextFieldGetAddMode, gxm_XmTextFieldGetAddMode_w, 1, 0, 0, NULL);
#endif
  XM_DEFINE_PROCEDURE(XmTextFieldGetEditable, gxm_XmTextFieldGetEditable_w, 1, 0, 0, H_XmTextFieldGetEditable);
  XM_DEFINE_PROCEDURE(XmTextFieldSetEditable, gxm_XmTextFieldSetEditable_w, 2, 0, 0, H_XmTextFieldSetEditable);
  XM_DEFINE_PROCEDURE(XmTextFieldGetMaxLength, gxm_XmTextFieldGetMaxLength_w, 1, 0, 0, H_XmTextFieldGetMaxLength);
  XM_DEFINE_PROCEDURE(XmTextFieldSetMaxLength, gxm_XmTextFieldSetMaxLength_w, 2, 0, 0, H_XmTextFieldSetMaxLength);
  XM_DEFINE_PROCEDURE(XmTextFieldGetCursorPosition, gxm_XmTextFieldGetCursorPosition_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmTextFieldGetInsertionPosition, gxm_XmTextFieldGetInsertionPosition_w, 1, 0, 0, H_XmTextFieldGetInsertionPosition);
  XM_DEFINE_PROCEDURE(XmTextFieldSetCursorPosition, gxm_XmTextFieldSetCursorPosition_w, 2, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmTextFieldSetInsertionPosition, gxm_XmTextFieldSetInsertionPosition_w, 2, 0, 0, H_XmTextFieldSetInsertionPosition);
  XM_DEFINE_PROCEDURE(XmTextFieldGetSelectionPosition, gxm_XmTextFieldGetSelectionPosition_w, 1, 0, 0, H_XmTextFieldGetSelectionPosition);
  XM_DEFINE_PROCEDURE(XmTextFieldGetSelection, gxm_XmTextFieldGetSelection_w, 1, 0, 0, H_XmTextFieldGetSelection);
  XM_DEFINE_PROCEDURE(XmTextFieldRemove, gxm_XmTextFieldRemove_w, 1, 0, 0, H_XmTextFieldRemove);
  XM_DEFINE_PROCEDURE(XmTextFieldCopy, gxm_XmTextFieldCopy_w, 2, 0, 0, H_XmTextFieldCopy);
  XM_DEFINE_PROCEDURE(XmTextFieldCut, gxm_XmTextFieldCut_w, 2, 0, 0, H_XmTextFieldCut);
  XM_DEFINE_PROCEDURE(XmTextFieldPaste, gxm_XmTextFieldPaste_w, 1, 0, 0, H_XmTextFieldPaste);
  XM_DEFINE_PROCEDURE(XmTextFieldClearSelection, gxm_XmTextFieldClearSelection_w, 2, 0, 0, H_XmTextFieldClearSelection);
  XM_DEFINE_PROCEDURE(XmTextFieldSetSelection, gxm_XmTextFieldSetSelection_w, 4, 0, 0, H_XmTextFieldSetSelection);
  XM_DEFINE_PROCEDURE(XmTextFieldXYToPos, gxm_XmTextFieldXYToPos_w, 3, 0, 0, H_XmTextFieldXYToPos);
  XM_DEFINE_PROCEDURE(XmTextFieldPosToXY, gxm_XmTextFieldPosToXY_w, 2, 0, 0, H_XmTextFieldPosToXY);
  XM_DEFINE_PROCEDURE(XmTextFieldShowPosition, gxm_XmTextFieldShowPosition_w, 2, 0, 0, H_XmTextFieldShowPosition);
  XM_DEFINE_PROCEDURE(XmTextFieldSetHighlight, gxm_XmTextFieldSetHighlight_w, 4, 0, 0, H_XmTextFieldSetHighlight);
  XM_DEFINE_PROCEDURE(XmTextFieldGetBaseline, gxm_XmTextFieldGetBaseline_w, 1, 0, 0, H_XmTextFieldGetBaseline);
  XM_DEFINE_PROCEDURE(XmCreateTextField, gxm_XmCreateTextField_w, 3, 1, 0, H_XmCreateTextField);
  XM_DEFINE_PROCEDURE(XmFileSelectionBoxGetChild, gxm_XmFileSelectionBoxGetChild_w, 2, 0, 0, H_XmFileSelectionBoxGetChild);
  XM_DEFINE_PROCEDURE(XmFileSelectionDoSearch, gxm_XmFileSelectionDoSearch_w, 2, 0, 0, H_XmFileSelectionDoSearch);
  XM_DEFINE_PROCEDURE(XmCreateFileSelectionBox, gxm_XmCreateFileSelectionBox_w, 3, 1, 0, H_XmCreateFileSelectionBox);
  XM_DEFINE_PROCEDURE(XmCreateFileSelectionDialog, gxm_XmCreateFileSelectionDialog_w, 3, 1, 0, H_XmCreateFileSelectionDialog);
  XM_DEFINE_PROCEDURE(XmTextSetHighlight, gxm_XmTextSetHighlight_w, 4, 0, 0, H_XmTextSetHighlight);
  XM_DEFINE_PROCEDURE(XmCreateScrolledText, gxm_XmCreateScrolledText_w, 3, 1, 0, H_XmCreateScrolledText);
  XM_DEFINE_PROCEDURE(XmCreateText, gxm_XmCreateText_w, 3, 1, 0, H_XmCreateText);
  XM_DEFINE_PROCEDURE(XmTextGetSubstring, gxm_XmTextGetSubstring_w, 3, 0, 0, H_XmTextGetSubstring);
  XM_DEFINE_PROCEDURE(XmTextGetString, gxm_XmTextGetString_w, 1, 0, 0, H_XmTextGetString);
  XM_DEFINE_PROCEDURE(XmTextGetLastPosition, gxm_XmTextGetLastPosition_w, 1, 0, 0, H_XmTextGetLastPosition);
  XM_DEFINE_PROCEDURE(XmTextSetString, gxm_XmTextSetString_w, 2, 0, 0, H_XmTextSetString);
  XM_DEFINE_PROCEDURE(XmTextReplace, gxm_XmTextReplace_w, 4, 0, 0, H_XmTextReplace);
  XM_DEFINE_PROCEDURE(XmTextInsert, gxm_XmTextInsert_w, 3, 0, 0, H_XmTextInsert);
#if MOTIF_2
  XM_DEFINE_PROCEDURE(XmTextSetAddMode, gxm_XmTextSetAddMode_w, 2, 0, 0, H_XmTextSetAddMode);
  XM_DEFINE_PROCEDURE(XmTextGetAddMode, gxm_XmTextGetAddMode_w, 1, 0, 0, NULL);
#endif
  XM_DEFINE_PROCEDURE(XmTextGetEditable, gxm_XmTextGetEditable_w, 1, 0, 0, H_XmTextGetEditable);
  XM_DEFINE_PROCEDURE(XmTextSetEditable, gxm_XmTextSetEditable_w, 2, 0, 0, H_XmTextSetEditable);
  XM_DEFINE_PROCEDURE(XmTextGetMaxLength, gxm_XmTextGetMaxLength_w, 1, 0, 0, H_XmTextGetMaxLength);
  XM_DEFINE_PROCEDURE(XmTextSetMaxLength, gxm_XmTextSetMaxLength_w, 2, 0, 0, H_XmTextSetMaxLength);
  XM_DEFINE_PROCEDURE(XmTextGetTopCharacter, gxm_XmTextGetTopCharacter_w, 1, 0, 0, H_XmTextGetTopCharacter);
  XM_DEFINE_PROCEDURE(XmTextSetTopCharacter, gxm_XmTextSetTopCharacter_w, 2, 0, 0, H_XmTextSetTopCharacter);
  XM_DEFINE_PROCEDURE(XmTextGetCursorPosition, gxm_XmTextGetCursorPosition_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmTextGetInsertionPosition, gxm_XmTextGetInsertionPosition_w, 1, 0, 0, H_XmTextGetInsertionPosition);
  XM_DEFINE_PROCEDURE(XmTextSetInsertionPosition, gxm_XmTextSetInsertionPosition_w, 2, 0, 0, H_XmTextSetInsertionPosition);
  XM_DEFINE_PROCEDURE(XmTextSetCursorPosition, gxm_XmTextSetCursorPosition_w, 2, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmTextRemove, gxm_XmTextRemove_w, 1, 0, 0, H_XmTextRemove);
  XM_DEFINE_PROCEDURE(XmTextCopy, gxm_XmTextCopy_w, 2, 0, 0, H_XmTextCopy);
  XM_DEFINE_PROCEDURE(XmTextCut, gxm_XmTextCut_w, 2, 0, 0, H_XmTextCut);
  XM_DEFINE_PROCEDURE(XmTextPaste, gxm_XmTextPaste_w, 1, 0, 0, H_XmTextPaste);
  XM_DEFINE_PROCEDURE(XmTextGetSelection, gxm_XmTextGetSelection_w, 1, 0, 0, H_XmTextGetSelection);
  XM_DEFINE_PROCEDURE(XmTextSetSelection, gxm_XmTextSetSelection_w, 4, 0, 0, H_XmTextSetSelection);
  XM_DEFINE_PROCEDURE(XmTextClearSelection, gxm_XmTextClearSelection_w, 2, 0, 0, H_XmTextClearSelection);
  XM_DEFINE_PROCEDURE(XmTextGetSelectionPosition, gxm_XmTextGetSelectionPosition_w, 1, 0, 0, H_XmTextGetSelectionPosition);
  XM_DEFINE_PROCEDURE(XmTextXYToPos, gxm_XmTextXYToPos_w, 3, 0, 0, H_XmTextXYToPos);
  XM_DEFINE_PROCEDURE(XmTextPosToXY, gxm_XmTextPosToXY_w, 2, 0, 0, H_XmTextPosToXY);
  XM_DEFINE_PROCEDURE(XmTextGetSource, gxm_XmTextGetSource_w, 1, 0, 0, H_XmTextGetSource);
  XM_DEFINE_PROCEDURE(XmTextSetSource, gxm_XmTextSetSource_w, 4, 0, 0, H_XmTextSetSource);
  XM_DEFINE_PROCEDURE(XmTextShowPosition, gxm_XmTextShowPosition_w, 2, 0, 0, H_XmTextShowPosition);
  XM_DEFINE_PROCEDURE(XmTextScroll, gxm_XmTextScroll_w, 2, 0, 0, H_XmTextScroll);
  XM_DEFINE_PROCEDURE(XmTextGetBaseline, gxm_XmTextGetBaseline_w, 1, 0, 0, H_XmTextGetBaseline);
  XM_DEFINE_PROCEDURE(XmTextDisableRedisplay, gxm_XmTextDisableRedisplay_w, 1, 0, 0, H_XmTextDisableRedisplay);
  XM_DEFINE_PROCEDURE(XmTextEnableRedisplay, gxm_XmTextEnableRedisplay_w, 1, 0, 0, H_XmTextEnableRedisplay);
  XM_DEFINE_PROCEDURE(XmTextFindString, gxm_XmTextFindString_w, 4, 0, 0, H_XmTextFindString);
  XM_DEFINE_PROCEDURE(XmCreateForm, gxm_XmCreateForm_w, 3, 1, 0, H_XmCreateForm);
  XM_DEFINE_PROCEDURE(XmCreateFormDialog, gxm_XmCreateFormDialog_w, 3, 1, 0, H_XmCreateFormDialog);
  XM_DEFINE_PROCEDURE(XmCreateFrame, gxm_XmCreateFrame_w, 3, 1, 0, H_XmCreateFrame);
  XM_DEFINE_PROCEDURE(XmToggleButtonGadgetGetState, gxm_XmToggleButtonGadgetGetState_w, 1, 0, 0, H_XmToggleButtonGadgetGetState);
  XM_DEFINE_PROCEDURE(XmToggleButtonGadgetSetState, gxm_XmToggleButtonGadgetSetState_w, 3, 0, 0, H_XmToggleButtonGadgetSetState);
  XM_DEFINE_PROCEDURE(XmCreateToggleButtonGadget, gxm_XmCreateToggleButtonGadget_w, 3, 1, 0, H_XmCreateToggleButtonGadget);
  XM_DEFINE_PROCEDURE(XmToggleButtonGetState, gxm_XmToggleButtonGetState_w, 1, 0, 0, H_XmToggleButtonGetState);
  XM_DEFINE_PROCEDURE(XmToggleButtonSetState, gxm_XmToggleButtonSetState_w, 3, 0, 0, H_XmToggleButtonSetState);
  XM_DEFINE_PROCEDURE(XmCreateToggleButton, gxm_XmCreateToggleButton_w, 3, 1, 0, H_XmCreateToggleButton);
  XM_DEFINE_PROCEDURE(XmCreateLabelGadget, gxm_XmCreateLabelGadget_w, 3, 1, 0, H_XmCreateLabelGadget);
  XM_DEFINE_PROCEDURE(XmCreateLabel, gxm_XmCreateLabel_w, 3, 1, 0, H_XmCreateLabel);
  XM_DEFINE_PROCEDURE(XmIsMotifWMRunning, gxm_XmIsMotifWMRunning_w, 1, 0, 0, H_XmIsMotifWMRunning);
  XM_DEFINE_PROCEDURE(XmListAddItem, gxm_XmListAddItem_w, 3, 0, 0, H_XmListAddItem);
  XM_DEFINE_PROCEDURE(XmListAddItems, gxm_XmListAddItems_w, 4, 0, 0, H_XmListAddItems);
  XM_DEFINE_PROCEDURE(XmListAddItemsUnselected, gxm_XmListAddItemsUnselected_w, 4, 0, 0, H_XmListAddItemsUnselected);
  XM_DEFINE_PROCEDURE(XmListAddItemUnselected, gxm_XmListAddItemUnselected_w, 3, 0, 0, H_XmListAddItemUnselected);
  XM_DEFINE_PROCEDURE(XmListDeleteItem, gxm_XmListDeleteItem_w, 2, 0, 0, H_XmListDeleteItem);
  XM_DEFINE_PROCEDURE(XmListDeleteItems, gxm_XmListDeleteItems_w, 3, 0, 0, H_XmListDeleteItems);
  XM_DEFINE_PROCEDURE(XmListDeletePositions, gxm_XmListDeletePositions_w, 3, 0, 0, H_XmListDeletePositions);
  XM_DEFINE_PROCEDURE(XmListDeletePos, gxm_XmListDeletePos_w, 2, 0, 0, H_XmListDeletePos);
  XM_DEFINE_PROCEDURE(XmListDeleteItemsPos, gxm_XmListDeleteItemsPos_w, 3, 0, 0, H_XmListDeleteItemsPos);
  XM_DEFINE_PROCEDURE(XmListDeleteAllItems, gxm_XmListDeleteAllItems_w, 1, 0, 0, H_XmListDeleteAllItems);
  XM_DEFINE_PROCEDURE(XmListReplaceItems, gxm_XmListReplaceItems_w, 4, 0, 0, H_XmListReplaceItems);
  XM_DEFINE_PROCEDURE(XmListReplaceItemsPos, gxm_XmListReplaceItemsPos_w, 4, 0, 0, H_XmListReplaceItemsPos);
  XM_DEFINE_PROCEDURE(XmListReplaceItemsUnselected, gxm_XmListReplaceItemsUnselected_w, 4, 0, 0, H_XmListReplaceItemsUnselected);
  XM_DEFINE_PROCEDURE(XmListReplaceItemsPosUnselected, gxm_XmListReplaceItemsPosUnselected_w, 4, 0, 0, H_XmListReplaceItemsPosUnselected);
  XM_DEFINE_PROCEDURE(XmListReplacePositions, gxm_XmListReplacePositions_w, 4, 0, 0, H_XmListReplacePositions);
  XM_DEFINE_PROCEDURE(XmListSelectItem, gxm_XmListSelectItem_w, 3, 0, 0, H_XmListSelectItem);
  XM_DEFINE_PROCEDURE(XmListSelectPos, gxm_XmListSelectPos_w, 3, 0, 0, H_XmListSelectPos);
  XM_DEFINE_PROCEDURE(XmListDeselectItem, gxm_XmListDeselectItem_w, 2, 0, 0, H_XmListDeselectItem);
  XM_DEFINE_PROCEDURE(XmListDeselectPos, gxm_XmListDeselectPos_w, 2, 0, 0, H_XmListDeselectPos);
  XM_DEFINE_PROCEDURE(XmListDeselectAllItems, gxm_XmListDeselectAllItems_w, 1, 0, 0, H_XmListDeselectAllItems);
  XM_DEFINE_PROCEDURE(XmListSetPos, gxm_XmListSetPos_w, 2, 0, 0, H_XmListSetPos);
  XM_DEFINE_PROCEDURE(XmListSetBottomPos, gxm_XmListSetBottomPos_w, 2, 0, 0, H_XmListSetBottomPos);
  XM_DEFINE_PROCEDURE(XmListSetItem, gxm_XmListSetItem_w, 2, 0, 0, H_XmListSetItem);
  XM_DEFINE_PROCEDURE(XmListSetBottomItem, gxm_XmListSetBottomItem_w, 2, 0, 0, H_XmListSetBottomItem);
  XM_DEFINE_PROCEDURE(XmListSetAddMode, gxm_XmListSetAddMode_w, 2, 0, 0, H_XmListSetAddMode);
  XM_DEFINE_PROCEDURE(XmListItemExists, gxm_XmListItemExists_w, 2, 0, 0, H_XmListItemExists);
  XM_DEFINE_PROCEDURE(XmListItemPos, gxm_XmListItemPos_w, 2, 0, 0, H_XmListItemPos);
  XM_DEFINE_PROCEDURE(XmListGetKbdItemPos, gxm_XmListGetKbdItemPos_w, 1, 0, 0, H_XmListGetKbdItemPos);
  XM_DEFINE_PROCEDURE(XmListSetKbdItemPos, gxm_XmListSetKbdItemPos_w, 2, 0, 0, H_XmListSetKbdItemPos);
  XM_DEFINE_PROCEDURE(XmListYToPos, gxm_XmListYToPos_w, 2, 0, 0, H_XmListYToPos);
  XM_DEFINE_PROCEDURE(XmListPosToBounds, gxm_XmListPosToBounds_w, 2, 0, 0, H_XmListPosToBounds);
  XM_DEFINE_PROCEDURE(XmListGetMatchPos, gxm_XmListGetMatchPos_w, 2, 0, 0, H_XmListGetMatchPos);
  XM_DEFINE_PROCEDURE(XmListSetHorizPos, gxm_XmListSetHorizPos_w, 2, 0, 0, H_XmListSetHorizPos);
  XM_DEFINE_PROCEDURE(XmListUpdateSelectedList, gxm_XmListUpdateSelectedList_w, 1, 0, 0, H_XmListUpdateSelectedList);
  XM_DEFINE_PROCEDURE(XmListPosSelected, gxm_XmListPosSelected_w, 2, 0, 0, H_XmListPosSelected);
  XM_DEFINE_PROCEDURE(XmCreateList, gxm_XmCreateList_w, 3, 1, 0, H_XmCreateList);
  XM_DEFINE_PROCEDURE(XmCreateScrolledList, gxm_XmCreateScrolledList_w, 3, 1, 0, H_XmCreateScrolledList);
  XM_DEFINE_PROCEDURE(XmTranslateKey, gxm_XmTranslateKey_w, 3, 0, 0, H_XmTranslateKey);
  XM_DEFINE_PROCEDURE(XmCreateMainWindow, gxm_XmCreateMainWindow_w, 3, 1, 0, H_XmCreateMainWindow);
  XM_DEFINE_PROCEDURE(XmInstallImage, gxm_XmInstallImage_w, 2, 0, 0, H_XmInstallImage);
  XM_DEFINE_PROCEDURE(XmUninstallImage, gxm_XmUninstallImage_w, 1, 0, 0, H_XmUninstallImage);
  XM_DEFINE_PROCEDURE(XmGetPixmap, gxm_XmGetPixmap_w, 4, 0, 0, H_XmGetPixmap);
  XM_DEFINE_PROCEDURE(XmGetPixmapByDepth, gxm_XmGetPixmapByDepth_w, 5, 0, 0, H_XmGetPixmapByDepth);
  XM_DEFINE_PROCEDURE(XmDestroyPixmap, gxm_XmDestroyPixmap_w, 2, 0, 0, H_XmDestroyPixmap);
  XM_DEFINE_PROCEDURE(XmUpdateDisplay, gxm_XmUpdateDisplay_w, 1, 0, 0, H_XmUpdateDisplay);
  XM_DEFINE_PROCEDURE(XmWidgetGetBaselines, gxm_XmWidgetGetBaselines_w, 1, 0, 0, H_XmWidgetGetBaselines);
  XM_DEFINE_PROCEDURE(XmRegisterSegmentEncoding, gxm_XmRegisterSegmentEncoding_w, 2, 0, 0, H_XmRegisterSegmentEncoding);
  XM_DEFINE_PROCEDURE(XmMapSegmentEncoding, gxm_XmMapSegmentEncoding_w, 1, 0, 0, H_XmMapSegmentEncoding);
  XM_DEFINE_PROCEDURE(XmCvtCTToXmString, gxm_XmCvtCTToXmString_w, 1, 0, 0, H_XmCvtCTToXmString);
  XM_DEFINE_PROCEDURE(XmCvtXmStringToCT, gxm_XmCvtXmStringToCT_w, 1, 0, 0, H_XmCvtXmStringToCT);
  XM_DEFINE_PROCEDURE(XmConvertUnits, gxm_XmConvertUnits_w, 5, 0, 0, H_XmConvertUnits);
  XM_DEFINE_PROCEDURE(XmCreateSimpleMenuBar, gxm_XmCreateSimpleMenuBar_w, 3, 1, 0, H_XmCreateSimpleMenuBar);
  XM_DEFINE_PROCEDURE(XmCreateSimplePopupMenu, gxm_XmCreateSimplePopupMenu_w, 3, 1, 0, H_XmCreateSimplePopupMenu);
  XM_DEFINE_PROCEDURE(XmCreateSimplePulldownMenu, gxm_XmCreateSimplePulldownMenu_w, 3, 1, 0, H_XmCreateSimplePulldownMenu);
  XM_DEFINE_PROCEDURE(XmCreateSimpleOptionMenu, gxm_XmCreateSimpleOptionMenu_w, 3, 1, 0, H_XmCreateSimpleOptionMenu);
  XM_DEFINE_PROCEDURE(XmCreateSimpleRadioBox, gxm_XmCreateSimpleRadioBox_w, 3, 1, 0, H_XmCreateSimpleRadioBox);
  XM_DEFINE_PROCEDURE(XmCreateSimpleCheckBox, gxm_XmCreateSimpleCheckBox_w, 3, 1, 0, H_XmCreateSimpleCheckBox);
  XM_DEFINE_PROCEDURE(XmVaCreateSimpleMenuBar, gxm_XmVaCreateSimpleMenuBar_w, 3, 0, 0, H_XmVaCreateSimpleMenuBar);
  XM_DEFINE_PROCEDURE(XmVaCreateSimplePopupMenu, gxm_XmVaCreateSimplePopupMenu_w, 4, 0, 0, H_XmVaCreateSimplePopupMenu);
  XM_DEFINE_PROCEDURE(XmVaCreateSimplePulldownMenu, gxm_XmVaCreateSimplePulldownMenu_w, 5, 0, 0, H_XmVaCreateSimplePulldownMenu);
  XM_DEFINE_PROCEDURE(XmVaCreateSimpleOptionMenu, gxm_XmVaCreateSimpleOptionMenu_w, 7, 0, 0, H_XmVaCreateSimpleOptionMenu);
  XM_DEFINE_PROCEDURE(XmVaCreateSimpleRadioBox, gxm_XmVaCreateSimpleRadioBox_w, 5, 0, 0, H_XmVaCreateSimpleRadioBox);
  XM_DEFINE_PROCEDURE(XmVaCreateSimpleCheckBox, gxm_XmVaCreateSimpleCheckBox_w, 4, 0, 0, H_XmVaCreateSimpleCheckBox);
  XM_DEFINE_PROCEDURE(XmTrackingEvent, gxm_XmTrackingEvent_w, 3, 0, 0, H_XmTrackingEvent);
  XM_DEFINE_PROCEDURE(XmSetColorCalculation, gxm_XmSetColorCalculation_w, 1, 0, 0, H_XmSetColorCalculation);
  XM_DEFINE_PROCEDURE(XmGetColorCalculation, gxm_XmGetColorCalculation_w, 0, 0, 0, H_XmGetColorCalculation);
  XM_DEFINE_PROCEDURE(XmGetColors, gxm_XmGetColors_w, 3, 0, 0, H_XmGetColors);
  XM_DEFINE_PROCEDURE(XmChangeColor, gxm_XmChangeColor_w, 2, 0, 0, H_XmChangeColor);
  XM_DEFINE_PROCEDURE(XmStringCreate, gxm_XmStringCreate_w, 2, 0, 0, H_XmStringCreate);
  XM_DEFINE_PROCEDURE(XmStringCreateLocalized, gxm_XmStringCreateLocalized_w, 1, 0, 0, H_XmStringCreateLocalized);
  XM_DEFINE_PROCEDURE(XmStringDirectionCreate, gxm_XmStringDirectionCreate_w, 1, 0, 0, H_XmStringDirectionCreate);
  XM_DEFINE_PROCEDURE(XmStringSeparatorCreate, gxm_XmStringSeparatorCreate_w, 0, 0, 0, H_XmStringSeparatorCreate);
  XM_DEFINE_PROCEDURE(XmStringInitContext, gxm_XmStringInitContext_w, 1, 0, 0, H_XmStringInitContext);
  XM_DEFINE_PROCEDURE(XmStringFreeContext, gxm_XmStringFreeContext_w, 1, 0, 0, H_XmStringFreeContext);

#if MOTIF_2
  XM_DEFINE_PROCEDURE(XmCvtXmStringToByteStream, gxm_XmCvtXmStringToByteStream_w, 1, 0, 0, H_XmCvtXmStringToByteStream);
  XM_DEFINE_PROCEDURE(XmCvtByteStreamToXmString, gxm_XmCvtByteStreamToXmString_w, 1, 0, 0, H_XmCvtByteStreamToXmString);
  XM_DEFINE_PROCEDURE(XmStringByteStreamLength, gxm_XmStringByteStreamLength_w, 1, 0, 0, H_XmStringByteStreamLength);
  XM_DEFINE_PROCEDURE(XmStringConcatAndFree, gxm_XmStringConcatAndFree_w, 2, 0, 0, H_XmStringConcatAndFree);
  XM_DEFINE_PROCEDURE(XmStringIsVoid, gxm_XmStringIsVoid_w, 1, 0, 0, H_XmStringIsVoid);
  XM_DEFINE_PROCEDURE(XmStringPeekNextTriple, gxm_XmStringPeekNextTriple_w, 1, 0, 0, H_XmStringPeekNextTriple);
  XM_DEFINE_PROCEDURE(XmStringGetNextTriple, gxm_XmStringGetNextTriple_w, 1, 0, 0, H_XmStringGetNextTriple);
  XM_DEFINE_PROCEDURE(XmStringComponentCreate, gxm_XmStringComponentCreate_w, 3, 0, 0, H_XmStringComponentCreate);
  XM_DEFINE_PROCEDURE(XmStringUnparse, gxm_XmStringUnparse_w, 7, 0, 0, H_XmStringUnparse);
  XM_DEFINE_PROCEDURE(XmStringParseText, gxm_XmStringParseText_w, 7, 0, 0, H_XmStringParseText);
  XM_DEFINE_PROCEDURE(XmStringToXmStringTable, gxm_XmStringToXmStringTable_w, 2, 0, 0, H_XmStringToXmStringTable);
  XM_DEFINE_PROCEDURE(XmStringTableToXmString, gxm_XmStringTableToXmString_w, 3, 0, 0, H_XmStringTableToXmString);
  XM_DEFINE_PROCEDURE(XmStringTableUnparse, gxm_XmStringTableUnparse_w, 8, 0, 0, H_XmStringTableUnparse);
  XM_DEFINE_PROCEDURE(XmStringTableParseStringArray, gxm_XmStringTableParseStringArray_w, 7, 0, 0, H_XmStringTableParseStringArray);
  XM_DEFINE_PROCEDURE(XmDirectionToStringDirection, gxm_XmDirectionToStringDirection_w, 1, 0, 0, H_XmDirectionToStringDirection);
  XM_DEFINE_PROCEDURE(XmStringDirectionToDirection, gxm_XmStringDirectionToDirection_w, 1, 0, 0, H_XmStringDirectionToDirection);
  XM_DEFINE_PROCEDURE(XmStringGenerate, gxm_XmStringGenerate_w, 4, 0, 0, H_XmStringGenerate);
  XM_DEFINE_PROCEDURE(XmStringPutRendition, gxm_XmStringPutRendition_w, 2, 0, 0, H_XmStringPutRendition);
  XM_DEFINE_PROCEDURE(XmParseMappingCreate, gxm_XmParseMappingCreate_w, 1, 1, 0, H_XmParseMappingCreate);
  XM_DEFINE_PROCEDURE(XmParseMappingSetValues, gxm_XmParseMappingSetValues_w, 2, 1, 0, H_XmParseMappingSetValues);
  XM_DEFINE_PROCEDURE(XmParseMappingGetValues, gxm_XmParseMappingGetValues_w, 2, 1, 0, H_XmParseMappingGetValues);
  XM_DEFINE_PROCEDURE(XmParseMappingFree, gxm_XmParseMappingFree_w, 1, 0, 0, H_XmParseMappingFree);
  XM_DEFINE_PROCEDURE(XmParseTableFree, gxm_XmParseTableFree_w, 1, 1, 0, H_XmParseTableFree);
  XM_DEFINE_PROCEDURE(XmStringTableProposeTablist, gxm_XmStringTableProposeTablist_w, 5, 0, 0, H_XmStringTableProposeTablist);
  XM_DEFINE_PROCEDURE(XmTabSetValue, gxm_XmTabSetValue_w, 2, 0, 0, H_XmTabSetValue);
  XM_DEFINE_PROCEDURE(XmTabGetValues, gxm_XmTabGetValues_w, 1, 0, 0, H_XmTabGetValues);
  XM_DEFINE_PROCEDURE(XmTabFree, gxm_XmTabFree_w, 1, 0, 0, H_XmTabFree);
  XM_DEFINE_PROCEDURE(XmTabListFree, gxm_XmTabListFree_w, 1, 0, 0, H_XmTabListFree);
  XM_DEFINE_PROCEDURE(XmTabCreate, gxm_XmTabCreate_w, 5, 0, 0, H_XmTabCreate);
  XM_DEFINE_PROCEDURE(XmTabListTabCount, gxm_XmTabListTabCount_w, 1, 0, 0, H_XmTabListTabCount);
  XM_DEFINE_PROCEDURE(XmTabListRemoveTabs, gxm_XmTabListRemoveTabs_w, 2, 1, 0, H_XmTabListRemoveTabs);
  XM_DEFINE_PROCEDURE(XmTabListReplacePositions, gxm_XmTabListReplacePositions_w, 3, 1, 0, H_XmTabListReplacePositions);
  XM_DEFINE_PROCEDURE(XmTabListGetTab, gxm_XmTabListGetTab_w, 2, 0, 0, H_XmTabListGetTab);
  XM_DEFINE_PROCEDURE(XmTabListCopy, gxm_XmTabListCopy_w, 3, 0, 0, H_XmTabListCopy);
  XM_DEFINE_PROCEDURE(XmTabListInsertTabs, gxm_XmTabListInsertTabs_w, 4, 0, 0, H_XmTabListInsertTabs);
  XM_DEFINE_PROCEDURE(XmRenderTableCvtFromProp, gxm_XmRenderTableCvtFromProp_w, 3, 0, 0, H_XmRenderTableCvtFromProp);
  XM_DEFINE_PROCEDURE(XmRenderTableCvtToProp, gxm_XmRenderTableCvtToProp_w, 2, 0, 0, H_XmRenderTableCvtToProp);
  XM_DEFINE_PROCEDURE(XmRenditionUpdate, gxm_XmRenditionUpdate_w, 2, 1, 0, H_XmRenditionUpdate);
  XM_DEFINE_PROCEDURE(XmRenditionRetrieve, gxm_XmRenditionRetrieve_w, 2, 1, 0, H_XmRenditionRetrieve);
  XM_DEFINE_PROCEDURE(XmRenditionFree, gxm_XmRenditionFree_w, 1, 0, 0, H_XmRenditionFree);
  XM_DEFINE_PROCEDURE(XmRenditionCreate, gxm_XmRenditionCreate_w, 3, 1, 0, H_XmRenditionCreate);
  XM_DEFINE_PROCEDURE(XmRenderTableGetRenditions, gxm_XmRenderTableGetRenditions_w, 0, 3, 0, H_XmRenderTableGetRenditions);
  XM_DEFINE_PROCEDURE(XmRenderTableGetRendition, gxm_XmRenderTableGetRendition_w, 2, 0, 0, H_XmRenderTableGetRendition);
  XM_DEFINE_PROCEDURE(XmRenderTableGetTags, gxm_XmRenderTableGetTags_w, 1, 0, 0, H_XmRenderTableGetTags);
  XM_DEFINE_PROCEDURE(XmRenderTableFree, gxm_XmRenderTableFree_w, 1, 0, 0, H_XmRenderTableFree);
  XM_DEFINE_PROCEDURE(XmRenderTableCopy, gxm_XmRenderTableCopy_w, 0, 3, 0, H_XmRenderTableCopy);
  XM_DEFINE_PROCEDURE(XmRenderTableRemoveRenditions, gxm_XmRenderTableRemoveRenditions_w, 0, 3, 0, H_XmRenderTableRemoveRenditions);
  XM_DEFINE_PROCEDURE(XmRenderTableAddRenditions, gxm_XmRenderTableAddRenditions_w, 4, 0, 0, H_XmRenderTableAddRenditions);
#endif
  XM_DEFINE_PROCEDURE(XmStringConcat, gxm_XmStringConcat_w, 2, 0, 0, H_XmStringConcat);
  XM_DEFINE_PROCEDURE(XmStringCopy, gxm_XmStringCopy_w, 1, 0, 0, H_XmStringCopy);
  XM_DEFINE_PROCEDURE(XmStringCompare, gxm_XmStringCompare_w, 2, 0, 0, H_XmStringCompare);
  XM_DEFINE_PROCEDURE(XmStringEmpty, gxm_XmStringEmpty_w, 1, 0, 0, H_XmStringEmpty);
  XM_DEFINE_PROCEDURE(XmStringHasSubstring, gxm_XmStringHasSubstring_w, 2, 0, 0, H_XmStringHasSubstring);
  XM_DEFINE_PROCEDURE(XmStringFree, gxm_XmStringFree_w, 1, 0, 0, H_XmStringFree);
  XM_DEFINE_PROCEDURE(XmStringBaseline, gxm_XmStringBaseline_w, 2, 0, 0, H_XmStringBaseline);
  XM_DEFINE_PROCEDURE(XmStringWidth, gxm_XmStringWidth_w, 2, 0, 0, H_XmStringWidth);
  XM_DEFINE_PROCEDURE(XmStringHeight, gxm_XmStringHeight_w, 2, 0, 0, H_XmStringHeight);
  XM_DEFINE_PROCEDURE(XmStringExtent, gxm_XmStringExtent_w, 2, 0, 0, H_XmStringExtent);
  XM_DEFINE_PROCEDURE(XmStringLineCount, gxm_XmStringLineCount_w, 1, 0, 0, H_XmStringLineCount);
  XM_DEFINE_PROCEDURE(XmStringDraw, gxm_XmStringDraw_w, 0, 0, 1, H_XmStringDraw);
  XM_DEFINE_PROCEDURE(XmStringDrawImage, gxm_XmStringDrawImage_w, 0, 0, 1, H_XmStringDrawImage);
  XM_DEFINE_PROCEDURE(XmStringDrawUnderline, gxm_XmStringDrawUnderline_w, 0, 0, 1, H_XmStringDrawUnderline);
  XM_DEFINE_PROCEDURE(XmGetDestination, gxm_XmGetDestination_w, 1, 0, 0, H_XmGetDestination);
  XM_DEFINE_PROCEDURE(XmIsTraversable, gxm_XmIsTraversable_w, 1, 0, 0, H_XmIsTraversable);
  XM_DEFINE_PROCEDURE(XmGetVisibility, gxm_XmGetVisibility_w, 1, 0, 0, H_XmGetVisibility);
  XM_DEFINE_PROCEDURE(XmGetTabGroup, gxm_XmGetTabGroup_w, 1, 0, 0, H_XmGetTabGroup);
  XM_DEFINE_PROCEDURE(XmGetFocusWidget, gxm_XmGetFocusWidget_w, 1, 0, 0, H_XmGetFocusWidget);
  XM_DEFINE_PROCEDURE(XmProcessTraversal, gxm_XmProcessTraversal_w, 2, 0, 0, H_XmProcessTraversal);
  XM_DEFINE_PROCEDURE(XmCreateMenuShell, gxm_XmCreateMenuShell_w, 3, 1, 0, H_XmCreateMenuShell);

  XM_DEFINE_PROCEDURE(XmIsMessageBox, gxm_XmIsMessageBox_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsArrowButtonGadget, gxm_XmIsArrowButtonGadget_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsArrowButton, gxm_XmIsArrowButton_w, 1, 0, 0, NULL);
#if MOTIF_2
  XM_DEFINE_PROCEDURE(XmIsNotebook, gxm_XmIsNotebook_w, 1, 0, 0, NULL);
#if HAVE_XM_XP
  XM_DEFINE_PROCEDURE(XmIsPrintShell, gxm_XmIsPrintShell_w, 1, 0, 0, NULL);
#endif
  XM_DEFINE_PROCEDURE(XmIsComboBox, gxm_XmIsComboBox_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsContainer, gxm_XmIsContainer_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsGrabShell, gxm_XmIsGrabShell_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsIconGadget, gxm_XmIsIconGadget_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsIconHeader, gxm_XmIsIconHeader_w, 1, 0, 0, NULL);
#endif
  XM_DEFINE_PROCEDURE(XmIsPanedWindow, gxm_XmIsPanedWindow_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsBulletinBoard, gxm_XmIsBulletinBoard_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsPrimitive, gxm_XmIsPrimitive_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsCascadeButtonGadget, gxm_XmIsCascadeButtonGadget_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsCascadeButton, gxm_XmIsCascadeButton_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsPushButtonGadget, gxm_XmIsPushButtonGadget_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsPushButton, gxm_XmIsPushButton_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsCommand, gxm_XmIsCommand_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsRowColumn, gxm_XmIsRowColumn_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsScale, gxm_XmIsScale_w, 1, 0, 0, NULL);
#if MOTIF_2
  XM_DEFINE_PROCEDURE(XmIsScreen, gxm_XmIsScreen_w, 1, 0, 0, NULL);
#endif
  XM_DEFINE_PROCEDURE(XmIsScrollBar, gxm_XmIsScrollBar_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsDialogShell, gxm_XmIsDialogShell_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsScrolledWindow, gxm_XmIsScrolledWindow_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsDisplay, gxm_XmIsDisplay_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsSelectionBox, gxm_XmIsSelectionBox_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsDragContext, gxm_XmIsDragContext_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsSeparatorGadget, gxm_XmIsSeparatorGadget_w, 1, 0, 0, NULL);
#if MOTIF_2
  XM_DEFINE_PROCEDURE(XmIsDragIconObjectClass, gxm_XmIsDragIconObjectClass_w, 1, 0, 0, NULL);
#endif
  XM_DEFINE_PROCEDURE(XmIsSeparator, gxm_XmIsSeparator_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsDrawingArea, gxm_XmIsDrawingArea_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsDrawnButton, gxm_XmIsDrawnButton_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsDropSiteManager, gxm_XmIsDropSiteManager_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsDropTransfer, gxm_XmIsDropTransfer_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsTextField, gxm_XmIsTextField_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsFileSelectionBox, gxm_XmIsFileSelectionBox_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsText, gxm_XmIsText_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsForm, gxm_XmIsForm_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsFrame, gxm_XmIsFrame_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsGadget, gxm_XmIsGadget_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsToggleButtonGadget, gxm_XmIsToggleButtonGadget_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsToggleButton, gxm_XmIsToggleButton_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsLabelGadget, gxm_XmIsLabelGadget_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsLabel, gxm_XmIsLabel_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsVendorShell, gxm_XmIsVendorShell_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsList, gxm_XmIsList_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsMainWindow, gxm_XmIsMainWindow_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsManager, gxm_XmIsManager_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIsMenuShell, gxm_XmIsMenuShell_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmListGetSelectedPos, gxm_XmListGetSelectedPos_w, 1, 0, 0, H_XmListGetSelectedPos);
#endif

#if HAVE_MOTIF
#if (!XM_DISABLE_DEPRECATED)
  XM_DEFINE_PROCEDURE(XtWarning, gxm_XtWarning_w, 1, 0, 0, H_XtWarning);
  XM_DEFINE_PROCEDURE(XtAppWarning, gxm_XtAppWarning_w, 2, 0, 0, H_XtAppWarning);
  XM_DEFINE_PROCEDURE(XtSetWarningMsgHandler, gxm_XtSetWarningMsgHandler_w, 1, 0, 0, H_XtSetWarningMsgHandler);
  XM_DEFINE_PROCEDURE(XtSetWarningHandler, gxm_XtSetWarningHandler_w, 1, 0, 0, H_XtSetWarningHandler);
  XM_DEFINE_PROCEDURE(XtWarningMsg, gxm_XtWarningMsg_w, 6, 0, 0, H_XtWarningMsg);
  XM_DEFINE_PROCEDURE(XtErrorMsg, gxm_XtErrorMsg_w, 6, 0, 0, H_XtErrorMsg);
  XM_DEFINE_PROCEDURE(XtError, gxm_XtError_w, 1, 0, 0, H_XtError);
  XM_DEFINE_PROCEDURE(XtSetErrorMsgHandler, gxm_XtSetErrorMsgHandler_w, 1, 0, 0, H_XtSetErrorMsgHandler);
  XM_DEFINE_PROCEDURE(XtSetErrorHandler, gxm_XtSetErrorHandler_w, 1, 0, 0, H_XtSetErrorHandler);
  XM_DEFINE_PROCEDURE(XtInitialize, gxm_XtInitialize_w, 4, 0, 0, H_XtInitialize);
  XM_DEFINE_PROCEDURE(XtCreateApplicationShell, gxm_XtCreateApplicationShell_w, 3, 1, 0, H_XtCreateApplicationShell);
  XM_DEFINE_PROCEDURE(XtSetSelectionTimeout, gxm_XtSetSelectionTimeout_w, 1, 0, 0, H_XtSetSelectionTimeout);
  XM_DEFINE_PROCEDURE(XtGetSelectionTimeout, gxm_XtGetSelectionTimeout_w, 0, 0, 0, H_XtGetSelectionTimeout);
  XM_DEFINE_PROCEDURE(XtAddActions, gxm_XtAddActions_w, 1, 0, 0, H_XtAddActions);
  XM_DEFINE_PROCEDURE(XtAddInput, gxm_XtAddInput_w, 3, 1, 0, H_XtAddInput);
  XM_DEFINE_PROCEDURE(XtAddWorkProc, gxm_XtAddWorkProc_w, 1, 1, 0, H_XtAddWorkProc);
  XM_DEFINE_PROCEDURE(XtAddTimeOut, gxm_XtAddTimeOut_w, 2, 1, 0, H_XtAddTimeOut);
  XM_DEFINE_PROCEDURE(XtMainLoop, gxm_XtMainLoop_w, 0, 0, 0, H_XtMainLoop);
  XM_DEFINE_PROCEDURE(XtProcessEvent, gxm_XtProcessEvent_w, 1, 0, 0, H_XtProcessEvent);
  XM_DEFINE_PROCEDURE(XtPending, gxm_XtPending_w, 0, 0, 0, H_XtPending);
  XM_DEFINE_PROCEDURE(XtNextEvent, gxm_XtNextEvent_w, 0, 0, 0, H_XtNextEvent);
  XM_DEFINE_PROCEDURE(XtPeekEvent, gxm_XtPeekEvent_w, 0, 0, 0, H_XtPeekEvent);

  XM_DEFINE_PROCEDURE(XmFontListEntryCreate, gxm_XmFontListEntryCreate_w, 3, 0, 0, H_XmFontListEntryCreate);
  XM_DEFINE_PROCEDURE(XmFontListEntryFree, gxm_XmFontListEntryFree_w, 1, 0, 0, H_XmFontListEntryFree);
  XM_DEFINE_PROCEDURE(XmFontListEntryGetFont, gxm_XmFontListEntryGetFont_w, 1, 0, 0, H_XmFontListEntryGetFont);
  XM_DEFINE_PROCEDURE(XmFontListEntryGetTag, gxm_XmFontListEntryGetTag_w, 1, 0, 0, H_XmFontListEntryGetTag);
  XM_DEFINE_PROCEDURE(XmFontListAppendEntry, gxm_XmFontListAppendEntry_w, 2, 0, 0, H_XmFontListAppendEntry);
  XM_DEFINE_PROCEDURE(XmFontListNextEntry, gxm_XmFontListNextEntry_w, 1, 0, 0, H_XmFontListNextEntry);
  XM_DEFINE_PROCEDURE(XmFontListRemoveEntry, gxm_XmFontListRemoveEntry_w, 2, 0, 0, H_XmFontListRemoveEntry);
  XM_DEFINE_PROCEDURE(XmFontListEntryLoad, gxm_XmFontListEntryLoad_w, 4, 0, 0, H_XmFontListEntryLoad);
  XM_DEFINE_PROCEDURE(XmFontListFree, gxm_XmFontListFree_w, 1, 0, 0, H_XmFontListFree);
  XM_DEFINE_PROCEDURE(XmFontListCopy, gxm_XmFontListCopy_w, 1, 0, 0, H_XmFontListCopy);
  XM_DEFINE_PROCEDURE(XmFontListInitFontContext, gxm_XmFontListInitFontContext_w, 1, 0, 0, H_XmFontListInitFontContext);
  XM_DEFINE_PROCEDURE(XmFontListFreeFontContext, gxm_XmFontListFreeFontContext_w, 1, 0, 0, H_XmFontListFreeFontContext);

  XM_DEFINE_PROCEDURE(XmFontContext?, XEN_XmFontContext_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmFontListEntry?, XEN_XmFontListEntry_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmFontList?, XEN_XmFontList_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmFontListCreate, gxm_XmFontListCreate_w, 2, 0, 0, H_XmFontListCreate);
  XM_DEFINE_PROCEDURE(XmTrackingLocate, gxm_XmTrackingLocate_w, 3, 0, 0, H_XmTrackingLocate);
  XM_DEFINE_PROCEDURE(XmFontListGetNextFont, gxm_XmFontListGetNextFont_w, 1, 0, 0, H_XmFontListGetNextFont);
  XM_DEFINE_PROCEDURE(XmStringByteCompare, gxm_XmStringByteCompare_w, 2, 0, 0, H_XmStringByteCompare);
  XM_DEFINE_PROCEDURE(XmStringCreateLtoR, gxm_XmStringCreateLtoR_w, 2, 0, 0, H_XmStringCreateLtoR);
  XM_DEFINE_PROCEDURE(XmStringCreateSimple, gxm_XmStringCreateSimple_w, 1, 0, 0, H_XmStringCreateSimple);
  XM_DEFINE_PROCEDURE(XmStringGetLtoR, gxm_XmStringGetLtoR_w, 2, 0, 0, H_XmStringGetLtoR);
  XM_DEFINE_PROCEDURE(XmStringGetNextSegment, gxm_XmStringGetNextSegment_w, 1, 0, 0, H_XmStringGetNextSegment);
  XM_DEFINE_PROCEDURE(XmStringSegmentCreate, gxm_XmStringSegmentCreate_w, 4, 0, 0, H_XmStringSegmentCreate);
  XM_DEFINE_PROCEDURE(XmStringPeekNextComponent, gxm_XmStringPeekNextComponent_w, 1, 0, 0, H_XmStringPeekNextComponent);
  XM_DEFINE_PROCEDURE(XmStringGetNextComponent, gxm_XmStringGetNextComponent_w, 1, 0, 0, H_XmStringGetNextComponent);
  XM_DEFINE_PROCEDURE(XmFontListAdd, gxm_XmFontListAdd_w, 3, 0, 0, H_XmFontListAdd);
  XM_DEFINE_PROCEDURE(XmStringLength, gxm_XmStringLength_w, 1, 0, 0, H_XmStringLength);
  XM_DEFINE_PROCEDURE(XmStringNConcat, gxm_XmStringNConcat_w, 3, 0, 0, H_XmStringNConcat);
  XM_DEFINE_PROCEDURE(XmStringNCopy, gxm_XmStringNCopy_w, 2, 0, 0, H_XmStringNCopy);
  XM_DEFINE_PROCEDURE(XmScrolledWindowSetAreas, gxm_XmScrolledWindowSetAreas_w, 4, 0, 0, H_XmScrolledWindowSetAreas);
  XM_DEFINE_PROCEDURE(XmSetFontUnits, gxm_XmSetFontUnits_w, 3, 0, 0, H_XmSetFontUnits);
  XM_DEFINE_PROCEDURE(XmSetFontUnit, gxm_XmSetFontUnit_w, 2, 0, 0, H_XmSetFontUnit);
  XM_DEFINE_PROCEDURE(XmGetMenuCursor, gxm_XmGetMenuCursor_w, 1, 0, 0, H_XmGetMenuCursor);
  XM_DEFINE_PROCEDURE(XmSetMenuCursor, gxm_XmSetMenuCursor_w, 2, 0, 0, H_XmSetMenuCursor);
  XM_DEFINE_PROCEDURE(XmMainWindowSetAreas, gxm_XmMainWindowSetAreas_w, 6, 0, 0, H_XmMainWindowSetAreas);
  XM_DEFINE_PROCEDURE(XmMainWindowSep1, gxm_XmMainWindowSep1_w, 1, 0, 0, H_XmMainWindowSep1);
  XM_DEFINE_PROCEDURE(XmMainWindowSep2, gxm_XmMainWindowSep2_w, 1, 0, 0, H_XmMainWindowSep2);
  XM_DEFINE_PROCEDURE(XmMainWindowSep3, gxm_XmMainWindowSep3_w, 1, 0, 0, H_XmMainWindowSep3);
#endif
#endif

#if HAVE_XPM
  XM_DEFINE_PROCEDURE(XpmCreatePixmapFromData, gxm_XpmCreatePixmapFromData_w, 4, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XpmCreateDataFromPixmap, gxm_XpmCreateDataFromPixmap_w, 4, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XpmReadFileToPixmap, gxm_XpmReadFileToPixmap_w, 4, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XpmReadFileToXpmImage, gxm_XpmReadFileToXpmImage_w, 1, 0, 0, NULL);
#if HAVE_XPM_GET_ERROR_STRING
  XM_DEFINE_PROCEDURE(XpmGetErrorString, gxm_XpmGetErrorString_w, 1, 0, 0, NULL);
#endif
  XM_DEFINE_PROCEDURE(XpmReadPixmapFile, gxm_XpmReadFileToPixmap_w, 4, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XpmWriteFileFromPixmap, gxm_XpmWriteFileFromPixmap_w, 5, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XpmWritePixmapFile, gxm_XpmWriteFileFromPixmap_w, 5, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XpmCreatePixmapFromXpmImage, gxm_XpmCreatePixmapFromXpmImage_w, 4, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XpmCreateXpmImageFromPixmap, gxm_XpmCreateXpmImageFromPixmap_w, 4, 0, 0, NULL);
#endif
  XM_DEFINE_PROCEDURE(XGetPixel, gxm_XGetPixel_w, 3, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XDestroyImage, gxm_XDestroyImage_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XPutPixel, gxm_XPutPixel_w, 4, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XSubImage, gxm_XSubImage_w, 5, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XAddPixel, gxm_XAddPixel_w, 2, 0, 0, NULL);

#if HAVE_MOTIF
  XM_DEFINE_PROCEDURE(XtAppContext?, XEN_XtAppContext_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XtRequestId?, XEN_XtRequestId_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XtWorkProcId?, XEN_XtWorkProcId_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XtInputId?, XEN_XtInputId_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XtIntervalId?, XEN_XtIntervalId_p_w, 1, 0, 0, NULL);
#endif
  XM_DEFINE_PROCEDURE(Screen?, XEN_Screen_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XEvent?, XEN_XEvent_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XRectangle?, XEN_XRectangle_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XArc?, XEN_XArc_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XPoint?, XEN_XPoint_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XSegment?, XEN_XSegment_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XColor?, XEN_XColor_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(Atom?, XEN_Atom_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(Colormap?, XEN_Colormap_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XModifierKeymap?, XEN_XModifierKeymap_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(Depth?, XEN_Depth_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(Display?, XEN_Display_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(Drawable?, XEN_Window_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(Font?, XEN_Font_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(GC?, XEN_GC_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(KeySym?, XEN_KeySym_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(Pixel?, XEN_Pixel_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(Pixmap?, XEN_Pixmap_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(Region?, XEN_Region_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(Time?, XEN_Time_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(Visual?, XEN_Visual_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(Window?, XEN_Window_p_w, 1, 0, 0, NULL);
#if HAVE_MOTIF
  XM_DEFINE_PROCEDURE(Widget?, XEN_Widget_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmStringContext?, XEN_XmStringContext_p_w, 1, 0, 0, NULL);
#endif
  XM_DEFINE_PROCEDURE(XFontProp?, XEN_XFontProp_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XFontSet?, XEN_XFontSet_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XFontStruct?, XEN_XFontStruct_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XGCValues?, XEN_XGCValues_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XImage?, XEN_XImage_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XVisualInfo?, XEN_XVisualInfo_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XWMHints?, XEN_XWMHints_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XWindowAttributes?, XEN_XWindowAttributes_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XWindowChanges?, XEN_XWindowChanges_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(KeyCode?, XEN_KeyCode_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XContext?, XEN_XContext_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XCharStruct?, XEN_XCharStruct_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XTextItem?, XEN_XTextItem_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XStandardColormap?, XEN_XStandardColormap_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(Cursor?, XEN_Cursor_p_w, 1, 0, 0, NULL);
#if HAVE_XM_XP
  XM_DEFINE_PROCEDURE(XPContext?, XEN_XPContext_p_w, 1, 0, 0, NULL);
#endif
#if HAVE_MOTIF
  XM_DEFINE_PROCEDURE(WidgetClass?, XEN_WidgetClass_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmString?, XEN_XmString_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmToggleButton?, gxm_XmIsToggleButton_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmDrawingArea?, gxm_XmIsDrawingArea_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmPushButton?, gxm_XmIsPushButton_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmTextField?, gxm_XmIsTextField_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmFileSelectionBox?, gxm_XmIsFileSelectionBox_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmText?, gxm_XmIsText_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmFrame?, gxm_XmIsFrame_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmLabel?, gxm_XmIsLabel_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmList?, gxm_XmIsList_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmArrowButton?, gxm_XmIsArrowButton_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmScrollBar?, gxm_XmIsScrollBar_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmCommand?, gxm_XmIsCommand_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmScale?, gxm_XmIsScale_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmRowColumn?, gxm_XmIsRowColumn_w, 1, 0, 0, NULL);
#if MOTIF_2
  XM_DEFINE_PROCEDURE(XmTab?, XEN_XmTab_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmNotebook?, gxm_XmIsNotebook_w, 1, 0, 0, NULL);
#if HAVE_XM_XP
  XM_DEFINE_PROCEDURE(XmPrintShell?, gxm_XmIsPrintShell_w, 1, 0, 0, NULL);
#endif
  XM_DEFINE_PROCEDURE(XmComboBox?, gxm_XmIsComboBox_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmContainer?, gxm_XmIsContainer_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIconHeader?, gxm_XmIsIconHeader_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmGrabShell?, gxm_XmIsGrabShell_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmRendition?, XEN_XmRendition_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmRenderTable?, XEN_XmRenderTable_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmIconGadget?, gxm_XmIsIconGadget_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmTabList?, XEN_XmTabList_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmParseMapping?, XEN_XmParseMapping_p_w, 1, 0, 0, NULL);
#endif
  XM_DEFINE_PROCEDURE(XmPanedWindow?, gxm_XmIsPanedWindow_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmScrolledWindow?, gxm_XmIsScrolledWindow_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmCascadeButton?, gxm_XmIsCascadeButton_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmForm?, gxm_XmIsForm_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmBulletinBoard?, gxm_XmIsBulletinBoard_w, 1, 0, 0, NULL);
#if MOTIF_2
  XM_DEFINE_PROCEDURE(XmScreen?, gxm_XmIsScreen_w, 1, 0, 0, NULL);
#endif
  XM_DEFINE_PROCEDURE(XmDialogShell?, gxm_XmIsDialogShell_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmDisplay?, gxm_XmIsDisplay_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmSelectionBox?, gxm_XmIsSelectionBox_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmDragContext?, gxm_XmIsDragContext_w, 1, 0, 0, NULL);
#if MOTIF_2
  XM_DEFINE_PROCEDURE(XmDragIconObjectClass?, gxm_XmIsDragIconObjectClass_w, 1, 0, 0, NULL);
#endif
  XM_DEFINE_PROCEDURE(XmSeparator?, gxm_XmIsSeparator_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmDropSiteManager?, gxm_XmIsDropSiteManager_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmDropTransfer?, gxm_XmIsDropTransfer_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmVendorShell?, gxm_XmIsVendorShell_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmMainWindow?, gxm_XmIsMainWindow_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmMessageBox?, gxm_XmIsMessageBox_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmManager?, gxm_XmIsManager_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmMenuShell?, gxm_XmIsMenuShell_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmLabelGadget?, gxm_XmIsLabelGadget_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmPushButtonGadget?, gxm_XmIsPushButtonGadget_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmSeparatorGadget?, gxm_XmIsSeparatorGadget_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmArrowButtonGadget?, gxm_XmIsArrowButtonGadget_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmCascadeButtonGadget?, gxm_XmIsCascadeButtonGadget_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmToggleButtonGadget?, gxm_XmIsToggleButtonGadget_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmDrawnButton?, gxm_XmIsDrawnButton_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmPrimitive?, gxm_XmIsPrimitive_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XmTextSource?, XEN_XmTextSource_p_w, 1, 0, 0, NULL);
#endif

#if HAVE_XPM
  XM_DEFINE_PROCEDURE(XpmAttributes?, XEN_XpmAttributes_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XpmImage?, XEN_XpmImage_p_w, 1, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XpmColorSymbol?, XEN_XpmColorSymbol_p_w, 1, 0, 0, NULL);
#endif

#if WITH_EDITRES
  XM_DEFINE_PROCEDURE(_XEditResCheckMessages, gxm_XEditResCheckMessages_w, 4, 0, 0, NULL);
#endif

#if HAVE_XSHAPEQUERYEXTENSION
  XM_DEFINE_PROCEDURE(XShapeQueryExtension, gxm_XShapeQueryExtension_w, 1, 0, 0, H_XShapeQueryExtension);
  XM_DEFINE_PROCEDURE(XShapeQueryVersion, gxm_XShapeQueryVersion_w, 1, 0, 0, H_XShapeQueryVersion);
  XM_DEFINE_PROCEDURE(XShapeQueryExtents, gxm_XShapeQueryExtents_w, 2, 0, 0, H_XShapeQueryExtents);
  XM_DEFINE_PROCEDURE(XShapeGetRectangles, gxm_XShapeGetRectangles_w, 3, 0, 0, H_XShapeGetRectangles);
  XM_DEFINE_PROCEDURE(XShapeOffsetShape, gxm_XShapeOffsetShape_w, 5, 0, 0, H_XShapeOffsetShape);
  XM_DEFINE_PROCEDURE(XShapeCombineRegion, gxm_XShapeCombineRegion_w, 7, 0, 0, H_XShapeCombineRegion);
  XM_DEFINE_PROCEDURE(XShapeCombineMask, gxm_XShapeCombineMask_w, 7, 0, 0, H_XShapeCombineMask);
  XM_DEFINE_PROCEDURE(XShapeCombineShape, gxm_XShapeCombineShape_w, 8, 0, 0, H_XShapeCombineShape);
  XM_DEFINE_PROCEDURE(XShapeCombineRectangles, gxm_XShapeCombineRectangles_w, 9, 0, 0, H_XShapeCombineRectangles);
#endif

}


  XEN_NARGIFY_4(gxm_XSegment_w, gxm_XSegment)
  XEN_NARGIFY_4(gxm_XRectangle_w, gxm_XRectangle)
  XEN_ARGIFY_6(gxm_XColor_w, gxm_XColor)
  XEN_NARGIFY_6(gxm_XArc_w, gxm_XArc)
  XEN_NARGIFY_7(gxm_XWindowChanges_w, gxm_XWindowChanges)
  XEN_VARGIFY(gxm_XSetWindowAttributes_w, gxm_XSetWindowAttributes)
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
  XEN_NARGIFY_1(gxm_colormap_w, gxm_colormap)
  XEN_NARGIFY_2(gxm_set_colormap_w, gxm_set_colormap)

  XEN_NARGIFY_2(gxm_set_input_w, gxm_set_input)
  XEN_NARGIFY_2(gxm_set_initial_state_w, gxm_set_initial_state)

  XEN_NARGIFY_1(gxm_min_height_w, gxm_min_height)
  XEN_NARGIFY_1(gxm_max_height_w, gxm_max_height)
  XEN_NARGIFY_1(gxm_min_width_w, gxm_min_width)
  XEN_NARGIFY_1(gxm_max_width_w, gxm_max_width)
  XEN_NARGIFY_1(gxm_height_inc_w, gxm_height_inc)
  XEN_NARGIFY_1(gxm_width_inc_w, gxm_width_inc)

  XEN_NARGIFY_2(gxm_set_data_w, gxm_set_data)
  XEN_NARGIFY_2(gxm_set_backing_store_w, gxm_set_backing_store)
  XEN_NARGIFY_2(gxm_set_background_pixel_w, gxm_set_background_pixel)
  XEN_NARGIFY_2(gxm_set_border_pixel_w, gxm_set_border_pixel)
  XEN_NARGIFY_2(gxm_set_bit_gravity_w, gxm_set_bit_gravity)
  XEN_NARGIFY_2(gxm_set_save_under_w, gxm_set_save_under)
  XEN_NARGIFY_2(gxm_set_event_mask_w, gxm_set_event_mask)
  XEN_NARGIFY_2(gxm_set_cursor_w, gxm_set_cursor)

#if HAVE_MOTIF
  XEN_NARGIFY_2(gxm_set_set_w, gxm_set_set)
  XEN_NARGIFY_2(gxm_set_click_count_w, gxm_set_click_count)
  XEN_NARGIFY_2(gxm_set_length_w, gxm_set_length)
  XEN_NARGIFY_1(gxm_ptr_w, gxm_ptr)
  XEN_NARGIFY_2(gxm_set_ptr_w, gxm_set_ptr)
  XEN_NARGIFY_2(gxm_set_reason_w, gxm_set_reason)
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
#if HAVE_XM_XP
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
  #define XM_DEFINE_ACCESSOR(Name, Value, SetName, SetValue, A1, A2, A3, A4) \
     XEN_DEFINE_PROCEDURE_WITH_SETTER(XM_FIELD_PREFIX #Name XM_POSTFIX, Value, NULL, XM_FIELD_PREFIX #SetName XM_POSTFIX, SetValue, A1, A2, A3, A4)
  #define XM_DEFINE_READER(Name, Value, A1, A2, A3) XEN_DEFINE_PROCEDURE(XM_FIELD_PREFIX #Name XM_POSTFIX, Value, A1, A2, A3, NULL)

  XM_DEFINE_ACCESSOR(pixel, gxm_pixel_w, set_pixel, gxm_set_pixel_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(red, gxm_red_w, set_red, gxm_set_red_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(green, gxm_green_w, set_green, gxm_set_green_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(blue, gxm_blue_w, set_blue, gxm_set_blue_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(flags, gxm_flags_w, set_flags, gxm_set_flags_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(pad, gxm_pad_w, set_pad, gxm_set_pad_w, 1, 0, 2, 0);
  XM_DEFINE_PROCEDURE(XColor, gxm_XColor_w, 0, 6, 0, NULL);
  XM_DEFINE_ACCESSOR(x, gxm_x_w, set_x, gxm_set_x_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(y, gxm_y_w, set_y, gxm_set_y_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(width, gxm_width_w, set_width, gxm_set_width_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(height, gxm_height_w, set_height, gxm_set_height_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(angle1, gxm_angle1_w, set_angle1, gxm_set_angle1_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(angle2, gxm_angle2_w, set_angle2, gxm_set_angle2_w, 1, 0, 2, 0);
  XM_DEFINE_PROCEDURE(XArc, gxm_XArc_w, 6, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XWindowChanges, gxm_XWindowChanges_w, 7, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XSetWindowAttributes, gxm_XSetWindowAttributes_w, 0, 0, 1, NULL);
  XM_DEFINE_PROCEDURE(XPoint, gxm_XPoint_w, 2, 0, 0, NULL);
  XM_DEFINE_ACCESSOR(x1, gxm_x1_w, set_x1, gxm_set_x1_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(y1, gxm_y1_w, set_y1, gxm_set_y1_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(x2, gxm_x2_w, set_x2, gxm_set_x2_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(y2, gxm_y2_w, set_y2, gxm_set_y2_w, 1, 0, 2, 0);
  XM_DEFINE_PROCEDURE(XSegment, gxm_XSegment_w, 4, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XRectangle, gxm_XRectangle_w, 4, 0, 0, NULL);
  XM_DEFINE_ACCESSOR(dashes, gxm_dashes_w, set_dashes, gxm_set_dashes_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(dash_offset, gxm_dash_offset_w, set_dash_offset, gxm_set_dash_offset_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(clip_mask, gxm_clip_mask_w, set_clip_mask, gxm_set_clip_mask_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(clip_y_origin, gxm_clip_y_origin_w, set_clip_y_origin, gxm_set_clip_y_origin_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(clip_x_origin, gxm_clip_x_origin_w, set_clip_x_origin, gxm_set_clip_x_origin_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(graphics_exposures, gxm_graphics_exposures_w, set_graphics_exposures, gxm_set_graphics_exposures_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(subwindow_mode, gxm_subwindow_mode_w, set_subwindow_mode, gxm_set_subwindow_mode_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(font, gxm_font_w, set_font, gxm_set_font_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(ts_y_origin, gxm_ts_y_origin_w, set_ts_y_origin, gxm_set_ts_y_origin_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(ts_x_origin, gxm_ts_x_origin_w, set_ts_x_origin, gxm_set_ts_x_origin_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(stipple, gxm_stipple_w, set_stipple, gxm_set_stipple_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(tile, gxm_tile_w, set_tile, gxm_set_tile_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(arc_mode, gxm_arc_mode_w, set_arc_mode, gxm_set_arc_mode_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(fill_rule, gxm_fill_rule_w, set_fill_rule, gxm_set_fill_rule_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(fill_style, gxm_fill_style_w, set_fill_style, gxm_set_fill_style_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(join_style, gxm_join_style_w, set_join_style, gxm_set_join_style_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(cap_style, gxm_cap_style_w, set_cap_style, gxm_set_cap_style_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(line_style, gxm_line_style_w, set_line_style, gxm_set_line_style_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(line_width, gxm_line_width_w, set_line_width, gxm_set_line_width_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(background, gxm_background_w, set_background, gxm_set_background_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(foreground, gxm_foreground_w, set_foreground, gxm_set_foreground_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(plane_mask, gxm_plane_mask_w, set_plane_mask, gxm_set_plane_mask_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(function, gxm_function_w, set_function, gxm_set_function_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(delta, gxm_delta_w, set_delta, gxm_set_delta_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(nchars, gxm_nchars_w, set_nchars, gxm_set_nchars_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(chars, gxm_chars_w, set_chars, gxm_set_chars_w, 1, 0, 2, 0);
  XM_DEFINE_PROCEDURE(XTextItem, gxm_XTextItem_w, 4, 0, 0, NULL);
  XM_DEFINE_ACCESSOR(name, gxm_name_w, set_name, gxm_set_name_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(depth, gxm_depth_w, set_depth, gxm_set_depth_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(visual, gxm_visual_w, set_visual, gxm_set_visual_w, 1, 0, 2, 0);

  XM_DEFINE_READER(mwidth, gxm_mwidth_w, 1, 0, 0);
  XM_DEFINE_READER(mheight, gxm_mheight_w, 1, 0, 0);
  XM_DEFINE_READER(ndepths, gxm_ndepths_w, 1, 0, 0);
  XM_DEFINE_READER(depths, gxm_depths_w, 1, 0, 0);
  XM_DEFINE_READER(root_depth, gxm_root_depth_w, 1, 0, 0);
  XM_DEFINE_READER(root_visual, gxm_root_visual_w, 1, 0, 0);
  XM_DEFINE_READER(default_gc, gxm_default_gc_w, 1, 0, 0);
  XM_DEFINE_READER(cmap, gxm_cmap_w, 1, 0, 0);
  XM_DEFINE_READER(white_pixel, gxm_white_pixel_w, 1, 0, 0);
  XM_DEFINE_READER(black_pixel, gxm_black_pixel_w, 1, 0, 0);
  XM_DEFINE_READER(max_maps, gxm_max_maps_w, 1, 0, 0);
  XM_DEFINE_READER(min_maps, gxm_min_maps_w, 1, 0, 0);
  XM_DEFINE_ACCESSOR(backing_store, gxm_backing_store_w, set_backing_store, gxm_set_backing_store_w, 1, 0, 2, 0);
  XM_DEFINE_READER(save_unders, gxm_save_unders_w, 1, 0, 0);
  XM_DEFINE_READER(root_input_mask, gxm_root_input_mask_w, 1, 0, 0);
  XM_DEFINE_READER(lbearing, gxm_lbearing_w, 1, 0, 0);
  XM_DEFINE_READER(rbearing, gxm_rbearing_w, 1, 0, 0);
  XM_DEFINE_READER(ascent, gxm_ascent_w, 1, 0, 0);
  XM_DEFINE_READER(descent, gxm_descent_w, 1, 0, 0);
  XM_DEFINE_READER(attributes, gxm_attributes_w, 1, 0, 0);
  XM_DEFINE_READER(card32, gxm_card32_w, 1, 0, 0);
  XM_DEFINE_READER(fid, gxm_fid_w, 1, 0, 0);
  XM_DEFINE_READER(properties, gxm_properties_w, 1, 0, 0);
  XM_DEFINE_READER(min_bounds, gxm_min_bounds_w, 1, 0, 0);
  XM_DEFINE_READER(max_bounds, gxm_max_bounds_w, 1, 0, 0);
  XM_DEFINE_READER(per_char, gxm_per_char_w, 1, 0, 0);

  XM_DEFINE_ACCESSOR(input, gxm_input_w, set_input, gxm_set_input_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(initial_state, gxm_initial_state_w, set_initial_state, gxm_set_initial_state_w, 1, 0, 2, 0);

  XM_DEFINE_READER(icon_pixmap, gxm_icon_pixmap_w, 1, 0, 0);
  XM_DEFINE_READER(icon_window, gxm_icon_window_w, 1, 0, 0);
  XM_DEFINE_READER(icon_x, gxm_icon_x_w, 1, 0, 0);
  XM_DEFINE_READER(icon_y, gxm_icon_y_w, 1, 0, 0);
  XM_DEFINE_READER(icon_mask, gxm_icon_mask_w, 1, 0, 0);
  XM_DEFINE_READER(window_group, gxm_window_group_w, 1, 0, 0);
  XM_DEFINE_READER(visualid, gxm_visualid_w, 1, 0, 0);
  XM_DEFINE_READER(class, gxm_class_w, 1, 0, 0);
  XM_DEFINE_READER(red_mask, gxm_red_mask_w, 1, 0, 0);
  XM_DEFINE_READER(green_mask, gxm_green_mask_w, 1, 0, 0);
  XM_DEFINE_READER(blue_mask, gxm_blue_mask_w, 1, 0, 0);
  XM_DEFINE_READER(bits_per_rgb, gxm_bits_per_rgb_w, 1, 0, 0);
  XM_DEFINE_READER(colormap_size, gxm_colormap_size_w, 1, 0, 0);
  XM_DEFINE_READER(map_entries, gxm_map_entries_w, 1, 0, 0);
  XM_DEFINE_READER(nvisuals, gxm_nvisuals_w, 1, 0, 0);
  XM_DEFINE_READER(visuals, gxm_visuals_w, 1, 0, 0);
  XM_DEFINE_READER(bits_per_pixel, gxm_bits_per_pixel_w, 1, 0, 0);
  XM_DEFINE_READER(background_pixmap, gxm_background_pixmap_w, 1, 0, 0);
  XM_DEFINE_ACCESSOR(background_pixel, gxm_background_pixel_w, set_background_pixel, gxm_set_background_pixel_w, 1, 0, 2, 0);
  XM_DEFINE_READER(border_pixmap, gxm_border_pixmap_w, 1, 0, 0);
  XM_DEFINE_ACCESSOR(border_pixel, gxm_border_pixel_w, set_border_pixel, gxm_set_border_pixel_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(bit_gravity, gxm_bit_gravity_w, set_bit_gravity, gxm_set_bit_gravity_w, 1, 0, 2, 0);
  XM_DEFINE_READER(win_gravity, gxm_win_gravity_w, 1, 0, 0);
  XM_DEFINE_READER(backing_planes, gxm_backing_planes_w, 1, 0, 0);
  XM_DEFINE_READER(backing_pixel, gxm_backing_pixel_w, 1, 0, 0);
  XM_DEFINE_ACCESSOR(save_under, gxm_save_under_w, set_save_under, gxm_set_save_under_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(event_mask, gxm_event_mask_w, set_event_mask, gxm_set_event_mask_w, 1, 0, 2, 0);
  XM_DEFINE_READER(do_not_propagate_mask, gxm_do_not_propagate_mask_w, 1, 0, 0);
  XM_DEFINE_ACCESSOR(cursor, gxm_cursor_w, set_cursor, gxm_set_cursor_w, 1, 0, 2, 0);
  XM_DEFINE_READER(map_installed, gxm_map_installed_w, 1, 0, 0);
  XM_DEFINE_READER(map_state, gxm_map_state_w, 1, 0, 0);
  XM_DEFINE_READER(all_event_masks, gxm_all_event_masks_w, 1, 0, 0);
  XM_DEFINE_READER(your_event_mask, gxm_your_event_mask_w, 1, 0, 0);
  XM_DEFINE_READER(screen, gxm_screen_w, 1, 0, 0);
  XM_DEFINE_READER(xoffset, gxm_xoffset_w, 1, 0, 0);
  XM_DEFINE_READER(byte_order, gxm_byte_order_w, 1, 0, 0);
  XM_DEFINE_READER(bitmap_unit, gxm_bitmap_unit_w, 1, 0, 0);
  XM_DEFINE_READER(bitmap_bit_order, gxm_bitmap_bit_order_w, 1, 0, 0);
  XM_DEFINE_READER(bitmap_pad, gxm_bitmap_pad_w, 1, 0, 0);
  XM_DEFINE_READER(bytes_per_line, gxm_bytes_per_line_w, 1, 0, 0);
  XM_DEFINE_READER(obdata, gxm_obdata_w, 1, 0, 0);
  XM_DEFINE_READER(sibling, gxm_sibling_w, 1, 0, 0);
  XM_DEFINE_READER(stack_mode, gxm_stack_mode_w, 1, 0, 0);
 
  XM_DEFINE_READER(red_max, gxm_red_max_w, 1, 0, 0);
  XM_DEFINE_READER(red_mult, gxm_red_mult_w, 1, 0, 0);
  XM_DEFINE_READER(green_max, gxm_green_max_w, 1, 0, 0);
  XM_DEFINE_READER(green_mult, gxm_green_mult_w, 1, 0, 0);
  XM_DEFINE_READER(blue_max, gxm_blue_max_w, 1, 0, 0);
  XM_DEFINE_READER(blue_mult, gxm_blue_mult_w, 1, 0, 0);
  XM_DEFINE_READER(base_pixel, gxm_base_pixel_w, 1, 0, 0);
  XM_DEFINE_READER(killid, gxm_killid_w, 1, 0, 0);

  XM_DEFINE_READER(min_height, gxm_min_height_w, 1, 0, 0);
  XM_DEFINE_READER(max_height, gxm_max_height_w, 1, 0, 0);
  XM_DEFINE_READER(min_width, gxm_min_width_w, 1, 0, 0);
  XM_DEFINE_READER(max_width, gxm_max_width_w, 1, 0, 0);
  XM_DEFINE_READER(height_inc, gxm_height_inc_w, 1, 0, 0);
  XM_DEFINE_READER(width_inc, gxm_width_inc_w, 1, 0, 0);

#if HAVE_MOTIF
#if MOTIF_2
  XM_DEFINE_READER(page_number, gxm_page_number_w, 1, 0, 0);
  XM_DEFINE_READER(page_widget, gxm_page_widget_w, 1, 0, 0);
  XM_DEFINE_READER(status_area_widget, gxm_status_area_widget_w, 1, 0, 0);
  XM_DEFINE_READER(major_tab_widget, gxm_major_tab_widget_w, 1, 0, 0);
  XM_DEFINE_READER(minor_tab_widget, gxm_minor_tab_widget_w, 1, 0, 0);
  XM_DEFINE_READER(source_data, gxm_source_data_w, 1, 0, 0);
  XM_DEFINE_READER(location_data, gxm_location_data_w, 1, 0, 0);
  XM_DEFINE_READER(parm, gxm_parm_w, 1, 0, 0);
  XM_DEFINE_READER(parm_format, gxm_parm_format_w, 1, 0, 0);
  XM_DEFINE_READER(parm_length, gxm_parm_length_w, 1, 0, 0);
  XM_DEFINE_READER(parm_type, gxm_parm_type_w, 1, 0, 0);
  XM_DEFINE_READER(transfer_id, gxm_transfer_id_w, 1, 0, 0);
  XM_DEFINE_READER(destination_data, gxm_destination_data_w, 1, 0, 0);
  XM_DEFINE_READER(remaining, gxm_remaining_w, 1, 0, 0);
  XM_DEFINE_READER(item_or_text, gxm_item_or_text_w, 1, 0, 0);
  XM_DEFINE_READER(auto_selection_type, gxm_auto_selection_type_w, 1, 0, 0);
  XM_DEFINE_READER(new_outline_state, gxm_new_outline_state_w, 1, 0, 0);
  XM_DEFINE_READER(prev_page_number, gxm_prev_page_number_w, 1, 0, 0);
  XM_DEFINE_READER(prev_page_widget, gxm_prev_page_widget_w, 1, 0, 0);
  XM_DEFINE_READER(rendition, gxm_rendition_w, 1, 0, 0);
  XM_DEFINE_READER(render_table, gxm_render_table_w, 1, 0, 0);
#if HAVE_XM_XP
  XM_DEFINE_READER(last_page, gxm_last_page_w, 1, 0, 0);
#endif
  XM_DEFINE_READER(crossed_boundary, gxm_crossed_boundary_w, 1, 0, 0);
  XM_DEFINE_READER(client_data, gxm_client_data_w, 1, 0, 0);
  XM_DEFINE_READER(status, gxm_status_w, 1, 0, 0);
  XM_DEFINE_READER(font_name, gxm_font_name_w, 1, 0, 0);
  XM_DEFINE_READER(tag, gxm_tag_w, 1, 0, 0);
  XM_DEFINE_READER(traversal_destination, gxm_traversal_destination_w, 1, 0, 0);
  XM_DEFINE_READER(dragProtocolStyle, gxm_dragProtocolStyle_w, 1, 0, 0);
  XM_DEFINE_READER(direction, gxm_direction_w, 1, 0, 0);
  XM_DEFINE_READER(position, gxm_position_w, 1, 0, 0);
  XM_DEFINE_ACCESSOR(menuToPost, gxm_menuToPost_w, set_menuToPost, gxm_set_menuToPost_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(postIt, gxm_postIt_w, set_postIt, gxm_set_postIt_w, 1, 0, 2, 0);
#endif
  XM_DEFINE_READER(timeStamp, gxm_timeStamp_w, 1, 0, 0);
  XM_DEFINE_ACCESSOR(operation, gxm_operation_w, set_operation, gxm_set_operation_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(reason, gxm_reason_w, set_reason, gxm_set_reason_w, 1, 0, 2, 0);
  XM_DEFINE_READER(operations, gxm_operations_w, 1, 0, 0);
  XM_DEFINE_ACCESSOR(dropSiteStatus, gxm_dropSiteStatus_w, set_dropSiteStatus, gxm_set_dropSiteStatus_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(set, gxm_set_w, set_set, gxm_set_set_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(click_count, gxm_click_count_w, set_click_count, gxm_set_click_count_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(length, gxm_length_w, set_length, gxm_set_length_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(ptr, gxm_ptr_w, set_ptr, gxm_set_ptr_w, 1, 0, 2, 0);
  XM_DEFINE_READER(dropAction, gxm_dropAction_w, 1, 0, 0);
  XM_DEFINE_READER(iccHandle, gxm_iccHandle_w, 1, 0, 0);
  XM_DEFINE_READER(completionStatus, gxm_completionStatus_w, 1, 0, 0);
  XM_DEFINE_READER(dragContext, gxm_dragContext_w, 1, 0, 0);
  XM_DEFINE_READER(animate, gxm_animate_w, 1, 0, 0);
  XM_DEFINE_READER(widget, gxm_widget_w, 1, 0, 0);
  XM_DEFINE_READER(item_position, gxm_item_position_w, 1, 0, 0);
  XM_DEFINE_READER(callbackstruct, gxm_callbackstruct_w, 1, 0, 0);
  XM_DEFINE_READER(item, gxm_item_w, 1, 0, 0);
  XM_DEFINE_READER(item_length, gxm_item_length_w, 1, 0, 0);
  XM_DEFINE_READER(selected_items, gxm_selected_items_w, 1, 0, 0);
  XM_DEFINE_READER(selected_item_count, gxm_selected_item_count_w, 1, 0, 0);
  XM_DEFINE_READER(selected_item_positions, gxm_selected_item_positions_w, 1, 0, 0);
  XM_DEFINE_READER(selection_type, gxm_selection_type_w, 1, 0, 0);
  XM_DEFINE_READER(mask, gxm_mask_w, 1, 0, 0);
  XM_DEFINE_READER(mask_length, gxm_mask_length_w, 1, 0, 0);
  XM_DEFINE_READER(dir, gxm_dir_w, 1, 0, 0);
  XM_DEFINE_READER(dir_length, gxm_dir_length_w, 1, 0, 0);
  XM_DEFINE_READER(pattern, gxm_pattern_w, 1, 0, 0);
  XM_DEFINE_READER(pattern_length, gxm_pattern_length_w, 1, 0, 0);
  XM_DEFINE_READER(currInsert, gxm_currInsert_w, 1, 0, 0);
  XM_DEFINE_READER(newInsert, gxm_newInsert_w, 1, 0, 0);
  XM_DEFINE_READER(startPos, gxm_startPos_w, 1, 0, 0);
  XM_DEFINE_READER(endPos, gxm_endPos_w, 1, 0, 0);
  XM_DEFINE_READER(text, gxm_text_w, 1, 0, 0);
  XM_DEFINE_ACCESSOR(value, gxm_value_w, set_value, gxm_set_value_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(doit, gxm_doit_w, set_doit, gxm_set_doit_w, 1, 0, 2, 0); 
#if HAVE_XPM
  XM_DEFINE_ACCESSOR(valuemask, gxm_valuemask_w, set_valuemask, gxm_set_valuemask_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(ncolors, gxm_ncolors_w, set_ncolors, gxm_set_ncolors_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(cpp, gxm_cpp_w, set_cpp, gxm_set_cpp_w, 1, 0, 2, 0);
  XM_DEFINE_PROCEDURE(XpmImage, gxm_XpmImage_w, 5, 0, 0, NULL);
  XM_DEFINE_ACCESSOR(numsymbols, gxm_numsymbols_w, set_numsymbols, gxm_set_numsymbols_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(colorsymbols, gxm_colorsymbols_w, set_colorsymbols, gxm_set_colorsymbols_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(npixels, gxm_npixels_w, set_npixels, gxm_set_npixels_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(y_hotspot, gxm_y_hotspot_w, set_y_hotspot, gxm_set_y_hotspot_w, 1, 0, 2, 0);
  XM_DEFINE_ACCESSOR(x_hotspot, gxm_x_hotspot_w, set_x_hotspot, gxm_set_x_hotspot_w, 1, 0, 2, 0);
  XM_DEFINE_PROCEDURE(XpmColorSymbol, gxm_XpmColorSymbol_w, 3, 0, 0, NULL);
  XM_DEFINE_PROCEDURE(XpmAttributes, gxm_XpmAttributes_w, 0, 0, 0, NULL);
#endif
#endif

  XM_DEFINE_ACCESSOR(request_code, gxm_request_code_w, set_request_code, gxm_set_request_code_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(error_code, gxm_error_code_w, set_error_code, gxm_set_error_code_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(first_keycode, gxm_first_keycode_w, set_first_keycode, gxm_set_first_keycode_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(request, gxm_request_w, set_request, gxm_set_request_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(resourceid, gxm_resourceid_w, set_resourceid, gxm_set_resourceid_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(format, gxm_format_w, set_format, gxm_set_format_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(data, gxm_data_w, set_data, gxm_set_data_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(message_type, gxm_message_type_w, set_message_type, gxm_set_message_type_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(new, gxm_new_w, set_new, gxm_set_new_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(property, gxm_property_w, set_property, gxm_set_property_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(display, gxm_display_w, set_display, gxm_set_display_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(target, gxm_target_w, set_target, gxm_set_target_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(requestor, gxm_requestor_w, set_requestor, gxm_set_requestor_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(owner, gxm_owner_w, set_owner, gxm_set_owner_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(selection, gxm_selection_w, set_selection, gxm_set_selection_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(atom, gxm_atom_w, set_atom, gxm_set_atom_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(place, gxm_place_w, set_place, gxm_set_place_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(value_mask, gxm_value_mask_w, set_value_mask, gxm_set_value_mask_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(above, gxm_above_w, set_above, gxm_set_above_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(from_configure, gxm_from_configure_w, set_from_configure, gxm_set_from_configure_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(event, gxm_event_w, set_event, gxm_set_event_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(override_redirect, gxm_override_redirect_w, set_override_redirect, gxm_set_override_redirect_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(border_width, gxm_border_width_w, set_border_width, gxm_set_border_width_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(parent, gxm_parent_w, set_parent, gxm_set_parent_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(minor_code, gxm_minor_code_w, set_minor_code, gxm_set_minor_code_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(major_code, gxm_major_code_w, set_major_code, gxm_set_major_code_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(drawable, gxm_drawable_w, set_drawable, gxm_set_drawable_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(count, gxm_count_w, set_count, gxm_set_count_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(key_vector, gxm_key_vector_w, set_key_vector, gxm_set_key_vector_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(focus, gxm_focus_w, set_focus, gxm_set_focus_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(detail, gxm_detail_w, set_detail, gxm_set_detail_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(mode, gxm_mode_w, set_mode, gxm_set_mode_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(is_hint, gxm_is_hint_w, set_is_hint, gxm_set_is_hint_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(button, gxm_button_w, set_button, gxm_set_button_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(same_screen, gxm_same_screen_w, set_same_screen, gxm_set_same_screen_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(keycode, gxm_keycode_w, set_keycode, gxm_set_keycode_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(state, gxm_state_w, set_state, gxm_set_state_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(y_root, gxm_y_root_w, set_y_root, gxm_set_y_root_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(x_root, gxm_x_root_w, set_x_root, gxm_set_x_root_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(root, gxm_root_w, set_root, gxm_set_root_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(time, gxm_time_w, set_time, gxm_set_time_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(subwindow, gxm_subwindow_w, set_subwindow, gxm_set_subwindow_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(window, gxm_window_w, set_window, gxm_set_window_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(send_event, gxm_send_event_w, set_send_event, gxm_set_send_event_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(serial, gxm_serial_w, set_serial, gxm_set_serial_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(type, gxm_type_w, set_type, gxm_set_type_w, 1, 0, 2, 0); 
  XM_DEFINE_ACCESSOR(colormap, gxm_colormap_w, set_colormap, gxm_set_colormap_w, 1, 0, 2, 0);
}
