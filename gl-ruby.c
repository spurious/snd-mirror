/* Ruby connection for gl.c */

#if USE_MOTIF
XEN_NARGIFY_3(gxg_glXChooseVisual_w, gxg_glXChooseVisual)
XEN_NARGIFY_4(gxg_glXCopyContext_w, gxg_glXCopyContext)
XEN_NARGIFY_4(gxg_glXCreateContext_w, gxg_glXCreateContext)
XEN_NARGIFY_3(gxg_glXCreateGLXPixmap_w, gxg_glXCreateGLXPixmap)
XEN_NARGIFY_2(gxg_glXDestroyContext_w, gxg_glXDestroyContext)
XEN_NARGIFY_2(gxg_glXDestroyGLXPixmap_w, gxg_glXDestroyGLXPixmap)
XEN_ARGIFY_4(gxg_glXGetConfig_w, gxg_glXGetConfig)
XEN_NARGIFY_0(gxg_glXGetCurrentContext_w, gxg_glXGetCurrentContext)
XEN_NARGIFY_0(gxg_glXGetCurrentDrawable_w, gxg_glXGetCurrentDrawable)
XEN_NARGIFY_2(gxg_glXIsDirect_w, gxg_glXIsDirect)
XEN_NARGIFY_3(gxg_glXMakeCurrent_w, gxg_glXMakeCurrent)
XEN_ARGIFY_3(gxg_glXQueryExtension_w, gxg_glXQueryExtension)
XEN_ARGIFY_3(gxg_glXQueryVersion_w, gxg_glXQueryVersion)
XEN_NARGIFY_2(gxg_glXSwapBuffers_w, gxg_glXSwapBuffers)
XEN_NARGIFY_4(gxg_glXUseXFont_w, gxg_glXUseXFont)
XEN_NARGIFY_0(gxg_glXWaitGL_w, gxg_glXWaitGL)
XEN_NARGIFY_0(gxg_glXWaitX_w, gxg_glXWaitX)
XEN_NARGIFY_2(gxg_glXGetClientString_w, gxg_glXGetClientString)
XEN_NARGIFY_3(gxg_glXQueryServerString_w, gxg_glXQueryServerString)
XEN_NARGIFY_2(gxg_glXQueryExtensionsString_w, gxg_glXQueryExtensionsString)
#endif
XEN_NARGIFY_1(gxg_glClearIndex_w, gxg_glClearIndex)
XEN_NARGIFY_4(gxg_glClearColor_w, gxg_glClearColor)
XEN_NARGIFY_1(gxg_glClear_w, gxg_glClear)
XEN_NARGIFY_1(gxg_glIndexMask_w, gxg_glIndexMask)
XEN_NARGIFY_4(gxg_glColorMask_w, gxg_glColorMask)
XEN_NARGIFY_2(gxg_glAlphaFunc_w, gxg_glAlphaFunc)
XEN_NARGIFY_2(gxg_glBlendFunc_w, gxg_glBlendFunc)
XEN_NARGIFY_1(gxg_glLogicOp_w, gxg_glLogicOp)
XEN_NARGIFY_1(gxg_glCullFace_w, gxg_glCullFace)
XEN_NARGIFY_1(gxg_glFrontFace_w, gxg_glFrontFace)
XEN_NARGIFY_1(gxg_glPointSize_w, gxg_glPointSize)
XEN_NARGIFY_1(gxg_glLineWidth_w, gxg_glLineWidth)
XEN_NARGIFY_2(gxg_glLineStipple_w, gxg_glLineStipple)
XEN_NARGIFY_2(gxg_glPolygonMode_w, gxg_glPolygonMode)
XEN_NARGIFY_2(gxg_glPolygonOffset_w, gxg_glPolygonOffset)
XEN_NARGIFY_1(gxg_glPolygonStipple_w, gxg_glPolygonStipple)
XEN_NARGIFY_1(gxg_glEdgeFlag_w, gxg_glEdgeFlag)
XEN_NARGIFY_4(gxg_glScissor_w, gxg_glScissor)
XEN_NARGIFY_2(gxg_glClipPlane_w, gxg_glClipPlane)
XEN_ARGIFY_2(gxg_glGetClipPlane_w, gxg_glGetClipPlane)
XEN_NARGIFY_1(gxg_glDrawBuffer_w, gxg_glDrawBuffer)
XEN_NARGIFY_1(gxg_glReadBuffer_w, gxg_glReadBuffer)
XEN_NARGIFY_1(gxg_glEnable_w, gxg_glEnable)
XEN_NARGIFY_1(gxg_glDisable_w, gxg_glDisable)
XEN_NARGIFY_1(gxg_glIsEnabled_w, gxg_glIsEnabled)
XEN_NARGIFY_1(gxg_glEnableClientState_w, gxg_glEnableClientState)
XEN_NARGIFY_1(gxg_glDisableClientState_w, gxg_glDisableClientState)
XEN_ARGIFY_2(gxg_glGetBooleanv_w, gxg_glGetBooleanv)
XEN_ARGIFY_2(gxg_glGetDoublev_w, gxg_glGetDoublev)
XEN_ARGIFY_2(gxg_glGetFloatv_w, gxg_glGetFloatv)
XEN_ARGIFY_2(gxg_glGetIntegerv_w, gxg_glGetIntegerv)
XEN_NARGIFY_1(gxg_glPushAttrib_w, gxg_glPushAttrib)
XEN_NARGIFY_0(gxg_glPopAttrib_w, gxg_glPopAttrib)
XEN_NARGIFY_1(gxg_glPushClientAttrib_w, gxg_glPushClientAttrib)
XEN_NARGIFY_0(gxg_glPopClientAttrib_w, gxg_glPopClientAttrib)
XEN_NARGIFY_1(gxg_glRenderMode_w, gxg_glRenderMode)
XEN_NARGIFY_0(gxg_glGetError_w, gxg_glGetError)
XEN_NARGIFY_1(gxg_glGetString_w, gxg_glGetString)
XEN_NARGIFY_0(gxg_glFinish_w, gxg_glFinish)
XEN_NARGIFY_0(gxg_glFlush_w, gxg_glFlush)
XEN_NARGIFY_2(gxg_glHint_w, gxg_glHint)
XEN_NARGIFY_1(gxg_glClearDepth_w, gxg_glClearDepth)
XEN_NARGIFY_1(gxg_glDepthFunc_w, gxg_glDepthFunc)
XEN_NARGIFY_1(gxg_glDepthMask_w, gxg_glDepthMask)
XEN_NARGIFY_2(gxg_glDepthRange_w, gxg_glDepthRange)
XEN_NARGIFY_4(gxg_glClearAccum_w, gxg_glClearAccum)
XEN_NARGIFY_2(gxg_glAccum_w, gxg_glAccum)
XEN_NARGIFY_1(gxg_glMatrixMode_w, gxg_glMatrixMode)
XEN_NARGIFY_6(gxg_glOrtho_w, gxg_glOrtho)
XEN_NARGIFY_6(gxg_glFrustum_w, gxg_glFrustum)
XEN_NARGIFY_4(gxg_glViewport_w, gxg_glViewport)
XEN_NARGIFY_0(gxg_glPushMatrix_w, gxg_glPushMatrix)
XEN_NARGIFY_0(gxg_glPopMatrix_w, gxg_glPopMatrix)
XEN_NARGIFY_0(gxg_glLoadIdentity_w, gxg_glLoadIdentity)
XEN_NARGIFY_1(gxg_glLoadMatrixd_w, gxg_glLoadMatrixd)
XEN_NARGIFY_1(gxg_glLoadMatrixf_w, gxg_glLoadMatrixf)
XEN_NARGIFY_1(gxg_glMultMatrixd_w, gxg_glMultMatrixd)
XEN_NARGIFY_1(gxg_glMultMatrixf_w, gxg_glMultMatrixf)
XEN_NARGIFY_4(gxg_glRotated_w, gxg_glRotated)
XEN_NARGIFY_4(gxg_glRotatef_w, gxg_glRotatef)
XEN_NARGIFY_3(gxg_glScaled_w, gxg_glScaled)
XEN_NARGIFY_3(gxg_glScalef_w, gxg_glScalef)
XEN_NARGIFY_3(gxg_glTranslated_w, gxg_glTranslated)
XEN_NARGIFY_3(gxg_glTranslatef_w, gxg_glTranslatef)
XEN_NARGIFY_1(gxg_glIsList_w, gxg_glIsList)
XEN_NARGIFY_2(gxg_glDeleteLists_w, gxg_glDeleteLists)
XEN_NARGIFY_1(gxg_glGenLists_w, gxg_glGenLists)
XEN_NARGIFY_2(gxg_glNewList_w, gxg_glNewList)
XEN_NARGIFY_0(gxg_glEndList_w, gxg_glEndList)
XEN_NARGIFY_1(gxg_glCallList_w, gxg_glCallList)
XEN_NARGIFY_3(gxg_glCallLists_w, gxg_glCallLists)
XEN_NARGIFY_1(gxg_glListBase_w, gxg_glListBase)
XEN_NARGIFY_1(gxg_glBegin_w, gxg_glBegin)
XEN_NARGIFY_0(gxg_glEnd_w, gxg_glEnd)
XEN_NARGIFY_2(gxg_glVertex2d_w, gxg_glVertex2d)
XEN_NARGIFY_2(gxg_glVertex2f_w, gxg_glVertex2f)
XEN_NARGIFY_2(gxg_glVertex2i_w, gxg_glVertex2i)
XEN_NARGIFY_2(gxg_glVertex2s_w, gxg_glVertex2s)
XEN_NARGIFY_3(gxg_glVertex3d_w, gxg_glVertex3d)
XEN_NARGIFY_3(gxg_glVertex3f_w, gxg_glVertex3f)
XEN_NARGIFY_3(gxg_glVertex3i_w, gxg_glVertex3i)
XEN_NARGIFY_3(gxg_glVertex3s_w, gxg_glVertex3s)
XEN_NARGIFY_4(gxg_glVertex4d_w, gxg_glVertex4d)
XEN_NARGIFY_4(gxg_glVertex4f_w, gxg_glVertex4f)
XEN_NARGIFY_4(gxg_glVertex4i_w, gxg_glVertex4i)
XEN_NARGIFY_4(gxg_glVertex4s_w, gxg_glVertex4s)
XEN_NARGIFY_3(gxg_glNormal3b_w, gxg_glNormal3b)
XEN_NARGIFY_3(gxg_glNormal3d_w, gxg_glNormal3d)
XEN_NARGIFY_3(gxg_glNormal3f_w, gxg_glNormal3f)
XEN_NARGIFY_3(gxg_glNormal3i_w, gxg_glNormal3i)
XEN_NARGIFY_3(gxg_glNormal3s_w, gxg_glNormal3s)
XEN_NARGIFY_1(gxg_glIndexd_w, gxg_glIndexd)
XEN_NARGIFY_1(gxg_glIndexf_w, gxg_glIndexf)
XEN_NARGIFY_1(gxg_glIndexi_w, gxg_glIndexi)
XEN_NARGIFY_1(gxg_glIndexs_w, gxg_glIndexs)
XEN_NARGIFY_1(gxg_glIndexub_w, gxg_glIndexub)
XEN_NARGIFY_3(gxg_glColor3b_w, gxg_glColor3b)
XEN_NARGIFY_3(gxg_glColor3d_w, gxg_glColor3d)
XEN_NARGIFY_3(gxg_glColor3f_w, gxg_glColor3f)
XEN_NARGIFY_3(gxg_glColor3i_w, gxg_glColor3i)
XEN_NARGIFY_3(gxg_glColor3s_w, gxg_glColor3s)
XEN_NARGIFY_3(gxg_glColor3ub_w, gxg_glColor3ub)
XEN_NARGIFY_3(gxg_glColor3ui_w, gxg_glColor3ui)
XEN_NARGIFY_3(gxg_glColor3us_w, gxg_glColor3us)
XEN_NARGIFY_4(gxg_glColor4b_w, gxg_glColor4b)
XEN_NARGIFY_4(gxg_glColor4d_w, gxg_glColor4d)
XEN_NARGIFY_4(gxg_glColor4f_w, gxg_glColor4f)
XEN_NARGIFY_4(gxg_glColor4i_w, gxg_glColor4i)
XEN_NARGIFY_4(gxg_glColor4s_w, gxg_glColor4s)
XEN_NARGIFY_4(gxg_glColor4ub_w, gxg_glColor4ub)
XEN_NARGIFY_4(gxg_glColor4ui_w, gxg_glColor4ui)
XEN_NARGIFY_4(gxg_glColor4us_w, gxg_glColor4us)
XEN_NARGIFY_1(gxg_glTexCoord1d_w, gxg_glTexCoord1d)
XEN_NARGIFY_1(gxg_glTexCoord1f_w, gxg_glTexCoord1f)
XEN_NARGIFY_1(gxg_glTexCoord1i_w, gxg_glTexCoord1i)
XEN_NARGIFY_1(gxg_glTexCoord1s_w, gxg_glTexCoord1s)
XEN_NARGIFY_2(gxg_glTexCoord2d_w, gxg_glTexCoord2d)
XEN_NARGIFY_2(gxg_glTexCoord2f_w, gxg_glTexCoord2f)
XEN_NARGIFY_2(gxg_glTexCoord2i_w, gxg_glTexCoord2i)
XEN_NARGIFY_2(gxg_glTexCoord2s_w, gxg_glTexCoord2s)
XEN_NARGIFY_3(gxg_glTexCoord3d_w, gxg_glTexCoord3d)
XEN_NARGIFY_3(gxg_glTexCoord3f_w, gxg_glTexCoord3f)
XEN_NARGIFY_3(gxg_glTexCoord3i_w, gxg_glTexCoord3i)
XEN_NARGIFY_3(gxg_glTexCoord3s_w, gxg_glTexCoord3s)
XEN_NARGIFY_4(gxg_glTexCoord4d_w, gxg_glTexCoord4d)
XEN_NARGIFY_4(gxg_glTexCoord4f_w, gxg_glTexCoord4f)
XEN_NARGIFY_4(gxg_glTexCoord4i_w, gxg_glTexCoord4i)
XEN_NARGIFY_4(gxg_glTexCoord4s_w, gxg_glTexCoord4s)
XEN_NARGIFY_2(gxg_glRasterPos2d_w, gxg_glRasterPos2d)
XEN_NARGIFY_2(gxg_glRasterPos2f_w, gxg_glRasterPos2f)
XEN_NARGIFY_2(gxg_glRasterPos2i_w, gxg_glRasterPos2i)
XEN_NARGIFY_2(gxg_glRasterPos2s_w, gxg_glRasterPos2s)
XEN_NARGIFY_3(gxg_glRasterPos3d_w, gxg_glRasterPos3d)
XEN_NARGIFY_3(gxg_glRasterPos3f_w, gxg_glRasterPos3f)
XEN_NARGIFY_3(gxg_glRasterPos3i_w, gxg_glRasterPos3i)
XEN_NARGIFY_3(gxg_glRasterPos3s_w, gxg_glRasterPos3s)
XEN_NARGIFY_4(gxg_glRasterPos4d_w, gxg_glRasterPos4d)
XEN_NARGIFY_4(gxg_glRasterPos4f_w, gxg_glRasterPos4f)
XEN_NARGIFY_4(gxg_glRasterPos4i_w, gxg_glRasterPos4i)
XEN_NARGIFY_4(gxg_glRasterPos4s_w, gxg_glRasterPos4s)
XEN_NARGIFY_4(gxg_glRectd_w, gxg_glRectd)
XEN_NARGIFY_4(gxg_glRectf_w, gxg_glRectf)
XEN_NARGIFY_4(gxg_glRecti_w, gxg_glRecti)
XEN_NARGIFY_4(gxg_glRects_w, gxg_glRects)
XEN_NARGIFY_4(gxg_glVertexPointer_w, gxg_glVertexPointer)
XEN_NARGIFY_3(gxg_glNormalPointer_w, gxg_glNormalPointer)
XEN_NARGIFY_4(gxg_glColorPointer_w, gxg_glColorPointer)
XEN_NARGIFY_3(gxg_glIndexPointer_w, gxg_glIndexPointer)
XEN_NARGIFY_4(gxg_glTexCoordPointer_w, gxg_glTexCoordPointer)
XEN_NARGIFY_2(gxg_glEdgeFlagPointer_w, gxg_glEdgeFlagPointer)
XEN_ARGIFY_2(gxg_glGetPointerv_w, gxg_glGetPointerv)
XEN_NARGIFY_1(gxg_glArrayElement_w, gxg_glArrayElement)
XEN_NARGIFY_3(gxg_glDrawArrays_w, gxg_glDrawArrays)
XEN_NARGIFY_4(gxg_glDrawElements_w, gxg_glDrawElements)
XEN_NARGIFY_3(gxg_glInterleavedArrays_w, gxg_glInterleavedArrays)
XEN_NARGIFY_1(gxg_glShadeModel_w, gxg_glShadeModel)
XEN_NARGIFY_3(gxg_glLightf_w, gxg_glLightf)
XEN_NARGIFY_3(gxg_glLighti_w, gxg_glLighti)
XEN_ARGIFY_3(gxg_glGetLightfv_w, gxg_glGetLightfv)
XEN_ARGIFY_3(gxg_glGetLightiv_w, gxg_glGetLightiv)
XEN_NARGIFY_2(gxg_glLightModelf_w, gxg_glLightModelf)
XEN_NARGIFY_2(gxg_glLightModeli_w, gxg_glLightModeli)
XEN_NARGIFY_3(gxg_glMaterialf_w, gxg_glMaterialf)
XEN_NARGIFY_3(gxg_glMateriali_w, gxg_glMateriali)
XEN_ARGIFY_3(gxg_glGetMaterialfv_w, gxg_glGetMaterialfv)
XEN_ARGIFY_3(gxg_glGetMaterialiv_w, gxg_glGetMaterialiv)
XEN_NARGIFY_2(gxg_glColorMaterial_w, gxg_glColorMaterial)
XEN_NARGIFY_2(gxg_glPixelZoom_w, gxg_glPixelZoom)
XEN_NARGIFY_2(gxg_glPixelStoref_w, gxg_glPixelStoref)
XEN_NARGIFY_2(gxg_glPixelStorei_w, gxg_glPixelStorei)
XEN_NARGIFY_2(gxg_glPixelTransferf_w, gxg_glPixelTransferf)
XEN_NARGIFY_2(gxg_glPixelTransferi_w, gxg_glPixelTransferi)
XEN_ARGIFY_2(gxg_glGetPixelMapfv_w, gxg_glGetPixelMapfv)
XEN_ARGIFY_2(gxg_glGetPixelMapuiv_w, gxg_glGetPixelMapuiv)
XEN_ARGIFY_2(gxg_glGetPixelMapusv_w, gxg_glGetPixelMapusv)
XEN_NARGIFY_7(gxg_glBitmap_w, gxg_glBitmap)
XEN_NARGIFY_7(gxg_glReadPixels_w, gxg_glReadPixels)
XEN_NARGIFY_5(gxg_glDrawPixels_w, gxg_glDrawPixels)
XEN_NARGIFY_5(gxg_glCopyPixels_w, gxg_glCopyPixels)
XEN_NARGIFY_3(gxg_glStencilFunc_w, gxg_glStencilFunc)
XEN_NARGIFY_1(gxg_glStencilMask_w, gxg_glStencilMask)
XEN_NARGIFY_3(gxg_glStencilOp_w, gxg_glStencilOp)
XEN_NARGIFY_1(gxg_glClearStencil_w, gxg_glClearStencil)
XEN_NARGIFY_3(gxg_glTexGend_w, gxg_glTexGend)
XEN_NARGIFY_3(gxg_glTexGenf_w, gxg_glTexGenf)
XEN_NARGIFY_3(gxg_glTexGeni_w, gxg_glTexGeni)
XEN_ARGIFY_3(gxg_glGetTexGendv_w, gxg_glGetTexGendv)
XEN_ARGIFY_3(gxg_glGetTexGenfv_w, gxg_glGetTexGenfv)
XEN_ARGIFY_3(gxg_glGetTexGeniv_w, gxg_glGetTexGeniv)
XEN_NARGIFY_3(gxg_glTexEnvf_w, gxg_glTexEnvf)
XEN_NARGIFY_3(gxg_glTexEnvi_w, gxg_glTexEnvi)
XEN_ARGIFY_3(gxg_glGetTexEnvfv_w, gxg_glGetTexEnvfv)
XEN_ARGIFY_3(gxg_glGetTexEnviv_w, gxg_glGetTexEnviv)
XEN_NARGIFY_3(gxg_glTexParameterf_w, gxg_glTexParameterf)
XEN_NARGIFY_3(gxg_glTexParameteri_w, gxg_glTexParameteri)
XEN_ARGIFY_3(gxg_glGetTexParameterfv_w, gxg_glGetTexParameterfv)
XEN_ARGIFY_3(gxg_glGetTexParameteriv_w, gxg_glGetTexParameteriv)
XEN_ARGIFY_4(gxg_glGetTexLevelParameterfv_w, gxg_glGetTexLevelParameterfv)
XEN_ARGIFY_4(gxg_glGetTexLevelParameteriv_w, gxg_glGetTexLevelParameteriv)
XEN_NARGIFY_8(gxg_glTexImage1D_w, gxg_glTexImage1D)
XEN_NARGIFY_9(gxg_glTexImage2D_w, gxg_glTexImage2D)
XEN_NARGIFY_2(gxg_glGenTextures_w, gxg_glGenTextures)
XEN_NARGIFY_2(gxg_glDeleteTextures_w, gxg_glDeleteTextures)
XEN_NARGIFY_2(gxg_glBindTexture_w, gxg_glBindTexture)
XEN_NARGIFY_3(gxg_glAreTexturesResident_w, gxg_glAreTexturesResident)
XEN_NARGIFY_1(gxg_glIsTexture_w, gxg_glIsTexture)
XEN_NARGIFY_7(gxg_glTexSubImage1D_w, gxg_glTexSubImage1D)
XEN_NARGIFY_9(gxg_glTexSubImage2D_w, gxg_glTexSubImage2D)
XEN_NARGIFY_7(gxg_glCopyTexImage1D_w, gxg_glCopyTexImage1D)
XEN_NARGIFY_8(gxg_glCopyTexImage2D_w, gxg_glCopyTexImage2D)
XEN_NARGIFY_6(gxg_glCopyTexSubImage1D_w, gxg_glCopyTexSubImage1D)
XEN_NARGIFY_8(gxg_glCopyTexSubImage2D_w, gxg_glCopyTexSubImage2D)
XEN_NARGIFY_6(gxg_glMap1d_w, gxg_glMap1d)
XEN_NARGIFY_6(gxg_glMap1f_w, gxg_glMap1f)
XEN_VARGIFY(gxg_glMap2d_w, gxg_glMap2d)
XEN_VARGIFY(gxg_glMap2f_w, gxg_glMap2f)
XEN_ARGIFY_3(gxg_glGetMapdv_w, gxg_glGetMapdv)
XEN_ARGIFY_3(gxg_glGetMapfv_w, gxg_glGetMapfv)
XEN_ARGIFY_3(gxg_glGetMapiv_w, gxg_glGetMapiv)
XEN_NARGIFY_1(gxg_glEvalCoord1d_w, gxg_glEvalCoord1d)
XEN_NARGIFY_1(gxg_glEvalCoord1f_w, gxg_glEvalCoord1f)
XEN_NARGIFY_2(gxg_glEvalCoord2d_w, gxg_glEvalCoord2d)
XEN_NARGIFY_2(gxg_glEvalCoord2f_w, gxg_glEvalCoord2f)
XEN_NARGIFY_3(gxg_glMapGrid1d_w, gxg_glMapGrid1d)
XEN_NARGIFY_3(gxg_glMapGrid1f_w, gxg_glMapGrid1f)
XEN_NARGIFY_6(gxg_glMapGrid2d_w, gxg_glMapGrid2d)
XEN_NARGIFY_6(gxg_glMapGrid2f_w, gxg_glMapGrid2f)
XEN_NARGIFY_1(gxg_glEvalPoint1_w, gxg_glEvalPoint1)
XEN_NARGIFY_2(gxg_glEvalPoint2_w, gxg_glEvalPoint2)
XEN_NARGIFY_3(gxg_glEvalMesh1_w, gxg_glEvalMesh1)
XEN_NARGIFY_5(gxg_glEvalMesh2_w, gxg_glEvalMesh2)
XEN_NARGIFY_2(gxg_glFogf_w, gxg_glFogf)
XEN_NARGIFY_2(gxg_glFogi_w, gxg_glFogi)
XEN_NARGIFY_3(gxg_glFeedbackBuffer_w, gxg_glFeedbackBuffer)
XEN_NARGIFY_1(gxg_glPassThrough_w, gxg_glPassThrough)
XEN_NARGIFY_2(gxg_glSelectBuffer_w, gxg_glSelectBuffer)
XEN_NARGIFY_0(gxg_glInitNames_w, gxg_glInitNames)
XEN_NARGIFY_1(gxg_glLoadName_w, gxg_glLoadName)
XEN_NARGIFY_1(gxg_glPushName_w, gxg_glPushName)
XEN_NARGIFY_0(gxg_glPopName_w, gxg_glPopName)
XEN_NARGIFY_6(gxg_glDrawRangeElements_w, gxg_glDrawRangeElements)
XEN_VARGIFY(gxg_glTexImage3D_w, gxg_glTexImage3D)
XEN_VARGIFY(gxg_glTexSubImage3D_w, gxg_glTexSubImage3D)
XEN_NARGIFY_9(gxg_glCopyTexSubImage3D_w, gxg_glCopyTexSubImage3D)
XEN_NARGIFY_6(gxg_glColorTable_w, gxg_glColorTable)
XEN_NARGIFY_6(gxg_glColorSubTable_w, gxg_glColorSubTable)
XEN_NARGIFY_5(gxg_glCopyColorSubTable_w, gxg_glCopyColorSubTable)
XEN_NARGIFY_5(gxg_glCopyColorTable_w, gxg_glCopyColorTable)
XEN_ARGIFY_3(gxg_glGetColorTableParameterfv_w, gxg_glGetColorTableParameterfv)
XEN_ARGIFY_3(gxg_glGetColorTableParameteriv_w, gxg_glGetColorTableParameteriv)
XEN_NARGIFY_1(gxg_glBlendEquation_w, gxg_glBlendEquation)
XEN_NARGIFY_4(gxg_glBlendColor_w, gxg_glBlendColor)
XEN_NARGIFY_4(gxg_glHistogram_w, gxg_glHistogram)
XEN_NARGIFY_1(gxg_glResetHistogram_w, gxg_glResetHistogram)
XEN_NARGIFY_5(gxg_glGetHistogram_w, gxg_glGetHistogram)
XEN_ARGIFY_3(gxg_glGetHistogramParameterfv_w, gxg_glGetHistogramParameterfv)
XEN_ARGIFY_3(gxg_glGetHistogramParameteriv_w, gxg_glGetHistogramParameteriv)
XEN_NARGIFY_3(gxg_glMinmax_w, gxg_glMinmax)
XEN_NARGIFY_1(gxg_glResetMinmax_w, gxg_glResetMinmax)
XEN_NARGIFY_5(gxg_glGetMinmax_w, gxg_glGetMinmax)
XEN_ARGIFY_3(gxg_glGetMinmaxParameterfv_w, gxg_glGetMinmaxParameterfv)
XEN_ARGIFY_3(gxg_glGetMinmaxParameteriv_w, gxg_glGetMinmaxParameteriv)
XEN_NARGIFY_6(gxg_glConvolutionFilter1D_w, gxg_glConvolutionFilter1D)
XEN_NARGIFY_7(gxg_glConvolutionFilter2D_w, gxg_glConvolutionFilter2D)
XEN_NARGIFY_3(gxg_glConvolutionParameterf_w, gxg_glConvolutionParameterf)
XEN_NARGIFY_3(gxg_glConvolutionParameteri_w, gxg_glConvolutionParameteri)
XEN_NARGIFY_5(gxg_glCopyConvolutionFilter1D_w, gxg_glCopyConvolutionFilter1D)
XEN_NARGIFY_6(gxg_glCopyConvolutionFilter2D_w, gxg_glCopyConvolutionFilter2D)
XEN_NARGIFY_8(gxg_glSeparableFilter2D_w, gxg_glSeparableFilter2D)
#ifdef GLU_VERSION_1_2
XEN_NARGIFY_1(gxg_gluBeginPolygon_w, gxg_gluBeginPolygon)
#endif
XEN_NARGIFY_6(gxg_gluBuild1DMipmaps_w, gxg_gluBuild1DMipmaps)
XEN_NARGIFY_7(gxg_gluBuild2DMipmaps_w, gxg_gluBuild2DMipmaps)
#ifdef GLU_VERSION_1_2
XEN_NARGIFY_1(gxg_gluDeleteTess_w, gxg_gluDeleteTess)
#endif
#ifdef GLU_VERSION_1_2
XEN_NARGIFY_1(gxg_gluEndPolygon_w, gxg_gluEndPolygon)
#endif
XEN_NARGIFY_1(gxg_gluErrorString_w, gxg_gluErrorString)
XEN_NARGIFY_1(gxg_gluGetString_w, gxg_gluGetString)
#ifdef GLU_VERSION_1_2
XEN_NARGIFY_3(gxg_gluGetTessProperty_w, gxg_gluGetTessProperty)
#endif
XEN_NARGIFY_9(gxg_gluLookAt_w, gxg_gluLookAt)
#ifdef GLU_VERSION_1_2
XEN_NARGIFY_0(gxg_gluNewTess_w, gxg_gluNewTess)
#endif
#ifdef GLU_VERSION_1_2
XEN_NARGIFY_2(gxg_gluNextContour_w, gxg_gluNextContour)
#endif
XEN_NARGIFY_4(gxg_gluOrtho2D_w, gxg_gluOrtho2D)
XEN_NARGIFY_4(gxg_gluPerspective_w, gxg_gluPerspective)
XEN_NARGIFY_5(gxg_gluPickMatrix_w, gxg_gluPickMatrix)
XEN_NARGIFY_9(gxg_gluProject_w, gxg_gluProject)
XEN_NARGIFY_9(gxg_gluScaleImage_w, gxg_gluScaleImage)
#ifdef GLU_VERSION_1_2
XEN_NARGIFY_1(gxg_gluTessBeginContour_w, gxg_gluTessBeginContour)
#endif
#ifdef GLU_VERSION_1_2
XEN_NARGIFY_2(gxg_gluTessBeginPolygon_w, gxg_gluTessBeginPolygon)
#endif
#ifdef GLU_VERSION_1_2
XEN_NARGIFY_1(gxg_gluTessEndContour_w, gxg_gluTessEndContour)
#endif
#ifdef GLU_VERSION_1_2
XEN_NARGIFY_1(gxg_gluTessEndPolygon_w, gxg_gluTessEndPolygon)
#endif
#ifdef GLU_VERSION_1_2
XEN_NARGIFY_4(gxg_gluTessNormal_w, gxg_gluTessNormal)
#endif
#ifdef GLU_VERSION_1_2
XEN_NARGIFY_3(gxg_gluTessProperty_w, gxg_gluTessProperty)
#endif
#ifdef GLU_VERSION_1_2
XEN_NARGIFY_3(gxg_gluTessVertex_w, gxg_gluTessVertex)
#endif
XEN_NARGIFY_9(gxg_gluUnProject_w, gxg_gluUnProject)
static void define_functions(void)
{
  #define GL_DEFINE_PROCEDURE(Name, Value, A1, A2, A3, Help) XEN_DEFINE_PROCEDURE(XL_PRE #Name XL_POST, Value, A1, A2, A3, Help)
#if USE_MOTIF
  GL_DEFINE_PROCEDURE(glXChooseVisual, gxg_glXChooseVisual_w, 3, 0, 0, H_glXChooseVisual);
  GL_DEFINE_PROCEDURE(glXCopyContext, gxg_glXCopyContext_w, 4, 0, 0, H_glXCopyContext);
  GL_DEFINE_PROCEDURE(glXCreateContext, gxg_glXCreateContext_w, 4, 0, 0, H_glXCreateContext);
  GL_DEFINE_PROCEDURE(glXCreateGLXPixmap, gxg_glXCreateGLXPixmap_w, 3, 0, 0, H_glXCreateGLXPixmap);
  GL_DEFINE_PROCEDURE(glXDestroyContext, gxg_glXDestroyContext_w, 2, 0, 0, H_glXDestroyContext);
  GL_DEFINE_PROCEDURE(glXDestroyGLXPixmap, gxg_glXDestroyGLXPixmap_w, 2, 0, 0, H_glXDestroyGLXPixmap);
  GL_DEFINE_PROCEDURE(glXGetConfig, gxg_glXGetConfig_w, 3, 1, 0, H_glXGetConfig);
  GL_DEFINE_PROCEDURE(glXGetCurrentContext, gxg_glXGetCurrentContext_w, 0, 0, 0, H_glXGetCurrentContext);
  GL_DEFINE_PROCEDURE(glXGetCurrentDrawable, gxg_glXGetCurrentDrawable_w, 0, 0, 0, H_glXGetCurrentDrawable);
  GL_DEFINE_PROCEDURE(glXIsDirect, gxg_glXIsDirect_w, 2, 0, 0, H_glXIsDirect);
  GL_DEFINE_PROCEDURE(glXMakeCurrent, gxg_glXMakeCurrent_w, 3, 0, 0, H_glXMakeCurrent);
  GL_DEFINE_PROCEDURE(glXQueryExtension, gxg_glXQueryExtension_w, 1, 2, 0, H_glXQueryExtension);
  GL_DEFINE_PROCEDURE(glXQueryVersion, gxg_glXQueryVersion_w, 1, 2, 0, H_glXQueryVersion);
  GL_DEFINE_PROCEDURE(glXSwapBuffers, gxg_glXSwapBuffers_w, 2, 0, 0, H_glXSwapBuffers);
  GL_DEFINE_PROCEDURE(glXUseXFont, gxg_glXUseXFont_w, 4, 0, 0, H_glXUseXFont);
  GL_DEFINE_PROCEDURE(glXWaitGL, gxg_glXWaitGL_w, 0, 0, 0, H_glXWaitGL);
  GL_DEFINE_PROCEDURE(glXWaitX, gxg_glXWaitX_w, 0, 0, 0, H_glXWaitX);
  GL_DEFINE_PROCEDURE(glXGetClientString, gxg_glXGetClientString_w, 2, 0, 0, H_glXGetClientString);
  GL_DEFINE_PROCEDURE(glXQueryServerString, gxg_glXQueryServerString_w, 3, 0, 0, H_glXQueryServerString);
  GL_DEFINE_PROCEDURE(glXQueryExtensionsString, gxg_glXQueryExtensionsString_w, 2, 0, 0, H_glXQueryExtensionsString);
#endif
  GL_DEFINE_PROCEDURE(glClearIndex, gxg_glClearIndex_w, 1, 0, 0, H_glClearIndex);
  GL_DEFINE_PROCEDURE(glClearColor, gxg_glClearColor_w, 4, 0, 0, H_glClearColor);
  GL_DEFINE_PROCEDURE(glClear, gxg_glClear_w, 1, 0, 0, H_glClear);
  GL_DEFINE_PROCEDURE(glIndexMask, gxg_glIndexMask_w, 1, 0, 0, H_glIndexMask);
  GL_DEFINE_PROCEDURE(glColorMask, gxg_glColorMask_w, 4, 0, 0, H_glColorMask);
  GL_DEFINE_PROCEDURE(glAlphaFunc, gxg_glAlphaFunc_w, 2, 0, 0, H_glAlphaFunc);
  GL_DEFINE_PROCEDURE(glBlendFunc, gxg_glBlendFunc_w, 2, 0, 0, H_glBlendFunc);
  GL_DEFINE_PROCEDURE(glLogicOp, gxg_glLogicOp_w, 1, 0, 0, H_glLogicOp);
  GL_DEFINE_PROCEDURE(glCullFace, gxg_glCullFace_w, 1, 0, 0, H_glCullFace);
  GL_DEFINE_PROCEDURE(glFrontFace, gxg_glFrontFace_w, 1, 0, 0, H_glFrontFace);
  GL_DEFINE_PROCEDURE(glPointSize, gxg_glPointSize_w, 1, 0, 0, H_glPointSize);
  GL_DEFINE_PROCEDURE(glLineWidth, gxg_glLineWidth_w, 1, 0, 0, H_glLineWidth);
  GL_DEFINE_PROCEDURE(glLineStipple, gxg_glLineStipple_w, 2, 0, 0, H_glLineStipple);
  GL_DEFINE_PROCEDURE(glPolygonMode, gxg_glPolygonMode_w, 2, 0, 0, H_glPolygonMode);
  GL_DEFINE_PROCEDURE(glPolygonOffset, gxg_glPolygonOffset_w, 2, 0, 0, H_glPolygonOffset);
  GL_DEFINE_PROCEDURE(glPolygonStipple, gxg_glPolygonStipple_w, 1, 0, 0, H_glPolygonStipple);
  GL_DEFINE_PROCEDURE(glEdgeFlag, gxg_glEdgeFlag_w, 1, 0, 0, H_glEdgeFlag);
  GL_DEFINE_PROCEDURE(glScissor, gxg_glScissor_w, 4, 0, 0, H_glScissor);
  GL_DEFINE_PROCEDURE(glClipPlane, gxg_glClipPlane_w, 2, 0, 0, H_glClipPlane);
  GL_DEFINE_PROCEDURE(glGetClipPlane, gxg_glGetClipPlane_w, 1, 1, 0, H_glGetClipPlane);
  GL_DEFINE_PROCEDURE(glDrawBuffer, gxg_glDrawBuffer_w, 1, 0, 0, H_glDrawBuffer);
  GL_DEFINE_PROCEDURE(glReadBuffer, gxg_glReadBuffer_w, 1, 0, 0, H_glReadBuffer);
  GL_DEFINE_PROCEDURE(glEnable, gxg_glEnable_w, 1, 0, 0, H_glEnable);
  GL_DEFINE_PROCEDURE(glDisable, gxg_glDisable_w, 1, 0, 0, H_glDisable);
  GL_DEFINE_PROCEDURE(glIsEnabled, gxg_glIsEnabled_w, 1, 0, 0, H_glIsEnabled);
  GL_DEFINE_PROCEDURE(glEnableClientState, gxg_glEnableClientState_w, 1, 0, 0, H_glEnableClientState);
  GL_DEFINE_PROCEDURE(glDisableClientState, gxg_glDisableClientState_w, 1, 0, 0, H_glDisableClientState);
  GL_DEFINE_PROCEDURE(glGetBooleanv, gxg_glGetBooleanv_w, 1, 1, 0, H_glGetBooleanv);
  GL_DEFINE_PROCEDURE(glGetDoublev, gxg_glGetDoublev_w, 1, 1, 0, H_glGetDoublev);
  GL_DEFINE_PROCEDURE(glGetFloatv, gxg_glGetFloatv_w, 1, 1, 0, H_glGetFloatv);
  GL_DEFINE_PROCEDURE(glGetIntegerv, gxg_glGetIntegerv_w, 1, 1, 0, H_glGetIntegerv);
  GL_DEFINE_PROCEDURE(glPushAttrib, gxg_glPushAttrib_w, 1, 0, 0, H_glPushAttrib);
  GL_DEFINE_PROCEDURE(glPopAttrib, gxg_glPopAttrib_w, 0, 0, 0, H_glPopAttrib);
  GL_DEFINE_PROCEDURE(glPushClientAttrib, gxg_glPushClientAttrib_w, 1, 0, 0, H_glPushClientAttrib);
  GL_DEFINE_PROCEDURE(glPopClientAttrib, gxg_glPopClientAttrib_w, 0, 0, 0, H_glPopClientAttrib);
  GL_DEFINE_PROCEDURE(glRenderMode, gxg_glRenderMode_w, 1, 0, 0, H_glRenderMode);
  GL_DEFINE_PROCEDURE(glGetError, gxg_glGetError_w, 0, 0, 0, H_glGetError);
  GL_DEFINE_PROCEDURE(glGetString, gxg_glGetString_w, 1, 0, 0, H_glGetString);
  GL_DEFINE_PROCEDURE(glFinish, gxg_glFinish_w, 0, 0, 0, H_glFinish);
  GL_DEFINE_PROCEDURE(glFlush, gxg_glFlush_w, 0, 0, 0, H_glFlush);
  GL_DEFINE_PROCEDURE(glHint, gxg_glHint_w, 2, 0, 0, H_glHint);
  GL_DEFINE_PROCEDURE(glClearDepth, gxg_glClearDepth_w, 1, 0, 0, H_glClearDepth);
  GL_DEFINE_PROCEDURE(glDepthFunc, gxg_glDepthFunc_w, 1, 0, 0, H_glDepthFunc);
  GL_DEFINE_PROCEDURE(glDepthMask, gxg_glDepthMask_w, 1, 0, 0, H_glDepthMask);
  GL_DEFINE_PROCEDURE(glDepthRange, gxg_glDepthRange_w, 2, 0, 0, H_glDepthRange);
  GL_DEFINE_PROCEDURE(glClearAccum, gxg_glClearAccum_w, 4, 0, 0, H_glClearAccum);
  GL_DEFINE_PROCEDURE(glAccum, gxg_glAccum_w, 2, 0, 0, H_glAccum);
  GL_DEFINE_PROCEDURE(glMatrixMode, gxg_glMatrixMode_w, 1, 0, 0, H_glMatrixMode);
  GL_DEFINE_PROCEDURE(glOrtho, gxg_glOrtho_w, 6, 0, 0, H_glOrtho);
  GL_DEFINE_PROCEDURE(glFrustum, gxg_glFrustum_w, 6, 0, 0, H_glFrustum);
  GL_DEFINE_PROCEDURE(glViewport, gxg_glViewport_w, 4, 0, 0, H_glViewport);
  GL_DEFINE_PROCEDURE(glPushMatrix, gxg_glPushMatrix_w, 0, 0, 0, H_glPushMatrix);
  GL_DEFINE_PROCEDURE(glPopMatrix, gxg_glPopMatrix_w, 0, 0, 0, H_glPopMatrix);
  GL_DEFINE_PROCEDURE(glLoadIdentity, gxg_glLoadIdentity_w, 0, 0, 0, H_glLoadIdentity);
  GL_DEFINE_PROCEDURE(glLoadMatrixd, gxg_glLoadMatrixd_w, 1, 0, 0, H_glLoadMatrixd);
  GL_DEFINE_PROCEDURE(glLoadMatrixf, gxg_glLoadMatrixf_w, 1, 0, 0, H_glLoadMatrixf);
  GL_DEFINE_PROCEDURE(glMultMatrixd, gxg_glMultMatrixd_w, 1, 0, 0, H_glMultMatrixd);
  GL_DEFINE_PROCEDURE(glMultMatrixf, gxg_glMultMatrixf_w, 1, 0, 0, H_glMultMatrixf);
  GL_DEFINE_PROCEDURE(glRotated, gxg_glRotated_w, 4, 0, 0, H_glRotated);
  GL_DEFINE_PROCEDURE(glRotatef, gxg_glRotatef_w, 4, 0, 0, H_glRotatef);
  GL_DEFINE_PROCEDURE(glScaled, gxg_glScaled_w, 3, 0, 0, H_glScaled);
  GL_DEFINE_PROCEDURE(glScalef, gxg_glScalef_w, 3, 0, 0, H_glScalef);
  GL_DEFINE_PROCEDURE(glTranslated, gxg_glTranslated_w, 3, 0, 0, H_glTranslated);
  GL_DEFINE_PROCEDURE(glTranslatef, gxg_glTranslatef_w, 3, 0, 0, H_glTranslatef);
  GL_DEFINE_PROCEDURE(glIsList, gxg_glIsList_w, 1, 0, 0, H_glIsList);
  GL_DEFINE_PROCEDURE(glDeleteLists, gxg_glDeleteLists_w, 2, 0, 0, H_glDeleteLists);
  GL_DEFINE_PROCEDURE(glGenLists, gxg_glGenLists_w, 1, 0, 0, H_glGenLists);
  GL_DEFINE_PROCEDURE(glNewList, gxg_glNewList_w, 2, 0, 0, H_glNewList);
  GL_DEFINE_PROCEDURE(glEndList, gxg_glEndList_w, 0, 0, 0, H_glEndList);
  GL_DEFINE_PROCEDURE(glCallList, gxg_glCallList_w, 1, 0, 0, H_glCallList);
  GL_DEFINE_PROCEDURE(glCallLists, gxg_glCallLists_w, 3, 0, 0, H_glCallLists);
  GL_DEFINE_PROCEDURE(glListBase, gxg_glListBase_w, 1, 0, 0, H_glListBase);
  GL_DEFINE_PROCEDURE(glBegin, gxg_glBegin_w, 1, 0, 0, H_glBegin);
  GL_DEFINE_PROCEDURE(glEnd, gxg_glEnd_w, 0, 0, 0, H_glEnd);
  GL_DEFINE_PROCEDURE(glVertex2d, gxg_glVertex2d_w, 2, 0, 0, H_glVertex2d);
  GL_DEFINE_PROCEDURE(glVertex2f, gxg_glVertex2f_w, 2, 0, 0, H_glVertex2f);
  GL_DEFINE_PROCEDURE(glVertex2i, gxg_glVertex2i_w, 2, 0, 0, H_glVertex2i);
  GL_DEFINE_PROCEDURE(glVertex2s, gxg_glVertex2s_w, 2, 0, 0, H_glVertex2s);
  GL_DEFINE_PROCEDURE(glVertex3d, gxg_glVertex3d_w, 3, 0, 0, H_glVertex3d);
  GL_DEFINE_PROCEDURE(glVertex3f, gxg_glVertex3f_w, 3, 0, 0, H_glVertex3f);
  GL_DEFINE_PROCEDURE(glVertex3i, gxg_glVertex3i_w, 3, 0, 0, H_glVertex3i);
  GL_DEFINE_PROCEDURE(glVertex3s, gxg_glVertex3s_w, 3, 0, 0, H_glVertex3s);
  GL_DEFINE_PROCEDURE(glVertex4d, gxg_glVertex4d_w, 4, 0, 0, H_glVertex4d);
  GL_DEFINE_PROCEDURE(glVertex4f, gxg_glVertex4f_w, 4, 0, 0, H_glVertex4f);
  GL_DEFINE_PROCEDURE(glVertex4i, gxg_glVertex4i_w, 4, 0, 0, H_glVertex4i);
  GL_DEFINE_PROCEDURE(glVertex4s, gxg_glVertex4s_w, 4, 0, 0, H_glVertex4s);
  GL_DEFINE_PROCEDURE(glNormal3b, gxg_glNormal3b_w, 3, 0, 0, H_glNormal3b);
  GL_DEFINE_PROCEDURE(glNormal3d, gxg_glNormal3d_w, 3, 0, 0, H_glNormal3d);
  GL_DEFINE_PROCEDURE(glNormal3f, gxg_glNormal3f_w, 3, 0, 0, H_glNormal3f);
  GL_DEFINE_PROCEDURE(glNormal3i, gxg_glNormal3i_w, 3, 0, 0, H_glNormal3i);
  GL_DEFINE_PROCEDURE(glNormal3s, gxg_glNormal3s_w, 3, 0, 0, H_glNormal3s);
  GL_DEFINE_PROCEDURE(glIndexd, gxg_glIndexd_w, 1, 0, 0, H_glIndexd);
  GL_DEFINE_PROCEDURE(glIndexf, gxg_glIndexf_w, 1, 0, 0, H_glIndexf);
  GL_DEFINE_PROCEDURE(glIndexi, gxg_glIndexi_w, 1, 0, 0, H_glIndexi);
  GL_DEFINE_PROCEDURE(glIndexs, gxg_glIndexs_w, 1, 0, 0, H_glIndexs);
  GL_DEFINE_PROCEDURE(glIndexub, gxg_glIndexub_w, 1, 0, 0, H_glIndexub);
  GL_DEFINE_PROCEDURE(glColor3b, gxg_glColor3b_w, 3, 0, 0, H_glColor3b);
  GL_DEFINE_PROCEDURE(glColor3d, gxg_glColor3d_w, 3, 0, 0, H_glColor3d);
  GL_DEFINE_PROCEDURE(glColor3f, gxg_glColor3f_w, 3, 0, 0, H_glColor3f);
  GL_DEFINE_PROCEDURE(glColor3i, gxg_glColor3i_w, 3, 0, 0, H_glColor3i);
  GL_DEFINE_PROCEDURE(glColor3s, gxg_glColor3s_w, 3, 0, 0, H_glColor3s);
  GL_DEFINE_PROCEDURE(glColor3ub, gxg_glColor3ub_w, 3, 0, 0, H_glColor3ub);
  GL_DEFINE_PROCEDURE(glColor3ui, gxg_glColor3ui_w, 3, 0, 0, H_glColor3ui);
  GL_DEFINE_PROCEDURE(glColor3us, gxg_glColor3us_w, 3, 0, 0, H_glColor3us);
  GL_DEFINE_PROCEDURE(glColor4b, gxg_glColor4b_w, 4, 0, 0, H_glColor4b);
  GL_DEFINE_PROCEDURE(glColor4d, gxg_glColor4d_w, 4, 0, 0, H_glColor4d);
  GL_DEFINE_PROCEDURE(glColor4f, gxg_glColor4f_w, 4, 0, 0, H_glColor4f);
  GL_DEFINE_PROCEDURE(glColor4i, gxg_glColor4i_w, 4, 0, 0, H_glColor4i);
  GL_DEFINE_PROCEDURE(glColor4s, gxg_glColor4s_w, 4, 0, 0, H_glColor4s);
  GL_DEFINE_PROCEDURE(glColor4ub, gxg_glColor4ub_w, 4, 0, 0, H_glColor4ub);
  GL_DEFINE_PROCEDURE(glColor4ui, gxg_glColor4ui_w, 4, 0, 0, H_glColor4ui);
  GL_DEFINE_PROCEDURE(glColor4us, gxg_glColor4us_w, 4, 0, 0, H_glColor4us);
  GL_DEFINE_PROCEDURE(glTexCoord1d, gxg_glTexCoord1d_w, 1, 0, 0, H_glTexCoord1d);
  GL_DEFINE_PROCEDURE(glTexCoord1f, gxg_glTexCoord1f_w, 1, 0, 0, H_glTexCoord1f);
  GL_DEFINE_PROCEDURE(glTexCoord1i, gxg_glTexCoord1i_w, 1, 0, 0, H_glTexCoord1i);
  GL_DEFINE_PROCEDURE(glTexCoord1s, gxg_glTexCoord1s_w, 1, 0, 0, H_glTexCoord1s);
  GL_DEFINE_PROCEDURE(glTexCoord2d, gxg_glTexCoord2d_w, 2, 0, 0, H_glTexCoord2d);
  GL_DEFINE_PROCEDURE(glTexCoord2f, gxg_glTexCoord2f_w, 2, 0, 0, H_glTexCoord2f);
  GL_DEFINE_PROCEDURE(glTexCoord2i, gxg_glTexCoord2i_w, 2, 0, 0, H_glTexCoord2i);
  GL_DEFINE_PROCEDURE(glTexCoord2s, gxg_glTexCoord2s_w, 2, 0, 0, H_glTexCoord2s);
  GL_DEFINE_PROCEDURE(glTexCoord3d, gxg_glTexCoord3d_w, 3, 0, 0, H_glTexCoord3d);
  GL_DEFINE_PROCEDURE(glTexCoord3f, gxg_glTexCoord3f_w, 3, 0, 0, H_glTexCoord3f);
  GL_DEFINE_PROCEDURE(glTexCoord3i, gxg_glTexCoord3i_w, 3, 0, 0, H_glTexCoord3i);
  GL_DEFINE_PROCEDURE(glTexCoord3s, gxg_glTexCoord3s_w, 3, 0, 0, H_glTexCoord3s);
  GL_DEFINE_PROCEDURE(glTexCoord4d, gxg_glTexCoord4d_w, 4, 0, 0, H_glTexCoord4d);
  GL_DEFINE_PROCEDURE(glTexCoord4f, gxg_glTexCoord4f_w, 4, 0, 0, H_glTexCoord4f);
  GL_DEFINE_PROCEDURE(glTexCoord4i, gxg_glTexCoord4i_w, 4, 0, 0, H_glTexCoord4i);
  GL_DEFINE_PROCEDURE(glTexCoord4s, gxg_glTexCoord4s_w, 4, 0, 0, H_glTexCoord4s);
  GL_DEFINE_PROCEDURE(glRasterPos2d, gxg_glRasterPos2d_w, 2, 0, 0, H_glRasterPos2d);
  GL_DEFINE_PROCEDURE(glRasterPos2f, gxg_glRasterPos2f_w, 2, 0, 0, H_glRasterPos2f);
  GL_DEFINE_PROCEDURE(glRasterPos2i, gxg_glRasterPos2i_w, 2, 0, 0, H_glRasterPos2i);
  GL_DEFINE_PROCEDURE(glRasterPos2s, gxg_glRasterPos2s_w, 2, 0, 0, H_glRasterPos2s);
  GL_DEFINE_PROCEDURE(glRasterPos3d, gxg_glRasterPos3d_w, 3, 0, 0, H_glRasterPos3d);
  GL_DEFINE_PROCEDURE(glRasterPos3f, gxg_glRasterPos3f_w, 3, 0, 0, H_glRasterPos3f);
  GL_DEFINE_PROCEDURE(glRasterPos3i, gxg_glRasterPos3i_w, 3, 0, 0, H_glRasterPos3i);
  GL_DEFINE_PROCEDURE(glRasterPos3s, gxg_glRasterPos3s_w, 3, 0, 0, H_glRasterPos3s);
  GL_DEFINE_PROCEDURE(glRasterPos4d, gxg_glRasterPos4d_w, 4, 0, 0, H_glRasterPos4d);
  GL_DEFINE_PROCEDURE(glRasterPos4f, gxg_glRasterPos4f_w, 4, 0, 0, H_glRasterPos4f);
  GL_DEFINE_PROCEDURE(glRasterPos4i, gxg_glRasterPos4i_w, 4, 0, 0, H_glRasterPos4i);
  GL_DEFINE_PROCEDURE(glRasterPos4s, gxg_glRasterPos4s_w, 4, 0, 0, H_glRasterPos4s);
  GL_DEFINE_PROCEDURE(glRectd, gxg_glRectd_w, 4, 0, 0, H_glRectd);
  GL_DEFINE_PROCEDURE(glRectf, gxg_glRectf_w, 4, 0, 0, H_glRectf);
  GL_DEFINE_PROCEDURE(glRecti, gxg_glRecti_w, 4, 0, 0, H_glRecti);
  GL_DEFINE_PROCEDURE(glRects, gxg_glRects_w, 4, 0, 0, H_glRects);
  GL_DEFINE_PROCEDURE(glVertexPointer, gxg_glVertexPointer_w, 4, 0, 0, H_glVertexPointer);
  GL_DEFINE_PROCEDURE(glNormalPointer, gxg_glNormalPointer_w, 3, 0, 0, H_glNormalPointer);
  GL_DEFINE_PROCEDURE(glColorPointer, gxg_glColorPointer_w, 4, 0, 0, H_glColorPointer);
  GL_DEFINE_PROCEDURE(glIndexPointer, gxg_glIndexPointer_w, 3, 0, 0, H_glIndexPointer);
  GL_DEFINE_PROCEDURE(glTexCoordPointer, gxg_glTexCoordPointer_w, 4, 0, 0, H_glTexCoordPointer);
  GL_DEFINE_PROCEDURE(glEdgeFlagPointer, gxg_glEdgeFlagPointer_w, 2, 0, 0, H_glEdgeFlagPointer);
  GL_DEFINE_PROCEDURE(glGetPointerv, gxg_glGetPointerv_w, 1, 1, 0, H_glGetPointerv);
  GL_DEFINE_PROCEDURE(glArrayElement, gxg_glArrayElement_w, 1, 0, 0, H_glArrayElement);
  GL_DEFINE_PROCEDURE(glDrawArrays, gxg_glDrawArrays_w, 3, 0, 0, H_glDrawArrays);
  GL_DEFINE_PROCEDURE(glDrawElements, gxg_glDrawElements_w, 4, 0, 0, H_glDrawElements);
  GL_DEFINE_PROCEDURE(glInterleavedArrays, gxg_glInterleavedArrays_w, 3, 0, 0, H_glInterleavedArrays);
  GL_DEFINE_PROCEDURE(glShadeModel, gxg_glShadeModel_w, 1, 0, 0, H_glShadeModel);
  GL_DEFINE_PROCEDURE(glLightf, gxg_glLightf_w, 3, 0, 0, H_glLightf);
  GL_DEFINE_PROCEDURE(glLighti, gxg_glLighti_w, 3, 0, 0, H_glLighti);
  GL_DEFINE_PROCEDURE(glGetLightfv, gxg_glGetLightfv_w, 2, 1, 0, H_glGetLightfv);
  GL_DEFINE_PROCEDURE(glGetLightiv, gxg_glGetLightiv_w, 2, 1, 0, H_glGetLightiv);
  GL_DEFINE_PROCEDURE(glLightModelf, gxg_glLightModelf_w, 2, 0, 0, H_glLightModelf);
  GL_DEFINE_PROCEDURE(glLightModeli, gxg_glLightModeli_w, 2, 0, 0, H_glLightModeli);
  GL_DEFINE_PROCEDURE(glMaterialf, gxg_glMaterialf_w, 3, 0, 0, H_glMaterialf);
  GL_DEFINE_PROCEDURE(glMateriali, gxg_glMateriali_w, 3, 0, 0, H_glMateriali);
  GL_DEFINE_PROCEDURE(glGetMaterialfv, gxg_glGetMaterialfv_w, 2, 1, 0, H_glGetMaterialfv);
  GL_DEFINE_PROCEDURE(glGetMaterialiv, gxg_glGetMaterialiv_w, 2, 1, 0, H_glGetMaterialiv);
  GL_DEFINE_PROCEDURE(glColorMaterial, gxg_glColorMaterial_w, 2, 0, 0, H_glColorMaterial);
  GL_DEFINE_PROCEDURE(glPixelZoom, gxg_glPixelZoom_w, 2, 0, 0, H_glPixelZoom);
  GL_DEFINE_PROCEDURE(glPixelStoref, gxg_glPixelStoref_w, 2, 0, 0, H_glPixelStoref);
  GL_DEFINE_PROCEDURE(glPixelStorei, gxg_glPixelStorei_w, 2, 0, 0, H_glPixelStorei);
  GL_DEFINE_PROCEDURE(glPixelTransferf, gxg_glPixelTransferf_w, 2, 0, 0, H_glPixelTransferf);
  GL_DEFINE_PROCEDURE(glPixelTransferi, gxg_glPixelTransferi_w, 2, 0, 0, H_glPixelTransferi);
  GL_DEFINE_PROCEDURE(glGetPixelMapfv, gxg_glGetPixelMapfv_w, 1, 1, 0, H_glGetPixelMapfv);
  GL_DEFINE_PROCEDURE(glGetPixelMapuiv, gxg_glGetPixelMapuiv_w, 1, 1, 0, H_glGetPixelMapuiv);
  GL_DEFINE_PROCEDURE(glGetPixelMapusv, gxg_glGetPixelMapusv_w, 1, 1, 0, H_glGetPixelMapusv);
  GL_DEFINE_PROCEDURE(glBitmap, gxg_glBitmap_w, 7, 0, 0, H_glBitmap);
  GL_DEFINE_PROCEDURE(glReadPixels, gxg_glReadPixels_w, 7, 0, 0, H_glReadPixels);
  GL_DEFINE_PROCEDURE(glDrawPixels, gxg_glDrawPixels_w, 5, 0, 0, H_glDrawPixels);
  GL_DEFINE_PROCEDURE(glCopyPixels, gxg_glCopyPixels_w, 5, 0, 0, H_glCopyPixels);
  GL_DEFINE_PROCEDURE(glStencilFunc, gxg_glStencilFunc_w, 3, 0, 0, H_glStencilFunc);
  GL_DEFINE_PROCEDURE(glStencilMask, gxg_glStencilMask_w, 1, 0, 0, H_glStencilMask);
  GL_DEFINE_PROCEDURE(glStencilOp, gxg_glStencilOp_w, 3, 0, 0, H_glStencilOp);
  GL_DEFINE_PROCEDURE(glClearStencil, gxg_glClearStencil_w, 1, 0, 0, H_glClearStencil);
  GL_DEFINE_PROCEDURE(glTexGend, gxg_glTexGend_w, 3, 0, 0, H_glTexGend);
  GL_DEFINE_PROCEDURE(glTexGenf, gxg_glTexGenf_w, 3, 0, 0, H_glTexGenf);
  GL_DEFINE_PROCEDURE(glTexGeni, gxg_glTexGeni_w, 3, 0, 0, H_glTexGeni);
  GL_DEFINE_PROCEDURE(glGetTexGendv, gxg_glGetTexGendv_w, 2, 1, 0, H_glGetTexGendv);
  GL_DEFINE_PROCEDURE(glGetTexGenfv, gxg_glGetTexGenfv_w, 2, 1, 0, H_glGetTexGenfv);
  GL_DEFINE_PROCEDURE(glGetTexGeniv, gxg_glGetTexGeniv_w, 2, 1, 0, H_glGetTexGeniv);
  GL_DEFINE_PROCEDURE(glTexEnvf, gxg_glTexEnvf_w, 3, 0, 0, H_glTexEnvf);
  GL_DEFINE_PROCEDURE(glTexEnvi, gxg_glTexEnvi_w, 3, 0, 0, H_glTexEnvi);
  GL_DEFINE_PROCEDURE(glGetTexEnvfv, gxg_glGetTexEnvfv_w, 2, 1, 0, H_glGetTexEnvfv);
  GL_DEFINE_PROCEDURE(glGetTexEnviv, gxg_glGetTexEnviv_w, 2, 1, 0, H_glGetTexEnviv);
  GL_DEFINE_PROCEDURE(glTexParameterf, gxg_glTexParameterf_w, 3, 0, 0, H_glTexParameterf);
  GL_DEFINE_PROCEDURE(glTexParameteri, gxg_glTexParameteri_w, 3, 0, 0, H_glTexParameteri);
  GL_DEFINE_PROCEDURE(glGetTexParameterfv, gxg_glGetTexParameterfv_w, 2, 1, 0, H_glGetTexParameterfv);
  GL_DEFINE_PROCEDURE(glGetTexParameteriv, gxg_glGetTexParameteriv_w, 2, 1, 0, H_glGetTexParameteriv);
  GL_DEFINE_PROCEDURE(glGetTexLevelParameterfv, gxg_glGetTexLevelParameterfv_w, 3, 1, 0, H_glGetTexLevelParameterfv);
  GL_DEFINE_PROCEDURE(glGetTexLevelParameteriv, gxg_glGetTexLevelParameteriv_w, 3, 1, 0, H_glGetTexLevelParameteriv);
  GL_DEFINE_PROCEDURE(glTexImage1D, gxg_glTexImage1D_w, 8, 0, 0, H_glTexImage1D);
  GL_DEFINE_PROCEDURE(glTexImage2D, gxg_glTexImage2D_w, 9, 0, 0, H_glTexImage2D);
  GL_DEFINE_PROCEDURE(glGenTextures, gxg_glGenTextures_w, 2, 0, 0, H_glGenTextures);
  GL_DEFINE_PROCEDURE(glDeleteTextures, gxg_glDeleteTextures_w, 2, 0, 0, H_glDeleteTextures);
  GL_DEFINE_PROCEDURE(glBindTexture, gxg_glBindTexture_w, 2, 0, 0, H_glBindTexture);
  GL_DEFINE_PROCEDURE(glAreTexturesResident, gxg_glAreTexturesResident_w, 3, 0, 0, H_glAreTexturesResident);
  GL_DEFINE_PROCEDURE(glIsTexture, gxg_glIsTexture_w, 1, 0, 0, H_glIsTexture);
  GL_DEFINE_PROCEDURE(glTexSubImage1D, gxg_glTexSubImage1D_w, 7, 0, 0, H_glTexSubImage1D);
  GL_DEFINE_PROCEDURE(glTexSubImage2D, gxg_glTexSubImage2D_w, 9, 0, 0, H_glTexSubImage2D);
  GL_DEFINE_PROCEDURE(glCopyTexImage1D, gxg_glCopyTexImage1D_w, 7, 0, 0, H_glCopyTexImage1D);
  GL_DEFINE_PROCEDURE(glCopyTexImage2D, gxg_glCopyTexImage2D_w, 8, 0, 0, H_glCopyTexImage2D);
  GL_DEFINE_PROCEDURE(glCopyTexSubImage1D, gxg_glCopyTexSubImage1D_w, 6, 0, 0, H_glCopyTexSubImage1D);
  GL_DEFINE_PROCEDURE(glCopyTexSubImage2D, gxg_glCopyTexSubImage2D_w, 8, 0, 0, H_glCopyTexSubImage2D);
  GL_DEFINE_PROCEDURE(glMap1d, gxg_glMap1d_w, 6, 0, 0, H_glMap1d);
  GL_DEFINE_PROCEDURE(glMap1f, gxg_glMap1f_w, 6, 0, 0, H_glMap1f);
  GL_DEFINE_PROCEDURE(glMap2d, gxg_glMap2d_w, 0, 0, 1, H_glMap2d);
  GL_DEFINE_PROCEDURE(glMap2f, gxg_glMap2f_w, 0, 0, 1, H_glMap2f);
  GL_DEFINE_PROCEDURE(glGetMapdv, gxg_glGetMapdv_w, 2, 1, 0, H_glGetMapdv);
  GL_DEFINE_PROCEDURE(glGetMapfv, gxg_glGetMapfv_w, 2, 1, 0, H_glGetMapfv);
  GL_DEFINE_PROCEDURE(glGetMapiv, gxg_glGetMapiv_w, 2, 1, 0, H_glGetMapiv);
  GL_DEFINE_PROCEDURE(glEvalCoord1d, gxg_glEvalCoord1d_w, 1, 0, 0, H_glEvalCoord1d);
  GL_DEFINE_PROCEDURE(glEvalCoord1f, gxg_glEvalCoord1f_w, 1, 0, 0, H_glEvalCoord1f);
  GL_DEFINE_PROCEDURE(glEvalCoord2d, gxg_glEvalCoord2d_w, 2, 0, 0, H_glEvalCoord2d);
  GL_DEFINE_PROCEDURE(glEvalCoord2f, gxg_glEvalCoord2f_w, 2, 0, 0, H_glEvalCoord2f);
  GL_DEFINE_PROCEDURE(glMapGrid1d, gxg_glMapGrid1d_w, 3, 0, 0, H_glMapGrid1d);
  GL_DEFINE_PROCEDURE(glMapGrid1f, gxg_glMapGrid1f_w, 3, 0, 0, H_glMapGrid1f);
  GL_DEFINE_PROCEDURE(glMapGrid2d, gxg_glMapGrid2d_w, 6, 0, 0, H_glMapGrid2d);
  GL_DEFINE_PROCEDURE(glMapGrid2f, gxg_glMapGrid2f_w, 6, 0, 0, H_glMapGrid2f);
  GL_DEFINE_PROCEDURE(glEvalPoint1, gxg_glEvalPoint1_w, 1, 0, 0, H_glEvalPoint1);
  GL_DEFINE_PROCEDURE(glEvalPoint2, gxg_glEvalPoint2_w, 2, 0, 0, H_glEvalPoint2);
  GL_DEFINE_PROCEDURE(glEvalMesh1, gxg_glEvalMesh1_w, 3, 0, 0, H_glEvalMesh1);
  GL_DEFINE_PROCEDURE(glEvalMesh2, gxg_glEvalMesh2_w, 5, 0, 0, H_glEvalMesh2);
  GL_DEFINE_PROCEDURE(glFogf, gxg_glFogf_w, 2, 0, 0, H_glFogf);
  GL_DEFINE_PROCEDURE(glFogi, gxg_glFogi_w, 2, 0, 0, H_glFogi);
  GL_DEFINE_PROCEDURE(glFeedbackBuffer, gxg_glFeedbackBuffer_w, 3, 0, 0, H_glFeedbackBuffer);
  GL_DEFINE_PROCEDURE(glPassThrough, gxg_glPassThrough_w, 1, 0, 0, H_glPassThrough);
  GL_DEFINE_PROCEDURE(glSelectBuffer, gxg_glSelectBuffer_w, 2, 0, 0, H_glSelectBuffer);
  GL_DEFINE_PROCEDURE(glInitNames, gxg_glInitNames_w, 0, 0, 0, H_glInitNames);
  GL_DEFINE_PROCEDURE(glLoadName, gxg_glLoadName_w, 1, 0, 0, H_glLoadName);
  GL_DEFINE_PROCEDURE(glPushName, gxg_glPushName_w, 1, 0, 0, H_glPushName);
  GL_DEFINE_PROCEDURE(glPopName, gxg_glPopName_w, 0, 0, 0, H_glPopName);
  GL_DEFINE_PROCEDURE(glDrawRangeElements, gxg_glDrawRangeElements_w, 6, 0, 0, H_glDrawRangeElements);
  GL_DEFINE_PROCEDURE(glTexImage3D, gxg_glTexImage3D_w, 0, 0, 1, H_glTexImage3D);
  GL_DEFINE_PROCEDURE(glTexSubImage3D, gxg_glTexSubImage3D_w, 0, 0, 1, H_glTexSubImage3D);
  GL_DEFINE_PROCEDURE(glCopyTexSubImage3D, gxg_glCopyTexSubImage3D_w, 9, 0, 0, H_glCopyTexSubImage3D);
  GL_DEFINE_PROCEDURE(glColorTable, gxg_glColorTable_w, 6, 0, 0, H_glColorTable);
  GL_DEFINE_PROCEDURE(glColorSubTable, gxg_glColorSubTable_w, 6, 0, 0, H_glColorSubTable);
  GL_DEFINE_PROCEDURE(glCopyColorSubTable, gxg_glCopyColorSubTable_w, 5, 0, 0, H_glCopyColorSubTable);
  GL_DEFINE_PROCEDURE(glCopyColorTable, gxg_glCopyColorTable_w, 5, 0, 0, H_glCopyColorTable);
  GL_DEFINE_PROCEDURE(glGetColorTableParameterfv, gxg_glGetColorTableParameterfv_w, 2, 1, 0, H_glGetColorTableParameterfv);
  GL_DEFINE_PROCEDURE(glGetColorTableParameteriv, gxg_glGetColorTableParameteriv_w, 2, 1, 0, H_glGetColorTableParameteriv);
  GL_DEFINE_PROCEDURE(glBlendEquation, gxg_glBlendEquation_w, 1, 0, 0, H_glBlendEquation);
  GL_DEFINE_PROCEDURE(glBlendColor, gxg_glBlendColor_w, 4, 0, 0, H_glBlendColor);
  GL_DEFINE_PROCEDURE(glHistogram, gxg_glHistogram_w, 4, 0, 0, H_glHistogram);
  GL_DEFINE_PROCEDURE(glResetHistogram, gxg_glResetHistogram_w, 1, 0, 0, H_glResetHistogram);
  GL_DEFINE_PROCEDURE(glGetHistogram, gxg_glGetHistogram_w, 5, 0, 0, H_glGetHistogram);
  GL_DEFINE_PROCEDURE(glGetHistogramParameterfv, gxg_glGetHistogramParameterfv_w, 2, 1, 0, H_glGetHistogramParameterfv);
  GL_DEFINE_PROCEDURE(glGetHistogramParameteriv, gxg_glGetHistogramParameteriv_w, 2, 1, 0, H_glGetHistogramParameteriv);
  GL_DEFINE_PROCEDURE(glMinmax, gxg_glMinmax_w, 3, 0, 0, H_glMinmax);
  GL_DEFINE_PROCEDURE(glResetMinmax, gxg_glResetMinmax_w, 1, 0, 0, H_glResetMinmax);
  GL_DEFINE_PROCEDURE(glGetMinmax, gxg_glGetMinmax_w, 5, 0, 0, H_glGetMinmax);
  GL_DEFINE_PROCEDURE(glGetMinmaxParameterfv, gxg_glGetMinmaxParameterfv_w, 2, 1, 0, H_glGetMinmaxParameterfv);
  GL_DEFINE_PROCEDURE(glGetMinmaxParameteriv, gxg_glGetMinmaxParameteriv_w, 2, 1, 0, H_glGetMinmaxParameteriv);
  GL_DEFINE_PROCEDURE(glConvolutionFilter1D, gxg_glConvolutionFilter1D_w, 6, 0, 0, H_glConvolutionFilter1D);
  GL_DEFINE_PROCEDURE(glConvolutionFilter2D, gxg_glConvolutionFilter2D_w, 7, 0, 0, H_glConvolutionFilter2D);
  GL_DEFINE_PROCEDURE(glConvolutionParameterf, gxg_glConvolutionParameterf_w, 3, 0, 0, H_glConvolutionParameterf);
  GL_DEFINE_PROCEDURE(glConvolutionParameteri, gxg_glConvolutionParameteri_w, 3, 0, 0, H_glConvolutionParameteri);
  GL_DEFINE_PROCEDURE(glCopyConvolutionFilter1D, gxg_glCopyConvolutionFilter1D_w, 5, 0, 0, H_glCopyConvolutionFilter1D);
  GL_DEFINE_PROCEDURE(glCopyConvolutionFilter2D, gxg_glCopyConvolutionFilter2D_w, 6, 0, 0, H_glCopyConvolutionFilter2D);
  GL_DEFINE_PROCEDURE(glSeparableFilter2D, gxg_glSeparableFilter2D_w, 8, 0, 0, H_glSeparableFilter2D);
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluBeginPolygon, gxg_gluBeginPolygon_w, 1, 0, 0, H_gluBeginPolygon);
#endif
  GL_DEFINE_PROCEDURE(gluBuild1DMipmaps, gxg_gluBuild1DMipmaps_w, 6, 0, 0, H_gluBuild1DMipmaps);
  GL_DEFINE_PROCEDURE(gluBuild2DMipmaps, gxg_gluBuild2DMipmaps_w, 7, 0, 0, H_gluBuild2DMipmaps);
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluDeleteTess, gxg_gluDeleteTess_w, 1, 0, 0, H_gluDeleteTess);
#endif
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluEndPolygon, gxg_gluEndPolygon_w, 1, 0, 0, H_gluEndPolygon);
#endif
  GL_DEFINE_PROCEDURE(gluErrorString, gxg_gluErrorString_w, 1, 0, 0, H_gluErrorString);
  GL_DEFINE_PROCEDURE(gluGetString, gxg_gluGetString_w, 1, 0, 0, H_gluGetString);
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluGetTessProperty, gxg_gluGetTessProperty_w, 3, 0, 0, H_gluGetTessProperty);
#endif
  GL_DEFINE_PROCEDURE(gluLookAt, gxg_gluLookAt_w, 9, 0, 0, H_gluLookAt);
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluNewTess, gxg_gluNewTess_w, 0, 0, 0, H_gluNewTess);
#endif
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluNextContour, gxg_gluNextContour_w, 2, 0, 0, H_gluNextContour);
#endif
  GL_DEFINE_PROCEDURE(gluOrtho2D, gxg_gluOrtho2D_w, 4, 0, 0, H_gluOrtho2D);
  GL_DEFINE_PROCEDURE(gluPerspective, gxg_gluPerspective_w, 4, 0, 0, H_gluPerspective);
  GL_DEFINE_PROCEDURE(gluPickMatrix, gxg_gluPickMatrix_w, 5, 0, 0, H_gluPickMatrix);
  GL_DEFINE_PROCEDURE(gluProject, gxg_gluProject_w, 9, 0, 0, H_gluProject);
  GL_DEFINE_PROCEDURE(gluScaleImage, gxg_gluScaleImage_w, 9, 0, 0, H_gluScaleImage);
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluTessBeginContour, gxg_gluTessBeginContour_w, 1, 0, 0, H_gluTessBeginContour);
#endif
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluTessBeginPolygon, gxg_gluTessBeginPolygon_w, 2, 0, 0, H_gluTessBeginPolygon);
#endif
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluTessEndContour, gxg_gluTessEndContour_w, 1, 0, 0, H_gluTessEndContour);
#endif
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluTessEndPolygon, gxg_gluTessEndPolygon_w, 1, 0, 0, H_gluTessEndPolygon);
#endif
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluTessNormal, gxg_gluTessNormal_w, 4, 0, 0, H_gluTessNormal);
#endif
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluTessProperty, gxg_gluTessProperty_w, 3, 0, 0, H_gluTessProperty);
#endif
#ifdef GLU_VERSION_1_2
  GL_DEFINE_PROCEDURE(gluTessVertex, gxg_gluTessVertex_w, 3, 0, 0, H_gluTessVertex);
#endif
  GL_DEFINE_PROCEDURE(gluUnProject, gxg_gluUnProject_w, 9, 0, 0, H_gluUnProject);
}

