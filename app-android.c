/* San Angeles Observation OpenGL ES version example
 * Copyright 2009 The Android Open Source Project
 * All rights reserved.
 *
 * This source is free software; you can redistribute it and/or
 * modify it under the terms of EITHER:
 *   (1) The GNU Lesser General Public License as published by the Free
 *       Software Foundation; either version 2.1 of the License, or (at
 *       your option) any later version. The text of the GNU Lesser
 *       General Public License is included with this source in the
 *       file LICENSE-LGPL.txt.
 *   (2) The BSD-style license that is included with this source in
 *       the file LICENSE-BSD.txt.
 *
 * This source is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the files
 * LICENSE-LGPL.txt and LICENSE-BSD.txt for more details.
 */
#include <jni.h>
#include <sys/time.h>
#include <time.h>
#include <android/log.h>
#include <stdint.h>
#include <stdio.h>

int   gAppAlive   = 1;

static int  sWindowWidth  = 320;
static int  sWindowHeight = 480;
static int  sDemoStopped  = 0;
static long sTimeOffset   = 0;
static int  sTimeOffsetInit = 0;
static long sTimeStopped  = 0;

static long
_getTime(void)
{
    struct timeval  now;

    gettimeofday(&now, NULL);
    return (long)(now.tv_sec*1000 + now.tv_usec/1000);
}

/* Call to initialize the graphics state */
void
Java_foam_nebogeo_nomadic_NomadicRenderer_nativeInit( JNIEnv*  env )
{
    importGLInit();
    appInit();
    gAppAlive    = 1;
    sDemoStopped = 0;
    sTimeOffsetInit = 0;
}

void
Java_foam_nebogeo_nomadic_NomadicRenderer_nativeResize( JNIEnv*  env, jobject  thiz, jint w, jint h )
{
    sWindowWidth  = w;
    sWindowHeight = h;
    __android_log_print(ANDROID_LOG_INFO, "SanAngeles", "resize w=%d h=%d", w, h);
}

/* Call to finalize the graphics state */
void
Java_foam_nebogeo_nomadic_NomadicRenderer_nativeDone( JNIEnv*  env )
{
    appDeinit();
    importGLDeinit();
}

/* This is called to indicate to the render loop that it should
 * stop as soon as possible.
 */
void
Java_foam_nebogeo_nomadic_FluxusGLSurfaceView_nativePause( JNIEnv*  env )
{
    sDemoStopped = !sDemoStopped;
    if (sDemoStopped) {
        /* we paused the animation, so store the current
         * time in sTimeStopped for future nativeRender calls */
        sTimeStopped = _getTime();
    } else {
        /* we resumed the animation, so adjust the time offset
         * to take care of the pause interval. */
        sTimeOffset -= _getTime() - sTimeStopped;
    }
}

/* Call to render the next GL frame */
void
Java_foam_nebogeo_nomadic_NomadicRenderer_nativeRender( JNIEnv*  env )
{
    long   curTime;

    /* NOTE: if sDemoStopped is TRUE, then we re-render the same frame
     *       on each iteration.
     */
    if (sDemoStopped) {
        curTime = sTimeStopped + sTimeOffset;
    } else {
        curTime = _getTime() + sTimeOffset;
        if (sTimeOffsetInit == 0) {
            sTimeOffsetInit = 1;
            sTimeOffset     = -curTime;
            curTime         = 0;
        } 
    }

    //__android_log_print(ANDROID_LOG_INFO, "SanAngeles", "curTime=%ld", curTime);

    appRender(curTime, sWindowWidth, sWindowHeight);
}

void
Java_foam_nebogeo_nomadic_NomadicRenderer_nativeEval( JNIEnv*  env, jobject  thiz, jstring code )
{
   const char *nativeCode = (*env)->GetStringUTFChars(env, code, 0);
   appEval(nativeCode);
   (*env)->ReleaseStringUTFChars(env, code, nativeCode);
}

void
Java_foam_nebogeo_nomadic_NomadicRenderer_nativeLoadTexture(JNIEnv* env, jobject thiz, jstring texname, jbyteArray arr, jint w, jint h)
{
    char *data = (char *) (*env)->GetByteArrayElements(env,arr,NULL);
    int len = (*env)->GetArrayLength(env, arr);
    const char *filename = (*env)->GetStringUTFChars(env, texname, 0);

    int id=appLoadTexture(filename,w,h,data);
    
    char txt[1024];
    sprintf(txt,"(display \"load tex loaded [%s] %d bytes\")(newline)",filename,len,id);
    appEval(txt);
  
    (*env)->ReleaseStringUTFChars(env, texname, filename);
    (*env)->ReleaseByteArrayElements(env,arr,data,JNI_ABORT);           
}
