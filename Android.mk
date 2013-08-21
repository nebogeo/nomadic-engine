LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

LOCAL_MODULE := nomadic

LOCAL_CFLAGS := -DANDROID_NDK -DUSE_MATH -DDISABLE_IMPORTGL -O3 --fast-math -Wno-write-strings

LOCAL_SRC_FILES := \
	core/list.cpp \
	core/fixed.cpp \
	core/geometry.cpp \
	engine/primitive.cpp \
	engine/text_primitive.cpp \
	engine/scenenode.cpp \
	engine/scenegraph.cpp \
	engine/texture.cpp \
    engine/importgl.c \
    engine/nomadic.cpp \
	engine/engine.cpp \
	scheme/scheme.cpp \
	jellyfish/jellyfish.cpp \
	jellyfish/jellyfish_primitive.cpp \
    app-android.c

LOCAL_LDLIBS := -lGLESv1_CM -ldl -llog

include $(BUILD_SHARED_LIBRARY)
