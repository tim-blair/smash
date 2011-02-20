#include <jni.h>
#include <stdio.h>
#include <errno.h>
#include "Launcher.h"

JNIEXPORT void JNICALL Java_Launcher_runVim(JNIEnv *env, jobject obj) {
	if( fork() == 0 )
		execve("/usr/bin/vim", NULL, NULL);
	else
		wait(NULL);
}
