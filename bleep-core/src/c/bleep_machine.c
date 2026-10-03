/*
 * bleep's machine probes: the OS calls behind bleep.machine.MachineProbe / ForkProbe on macOS and Windows.
 * Linux needs none of this; it reads /proc from the JVM.
 *
 * Loaded by bleep.machine.MachineNative, which declares the native methods below. The JNI surface is primitives and
 * long[] only: no FindClass, no exceptions thrown from C, no callbacks. A failing OS call is reported as a status the
 * Scala side turns into an exception naming the call, so this file never needs to know about Java classes.
 *
 * Built by bleep.scripts.BuildMachineNative (scripts-init), the sourcegen step of bleep-core.
 */
#include <jni.h>

/* Bump together with MachineNative.AbiVersion whenever a native method's signature or meaning changes. The loader
 * checks it, so a stale library in the cache directory is an error instead of a crash. */
#define BLEEP_MACHINE_ABI_VERSION 1

JNIEXPORT jint JNICALL Java_bleep_machine_MachineNative_abiVersion(JNIEnv *env, jobject self) {
    (void)env;
    (void)self;
    return BLEEP_MACHINE_ABI_VERSION;
}
