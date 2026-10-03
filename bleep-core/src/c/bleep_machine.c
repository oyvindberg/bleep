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

#ifdef __APPLE__
#include <errno.h>
#include <libproc.h>
#include <mach/mach.h>
#include <sys/resource.h>
#include <sys/sysctl.h>

/* The host port, for the life of the probe. mach_host_self() adds a send-right reference on every call, so it is asked
 * once rather than per sample. */
JNIEXPORT jlong JNICALL Java_bleep_machine_MachineNative_macHostPort(JNIEnv *env, jobject self) {
    (void)env;
    (void)self;
    return (jlong)mach_host_self();
}

/* Statuses of macSample. On failure out[0] holds the call's own error code (kern_return_t or errno). */
#define MAC_OK 0
#define MAC_HOST_STATISTICS64 1
#define MAC_PRESSURE_LEVEL 2
#define MAC_MEMSIZE 3

/* One reading of the machine, into out[0..6]:
 *   0 hw.memsize (bytes)            1 page size (bytes)
 *   2 internal_page_count            vm_stat's "Anonymous pages"
 *   3 wire_count                     vm_stat's "Pages wired down"
 *   4 compressor_page_count          vm_stat's "Pages occupied by compressor"
 *   5 kern.memorystatus_vm_pressure_level (1 normal, 2 warning, 4 critical)
 *   6 purgeable_count                vm_stat's "Pages purgeable" (part of internal; reported, not used for usedMb)
 */
JNIEXPORT jint JNICALL Java_bleep_machine_MachineNative_macSample(JNIEnv *env, jobject self, jlong hostPort, jlongArray out) {
    (void)self;
    jlong values[7];

    vm_statistics64_data_t vm;
    mach_msg_type_number_t count = HOST_VM_INFO64_COUNT;
    kern_return_t kr = host_statistics64((host_t)hostPort, HOST_VM_INFO64, (host_info64_t)&vm, &count);
    if (kr != KERN_SUCCESS) {
        values[0] = kr;
        (*env)->SetLongArrayRegion(env, out, 0, 1, values);
        return MAC_HOST_STATISTICS64;
    }

    int level = 0;
    size_t levelSize = sizeof(level);
    if (sysctlbyname("kern.memorystatus_vm_pressure_level", &level, &levelSize, NULL, 0) != 0) {
        values[0] = errno;
        (*env)->SetLongArrayRegion(env, out, 0, 1, values);
        return MAC_PRESSURE_LEVEL;
    }

    uint64_t memsize = 0;
    size_t memsizeSize = sizeof(memsize);
    if (sysctlbyname("hw.memsize", &memsize, &memsizeSize, NULL, 0) != 0) {
        values[0] = errno;
        (*env)->SetLongArrayRegion(env, out, 0, 1, values);
        return MAC_MEMSIZE;
    }

    values[0] = (jlong)memsize;
    /* The page size host_statistics64 counts in: 16 KB on Apple silicon. */
    values[1] = (jlong)vm_kernel_page_size;
    values[2] = (jlong)vm.internal_page_count;
    values[3] = (jlong)vm.wire_count;
    values[4] = (jlong)vm.compressor_page_count;
    values[5] = (jlong)level;
    values[6] = (jlong)vm.purgeable_count;
    (*env)->SetLongArrayRegion(env, out, 0, 7, values);
    return MAC_OK;
}

/* ri_phys_footprint of pid in bytes, or -errno: -ESRCH when there is no such process. */
JNIEXPORT jlong JNICALL Java_bleep_machine_MachineNative_macFootprint(JNIEnv *env, jobject self, jint pid) {
    (void)env;
    (void)self;
    struct rusage_info_v4 ri;
    if (proc_pid_rusage(pid, RUSAGE_INFO_V4, (rusage_info_t *)&ri) != 0) return -(jlong)errno;
    return (jlong)ri.ri_phys_footprint;
}
#endif

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <psapi.h>
#include <stdint.h>

/* The system's low-memory resource notification, for the life of the probe: a handle, or -GetLastError(). */
JNIEXPORT jlong JNICALL Java_bleep_machine_MachineNative_winLowMemoryNotification(JNIEnv *env, jobject self) {
    (void)env;
    (void)self;
    HANDLE h = CreateMemoryResourceNotification(LowMemoryResourceNotification);
    if (h == NULL) return -(jlong)GetLastError();
    return (jlong)(intptr_t)h;
}

/* Statuses of winSample. On failure out[0] holds GetLastError(). */
#define WIN_OK 0
#define WIN_GLOBAL_MEMORY_STATUS 1
#define WIN_QUERY_NOTIFICATION 2

/* One reading of the machine, into out[0..5]:
 *   0 ullTotalPhys    1 ullAvailPhys    2 dwMemoryLoad
 *   3 ullTotalPageFile (the commit limit)    4 ullAvailPageFile
 *   5 1 if the low-memory resource notification is signalled, else 0
 */
JNIEXPORT jint JNICALL Java_bleep_machine_MachineNative_winSample(JNIEnv *env, jobject self, jlong notification, jlongArray out) {
    (void)self;
    jlong values[6];

    MEMORYSTATUSEX status;
    status.dwLength = sizeof(status);
    if (!GlobalMemoryStatusEx(&status)) {
        values[0] = (jlong)GetLastError();
        (*env)->SetLongArrayRegion(env, out, 0, 1, values);
        return WIN_GLOBAL_MEMORY_STATUS;
    }

    BOOL low = FALSE;
    if (!QueryMemoryResourceNotification((HANDLE)(intptr_t)notification, &low)) {
        values[0] = (jlong)GetLastError();
        (*env)->SetLongArrayRegion(env, out, 0, 1, values);
        return WIN_QUERY_NOTIFICATION;
    }

    values[0] = (jlong)status.ullTotalPhys;
    values[1] = (jlong)status.ullAvailPhys;
    values[2] = (jlong)status.dwMemoryLoad;
    values[3] = (jlong)status.ullTotalPageFile;
    values[4] = (jlong)status.ullAvailPageFile;
    values[5] = low ? 1 : 0;
    (*env)->SetLongArrayRegion(env, out, 0, 6, values);
    return WIN_OK;
}

/* Returned by winFootprint when the process no longer exists. Every other negative value is -GetLastError(). */
#define WIN_GONE INT64_MIN

/* PrivateUsage of pid in bytes (PROCESS_MEMORY_COUNTERS_EX: private committed memory, resident or paged out). */
JNIEXPORT jlong JNICALL Java_bleep_machine_MachineNative_winFootprint(JNIEnv *env, jobject self, jint pid) {
    (void)env;
    (void)self;
    HANDLE process = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, FALSE, (DWORD)pid);
    if (process == NULL) {
        DWORD error = GetLastError();
        /* No process with that id. */
        if (error == ERROR_INVALID_PARAMETER) return WIN_GONE;
        return -(jlong)error;
    }

    /* A process object outlives the process while anyone holds a handle to it — the JVM that started it does, until the
     * Process is collected — so opening it succeeds after it has exited. Its exit code says whether it still runs. */
    DWORD exitCode = 0;
    if (!GetExitCodeProcess(process, &exitCode)) {
        DWORD error = GetLastError();
        CloseHandle(process);
        return -(jlong)error;
    }
    if (exitCode != STILL_ACTIVE) {
        CloseHandle(process);
        return WIN_GONE;
    }

    PROCESS_MEMORY_COUNTERS_EX counters;
    ZeroMemory(&counters, sizeof(counters));
    counters.cb = sizeof(counters);
    if (!GetProcessMemoryInfo(process, (PROCESS_MEMORY_COUNTERS *)&counters, sizeof(counters))) {
        DWORD error = GetLastError();
        CloseHandle(process);
        return -(jlong)error;
    }
    CloseHandle(process);
    return (jlong)counters.PrivateUsage;
}
#endif
