package build.bleep.intellij

import com.intellij.openapi.application.PathManager
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.progress.ProgressIndicator
import com.intellij.util.io.HttpRequests
import java.io.File
import java.io.FileOutputStream
import java.nio.file.Files
import java.nio.file.Path
import java.util.Locale
import java.util.zip.GZIPInputStream
import java.util.zip.ZipInputStream
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream

/**
 * Downloads and caches bleep binaries.
 * Ported from bleep's FetchBleepRelease.scala
 */
object BleepDownloader {
    private val LOG = Logger.getInstance(BleepDownloader::class.java)

    private val cacheDir: Path by lazy {
        Path.of(PathManager.getSystemPath(), "bleep-cache")
    }

    enum class Os { LINUX, MACOS, WINDOWS }
    enum class Arch { AMD64, ARM64 }

    data class OsArch(val os: Os, val arch: Arch)

    fun detectOsArch(): OsArch {
        val osName = System.getProperty("os.name", "").lowercase(Locale.ROOT)
        val archName = System.getProperty("os.arch", "").lowercase(Locale.ROOT)

        val os = when {
            osName.contains("windows") -> Os.WINDOWS
            osName.contains("linux") -> Os.LINUX
            osName.contains("mac") -> Os.MACOS
            else -> throw IllegalStateException("Unsupported OS: $osName")
        }

        val arch = when (archName) {
            "x86_64", "amd64" -> Arch.AMD64
            "aarch64" -> Arch.ARM64
            else -> throw IllegalStateException("Unsupported architecture: $archName")
        }

        return OsArch(os, arch)
    }

    fun getBleepDownloadUrl(version: String, osArch: OsArch): String {
        val base = "https://github.com/oyvindberg/bleep/releases/download/v$version"

        return when (osArch) {
            OsArch(Os.MACOS, Arch.ARM64) -> "$base/bleep-arm64-apple-darwin.tar.gz"
            OsArch(Os.MACOS, Arch.AMD64) -> "$base/bleep-x86_64-apple-darwin.tar.gz"
            OsArch(Os.LINUX, Arch.AMD64) -> "$base/bleep-x86_64-pc-linux.tar.gz"
            OsArch(Os.WINDOWS, Arch.AMD64) -> "$base/bleep-x86_64-pc-win32.zip"
            else -> throw IllegalStateException("No native image available for $osArch")
        }
    }

    fun getCachedBleep(version: String): File? {
        val osArch = detectOsArch()
        val executableName = if (osArch.os == Os.WINDOWS) "bleep.exe" else "bleep"
        val cachedFile = cacheDir.resolve(version).resolve(executableName).toFile()
        return if (cachedFile.exists() && cachedFile.canExecute()) cachedFile else null
    }

    fun downloadBleep(version: String, indicator: ProgressIndicator? = null): File {
        val osArch = detectOsArch()
        val executableName = if (osArch.os == Os.WINDOWS) "bleep.exe" else "bleep"
        val versionDir = cacheDir.resolve(version)
        val targetFile = versionDir.resolve(executableName).toFile()

        if (targetFile.exists() && targetFile.canExecute()) {
            LOG.info("Using cached bleep $version at $targetFile")
            return targetFile
        }

        Files.createDirectories(versionDir)

        val url = getBleepDownloadUrl(version, osArch)
        LOG.info("Downloading bleep $version from $url")

        indicator?.text = "Downloading bleep $version..."
        indicator?.isIndeterminate = false

        val tempFile = Files.createTempFile("bleep-download-", getExtension(url)).toFile()
        try {
            HttpRequests.request(url)
                .productNameAsUserAgent()
                .connect { request ->
                    val contentLength = request.connection.contentLengthLong
                    request.inputStream.use { input ->
                        FileOutputStream(tempFile).use { output ->
                            val buffer = ByteArray(8192)
                            var bytesRead: Long = 0
                            var n: Int
                            while (input.read(buffer).also { n = it } != -1) {
                                output.write(buffer, 0, n)
                                bytesRead += n
                                if (contentLength > 0) {
                                    indicator?.fraction = bytesRead.toDouble() / contentLength
                                }
                            }
                        }
                    }
                }

            indicator?.text = "Extracting bleep..."
            indicator?.isIndeterminate = true

            extractBleep(tempFile, targetFile, url)

            if (osArch.os != Os.WINDOWS) {
                targetFile.setExecutable(true)
            }

            LOG.info("Bleep $version installed to $targetFile")
            return targetFile
        } finally {
            tempFile.delete()
        }
    }

    private fun getExtension(url: String): String {
        return when {
            url.endsWith(".tar.gz") -> ".tar.gz"
            url.endsWith(".zip") -> ".zip"
            url.endsWith(".gz") -> ".gz"
            else -> ""
        }
    }

    private fun extractBleep(archive: File, target: File, url: String) {
        when {
            url.endsWith(".tar.gz") -> extractTarGz(archive, target)
            url.endsWith(".zip") -> extractZip(archive, target)
            url.endsWith(".gz") -> extractGz(archive, target)
            else -> throw IllegalArgumentException("Unknown archive format: $url")
        }
    }

    private fun extractTarGz(archive: File, target: File) {
        GzipCompressorInputStream(archive.inputStream().buffered()).use { gzipIn ->
            TarArchiveInputStream(gzipIn).use { tarIn ->
                var entry = tarIn.nextEntry
                while (entry != null) {
                    if (!entry.isDirectory && entry.name.contains("bleep")) {
                        FileOutputStream(target).use { out ->
                            tarIn.copyTo(out)
                        }
                        return
                    }
                    entry = tarIn.nextEntry
                }
            }
        }
        throw IllegalStateException("Could not find bleep executable in archive")
    }

    private fun extractZip(archive: File, target: File) {
        ZipInputStream(archive.inputStream().buffered()).use { zipIn ->
            var entry = zipIn.nextEntry
            while (entry != null) {
                if (!entry.isDirectory && entry.name.contains("bleep")) {
                    FileOutputStream(target).use { out ->
                        zipIn.copyTo(out)
                    }
                    return
                }
                entry = zipIn.nextEntry
            }
        }
        throw IllegalStateException("Could not find bleep executable in archive")
    }

    private fun extractGz(archive: File, target: File) {
        GZIPInputStream(archive.inputStream().buffered()).use { gzipIn ->
            FileOutputStream(target).use { out ->
                gzipIn.copyTo(out)
            }
        }
    }
}
