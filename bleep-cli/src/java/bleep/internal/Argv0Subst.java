package bleep.internal;

import com.oracle.svm.core.annotate.Substitute;
import com.oracle.svm.core.annotate.TargetClass;
import org.graalvm.nativeimage.Platform;
import org.graalvm.nativeimage.Platforms;

@TargetClass(className = "bleep.internal.Argv0")
@Platforms({Platform.LINUX.class, Platform.DARWIN.class})
final class Argv0Subst {

    @Substitute
    String get(String defaultValue) {
        return com.oracle.svm.core.JavaMainWrapper.getCRuntimeArgument0();
    }

}
