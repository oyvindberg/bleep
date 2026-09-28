
package scripts

import bleep.{BleepCodegenScript, Commands, Started}

import java.nio.file.Files

object GenerateForJavalinTest extends BleepCodegenScript("GenerateForJavalinTest") {
  override def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit = {
    started.logger.error("This script is a placeholder! You'll need to replace the contents with code which actually generates the files you want")

    targets.foreach { target =>
      if (Set(s"""|javalin-test""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|io/javalin/performance/jmh_generated/PathMatcherBenchmark_jmhType.java""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package io.javalin.performance.jmh_generated;
      |public class PathMatcherBenchmark_jmhType extends PathMatcherBenchmark_jmhType_B3 {
      |}
      |
      |""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|javalin-test""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|io/javalin/performance/jmh_generated/PathMatcherBenchmark_jmhType_B1.java""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package io.javalin.performance.jmh_generated;
      |import io.javalin.performance.PathMatcherBenchmark;
      |public class PathMatcherBenchmark_jmhType_B1 extends io.javalin.performance.PathMatcherBenchmark {
      |    byte b1_000, b1_001, b1_002, b1_003, b1_004, b1_005, b1_006, b1_007, b1_008, b1_009, b1_010, b1_011, b1_012, b1_013, b1_014, b1_015;
      |    byte b1_016, b1_017, b1_018, b1_019, b1_020, b1_021, b1_022, b1_023, b1_024, b1_025, b1_026, b1_027, b1_028, b1_029, b1_030, b1_031;
      |    byte b1_032, b1_033, b1_034, b1_035, b1_036, b1_037, b1_038, b1_039, b1_040, b1_041, b1_042, b1_043, b1_044, b1_045, b1_046, b1_047;
      |    byte b1_048, b1_049, b1_050, b1_051, b1_052, b1_053, b1_054, b1_055, b1_056, b1_057, b1_058, b1_059, b1_060, b1_061, b1_062, b1_063;
      |    byte b1_064, b1_065, b1_066, b1_067, b1_068, b1_069, b1_070, b1_071, b1_072, b1_073, b1_074, b1_075, b1_076, b1_077, b1_078, b1_079;
      |    byte b1_080, b1_081, b1_082, b1_083, b1_084, b1_085, b1_086, b1_087, b1_088, b1_089, b1_090, b1_091, b1_092, b1_093, b1_094, b1_095;
      |    byte b1_096, b1_097, b1_098, b1_099, b1_100, b1_101, b1_102, b1_103, b1_104, b1_105, b1_106, b1_107, b1_108, b1_109, b1_110, b1_111;
      |    byte b1_112, b1_113, b1_114, b1_115, b1_116, b1_117, b1_118, b1_119, b1_120, b1_121, b1_122, b1_123, b1_124, b1_125, b1_126, b1_127;
      |    byte b1_128, b1_129, b1_130, b1_131, b1_132, b1_133, b1_134, b1_135, b1_136, b1_137, b1_138, b1_139, b1_140, b1_141, b1_142, b1_143;
      |    byte b1_144, b1_145, b1_146, b1_147, b1_148, b1_149, b1_150, b1_151, b1_152, b1_153, b1_154, b1_155, b1_156, b1_157, b1_158, b1_159;
      |    byte b1_160, b1_161, b1_162, b1_163, b1_164, b1_165, b1_166, b1_167, b1_168, b1_169, b1_170, b1_171, b1_172, b1_173, b1_174, b1_175;
      |    byte b1_176, b1_177, b1_178, b1_179, b1_180, b1_181, b1_182, b1_183, b1_184, b1_185, b1_186, b1_187, b1_188, b1_189, b1_190, b1_191;
      |    byte b1_192, b1_193, b1_194, b1_195, b1_196, b1_197, b1_198, b1_199, b1_200, b1_201, b1_202, b1_203, b1_204, b1_205, b1_206, b1_207;
      |    byte b1_208, b1_209, b1_210, b1_211, b1_212, b1_213, b1_214, b1_215, b1_216, b1_217, b1_218, b1_219, b1_220, b1_221, b1_222, b1_223;
      |    byte b1_224, b1_225, b1_226, b1_227, b1_228, b1_229, b1_230, b1_231, b1_232, b1_233, b1_234, b1_235, b1_236, b1_237, b1_238, b1_239;
      |    byte b1_240, b1_241, b1_242, b1_243, b1_244, b1_245, b1_246, b1_247, b1_248, b1_249, b1_250, b1_251, b1_252, b1_253, b1_254, b1_255;
      |}
      |""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|javalin-test""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|io/javalin/performance/jmh_generated/PathMatcherBenchmark_jmhType_B2.java""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package io.javalin.performance.jmh_generated;
      |import java.util.concurrent.atomic.AtomicIntegerFieldUpdater;
      |public class PathMatcherBenchmark_jmhType_B2 extends PathMatcherBenchmark_jmhType_B1 {
      |    public volatile int setupTrialMutex;
      |    public volatile int tearTrialMutex;
      |    public final static AtomicIntegerFieldUpdater<PathMatcherBenchmark_jmhType_B2> setupTrialMutexUpdater = AtomicIntegerFieldUpdater.newUpdater(PathMatcherBenchmark_jmhType_B2.class, "setupTrialMutex");
      |    public final static AtomicIntegerFieldUpdater<PathMatcherBenchmark_jmhType_B2> tearTrialMutexUpdater = AtomicIntegerFieldUpdater.newUpdater(PathMatcherBenchmark_jmhType_B2.class, "tearTrialMutex");
      |
      |    public volatile int setupIterationMutex;
      |    public volatile int tearIterationMutex;
      |    public final static AtomicIntegerFieldUpdater<PathMatcherBenchmark_jmhType_B2> setupIterationMutexUpdater = AtomicIntegerFieldUpdater.newUpdater(PathMatcherBenchmark_jmhType_B2.class, "setupIterationMutex");
      |    public final static AtomicIntegerFieldUpdater<PathMatcherBenchmark_jmhType_B2> tearIterationMutexUpdater = AtomicIntegerFieldUpdater.newUpdater(PathMatcherBenchmark_jmhType_B2.class, "tearIterationMutex");
      |
      |    public volatile int setupInvocationMutex;
      |    public volatile int tearInvocationMutex;
      |    public final static AtomicIntegerFieldUpdater<PathMatcherBenchmark_jmhType_B2> setupInvocationMutexUpdater = AtomicIntegerFieldUpdater.newUpdater(PathMatcherBenchmark_jmhType_B2.class, "setupInvocationMutex");
      |    public final static AtomicIntegerFieldUpdater<PathMatcherBenchmark_jmhType_B2> tearInvocationMutexUpdater = AtomicIntegerFieldUpdater.newUpdater(PathMatcherBenchmark_jmhType_B2.class, "tearInvocationMutex");
      |
      |    public volatile boolean readyTrial;
      |    public volatile boolean readyIteration;
      |    public volatile boolean readyInvocation;
      |}
      |""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|javalin-test""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|io/javalin/performance/jmh_generated/PathMatcherBenchmark_jmhType_B3.java""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package io.javalin.performance.jmh_generated;
      |public class PathMatcherBenchmark_jmhType_B3 extends PathMatcherBenchmark_jmhType_B2 {
      |    byte b3_000, b3_001, b3_002, b3_003, b3_004, b3_005, b3_006, b3_007, b3_008, b3_009, b3_010, b3_011, b3_012, b3_013, b3_014, b3_015;
      |    byte b3_016, b3_017, b3_018, b3_019, b3_020, b3_021, b3_022, b3_023, b3_024, b3_025, b3_026, b3_027, b3_028, b3_029, b3_030, b3_031;
      |    byte b3_032, b3_033, b3_034, b3_035, b3_036, b3_037, b3_038, b3_039, b3_040, b3_041, b3_042, b3_043, b3_044, b3_045, b3_046, b3_047;
      |    byte b3_048, b3_049, b3_050, b3_051, b3_052, b3_053, b3_054, b3_055, b3_056, b3_057, b3_058, b3_059, b3_060, b3_061, b3_062, b3_063;
      |    byte b3_064, b3_065, b3_066, b3_067, b3_068, b3_069, b3_070, b3_071, b3_072, b3_073, b3_074, b3_075, b3_076, b3_077, b3_078, b3_079;
      |    byte b3_080, b3_081, b3_082, b3_083, b3_084, b3_085, b3_086, b3_087, b3_088, b3_089, b3_090, b3_091, b3_092, b3_093, b3_094, b3_095;
      |    byte b3_096, b3_097, b3_098, b3_099, b3_100, b3_101, b3_102, b3_103, b3_104, b3_105, b3_106, b3_107, b3_108, b3_109, b3_110, b3_111;
      |    byte b3_112, b3_113, b3_114, b3_115, b3_116, b3_117, b3_118, b3_119, b3_120, b3_121, b3_122, b3_123, b3_124, b3_125, b3_126, b3_127;
      |    byte b3_128, b3_129, b3_130, b3_131, b3_132, b3_133, b3_134, b3_135, b3_136, b3_137, b3_138, b3_139, b3_140, b3_141, b3_142, b3_143;
      |    byte b3_144, b3_145, b3_146, b3_147, b3_148, b3_149, b3_150, b3_151, b3_152, b3_153, b3_154, b3_155, b3_156, b3_157, b3_158, b3_159;
      |    byte b3_160, b3_161, b3_162, b3_163, b3_164, b3_165, b3_166, b3_167, b3_168, b3_169, b3_170, b3_171, b3_172, b3_173, b3_174, b3_175;
      |    byte b3_176, b3_177, b3_178, b3_179, b3_180, b3_181, b3_182, b3_183, b3_184, b3_185, b3_186, b3_187, b3_188, b3_189, b3_190, b3_191;
      |    byte b3_192, b3_193, b3_194, b3_195, b3_196, b3_197, b3_198, b3_199, b3_200, b3_201, b3_202, b3_203, b3_204, b3_205, b3_206, b3_207;
      |    byte b3_208, b3_209, b3_210, b3_211, b3_212, b3_213, b3_214, b3_215, b3_216, b3_217, b3_218, b3_219, b3_220, b3_221, b3_222, b3_223;
      |    byte b3_224, b3_225, b3_226, b3_227, b3_228, b3_229, b3_230, b3_231, b3_232, b3_233, b3_234, b3_235, b3_236, b3_237, b3_238, b3_239;
      |    byte b3_240, b3_241, b3_242, b3_243, b3_244, b3_245, b3_246, b3_247, b3_248, b3_249, b3_250, b3_251, b3_252, b3_253, b3_254, b3_255;
      |}
      |
      |""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|javalin-test""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|io/javalin/performance/jmh_generated/PathMatcherBenchmark_matchFirstList_jmhTest.java""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package io.javalin.performance.jmh_generated;
      |
      |import java.util.List;
      |import java.util.concurrent.atomic.AtomicInteger;
      |import java.util.Collection;
      |import java.util.ArrayList;
      |import java.util.concurrent.TimeUnit;
      |import org.openjdk.jmh.annotations.CompilerControl;
      |import org.openjdk.jmh.runner.InfraControl;
      |import org.openjdk.jmh.infra.ThreadParams;
      |import org.openjdk.jmh.results.BenchmarkTaskResult;
      |import org.openjdk.jmh.results.Result;
      |import org.openjdk.jmh.results.ThroughputResult;
      |import org.openjdk.jmh.results.AverageTimeResult;
      |import org.openjdk.jmh.results.SampleTimeResult;
      |import org.openjdk.jmh.results.SingleShotResult;
      |import org.openjdk.jmh.util.SampleBuffer;
      |import org.openjdk.jmh.annotations.Mode;
      |import org.openjdk.jmh.annotations.Fork;
      |import org.openjdk.jmh.annotations.Measurement;
      |import org.openjdk.jmh.annotations.Threads;
      |import org.openjdk.jmh.annotations.Warmup;
      |import org.openjdk.jmh.annotations.BenchmarkMode;
      |import org.openjdk.jmh.results.RawResults;
      |import org.openjdk.jmh.results.ResultRole;
      |import java.lang.reflect.Field;
      |import org.openjdk.jmh.infra.BenchmarkParams;
      |import org.openjdk.jmh.infra.IterationParams;
      |import org.openjdk.jmh.infra.Blackhole;
      |import org.openjdk.jmh.infra.Control;
      |import org.openjdk.jmh.results.ScalarResult;
      |import org.openjdk.jmh.results.AggregationPolicy;
      |import org.openjdk.jmh.runner.FailureAssistException;
      |
      |import io.javalin.performance.jmh_generated.PathMatcherBenchmark_jmhType;
      |public final class PathMatcherBenchmark_matchFirstList_jmhTest {
      |
      |    byte p000, p001, p002, p003, p004, p005, p006, p007, p008, p009, p010, p011, p012, p013, p014, p015;
      |    byte p016, p017, p018, p019, p020, p021, p022, p023, p024, p025, p026, p027, p028, p029, p030, p031;
      |    byte p032, p033, p034, p035, p036, p037, p038, p039, p040, p041, p042, p043, p044, p045, p046, p047;
      |    byte p048, p049, p050, p051, p052, p053, p054, p055, p056, p057, p058, p059, p060, p061, p062, p063;
      |    byte p064, p065, p066, p067, p068, p069, p070, p071, p072, p073, p074, p075, p076, p077, p078, p079;
      |    byte p080, p081, p082, p083, p084, p085, p086, p087, p088, p089, p090, p091, p092, p093, p094, p095;
      |    byte p096, p097, p098, p099, p100, p101, p102, p103, p104, p105, p106, p107, p108, p109, p110, p111;
      |    byte p112, p113, p114, p115, p116, p117, p118, p119, p120, p121, p122, p123, p124, p125, p126, p127;
      |    byte p128, p129, p130, p131, p132, p133, p134, p135, p136, p137, p138, p139, p140, p141, p142, p143;
      |    byte p144, p145, p146, p147, p148, p149, p150, p151, p152, p153, p154, p155, p156, p157, p158, p159;
      |    byte p160, p161, p162, p163, p164, p165, p166, p167, p168, p169, p170, p171, p172, p173, p174, p175;
      |    byte p176, p177, p178, p179, p180, p181, p182, p183, p184, p185, p186, p187, p188, p189, p190, p191;
      |    byte p192, p193, p194, p195, p196, p197, p198, p199, p200, p201, p202, p203, p204, p205, p206, p207;
      |    byte p208, p209, p210, p211, p212, p213, p214, p215, p216, p217, p218, p219, p220, p221, p222, p223;
      |    byte p224, p225, p226, p227, p228, p229, p230, p231, p232, p233, p234, p235, p236, p237, p238, p239;
      |    byte p240, p241, p242, p243, p244, p245, p246, p247, p248, p249, p250, p251, p252, p253, p254, p255;
      |    int startRndMask;
      |    BenchmarkParams benchmarkParams;
      |    IterationParams iterationParams;
      |    ThreadParams threadParams;
      |    Blackhole blackhole;
      |    Control notifyControl;
      |
      |    public BenchmarkTaskResult matchFirstList_Throughput(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G = _jmh_tryInit_f_pathmatcherbenchmark0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_pathmatcherbenchmark0_G.matchFirstList(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            matchFirstList_thrpt_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, l_pathmatcherbenchmark0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_pathmatcherbenchmark0_G.matchFirstList(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.compareAndSet(l_pathmatcherbenchmark0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_pathmatcherbenchmark0_G.readyTrial) {
      |                            l_pathmatcherbenchmark0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.set(l_pathmatcherbenchmark0_G, 0);
      |                    }
      |                } else {
      |                    long l_pathmatcherbenchmark0_G_backoff = 1;
      |                    while (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.get(l_pathmatcherbenchmark0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_pathmatcherbenchmark0_G_backoff);
      |                        l_pathmatcherbenchmark0_G_backoff = Math.max(1024, l_pathmatcherbenchmark0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_pathmatcherbenchmark0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps;
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            res.measuredOps /= batchSize;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new ThroughputResult(ResultRole.PRIMARY, "matchFirstList", res.measuredOps, res.getTime(), benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void matchFirstList_thrpt_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G) throws Throwable {
      |        long operations = 0;
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        do {
      |            l_pathmatcherbenchmark0_G.matchFirstList(blackhole);
      |            operations++;
      |        } while(!control.isDone);
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult matchFirstList_AverageTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G = _jmh_tryInit_f_pathmatcherbenchmark0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_pathmatcherbenchmark0_G.matchFirstList(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            matchFirstList_avgt_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, l_pathmatcherbenchmark0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_pathmatcherbenchmark0_G.matchFirstList(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.compareAndSet(l_pathmatcherbenchmark0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_pathmatcherbenchmark0_G.readyTrial) {
      |                            l_pathmatcherbenchmark0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.set(l_pathmatcherbenchmark0_G, 0);
      |                    }
      |                } else {
      |                    long l_pathmatcherbenchmark0_G_backoff = 1;
      |                    while (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.get(l_pathmatcherbenchmark0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_pathmatcherbenchmark0_G_backoff);
      |                        l_pathmatcherbenchmark0_G_backoff = Math.max(1024, l_pathmatcherbenchmark0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_pathmatcherbenchmark0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps;
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            res.measuredOps /= batchSize;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new AverageTimeResult(ResultRole.PRIMARY, "matchFirstList", res.measuredOps, res.getTime(), benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void matchFirstList_avgt_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G) throws Throwable {
      |        long operations = 0;
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        do {
      |            l_pathmatcherbenchmark0_G.matchFirstList(blackhole);
      |            operations++;
      |        } while(!control.isDone);
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult matchFirstList_SampleTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G = _jmh_tryInit_f_pathmatcherbenchmark0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_pathmatcherbenchmark0_G.matchFirstList(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            int targetSamples = (int) (control.getDuration(TimeUnit.MILLISECONDS) * 20); // at max, 20 timestamps per millisecond
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            SampleBuffer buffer = new SampleBuffer();
      |            matchFirstList_sample_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, buffer, targetSamples, opsPerInv, batchSize, l_pathmatcherbenchmark0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_pathmatcherbenchmark0_G.matchFirstList(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.compareAndSet(l_pathmatcherbenchmark0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_pathmatcherbenchmark0_G.readyTrial) {
      |                            l_pathmatcherbenchmark0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.set(l_pathmatcherbenchmark0_G, 0);
      |                    }
      |                } else {
      |                    long l_pathmatcherbenchmark0_G_backoff = 1;
      |                    while (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.get(l_pathmatcherbenchmark0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_pathmatcherbenchmark0_G_backoff);
      |                        l_pathmatcherbenchmark0_G_backoff = Math.max(1024, l_pathmatcherbenchmark0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_pathmatcherbenchmark0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps * batchSize;
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new SampleTimeResult(ResultRole.PRIMARY, "matchFirstList", buffer, benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void matchFirstList_sample_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, SampleBuffer buffer, int targetSamples, long opsPerInv, int batchSize, PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G) throws Throwable {
      |        long realTime = 0;
      |        long operations = 0;
      |        int rnd = (int)System.nanoTime();
      |        int rndMask = startRndMask;
      |        long time = 0;
      |        int currentStride = 0;
      |        do {
      |            rnd = (rnd * 1664525 + 1013904223);
      |            boolean sample = (rnd & rndMask) == 0;
      |            if (sample) {
      |                time = System.nanoTime();
      |            }
      |            for (int b = 0; b < batchSize; b++) {
      |                if (control.volatileSpoiler) return;
      |                l_pathmatcherbenchmark0_G.matchFirstList(blackhole);
      |            }
      |            if (sample) {
      |                buffer.add((System.nanoTime() - time) / opsPerInv);
      |                if (currentStride++ > targetSamples) {
      |                    buffer.half();
      |                    currentStride = 0;
      |                    rndMask = (rndMask << 1) + 1;
      |                }
      |            }
      |            operations++;
      |        } while(!control.isDone);
      |        startRndMask = Math.max(startRndMask, rndMask);
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult matchFirstList_SingleShotTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G = _jmh_tryInit_f_pathmatcherbenchmark0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            notifyControl.startMeasurement = true;
      |            RawResults res = new RawResults();
      |            int batchSize = iterationParams.getBatchSize();
      |            matchFirstList_ss_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, batchSize, l_pathmatcherbenchmark0_G);
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.compareAndSet(l_pathmatcherbenchmark0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_pathmatcherbenchmark0_G.readyTrial) {
      |                            l_pathmatcherbenchmark0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.set(l_pathmatcherbenchmark0_G, 0);
      |                    }
      |                } else {
      |                    long l_pathmatcherbenchmark0_G_backoff = 1;
      |                    while (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.get(l_pathmatcherbenchmark0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_pathmatcherbenchmark0_G_backoff);
      |                        l_pathmatcherbenchmark0_G_backoff = Math.max(1024, l_pathmatcherbenchmark0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_pathmatcherbenchmark0_G = null;
      |                }
      |            }
      |            int opsPerInv = control.benchmarkParams.getOpsPerInvocation();
      |            long totalOps = opsPerInv;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult(totalOps, totalOps);
      |            results.add(new SingleShotResult(ResultRole.PRIMARY, "matchFirstList", res.getTime(), totalOps, benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void matchFirstList_ss_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, int batchSize, PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G) throws Throwable {
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        for (int b = 0; b < batchSize; b++) {
      |            if (control.volatileSpoiler) return;
      |            l_pathmatcherbenchmark0_G.matchFirstList(blackhole);
      |        }
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |    }
      |
      |    
      |    static volatile PathMatcherBenchmark_jmhType f_pathmatcherbenchmark0_G;
      |    
      |    PathMatcherBenchmark_jmhType _jmh_tryInit_f_pathmatcherbenchmark0_G(InfraControl control) throws Throwable {
      |        PathMatcherBenchmark_jmhType val = f_pathmatcherbenchmark0_G;
      |        if (val != null) {
      |            return val;
      |        }
      |        synchronized(this.getClass()) {
      |            try {
      |            if (control.isFailing) throw new FailureAssistException();
      |            val = f_pathmatcherbenchmark0_G;
      |            if (val != null) {
      |                return val;
      |            }
      |            val = new PathMatcherBenchmark_jmhType();
      |            val.setup();
      |            val.readyTrial = true;
      |            f_pathmatcherbenchmark0_G = val;
      |            } catch (Throwable t) {
      |                control.isFailing = true;
      |                throw t;
      |            }
      |        }
      |        return val;
      |    }
      |
      |
      |}
      |
      |""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|javalin-test""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|io/javalin/performance/jmh_generated/PathMatcherBenchmark_matchFirstStream_jmhTest.java""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package io.javalin.performance.jmh_generated;
      |
      |import java.util.List;
      |import java.util.concurrent.atomic.AtomicInteger;
      |import java.util.Collection;
      |import java.util.ArrayList;
      |import java.util.concurrent.TimeUnit;
      |import org.openjdk.jmh.annotations.CompilerControl;
      |import org.openjdk.jmh.runner.InfraControl;
      |import org.openjdk.jmh.infra.ThreadParams;
      |import org.openjdk.jmh.results.BenchmarkTaskResult;
      |import org.openjdk.jmh.results.Result;
      |import org.openjdk.jmh.results.ThroughputResult;
      |import org.openjdk.jmh.results.AverageTimeResult;
      |import org.openjdk.jmh.results.SampleTimeResult;
      |import org.openjdk.jmh.results.SingleShotResult;
      |import org.openjdk.jmh.util.SampleBuffer;
      |import org.openjdk.jmh.annotations.Mode;
      |import org.openjdk.jmh.annotations.Fork;
      |import org.openjdk.jmh.annotations.Measurement;
      |import org.openjdk.jmh.annotations.Threads;
      |import org.openjdk.jmh.annotations.Warmup;
      |import org.openjdk.jmh.annotations.BenchmarkMode;
      |import org.openjdk.jmh.results.RawResults;
      |import org.openjdk.jmh.results.ResultRole;
      |import java.lang.reflect.Field;
      |import org.openjdk.jmh.infra.BenchmarkParams;
      |import org.openjdk.jmh.infra.IterationParams;
      |import org.openjdk.jmh.infra.Blackhole;
      |import org.openjdk.jmh.infra.Control;
      |import org.openjdk.jmh.results.ScalarResult;
      |import org.openjdk.jmh.results.AggregationPolicy;
      |import org.openjdk.jmh.runner.FailureAssistException;
      |
      |import io.javalin.performance.jmh_generated.PathMatcherBenchmark_jmhType;
      |public final class PathMatcherBenchmark_matchFirstStream_jmhTest {
      |
      |    byte p000, p001, p002, p003, p004, p005, p006, p007, p008, p009, p010, p011, p012, p013, p014, p015;
      |    byte p016, p017, p018, p019, p020, p021, p022, p023, p024, p025, p026, p027, p028, p029, p030, p031;
      |    byte p032, p033, p034, p035, p036, p037, p038, p039, p040, p041, p042, p043, p044, p045, p046, p047;
      |    byte p048, p049, p050, p051, p052, p053, p054, p055, p056, p057, p058, p059, p060, p061, p062, p063;
      |    byte p064, p065, p066, p067, p068, p069, p070, p071, p072, p073, p074, p075, p076, p077, p078, p079;
      |    byte p080, p081, p082, p083, p084, p085, p086, p087, p088, p089, p090, p091, p092, p093, p094, p095;
      |    byte p096, p097, p098, p099, p100, p101, p102, p103, p104, p105, p106, p107, p108, p109, p110, p111;
      |    byte p112, p113, p114, p115, p116, p117, p118, p119, p120, p121, p122, p123, p124, p125, p126, p127;
      |    byte p128, p129, p130, p131, p132, p133, p134, p135, p136, p137, p138, p139, p140, p141, p142, p143;
      |    byte p144, p145, p146, p147, p148, p149, p150, p151, p152, p153, p154, p155, p156, p157, p158, p159;
      |    byte p160, p161, p162, p163, p164, p165, p166, p167, p168, p169, p170, p171, p172, p173, p174, p175;
      |    byte p176, p177, p178, p179, p180, p181, p182, p183, p184, p185, p186, p187, p188, p189, p190, p191;
      |    byte p192, p193, p194, p195, p196, p197, p198, p199, p200, p201, p202, p203, p204, p205, p206, p207;
      |    byte p208, p209, p210, p211, p212, p213, p214, p215, p216, p217, p218, p219, p220, p221, p222, p223;
      |    byte p224, p225, p226, p227, p228, p229, p230, p231, p232, p233, p234, p235, p236, p237, p238, p239;
      |    byte p240, p241, p242, p243, p244, p245, p246, p247, p248, p249, p250, p251, p252, p253, p254, p255;
      |    int startRndMask;
      |    BenchmarkParams benchmarkParams;
      |    IterationParams iterationParams;
      |    ThreadParams threadParams;
      |    Blackhole blackhole;
      |    Control notifyControl;
      |
      |    public BenchmarkTaskResult matchFirstStream_Throughput(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G = _jmh_tryInit_f_pathmatcherbenchmark0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_pathmatcherbenchmark0_G.matchFirstStream(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            matchFirstStream_thrpt_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, l_pathmatcherbenchmark0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_pathmatcherbenchmark0_G.matchFirstStream(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.compareAndSet(l_pathmatcherbenchmark0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_pathmatcherbenchmark0_G.readyTrial) {
      |                            l_pathmatcherbenchmark0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.set(l_pathmatcherbenchmark0_G, 0);
      |                    }
      |                } else {
      |                    long l_pathmatcherbenchmark0_G_backoff = 1;
      |                    while (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.get(l_pathmatcherbenchmark0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_pathmatcherbenchmark0_G_backoff);
      |                        l_pathmatcherbenchmark0_G_backoff = Math.max(1024, l_pathmatcherbenchmark0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_pathmatcherbenchmark0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps;
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            res.measuredOps /= batchSize;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new ThroughputResult(ResultRole.PRIMARY, "matchFirstStream", res.measuredOps, res.getTime(), benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void matchFirstStream_thrpt_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G) throws Throwable {
      |        long operations = 0;
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        do {
      |            l_pathmatcherbenchmark0_G.matchFirstStream(blackhole);
      |            operations++;
      |        } while(!control.isDone);
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult matchFirstStream_AverageTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G = _jmh_tryInit_f_pathmatcherbenchmark0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_pathmatcherbenchmark0_G.matchFirstStream(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            matchFirstStream_avgt_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, l_pathmatcherbenchmark0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_pathmatcherbenchmark0_G.matchFirstStream(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.compareAndSet(l_pathmatcherbenchmark0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_pathmatcherbenchmark0_G.readyTrial) {
      |                            l_pathmatcherbenchmark0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.set(l_pathmatcherbenchmark0_G, 0);
      |                    }
      |                } else {
      |                    long l_pathmatcherbenchmark0_G_backoff = 1;
      |                    while (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.get(l_pathmatcherbenchmark0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_pathmatcherbenchmark0_G_backoff);
      |                        l_pathmatcherbenchmark0_G_backoff = Math.max(1024, l_pathmatcherbenchmark0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_pathmatcherbenchmark0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps;
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            res.measuredOps /= batchSize;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new AverageTimeResult(ResultRole.PRIMARY, "matchFirstStream", res.measuredOps, res.getTime(), benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void matchFirstStream_avgt_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G) throws Throwable {
      |        long operations = 0;
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        do {
      |            l_pathmatcherbenchmark0_G.matchFirstStream(blackhole);
      |            operations++;
      |        } while(!control.isDone);
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult matchFirstStream_SampleTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G = _jmh_tryInit_f_pathmatcherbenchmark0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_pathmatcherbenchmark0_G.matchFirstStream(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            int targetSamples = (int) (control.getDuration(TimeUnit.MILLISECONDS) * 20); // at max, 20 timestamps per millisecond
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            SampleBuffer buffer = new SampleBuffer();
      |            matchFirstStream_sample_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, buffer, targetSamples, opsPerInv, batchSize, l_pathmatcherbenchmark0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_pathmatcherbenchmark0_G.matchFirstStream(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.compareAndSet(l_pathmatcherbenchmark0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_pathmatcherbenchmark0_G.readyTrial) {
      |                            l_pathmatcherbenchmark0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.set(l_pathmatcherbenchmark0_G, 0);
      |                    }
      |                } else {
      |                    long l_pathmatcherbenchmark0_G_backoff = 1;
      |                    while (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.get(l_pathmatcherbenchmark0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_pathmatcherbenchmark0_G_backoff);
      |                        l_pathmatcherbenchmark0_G_backoff = Math.max(1024, l_pathmatcherbenchmark0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_pathmatcherbenchmark0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps * batchSize;
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new SampleTimeResult(ResultRole.PRIMARY, "matchFirstStream", buffer, benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void matchFirstStream_sample_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, SampleBuffer buffer, int targetSamples, long opsPerInv, int batchSize, PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G) throws Throwable {
      |        long realTime = 0;
      |        long operations = 0;
      |        int rnd = (int)System.nanoTime();
      |        int rndMask = startRndMask;
      |        long time = 0;
      |        int currentStride = 0;
      |        do {
      |            rnd = (rnd * 1664525 + 1013904223);
      |            boolean sample = (rnd & rndMask) == 0;
      |            if (sample) {
      |                time = System.nanoTime();
      |            }
      |            for (int b = 0; b < batchSize; b++) {
      |                if (control.volatileSpoiler) return;
      |                l_pathmatcherbenchmark0_G.matchFirstStream(blackhole);
      |            }
      |            if (sample) {
      |                buffer.add((System.nanoTime() - time) / opsPerInv);
      |                if (currentStride++ > targetSamples) {
      |                    buffer.half();
      |                    currentStride = 0;
      |                    rndMask = (rndMask << 1) + 1;
      |                }
      |            }
      |            operations++;
      |        } while(!control.isDone);
      |        startRndMask = Math.max(startRndMask, rndMask);
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult matchFirstStream_SingleShotTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G = _jmh_tryInit_f_pathmatcherbenchmark0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            notifyControl.startMeasurement = true;
      |            RawResults res = new RawResults();
      |            int batchSize = iterationParams.getBatchSize();
      |            matchFirstStream_ss_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, batchSize, l_pathmatcherbenchmark0_G);
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.compareAndSet(l_pathmatcherbenchmark0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_pathmatcherbenchmark0_G.readyTrial) {
      |                            l_pathmatcherbenchmark0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.set(l_pathmatcherbenchmark0_G, 0);
      |                    }
      |                } else {
      |                    long l_pathmatcherbenchmark0_G_backoff = 1;
      |                    while (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.get(l_pathmatcherbenchmark0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_pathmatcherbenchmark0_G_backoff);
      |                        l_pathmatcherbenchmark0_G_backoff = Math.max(1024, l_pathmatcherbenchmark0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_pathmatcherbenchmark0_G = null;
      |                }
      |            }
      |            int opsPerInv = control.benchmarkParams.getOpsPerInvocation();
      |            long totalOps = opsPerInv;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult(totalOps, totalOps);
      |            results.add(new SingleShotResult(ResultRole.PRIMARY, "matchFirstStream", res.getTime(), totalOps, benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void matchFirstStream_ss_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, int batchSize, PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G) throws Throwable {
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        for (int b = 0; b < batchSize; b++) {
      |            if (control.volatileSpoiler) return;
      |            l_pathmatcherbenchmark0_G.matchFirstStream(blackhole);
      |        }
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |    }
      |
      |    
      |    static volatile PathMatcherBenchmark_jmhType f_pathmatcherbenchmark0_G;
      |    
      |    PathMatcherBenchmark_jmhType _jmh_tryInit_f_pathmatcherbenchmark0_G(InfraControl control) throws Throwable {
      |        PathMatcherBenchmark_jmhType val = f_pathmatcherbenchmark0_G;
      |        if (val != null) {
      |            return val;
      |        }
      |        synchronized(this.getClass()) {
      |            try {
      |            if (control.isFailing) throw new FailureAssistException();
      |            val = f_pathmatcherbenchmark0_G;
      |            if (val != null) {
      |                return val;
      |            }
      |            val = new PathMatcherBenchmark_jmhType();
      |            val.setup();
      |            val.readyTrial = true;
      |            f_pathmatcherbenchmark0_G = val;
      |            } catch (Throwable t) {
      |                control.isFailing = true;
      |                throw t;
      |            }
      |        }
      |        return val;
      |    }
      |
      |
      |}
      |
      |""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|javalin-test""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|io/javalin/performance/jmh_generated/PathMatcherBenchmark_matchLastList_jmhTest.java""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package io.javalin.performance.jmh_generated;
      |
      |import java.util.List;
      |import java.util.concurrent.atomic.AtomicInteger;
      |import java.util.Collection;
      |import java.util.ArrayList;
      |import java.util.concurrent.TimeUnit;
      |import org.openjdk.jmh.annotations.CompilerControl;
      |import org.openjdk.jmh.runner.InfraControl;
      |import org.openjdk.jmh.infra.ThreadParams;
      |import org.openjdk.jmh.results.BenchmarkTaskResult;
      |import org.openjdk.jmh.results.Result;
      |import org.openjdk.jmh.results.ThroughputResult;
      |import org.openjdk.jmh.results.AverageTimeResult;
      |import org.openjdk.jmh.results.SampleTimeResult;
      |import org.openjdk.jmh.results.SingleShotResult;
      |import org.openjdk.jmh.util.SampleBuffer;
      |import org.openjdk.jmh.annotations.Mode;
      |import org.openjdk.jmh.annotations.Fork;
      |import org.openjdk.jmh.annotations.Measurement;
      |import org.openjdk.jmh.annotations.Threads;
      |import org.openjdk.jmh.annotations.Warmup;
      |import org.openjdk.jmh.annotations.BenchmarkMode;
      |import org.openjdk.jmh.results.RawResults;
      |import org.openjdk.jmh.results.ResultRole;
      |import java.lang.reflect.Field;
      |import org.openjdk.jmh.infra.BenchmarkParams;
      |import org.openjdk.jmh.infra.IterationParams;
      |import org.openjdk.jmh.infra.Blackhole;
      |import org.openjdk.jmh.infra.Control;
      |import org.openjdk.jmh.results.ScalarResult;
      |import org.openjdk.jmh.results.AggregationPolicy;
      |import org.openjdk.jmh.runner.FailureAssistException;
      |
      |import io.javalin.performance.jmh_generated.PathMatcherBenchmark_jmhType;
      |public final class PathMatcherBenchmark_matchLastList_jmhTest {
      |
      |    byte p000, p001, p002, p003, p004, p005, p006, p007, p008, p009, p010, p011, p012, p013, p014, p015;
      |    byte p016, p017, p018, p019, p020, p021, p022, p023, p024, p025, p026, p027, p028, p029, p030, p031;
      |    byte p032, p033, p034, p035, p036, p037, p038, p039, p040, p041, p042, p043, p044, p045, p046, p047;
      |    byte p048, p049, p050, p051, p052, p053, p054, p055, p056, p057, p058, p059, p060, p061, p062, p063;
      |    byte p064, p065, p066, p067, p068, p069, p070, p071, p072, p073, p074, p075, p076, p077, p078, p079;
      |    byte p080, p081, p082, p083, p084, p085, p086, p087, p088, p089, p090, p091, p092, p093, p094, p095;
      |    byte p096, p097, p098, p099, p100, p101, p102, p103, p104, p105, p106, p107, p108, p109, p110, p111;
      |    byte p112, p113, p114, p115, p116, p117, p118, p119, p120, p121, p122, p123, p124, p125, p126, p127;
      |    byte p128, p129, p130, p131, p132, p133, p134, p135, p136, p137, p138, p139, p140, p141, p142, p143;
      |    byte p144, p145, p146, p147, p148, p149, p150, p151, p152, p153, p154, p155, p156, p157, p158, p159;
      |    byte p160, p161, p162, p163, p164, p165, p166, p167, p168, p169, p170, p171, p172, p173, p174, p175;
      |    byte p176, p177, p178, p179, p180, p181, p182, p183, p184, p185, p186, p187, p188, p189, p190, p191;
      |    byte p192, p193, p194, p195, p196, p197, p198, p199, p200, p201, p202, p203, p204, p205, p206, p207;
      |    byte p208, p209, p210, p211, p212, p213, p214, p215, p216, p217, p218, p219, p220, p221, p222, p223;
      |    byte p224, p225, p226, p227, p228, p229, p230, p231, p232, p233, p234, p235, p236, p237, p238, p239;
      |    byte p240, p241, p242, p243, p244, p245, p246, p247, p248, p249, p250, p251, p252, p253, p254, p255;
      |    int startRndMask;
      |    BenchmarkParams benchmarkParams;
      |    IterationParams iterationParams;
      |    ThreadParams threadParams;
      |    Blackhole blackhole;
      |    Control notifyControl;
      |
      |    public BenchmarkTaskResult matchLastList_Throughput(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G = _jmh_tryInit_f_pathmatcherbenchmark0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_pathmatcherbenchmark0_G.matchLastList(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            matchLastList_thrpt_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, l_pathmatcherbenchmark0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_pathmatcherbenchmark0_G.matchLastList(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.compareAndSet(l_pathmatcherbenchmark0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_pathmatcherbenchmark0_G.readyTrial) {
      |                            l_pathmatcherbenchmark0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.set(l_pathmatcherbenchmark0_G, 0);
      |                    }
      |                } else {
      |                    long l_pathmatcherbenchmark0_G_backoff = 1;
      |                    while (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.get(l_pathmatcherbenchmark0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_pathmatcherbenchmark0_G_backoff);
      |                        l_pathmatcherbenchmark0_G_backoff = Math.max(1024, l_pathmatcherbenchmark0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_pathmatcherbenchmark0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps;
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            res.measuredOps /= batchSize;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new ThroughputResult(ResultRole.PRIMARY, "matchLastList", res.measuredOps, res.getTime(), benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void matchLastList_thrpt_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G) throws Throwable {
      |        long operations = 0;
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        do {
      |            l_pathmatcherbenchmark0_G.matchLastList(blackhole);
      |            operations++;
      |        } while(!control.isDone);
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult matchLastList_AverageTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G = _jmh_tryInit_f_pathmatcherbenchmark0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_pathmatcherbenchmark0_G.matchLastList(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            matchLastList_avgt_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, l_pathmatcherbenchmark0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_pathmatcherbenchmark0_G.matchLastList(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.compareAndSet(l_pathmatcherbenchmark0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_pathmatcherbenchmark0_G.readyTrial) {
      |                            l_pathmatcherbenchmark0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.set(l_pathmatcherbenchmark0_G, 0);
      |                    }
      |                } else {
      |                    long l_pathmatcherbenchmark0_G_backoff = 1;
      |                    while (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.get(l_pathmatcherbenchmark0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_pathmatcherbenchmark0_G_backoff);
      |                        l_pathmatcherbenchmark0_G_backoff = Math.max(1024, l_pathmatcherbenchmark0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_pathmatcherbenchmark0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps;
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            res.measuredOps /= batchSize;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new AverageTimeResult(ResultRole.PRIMARY, "matchLastList", res.measuredOps, res.getTime(), benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void matchLastList_avgt_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G) throws Throwable {
      |        long operations = 0;
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        do {
      |            l_pathmatcherbenchmark0_G.matchLastList(blackhole);
      |            operations++;
      |        } while(!control.isDone);
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult matchLastList_SampleTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G = _jmh_tryInit_f_pathmatcherbenchmark0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_pathmatcherbenchmark0_G.matchLastList(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            int targetSamples = (int) (control.getDuration(TimeUnit.MILLISECONDS) * 20); // at max, 20 timestamps per millisecond
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            SampleBuffer buffer = new SampleBuffer();
      |            matchLastList_sample_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, buffer, targetSamples, opsPerInv, batchSize, l_pathmatcherbenchmark0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_pathmatcherbenchmark0_G.matchLastList(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.compareAndSet(l_pathmatcherbenchmark0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_pathmatcherbenchmark0_G.readyTrial) {
      |                            l_pathmatcherbenchmark0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.set(l_pathmatcherbenchmark0_G, 0);
      |                    }
      |                } else {
      |                    long l_pathmatcherbenchmark0_G_backoff = 1;
      |                    while (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.get(l_pathmatcherbenchmark0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_pathmatcherbenchmark0_G_backoff);
      |                        l_pathmatcherbenchmark0_G_backoff = Math.max(1024, l_pathmatcherbenchmark0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_pathmatcherbenchmark0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps * batchSize;
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new SampleTimeResult(ResultRole.PRIMARY, "matchLastList", buffer, benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void matchLastList_sample_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, SampleBuffer buffer, int targetSamples, long opsPerInv, int batchSize, PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G) throws Throwable {
      |        long realTime = 0;
      |        long operations = 0;
      |        int rnd = (int)System.nanoTime();
      |        int rndMask = startRndMask;
      |        long time = 0;
      |        int currentStride = 0;
      |        do {
      |            rnd = (rnd * 1664525 + 1013904223);
      |            boolean sample = (rnd & rndMask) == 0;
      |            if (sample) {
      |                time = System.nanoTime();
      |            }
      |            for (int b = 0; b < batchSize; b++) {
      |                if (control.volatileSpoiler) return;
      |                l_pathmatcherbenchmark0_G.matchLastList(blackhole);
      |            }
      |            if (sample) {
      |                buffer.add((System.nanoTime() - time) / opsPerInv);
      |                if (currentStride++ > targetSamples) {
      |                    buffer.half();
      |                    currentStride = 0;
      |                    rndMask = (rndMask << 1) + 1;
      |                }
      |            }
      |            operations++;
      |        } while(!control.isDone);
      |        startRndMask = Math.max(startRndMask, rndMask);
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult matchLastList_SingleShotTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G = _jmh_tryInit_f_pathmatcherbenchmark0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            notifyControl.startMeasurement = true;
      |            RawResults res = new RawResults();
      |            int batchSize = iterationParams.getBatchSize();
      |            matchLastList_ss_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, batchSize, l_pathmatcherbenchmark0_G);
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.compareAndSet(l_pathmatcherbenchmark0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_pathmatcherbenchmark0_G.readyTrial) {
      |                            l_pathmatcherbenchmark0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.set(l_pathmatcherbenchmark0_G, 0);
      |                    }
      |                } else {
      |                    long l_pathmatcherbenchmark0_G_backoff = 1;
      |                    while (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.get(l_pathmatcherbenchmark0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_pathmatcherbenchmark0_G_backoff);
      |                        l_pathmatcherbenchmark0_G_backoff = Math.max(1024, l_pathmatcherbenchmark0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_pathmatcherbenchmark0_G = null;
      |                }
      |            }
      |            int opsPerInv = control.benchmarkParams.getOpsPerInvocation();
      |            long totalOps = opsPerInv;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult(totalOps, totalOps);
      |            results.add(new SingleShotResult(ResultRole.PRIMARY, "matchLastList", res.getTime(), totalOps, benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void matchLastList_ss_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, int batchSize, PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G) throws Throwable {
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        for (int b = 0; b < batchSize; b++) {
      |            if (control.volatileSpoiler) return;
      |            l_pathmatcherbenchmark0_G.matchLastList(blackhole);
      |        }
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |    }
      |
      |    
      |    static volatile PathMatcherBenchmark_jmhType f_pathmatcherbenchmark0_G;
      |    
      |    PathMatcherBenchmark_jmhType _jmh_tryInit_f_pathmatcherbenchmark0_G(InfraControl control) throws Throwable {
      |        PathMatcherBenchmark_jmhType val = f_pathmatcherbenchmark0_G;
      |        if (val != null) {
      |            return val;
      |        }
      |        synchronized(this.getClass()) {
      |            try {
      |            if (control.isFailing) throw new FailureAssistException();
      |            val = f_pathmatcherbenchmark0_G;
      |            if (val != null) {
      |                return val;
      |            }
      |            val = new PathMatcherBenchmark_jmhType();
      |            val.setup();
      |            val.readyTrial = true;
      |            f_pathmatcherbenchmark0_G = val;
      |            } catch (Throwable t) {
      |                control.isFailing = true;
      |                throw t;
      |            }
      |        }
      |        return val;
      |    }
      |
      |
      |}
      |
      |""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|javalin-test""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|io/javalin/performance/jmh_generated/PathMatcherBenchmark_matchLastStream_jmhTest.java""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package io.javalin.performance.jmh_generated;
      |
      |import java.util.List;
      |import java.util.concurrent.atomic.AtomicInteger;
      |import java.util.Collection;
      |import java.util.ArrayList;
      |import java.util.concurrent.TimeUnit;
      |import org.openjdk.jmh.annotations.CompilerControl;
      |import org.openjdk.jmh.runner.InfraControl;
      |import org.openjdk.jmh.infra.ThreadParams;
      |import org.openjdk.jmh.results.BenchmarkTaskResult;
      |import org.openjdk.jmh.results.Result;
      |import org.openjdk.jmh.results.ThroughputResult;
      |import org.openjdk.jmh.results.AverageTimeResult;
      |import org.openjdk.jmh.results.SampleTimeResult;
      |import org.openjdk.jmh.results.SingleShotResult;
      |import org.openjdk.jmh.util.SampleBuffer;
      |import org.openjdk.jmh.annotations.Mode;
      |import org.openjdk.jmh.annotations.Fork;
      |import org.openjdk.jmh.annotations.Measurement;
      |import org.openjdk.jmh.annotations.Threads;
      |import org.openjdk.jmh.annotations.Warmup;
      |import org.openjdk.jmh.annotations.BenchmarkMode;
      |import org.openjdk.jmh.results.RawResults;
      |import org.openjdk.jmh.results.ResultRole;
      |import java.lang.reflect.Field;
      |import org.openjdk.jmh.infra.BenchmarkParams;
      |import org.openjdk.jmh.infra.IterationParams;
      |import org.openjdk.jmh.infra.Blackhole;
      |import org.openjdk.jmh.infra.Control;
      |import org.openjdk.jmh.results.ScalarResult;
      |import org.openjdk.jmh.results.AggregationPolicy;
      |import org.openjdk.jmh.runner.FailureAssistException;
      |
      |import io.javalin.performance.jmh_generated.PathMatcherBenchmark_jmhType;
      |public final class PathMatcherBenchmark_matchLastStream_jmhTest {
      |
      |    byte p000, p001, p002, p003, p004, p005, p006, p007, p008, p009, p010, p011, p012, p013, p014, p015;
      |    byte p016, p017, p018, p019, p020, p021, p022, p023, p024, p025, p026, p027, p028, p029, p030, p031;
      |    byte p032, p033, p034, p035, p036, p037, p038, p039, p040, p041, p042, p043, p044, p045, p046, p047;
      |    byte p048, p049, p050, p051, p052, p053, p054, p055, p056, p057, p058, p059, p060, p061, p062, p063;
      |    byte p064, p065, p066, p067, p068, p069, p070, p071, p072, p073, p074, p075, p076, p077, p078, p079;
      |    byte p080, p081, p082, p083, p084, p085, p086, p087, p088, p089, p090, p091, p092, p093, p094, p095;
      |    byte p096, p097, p098, p099, p100, p101, p102, p103, p104, p105, p106, p107, p108, p109, p110, p111;
      |    byte p112, p113, p114, p115, p116, p117, p118, p119, p120, p121, p122, p123, p124, p125, p126, p127;
      |    byte p128, p129, p130, p131, p132, p133, p134, p135, p136, p137, p138, p139, p140, p141, p142, p143;
      |    byte p144, p145, p146, p147, p148, p149, p150, p151, p152, p153, p154, p155, p156, p157, p158, p159;
      |    byte p160, p161, p162, p163, p164, p165, p166, p167, p168, p169, p170, p171, p172, p173, p174, p175;
      |    byte p176, p177, p178, p179, p180, p181, p182, p183, p184, p185, p186, p187, p188, p189, p190, p191;
      |    byte p192, p193, p194, p195, p196, p197, p198, p199, p200, p201, p202, p203, p204, p205, p206, p207;
      |    byte p208, p209, p210, p211, p212, p213, p214, p215, p216, p217, p218, p219, p220, p221, p222, p223;
      |    byte p224, p225, p226, p227, p228, p229, p230, p231, p232, p233, p234, p235, p236, p237, p238, p239;
      |    byte p240, p241, p242, p243, p244, p245, p246, p247, p248, p249, p250, p251, p252, p253, p254, p255;
      |    int startRndMask;
      |    BenchmarkParams benchmarkParams;
      |    IterationParams iterationParams;
      |    ThreadParams threadParams;
      |    Blackhole blackhole;
      |    Control notifyControl;
      |
      |    public BenchmarkTaskResult matchLastStream_Throughput(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G = _jmh_tryInit_f_pathmatcherbenchmark0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_pathmatcherbenchmark0_G.matchLastStream(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            matchLastStream_thrpt_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, l_pathmatcherbenchmark0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_pathmatcherbenchmark0_G.matchLastStream(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.compareAndSet(l_pathmatcherbenchmark0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_pathmatcherbenchmark0_G.readyTrial) {
      |                            l_pathmatcherbenchmark0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.set(l_pathmatcherbenchmark0_G, 0);
      |                    }
      |                } else {
      |                    long l_pathmatcherbenchmark0_G_backoff = 1;
      |                    while (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.get(l_pathmatcherbenchmark0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_pathmatcherbenchmark0_G_backoff);
      |                        l_pathmatcherbenchmark0_G_backoff = Math.max(1024, l_pathmatcherbenchmark0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_pathmatcherbenchmark0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps;
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            res.measuredOps /= batchSize;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new ThroughputResult(ResultRole.PRIMARY, "matchLastStream", res.measuredOps, res.getTime(), benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void matchLastStream_thrpt_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G) throws Throwable {
      |        long operations = 0;
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        do {
      |            l_pathmatcherbenchmark0_G.matchLastStream(blackhole);
      |            operations++;
      |        } while(!control.isDone);
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult matchLastStream_AverageTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G = _jmh_tryInit_f_pathmatcherbenchmark0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_pathmatcherbenchmark0_G.matchLastStream(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            matchLastStream_avgt_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, l_pathmatcherbenchmark0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_pathmatcherbenchmark0_G.matchLastStream(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.compareAndSet(l_pathmatcherbenchmark0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_pathmatcherbenchmark0_G.readyTrial) {
      |                            l_pathmatcherbenchmark0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.set(l_pathmatcherbenchmark0_G, 0);
      |                    }
      |                } else {
      |                    long l_pathmatcherbenchmark0_G_backoff = 1;
      |                    while (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.get(l_pathmatcherbenchmark0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_pathmatcherbenchmark0_G_backoff);
      |                        l_pathmatcherbenchmark0_G_backoff = Math.max(1024, l_pathmatcherbenchmark0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_pathmatcherbenchmark0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps;
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            res.measuredOps /= batchSize;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new AverageTimeResult(ResultRole.PRIMARY, "matchLastStream", res.measuredOps, res.getTime(), benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void matchLastStream_avgt_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G) throws Throwable {
      |        long operations = 0;
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        do {
      |            l_pathmatcherbenchmark0_G.matchLastStream(blackhole);
      |            operations++;
      |        } while(!control.isDone);
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult matchLastStream_SampleTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G = _jmh_tryInit_f_pathmatcherbenchmark0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_pathmatcherbenchmark0_G.matchLastStream(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            int targetSamples = (int) (control.getDuration(TimeUnit.MILLISECONDS) * 20); // at max, 20 timestamps per millisecond
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            SampleBuffer buffer = new SampleBuffer();
      |            matchLastStream_sample_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, buffer, targetSamples, opsPerInv, batchSize, l_pathmatcherbenchmark0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_pathmatcherbenchmark0_G.matchLastStream(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.compareAndSet(l_pathmatcherbenchmark0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_pathmatcherbenchmark0_G.readyTrial) {
      |                            l_pathmatcherbenchmark0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.set(l_pathmatcherbenchmark0_G, 0);
      |                    }
      |                } else {
      |                    long l_pathmatcherbenchmark0_G_backoff = 1;
      |                    while (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.get(l_pathmatcherbenchmark0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_pathmatcherbenchmark0_G_backoff);
      |                        l_pathmatcherbenchmark0_G_backoff = Math.max(1024, l_pathmatcherbenchmark0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_pathmatcherbenchmark0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps * batchSize;
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new SampleTimeResult(ResultRole.PRIMARY, "matchLastStream", buffer, benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void matchLastStream_sample_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, SampleBuffer buffer, int targetSamples, long opsPerInv, int batchSize, PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G) throws Throwable {
      |        long realTime = 0;
      |        long operations = 0;
      |        int rnd = (int)System.nanoTime();
      |        int rndMask = startRndMask;
      |        long time = 0;
      |        int currentStride = 0;
      |        do {
      |            rnd = (rnd * 1664525 + 1013904223);
      |            boolean sample = (rnd & rndMask) == 0;
      |            if (sample) {
      |                time = System.nanoTime();
      |            }
      |            for (int b = 0; b < batchSize; b++) {
      |                if (control.volatileSpoiler) return;
      |                l_pathmatcherbenchmark0_G.matchLastStream(blackhole);
      |            }
      |            if (sample) {
      |                buffer.add((System.nanoTime() - time) / opsPerInv);
      |                if (currentStride++ > targetSamples) {
      |                    buffer.half();
      |                    currentStride = 0;
      |                    rndMask = (rndMask << 1) + 1;
      |                }
      |            }
      |            operations++;
      |        } while(!control.isDone);
      |        startRndMask = Math.max(startRndMask, rndMask);
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult matchLastStream_SingleShotTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G = _jmh_tryInit_f_pathmatcherbenchmark0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            notifyControl.startMeasurement = true;
      |            RawResults res = new RawResults();
      |            int batchSize = iterationParams.getBatchSize();
      |            matchLastStream_ss_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, batchSize, l_pathmatcherbenchmark0_G);
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.compareAndSet(l_pathmatcherbenchmark0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_pathmatcherbenchmark0_G.readyTrial) {
      |                            l_pathmatcherbenchmark0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.set(l_pathmatcherbenchmark0_G, 0);
      |                    }
      |                } else {
      |                    long l_pathmatcherbenchmark0_G_backoff = 1;
      |                    while (PathMatcherBenchmark_jmhType.tearTrialMutexUpdater.get(l_pathmatcherbenchmark0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_pathmatcherbenchmark0_G_backoff);
      |                        l_pathmatcherbenchmark0_G_backoff = Math.max(1024, l_pathmatcherbenchmark0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_pathmatcherbenchmark0_G = null;
      |                }
      |            }
      |            int opsPerInv = control.benchmarkParams.getOpsPerInvocation();
      |            long totalOps = opsPerInv;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult(totalOps, totalOps);
      |            results.add(new SingleShotResult(ResultRole.PRIMARY, "matchLastStream", res.getTime(), totalOps, benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void matchLastStream_ss_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, int batchSize, PathMatcherBenchmark_jmhType l_pathmatcherbenchmark0_G) throws Throwable {
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        for (int b = 0; b < batchSize; b++) {
      |            if (control.volatileSpoiler) return;
      |            l_pathmatcherbenchmark0_G.matchLastStream(blackhole);
      |        }
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |    }
      |
      |    
      |    static volatile PathMatcherBenchmark_jmhType f_pathmatcherbenchmark0_G;
      |    
      |    PathMatcherBenchmark_jmhType _jmh_tryInit_f_pathmatcherbenchmark0_G(InfraControl control) throws Throwable {
      |        PathMatcherBenchmark_jmhType val = f_pathmatcherbenchmark0_G;
      |        if (val != null) {
      |            return val;
      |        }
      |        synchronized(this.getClass()) {
      |            try {
      |            if (control.isFailing) throw new FailureAssistException();
      |            val = f_pathmatcherbenchmark0_G;
      |            if (val != null) {
      |                return val;
      |            }
      |            val = new PathMatcherBenchmark_jmhType();
      |            val.setup();
      |            val.readyTrial = true;
      |            f_pathmatcherbenchmark0_G = val;
      |            } catch (Throwable t) {
      |                control.isFailing = true;
      |                throw t;
      |            }
      |        }
      |        return val;
      |    }
      |
      |
      |}
      |
      |""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|javalin-test""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|io/javalin/performance/jmh_generated/PerformanceBenchmarkSuite_jmhType.java""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package io.javalin.performance.jmh_generated;
      |public class PerformanceBenchmarkSuite_jmhType extends PerformanceBenchmarkSuite_jmhType_B3 {
      |}
      |
      |""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|javalin-test""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|io/javalin/performance/jmh_generated/PerformanceBenchmarkSuite_jmhType_B1.java""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package io.javalin.performance.jmh_generated;
      |import io.javalin.performance.PerformanceBenchmarkSuite;
      |public class PerformanceBenchmarkSuite_jmhType_B1 extends io.javalin.performance.PerformanceBenchmarkSuite {
      |    byte b1_000, b1_001, b1_002, b1_003, b1_004, b1_005, b1_006, b1_007, b1_008, b1_009, b1_010, b1_011, b1_012, b1_013, b1_014, b1_015;
      |    byte b1_016, b1_017, b1_018, b1_019, b1_020, b1_021, b1_022, b1_023, b1_024, b1_025, b1_026, b1_027, b1_028, b1_029, b1_030, b1_031;
      |    byte b1_032, b1_033, b1_034, b1_035, b1_036, b1_037, b1_038, b1_039, b1_040, b1_041, b1_042, b1_043, b1_044, b1_045, b1_046, b1_047;
      |    byte b1_048, b1_049, b1_050, b1_051, b1_052, b1_053, b1_054, b1_055, b1_056, b1_057, b1_058, b1_059, b1_060, b1_061, b1_062, b1_063;
      |    byte b1_064, b1_065, b1_066, b1_067, b1_068, b1_069, b1_070, b1_071, b1_072, b1_073, b1_074, b1_075, b1_076, b1_077, b1_078, b1_079;
      |    byte b1_080, b1_081, b1_082, b1_083, b1_084, b1_085, b1_086, b1_087, b1_088, b1_089, b1_090, b1_091, b1_092, b1_093, b1_094, b1_095;
      |    byte b1_096, b1_097, b1_098, b1_099, b1_100, b1_101, b1_102, b1_103, b1_104, b1_105, b1_106, b1_107, b1_108, b1_109, b1_110, b1_111;
      |    byte b1_112, b1_113, b1_114, b1_115, b1_116, b1_117, b1_118, b1_119, b1_120, b1_121, b1_122, b1_123, b1_124, b1_125, b1_126, b1_127;
      |    byte b1_128, b1_129, b1_130, b1_131, b1_132, b1_133, b1_134, b1_135, b1_136, b1_137, b1_138, b1_139, b1_140, b1_141, b1_142, b1_143;
      |    byte b1_144, b1_145, b1_146, b1_147, b1_148, b1_149, b1_150, b1_151, b1_152, b1_153, b1_154, b1_155, b1_156, b1_157, b1_158, b1_159;
      |    byte b1_160, b1_161, b1_162, b1_163, b1_164, b1_165, b1_166, b1_167, b1_168, b1_169, b1_170, b1_171, b1_172, b1_173, b1_174, b1_175;
      |    byte b1_176, b1_177, b1_178, b1_179, b1_180, b1_181, b1_182, b1_183, b1_184, b1_185, b1_186, b1_187, b1_188, b1_189, b1_190, b1_191;
      |    byte b1_192, b1_193, b1_194, b1_195, b1_196, b1_197, b1_198, b1_199, b1_200, b1_201, b1_202, b1_203, b1_204, b1_205, b1_206, b1_207;
      |    byte b1_208, b1_209, b1_210, b1_211, b1_212, b1_213, b1_214, b1_215, b1_216, b1_217, b1_218, b1_219, b1_220, b1_221, b1_222, b1_223;
      |    byte b1_224, b1_225, b1_226, b1_227, b1_228, b1_229, b1_230, b1_231, b1_232, b1_233, b1_234, b1_235, b1_236, b1_237, b1_238, b1_239;
      |    byte b1_240, b1_241, b1_242, b1_243, b1_244, b1_245, b1_246, b1_247, b1_248, b1_249, b1_250, b1_251, b1_252, b1_253, b1_254, b1_255;
      |}
      |""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|javalin-test""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|io/javalin/performance/jmh_generated/PerformanceBenchmarkSuite_jmhType_B2.java""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package io.javalin.performance.jmh_generated;
      |import java.util.concurrent.atomic.AtomicIntegerFieldUpdater;
      |public class PerformanceBenchmarkSuite_jmhType_B2 extends PerformanceBenchmarkSuite_jmhType_B1 {
      |    public volatile int setupTrialMutex;
      |    public volatile int tearTrialMutex;
      |    public final static AtomicIntegerFieldUpdater<PerformanceBenchmarkSuite_jmhType_B2> setupTrialMutexUpdater = AtomicIntegerFieldUpdater.newUpdater(PerformanceBenchmarkSuite_jmhType_B2.class, "setupTrialMutex");
      |    public final static AtomicIntegerFieldUpdater<PerformanceBenchmarkSuite_jmhType_B2> tearTrialMutexUpdater = AtomicIntegerFieldUpdater.newUpdater(PerformanceBenchmarkSuite_jmhType_B2.class, "tearTrialMutex");
      |
      |    public volatile int setupIterationMutex;
      |    public volatile int tearIterationMutex;
      |    public final static AtomicIntegerFieldUpdater<PerformanceBenchmarkSuite_jmhType_B2> setupIterationMutexUpdater = AtomicIntegerFieldUpdater.newUpdater(PerformanceBenchmarkSuite_jmhType_B2.class, "setupIterationMutex");
      |    public final static AtomicIntegerFieldUpdater<PerformanceBenchmarkSuite_jmhType_B2> tearIterationMutexUpdater = AtomicIntegerFieldUpdater.newUpdater(PerformanceBenchmarkSuite_jmhType_B2.class, "tearIterationMutex");
      |
      |    public volatile int setupInvocationMutex;
      |    public volatile int tearInvocationMutex;
      |    public final static AtomicIntegerFieldUpdater<PerformanceBenchmarkSuite_jmhType_B2> setupInvocationMutexUpdater = AtomicIntegerFieldUpdater.newUpdater(PerformanceBenchmarkSuite_jmhType_B2.class, "setupInvocationMutex");
      |    public final static AtomicIntegerFieldUpdater<PerformanceBenchmarkSuite_jmhType_B2> tearInvocationMutexUpdater = AtomicIntegerFieldUpdater.newUpdater(PerformanceBenchmarkSuite_jmhType_B2.class, "tearInvocationMutex");
      |
      |    public volatile boolean readyTrial;
      |    public volatile boolean readyIteration;
      |    public volatile boolean readyInvocation;
      |}
      |""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|javalin-test""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|io/javalin/performance/jmh_generated/PerformanceBenchmarkSuite_jmhType_B3.java""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package io.javalin.performance.jmh_generated;
      |public class PerformanceBenchmarkSuite_jmhType_B3 extends PerformanceBenchmarkSuite_jmhType_B2 {
      |    byte b3_000, b3_001, b3_002, b3_003, b3_004, b3_005, b3_006, b3_007, b3_008, b3_009, b3_010, b3_011, b3_012, b3_013, b3_014, b3_015;
      |    byte b3_016, b3_017, b3_018, b3_019, b3_020, b3_021, b3_022, b3_023, b3_024, b3_025, b3_026, b3_027, b3_028, b3_029, b3_030, b3_031;
      |    byte b3_032, b3_033, b3_034, b3_035, b3_036, b3_037, b3_038, b3_039, b3_040, b3_041, b3_042, b3_043, b3_044, b3_045, b3_046, b3_047;
      |    byte b3_048, b3_049, b3_050, b3_051, b3_052, b3_053, b3_054, b3_055, b3_056, b3_057, b3_058, b3_059, b3_060, b3_061, b3_062, b3_063;
      |    byte b3_064, b3_065, b3_066, b3_067, b3_068, b3_069, b3_070, b3_071, b3_072, b3_073, b3_074, b3_075, b3_076, b3_077, b3_078, b3_079;
      |    byte b3_080, b3_081, b3_082, b3_083, b3_084, b3_085, b3_086, b3_087, b3_088, b3_089, b3_090, b3_091, b3_092, b3_093, b3_094, b3_095;
      |    byte b3_096, b3_097, b3_098, b3_099, b3_100, b3_101, b3_102, b3_103, b3_104, b3_105, b3_106, b3_107, b3_108, b3_109, b3_110, b3_111;
      |    byte b3_112, b3_113, b3_114, b3_115, b3_116, b3_117, b3_118, b3_119, b3_120, b3_121, b3_122, b3_123, b3_124, b3_125, b3_126, b3_127;
      |    byte b3_128, b3_129, b3_130, b3_131, b3_132, b3_133, b3_134, b3_135, b3_136, b3_137, b3_138, b3_139, b3_140, b3_141, b3_142, b3_143;
      |    byte b3_144, b3_145, b3_146, b3_147, b3_148, b3_149, b3_150, b3_151, b3_152, b3_153, b3_154, b3_155, b3_156, b3_157, b3_158, b3_159;
      |    byte b3_160, b3_161, b3_162, b3_163, b3_164, b3_165, b3_166, b3_167, b3_168, b3_169, b3_170, b3_171, b3_172, b3_173, b3_174, b3_175;
      |    byte b3_176, b3_177, b3_178, b3_179, b3_180, b3_181, b3_182, b3_183, b3_184, b3_185, b3_186, b3_187, b3_188, b3_189, b3_190, b3_191;
      |    byte b3_192, b3_193, b3_194, b3_195, b3_196, b3_197, b3_198, b3_199, b3_200, b3_201, b3_202, b3_203, b3_204, b3_205, b3_206, b3_207;
      |    byte b3_208, b3_209, b3_210, b3_211, b3_212, b3_213, b3_214, b3_215, b3_216, b3_217, b3_218, b3_219, b3_220, b3_221, b3_222, b3_223;
      |    byte b3_224, b3_225, b3_226, b3_227, b3_228, b3_229, b3_230, b3_231, b3_232, b3_233, b3_234, b3_235, b3_236, b3_237, b3_238, b3_239;
      |    byte b3_240, b3_241, b3_242, b3_243, b3_244, b3_245, b3_246, b3_247, b3_248, b3_249, b3_250, b3_251, b3_252, b3_253, b3_254, b3_255;
      |}
      |
      |""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|javalin-test""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|io/javalin/performance/jmh_generated/PerformanceBenchmarkSuite_manyRoutes_first_jmhTest.java""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package io.javalin.performance.jmh_generated;
      |
      |import java.util.List;
      |import java.util.concurrent.atomic.AtomicInteger;
      |import java.util.Collection;
      |import java.util.ArrayList;
      |import java.util.concurrent.TimeUnit;
      |import org.openjdk.jmh.annotations.CompilerControl;
      |import org.openjdk.jmh.runner.InfraControl;
      |import org.openjdk.jmh.infra.ThreadParams;
      |import org.openjdk.jmh.results.BenchmarkTaskResult;
      |import org.openjdk.jmh.results.Result;
      |import org.openjdk.jmh.results.ThroughputResult;
      |import org.openjdk.jmh.results.AverageTimeResult;
      |import org.openjdk.jmh.results.SampleTimeResult;
      |import org.openjdk.jmh.results.SingleShotResult;
      |import org.openjdk.jmh.util.SampleBuffer;
      |import org.openjdk.jmh.annotations.Mode;
      |import org.openjdk.jmh.annotations.Fork;
      |import org.openjdk.jmh.annotations.Measurement;
      |import org.openjdk.jmh.annotations.Threads;
      |import org.openjdk.jmh.annotations.Warmup;
      |import org.openjdk.jmh.annotations.BenchmarkMode;
      |import org.openjdk.jmh.results.RawResults;
      |import org.openjdk.jmh.results.ResultRole;
      |import java.lang.reflect.Field;
      |import org.openjdk.jmh.infra.BenchmarkParams;
      |import org.openjdk.jmh.infra.IterationParams;
      |import org.openjdk.jmh.infra.Blackhole;
      |import org.openjdk.jmh.infra.Control;
      |import org.openjdk.jmh.results.ScalarResult;
      |import org.openjdk.jmh.results.AggregationPolicy;
      |import org.openjdk.jmh.runner.FailureAssistException;
      |
      |import io.javalin.performance.jmh_generated.PerformanceBenchmarkSuite_jmhType;
      |public final class PerformanceBenchmarkSuite_manyRoutes_first_jmhTest {
      |
      |    byte p000, p001, p002, p003, p004, p005, p006, p007, p008, p009, p010, p011, p012, p013, p014, p015;
      |    byte p016, p017, p018, p019, p020, p021, p022, p023, p024, p025, p026, p027, p028, p029, p030, p031;
      |    byte p032, p033, p034, p035, p036, p037, p038, p039, p040, p041, p042, p043, p044, p045, p046, p047;
      |    byte p048, p049, p050, p051, p052, p053, p054, p055, p056, p057, p058, p059, p060, p061, p062, p063;
      |    byte p064, p065, p066, p067, p068, p069, p070, p071, p072, p073, p074, p075, p076, p077, p078, p079;
      |    byte p080, p081, p082, p083, p084, p085, p086, p087, p088, p089, p090, p091, p092, p093, p094, p095;
      |    byte p096, p097, p098, p099, p100, p101, p102, p103, p104, p105, p106, p107, p108, p109, p110, p111;
      |    byte p112, p113, p114, p115, p116, p117, p118, p119, p120, p121, p122, p123, p124, p125, p126, p127;
      |    byte p128, p129, p130, p131, p132, p133, p134, p135, p136, p137, p138, p139, p140, p141, p142, p143;
      |    byte p144, p145, p146, p147, p148, p149, p150, p151, p152, p153, p154, p155, p156, p157, p158, p159;
      |    byte p160, p161, p162, p163, p164, p165, p166, p167, p168, p169, p170, p171, p172, p173, p174, p175;
      |    byte p176, p177, p178, p179, p180, p181, p182, p183, p184, p185, p186, p187, p188, p189, p190, p191;
      |    byte p192, p193, p194, p195, p196, p197, p198, p199, p200, p201, p202, p203, p204, p205, p206, p207;
      |    byte p208, p209, p210, p211, p212, p213, p214, p215, p216, p217, p218, p219, p220, p221, p222, p223;
      |    byte p224, p225, p226, p227, p228, p229, p230, p231, p232, p233, p234, p235, p236, p237, p238, p239;
      |    byte p240, p241, p242, p243, p244, p245, p246, p247, p248, p249, p250, p251, p252, p253, p254, p255;
      |    int startRndMask;
      |    BenchmarkParams benchmarkParams;
      |    IterationParams iterationParams;
      |    ThreadParams threadParams;
      |    Blackhole blackhole;
      |    Control notifyControl;
      |
      |    public BenchmarkTaskResult manyRoutes_first_Throughput(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_performancebenchmarksuite0_G.manyRoutes_first(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            manyRoutes_first_thrpt_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, l_performancebenchmarksuite0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_performancebenchmarksuite0_G.manyRoutes_first(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps;
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            res.measuredOps /= batchSize;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new ThroughputResult(ResultRole.PRIMARY, "manyRoutes_first", res.measuredOps, res.getTime(), benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void manyRoutes_first_thrpt_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long operations = 0;
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        do {
      |            l_performancebenchmarksuite0_G.manyRoutes_first(blackhole);
      |            operations++;
      |        } while(!control.isDone);
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult manyRoutes_first_AverageTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_performancebenchmarksuite0_G.manyRoutes_first(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            manyRoutes_first_avgt_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, l_performancebenchmarksuite0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_performancebenchmarksuite0_G.manyRoutes_first(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps;
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            res.measuredOps /= batchSize;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new AverageTimeResult(ResultRole.PRIMARY, "manyRoutes_first", res.measuredOps, res.getTime(), benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void manyRoutes_first_avgt_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long operations = 0;
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        do {
      |            l_performancebenchmarksuite0_G.manyRoutes_first(blackhole);
      |            operations++;
      |        } while(!control.isDone);
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult manyRoutes_first_SampleTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_performancebenchmarksuite0_G.manyRoutes_first(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            int targetSamples = (int) (control.getDuration(TimeUnit.MILLISECONDS) * 20); // at max, 20 timestamps per millisecond
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            SampleBuffer buffer = new SampleBuffer();
      |            manyRoutes_first_sample_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, buffer, targetSamples, opsPerInv, batchSize, l_performancebenchmarksuite0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_performancebenchmarksuite0_G.manyRoutes_first(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps * batchSize;
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new SampleTimeResult(ResultRole.PRIMARY, "manyRoutes_first", buffer, benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void manyRoutes_first_sample_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, SampleBuffer buffer, int targetSamples, long opsPerInv, int batchSize, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long realTime = 0;
      |        long operations = 0;
      |        int rnd = (int)System.nanoTime();
      |        int rndMask = startRndMask;
      |        long time = 0;
      |        int currentStride = 0;
      |        do {
      |            rnd = (rnd * 1664525 + 1013904223);
      |            boolean sample = (rnd & rndMask) == 0;
      |            if (sample) {
      |                time = System.nanoTime();
      |            }
      |            for (int b = 0; b < batchSize; b++) {
      |                if (control.volatileSpoiler) return;
      |                l_performancebenchmarksuite0_G.manyRoutes_first(blackhole);
      |            }
      |            if (sample) {
      |                buffer.add((System.nanoTime() - time) / opsPerInv);
      |                if (currentStride++ > targetSamples) {
      |                    buffer.half();
      |                    currentStride = 0;
      |                    rndMask = (rndMask << 1) + 1;
      |                }
      |            }
      |            operations++;
      |        } while(!control.isDone);
      |        startRndMask = Math.max(startRndMask, rndMask);
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult manyRoutes_first_SingleShotTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            notifyControl.startMeasurement = true;
      |            RawResults res = new RawResults();
      |            int batchSize = iterationParams.getBatchSize();
      |            manyRoutes_first_ss_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, batchSize, l_performancebenchmarksuite0_G);
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            int opsPerInv = control.benchmarkParams.getOpsPerInvocation();
      |            long totalOps = opsPerInv;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult(totalOps, totalOps);
      |            results.add(new SingleShotResult(ResultRole.PRIMARY, "manyRoutes_first", res.getTime(), totalOps, benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void manyRoutes_first_ss_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, int batchSize, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        for (int b = 0; b < batchSize; b++) {
      |            if (control.volatileSpoiler) return;
      |            l_performancebenchmarksuite0_G.manyRoutes_first(blackhole);
      |        }
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |    }
      |
      |    
      |    static volatile PerformanceBenchmarkSuite_jmhType f_performancebenchmarksuite0_G;
      |    
      |    PerformanceBenchmarkSuite_jmhType _jmh_tryInit_f_performancebenchmarksuite0_G(InfraControl control) throws Throwable {
      |        PerformanceBenchmarkSuite_jmhType val = f_performancebenchmarksuite0_G;
      |        if (val != null) {
      |            return val;
      |        }
      |        synchronized(this.getClass()) {
      |            try {
      |            if (control.isFailing) throw new FailureAssistException();
      |            val = f_performancebenchmarksuite0_G;
      |            if (val != null) {
      |                return val;
      |            }
      |            val = new PerformanceBenchmarkSuite_jmhType();
      |            val.setup();
      |            val.readyTrial = true;
      |            f_performancebenchmarksuite0_G = val;
      |            } catch (Throwable t) {
      |                control.isFailing = true;
      |                throw t;
      |            }
      |        }
      |        return val;
      |    }
      |
      |
      |}
      |
      |""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|javalin-test""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|io/javalin/performance/jmh_generated/PerformanceBenchmarkSuite_manyRoutes_last_jmhTest.java""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package io.javalin.performance.jmh_generated;
      |
      |import java.util.List;
      |import java.util.concurrent.atomic.AtomicInteger;
      |import java.util.Collection;
      |import java.util.ArrayList;
      |import java.util.concurrent.TimeUnit;
      |import org.openjdk.jmh.annotations.CompilerControl;
      |import org.openjdk.jmh.runner.InfraControl;
      |import org.openjdk.jmh.infra.ThreadParams;
      |import org.openjdk.jmh.results.BenchmarkTaskResult;
      |import org.openjdk.jmh.results.Result;
      |import org.openjdk.jmh.results.ThroughputResult;
      |import org.openjdk.jmh.results.AverageTimeResult;
      |import org.openjdk.jmh.results.SampleTimeResult;
      |import org.openjdk.jmh.results.SingleShotResult;
      |import org.openjdk.jmh.util.SampleBuffer;
      |import org.openjdk.jmh.annotations.Mode;
      |import org.openjdk.jmh.annotations.Fork;
      |import org.openjdk.jmh.annotations.Measurement;
      |import org.openjdk.jmh.annotations.Threads;
      |import org.openjdk.jmh.annotations.Warmup;
      |import org.openjdk.jmh.annotations.BenchmarkMode;
      |import org.openjdk.jmh.results.RawResults;
      |import org.openjdk.jmh.results.ResultRole;
      |import java.lang.reflect.Field;
      |import org.openjdk.jmh.infra.BenchmarkParams;
      |import org.openjdk.jmh.infra.IterationParams;
      |import org.openjdk.jmh.infra.Blackhole;
      |import org.openjdk.jmh.infra.Control;
      |import org.openjdk.jmh.results.ScalarResult;
      |import org.openjdk.jmh.results.AggregationPolicy;
      |import org.openjdk.jmh.runner.FailureAssistException;
      |
      |import io.javalin.performance.jmh_generated.PerformanceBenchmarkSuite_jmhType;
      |public final class PerformanceBenchmarkSuite_manyRoutes_last_jmhTest {
      |
      |    byte p000, p001, p002, p003, p004, p005, p006, p007, p008, p009, p010, p011, p012, p013, p014, p015;
      |    byte p016, p017, p018, p019, p020, p021, p022, p023, p024, p025, p026, p027, p028, p029, p030, p031;
      |    byte p032, p033, p034, p035, p036, p037, p038, p039, p040, p041, p042, p043, p044, p045, p046, p047;
      |    byte p048, p049, p050, p051, p052, p053, p054, p055, p056, p057, p058, p059, p060, p061, p062, p063;
      |    byte p064, p065, p066, p067, p068, p069, p070, p071, p072, p073, p074, p075, p076, p077, p078, p079;
      |    byte p080, p081, p082, p083, p084, p085, p086, p087, p088, p089, p090, p091, p092, p093, p094, p095;
      |    byte p096, p097, p098, p099, p100, p101, p102, p103, p104, p105, p106, p107, p108, p109, p110, p111;
      |    byte p112, p113, p114, p115, p116, p117, p118, p119, p120, p121, p122, p123, p124, p125, p126, p127;
      |    byte p128, p129, p130, p131, p132, p133, p134, p135, p136, p137, p138, p139, p140, p141, p142, p143;
      |    byte p144, p145, p146, p147, p148, p149, p150, p151, p152, p153, p154, p155, p156, p157, p158, p159;
      |    byte p160, p161, p162, p163, p164, p165, p166, p167, p168, p169, p170, p171, p172, p173, p174, p175;
      |    byte p176, p177, p178, p179, p180, p181, p182, p183, p184, p185, p186, p187, p188, p189, p190, p191;
      |    byte p192, p193, p194, p195, p196, p197, p198, p199, p200, p201, p202, p203, p204, p205, p206, p207;
      |    byte p208, p209, p210, p211, p212, p213, p214, p215, p216, p217, p218, p219, p220, p221, p222, p223;
      |    byte p224, p225, p226, p227, p228, p229, p230, p231, p232, p233, p234, p235, p236, p237, p238, p239;
      |    byte p240, p241, p242, p243, p244, p245, p246, p247, p248, p249, p250, p251, p252, p253, p254, p255;
      |    int startRndMask;
      |    BenchmarkParams benchmarkParams;
      |    IterationParams iterationParams;
      |    ThreadParams threadParams;
      |    Blackhole blackhole;
      |    Control notifyControl;
      |
      |    public BenchmarkTaskResult manyRoutes_last_Throughput(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_performancebenchmarksuite0_G.manyRoutes_last(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            manyRoutes_last_thrpt_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, l_performancebenchmarksuite0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_performancebenchmarksuite0_G.manyRoutes_last(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps;
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            res.measuredOps /= batchSize;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new ThroughputResult(ResultRole.PRIMARY, "manyRoutes_last", res.measuredOps, res.getTime(), benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void manyRoutes_last_thrpt_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long operations = 0;
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        do {
      |            l_performancebenchmarksuite0_G.manyRoutes_last(blackhole);
      |            operations++;
      |        } while(!control.isDone);
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult manyRoutes_last_AverageTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_performancebenchmarksuite0_G.manyRoutes_last(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            manyRoutes_last_avgt_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, l_performancebenchmarksuite0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_performancebenchmarksuite0_G.manyRoutes_last(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps;
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            res.measuredOps /= batchSize;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new AverageTimeResult(ResultRole.PRIMARY, "manyRoutes_last", res.measuredOps, res.getTime(), benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void manyRoutes_last_avgt_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long operations = 0;
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        do {
      |            l_performancebenchmarksuite0_G.manyRoutes_last(blackhole);
      |            operations++;
      |        } while(!control.isDone);
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult manyRoutes_last_SampleTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_performancebenchmarksuite0_G.manyRoutes_last(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            int targetSamples = (int) (control.getDuration(TimeUnit.MILLISECONDS) * 20); // at max, 20 timestamps per millisecond
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            SampleBuffer buffer = new SampleBuffer();
      |            manyRoutes_last_sample_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, buffer, targetSamples, opsPerInv, batchSize, l_performancebenchmarksuite0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_performancebenchmarksuite0_G.manyRoutes_last(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps * batchSize;
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new SampleTimeResult(ResultRole.PRIMARY, "manyRoutes_last", buffer, benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void manyRoutes_last_sample_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, SampleBuffer buffer, int targetSamples, long opsPerInv, int batchSize, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long realTime = 0;
      |        long operations = 0;
      |        int rnd = (int)System.nanoTime();
      |        int rndMask = startRndMask;
      |        long time = 0;
      |        int currentStride = 0;
      |        do {
      |            rnd = (rnd * 1664525 + 1013904223);
      |            boolean sample = (rnd & rndMask) == 0;
      |            if (sample) {
      |                time = System.nanoTime();
      |            }
      |            for (int b = 0; b < batchSize; b++) {
      |                if (control.volatileSpoiler) return;
      |                l_performancebenchmarksuite0_G.manyRoutes_last(blackhole);
      |            }
      |            if (sample) {
      |                buffer.add((System.nanoTime() - time) / opsPerInv);
      |                if (currentStride++ > targetSamples) {
      |                    buffer.half();
      |                    currentStride = 0;
      |                    rndMask = (rndMask << 1) + 1;
      |                }
      |            }
      |            operations++;
      |        } while(!control.isDone);
      |        startRndMask = Math.max(startRndMask, rndMask);
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult manyRoutes_last_SingleShotTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            notifyControl.startMeasurement = true;
      |            RawResults res = new RawResults();
      |            int batchSize = iterationParams.getBatchSize();
      |            manyRoutes_last_ss_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, batchSize, l_performancebenchmarksuite0_G);
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            int opsPerInv = control.benchmarkParams.getOpsPerInvocation();
      |            long totalOps = opsPerInv;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult(totalOps, totalOps);
      |            results.add(new SingleShotResult(ResultRole.PRIMARY, "manyRoutes_last", res.getTime(), totalOps, benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void manyRoutes_last_ss_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, int batchSize, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        for (int b = 0; b < batchSize; b++) {
      |            if (control.volatileSpoiler) return;
      |            l_performancebenchmarksuite0_G.manyRoutes_last(blackhole);
      |        }
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |    }
      |
      |    
      |    static volatile PerformanceBenchmarkSuite_jmhType f_performancebenchmarksuite0_G;
      |    
      |    PerformanceBenchmarkSuite_jmhType _jmh_tryInit_f_performancebenchmarksuite0_G(InfraControl control) throws Throwable {
      |        PerformanceBenchmarkSuite_jmhType val = f_performancebenchmarksuite0_G;
      |        if (val != null) {
      |            return val;
      |        }
      |        synchronized(this.getClass()) {
      |            try {
      |            if (control.isFailing) throw new FailureAssistException();
      |            val = f_performancebenchmarksuite0_G;
      |            if (val != null) {
      |                return val;
      |            }
      |            val = new PerformanceBenchmarkSuite_jmhType();
      |            val.setup();
      |            val.readyTrial = true;
      |            f_performancebenchmarksuite0_G = val;
      |            } catch (Throwable t) {
      |                control.isFailing = true;
      |                throw t;
      |            }
      |        }
      |        return val;
      |    }
      |
      |
      |}
      |
      |""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|javalin-test""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|io/javalin/performance/jmh_generated/PerformanceBenchmarkSuite_multiPathParam_jmhTest.java""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package io.javalin.performance.jmh_generated;
      |
      |import java.util.List;
      |import java.util.concurrent.atomic.AtomicInteger;
      |import java.util.Collection;
      |import java.util.ArrayList;
      |import java.util.concurrent.TimeUnit;
      |import org.openjdk.jmh.annotations.CompilerControl;
      |import org.openjdk.jmh.runner.InfraControl;
      |import org.openjdk.jmh.infra.ThreadParams;
      |import org.openjdk.jmh.results.BenchmarkTaskResult;
      |import org.openjdk.jmh.results.Result;
      |import org.openjdk.jmh.results.ThroughputResult;
      |import org.openjdk.jmh.results.AverageTimeResult;
      |import org.openjdk.jmh.results.SampleTimeResult;
      |import org.openjdk.jmh.results.SingleShotResult;
      |import org.openjdk.jmh.util.SampleBuffer;
      |import org.openjdk.jmh.annotations.Mode;
      |import org.openjdk.jmh.annotations.Fork;
      |import org.openjdk.jmh.annotations.Measurement;
      |import org.openjdk.jmh.annotations.Threads;
      |import org.openjdk.jmh.annotations.Warmup;
      |import org.openjdk.jmh.annotations.BenchmarkMode;
      |import org.openjdk.jmh.results.RawResults;
      |import org.openjdk.jmh.results.ResultRole;
      |import java.lang.reflect.Field;
      |import org.openjdk.jmh.infra.BenchmarkParams;
      |import org.openjdk.jmh.infra.IterationParams;
      |import org.openjdk.jmh.infra.Blackhole;
      |import org.openjdk.jmh.infra.Control;
      |import org.openjdk.jmh.results.ScalarResult;
      |import org.openjdk.jmh.results.AggregationPolicy;
      |import org.openjdk.jmh.runner.FailureAssistException;
      |
      |import io.javalin.performance.jmh_generated.PerformanceBenchmarkSuite_jmhType;
      |public final class PerformanceBenchmarkSuite_multiPathParam_jmhTest {
      |
      |    byte p000, p001, p002, p003, p004, p005, p006, p007, p008, p009, p010, p011, p012, p013, p014, p015;
      |    byte p016, p017, p018, p019, p020, p021, p022, p023, p024, p025, p026, p027, p028, p029, p030, p031;
      |    byte p032, p033, p034, p035, p036, p037, p038, p039, p040, p041, p042, p043, p044, p045, p046, p047;
      |    byte p048, p049, p050, p051, p052, p053, p054, p055, p056, p057, p058, p059, p060, p061, p062, p063;
      |    byte p064, p065, p066, p067, p068, p069, p070, p071, p072, p073, p074, p075, p076, p077, p078, p079;
      |    byte p080, p081, p082, p083, p084, p085, p086, p087, p088, p089, p090, p091, p092, p093, p094, p095;
      |    byte p096, p097, p098, p099, p100, p101, p102, p103, p104, p105, p106, p107, p108, p109, p110, p111;
      |    byte p112, p113, p114, p115, p116, p117, p118, p119, p120, p121, p122, p123, p124, p125, p126, p127;
      |    byte p128, p129, p130, p131, p132, p133, p134, p135, p136, p137, p138, p139, p140, p141, p142, p143;
      |    byte p144, p145, p146, p147, p148, p149, p150, p151, p152, p153, p154, p155, p156, p157, p158, p159;
      |    byte p160, p161, p162, p163, p164, p165, p166, p167, p168, p169, p170, p171, p172, p173, p174, p175;
      |    byte p176, p177, p178, p179, p180, p181, p182, p183, p184, p185, p186, p187, p188, p189, p190, p191;
      |    byte p192, p193, p194, p195, p196, p197, p198, p199, p200, p201, p202, p203, p204, p205, p206, p207;
      |    byte p208, p209, p210, p211, p212, p213, p214, p215, p216, p217, p218, p219, p220, p221, p222, p223;
      |    byte p224, p225, p226, p227, p228, p229, p230, p231, p232, p233, p234, p235, p236, p237, p238, p239;
      |    byte p240, p241, p242, p243, p244, p245, p246, p247, p248, p249, p250, p251, p252, p253, p254, p255;
      |    int startRndMask;
      |    BenchmarkParams benchmarkParams;
      |    IterationParams iterationParams;
      |    ThreadParams threadParams;
      |    Blackhole blackhole;
      |    Control notifyControl;
      |
      |    public BenchmarkTaskResult multiPathParam_Throughput(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_performancebenchmarksuite0_G.multiPathParam(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            multiPathParam_thrpt_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, l_performancebenchmarksuite0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_performancebenchmarksuite0_G.multiPathParam(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps;
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            res.measuredOps /= batchSize;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new ThroughputResult(ResultRole.PRIMARY, "multiPathParam", res.measuredOps, res.getTime(), benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void multiPathParam_thrpt_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long operations = 0;
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        do {
      |            l_performancebenchmarksuite0_G.multiPathParam(blackhole);
      |            operations++;
      |        } while(!control.isDone);
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult multiPathParam_AverageTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_performancebenchmarksuite0_G.multiPathParam(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            multiPathParam_avgt_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, l_performancebenchmarksuite0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_performancebenchmarksuite0_G.multiPathParam(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps;
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            res.measuredOps /= batchSize;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new AverageTimeResult(ResultRole.PRIMARY, "multiPathParam", res.measuredOps, res.getTime(), benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void multiPathParam_avgt_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long operations = 0;
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        do {
      |            l_performancebenchmarksuite0_G.multiPathParam(blackhole);
      |            operations++;
      |        } while(!control.isDone);
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult multiPathParam_SampleTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_performancebenchmarksuite0_G.multiPathParam(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            int targetSamples = (int) (control.getDuration(TimeUnit.MILLISECONDS) * 20); // at max, 20 timestamps per millisecond
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            SampleBuffer buffer = new SampleBuffer();
      |            multiPathParam_sample_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, buffer, targetSamples, opsPerInv, batchSize, l_performancebenchmarksuite0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_performancebenchmarksuite0_G.multiPathParam(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps * batchSize;
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new SampleTimeResult(ResultRole.PRIMARY, "multiPathParam", buffer, benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void multiPathParam_sample_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, SampleBuffer buffer, int targetSamples, long opsPerInv, int batchSize, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long realTime = 0;
      |        long operations = 0;
      |        int rnd = (int)System.nanoTime();
      |        int rndMask = startRndMask;
      |        long time = 0;
      |        int currentStride = 0;
      |        do {
      |            rnd = (rnd * 1664525 + 1013904223);
      |            boolean sample = (rnd & rndMask) == 0;
      |            if (sample) {
      |                time = System.nanoTime();
      |            }
      |            for (int b = 0; b < batchSize; b++) {
      |                if (control.volatileSpoiler) return;
      |                l_performancebenchmarksuite0_G.multiPathParam(blackhole);
      |            }
      |            if (sample) {
      |                buffer.add((System.nanoTime() - time) / opsPerInv);
      |                if (currentStride++ > targetSamples) {
      |                    buffer.half();
      |                    currentStride = 0;
      |                    rndMask = (rndMask << 1) + 1;
      |                }
      |            }
      |            operations++;
      |        } while(!control.isDone);
      |        startRndMask = Math.max(startRndMask, rndMask);
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult multiPathParam_SingleShotTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            notifyControl.startMeasurement = true;
      |            RawResults res = new RawResults();
      |            int batchSize = iterationParams.getBatchSize();
      |            multiPathParam_ss_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, batchSize, l_performancebenchmarksuite0_G);
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            int opsPerInv = control.benchmarkParams.getOpsPerInvocation();
      |            long totalOps = opsPerInv;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult(totalOps, totalOps);
      |            results.add(new SingleShotResult(ResultRole.PRIMARY, "multiPathParam", res.getTime(), totalOps, benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void multiPathParam_ss_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, int batchSize, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        for (int b = 0; b < batchSize; b++) {
      |            if (control.volatileSpoiler) return;
      |            l_performancebenchmarksuite0_G.multiPathParam(blackhole);
      |        }
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |    }
      |
      |    
      |    static volatile PerformanceBenchmarkSuite_jmhType f_performancebenchmarksuite0_G;
      |    
      |    PerformanceBenchmarkSuite_jmhType _jmh_tryInit_f_performancebenchmarksuite0_G(InfraControl control) throws Throwable {
      |        PerformanceBenchmarkSuite_jmhType val = f_performancebenchmarksuite0_G;
      |        if (val != null) {
      |            return val;
      |        }
      |        synchronized(this.getClass()) {
      |            try {
      |            if (control.isFailing) throw new FailureAssistException();
      |            val = f_performancebenchmarksuite0_G;
      |            if (val != null) {
      |                return val;
      |            }
      |            val = new PerformanceBenchmarkSuite_jmhType();
      |            val.setup();
      |            val.readyTrial = true;
      |            f_performancebenchmarksuite0_G = val;
      |            } catch (Throwable t) {
      |                control.isFailing = true;
      |                throw t;
      |            }
      |        }
      |        return val;
      |    }
      |
      |
      |}
      |
      |""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|javalin-test""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|io/javalin/performance/jmh_generated/PerformanceBenchmarkSuite_plainGet_jmhTest.java""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package io.javalin.performance.jmh_generated;
      |
      |import java.util.List;
      |import java.util.concurrent.atomic.AtomicInteger;
      |import java.util.Collection;
      |import java.util.ArrayList;
      |import java.util.concurrent.TimeUnit;
      |import org.openjdk.jmh.annotations.CompilerControl;
      |import org.openjdk.jmh.runner.InfraControl;
      |import org.openjdk.jmh.infra.ThreadParams;
      |import org.openjdk.jmh.results.BenchmarkTaskResult;
      |import org.openjdk.jmh.results.Result;
      |import org.openjdk.jmh.results.ThroughputResult;
      |import org.openjdk.jmh.results.AverageTimeResult;
      |import org.openjdk.jmh.results.SampleTimeResult;
      |import org.openjdk.jmh.results.SingleShotResult;
      |import org.openjdk.jmh.util.SampleBuffer;
      |import org.openjdk.jmh.annotations.Mode;
      |import org.openjdk.jmh.annotations.Fork;
      |import org.openjdk.jmh.annotations.Measurement;
      |import org.openjdk.jmh.annotations.Threads;
      |import org.openjdk.jmh.annotations.Warmup;
      |import org.openjdk.jmh.annotations.BenchmarkMode;
      |import org.openjdk.jmh.results.RawResults;
      |import org.openjdk.jmh.results.ResultRole;
      |import java.lang.reflect.Field;
      |import org.openjdk.jmh.infra.BenchmarkParams;
      |import org.openjdk.jmh.infra.IterationParams;
      |import org.openjdk.jmh.infra.Blackhole;
      |import org.openjdk.jmh.infra.Control;
      |import org.openjdk.jmh.results.ScalarResult;
      |import org.openjdk.jmh.results.AggregationPolicy;
      |import org.openjdk.jmh.runner.FailureAssistException;
      |
      |import io.javalin.performance.jmh_generated.PerformanceBenchmarkSuite_jmhType;
      |public final class PerformanceBenchmarkSuite_plainGet_jmhTest {
      |
      |    byte p000, p001, p002, p003, p004, p005, p006, p007, p008, p009, p010, p011, p012, p013, p014, p015;
      |    byte p016, p017, p018, p019, p020, p021, p022, p023, p024, p025, p026, p027, p028, p029, p030, p031;
      |    byte p032, p033, p034, p035, p036, p037, p038, p039, p040, p041, p042, p043, p044, p045, p046, p047;
      |    byte p048, p049, p050, p051, p052, p053, p054, p055, p056, p057, p058, p059, p060, p061, p062, p063;
      |    byte p064, p065, p066, p067, p068, p069, p070, p071, p072, p073, p074, p075, p076, p077, p078, p079;
      |    byte p080, p081, p082, p083, p084, p085, p086, p087, p088, p089, p090, p091, p092, p093, p094, p095;
      |    byte p096, p097, p098, p099, p100, p101, p102, p103, p104, p105, p106, p107, p108, p109, p110, p111;
      |    byte p112, p113, p114, p115, p116, p117, p118, p119, p120, p121, p122, p123, p124, p125, p126, p127;
      |    byte p128, p129, p130, p131, p132, p133, p134, p135, p136, p137, p138, p139, p140, p141, p142, p143;
      |    byte p144, p145, p146, p147, p148, p149, p150, p151, p152, p153, p154, p155, p156, p157, p158, p159;
      |    byte p160, p161, p162, p163, p164, p165, p166, p167, p168, p169, p170, p171, p172, p173, p174, p175;
      |    byte p176, p177, p178, p179, p180, p181, p182, p183, p184, p185, p186, p187, p188, p189, p190, p191;
      |    byte p192, p193, p194, p195, p196, p197, p198, p199, p200, p201, p202, p203, p204, p205, p206, p207;
      |    byte p208, p209, p210, p211, p212, p213, p214, p215, p216, p217, p218, p219, p220, p221, p222, p223;
      |    byte p224, p225, p226, p227, p228, p229, p230, p231, p232, p233, p234, p235, p236, p237, p238, p239;
      |    byte p240, p241, p242, p243, p244, p245, p246, p247, p248, p249, p250, p251, p252, p253, p254, p255;
      |    int startRndMask;
      |    BenchmarkParams benchmarkParams;
      |    IterationParams iterationParams;
      |    ThreadParams threadParams;
      |    Blackhole blackhole;
      |    Control notifyControl;
      |
      |    public BenchmarkTaskResult plainGet_Throughput(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_performancebenchmarksuite0_G.plainGet(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            plainGet_thrpt_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, l_performancebenchmarksuite0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_performancebenchmarksuite0_G.plainGet(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps;
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            res.measuredOps /= batchSize;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new ThroughputResult(ResultRole.PRIMARY, "plainGet", res.measuredOps, res.getTime(), benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void plainGet_thrpt_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long operations = 0;
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        do {
      |            l_performancebenchmarksuite0_G.plainGet(blackhole);
      |            operations++;
      |        } while(!control.isDone);
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult plainGet_AverageTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_performancebenchmarksuite0_G.plainGet(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            plainGet_avgt_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, l_performancebenchmarksuite0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_performancebenchmarksuite0_G.plainGet(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps;
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            res.measuredOps /= batchSize;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new AverageTimeResult(ResultRole.PRIMARY, "plainGet", res.measuredOps, res.getTime(), benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void plainGet_avgt_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long operations = 0;
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        do {
      |            l_performancebenchmarksuite0_G.plainGet(blackhole);
      |            operations++;
      |        } while(!control.isDone);
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult plainGet_SampleTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_performancebenchmarksuite0_G.plainGet(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            int targetSamples = (int) (control.getDuration(TimeUnit.MILLISECONDS) * 20); // at max, 20 timestamps per millisecond
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            SampleBuffer buffer = new SampleBuffer();
      |            plainGet_sample_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, buffer, targetSamples, opsPerInv, batchSize, l_performancebenchmarksuite0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_performancebenchmarksuite0_G.plainGet(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps * batchSize;
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new SampleTimeResult(ResultRole.PRIMARY, "plainGet", buffer, benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void plainGet_sample_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, SampleBuffer buffer, int targetSamples, long opsPerInv, int batchSize, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long realTime = 0;
      |        long operations = 0;
      |        int rnd = (int)System.nanoTime();
      |        int rndMask = startRndMask;
      |        long time = 0;
      |        int currentStride = 0;
      |        do {
      |            rnd = (rnd * 1664525 + 1013904223);
      |            boolean sample = (rnd & rndMask) == 0;
      |            if (sample) {
      |                time = System.nanoTime();
      |            }
      |            for (int b = 0; b < batchSize; b++) {
      |                if (control.volatileSpoiler) return;
      |                l_performancebenchmarksuite0_G.plainGet(blackhole);
      |            }
      |            if (sample) {
      |                buffer.add((System.nanoTime() - time) / opsPerInv);
      |                if (currentStride++ > targetSamples) {
      |                    buffer.half();
      |                    currentStride = 0;
      |                    rndMask = (rndMask << 1) + 1;
      |                }
      |            }
      |            operations++;
      |        } while(!control.isDone);
      |        startRndMask = Math.max(startRndMask, rndMask);
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult plainGet_SingleShotTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            notifyControl.startMeasurement = true;
      |            RawResults res = new RawResults();
      |            int batchSize = iterationParams.getBatchSize();
      |            plainGet_ss_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, batchSize, l_performancebenchmarksuite0_G);
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            int opsPerInv = control.benchmarkParams.getOpsPerInvocation();
      |            long totalOps = opsPerInv;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult(totalOps, totalOps);
      |            results.add(new SingleShotResult(ResultRole.PRIMARY, "plainGet", res.getTime(), totalOps, benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void plainGet_ss_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, int batchSize, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        for (int b = 0; b < batchSize; b++) {
      |            if (control.volatileSpoiler) return;
      |            l_performancebenchmarksuite0_G.plainGet(blackhole);
      |        }
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |    }
      |
      |    
      |    static volatile PerformanceBenchmarkSuite_jmhType f_performancebenchmarksuite0_G;
      |    
      |    PerformanceBenchmarkSuite_jmhType _jmh_tryInit_f_performancebenchmarksuite0_G(InfraControl control) throws Throwable {
      |        PerformanceBenchmarkSuite_jmhType val = f_performancebenchmarksuite0_G;
      |        if (val != null) {
      |            return val;
      |        }
      |        synchronized(this.getClass()) {
      |            try {
      |            if (control.isFailing) throw new FailureAssistException();
      |            val = f_performancebenchmarksuite0_G;
      |            if (val != null) {
      |                return val;
      |            }
      |            val = new PerformanceBenchmarkSuite_jmhType();
      |            val.setup();
      |            val.readyTrial = true;
      |            f_performancebenchmarksuite0_G = val;
      |            } catch (Throwable t) {
      |                control.isFailing = true;
      |                throw t;
      |            }
      |        }
      |        return val;
      |    }
      |
      |
      |}
      |
      |""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|javalin-test""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|io/javalin/performance/jmh_generated/PerformanceBenchmarkSuite_postBody_jmhTest.java""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package io.javalin.performance.jmh_generated;
      |
      |import java.util.List;
      |import java.util.concurrent.atomic.AtomicInteger;
      |import java.util.Collection;
      |import java.util.ArrayList;
      |import java.util.concurrent.TimeUnit;
      |import org.openjdk.jmh.annotations.CompilerControl;
      |import org.openjdk.jmh.runner.InfraControl;
      |import org.openjdk.jmh.infra.ThreadParams;
      |import org.openjdk.jmh.results.BenchmarkTaskResult;
      |import org.openjdk.jmh.results.Result;
      |import org.openjdk.jmh.results.ThroughputResult;
      |import org.openjdk.jmh.results.AverageTimeResult;
      |import org.openjdk.jmh.results.SampleTimeResult;
      |import org.openjdk.jmh.results.SingleShotResult;
      |import org.openjdk.jmh.util.SampleBuffer;
      |import org.openjdk.jmh.annotations.Mode;
      |import org.openjdk.jmh.annotations.Fork;
      |import org.openjdk.jmh.annotations.Measurement;
      |import org.openjdk.jmh.annotations.Threads;
      |import org.openjdk.jmh.annotations.Warmup;
      |import org.openjdk.jmh.annotations.BenchmarkMode;
      |import org.openjdk.jmh.results.RawResults;
      |import org.openjdk.jmh.results.ResultRole;
      |import java.lang.reflect.Field;
      |import org.openjdk.jmh.infra.BenchmarkParams;
      |import org.openjdk.jmh.infra.IterationParams;
      |import org.openjdk.jmh.infra.Blackhole;
      |import org.openjdk.jmh.infra.Control;
      |import org.openjdk.jmh.results.ScalarResult;
      |import org.openjdk.jmh.results.AggregationPolicy;
      |import org.openjdk.jmh.runner.FailureAssistException;
      |
      |import io.javalin.performance.jmh_generated.PerformanceBenchmarkSuite_jmhType;
      |public final class PerformanceBenchmarkSuite_postBody_jmhTest {
      |
      |    byte p000, p001, p002, p003, p004, p005, p006, p007, p008, p009, p010, p011, p012, p013, p014, p015;
      |    byte p016, p017, p018, p019, p020, p021, p022, p023, p024, p025, p026, p027, p028, p029, p030, p031;
      |    byte p032, p033, p034, p035, p036, p037, p038, p039, p040, p041, p042, p043, p044, p045, p046, p047;
      |    byte p048, p049, p050, p051, p052, p053, p054, p055, p056, p057, p058, p059, p060, p061, p062, p063;
      |    byte p064, p065, p066, p067, p068, p069, p070, p071, p072, p073, p074, p075, p076, p077, p078, p079;
      |    byte p080, p081, p082, p083, p084, p085, p086, p087, p088, p089, p090, p091, p092, p093, p094, p095;
      |    byte p096, p097, p098, p099, p100, p101, p102, p103, p104, p105, p106, p107, p108, p109, p110, p111;
      |    byte p112, p113, p114, p115, p116, p117, p118, p119, p120, p121, p122, p123, p124, p125, p126, p127;
      |    byte p128, p129, p130, p131, p132, p133, p134, p135, p136, p137, p138, p139, p140, p141, p142, p143;
      |    byte p144, p145, p146, p147, p148, p149, p150, p151, p152, p153, p154, p155, p156, p157, p158, p159;
      |    byte p160, p161, p162, p163, p164, p165, p166, p167, p168, p169, p170, p171, p172, p173, p174, p175;
      |    byte p176, p177, p178, p179, p180, p181, p182, p183, p184, p185, p186, p187, p188, p189, p190, p191;
      |    byte p192, p193, p194, p195, p196, p197, p198, p199, p200, p201, p202, p203, p204, p205, p206, p207;
      |    byte p208, p209, p210, p211, p212, p213, p214, p215, p216, p217, p218, p219, p220, p221, p222, p223;
      |    byte p224, p225, p226, p227, p228, p229, p230, p231, p232, p233, p234, p235, p236, p237, p238, p239;
      |    byte p240, p241, p242, p243, p244, p245, p246, p247, p248, p249, p250, p251, p252, p253, p254, p255;
      |    int startRndMask;
      |    BenchmarkParams benchmarkParams;
      |    IterationParams iterationParams;
      |    ThreadParams threadParams;
      |    Blackhole blackhole;
      |    Control notifyControl;
      |
      |    public BenchmarkTaskResult postBody_Throughput(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_performancebenchmarksuite0_G.postBody(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            postBody_thrpt_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, l_performancebenchmarksuite0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_performancebenchmarksuite0_G.postBody(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps;
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            res.measuredOps /= batchSize;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new ThroughputResult(ResultRole.PRIMARY, "postBody", res.measuredOps, res.getTime(), benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void postBody_thrpt_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long operations = 0;
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        do {
      |            l_performancebenchmarksuite0_G.postBody(blackhole);
      |            operations++;
      |        } while(!control.isDone);
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult postBody_AverageTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_performancebenchmarksuite0_G.postBody(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            postBody_avgt_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, l_performancebenchmarksuite0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_performancebenchmarksuite0_G.postBody(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps;
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            res.measuredOps /= batchSize;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new AverageTimeResult(ResultRole.PRIMARY, "postBody", res.measuredOps, res.getTime(), benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void postBody_avgt_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long operations = 0;
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        do {
      |            l_performancebenchmarksuite0_G.postBody(blackhole);
      |            operations++;
      |        } while(!control.isDone);
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult postBody_SampleTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_performancebenchmarksuite0_G.postBody(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            int targetSamples = (int) (control.getDuration(TimeUnit.MILLISECONDS) * 20); // at max, 20 timestamps per millisecond
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            SampleBuffer buffer = new SampleBuffer();
      |            postBody_sample_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, buffer, targetSamples, opsPerInv, batchSize, l_performancebenchmarksuite0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_performancebenchmarksuite0_G.postBody(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps * batchSize;
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new SampleTimeResult(ResultRole.PRIMARY, "postBody", buffer, benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void postBody_sample_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, SampleBuffer buffer, int targetSamples, long opsPerInv, int batchSize, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long realTime = 0;
      |        long operations = 0;
      |        int rnd = (int)System.nanoTime();
      |        int rndMask = startRndMask;
      |        long time = 0;
      |        int currentStride = 0;
      |        do {
      |            rnd = (rnd * 1664525 + 1013904223);
      |            boolean sample = (rnd & rndMask) == 0;
      |            if (sample) {
      |                time = System.nanoTime();
      |            }
      |            for (int b = 0; b < batchSize; b++) {
      |                if (control.volatileSpoiler) return;
      |                l_performancebenchmarksuite0_G.postBody(blackhole);
      |            }
      |            if (sample) {
      |                buffer.add((System.nanoTime() - time) / opsPerInv);
      |                if (currentStride++ > targetSamples) {
      |                    buffer.half();
      |                    currentStride = 0;
      |                    rndMask = (rndMask << 1) + 1;
      |                }
      |            }
      |            operations++;
      |        } while(!control.isDone);
      |        startRndMask = Math.max(startRndMask, rndMask);
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult postBody_SingleShotTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            notifyControl.startMeasurement = true;
      |            RawResults res = new RawResults();
      |            int batchSize = iterationParams.getBatchSize();
      |            postBody_ss_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, batchSize, l_performancebenchmarksuite0_G);
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            int opsPerInv = control.benchmarkParams.getOpsPerInvocation();
      |            long totalOps = opsPerInv;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult(totalOps, totalOps);
      |            results.add(new SingleShotResult(ResultRole.PRIMARY, "postBody", res.getTime(), totalOps, benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void postBody_ss_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, int batchSize, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        for (int b = 0; b < batchSize; b++) {
      |            if (control.volatileSpoiler) return;
      |            l_performancebenchmarksuite0_G.postBody(blackhole);
      |        }
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |    }
      |
      |    
      |    static volatile PerformanceBenchmarkSuite_jmhType f_performancebenchmarksuite0_G;
      |    
      |    PerformanceBenchmarkSuite_jmhType _jmh_tryInit_f_performancebenchmarksuite0_G(InfraControl control) throws Throwable {
      |        PerformanceBenchmarkSuite_jmhType val = f_performancebenchmarksuite0_G;
      |        if (val != null) {
      |            return val;
      |        }
      |        synchronized(this.getClass()) {
      |            try {
      |            if (control.isFailing) throw new FailureAssistException();
      |            val = f_performancebenchmarksuite0_G;
      |            if (val != null) {
      |                return val;
      |            }
      |            val = new PerformanceBenchmarkSuite_jmhType();
      |            val.setup();
      |            val.readyTrial = true;
      |            f_performancebenchmarksuite0_G = val;
      |            } catch (Throwable t) {
      |                control.isFailing = true;
      |                throw t;
      |            }
      |        }
      |        return val;
      |    }
      |
      |
      |}
      |
      |""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|javalin-test""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|io/javalin/performance/jmh_generated/PerformanceBenchmarkSuite_singlePathParam_jmhTest.java""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package io.javalin.performance.jmh_generated;
      |
      |import java.util.List;
      |import java.util.concurrent.atomic.AtomicInteger;
      |import java.util.Collection;
      |import java.util.ArrayList;
      |import java.util.concurrent.TimeUnit;
      |import org.openjdk.jmh.annotations.CompilerControl;
      |import org.openjdk.jmh.runner.InfraControl;
      |import org.openjdk.jmh.infra.ThreadParams;
      |import org.openjdk.jmh.results.BenchmarkTaskResult;
      |import org.openjdk.jmh.results.Result;
      |import org.openjdk.jmh.results.ThroughputResult;
      |import org.openjdk.jmh.results.AverageTimeResult;
      |import org.openjdk.jmh.results.SampleTimeResult;
      |import org.openjdk.jmh.results.SingleShotResult;
      |import org.openjdk.jmh.util.SampleBuffer;
      |import org.openjdk.jmh.annotations.Mode;
      |import org.openjdk.jmh.annotations.Fork;
      |import org.openjdk.jmh.annotations.Measurement;
      |import org.openjdk.jmh.annotations.Threads;
      |import org.openjdk.jmh.annotations.Warmup;
      |import org.openjdk.jmh.annotations.BenchmarkMode;
      |import org.openjdk.jmh.results.RawResults;
      |import org.openjdk.jmh.results.ResultRole;
      |import java.lang.reflect.Field;
      |import org.openjdk.jmh.infra.BenchmarkParams;
      |import org.openjdk.jmh.infra.IterationParams;
      |import org.openjdk.jmh.infra.Blackhole;
      |import org.openjdk.jmh.infra.Control;
      |import org.openjdk.jmh.results.ScalarResult;
      |import org.openjdk.jmh.results.AggregationPolicy;
      |import org.openjdk.jmh.runner.FailureAssistException;
      |
      |import io.javalin.performance.jmh_generated.PerformanceBenchmarkSuite_jmhType;
      |public final class PerformanceBenchmarkSuite_singlePathParam_jmhTest {
      |
      |    byte p000, p001, p002, p003, p004, p005, p006, p007, p008, p009, p010, p011, p012, p013, p014, p015;
      |    byte p016, p017, p018, p019, p020, p021, p022, p023, p024, p025, p026, p027, p028, p029, p030, p031;
      |    byte p032, p033, p034, p035, p036, p037, p038, p039, p040, p041, p042, p043, p044, p045, p046, p047;
      |    byte p048, p049, p050, p051, p052, p053, p054, p055, p056, p057, p058, p059, p060, p061, p062, p063;
      |    byte p064, p065, p066, p067, p068, p069, p070, p071, p072, p073, p074, p075, p076, p077, p078, p079;
      |    byte p080, p081, p082, p083, p084, p085, p086, p087, p088, p089, p090, p091, p092, p093, p094, p095;
      |    byte p096, p097, p098, p099, p100, p101, p102, p103, p104, p105, p106, p107, p108, p109, p110, p111;
      |    byte p112, p113, p114, p115, p116, p117, p118, p119, p120, p121, p122, p123, p124, p125, p126, p127;
      |    byte p128, p129, p130, p131, p132, p133, p134, p135, p136, p137, p138, p139, p140, p141, p142, p143;
      |    byte p144, p145, p146, p147, p148, p149, p150, p151, p152, p153, p154, p155, p156, p157, p158, p159;
      |    byte p160, p161, p162, p163, p164, p165, p166, p167, p168, p169, p170, p171, p172, p173, p174, p175;
      |    byte p176, p177, p178, p179, p180, p181, p182, p183, p184, p185, p186, p187, p188, p189, p190, p191;
      |    byte p192, p193, p194, p195, p196, p197, p198, p199, p200, p201, p202, p203, p204, p205, p206, p207;
      |    byte p208, p209, p210, p211, p212, p213, p214, p215, p216, p217, p218, p219, p220, p221, p222, p223;
      |    byte p224, p225, p226, p227, p228, p229, p230, p231, p232, p233, p234, p235, p236, p237, p238, p239;
      |    byte p240, p241, p242, p243, p244, p245, p246, p247, p248, p249, p250, p251, p252, p253, p254, p255;
      |    int startRndMask;
      |    BenchmarkParams benchmarkParams;
      |    IterationParams iterationParams;
      |    ThreadParams threadParams;
      |    Blackhole blackhole;
      |    Control notifyControl;
      |
      |    public BenchmarkTaskResult singlePathParam_Throughput(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_performancebenchmarksuite0_G.singlePathParam(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            singlePathParam_thrpt_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, l_performancebenchmarksuite0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_performancebenchmarksuite0_G.singlePathParam(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps;
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            res.measuredOps /= batchSize;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new ThroughputResult(ResultRole.PRIMARY, "singlePathParam", res.measuredOps, res.getTime(), benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void singlePathParam_thrpt_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long operations = 0;
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        do {
      |            l_performancebenchmarksuite0_G.singlePathParam(blackhole);
      |            operations++;
      |        } while(!control.isDone);
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult singlePathParam_AverageTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_performancebenchmarksuite0_G.singlePathParam(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            singlePathParam_avgt_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, l_performancebenchmarksuite0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_performancebenchmarksuite0_G.singlePathParam(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps;
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            res.measuredOps /= batchSize;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new AverageTimeResult(ResultRole.PRIMARY, "singlePathParam", res.measuredOps, res.getTime(), benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void singlePathParam_avgt_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long operations = 0;
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        do {
      |            l_performancebenchmarksuite0_G.singlePathParam(blackhole);
      |            operations++;
      |        } while(!control.isDone);
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult singlePathParam_SampleTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_performancebenchmarksuite0_G.singlePathParam(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            int targetSamples = (int) (control.getDuration(TimeUnit.MILLISECONDS) * 20); // at max, 20 timestamps per millisecond
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            SampleBuffer buffer = new SampleBuffer();
      |            singlePathParam_sample_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, buffer, targetSamples, opsPerInv, batchSize, l_performancebenchmarksuite0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_performancebenchmarksuite0_G.singlePathParam(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps * batchSize;
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new SampleTimeResult(ResultRole.PRIMARY, "singlePathParam", buffer, benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void singlePathParam_sample_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, SampleBuffer buffer, int targetSamples, long opsPerInv, int batchSize, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long realTime = 0;
      |        long operations = 0;
      |        int rnd = (int)System.nanoTime();
      |        int rndMask = startRndMask;
      |        long time = 0;
      |        int currentStride = 0;
      |        do {
      |            rnd = (rnd * 1664525 + 1013904223);
      |            boolean sample = (rnd & rndMask) == 0;
      |            if (sample) {
      |                time = System.nanoTime();
      |            }
      |            for (int b = 0; b < batchSize; b++) {
      |                if (control.volatileSpoiler) return;
      |                l_performancebenchmarksuite0_G.singlePathParam(blackhole);
      |            }
      |            if (sample) {
      |                buffer.add((System.nanoTime() - time) / opsPerInv);
      |                if (currentStride++ > targetSamples) {
      |                    buffer.half();
      |                    currentStride = 0;
      |                    rndMask = (rndMask << 1) + 1;
      |                }
      |            }
      |            operations++;
      |        } while(!control.isDone);
      |        startRndMask = Math.max(startRndMask, rndMask);
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult singlePathParam_SingleShotTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            notifyControl.startMeasurement = true;
      |            RawResults res = new RawResults();
      |            int batchSize = iterationParams.getBatchSize();
      |            singlePathParam_ss_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, batchSize, l_performancebenchmarksuite0_G);
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            int opsPerInv = control.benchmarkParams.getOpsPerInvocation();
      |            long totalOps = opsPerInv;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult(totalOps, totalOps);
      |            results.add(new SingleShotResult(ResultRole.PRIMARY, "singlePathParam", res.getTime(), totalOps, benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void singlePathParam_ss_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, int batchSize, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        for (int b = 0; b < batchSize; b++) {
      |            if (control.volatileSpoiler) return;
      |            l_performancebenchmarksuite0_G.singlePathParam(blackhole);
      |        }
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |    }
      |
      |    
      |    static volatile PerformanceBenchmarkSuite_jmhType f_performancebenchmarksuite0_G;
      |    
      |    PerformanceBenchmarkSuite_jmhType _jmh_tryInit_f_performancebenchmarksuite0_G(InfraControl control) throws Throwable {
      |        PerformanceBenchmarkSuite_jmhType val = f_performancebenchmarksuite0_G;
      |        if (val != null) {
      |            return val;
      |        }
      |        synchronized(this.getClass()) {
      |            try {
      |            if (control.isFailing) throw new FailureAssistException();
      |            val = f_performancebenchmarksuite0_G;
      |            if (val != null) {
      |                return val;
      |            }
      |            val = new PerformanceBenchmarkSuite_jmhType();
      |            val.setup();
      |            val.readyTrial = true;
      |            f_performancebenchmarksuite0_G = val;
      |            } catch (Throwable t) {
      |                control.isFailing = true;
      |                throw t;
      |            }
      |        }
      |        return val;
      |    }
      |
      |
      |}
      |
      |""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|javalin-test""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|io/javalin/performance/jmh_generated/PerformanceBenchmarkSuite_staticFile_jmhTest.java""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package io.javalin.performance.jmh_generated;
      |
      |import java.util.List;
      |import java.util.concurrent.atomic.AtomicInteger;
      |import java.util.Collection;
      |import java.util.ArrayList;
      |import java.util.concurrent.TimeUnit;
      |import org.openjdk.jmh.annotations.CompilerControl;
      |import org.openjdk.jmh.runner.InfraControl;
      |import org.openjdk.jmh.infra.ThreadParams;
      |import org.openjdk.jmh.results.BenchmarkTaskResult;
      |import org.openjdk.jmh.results.Result;
      |import org.openjdk.jmh.results.ThroughputResult;
      |import org.openjdk.jmh.results.AverageTimeResult;
      |import org.openjdk.jmh.results.SampleTimeResult;
      |import org.openjdk.jmh.results.SingleShotResult;
      |import org.openjdk.jmh.util.SampleBuffer;
      |import org.openjdk.jmh.annotations.Mode;
      |import org.openjdk.jmh.annotations.Fork;
      |import org.openjdk.jmh.annotations.Measurement;
      |import org.openjdk.jmh.annotations.Threads;
      |import org.openjdk.jmh.annotations.Warmup;
      |import org.openjdk.jmh.annotations.BenchmarkMode;
      |import org.openjdk.jmh.results.RawResults;
      |import org.openjdk.jmh.results.ResultRole;
      |import java.lang.reflect.Field;
      |import org.openjdk.jmh.infra.BenchmarkParams;
      |import org.openjdk.jmh.infra.IterationParams;
      |import org.openjdk.jmh.infra.Blackhole;
      |import org.openjdk.jmh.infra.Control;
      |import org.openjdk.jmh.results.ScalarResult;
      |import org.openjdk.jmh.results.AggregationPolicy;
      |import org.openjdk.jmh.runner.FailureAssistException;
      |
      |import io.javalin.performance.jmh_generated.PerformanceBenchmarkSuite_jmhType;
      |public final class PerformanceBenchmarkSuite_staticFile_jmhTest {
      |
      |    byte p000, p001, p002, p003, p004, p005, p006, p007, p008, p009, p010, p011, p012, p013, p014, p015;
      |    byte p016, p017, p018, p019, p020, p021, p022, p023, p024, p025, p026, p027, p028, p029, p030, p031;
      |    byte p032, p033, p034, p035, p036, p037, p038, p039, p040, p041, p042, p043, p044, p045, p046, p047;
      |    byte p048, p049, p050, p051, p052, p053, p054, p055, p056, p057, p058, p059, p060, p061, p062, p063;
      |    byte p064, p065, p066, p067, p068, p069, p070, p071, p072, p073, p074, p075, p076, p077, p078, p079;
      |    byte p080, p081, p082, p083, p084, p085, p086, p087, p088, p089, p090, p091, p092, p093, p094, p095;
      |    byte p096, p097, p098, p099, p100, p101, p102, p103, p104, p105, p106, p107, p108, p109, p110, p111;
      |    byte p112, p113, p114, p115, p116, p117, p118, p119, p120, p121, p122, p123, p124, p125, p126, p127;
      |    byte p128, p129, p130, p131, p132, p133, p134, p135, p136, p137, p138, p139, p140, p141, p142, p143;
      |    byte p144, p145, p146, p147, p148, p149, p150, p151, p152, p153, p154, p155, p156, p157, p158, p159;
      |    byte p160, p161, p162, p163, p164, p165, p166, p167, p168, p169, p170, p171, p172, p173, p174, p175;
      |    byte p176, p177, p178, p179, p180, p181, p182, p183, p184, p185, p186, p187, p188, p189, p190, p191;
      |    byte p192, p193, p194, p195, p196, p197, p198, p199, p200, p201, p202, p203, p204, p205, p206, p207;
      |    byte p208, p209, p210, p211, p212, p213, p214, p215, p216, p217, p218, p219, p220, p221, p222, p223;
      |    byte p224, p225, p226, p227, p228, p229, p230, p231, p232, p233, p234, p235, p236, p237, p238, p239;
      |    byte p240, p241, p242, p243, p244, p245, p246, p247, p248, p249, p250, p251, p252, p253, p254, p255;
      |    int startRndMask;
      |    BenchmarkParams benchmarkParams;
      |    IterationParams iterationParams;
      |    ThreadParams threadParams;
      |    Blackhole blackhole;
      |    Control notifyControl;
      |
      |    public BenchmarkTaskResult staticFile_Throughput(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_performancebenchmarksuite0_G.staticFile(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            staticFile_thrpt_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, l_performancebenchmarksuite0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_performancebenchmarksuite0_G.staticFile(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps;
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            res.measuredOps /= batchSize;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new ThroughputResult(ResultRole.PRIMARY, "staticFile", res.measuredOps, res.getTime(), benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void staticFile_thrpt_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long operations = 0;
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        do {
      |            l_performancebenchmarksuite0_G.staticFile(blackhole);
      |            operations++;
      |        } while(!control.isDone);
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult staticFile_AverageTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_performancebenchmarksuite0_G.staticFile(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            staticFile_avgt_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, l_performancebenchmarksuite0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_performancebenchmarksuite0_G.staticFile(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps;
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            res.measuredOps /= batchSize;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new AverageTimeResult(ResultRole.PRIMARY, "staticFile", res.measuredOps, res.getTime(), benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void staticFile_avgt_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long operations = 0;
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        do {
      |            l_performancebenchmarksuite0_G.staticFile(blackhole);
      |            operations++;
      |        } while(!control.isDone);
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult staticFile_SampleTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_performancebenchmarksuite0_G.staticFile(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            int targetSamples = (int) (control.getDuration(TimeUnit.MILLISECONDS) * 20); // at max, 20 timestamps per millisecond
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            SampleBuffer buffer = new SampleBuffer();
      |            staticFile_sample_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, buffer, targetSamples, opsPerInv, batchSize, l_performancebenchmarksuite0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_performancebenchmarksuite0_G.staticFile(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps * batchSize;
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new SampleTimeResult(ResultRole.PRIMARY, "staticFile", buffer, benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void staticFile_sample_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, SampleBuffer buffer, int targetSamples, long opsPerInv, int batchSize, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long realTime = 0;
      |        long operations = 0;
      |        int rnd = (int)System.nanoTime();
      |        int rndMask = startRndMask;
      |        long time = 0;
      |        int currentStride = 0;
      |        do {
      |            rnd = (rnd * 1664525 + 1013904223);
      |            boolean sample = (rnd & rndMask) == 0;
      |            if (sample) {
      |                time = System.nanoTime();
      |            }
      |            for (int b = 0; b < batchSize; b++) {
      |                if (control.volatileSpoiler) return;
      |                l_performancebenchmarksuite0_G.staticFile(blackhole);
      |            }
      |            if (sample) {
      |                buffer.add((System.nanoTime() - time) / opsPerInv);
      |                if (currentStride++ > targetSamples) {
      |                    buffer.half();
      |                    currentStride = 0;
      |                    rndMask = (rndMask << 1) + 1;
      |                }
      |            }
      |            operations++;
      |        } while(!control.isDone);
      |        startRndMask = Math.max(startRndMask, rndMask);
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult staticFile_SingleShotTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            notifyControl.startMeasurement = true;
      |            RawResults res = new RawResults();
      |            int batchSize = iterationParams.getBatchSize();
      |            staticFile_ss_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, batchSize, l_performancebenchmarksuite0_G);
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            int opsPerInv = control.benchmarkParams.getOpsPerInvocation();
      |            long totalOps = opsPerInv;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult(totalOps, totalOps);
      |            results.add(new SingleShotResult(ResultRole.PRIMARY, "staticFile", res.getTime(), totalOps, benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void staticFile_ss_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, int batchSize, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        for (int b = 0; b < batchSize; b++) {
      |            if (control.volatileSpoiler) return;
      |            l_performancebenchmarksuite0_G.staticFile(blackhole);
      |        }
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |    }
      |
      |    
      |    static volatile PerformanceBenchmarkSuite_jmhType f_performancebenchmarksuite0_G;
      |    
      |    PerformanceBenchmarkSuite_jmhType _jmh_tryInit_f_performancebenchmarksuite0_G(InfraControl control) throws Throwable {
      |        PerformanceBenchmarkSuite_jmhType val = f_performancebenchmarksuite0_G;
      |        if (val != null) {
      |            return val;
      |        }
      |        synchronized(this.getClass()) {
      |            try {
      |            if (control.isFailing) throw new FailureAssistException();
      |            val = f_performancebenchmarksuite0_G;
      |            if (val != null) {
      |                return val;
      |            }
      |            val = new PerformanceBenchmarkSuite_jmhType();
      |            val.setup();
      |            val.readyTrial = true;
      |            f_performancebenchmarksuite0_G = val;
      |            } catch (Throwable t) {
      |                control.isFailing = true;
      |                throw t;
      |            }
      |        }
      |        return val;
      |    }
      |
      |
      |}
      |
      |""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }



    targets.foreach { target =>
      if (Set(s"""|javalin-test""".stripMargin).contains(target.project.value)) {
        val to = target.sources.resolve(s"""|io/javalin/performance/jmh_generated/PerformanceBenchmarkSuite_staticFile_miss_jmhTest.java""".stripMargin)
        started.logger.withContext("project", target.project.value).warn(s"Writing $to")
        val content = s"""|package io.javalin.performance.jmh_generated;
      |
      |import java.util.List;
      |import java.util.concurrent.atomic.AtomicInteger;
      |import java.util.Collection;
      |import java.util.ArrayList;
      |import java.util.concurrent.TimeUnit;
      |import org.openjdk.jmh.annotations.CompilerControl;
      |import org.openjdk.jmh.runner.InfraControl;
      |import org.openjdk.jmh.infra.ThreadParams;
      |import org.openjdk.jmh.results.BenchmarkTaskResult;
      |import org.openjdk.jmh.results.Result;
      |import org.openjdk.jmh.results.ThroughputResult;
      |import org.openjdk.jmh.results.AverageTimeResult;
      |import org.openjdk.jmh.results.SampleTimeResult;
      |import org.openjdk.jmh.results.SingleShotResult;
      |import org.openjdk.jmh.util.SampleBuffer;
      |import org.openjdk.jmh.annotations.Mode;
      |import org.openjdk.jmh.annotations.Fork;
      |import org.openjdk.jmh.annotations.Measurement;
      |import org.openjdk.jmh.annotations.Threads;
      |import org.openjdk.jmh.annotations.Warmup;
      |import org.openjdk.jmh.annotations.BenchmarkMode;
      |import org.openjdk.jmh.results.RawResults;
      |import org.openjdk.jmh.results.ResultRole;
      |import java.lang.reflect.Field;
      |import org.openjdk.jmh.infra.BenchmarkParams;
      |import org.openjdk.jmh.infra.IterationParams;
      |import org.openjdk.jmh.infra.Blackhole;
      |import org.openjdk.jmh.infra.Control;
      |import org.openjdk.jmh.results.ScalarResult;
      |import org.openjdk.jmh.results.AggregationPolicy;
      |import org.openjdk.jmh.runner.FailureAssistException;
      |
      |import io.javalin.performance.jmh_generated.PerformanceBenchmarkSuite_jmhType;
      |public final class PerformanceBenchmarkSuite_staticFile_miss_jmhTest {
      |
      |    byte p000, p001, p002, p003, p004, p005, p006, p007, p008, p009, p010, p011, p012, p013, p014, p015;
      |    byte p016, p017, p018, p019, p020, p021, p022, p023, p024, p025, p026, p027, p028, p029, p030, p031;
      |    byte p032, p033, p034, p035, p036, p037, p038, p039, p040, p041, p042, p043, p044, p045, p046, p047;
      |    byte p048, p049, p050, p051, p052, p053, p054, p055, p056, p057, p058, p059, p060, p061, p062, p063;
      |    byte p064, p065, p066, p067, p068, p069, p070, p071, p072, p073, p074, p075, p076, p077, p078, p079;
      |    byte p080, p081, p082, p083, p084, p085, p086, p087, p088, p089, p090, p091, p092, p093, p094, p095;
      |    byte p096, p097, p098, p099, p100, p101, p102, p103, p104, p105, p106, p107, p108, p109, p110, p111;
      |    byte p112, p113, p114, p115, p116, p117, p118, p119, p120, p121, p122, p123, p124, p125, p126, p127;
      |    byte p128, p129, p130, p131, p132, p133, p134, p135, p136, p137, p138, p139, p140, p141, p142, p143;
      |    byte p144, p145, p146, p147, p148, p149, p150, p151, p152, p153, p154, p155, p156, p157, p158, p159;
      |    byte p160, p161, p162, p163, p164, p165, p166, p167, p168, p169, p170, p171, p172, p173, p174, p175;
      |    byte p176, p177, p178, p179, p180, p181, p182, p183, p184, p185, p186, p187, p188, p189, p190, p191;
      |    byte p192, p193, p194, p195, p196, p197, p198, p199, p200, p201, p202, p203, p204, p205, p206, p207;
      |    byte p208, p209, p210, p211, p212, p213, p214, p215, p216, p217, p218, p219, p220, p221, p222, p223;
      |    byte p224, p225, p226, p227, p228, p229, p230, p231, p232, p233, p234, p235, p236, p237, p238, p239;
      |    byte p240, p241, p242, p243, p244, p245, p246, p247, p248, p249, p250, p251, p252, p253, p254, p255;
      |    int startRndMask;
      |    BenchmarkParams benchmarkParams;
      |    IterationParams iterationParams;
      |    ThreadParams threadParams;
      |    Blackhole blackhole;
      |    Control notifyControl;
      |
      |    public BenchmarkTaskResult staticFile_miss_Throughput(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_performancebenchmarksuite0_G.staticFile_miss(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            staticFile_miss_thrpt_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, l_performancebenchmarksuite0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_performancebenchmarksuite0_G.staticFile_miss(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps;
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            res.measuredOps /= batchSize;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new ThroughputResult(ResultRole.PRIMARY, "staticFile_miss", res.measuredOps, res.getTime(), benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void staticFile_miss_thrpt_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long operations = 0;
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        do {
      |            l_performancebenchmarksuite0_G.staticFile_miss(blackhole);
      |            operations++;
      |        } while(!control.isDone);
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult staticFile_miss_AverageTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_performancebenchmarksuite0_G.staticFile_miss(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            staticFile_miss_avgt_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, l_performancebenchmarksuite0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_performancebenchmarksuite0_G.staticFile_miss(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps;
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            res.measuredOps /= batchSize;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new AverageTimeResult(ResultRole.PRIMARY, "staticFile_miss", res.measuredOps, res.getTime(), benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void staticFile_miss_avgt_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long operations = 0;
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        do {
      |            l_performancebenchmarksuite0_G.staticFile_miss(blackhole);
      |            operations++;
      |        } while(!control.isDone);
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult staticFile_miss_SampleTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            RawResults res = new RawResults();
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            control.announceWarmupReady();
      |            while (control.warmupShouldWait) {
      |                l_performancebenchmarksuite0_G.staticFile_miss(blackhole);
      |                if (control.shouldYield) Thread.yield();
      |                res.allOps++;
      |            }
      |
      |            notifyControl.startMeasurement = true;
      |            int targetSamples = (int) (control.getDuration(TimeUnit.MILLISECONDS) * 20); // at max, 20 timestamps per millisecond
      |            int batchSize = iterationParams.getBatchSize();
      |            int opsPerInv = benchmarkParams.getOpsPerInvocation();
      |            SampleBuffer buffer = new SampleBuffer();
      |            staticFile_miss_sample_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, buffer, targetSamples, opsPerInv, batchSize, l_performancebenchmarksuite0_G);
      |            notifyControl.stopMeasurement = true;
      |            control.announceWarmdownReady();
      |            try {
      |                while (control.warmdownShouldWait) {
      |                    l_performancebenchmarksuite0_G.staticFile_miss(blackhole);
      |                    if (control.shouldYield) Thread.yield();
      |                    res.allOps++;
      |                }
      |            } catch (Throwable e) {
      |                if (!(e instanceof InterruptedException)) throw e;
      |            }
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            res.allOps += res.measuredOps * batchSize;
      |            res.allOps *= opsPerInv;
      |            res.allOps /= batchSize;
      |            res.measuredOps *= opsPerInv;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult((long)res.allOps, (long)res.measuredOps);
      |            results.add(new SampleTimeResult(ResultRole.PRIMARY, "staticFile_miss", buffer, benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void staticFile_miss_sample_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, SampleBuffer buffer, int targetSamples, long opsPerInv, int batchSize, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long realTime = 0;
      |        long operations = 0;
      |        int rnd = (int)System.nanoTime();
      |        int rndMask = startRndMask;
      |        long time = 0;
      |        int currentStride = 0;
      |        do {
      |            rnd = (rnd * 1664525 + 1013904223);
      |            boolean sample = (rnd & rndMask) == 0;
      |            if (sample) {
      |                time = System.nanoTime();
      |            }
      |            for (int b = 0; b < batchSize; b++) {
      |                if (control.volatileSpoiler) return;
      |                l_performancebenchmarksuite0_G.staticFile_miss(blackhole);
      |            }
      |            if (sample) {
      |                buffer.add((System.nanoTime() - time) / opsPerInv);
      |                if (currentStride++ > targetSamples) {
      |                    buffer.half();
      |                    currentStride = 0;
      |                    rndMask = (rndMask << 1) + 1;
      |                }
      |            }
      |            operations++;
      |        } while(!control.isDone);
      |        startRndMask = Math.max(startRndMask, rndMask);
      |        result.realTime = realTime;
      |        result.measuredOps = operations;
      |    }
      |
      |
      |    public BenchmarkTaskResult staticFile_miss_SingleShotTime(InfraControl control, ThreadParams threadParams) throws Throwable {
      |        this.benchmarkParams = control.benchmarkParams;
      |        this.iterationParams = control.iterationParams;
      |        this.threadParams    = threadParams;
      |        this.notifyControl   = control.notifyControl;
      |        if (this.blackhole == null) {
      |            this.blackhole = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.");
      |        }
      |        if (threadParams.getSubgroupIndex() == 0) {
      |            PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G = _jmh_tryInit_f_performancebenchmarksuite0_G(control);
      |
      |            control.preSetup();
      |
      |
      |            notifyControl.startMeasurement = true;
      |            RawResults res = new RawResults();
      |            int batchSize = iterationParams.getBatchSize();
      |            staticFile_miss_ss_jmhStub(control, res, benchmarkParams, iterationParams, threadParams, blackhole, notifyControl, startRndMask, batchSize, l_performancebenchmarksuite0_G);
      |            control.preTearDown();
      |
      |            if (control.isLastIteration()) {
      |                if (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.compareAndSet(l_performancebenchmarksuite0_G, 0, 1)) {
      |                    try {
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (l_performancebenchmarksuite0_G.readyTrial) {
      |                            l_performancebenchmarksuite0_G.tearDown();
      |                            l_performancebenchmarksuite0_G.readyTrial = false;
      |                        }
      |                    } catch (Throwable t) {
      |                        control.isFailing = true;
      |                        throw t;
      |                    } finally {
      |                        PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.set(l_performancebenchmarksuite0_G, 0);
      |                    }
      |                } else {
      |                    long l_performancebenchmarksuite0_G_backoff = 1;
      |                    while (PerformanceBenchmarkSuite_jmhType.tearTrialMutexUpdater.get(l_performancebenchmarksuite0_G) == 1) {
      |                        TimeUnit.MILLISECONDS.sleep(l_performancebenchmarksuite0_G_backoff);
      |                        l_performancebenchmarksuite0_G_backoff = Math.max(1024, l_performancebenchmarksuite0_G_backoff * 2);
      |                        if (control.isFailing) throw new FailureAssistException();
      |                        if (Thread.interrupted()) throw new InterruptedException();
      |                    }
      |                }
      |                synchronized(this.getClass()) {
      |                    f_performancebenchmarksuite0_G = null;
      |                }
      |            }
      |            int opsPerInv = control.benchmarkParams.getOpsPerInvocation();
      |            long totalOps = opsPerInv;
      |            BenchmarkTaskResult results = new BenchmarkTaskResult(totalOps, totalOps);
      |            results.add(new SingleShotResult(ResultRole.PRIMARY, "staticFile_miss", res.getTime(), totalOps, benchmarkParams.getTimeUnit()));
      |            this.blackhole.evaporate("Yes, I am Stephen Hawking, and know a thing or two about black holes.");
      |            return results;
      |        } else
      |            throw new IllegalStateException("Harness failed to distribute threads among groups properly");
      |    }
      |
      |    public static void staticFile_miss_ss_jmhStub(InfraControl control, RawResults result, BenchmarkParams benchmarkParams, IterationParams iterationParams, ThreadParams threadParams, Blackhole blackhole, Control notifyControl, int startRndMask, int batchSize, PerformanceBenchmarkSuite_jmhType l_performancebenchmarksuite0_G) throws Throwable {
      |        long realTime = 0;
      |        result.startTime = System.nanoTime();
      |        for (int b = 0; b < batchSize; b++) {
      |            if (control.volatileSpoiler) return;
      |            l_performancebenchmarksuite0_G.staticFile_miss(blackhole);
      |        }
      |        result.stopTime = System.nanoTime();
      |        result.realTime = realTime;
      |    }
      |
      |    
      |    static volatile PerformanceBenchmarkSuite_jmhType f_performancebenchmarksuite0_G;
      |    
      |    PerformanceBenchmarkSuite_jmhType _jmh_tryInit_f_performancebenchmarksuite0_G(InfraControl control) throws Throwable {
      |        PerformanceBenchmarkSuite_jmhType val = f_performancebenchmarksuite0_G;
      |        if (val != null) {
      |            return val;
      |        }
      |        synchronized(this.getClass()) {
      |            try {
      |            if (control.isFailing) throw new FailureAssistException();
      |            val = f_performancebenchmarksuite0_G;
      |            if (val != null) {
      |                return val;
      |            }
      |            val = new PerformanceBenchmarkSuite_jmhType();
      |            val.setup();
      |            val.readyTrial = true;
      |            f_performancebenchmarksuite0_G = val;
      |            } catch (Throwable t) {
      |                control.isFailing = true;
      |                throw t;
      |            }
      |        }
      |        return val;
      |    }
      |
      |
      |}
      |
      |""".stripMargin
        Files.createDirectories(to.getParent)
        Files.writeString(to, content)
      }
    }

  }
}