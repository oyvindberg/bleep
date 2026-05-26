package bleep.test.ksp

@Target(AnnotationTarget.CLASS)
@Retention(AnnotationRetention.SOURCE)
annotation class GenerateKotlin(val suffix: String)

@Target(AnnotationTarget.CLASS)
@Retention(AnnotationRetention.SOURCE)
annotation class GenerateJava(val suffix: String)

@Target(AnnotationTarget.CLASS) @Retention(AnnotationRetention.SOURCE) annotation class ThrowOnMe
