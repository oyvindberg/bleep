package bleepscript;

/**
 * What a test run did, returned by {@link Commands#test}.
 *
 * <p>A failing test throws, so this describes a run in which everything passed. There is
 * deliberately no failure list on it: by the time a caller could read one, the throw has already
 * happened. Counts are what remain useful — chiefly for asserting that a run was not silently
 * empty.
 */
public record TestReport(int total, int passed, int skipped, int ignored, int suites) {}
