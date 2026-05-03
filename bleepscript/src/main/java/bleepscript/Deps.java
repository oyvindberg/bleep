package bleepscript;

import java.util.Objects;

public final class Deps {
  private Deps() {}

  public static Dep parse(String coordinates) {
    Objects.requireNonNull(coordinates, "coordinates");
    return BleepscriptServices.Holder.INSTANCE.parseDep(coordinates);
  }

  public static Dep.Java javaDep(String organization, String moduleName, String version) {
    return new Dep.Java(organization, moduleName, version, true);
  }

  public static Dep.Scala scalaDep(String organization, String moduleName, String version) {
    return new Dep.Scala(organization, moduleName, version, true, false, false, false, false);
  }

  public static Dep.Scala scalaFullVersionDep(
      String organization, String moduleName, String version) {
    return new Dep.Scala(organization, moduleName, version, true, true, false, false, false);
  }
}
