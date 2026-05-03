package bleepscript;

public final class ManifestCreators {
  private ManifestCreators() {}

  public static ManifestCreator defaultCreator() {
    return BleepscriptServices.Holder.INSTANCE.defaultManifestCreator();
  }
}
