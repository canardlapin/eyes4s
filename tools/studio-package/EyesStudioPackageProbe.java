/* Copyright 2026 canardlapin. Licensed under the Apache License, Version 2.0. */
import java.awt.image.BufferedImage;
import java.lang.instrument.Instrumentation;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.*;
import javax.imageio.ImageIO;
import javafx.application.Platform;
import javafx.scene.control.MenuBar;
import javafx.scene.control.ListView;
import javafx.scene.image.PixelFormat;
import javafx.stage.Stage;
import javafx.stage.Window;
import javafx.stage.WindowEvent;

/** Test-only observer injected into the packaged launcher. Never shipped in the app. */
public final class EyesStudioPackageProbe {
  private static Path output;
  private static String expected;
  private static long matchedAt;
  private static boolean finished;
  private static final Set<Integer> colours = new HashSet<>();

  public static void premain(String directory, Instrumentation instrumentation) throws Exception {
    output = Path.of(directory);
    expected = Files.readString(output.resolve("expected-title.txt")).trim();
    var scheduler = Executors.newSingleThreadScheduledExecutor(r -> {
      var thread = new Thread(r, "packaged-smoke-observer");
      thread.setDaemon(true);
      return thread;
    });
    scheduler.scheduleWithFixedDelay(() -> {
      try { Platform.runLater(EyesStudioPackageProbe::observe); }
      catch (IllegalStateException notStarted) { /* JavaFX has not started yet. */ }
    }, 0, 200, TimeUnit.MILLISECONDS);
  }

  private static String quote(String s) {
    return "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n") + "\"";
  }

  private static void observe() {
    if (finished) return;
    try {
      for (var window : List.copyOf(Window.getWindows())) {
        if (!(window instanceof Stage stage) || !stage.isShowing() || stage.getScene() == null) continue;
        if (!Files.exists(output.resolve("ready.json"))) {
          Files.writeString(output.resolve("ready.json"), "{\"title\":" + quote(stage.getTitle()) + "}\n");
        }
        if (!stage.getTitle().contains(expected)) continue;
        var participants = stage.getScene().getRoot().lookup(".participants-list");
        // A title and nonblank scene can also describe an error page. Require
        // actual backend results before accepting the bundled example.
        if (!(participants instanceof ListView<?> rows) || rows.getItems().isEmpty()) continue;
        if (matchedAt == 0) matchedAt = System.nanoTime();
        if (System.nanoTime() - matchedAt < TimeUnit.SECONDS.toNanos(5)) continue;
        var scene = stage.getScene();
        var root = scene.getRoot();
        root.applyCss();
        root.layout();
        var image = scene.snapshot(null);
        int width = (int)image.getWidth(), height = (int)image.getHeight();
        int[] pixels = new int[width * height];
        image.getPixelReader().getPixels(0, 0, width, height, PixelFormat.getIntArgbInstance(), pixels, 0, width);
        for (int p : pixels) colours.add(p);
        if (width < 600 || height < 400 || colours.size() < 32) throw new IllegalStateException("Blank or undersized native scene");
        var png = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
        png.setRGB(0, 0, width, height, pixels, 0, width);
        ImageIO.write(png, "png", output.resolve("native-scene.png").toFile());
        var menus = root.lookupAll(".menu-bar");
        boolean nativeMenu = menus.stream().anyMatch(n -> n instanceof MenuBar m && m.isUseSystemMenuBar() && m.getMenus().size() >= 3);
        if (!nativeMenu) throw new IllegalStateException("Native menu policy absent");
        String glass = com.sun.glass.ui.Application.GetApplication().getClass().getName();
        if (!glass.contains(".mac.MacApplication")) throw new IllegalStateException("Not native Mac Glass: " + glass);
        Files.writeString(output.resolve("native-smoke.json"),
          "{\"status\":\"passed\",\"title\":" + quote(stage.getTitle()) +
          ",\"java_home\":" + quote(System.getProperty("java.home")) +
          ",\"glass\":" + quote(glass) + ",\"native_menu_policy\":true" +
          ",\"scene_width\":" + width + ",\"scene_height\":" + height +
          ",\"output_scale_x\":" + stage.getOutputScaleX() +
          ",\"output_scale_y\":" + stage.getOutputScaleY() +
          ",\"participant_rows\":" + rows.getItems().size() +
          ",\"distinct_pixel_colours\":" + colours.size() + "}\n");
        finished = true;
        // Exercise the application's ordinary quit/save/lock-release lifecycle.
        stage.fireEvent(new WindowEvent(stage, WindowEvent.WINDOW_CLOSE_REQUEST));
        return;
      }
    } catch (Throwable failure) {
      finished = true;
      try { Files.writeString(output.resolve("native-smoke-error.txt"), failure.toString()); }
      catch (Exception ignored) { failure.printStackTrace(); }
    }
  }
}
