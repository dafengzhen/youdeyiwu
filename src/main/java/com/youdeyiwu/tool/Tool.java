package com.youdeyiwu.tool;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.Objects;
import java.util.UUID;
import javax.imageio.ImageIO;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.safety.Safelist;
import org.springframework.web.multipart.MultipartFile;

/**
 * tool.
 *
 * @author dafengzhen
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class Tool {

  /**
   * get the html relaxed safe list.
   *
   * @return Safelist
   */
  public static Safelist safeListRelaxed() {
    return Safelist.relaxed()
        .addTags("figure")
        .addAttributes(":all", "style", "class")
        .addAttributes("a", "data-fid", "target", "rel", "id")
        .addProtocols("a", "href", "#")
        .addAttributes("iframe", "src", "width", "height", "title", "allow", "allowfullscreen")
        .addAttributes("div", "data-oembed-url");
  }

  /**
   * get html default output settings.
   *
   * @return Document.OutputSettings
   */
  public static Document.OutputSettings outputSettings() {
    return new Document.OutputSettings().prettyPrint(false);
  }

  /**
   * clean html content.
   *
   * @param content content
   * @return String
   */
  public static String cleanHtmlContent(String content) {
    return Jsoup.clean(content, "", safeListRelaxed(), outputSettings());
  }

  /**
   * clean html basic content.
   *
   * @param content content
   * @return String
   */
  public static String cleanBasicContent(String content) {
    return Jsoup.clean(content, "", Safelist.basic(), outputSettings());
  }

  /**
   * whether the protocol is http or https.
   *
   * @param value value
   * @return boolean
   */
  public static boolean isHttpOrHttps(String value) {
    return value.startsWith("http://") || value.startsWith("https://");
  }

  /**
   * random uuid.
   *
   * @return String
   */
  public static String randomUuId() {
    return UUID.randomUUID().toString().replace("-", "");
  }

  /**
   * is valid image.
   *
   * @param file        file
   * @param maxSizeInMb maxSizeInMb
   * @return boolean
   */
  public static boolean isValidImage(MultipartFile file, long maxSizeInMb) {
    BufferedImage image;
    try {
      image = ImageIO.read(file.getInputStream());
    } catch (IOException e) {
      throw new RuntimeException(e);
    }

    // One megabyte equals 1024 kilobytes, and one kilobyte equals 1024 bytes
    long maxSizeBytes = maxSizeInMb * 1024 * 1024;
    return Objects.nonNull(image)
        && image.getWidth() > 0
        && image.getHeight() > 0
        && file.getSize() <= maxSizeBytes;
  }
}