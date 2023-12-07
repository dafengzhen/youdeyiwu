package com.youdeyiwu.tool;

import java.util.UUID;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.safety.Safelist;

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
}