package com.youdeyiwu.tool;

import static org.springframework.http.MediaType.IMAGE_JPEG;
import static org.springframework.http.MediaType.IMAGE_JPEG_VALUE;
import static org.springframework.http.MediaType.IMAGE_PNG;
import static org.springframework.http.MediaType.IMAGE_PNG_VALUE;

import com.youdeyiwu.enums.file.FileTypeEnum;
import java.io.IOException;
import java.util.Arrays;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.safety.Safelist;
import org.springframework.http.MediaType;
import org.springframework.web.multipart.MultipartFile;

/**
 * tool.
 *
 * @author dafengzhen
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class Tool {

  protected static final int BYTES_PER_KB = 1024;

  protected static final byte[] PNG_HEADER = {
      (byte) 0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A
  };

  protected static final byte[] JPEG_HEADER = {(byte) 0xFF, (byte) 0xD8, (byte) 0xFF};

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
   * is valid image file.
   *
   * @param file        file
   * @param maxSizeInKb maxSizeInKb
   * @param fileTypes   fileTypes
   * @return boolean
   */
  public static boolean isValidImageFile(
      MultipartFile file,
      long maxSizeInKb,
      Set<FileTypeEnum> fileTypes
  ) {
    // One kilobyte equals 1024 bytes
    if (file.isEmpty() || file.getSize() > (maxSizeInKb * BYTES_PER_KB)) {
      return false;
    }

    byte[] fileHeader;
    try {
      fileHeader = new byte[Math.min(file.getBytes().length, PNG_HEADER.length)];
      System.arraycopy(file.getBytes(), 0, fileHeader, 0, fileHeader.length);
    } catch (IOException e) {
      return false;
    }

    for (FileTypeEnum fileType : fileTypes) {
      byte[] fileTypeHeader = switch (fileType) {
        case PNG -> PNG_HEADER;
        case JPG -> JPEG_HEADER;
      };
      if (Arrays.equals(fileHeader, fileTypeHeader)) {
        return true;
      }
    }

    return false;
  }

  /**
   * get file type.
   *
   * @param file file
   * @return FileTypeEnum
   */
  public static FileTypeEnum getFileType(MultipartFile file) {
    String contentType = file.getContentType();
    if (Objects.nonNull(contentType)) {
      return switch (contentType) {
        case IMAGE_JPEG_VALUE -> FileTypeEnum.JPG;
        case IMAGE_PNG_VALUE -> FileTypeEnum.PNG;
        default -> FileTypeEnum.JPG;
      };
    }
    return FileTypeEnum.JPG;
  }

  /**
   * get media type.
   *
   * @param fileType fileType
   * @return MediaType
   */
  public static MediaType getMediaType(FileTypeEnum fileType) {
    if (Objects.nonNull(fileType)) {
      return switch (fileType) {
        case JPG -> IMAGE_JPEG;
        case PNG -> IMAGE_PNG;
        default -> IMAGE_JPEG;
      };
    }
    return IMAGE_JPEG;
  }
}