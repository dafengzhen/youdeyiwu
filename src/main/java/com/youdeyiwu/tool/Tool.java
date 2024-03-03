package com.youdeyiwu.tool;

import static com.youdeyiwu.constant.RoleConstant.ROLE_PREFIX;
import static org.springframework.http.MediaType.IMAGE_JPEG;
import static org.springframework.http.MediaType.IMAGE_JPEG_VALUE;
import static org.springframework.http.MediaType.IMAGE_PNG;
import static org.springframework.http.MediaType.IMAGE_PNG_VALUE;

import com.youdeyiwu.enums.file.FileTypeEnum;
import com.youdeyiwu.enums.point.SignEnum;
import com.youdeyiwu.model.entity.point.PointEntity;
import com.youdeyiwu.model.entity.user.RoleEntity;
import java.io.IOException;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.function.ObjIntConsumer;
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
   * @param link link
   * @return boolean
   */
  public static boolean isHttpOrHttps(String link) {
    return link.startsWith("http://") || link.startsWith("https://");
  }

  /**
   * is valid link.
   *
   * @param link link
   * @return boolean
   */
  public static boolean isValidLink(String link) {
    return isHttpOrHttps(link) || link.startsWith("/");
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

    try {
      byte[] fileBytes = file.getBytes();
      for (FileTypeEnum fileType : fileTypes) {
        byte[] fileTypeHeader = switch (fileType) {
          case PNG -> PNG_HEADER;
          case JPG -> JPEG_HEADER;
        };

        if (
            Arrays.equals(
                Arrays.copyOf(fileBytes, Math.min(fileBytes.length, fileTypeHeader.length)),
                fileTypeHeader
            )
        ) {
          return true;
        }
      }

      return false;
    } catch (IOException e) {
      return false;
    }
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

  /**
   * get current date time.
   *
   * @return String
   */
  public static String getCurrentDateTime() {
    return getCurrentDateTime(OffsetDateTime.now());
  }

  /**
   * get current date time.
   *
   * @param dateTime dateTime
   * @return String
   */
  public static String getCurrentDateTime(OffsetDateTime dateTime) {
    return dateTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
  }

  /**
   * get current date time.
   *
   * @param dateTime dateTime
   * @return String
   */
  public static String getCurrentDateTime(LocalDate dateTime) {
    return dateTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd"));
  }

  /**
   * calculate points.
   *
   * @param requiredPoints requiredPoints
   * @param sign           sign
   * @return Integer
   */
  public static Integer calculatePoints(Integer requiredPoints, SignEnum sign) {
    int points = Math.abs(requiredPoints);
    return switch (sign) {
      case POSITIVE -> points;
      case NEGATIVE -> -points;
      case ZERO -> 0;
    };
  }

  /**
   * Determines the sign of an integer and returns the result through a callback parameter.
   *
   * @param number   The integer to determine the sign of.
   * @param callback The callback function to handle the result string.
   */
  public static void getSign(Integer number, Consumer<SignEnum> callback) {
    if (number > 0) {
      callback.accept(SignEnum.POSITIVE);
    } else if (number < 0) {
      callback.accept(SignEnum.NEGATIVE);
    } else {
      callback.accept(SignEnum.ZERO);
    }
  }

  /**
   * Calculates the sign of the difference between the points and oldPoints of a PointEntity object
   * and invokes the specified callback with the corresponding SignEnum value and the absolute difference.
   *
   * @param pointEntity The PointEntity object containing the points and oldPoints values.
   * @param callback    The callback function to be invoked with the calculated SignEnum value and the absolute difference.
   */
  public static void getDifferenceSign(PointEntity pointEntity, ObjIntConsumer<SignEnum> callback) {
    int difference = Objects.nonNull(pointEntity.getOldPoints())
        ? pointEntity.getPoints() - pointEntity.getOldPoints()
        : 0;
    int absoluteDifference = Math.abs(difference);
    if (difference > 0) {
      callback.accept(SignEnum.POSITIVE, absoluteDifference);
    } else if (difference < 0) {
      callback.accept(SignEnum.NEGATIVE, absoluteDifference);
    } else {
      callback.accept(SignEnum.ZERO, absoluteDifference);
    }
  }

  /**
   * get role attribute.
   *
   * @param roleEntity roleEntity
   * @return String
   */
  public static String getRoleAttribute(RoleEntity roleEntity) {
    return ROLE_PREFIX + roleEntity.getName() + "_" + roleEntity.getId();
  }
}