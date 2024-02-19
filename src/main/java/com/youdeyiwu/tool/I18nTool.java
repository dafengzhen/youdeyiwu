package com.youdeyiwu.tool;

import java.text.MessageFormat;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

/**
 * i18n.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Component
public class I18nTool {

  private final MessageSource messageSource;

  /**
   * get message.
   *
   * @param code code
   * @return String
   */
  public String getMessage(String code) {
    return messageSource.getMessage(
        code,
        null,
        null,
        LocaleContextHolder.getLocale()
    );
  }

  /**
   * get message.
   *
   * @param code code
   * @param args args
   * @return String
   */
  public String getMessage(String code, Map<String, Object> args) {
    Locale locale = LocaleContextHolder.getLocale();
    String messageTemplate =
        messageSource.getMessage(
            code,
            null,
            null,
            locale
        );

    if (Objects.isNull(messageTemplate)) {
      return "Message template not found";
    }

    return new MessageFormat(messageTemplate, locale).format(args.values().toArray());
  }
}
