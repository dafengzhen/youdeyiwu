package com.youdeyiwu.tool;

import java.util.Map;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.apache.commons.text.StringSubstitutor;
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
    String messageTemplate = getMessage(code);

    if (Objects.isNull(messageTemplate)) {
      return "Message template not found";
    }

    StringSubstitutor stringSubstitutor = new StringSubstitutor(
        key -> {
          if (args.get(key) instanceof String value && value.isEmpty()) {
            return "-";
          }
          return args.getOrDefault(key, "-").toString();
        }
    );
    stringSubstitutor.setVariablePrefix("{");
    stringSubstitutor.setVariableSuffix("}");
    return stringSubstitutor.replace(messageTemplate);
  }
}
