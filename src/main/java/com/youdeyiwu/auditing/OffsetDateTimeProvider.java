package com.youdeyiwu.auditing;

import java.time.OffsetDateTime;
import java.time.temporal.TemporalAccessor;
import java.util.Optional;
import org.springframework.data.auditing.DateTimeProvider;
import org.springframework.stereotype.Component;

/**
 * offset date time.
 *
 * @author dafengzhen
 */
@Component(value = "dateTimeProvider")
public class OffsetDateTimeProvider implements DateTimeProvider {
  @Override
  public Optional<TemporalAccessor> getNow() {
    return Optional.of(OffsetDateTime.now());
  }
}
