package com.youdeyiwu.security;

import com.github.benmanes.caffeine.cache.Caffeine;
import com.youdeyiwu.exception.TooManyRequestsException;
import io.github.bucket4j.Bandwidth;
import io.github.bucket4j.Bucket;
import io.github.bucket4j.BucketConfiguration;
import io.github.bucket4j.ConsumptionProbe;
import io.github.bucket4j.caffeine.CaffeineProxyManager;
import io.github.bucket4j.distributed.proxy.ProxyManager;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.time.Duration;
import java.util.concurrent.TimeUnit;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

/**
 * rate filtering.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Component
public class DefaultRateLimitFilter extends OncePerRequestFilter {

  private static final BucketConfiguration configuration = BucketConfiguration.builder()
      .addLimit(
          Bandwidth.builder()
              .capacity(3600)
              .refillIntervally(3600, Duration.ofHours(1))
              .build()
      )
      .addLimit(
          Bandwidth.builder()
              .capacity(600)
              .refillIntervally(600, Duration.ofMinutes(10))
              .initialTokens(300)
              .build()
      )
      .build();
  private final SecurityService securityService;
  private ProxyManager<String> buckets;

  @Override
  protected void initFilterBean() {
    buckets = new CaffeineProxyManager<>(
        Caffeine.newBuilder().maximumSize(100),
        Duration.ofMinutes(1)
    );
  }

  @Override
  protected void doFilterInternal(
      HttpServletRequest request,
      HttpServletResponse response,
      FilterChain filterChain
  ) throws ServletException, IOException {
    response.addHeader("X-Powered-By", "www.youdeyiwu.com");
    Bucket bucket = buckets.builder().build(
        securityService.getDetails(request).getRemoteAddress(),
        () -> configuration
    );
    ConsumptionProbe consumptionProbe = bucket.tryConsumeAndReturnRemaining(1);
    if (consumptionProbe.isConsumed()) {
      response.addHeader("X-Rate-Limit-Remaining",
          Long.toString(consumptionProbe.getRemainingTokens()));
      filterChain.doFilter(request, response);
    } else {
      response.addHeader(
          "X-Rate-Limit-Retry-After-Seconds",
          Long.toString(TimeUnit.NANOSECONDS.toSeconds(consumptionProbe.getNanosToWaitForRefill()))
      );
      response.setStatus(HttpStatus.TOO_MANY_REQUESTS.value());
      throw new TooManyRequestsException();
    }
  }
}
