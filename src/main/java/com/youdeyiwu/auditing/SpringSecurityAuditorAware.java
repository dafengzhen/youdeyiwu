package com.youdeyiwu.auditing;

import com.youdeyiwu.security.SecurityService;
import java.util.Optional;
import org.springframework.data.domain.AuditorAware;

/**
 * SpringSecurityAuditorAware.
 *
 * @author dafengzhen
 */
public class SpringSecurityAuditorAware implements AuditorAware<Long> {

  private final SecurityService securityService;

  public SpringSecurityAuditorAware(SecurityService securityService) {
    this.securityService = securityService;
  }

  @Override
  public Optional<Long> getCurrentAuditor() {
    if (securityService.isAnonymous()) {
      return Optional.empty();
    }
    return Optional.of(securityService.getUserId());
  }
}
