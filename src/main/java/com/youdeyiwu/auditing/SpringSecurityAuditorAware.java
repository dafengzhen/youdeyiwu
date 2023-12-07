package com.youdeyiwu.auditing;

import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.security.SecurityService;
import java.util.Optional;
import org.springframework.data.domain.AuditorAware;

/**
 * SpringSecurityAuditorAware.
 *
 * @author dafengzhen
 */
public class SpringSecurityAuditorAware implements AuditorAware<UserEntity> {

  private final SecurityService securityService;

  public SpringSecurityAuditorAware(SecurityService securityService) {
    this.securityService = securityService;
  }

  @Override
  public Optional<UserEntity> getCurrentAuditor() {
    if (securityService.isAnonymous()) {
      return Optional.empty();
    }
    return Optional.of(securityService.getAuthenticatedUser());
  }
}
