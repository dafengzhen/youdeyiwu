package com.youdeyiwu.security;

import java.util.Collection;
import java.util.function.Supplier;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.ConfigAttribute;
import org.springframework.security.authorization.AuthorityAuthorizationManager;
import org.springframework.security.authorization.AuthorizationDecision;
import org.springframework.security.authorization.AuthorizationManager;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.access.intercept.RequestAuthorizationContext;
import org.springframework.stereotype.Component;

/**
 * access decision.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Component
public class AccessDecisionAuthorizationManager
    implements AuthorizationManager<RequestAuthorizationContext> {

  private final SecurityMetadataSource securityMetadataSource;

  @Override
  public AuthorizationDecision check(
      Supplier<Authentication> authentication,
      RequestAuthorizationContext object
  ) {
    Collection<ConfigAttribute> attributes = this.securityMetadataSource.getAttributes(object);
    if (attributes.isEmpty()) {
      return null;
    }

    AuthorityAuthorizationManager<Object> authority = AuthorityAuthorizationManager.hasAnyAuthority(
        attributes
            .stream()
            .map(ConfigAttribute::getAttribute)
            .toArray(value -> new String[attributes.size()])
    );
    return authority.check(authentication, object);
  }

  @Override
  public void verify(Supplier<Authentication> authentication, RequestAuthorizationContext object) {
    AuthorizationManager.super.verify(authentication, object);
  }
}