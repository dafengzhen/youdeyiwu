package com.youdeyiwu.security;

import com.youdeyiwu.exception.UnauthorizedException;
import com.youdeyiwu.model.entity.user.UserEntity;
import jakarta.servlet.http.HttpServletRequest;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.WebAuthenticationDetails;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Service;

/**
 * security.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Service
public class SecurityServiceImpl implements SecurityService {

  private final HttpServletRequest request;

  @Override
  public Authentication getAuthentication() {
    return SecurityContextHolder.getContext().getAuthentication();
  }

  @Override
  public void setAuthentication(UserEntity entity) {
    setAuthentication(entity, request);
  }

  @Override
  public void setAuthentication(UserEntity entity, HttpServletRequest request) {
    UsernamePasswordAuthenticationToken authRequest = new UsernamePasswordAuthenticationToken(
        entity,
        null,
        entity.getAuthorities()
    );
    authRequest.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
    SecurityContext context = SecurityContextHolder.createEmptyContext();
    context.setAuthentication(authRequest);
    SecurityContextHolder.setContext(context);
  }

  @Override
  public void setAnonAuthentication(UserEntity entity, HttpServletRequest request) {
    AnonymousAuthenticationToken authRequest = new AnonymousAuthenticationToken(
        String.valueOf(entity.getId()),
        entity,
        entity.getAuthorities()
    );
    authRequest.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
    SecurityContext context = SecurityContextHolder.createEmptyContext();
    context.setAuthentication(authRequest);
    SecurityContextHolder.setContext(context);
  }

  @Override
  public boolean isAnonymous() {
    Authentication authentication = getAuthentication();
    return Objects.isNull(authentication)
        || AnonymousAuthenticationToken.class.isAssignableFrom(authentication.getClass());
  }

  @Override
  public boolean isAuthenticated() {
    return !isAnonymous();
  }

  @Override
  public void checkAuthenticationStatus() {
    if (isAnonymous()) {
      throw new UnauthorizedException();
    }
  }

  @Override
  public AnonymousAuthenticationToken getAnonymousUser() {
    return (AnonymousAuthenticationToken) getAuthentication();
  }

  @Override
  public UserEntity getAuthenticatedUser() {
    if (isAnonymous()) {
      throw new UnauthorizedException();
    }
    return (UserEntity) getAuthentication().getPrincipal();
  }

  @Override
  public WebAuthenticationDetails getDetails(HttpServletRequest request) {
    return new WebAuthenticationDetails(request);
  }

  @Override
  public WebAuthenticationDetails getDetails() {
    return (WebAuthenticationDetails) SecurityContextHolder.getContext()
        .getAuthentication()
        .getDetails();
  }

  @Override
  public Long getUserId() {
    return getAuthenticatedUser().getId();
  }

  @Override
  public Long getUserIdOrNull() {
    return isAuthenticated() ? getUserId() : null;
  }
}