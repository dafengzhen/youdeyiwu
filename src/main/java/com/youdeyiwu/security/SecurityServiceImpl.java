package com.youdeyiwu.security;

import static com.youdeyiwu.tool.Tool.getRoleAttribute;

import com.youdeyiwu.exception.UnauthorizedException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.model.entity.user.RoleEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.other.UserContext;
import com.youdeyiwu.repository.user.UserRepository;
import jakarta.servlet.http.HttpServletRequest;
import java.util.Comparator;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.WebAuthenticationDetails;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

/**
 * security.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Service
public class SecurityServiceImpl implements SecurityService {

  private final HttpServletRequest request;

  private final UserRepository userRepository;

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
    entity.setAuthorities(
        userRepository.findAllRoleById(entity.getId())
            .getRoles()
            .stream()
            .sorted(
                Comparator.comparing(RoleEntity::getSort)
                    .thenComparing(RoleEntity::getId).reversed()
            )
            .map(roleEntity -> new SimpleGrantedAuthority(getRoleAttribute(roleEntity)))
            .toList()
    );
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

  @Override
  public String getAlias(UserEntity entity) {
    String alias;
    if (StringUtils.hasText(entity.getAlias())) {
      alias = entity.getAlias();
    } else if (StringUtils.hasText(entity.getUsername())) {
      alias = entity.getUsername();
    } else {
      alias = entity.getId().toString();
    }
    return alias;
  }

  @Override
  public String getAliasAndId(UserEntity entity) {
    return getAlias(entity) + "#" + entity.getId();
  }

  @Override
  public String getAliasAndIdOrNull(UserEntity entity) {
    return Objects.isNull(entity) ? null : getAliasAndId(entity);
  }

  @Override
  public String getAliasAndIdOrNull() {
    return isAuthenticated() ? getAliasAndId(getAuthenticatedUser()) : null;
  }

  @Override
  public UserContext getUserContext() {
    UserEntity user = null;
    UserEntity root = null;
    boolean anonymous = isAnonymous();

    if (!anonymous) {
      user = userRepository.findById(getUserId())
          .orElseThrow(UserNotFoundException::new);
      if (Boolean.TRUE.equals(user.getRoot())) {
        root = user;
      }
    }

    return new UserContext(anonymous, user, root);
  }
}