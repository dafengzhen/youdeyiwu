package com.youdeyiwu.security;

import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.other.UserContext;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.WebAuthenticationDetails;

/**
 * security.
 *
 * @author dafengzhen
 */
public interface SecurityService {

  /**
   * get authentication.
   *
   * @return Authentication
   */
  Authentication getAuthentication();

  /**
   * set authentication.
   *
   * @param entity entity
   */
  void setAuthentication(UserEntity entity);

  /**
   * set authentication.
   *
   * @param entity  entity
   * @param request request
   */
  void setAuthentication(UserEntity entity, HttpServletRequest request);

  /**
   * set anon authentication.
   *
   * @param entity  entity
   * @param request request
   */
  void setAnonAuthentication(UserEntity entity, HttpServletRequest request);

  /**
   * is anonymous.
   *
   * @return boolean
   */
  boolean isAnonymous();

  /**
   * is authenticated.
   *
   * @return boolean
   */
  boolean isAuthenticated();

  /**
   * check authentication status.
   */
  void checkAuthenticationStatus();

  /**
   * get anonymous user.
   *
   * @return AnonymousAuthenticationToken
   */
  AnonymousAuthenticationToken getAnonymousUser();

  /**
   * get authenticated user.
   *
   * @return UserEntity
   */
  UserEntity getAuthenticatedUser();

  /**
   * get details.
   *
   * @param request request
   * @return WebAuthenticationDetails
   */
  WebAuthenticationDetails getDetails(HttpServletRequest request);

  /**
   * get details.
   *
   * @return WebAuthenticationDetails
   */
  WebAuthenticationDetails getDetails();

  /**
   * get user id.
   *
   * @return Long
   */
  Long getUserId();

  /**
   * get user id or null.
   *
   * @return Long
   */
  Long getUserIdOrNull();

  /**
   * get alias.
   *
   * @param entity entity
   * @return String
   */
  String getAlias(UserEntity entity);

  /**
   * get alias and id.
   *
   * @param entity entity
   * @return String
   */
  String getAliasAndId(UserEntity entity);

  /**
   * get alias and id or null.
   *
   * @param entity entity
   * @return String
   */
  String getAliasAndIdOrNull(UserEntity entity);

  /**
   * get alias and id or null.
   *
   * @return String
   */
  String getAliasAndIdOrNull();

  /**
   * get user context.
   *
   * @return UserContext
   */
  UserContext getUserContext();
}