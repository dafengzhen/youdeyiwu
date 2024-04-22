package com.youdeyiwu.security;

import static com.youdeyiwu.tool.JwtTool.decodeSecret;
import static com.youdeyiwu.tool.JwtTool.verifyJwt;

import com.youdeyiwu.constant.JwtConfigConstant;
import com.youdeyiwu.enums.config.ConfigTypeEnum;
import com.youdeyiwu.exception.ConfigNotFoundException;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.repository.config.ConfigRepository;
import com.youdeyiwu.repository.user.UserRepository;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.JwtException;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Objects;
import javax.crypto.SecretKey;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.userdetails.UserCache;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.web.filter.OncePerRequestFilter;

/**
 * token authentication.
 *
 * @author dafengzhen
 */
@Log4j2
@RequiredArgsConstructor
@Component
public class JwtAuthenticationFilter extends OncePerRequestFilter {

  public static final String AUTHORIZATION = "Authorization";

  public static final String BEARER_PREFIX = "Bearer ";

  private final SecurityService securityService;

  private final ConfigRepository configRepository;

  private final UserRepository userRepository;

  private final UserCache userCache;

  @Override
  protected void doFilterInternal(
      HttpServletRequest request,
      HttpServletResponse response,
      FilterChain filterChain
  ) throws ServletException, IOException {
    setAuthRequest(request, response);
    filterChain.doFilter(request, response);
  }

  /**
   * Set authentication request.
   */
  private void setAuthRequest(
      HttpServletRequest request,
      HttpServletResponse response
  ) throws IOException {
    String header = request.getHeader(AUTHORIZATION);
    if (!isValidAuthorizationHeader(header)) {
      return;
    }

    String token = extractTokenFromHeader(header);
    if (!isValidToken(token)) {
      return;
    }

    Claims claims;
    try {
      claims = verifyJwt(token, getDecodedSecret());
    } catch (JwtException e) {
      log.error(e);
      response.sendError(
          HttpStatus.UNAUTHORIZED.value(),
          HttpStatus.UNAUTHORIZED.getReasonPhrase()
      );
      return;
    }

    UserEntity entity = getUserFromClaims(claims);
    if (Objects.isNull(entity)) {
      return;
    }

    if (!isTokenValid(entity, token)) {
      userCache.removeUserFromCache(String.valueOf(entity.getId()));
      return;
    }

    userCache.putUserInCache(entity);
    securityService.setAuthentication(entity, request);
  }

  /**
   * Check if the authorization header is valid.
   */
  private boolean isValidAuthorizationHeader(String header) {
    return StringUtils.hasText(header) && header.startsWith(BEARER_PREFIX);
  }

  /**
   * Extract the token from the authorization header.
   */
  private String extractTokenFromHeader(String header) {
    return header.substring(BEARER_PREFIX.length());
  }

  /**
   * Check if the token is valid.
   */
  private boolean isValidToken(String token) {
    return token.contains(".");
  }

  /**
   * Get the decoded secret from the configuration.
   */
  private SecretKey getDecodedSecret() {
    return decodeSecret(
        configRepository
            .findOptionalByTypeAndName(ConfigTypeEnum.JWT, JwtConfigConstant.SECRET)
            .orElseThrow(ConfigNotFoundException::new)
            .getValue()
    );
  }

  /**
   * Get the user entity from the claims.
   */
  private UserEntity getUserFromClaims(Claims claims) {
    Object user = userCache.getUserFromCache(claims.getSubject());
    if (Objects.nonNull(user) && user instanceof UserEntity userentity) {
      return userentity;
    }
    return userRepository.findById(Long.parseLong(claims.getSubject())).orElse(null);
  }

  /**
   * Check if the token is valid for the user entity.
   */
  private boolean isTokenValid(UserEntity entity, String token) {
    return token.equals(entity.getToken());
  }
}