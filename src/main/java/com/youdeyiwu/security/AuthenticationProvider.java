package com.youdeyiwu.security;

import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.core.userdetails.UserCache;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.stereotype.Component;

/**
 * authentication provider.
 *
 * @author dafengzhen
 */
@Component
public class AuthenticationProvider extends DaoAuthenticationProvider {

  public AuthenticationProvider(UserDetailsService userDetailsService, UserCache userCache) {
    setUserDetailsService(userDetailsService);
    setUserCache(userCache);
  }
}
