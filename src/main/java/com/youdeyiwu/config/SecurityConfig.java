package com.youdeyiwu.config;

import static org.springframework.security.config.Customizer.withDefaults;

import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.AccessDecisionAuthorizationManager;
import com.youdeyiwu.security.DefaultRateLimitFilter;
import com.youdeyiwu.security.JwtAuthenticationFilter;
import com.youdeyiwu.service.user.impl.UserDetailsServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.core.userdetails.UserCache;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

/**
 * security config.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Configuration
public class SecurityConfig {

  private final UserCache userCache;

  private final DefaultRateLimitFilter defaultRateLimitFilter;

  private final JwtAuthenticationFilter jwtAuthenticationFilter;

  private final AccessDecisionAuthorizationManager accessDecisionAuthorizationManager;

  @Bean
  public PasswordEncoder passwordEncoder() {
    return new BCryptPasswordEncoder();
  }

  /**
   * daoAuthenticationProvider.
   *
   * @return DaoAuthenticationProvider
   */
  @Bean
  public DaoAuthenticationProvider daoAuthenticationProvider(UserRepository userRepository) {
    DaoAuthenticationProvider daoAuthenticationProvider = new DaoAuthenticationProvider(passwordEncoder());
    UserDetailsService userDetailsService = new UserDetailsServiceImpl(userRepository);
    daoAuthenticationProvider.setUserDetailsService(userDetailsService);
    daoAuthenticationProvider.setUserCache(userCache);
    return daoAuthenticationProvider;
  }

  /**
   * filterChain.
   *
   * @param http http
   * @return SecurityFilterChain
   * @throws Exception ex
   */
  @Bean
  public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
    http.cors(withDefaults());
    http.formLogin(AbstractHttpConfigurer::disable);
    http.httpBasic(AbstractHttpConfigurer::disable);
    http.csrf(AbstractHttpConfigurer::disable);
    http.logout(AbstractHttpConfigurer::disable);
    http.rememberMe(AbstractHttpConfigurer::disable);
    http.rememberMe(AbstractHttpConfigurer::disable);
    http.authorizeHttpRequests(
        authorizationManagerRequestMatcherRegistry -> authorizationManagerRequestMatcherRegistry
            .anyRequest()
            .access(accessDecisionAuthorizationManager)
    );
    http.sessionManagement(
        httpSecuritySessionManagementConfigurer -> httpSecuritySessionManagementConfigurer
            .sessionCreationPolicy(SessionCreationPolicy.STATELESS)
    );

    http.addFilterBefore(defaultRateLimitFilter, UsernamePasswordAuthenticationFilter.class)
        .addFilterBefore(jwtAuthenticationFilter, UsernamePasswordAuthenticationFilter.class);
    return http.build();
  }
}
