package com.youdeyiwu.config;

import com.github.benmanes.caffeine.cache.Caffeine;
import com.youdeyiwu.constant.CacheConstant;
import java.util.concurrent.TimeUnit;
import org.springframework.cache.caffeine.CaffeineCacheManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * cache config.
 *
 * @author dafengzhen
 */
@Configuration
public class CacheConfig {

  /**
   * caffeineCacheManager.
   *
   * @return CaffeineCacheManager
   */
  @Bean
  public CaffeineCacheManager caffeineCacheManager() {
    CaffeineCacheManager caffeineCacheManager = new CaffeineCacheManager();
    caffeineCacheManager.registerCustomCache(
        CacheConstant.USER_CACHE,
        Caffeine.newBuilder().expireAfterAccess(26, TimeUnit.DAYS).maximumSize(1000).build()
    );
    return caffeineCacheManager;
  }
}
