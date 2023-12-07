package com.youdeyiwu.service.user.impl;

import com.github.benmanes.caffeine.cache.Cache;
import com.youdeyiwu.constant.CacheConstant;
import com.youdeyiwu.model.entity.user.UserEntity;
import java.util.Objects;
import org.springframework.cache.caffeine.CaffeineCache;
import org.springframework.cache.caffeine.CaffeineCacheManager;
import org.springframework.security.core.userdetails.UserCache;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;

/**
 * user cache.
 *
 * @author dafengzhen
 */
@Service
public class UserCacheImpl implements UserCache {

  public final Cache<Object, Object> cache;

  /**
   * UserCacheImpl.
   */
  public UserCacheImpl(CaffeineCacheManager caffeineCacheManager) {
    CaffeineCache caffeineCache =
        (CaffeineCache) caffeineCacheManager.getCache(CacheConstant.USER_CACHE);
    cache = Objects.requireNonNull(caffeineCache).getNativeCache();
  }

  @Override
  public UserDetails getUserFromCache(String username) {
    return (UserDetails) cache.getIfPresent(Long.parseLong(username));
  }

  @Override
  public void putUserInCache(UserDetails user) {
    cache.put(((UserEntity) user).getId(), user);
  }

  @Override
  public void removeUserFromCache(String username) {
    cache.invalidate(Long.parseLong(username));
  }
}
