package com.youdeyiwu.security;

import static com.youdeyiwu.tool.Tool.getRoleAttribute;

import com.youdeyiwu.model.entity.user.PermissionEntity;
import com.youdeyiwu.model.entity.user.RoleEntity;
import com.youdeyiwu.repository.user.PermissionRepository;
import java.util.Collection;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.ConfigAttribute;
import org.springframework.security.access.SecurityConfig;
import org.springframework.security.web.FilterInvocation;
import org.springframework.security.web.access.intercept.FilterInvocationSecurityMetadataSource;
import org.springframework.security.web.access.intercept.RequestAuthorizationContext;
import org.springframework.security.web.util.matcher.AndRequestMatcher;
import org.springframework.security.web.util.matcher.AntPathRequestMatcher;
import org.springframework.security.web.util.matcher.RegexRequestMatcher;
import org.springframework.security.web.util.matcher.RequestMatcher;
import org.springframework.stereotype.Service;
import org.springframework.web.util.UrlPathHelper;

/**
 * metadata configuration.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Service
public class SecurityMetadataSource implements FilterInvocationSecurityMetadataSource {

  public final Map<RequestMatcher, Collection<ConfigAttribute>> requestMap = new LinkedHashMap<>();

  private final PermissionRepository permissionRepository;

  @Override
  public Collection<ConfigAttribute> getAllConfigAttributes() {
    return requestMap
        .values()
        .parallelStream()
        .flatMap(Collection::parallelStream)
        .collect(Collectors.toSet());
  }

  @Override
  public Collection<ConfigAttribute> getAttributes(Object object) {
    return requestMap.entrySet()
        .parallelStream()
        .filter(requestMatcherCollectionEntry -> requestMatcherCollectionEntry
            .getKey()
            .matches(((RequestAuthorizationContext) object).getRequest())
        )
        .map(Map.Entry::getValue)
        .flatMap(Collection::parallelStream)
        .distinct()
        .toList();
  }

  @Override
  public boolean supports(Class<?> clazz) {
    return FilterInvocation.class.isAssignableFrom(clazz);
  }

  /**
   * initialize metadata.
   */
  public void initMetadata() {
    permissionRepository.findAll().forEach(this::update);
  }

  /**
   * update permissions.
   *
   * @param permissionEntity permissionEntity
   */
  public void update(PermissionEntity permissionEntity) {
    requestMap.put(
        createRequestMatcher(permissionEntity),
        permissionEntity.getRoles()
            .parallelStream()
            .map(roleEntity -> (ConfigAttribute) new SecurityConfig(getRoleAttribute(roleEntity)))
            .toList()
    );
  }

  /**
   * remove permissions.
   *
   * @param permissionEntity permissionEntity
   */
  public void remove(PermissionEntity permissionEntity, RoleEntity roleEntity) {
    requestMap.computeIfPresent(
        createRequestMatcher(permissionEntity),
        (matcher, configAttributes) -> configAttributes
            .parallelStream()
            .filter(configAttribute -> !configAttribute.equals(new SecurityConfig(getRoleAttribute(roleEntity))))
            .toList()
    );
  }

  /**
   * create request matcher.
   *
   * @param permissionEntity permissionEntity
   * @return RequestMatcher
   */
  private RequestMatcher createRequestMatcher(PermissionEntity permissionEntity) {
    List<PermissionEntity> matchers = permissionEntity.getMatchers()
        .parallelStream()
        .sorted(
            Comparator.comparing(PermissionEntity::getSort)
                .thenComparing(PermissionEntity::getId).reversed()
        )
        .toList();
    return matchers.isEmpty()
        ? getMatcher(permissionEntity)
        : new AndRequestMatcher(matchers.parallelStream().map(this::getMatcher).toList());
  }

  /**
   * get matcher.
   *
   * @param permissionEntity permissionEntity
   * @return RequestMatcher
   */
  private RequestMatcher getMatcher(PermissionEntity permissionEntity) {
    return switch (permissionEntity.getType()) {
      case ANT -> new AntPathRequestMatcher(
          permissionEntity.getName(),
          permissionEntity.getMethod().name(),
          permissionEntity.getCaseInsensitive(),
          UrlPathHelper.defaultInstance
      );
      case REGEX -> new RegexRequestMatcher(
          permissionEntity.getName(),
          permissionEntity.getMethod().name(),
          permissionEntity.getCaseInsensitive()
      );
    };
  }
}