package com.youdeyiwu.security;

import com.youdeyiwu.constant.RoleConstant;
import com.youdeyiwu.model.entity.user.PermissionEntity;
import com.youdeyiwu.model.entity.user.RoleEntity;
import com.youdeyiwu.repository.user.PermissionRepository;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Sort;
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
@Log4j2
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
    UrlPathHelper helper = UrlPathHelper.defaultInstance;
    permissionRepository.findAll(Sort.by(Sort.Direction.DESC, "sort", "id"))
        .forEach(permissionEntity -> update(helper, permissionEntity));
    System.out.println("xxx");
  }

  /**
   * update permissions.
   *
   * @param helper           helper
   * @param permissionEntity permissionEntity
   */
  public void update(UrlPathHelper helper, PermissionEntity permissionEntity) {
    Set<PermissionEntity> matchers = permissionEntity.getMatchers();
    RequestMatcher requestMatcher =
        matchers.isEmpty()
            ? getMatcher(permissionEntity, helper)
            : new AndRequestMatcher(
            matchers.parallelStream()
                .map(entity -> getMatcher(entity, helper))
                .toList()
        );
    requestMap.put(
        requestMatcher,
        permissionEntity.getRoles()
            .parallelStream()
            .map(roleEntity -> (ConfigAttribute) new SecurityConfig(
                    RoleConstant.ROLE_PREFIX + roleEntity.getName() + "_" + roleEntity.getId()
                )
            )
            .toList()
    );
  }

  /**
   * remove permissions.
   *
   * @param helper           helper
   * @param permissionEntity permissionEntity
   */
  public void remove(
      UrlPathHelper helper,
      PermissionEntity permissionEntity,
      RoleEntity roleEntity
  ) {
    Set<PermissionEntity> matchers = permissionEntity.getMatchers();
    RequestMatcher requestMatcher =
        matchers.isEmpty()
            ? getMatcher(permissionEntity, helper)
            : new AndRequestMatcher(
            matchers.parallelStream()
                .map(entity -> getMatcher(entity, helper))
                .toList()
        );
    requestMap.computeIfPresent(
        requestMatcher,
        (matcher, configAttributes) -> configAttributes
            .parallelStream()
            .filter(configAttribute -> !configAttribute.equals(
                    new SecurityConfig(
                        RoleConstant.ROLE_PREFIX + roleEntity.getName() + "_" + roleEntity.getId()
                    )
                )
            )
            .toList()
    );
  }

  /**
   * get matcher.
   *
   * @param permissionEntity permissionEntity
   * @param helper           helper
   * @return RequestMatcher
   */
  private RequestMatcher getMatcher(
      PermissionEntity permissionEntity,
      UrlPathHelper helper
  ) {
    return switch (permissionEntity.getType()) {
      case ANT -> new AntPathRequestMatcher(
          permissionEntity.getName(),
          permissionEntity.getMethod().name(),
          permissionEntity.getCaseInsensitive(),
          helper
      );
      case REGEX -> new RegexRequestMatcher(
          permissionEntity.getName(),
          permissionEntity.getMethod().name(),
          permissionEntity.getCaseInsensitive()
      );
    };
  }
}