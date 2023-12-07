package com.youdeyiwu.service.user.impl;

import com.youdeyiwu.exception.PermissionNotFoundException;
import com.youdeyiwu.exception.RoleNotFoundException;
import com.youdeyiwu.mapper.user.PermissionMapper;
import com.youdeyiwu.mapper.user.RoleMapper;
import com.youdeyiwu.model.dto.user.CreatePermissionDto;
import com.youdeyiwu.model.dto.user.UpdatePermissionDto;
import com.youdeyiwu.model.dto.user.UpdateRolesPermissionDto;
import com.youdeyiwu.model.entity.user.PermissionEntity;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.user.PermissionEntityVo;
import com.youdeyiwu.repository.user.PermissionRepository;
import com.youdeyiwu.repository.user.RoleRepository;
import com.youdeyiwu.service.user.PermissionService;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * permission.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Service
public class PermissionServiceImpl implements PermissionService {

  private final PermissionRepository permissionRepository;

  private final RoleRepository roleRepository;

  private final PermissionMapper permissionMapper;

  private final RoleMapper roleMapper;

  @Transactional
  @Override
  public PermissionEntity create(CreatePermissionDto dto) {
    PermissionEntity permissionEntity = new PermissionEntity();
    permissionMapper.dtoToEntity(dto, permissionEntity);
    updateMatchers(dto.matchers(), permissionEntity);
    permissionRepository.save(permissionEntity);
    return permissionEntity;
  }

  @Transactional
  @Override
  public void updateRoles(Long id, UpdateRolesPermissionDto dto) {
    PermissionEntity permissionEntity = permissionRepository.findById(id)
        .orElseThrow(PermissionNotFoundException::new);

    if (Objects.nonNull(dto.roles())) {
      permissionEntity.setRoles(
          dto.roles()
              .stream()
              .map(pid -> roleRepository.findById(pid)
                  .orElseThrow(RoleNotFoundException::new)
              )
              .collect(Collectors.toSet())
      );
    }
  }

  @Transactional
  @Override
  public void update(Long id, UpdatePermissionDto dto) {
    PermissionEntity permissionEntity = permissionRepository.findById(id)
        .orElseThrow(PermissionNotFoundException::new);

    permissionMapper.dtoToEntity(dto, permissionEntity);
    updateMatchers(dto.matchers(), permissionEntity);
  }

  @Override
  public PermissionEntityVo query(Long id) {
    PermissionEntity permissionEntity = permissionRepository.findById(id)
        .orElseThrow(PermissionNotFoundException::new);

    PermissionEntityVo vo = permissionMapper.entityToVo(permissionEntity);
    setRoles(vo, permissionEntity);
    setMatchers(vo, permissionEntity);
    return vo;
  }

  @Override
  public PageVo<PermissionEntityVo> queryAll(Pageable pageable) {
    return new PageVo<>(permissionRepository.findAll(pageable).map(permissionEntity -> {
      PermissionEntityVo vo = permissionMapper.entityToVo(permissionEntity);
      setRoles(vo, permissionEntity);
      setMatchers(vo, permissionEntity);
      return vo;
    }));
  }

  @Transactional
  @Override
  public void delete(Long id) {
    PermissionEntity permissionEntity = permissionRepository.findById(id)
        .orElseThrow(PermissionNotFoundException::new);
    permissionRepository.delete(permissionEntity);
  }

  /**
   * set roles.
   *
   * @param vo               vo
   * @param permissionEntity permissionEntity
   */
  private void setRoles(PermissionEntityVo vo, PermissionEntity permissionEntity) {
    vo.setRoles(
        permissionEntity.getRoles()
            .stream()
            .map(roleMapper::entityToVo)
            .collect(Collectors.toSet())
    );
  }

  /**
   * set matchers.
   *
   * @param vo               vo
   * @param permissionEntity permissionEntity
   */
  private void setMatchers(PermissionEntityVo vo, PermissionEntity permissionEntity) {
    vo.setMatchers(
        permissionEntity.getMatchers()
            .stream()
            .map(permissionMapper::entityToVo)
            .collect(Collectors.toSet())
    );
  }

  /**
   * update matchers.
   *
   * @param ids              ids
   * @param permissionEntity permissionEntity
   */
  private void updateMatchers(Set<Long> ids, PermissionEntity permissionEntity) {
    if (Objects.nonNull(ids)) {
      Set<PermissionEntity> matchers = ids
          .stream()
          .map(pid -> permissionRepository.findById(pid)
              .orElseThrow(PermissionNotFoundException::new)
          )
          .collect(Collectors.toSet());
      permissionEntity.setMatchers(matchers);
    }
  }
}