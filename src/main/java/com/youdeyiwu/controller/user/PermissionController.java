package com.youdeyiwu.controller.user;

import com.youdeyiwu.model.dto.user.CreatePermissionDto;
import com.youdeyiwu.model.dto.user.UpdatePermissionDto;
import com.youdeyiwu.model.dto.user.UpdateRolesPermissionDto;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.user.PermissionEntityVo;
import com.youdeyiwu.service.user.PermissionService;
import jakarta.validation.Valid;
import java.net.URI;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.data.web.SortDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * permission.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@RequestMapping(value = "/permissions")
@RestController
public class PermissionController {

  private final PermissionService permissionService;

  /**
   * create.
   *
   * @param dto dto
   * @return ResponseEntity
   */
  @PostMapping
  public ResponseEntity<Void> create(@Valid @RequestBody CreatePermissionDto dto) {
    return ResponseEntity
        .created(URI.create("/permissions/" + permissionService.create(dto).getId()))
        .build();
  }

  @PutMapping("/{id}/roles")
  public ResponseEntity<Void> updateRoles(
      @PathVariable Long id,
      @Valid @RequestBody UpdateRolesPermissionDto dto
  ) {
    permissionService.updateRoles(id, dto);
    return ResponseEntity.noContent().build();
  }

  @PutMapping("/{id}")
  public ResponseEntity<Void> update(
      @PathVariable Long id,
      @Valid @RequestBody UpdatePermissionDto dto
  ) {
    permissionService.update(id, dto);
    return ResponseEntity.noContent().build();
  }

  @GetMapping(value = "/{id}")
  public ResponseEntity<PermissionEntityVo> query(@PathVariable Long id) {
    return ResponseEntity.ok(permissionService.query(id));
  }

  @GetMapping
  public ResponseEntity<PageVo<PermissionEntityVo>> queryAll(
      @PageableDefault(size = 15)
      @SortDefault(value = {"sort", "id"}, direction = Sort.Direction.DESC)
      Pageable pageable
  ) {
    return ResponseEntity.ok(permissionService.queryAll(pageable));
  }

  @DeleteMapping(value = "/{id}")
  public ResponseEntity<Void> delete(@PathVariable Long id) {
    permissionService.delete(id);
    return ResponseEntity.noContent().build();
  }
}