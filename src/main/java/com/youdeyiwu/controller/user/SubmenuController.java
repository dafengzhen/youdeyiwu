package com.youdeyiwu.controller.user;

import com.youdeyiwu.model.dto.user.AssignPermissionsDto;
import com.youdeyiwu.model.dto.user.CreateRoleDto;
import com.youdeyiwu.model.dto.user.UpdatePermissionsRoleDto;
import com.youdeyiwu.model.dto.user.UpdateRoleDto;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.user.RoleEntityVo;
import com.youdeyiwu.model.vo.user.RolePermissionsVo;
import com.youdeyiwu.service.user.SubmenuService;
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
 * submenu.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@RequestMapping(value = "/submenus")
@RestController
public class SubmenuController {

  private final SubmenuService submenuService;

  @PostMapping
  public ResponseEntity<Void> create(@Valid @RequestBody CreateRoleDto dto) {
    return ResponseEntity.created(URI.create("/roles/" + submenuService.create(dto).getId())).build();
  }

  @PostMapping("/{id}/add-permissions")
  public ResponseEntity<Void> addPermissions(
      @PathVariable Long id,
      @Valid @RequestBody AssignPermissionsDto dto
  ) {
    submenuService.addPermissions(id, dto);
    return ResponseEntity.noContent().build();
  }

  @PostMapping("/{id}/remove-permissions")
  public ResponseEntity<Void> removePermissions(
      @PathVariable Long id,
      @Valid @RequestBody AssignPermissionsDto dto
  ) {
    submenuService.removePermissions(id, dto);
    return ResponseEntity.noContent().build();
  }

  @PutMapping("/{id}/permissions")
  public ResponseEntity<Void> updatePermissions(
      @PathVariable Long id,
      @Valid @RequestBody UpdatePermissionsRoleDto dto
  ) {
    submenuService.updatePermissions(id, dto);
    return ResponseEntity.noContent().build();
  }

  @PutMapping("/{id}")
  public ResponseEntity<Void> update(
      @PathVariable Long id,
      @Valid @RequestBody UpdateRoleDto dto
  ) {
    submenuService.update(id, dto);
    return ResponseEntity.noContent().build();
  }

  @GetMapping("/{id}/permissions")
  public ResponseEntity<RolePermissionsVo> getPermissions(@PathVariable Long id) {
    return ResponseEntity.ok().body(submenuService.getPermissions(id));
  }

  @GetMapping(value = "/{id}")
  public ResponseEntity<RoleEntityVo> query(@PathVariable Long id) {
    return ResponseEntity.ok().body(submenuService.query(id));
  }

  @GetMapping
  public ResponseEntity<PageVo<RoleEntityVo>> queryAll(
      @PageableDefault(size = 15)
      @SortDefault(value = {"sort", "id"}, direction = Sort.Direction.DESC)
      Pageable pageable
  ) {
    return ResponseEntity.ok().body(submenuService.queryAll(pageable));
  }

  @DeleteMapping(value = "/{id}")
  public ResponseEntity<Void> delete(@PathVariable Long id) {
    submenuService.delete(id);
    return ResponseEntity.noContent().build();
  }
}