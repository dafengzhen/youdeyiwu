package com.youdeyiwu.controller.user;

import com.youdeyiwu.model.dto.user.CreateSubmenuDto;
import com.youdeyiwu.model.dto.user.UpdateRolesSubmenuDto;
import com.youdeyiwu.model.dto.user.UpdateSubmenuDto;
import com.youdeyiwu.model.vo.user.SubmenuEntityVo;
import com.youdeyiwu.service.user.SubmenuService;
import jakarta.validation.Valid;
import java.net.URI;
import java.util.Set;
import lombok.RequiredArgsConstructor;
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

  /**
   * create.
   *
   * @param dto dto
   * @return ResponseEntity
   */
  @PostMapping
  public ResponseEntity<Void> create(@Valid @RequestBody CreateSubmenuDto dto) {
    return ResponseEntity
        .created(URI.create("/submenus/" + submenuService.create(dto).getId()))
        .build();
  }

  @PutMapping(value = "/{id}/roles")
  public ResponseEntity<Void> updateRoles(
      @PathVariable Long id,
      @Valid @RequestBody UpdateRolesSubmenuDto dto
  ) {
    submenuService.updateRoles(id, dto);
    return ResponseEntity.noContent().build();
  }

  @PutMapping("/{id}")
  public ResponseEntity<Void> update(
      @PathVariable Long id,
      @Valid @RequestBody UpdateSubmenuDto dto
  ) {
    submenuService.update(id, dto);
    return ResponseEntity.noContent().build();
  }

  @GetMapping(value = "/{id}")
  public ResponseEntity<SubmenuEntityVo> query(@PathVariable Long id) {
    return ResponseEntity.ok().body(submenuService.query(id));
  }

  @GetMapping
  public ResponseEntity<Set<SubmenuEntityVo>> queryAll() {
    return ResponseEntity.ok().body(submenuService.queryAll());
  }

  @DeleteMapping(value = "/{id}")
  public ResponseEntity<Void> delete(@PathVariable Long id) {
    submenuService.delete(id);
    return ResponseEntity.noContent().build();
  }
}