package com.youdeyiwu.controller.user;

import com.youdeyiwu.model.dto.user.CreateMenuDto;
import com.youdeyiwu.model.dto.user.UpdateMenuDto;
import com.youdeyiwu.model.dto.user.UpdateRolesMenuDto;
import com.youdeyiwu.model.vo.user.MenuEntityVo;
import com.youdeyiwu.service.user.MenuService;
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
 * menu.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@RequestMapping(value = "/menus")
@RestController
public class MenuController {

  private final MenuService menuService;

  @PostMapping
  public ResponseEntity<Void> create(@Valid @RequestBody CreateMenuDto dto) {
    return ResponseEntity.created(URI.create("/menus/" + menuService.create(dto).getId())).build();
  }

  @PutMapping(value = "/{id}/roles")
  public ResponseEntity<Void> updateRoles(
      @PathVariable Long id,
      @Valid @RequestBody UpdateRolesMenuDto dto
  ) {
    menuService.updateRoles(id, dto);
    return ResponseEntity.noContent().build();
  }

  @PutMapping("/{id}")
  public ResponseEntity<Void> update(
      @PathVariable Long id,
      @Valid @RequestBody UpdateMenuDto dto
  ) {
    menuService.update(id, dto);
    return ResponseEntity.noContent().build();
  }

  @GetMapping(value = "/{id}")
  public ResponseEntity<MenuEntityVo> query(@PathVariable Long id) {
    return ResponseEntity.ok(menuService.query(id));
  }

  @GetMapping
  public ResponseEntity<Set<MenuEntityVo>> queryAll() {
    return ResponseEntity.ok(menuService.queryAll());
  }

  @DeleteMapping(value = "/{id}")
  public ResponseEntity<Void> delete(@PathVariable Long id) {
    menuService.delete(id);
    return ResponseEntity.noContent().build();
  }
}