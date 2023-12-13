package com.youdeyiwu.controller.user;

import com.youdeyiwu.model.dto.user.CreateActionDto;
import com.youdeyiwu.model.dto.user.UpdateActionDto;
import com.youdeyiwu.model.vo.user.ActionEntityVo;
import com.youdeyiwu.service.user.ActionService;
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
 * action.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@RequestMapping(value = "/actions")
@RestController
public class ActionController {

  private final ActionService actionService;

  /**
   * create.
   *
   * @param dto dto
   * @return ResponseEntity
   */
  @PostMapping
  public ResponseEntity<Void> create(@Valid @RequestBody CreateActionDto dto) {
    return ResponseEntity
        .created(URI.create("/actions/" + actionService.create(dto).getId()))
        .build();
  }

  @PutMapping("/{id}")
  public ResponseEntity<Void> update(
      @PathVariable Long id,
      @Valid @RequestBody UpdateActionDto dto
  ) {
    actionService.update(id, dto);
    return ResponseEntity.noContent().build();
  }

  @GetMapping(value = "/{id}")
  public ResponseEntity<ActionEntityVo> query(@PathVariable Long id) {
    return ResponseEntity.ok().body(actionService.query(id));
  }

  @GetMapping
  public ResponseEntity<Set<ActionEntityVo>> queryAll() {
    return ResponseEntity.ok().body(actionService.queryAll());
  }

  @DeleteMapping(value = "/{id}")
  public ResponseEntity<Void> delete(@PathVariable Long id) {
    actionService.delete(id);
    return ResponseEntity.noContent().build();
  }
}