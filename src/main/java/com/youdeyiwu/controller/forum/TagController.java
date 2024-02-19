package com.youdeyiwu.controller.forum;

import com.youdeyiwu.model.dto.forum.CreateTagDto;
import com.youdeyiwu.model.dto.forum.UpdateTagDto;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.forum.TagEntityVo;
import com.youdeyiwu.service.forum.TagService;
import jakarta.validation.Valid;
import java.net.URI;
import java.util.List;
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
 * tag.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@RequestMapping(value = "/tags")
@RestController
public class TagController {

  private final TagService tagService;

  @PostMapping
  public ResponseEntity<Void> create(@Valid @RequestBody CreateTagDto dto) {
    return ResponseEntity.created(URI.create("/tags/" + tagService.create(dto, true).getId()))
        .build();
  }

  @PutMapping(value = "/{id}")
  public ResponseEntity<Void> update(
      @PathVariable Long id,
      @Valid @RequestBody UpdateTagDto dto
  ) {
    tagService.update(id, dto);
    return ResponseEntity.noContent().build();
  }

  @GetMapping
  public ResponseEntity<PageVo<TagEntityVo>> queryAll(
      @PageableDefault(size = 15)
      @SortDefault(value = {"sort", "id"}, direction = Sort.Direction.DESC)
      Pageable pageable
  ) {
    return ResponseEntity.ok(tagService.queryAll(pageable));
  }

  @GetMapping(value = "/select-all")
  public ResponseEntity<List<TagEntityVo>> selectAll() {
    return ResponseEntity.ok(tagService.selectAll());
  }

  @GetMapping(value = "/{id}")
  public ResponseEntity<TagEntityVo> query(@PathVariable Long id) {
    return ResponseEntity.ok(tagService.query(id));
  }

  @DeleteMapping(value = "/{id}")
  public ResponseEntity<Void> delete(@PathVariable Long id) {
    tagService.delete(id);
    return ResponseEntity.noContent().build();
  }
}