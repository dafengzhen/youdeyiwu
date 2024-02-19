package com.youdeyiwu.controller.forum;

import com.youdeyiwu.model.dto.forum.CreateTagGroupDto;
import com.youdeyiwu.model.dto.forum.UpdateTagGroupDto;
import com.youdeyiwu.model.dto.forum.UpdateTagsTagGroupDto;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.forum.TagGroupEntityVo;
import com.youdeyiwu.service.forum.TagGroupService;
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
 * tag group.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@RequestMapping(value = "/tag-groups")
@RestController
public class TagGroupController {

  private final TagGroupService tagGroupService;

  @PostMapping
  public ResponseEntity<Void> create(@Valid @RequestBody CreateTagGroupDto dto) {
    return ResponseEntity.created(URI.create("/tag-groups/" + tagGroupService.create(dto).getId()))
        .build();
  }

  @PutMapping(value = "/{id}/tags")
  public ResponseEntity<Void> updateTags(
      @PathVariable Long id,
      @Valid @RequestBody UpdateTagsTagGroupDto dto
  ) {
    tagGroupService.updateTags(id, dto);
    return ResponseEntity.noContent().build();
  }

  @PutMapping(value = "/{id}")
  public ResponseEntity<Void> update(
      @PathVariable Long id,
      @Valid @RequestBody UpdateTagGroupDto dto
  ) {
    tagGroupService.update(id, dto);
    return ResponseEntity.noContent().build();
  }

  @GetMapping
  public ResponseEntity<PageVo<TagGroupEntityVo>> queryAll(
      @PageableDefault(size = 15)
      @SortDefault(value = {"sort", "id"}, direction = Sort.Direction.DESC)
      Pageable pageable
  ) {
    return ResponseEntity.ok(tagGroupService.queryAll(pageable));
  }

  @GetMapping(value = "/{id}")
  public ResponseEntity<TagGroupEntityVo> query(@PathVariable Long id) {
    return ResponseEntity.ok(tagGroupService.query(id));
  }

  @DeleteMapping(value = "/{id}")
  public ResponseEntity<Void> delete(@PathVariable Long id) {
    tagGroupService.delete(id);
    return ResponseEntity.noContent().build();
  }
}