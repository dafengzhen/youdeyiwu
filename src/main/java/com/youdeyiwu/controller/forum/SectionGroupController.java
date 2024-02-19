package com.youdeyiwu.controller.forum;

import com.youdeyiwu.model.dto.forum.CreateSectionGroupDto;
import com.youdeyiwu.model.dto.forum.UpdateSectionGroupDto;
import com.youdeyiwu.model.dto.forum.UpdateSectionsSectionGroupDto;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.forum.SectionGroupEntityVo;
import com.youdeyiwu.service.forum.SectionGroupService;
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
 * section group.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@RequestMapping(value = "/section-groups")
@RestController
public class SectionGroupController {

  private final SectionGroupService sectionGroupService;

  /**
   * create.
   *
   * @param dto dto
   * @return ResponseEntity
   */
  @PostMapping
  public ResponseEntity<Void> create(@Valid @RequestBody CreateSectionGroupDto dto) {
    return ResponseEntity
        .created(URI.create("/section-groups/" + sectionGroupService.create(dto).getId()))
        .build();
  }

  @PutMapping(value = "/{id}/sections")
  public ResponseEntity<Void> updateSections(
      @PathVariable Long id,
      @Valid @RequestBody UpdateSectionsSectionGroupDto dto
  ) {
    sectionGroupService.updateSections(id, dto);
    return ResponseEntity.noContent().build();
  }

  @PutMapping(value = "/{id}")
  public ResponseEntity<Void> update(
      @PathVariable Long id,
      @Valid @RequestBody UpdateSectionGroupDto dto
  ) {
    sectionGroupService.update(id, dto);
    return ResponseEntity.noContent().build();
  }

  @GetMapping
  public ResponseEntity<PageVo<SectionGroupEntityVo>> queryAll(
      @PageableDefault(size = 15)
      @SortDefault(value = {"sort", "id"}, direction = Sort.Direction.DESC)
      Pageable pageable
  ) {
    return ResponseEntity.ok(sectionGroupService.queryAll(pageable));
  }

  @GetMapping(value = "/select-all")
  public ResponseEntity<List<SectionGroupEntityVo>> selectAll() {
    return ResponseEntity.ok(sectionGroupService.selectAll());
  }

  @GetMapping(value = "/{id}")
  public ResponseEntity<SectionGroupEntityVo> query(@PathVariable Long id) {
    return ResponseEntity.ok(sectionGroupService.query(id));
  }

  @DeleteMapping(value = "/{id}")
  public ResponseEntity<Void> delete(@PathVariable Long id) {
    sectionGroupService.delete(id);
    return ResponseEntity.noContent().build();
  }
}