package com.youdeyiwu.controller.forum;

import com.youdeyiwu.model.dto.forum.CreateSectionDto;
import com.youdeyiwu.model.dto.forum.UpdateAdminsSectionDto;
import com.youdeyiwu.model.dto.forum.UpdateSectionDto;
import com.youdeyiwu.model.dto.forum.UpdateStatesSectionDto;
import com.youdeyiwu.model.dto.forum.UpdateTagGroupsSectionDto;
import com.youdeyiwu.model.dto.forum.UpdateTagsSectionDto;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.TokenVo;
import com.youdeyiwu.model.vo.forum.PostEntityVo;
import com.youdeyiwu.model.vo.forum.SectionEntityVo;
import com.youdeyiwu.service.forum.SectionService;
import jakarta.validation.Valid;
import java.net.URI;
import java.util.Set;
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
 * section.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@RequestMapping(value = "/sections")
@RestController
public class SectionController {

  private final SectionService sectionService;

  @PostMapping
  public ResponseEntity<TokenVo> create(@Valid @RequestBody CreateSectionDto dto) {
    return ResponseEntity.created(URI.create("/sections/" + sectionService.create(dto).getId()))
        .build();
  }

  @PutMapping(value = "/{id}/states")
  public ResponseEntity<Void> updateStates(
      @PathVariable Long id,
      @Valid @RequestBody UpdateStatesSectionDto dto
  ) {
    sectionService.updateStates(id, dto);
    return ResponseEntity.noContent().build();
  }

  @PutMapping(value = "/{id}/admins")
  public ResponseEntity<Void> updateAdmins(
      @PathVariable Long id,
      @Valid @RequestBody UpdateAdminsSectionDto dto
  ) {
    sectionService.updateAdmins(id, dto);
    return ResponseEntity.noContent().build();
  }

  @PutMapping(value = "/{id}/tags")
  public ResponseEntity<Void> updateTags(
      @PathVariable Long id,
      @Valid @RequestBody UpdateTagsSectionDto dto
  ) {
    sectionService.updateTags(id, dto);
    return ResponseEntity.noContent().build();
  }

  @PutMapping(value = "/{id}/tag-groups")
  public ResponseEntity<Void> updateTagGroups(
      @PathVariable Long id,
      @Valid @RequestBody UpdateTagGroupsSectionDto dto
  ) {
    sectionService.updateTagGroups(id, dto);
    return ResponseEntity.noContent().build();
  }

  @PutMapping(value = "/{id}")
  public ResponseEntity<Void> update(
      @PathVariable Long id,
      @Valid @RequestBody UpdateSectionDto dto
  ) {
    sectionService.update(id, dto);
    return ResponseEntity.noContent().build();
  }

  @GetMapping(value = "/select-all")
  public ResponseEntity<Set<SectionEntityVo>> selectAll() {
    return ResponseEntity.ok().body(sectionService.selectAll());
  }

  @GetMapping(value = "/{id}/details")
  public ResponseEntity<SectionEntityVo> queryDetails(@PathVariable Long id) {
    return ResponseEntity.ok().body(sectionService.queryDetails(id));
  }

  @GetMapping(value = "/{id}")
  public ResponseEntity<SectionEntityVo> query(@PathVariable Long id) {
    return ResponseEntity.ok().body(sectionService.query(id));
  }

  @GetMapping
  public ResponseEntity<PageVo<SectionEntityVo>> queryAll(
      @PageableDefault(size = 15)
      @SortDefault(value = {"sort", "id"}, direction = Sort.Direction.DESC)
      Pageable pageable
  ) {
    return ResponseEntity.ok().body(sectionService.queryAll(pageable));
  }

  @DeleteMapping(value = "/{id}")
  public ResponseEntity<Void> delete(@PathVariable Long id) {
    sectionService.delete(id);
    return ResponseEntity.noContent().build();
  }
}