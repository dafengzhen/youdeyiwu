package com.youdeyiwu.controller.data;

import com.youdeyiwu.model.dto.data.CreatePostImportDto;
import com.youdeyiwu.model.dto.data.CreateSectionGroupImportDto;
import com.youdeyiwu.model.dto.data.CreateSectionImportDto;
import com.youdeyiwu.model.dto.data.CreateTagGroupImportDto;
import com.youdeyiwu.model.dto.data.CreateTagImportDto;
import com.youdeyiwu.model.dto.data.CreateUserImportDto;
import com.youdeyiwu.service.data.ImportService;
import jakarta.validation.Valid;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * import.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@RequestMapping(value = "/imports")
@RestController
public class ImportController {

  private final ImportService importService;

  @PostMapping(value = "/users")
  public ResponseEntity<List<CreateUserImportDto.UserImportVo>> createUsers(
      @Valid @RequestBody CreateUserImportDto dto
  ) {
    return ResponseEntity.ok(importService.createUsers(dto));
  }

  @PostMapping(value = "/sections")
  public ResponseEntity<List<CreateSectionImportDto.SectionImportVo>> createSections(
      @Valid @RequestBody CreateSectionImportDto dto
  ) {
    return ResponseEntity.ok(importService.createSections(dto));
  }

  @PostMapping(value = "/section-groups")
  public ResponseEntity<List<CreateSectionGroupImportDto.SectionGroupImportVo>> createSectionGroups(
      @Valid @RequestBody CreateSectionGroupImportDto dto
  ) {
    return ResponseEntity.ok(importService.createSectionGroups(dto));
  }

  @PostMapping(value = "/tags")
  public ResponseEntity<List<CreateTagImportDto.TagImportVo>> createTags(
      @Valid @RequestBody CreateTagImportDto dto
  ) {
    return ResponseEntity.ok(importService.createTags(dto));
  }

  @PostMapping(value = "/tag-groups")
  public ResponseEntity<List<CreateTagGroupImportDto.TagGroupImportVo>> createTagGroups(
      @Valid @RequestBody CreateTagGroupImportDto dto
  ) {
    return ResponseEntity.ok(importService.createTagGroups(dto));
  }

  @PostMapping(value = "/posts")
  public ResponseEntity<List<CreatePostImportDto.PostImportVo>> createPosts(
      @Valid @RequestBody CreatePostImportDto dto
  ) {
    return ResponseEntity.ok(importService.createPosts(dto));
  }
}