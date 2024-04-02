package com.youdeyiwu.controller.config;

import com.youdeyiwu.model.dto.config.UpdateCreateGuidePostConfigDto;
import com.youdeyiwu.service.config.PostConfigService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * post.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@RequestMapping(value = "/configs/post")
@RestController
public class PostConfigController {

  private final PostConfigService postConfigService;

  @GetMapping(value = "/create-guide")
  public ResponseEntity<String> queryCreateGuide() {
    return ResponseEntity.ok(postConfigService.queryCreateGuide());
  }

  @PutMapping(value = "/create-guide")
  public ResponseEntity<Void> updateCreateGuide(@Valid @RequestBody UpdateCreateGuidePostConfigDto dto) {
    postConfigService.updateCreateGuide(dto);
    return ResponseEntity.noContent().build();
  }
}