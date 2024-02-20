package com.youdeyiwu.controller.config;

import com.youdeyiwu.model.dto.config.UpdateJwtConfigDto;
import com.youdeyiwu.model.vo.config.JwtConfigVo;
import com.youdeyiwu.service.config.JwtConfigService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * jwt.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@RequestMapping(value = "/configs/jwt")
@RestController
public class JwtConfigController {

  private final JwtConfigService jwtConfigService;

  @GetMapping(value = "/generate-random-secret")
  public ResponseEntity<String> generateRandomSecret() {
    return ResponseEntity.ok(jwtConfigService.generateRandomSecret());
  }

  @GetMapping
  public ResponseEntity<JwtConfigVo> query() {
    return ResponseEntity.ok(jwtConfigService.query());
  }

  @PutMapping
  public ResponseEntity<Void> update(@Valid @RequestBody UpdateJwtConfigDto dto) {
    jwtConfigService.update(dto);
    return ResponseEntity.noContent().build();
  }
}