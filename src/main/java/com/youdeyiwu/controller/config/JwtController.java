package com.youdeyiwu.controller.config;

import com.youdeyiwu.model.dto.config.UpdateJwtConfigDto;
import com.youdeyiwu.model.vo.config.JwtConfigVo;
import com.youdeyiwu.service.config.JwtService;
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
public class JwtController {

  private final JwtService jwtService;

  @GetMapping(value = "/generate-random-secret")
  public ResponseEntity<String> generateRandomSecret() {
    return ResponseEntity.ok().body(jwtService.generateRandomSecret());
  }

  @GetMapping
  public ResponseEntity<JwtConfigVo> query() {
    return ResponseEntity.ok().body(jwtService.query());
  }

  @PutMapping
  public ResponseEntity<Void> update(@Valid @RequestBody UpdateJwtConfigDto dto) {
    jwtService.update(dto);
    return ResponseEntity.noContent().build();
  }
}