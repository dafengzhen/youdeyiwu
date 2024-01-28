package com.youdeyiwu.controller.config;

import com.youdeyiwu.model.dto.config.UpdateJwtConfigDto;
import com.youdeyiwu.model.dto.config.UpdatePointConfigDto;
import com.youdeyiwu.model.vo.config.JwtConfigVo;
import com.youdeyiwu.model.vo.config.PointConfigVo;
import com.youdeyiwu.service.config.PointConfigService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * point.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@RequestMapping(value = "/configs/point")
@RestController
public class PointConfigController {

  private final PointConfigService pointConfigService;

  @GetMapping
  public ResponseEntity<PointConfigVo> query() {
    return ResponseEntity.ok().body(pointConfigService.query());
  }

  @PutMapping
  public ResponseEntity<Void> update(@Valid @RequestBody UpdatePointConfigDto dto) {
    pointConfigService.update(dto);
    return ResponseEntity.noContent().build();
  }
}