package com.youdeyiwu.controller.point;

import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.forum.TagEntityVo;
import com.youdeyiwu.model.vo.point.PointHistoryEntityVo;
import com.youdeyiwu.service.point.PointService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.data.web.SortDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * point.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@RequestMapping(value = "/points")
@RestController
public class PointController {

  private final PointService pointService;

  @GetMapping(value = "/histories")
  public ResponseEntity<PageVo<PointHistoryEntityVo>> queryAll(
      @PageableDefault(size = 15)
      @SortDefault(value = {"id"}, direction = Sort.Direction.DESC)
      Pageable pageable
  ) {
    return ResponseEntity.ok().body(pointService.queryAll(pageable));
  }
}