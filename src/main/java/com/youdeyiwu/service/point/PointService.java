package com.youdeyiwu.service.point;

import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.forum.TagEntityVo;
import com.youdeyiwu.model.vo.point.PointHistoryEntityVo;
import org.springframework.data.domain.Pageable;

/**
 * point.
 *
 * @author dafengzhen
 */
public interface PointService {

  /**
   * queryAll.
   *
   * @param pageable pageable
   * @return PageVo
   */
  PageVo<PointHistoryEntityVo> queryAll(Pageable pageable);
}