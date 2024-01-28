package com.youdeyiwu.service.point.impl;

import com.youdeyiwu.mapper.point.PointMapper;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.point.PointHistoryEntityVo;
import com.youdeyiwu.repository.point.PointHistoryRepository;
import com.youdeyiwu.service.point.PointService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * point.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Service
public class PointServiceImpl implements PointService {

  private final PointHistoryRepository pointHistoryRepository;

  private final PointMapper pointMapper;

  @Override
  public PageVo<PointHistoryEntityVo> queryAll(Pageable pageable) {
    return new PageVo<>(pointHistoryRepository.findAll(pageable).map(pointMapper::entityToVo));
  }
}