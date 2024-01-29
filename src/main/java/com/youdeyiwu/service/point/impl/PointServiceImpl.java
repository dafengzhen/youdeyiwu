package com.youdeyiwu.service.point.impl;

import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.mapper.point.PointMapper;
import com.youdeyiwu.model.entity.point.PointEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.point.PointHistoryEntityVo;
import com.youdeyiwu.repository.config.ConfigRepository;
import com.youdeyiwu.repository.point.PointHistoryRepository;
import com.youdeyiwu.repository.point.PointRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.service.point.PointCoreService;
import com.youdeyiwu.service.point.PointService;
import java.util.Objects;
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

  private final PointRepository pointRepository;

  private final PointHistoryRepository pointHistoryRepository;

  private final UserRepository userRepository;

  private final ConfigRepository configRepository;

  private final PointMapper pointMapper;

  private final PointCoreService pointCoreService;

  @Override
  public PageVo<PointHistoryEntityVo> queryAll(Pageable pageable) {
    return new PageVo<>(pointHistoryRepository.findAll(pageable).map(pointMapper::entityToVo));
  }

  @Override
  public PointEntity findPointByUserId(Long userId) {
    UserEntity userEntity = userRepository.findById(userId)
        .orElseThrow(UserNotFoundException::new);

    PointEntity pointEntity;
    if (Objects.isNull(userEntity.getPoint())) {
      return pointCoreService.create(userEntity);
    } else {
      pointEntity = userEntity.getPoint();
    }

    return pointEntity;
  }

  @Override
  public PointEntity findPointByUserEntity(UserEntity userEntity) {
    return findPointByUserId(userEntity.getId());
  }
}