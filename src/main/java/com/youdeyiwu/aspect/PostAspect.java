package com.youdeyiwu.aspect;

import com.youdeyiwu.event.PostReviewStateApplicationEvent;
import com.youdeyiwu.model.dto.forum.UpdateStatesPostDto;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.repository.forum.PostRepository;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.aspectj.lang.annotation.After;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

/**
 * post.
 *
 * @author dafengzhen
 */
@Aspect
@Component
@RequiredArgsConstructor
public class PostAspect {

  private final ApplicationEventPublisher publisher;

  private final PostRepository postRepository;

  /**
   * updateStates.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.forum.impl.PostServiceImpl.updateStates(..)) && args(id,dto)", argNames = "id,dto")
  public void updateStatesPointcut(Long id, UpdateStatesPostDto dto) {
    // Pointcut
  }

  /**
   * after advice.
   */
  @After(value = "updateStatesPointcut(id,dto)", argNames = "id,dto")
  public void updateStatesAfterAdvice(Long id, UpdateStatesPostDto dto) {
    PostEntity postEntity = postRepository.getReferenceById(id);

    if (
        Objects.nonNull(dto.reviewState())
            && dto.reviewState() != postEntity.getOldReviewState()
    ) {
      publisher.publishEvent(new PostReviewStateApplicationEvent(postEntity));
    }
  }
}
