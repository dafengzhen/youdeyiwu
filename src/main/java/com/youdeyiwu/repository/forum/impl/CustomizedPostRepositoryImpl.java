package com.youdeyiwu.repository.forum.impl;

import static com.youdeyiwu.tool.PostSorterTool.TOTAL_TIME_SECONDS;

import com.youdeyiwu.enums.forum.PostStateEnum;
import com.youdeyiwu.model.dto.PaginationPositionDto;
import com.youdeyiwu.model.dto.forum.QueryParamsPost;
import com.youdeyiwu.model.dto.forum.TypedQueryPostPage;
import com.youdeyiwu.model.entity.forum.CommentEntity;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.forum.PostUserEntity;
import com.youdeyiwu.model.entity.forum.QuoteReplyEntity;
import com.youdeyiwu.model.entity.forum.SectionEntity;
import com.youdeyiwu.model.entity.forum.SectionGroupEntity;
import com.youdeyiwu.model.entity.forum.TagEntity;
import com.youdeyiwu.model.entity.forum.TagGroupEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.vo.forum.CommentReplyEntityVo;
import com.youdeyiwu.repository.forum.CustomizedPostRepository;
import jakarta.persistence.EntityManager;
import jakarta.persistence.Tuple;
import jakarta.persistence.TypedQuery;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

/**
 * post.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Repository
public class CustomizedPostRepositoryImpl implements CustomizedPostRepository {

  private final EntityManager entityManager;

  @Override
  public List<PostEntity> findRandomPosts() {
    Tuple tuple = entityManager.createQuery(
            "SELECT MIN(p.id) as min, MAX(p.id) as max FROM PostEntity p",
            Tuple.class
        )
        .getSingleResult();
    Object min = tuple.get(0);
    Object max = tuple.get(1);
    //noinspection unchecked
    return entityManager.createNativeQuery("""
            SELECT a.*
            FROM post_entity a
                     JOIN (SELECT id
                           FROM (SELECT id
                                 FROM (SELECT %s + (%s - %s + 1 - 50) * RAND()
                                                  AS start
                                       FROM DUAL) AS init
                                          JOIN post_entity y
                                 WHERE y.id > init.start
                                 ORDER BY y.id
                                 LIMIT 50) z
                           ORDER BY RAND()
                           LIMIT 5) r ON a.id = r.id;
            """.formatted(min, max, min), PostEntity.class)
        .getResultStream()
        .toList();
  }

  @Override
  public Page<PostEntity> findAll(
      PaginationPositionDto position,
      QueryParamsPost dto,
      String accessKey,
      Boolean isAnonymous,
      UserEntity user,
      UserEntity root
  ) {
    TypedQuery<PostEntity> query;
    TypedQuery<Long> totalSizeQuery;

    if (Boolean.TRUE.equals(isAnonymous)) {
      TypedQueryPostPage typedQuery = queryAnonymousUserPosts(dto, accessKey);
      query = typedQuery.query();
      totalSizeQuery = typedQuery.totalSizeQuery();
    } else if (Objects.nonNull(root) || Objects.isNull(user)) {
      TypedQueryPostPage typedQuery = queryRootUserPosts(dto);
      query = typedQuery.query();
      totalSizeQuery = typedQuery.totalSizeQuery();
    } else {
      TypedQueryPostPage typedQuery = queryUserPosts(dto, accessKey, user);
      query = typedQuery.query();
      totalSizeQuery = typedQuery.totalSizeQuery();
    }

    return new PageImpl<>(
        query.setFirstResult(position.firstResult())
            .setMaxResults(position.maxResults())
            .getResultList(),
        position.pageable(),
        totalSizeQuery.getSingleResult()
    );
  }

  @Override
  public Page<CommentReplyEntityVo> findAllCommentReply(
      PaginationPositionDto position,
      PostEntity postEntity,
      Boolean isAnonymous,
      UserEntity user,
      UserEntity root
  ) {
    TypedQuery<Tuple> query;
    TypedQuery<Tuple> totalSizeQuery;

    if (Boolean.TRUE.equals(isAnonymous)) {
      query = entityManager.createQuery(
              """
                      select c, r from CommentEntity c left join fetch QuoteReplyEntity r
                      on c.id = r.comment.id
                      where c.post = :post and (c.reviewState = 0 or r.reviewState = 0)
                      order by c.id, r.id
                  """,
              Tuple.class
          )
          .setParameter("post", postEntity);

      totalSizeQuery = entityManager.createQuery(
              """
                      select count(c), count(r) from CommentEntity c left join QuoteReplyEntity r
                      on c.id = r.comment.id
                      where c.post = :post and (c.reviewState = 0 or r.reviewState = 0)
                  """,
              Tuple.class
          )
          .setParameter("post", postEntity);
    } else if (Objects.nonNull(root) || Objects.isNull(user)) {
      query = entityManager.createQuery(
              """
                      select c, r from CommentEntity c left join fetch QuoteReplyEntity r
                      on c.id = r.comment.id
                      where c.post = :post
                      order by c.id, r.id
                  """,
              Tuple.class
          )
          .setParameter("post", postEntity);

      totalSizeQuery = entityManager.createQuery(
              """
                      select count(c), count(r) from CommentEntity c left join QuoteReplyEntity r
                      on c.id = r.comment.id
                      where c.post = :post
                  """,
              Tuple.class
          )
          .setParameter("post", postEntity);
    } else {
      query = entityManager.createQuery(
              """
                      select c, r from CommentEntity c left join fetch QuoteReplyEntity r
                      on c.id = r.comment.id
                      where c.post = :post and (c.reviewState = 0 or r.reviewState = 0)
                      or (c.user = :user or r.user = :user)
                      order by c.id, r.id
                  """,
              Tuple.class
          )
          .setParameter("post", postEntity)
          .setParameter("user", user);

      totalSizeQuery = entityManager.createQuery(
              """
                      select count(c), count(r) from CommentEntity c left join QuoteReplyEntity r
                      on c.id = r.comment.id
                      where c.post = :post and (c.reviewState = 0 or r.reviewState = 0)
                      or (c.user = :user or r.user = :user)
                  """,
              Tuple.class
          )
          .setParameter("post", postEntity)
          .setParameter("user", user);
    }

    List<Tuple> tuples = query
        .setFirstResult(position.firstResult())
        .setMaxResults(position.maxResults())
        .getResultList();

    Tuple result = totalSizeQuery.getSingleResult();
    long totalSize = (long) result.get(0) + (long) result.get(1);

    List<CommentReplyEntityVo> commentReplyEntityVos = tuples.stream()
        .flatMap(tuple -> Stream.of(tuple.get(0), tuple.get(1)))
        .filter(Objects::nonNull)
        .map(obj -> {
          CommentReplyEntityVo vo = new CommentReplyEntityVo();
          if (obj instanceof CommentEntity commententity) {
            vo.setComment(commententity);
          } else if (obj instanceof QuoteReplyEntity quoteReplyEntity) {
            vo.setReply(quoteReplyEntity);
          }
          return vo;
        })
        .distinct()
        .toList();

    return new PageImpl<>(commentReplyEntityVos, position.pageable(), totalSize);
  }

  @Override
  public Page<PostEntity> findPostReviewQueues(
      PaginationPositionDto position,
      Boolean isAnonymous,
      UserEntity user,
      UserEntity root
  ) {
    TypedQuery<PostEntity> query;
    TypedQuery<Long> totalSizeQuery;

    if (Boolean.TRUE.equals(isAnonymous) || Objects.nonNull(root) || Objects.isNull(user)) {
      query = entityManager.createQuery(
          """
              select p from PostEntity p
              where p.reviewState = 2
              order by p.initialScore desc, p.sortState desc, p.id desc
              """,
          PostEntity.class
      );

      totalSizeQuery = entityManager.createQuery(
          """
              select count(p.id) from PostEntity p
              where p.reviewState = 2
              """,
          Long.class
      );
    } else {
      query = entityManager.createQuery(
              """
                  select p from PostEntity p left join fetch p.section ps
                  where p.reviewState = 2
                  and :user member of ps.admins
                  order by p.initialScore desc, p.sortState desc, p.id desc
                  """,
              PostEntity.class
          )
          .setParameter("user", user);

      totalSizeQuery = entityManager.createQuery(
              """
                  select count(p.id) from PostEntity p left join p.section ps
                  where p.reviewState = 2
                  and :user member of ps.admins
                  """,
              Long.class
          )
          .setParameter("user", user);
    }

    return new PageImpl<>(
        query.setFirstResult(position.firstResult())
            .setMaxResults(position.maxResults())
            .getResultList(),
        position.pageable(),
        totalSizeQuery.getSingleResult()
    );
  }

  @Override
  public Page<PostUserEntity> findPostUsersByPost(PaginationPositionDto position, PostEntity post) {
    return new PageImpl<>(
        entityManager.createQuery(
                """
                    select pu from PostUserEntity pu
                    where pu.post = :post
                    order by pu.user.id desc
                    """,
                PostUserEntity.class
            )
            .setFirstResult(position.firstResult())
            .setMaxResults(position.maxResults())
            .setParameter("post", post)
            .getResultList(),
        position.pageable(),
        entityManager.createQuery(
                """
                    select count(pu.user.id) from PostUserEntity pu
                    where pu.post = :post
                    """,
                Long.class
            )
            .setParameter("post", post)
            .getSingleResult()
    );
  }

  @Override
  public PostUserEntity findPostUserByPostAndUser(PostEntity post, UserEntity user) {
    return entityManager.createQuery(
            """
                select pu from PostUserEntity pu
                where pu.post = :post and pu.user = :user
                """,
            PostUserEntity.class
        )
        .setParameter("post", post)
        .setParameter("user", user)
        .getSingleResult();
  }

  /**
   * query anonymous user posts.
   *
   * @param dto       dto
   * @param accessKey accessKey
   * @return TypedQueryPostPage
   */
  @SuppressWarnings("JpaQlInspection")
  private TypedQueryPostPage queryAnonymousUserPosts(
      QueryParamsPost dto,
      String accessKey
  ) {
    final int totalTimeSeconds = TOTAL_TIME_SECONDS;
    SectionGroupEntity sectionGroupEntity = dto.sectionGroup();
    SectionEntity sectionEntity = dto.section();
    TagGroupEntity tagGroupEntity = dto.tagGroup();
    TagEntity tagEntity = dto.tag();

    TypedQuery<PostEntity> query;
    TypedQuery<Long> totalSizeQuery;
    if (Objects.nonNull(sectionGroupEntity)) {
      query = entityManager.createQuery(
              """
                  select p from PostEntity p
                  where :sectionGroup member of p.section.sectionGroups
                  and p.reviewState = 0
                  and (:show member of p.states
                  or (:lock member of p.states and p.accessKey = :accessKey))
                  order by p.sortState desc, p.initialScore desc,
                    (p.pageViews + p.commentsCount + p.repliesCount) *
                    (1 - timestampdiff(second, p.createdOn, now()) / :totalTimeSeconds) desc,
                    p.createdOn desc
                  """,
              PostEntity.class
          )
          .setParameter("sectionGroup", sectionGroupEntity)
          .setParameter("show", PostStateEnum.SHOW)
          .setParameter("lock", PostStateEnum.LOCK)
          .setParameter("accessKey", accessKey)
          .setParameter("totalTimeSeconds", totalTimeSeconds);

      totalSizeQuery = entityManager.createQuery(
              """
                  select count(p.id) from PostEntity p
                  where :sectionGroup member of p.section.sectionGroups
                  and p.reviewState = 0
                  and (:show member of p.states
                  or (:lock member of p.states and p.accessKey = :accessKey))
                  """,
              Long.class
          )
          .setParameter("sectionGroup", sectionGroupEntity)
          .setParameter("show", PostStateEnum.SHOW)
          .setParameter("lock", PostStateEnum.LOCK)
          .setParameter("accessKey", accessKey);
    } else if (Objects.nonNull(sectionEntity)) {
      query = entityManager.createQuery(
              """
                  select p from PostEntity p
                  where p.section = :section
                  and p.reviewState = 0
                  and (:show member of p.states
                  or (:lock member of p.states and p.accessKey = :accessKey))
                  order by p.sortState desc, p.initialScore desc,
                    (p.pageViews + p.commentsCount + p.repliesCount) *
                    (1 - timestampdiff(second, p.createdOn, now()) / :totalTimeSeconds) desc,
                    p.createdOn desc
                  """,
              PostEntity.class
          )
          .setParameter("section", sectionEntity)
          .setParameter("show", PostStateEnum.SHOW)
          .setParameter("lock", PostStateEnum.LOCK)
          .setParameter("accessKey", accessKey)
          .setParameter("totalTimeSeconds", totalTimeSeconds);

      totalSizeQuery = entityManager.createQuery(
              """
                  select count(p.id) from PostEntity p
                  where p.section = :section
                  and p.reviewState = 0
                  and (:show member of p.states
                  or (:lock member of p.states and p.accessKey = :accessKey))
                  """,
              Long.class
          )
          .setParameter("section", sectionEntity)
          .setParameter("show", PostStateEnum.SHOW)
          .setParameter("lock", PostStateEnum.LOCK)
          .setParameter("accessKey", accessKey);
    } else if (Objects.nonNull(tagGroupEntity)) {
      query = entityManager.createQuery(
              """
                  select p from PostEntity p, TagEntity t
                  where t member of p.tags and :tagGroup member of t.tagGroups
                  and p.reviewState = 0
                  and (:show member of p.states
                  or (:lock member of p.states and p.accessKey = :accessKey))
                  order by p.sortState desc, p.initialScore desc,
                    (p.pageViews + p.commentsCount + p.repliesCount) *
                    (1 - timestampdiff(second, p.createdOn, now()) / :totalTimeSeconds) desc,
                    p.createdOn desc
                  """,
              PostEntity.class
          )
          .setParameter("tagGroup", tagGroupEntity)
          .setParameter("show", PostStateEnum.SHOW)
          .setParameter("lock", PostStateEnum.LOCK)
          .setParameter("accessKey", accessKey)
          .setParameter("totalTimeSeconds", totalTimeSeconds);

      totalSizeQuery = entityManager.createQuery(
              """
                  select count(p.id) from PostEntity p, TagEntity t
                  where t member of p.tags and :tagGroup member of t.tagGroups
                  and p.reviewState = 0
                  and (:show member of p.states
                  or (:lock member of p.states and p.accessKey = :accessKey))
                  """,
              Long.class
          )
          .setParameter("tagGroup", tagGroupEntity)
          .setParameter("show", PostStateEnum.SHOW)
          .setParameter("lock", PostStateEnum.LOCK)
          .setParameter("accessKey", accessKey);
    } else if (Objects.nonNull(tagEntity)) {
      query = entityManager.createQuery(
              """
                  select p from PostEntity p
                  where :tag member of p.tags
                  and p.reviewState = 0
                  and (:show member of p.states
                  or (:lock member of p.states and p.accessKey = :accessKey))
                  order by p.sortState desc, p.initialScore desc,
                    (p.pageViews + p.commentsCount + p.repliesCount) *
                    (1 - timestampdiff(second, p.createdOn, now()) / :totalTimeSeconds) desc,
                    p.createdOn desc
                  """,
              PostEntity.class
          )
          .setParameter("tag", tagEntity)
          .setParameter("show", PostStateEnum.SHOW)
          .setParameter("lock", PostStateEnum.LOCK)
          .setParameter("accessKey", accessKey)
          .setParameter("totalTimeSeconds", totalTimeSeconds);

      totalSizeQuery = entityManager.createQuery(
              """
                  select count(p.id) from PostEntity p
                  where :tag member of p.tags
                  and p.reviewState = 0
                  and (:show member of p.states
                  or (:lock member of p.states and p.accessKey = :accessKey))
                  """,
              Long.class
          )
          .setParameter("tag", tagEntity)
          .setParameter("show", PostStateEnum.SHOW)
          .setParameter("lock", PostStateEnum.LOCK)
          .setParameter("accessKey", accessKey);
    } else {
      query = entityManager.createQuery(
              """
                  select p from PostEntity p
                  where p.reviewState = 0
                  and (:show member of p.states
                  or (:lock member of p.states and p.accessKey = :accessKey))
                  order by p.sortState desc, p.initialScore desc,
                    (p.pageViews + p.commentsCount + p.repliesCount) *
                    (1 - timestampdiff(second, p.createdOn, now()) / :totalTimeSeconds) desc,
                    p.createdOn desc
                  """,
              PostEntity.class
          )
          .setParameter("show", PostStateEnum.SHOW)
          .setParameter("lock", PostStateEnum.LOCK)
          .setParameter("accessKey", accessKey)
          .setParameter("totalTimeSeconds", totalTimeSeconds);

      totalSizeQuery = entityManager.createQuery(
              """
                  select count(p.id) from PostEntity p
                  where p.reviewState = 0
                  and (:show member of p.states
                  or (:lock member of p.states and p.accessKey = :accessKey))
                  """,
              Long.class
          )
          .setParameter("show", PostStateEnum.SHOW)
          .setParameter("lock", PostStateEnum.LOCK)
          .setParameter("accessKey", accessKey);
    }

    return new TypedQueryPostPage(query, totalSizeQuery);
  }

  /**
   * query root user posts.
   *
   * @param dto dto
   * @return TypedQueryPostPage
   */
  @SuppressWarnings("JpaQlInspection")
  private TypedQueryPostPage queryRootUserPosts(QueryParamsPost dto) {
    final int totalTimeSeconds = TOTAL_TIME_SECONDS;
    SectionGroupEntity sectionGroupEntity = dto.sectionGroup();
    SectionEntity sectionEntity = dto.section();
    TagGroupEntity tagGroupEntity = dto.tagGroup();
    TagEntity tagEntity = dto.tag();

    TypedQuery<PostEntity> query;
    TypedQuery<Long> totalSizeQuery;
    if (Objects.nonNull(sectionGroupEntity)) {
      query = entityManager.createQuery(
              """
                  select p from PostEntity p
                  where :sectionGroup member of p.section.sectionGroups
                  order by p.sortState desc, p.initialScore desc,
                    (p.pageViews + p.commentsCount + p.repliesCount) *
                    (1 - timestampdiff(second, p.createdOn, now()) / :totalTimeSeconds) desc,
                    p.createdOn desc
                  """,
              PostEntity.class
          )
          .setParameter("sectionGroup", sectionGroupEntity)
          .setParameter("totalTimeSeconds", totalTimeSeconds);

      totalSizeQuery = entityManager.createQuery(
              """
                  select count(p.id) from PostEntity p
                  where :sectionGroup member of p.section.sectionGroups
                  """,
              Long.class
          )
          .setParameter("sectionGroup", sectionGroupEntity);
    } else if (Objects.nonNull(sectionEntity)) {
      query = entityManager.createQuery(
              """
                  select p from PostEntity p
                  where p.section = :section
                  order by p.sortState desc, p.initialScore desc,
                    (p.pageViews + p.commentsCount + p.repliesCount) *
                    (1 - timestampdiff(second, p.createdOn, now()) / :totalTimeSeconds) desc,
                    p.createdOn desc
                  """,
              PostEntity.class
          )
          .setParameter("section", sectionEntity)
          .setParameter("totalTimeSeconds", totalTimeSeconds);

      totalSizeQuery = entityManager.createQuery(
              """
                  select count(p.id) from PostEntity p
                  where p.section = :section
                  """,
              Long.class
          )
          .setParameter("section", sectionEntity);
    } else if (Objects.nonNull(tagGroupEntity)) {
      query = entityManager.createQuery(
              """
                  select p from PostEntity p, TagEntity t
                  where t member of p.tags and :tagGroup member of t.tagGroups
                  order by p.sortState desc, p.initialScore desc,
                    (p.pageViews + p.commentsCount + p.repliesCount) *
                    (1 - timestampdiff(second, p.createdOn, now()) / :totalTimeSeconds) desc,
                    p.createdOn desc
                  """,
              PostEntity.class
          )
          .setParameter("tagGroup", tagGroupEntity)
          .setParameter("totalTimeSeconds", totalTimeSeconds);

      totalSizeQuery = entityManager.createQuery(
              """
                  select count(p.id) from PostEntity p, TagEntity t
                  where t member of p.tags and :tagGroup member of t.tagGroups
                  """,
              Long.class
          )
          .setParameter("tagGroup", tagGroupEntity);
    } else if (Objects.nonNull(tagEntity)) {
      query = entityManager.createQuery(
              """
                  select p from PostEntity p
                  where :tag member of p.tags
                  order by p.sortState desc, p.initialScore desc,
                    (p.pageViews + p.commentsCount + p.repliesCount) *
                    (1 - timestampdiff(second, p.createdOn, now()) / :totalTimeSeconds) desc,
                    p.createdOn desc
                  """,
              PostEntity.class
          )
          .setParameter("tag", tagEntity)
          .setParameter("totalTimeSeconds", totalTimeSeconds);

      totalSizeQuery = entityManager.createQuery(
              """
                  select count(p.id) from PostEntity p
                  where :tag member of p.tags
                  """,
              Long.class
          )
          .setParameter("tag", tagEntity);
    } else {
      query = entityManager.createQuery(
              """
                  select p from PostEntity p
                  where :show member of p.states
                  order by p.sortState desc, p.initialScore desc,
                    (p.pageViews + p.commentsCount + p.repliesCount) *
                    (1 - timestampdiff(second, p.createdOn, now()) / :totalTimeSeconds) desc,
                    p.createdOn desc
                  """,
              PostEntity.class
          )
          .setParameter("show", PostStateEnum.SHOW)
          .setParameter("totalTimeSeconds", totalTimeSeconds);

      totalSizeQuery = entityManager.createQuery(
              """
                  select count(p.id) from PostEntity p
                  where :show member of p.states
                  """,
              Long.class
          )
          .setParameter("show", PostStateEnum.SHOW);
    }

    return new TypedQueryPostPage(query, totalSizeQuery);
  }

  /**
   * query user posts.
   *
   * @param dto       dto
   * @param accessKey accessKey
   * @return TypedQueryPostPage
   */
  @SuppressWarnings("JpaQlInspection")
  private TypedQueryPostPage queryUserPosts(
      QueryParamsPost dto,
      String accessKey,
      UserEntity user
  ) {
    final int totalTimeSeconds = TOTAL_TIME_SECONDS;
    SectionGroupEntity sectionGroupEntity = dto.sectionGroup();
    SectionEntity sectionEntity = dto.section();
    TagGroupEntity tagGroupEntity = dto.tagGroup();
    TagEntity tagEntity = dto.tag();

    TypedQuery<PostEntity> query;
    TypedQuery<Long> totalSizeQuery;
    if (Objects.nonNull(sectionGroupEntity)) {
      query = entityManager.createQuery(
              """
                  select p from PostEntity p left join fetch p.section ps
                  where :sectionGroup member of ps.sectionGroups
                  and p.reviewState = 0
                  and (:show member of p.states
                  or (:hide member of p.states and (:user member of ps.admins or :user member of p.allows))
                  or (:lock member of p.states and (p.accessKey = :accessKey or :user member of p.allows))
                  or (:block member of p.states and not (:user member of p.blocks))
                  or p.user = :user)
                  order by p.sortState desc, p.initialScore desc,
                    (p.pageViews + p.commentsCount + p.repliesCount) *
                    (1 - timestampdiff(second, p.createdOn, now()) / :totalTimeSeconds) desc,
                    p.createdOn desc
                  """,
              PostEntity.class
          )
          .setParameter("sectionGroup", sectionGroupEntity)
          .setParameter("show", PostStateEnum.SHOW)
          .setParameter("hide", PostStateEnum.HIDE)
          .setParameter("lock", PostStateEnum.LOCK)
          .setParameter("accessKey", accessKey)
          .setParameter("block", PostStateEnum.BLOCK)
          .setParameter("user", user)
          .setParameter("totalTimeSeconds", totalTimeSeconds);

      totalSizeQuery = entityManager.createQuery(
              """
                  select count(p.id) from PostEntity p left join p.section ps
                  where :sectionGroup member of ps.sectionGroups
                  and p.reviewState = 0
                  and (:show member of p.states
                  or (:hide member of p.states and (:user member of ps.admins or :user member of p.allows))
                  or (:lock member of p.states and (p.accessKey = :accessKey or :user member of p.allows))
                  or (:block member of p.states and not (:user member of p.blocks))
                  or p.user = :user)
                  """,
              Long.class
          )
          .setParameter("sectionGroup", sectionGroupEntity)
          .setParameter("show", PostStateEnum.SHOW)
          .setParameter("hide", PostStateEnum.HIDE)
          .setParameter("lock", PostStateEnum.LOCK)
          .setParameter("accessKey", accessKey)
          .setParameter("block", PostStateEnum.BLOCK)
          .setParameter("user", user);
    } else if (Objects.nonNull(sectionEntity)) {
      query = entityManager.createQuery(
              """
                  select p from PostEntity p left join fetch p.section ps
                  where ps = :section
                  and p.reviewState = 0
                  and (:show member of p.states
                  or (:hide member of p.states and (:user member of ps.admins or :user member of p.allows))
                  or (:lock member of p.states and (p.accessKey = :accessKey or :user member of p.allows))
                  or (:block member of p.states and not (:user member of p.blocks))
                  or p.user = :user)
                  order by p.sortState desc, p.initialScore desc,
                    (p.pageViews + p.commentsCount + p.repliesCount) *
                    (1 - timestampdiff(second, p.createdOn, now()) / :totalTimeSeconds) desc,
                    p.createdOn desc
                  """,
              PostEntity.class
          )
          .setParameter("section", sectionEntity)
          .setParameter("show", PostStateEnum.SHOW)
          .setParameter("hide", PostStateEnum.HIDE)
          .setParameter("lock", PostStateEnum.LOCK)
          .setParameter("accessKey", accessKey)
          .setParameter("block", PostStateEnum.BLOCK)
          .setParameter("user", user)
          .setParameter("totalTimeSeconds", totalTimeSeconds);

      totalSizeQuery = entityManager.createQuery(
              """
                  select count(p.id) from PostEntity p left join p.section ps
                  where ps = :section
                  and p.reviewState = 0
                  and (:show member of p.states
                  or (:hide member of p.states and (:user member of ps.admins or :user member of p.allows))
                  or (:lock member of p.states and (p.accessKey = :accessKey or :user member of p.allows))
                  or (:block member of p.states and not (:user member of p.blocks))
                  or p.user = :user)
                  """,
              Long.class
          )
          .setParameter("section", sectionEntity)
          .setParameter("show", PostStateEnum.SHOW)
          .setParameter("hide", PostStateEnum.HIDE)
          .setParameter("lock", PostStateEnum.LOCK)
          .setParameter("accessKey", accessKey)
          .setParameter("block", PostStateEnum.BLOCK)
          .setParameter("user", user);
    } else if (Objects.nonNull(tagGroupEntity)) {
      query = entityManager.createQuery(
              """
                  select p from PostEntity p, TagEntity t left join fetch p.section ps
                  where t member of p.tags and :tagGroup member of t.tagGroups
                  and p.reviewState = 0
                  and (:show member of p.states
                  or (:hide member of p.states and (:user member of ps.admins or :user member of p.allows))
                  or (:lock member of p.states and (p.accessKey = :accessKey or :user member of p.allows))
                  or (:block member of p.states and not (:user member of p.blocks))
                  or p.user = :user)
                  order by p.sortState desc, p.initialScore desc,
                    (p.pageViews + p.commentsCount + p.repliesCount) *
                    (1 - timestampdiff(second, p.createdOn, now()) / :totalTimeSeconds) desc,
                    p.createdOn desc
                  """,
              PostEntity.class
          )
          .setParameter("tagGroup", tagGroupEntity)
          .setParameter("show", PostStateEnum.SHOW)
          .setParameter("hide", PostStateEnum.HIDE)
          .setParameter("lock", PostStateEnum.LOCK)
          .setParameter("accessKey", accessKey)
          .setParameter("block", PostStateEnum.BLOCK)
          .setParameter("user", user)
          .setParameter("totalTimeSeconds", totalTimeSeconds);

      totalSizeQuery = entityManager.createQuery(
              """
                  select count(p.id) from PostEntity p, TagEntity t left join p.section ps
                  where t member of p.tags and :tagGroup member of t.tagGroups
                  and p.reviewState = 0
                  and (:show member of p.states
                  or (:hide member of p.states and (:user member of ps.admins or :user member of p.allows))
                  or (:lock member of p.states and (p.accessKey = :accessKey or :user member of p.allows))
                  or (:block member of p.states and not (:user member of p.blocks))
                  or p.user = :user)
                  """,
              Long.class
          )
          .setParameter("tagGroup", tagGroupEntity)
          .setParameter("show", PostStateEnum.SHOW)
          .setParameter("hide", PostStateEnum.HIDE)
          .setParameter("lock", PostStateEnum.LOCK)
          .setParameter("accessKey", accessKey)
          .setParameter("block", PostStateEnum.BLOCK)
          .setParameter("user", user);
    } else if (Objects.nonNull(tagEntity)) {
      query = entityManager.createQuery(
              """
                  select p from PostEntity p left join fetch p.section ps
                  where :tag member of p.tags
                  and p.reviewState = 0
                  and (:show member of p.states
                  or (:hide member of p.states and (:user member of ps.admins or :user member of p.allows))
                  or (:lock member of p.states and (p.accessKey = :accessKey or :user member of p.allows))
                  or (:block member of p.states and not (:user member of p.blocks))
                  or p.user = :user)
                  order by p.sortState desc, p.initialScore desc,
                    (p.pageViews + p.commentsCount + p.repliesCount) *
                    (1 - timestampdiff(second, p.createdOn, now()) / :totalTimeSeconds) desc,
                    p.createdOn desc
                  """,
              PostEntity.class
          )
          .setParameter("tag", tagEntity)
          .setParameter("show", PostStateEnum.SHOW)
          .setParameter("hide", PostStateEnum.HIDE)
          .setParameter("lock", PostStateEnum.LOCK)
          .setParameter("accessKey", accessKey)
          .setParameter("block", PostStateEnum.BLOCK)
          .setParameter("user", user)
          .setParameter("totalTimeSeconds", totalTimeSeconds);

      totalSizeQuery = entityManager.createQuery(
              """
                  select count(p.id) from PostEntity p left join p.section ps
                  where :tag member of p.tags
                  and p.reviewState = 0
                  and (:show member of p.states
                  or (:hide member of p.states and (:user member of ps.admins or :user member of p.allows))
                  or (:lock member of p.states and (p.accessKey = :accessKey or :user member of p.allows))
                  or (:block member of p.states and not (:user member of p.blocks))
                  or p.user = :user)
                  """,
              Long.class
          )
          .setParameter("tag", tagEntity)
          .setParameter("show", PostStateEnum.SHOW)
          .setParameter("hide", PostStateEnum.HIDE)
          .setParameter("lock", PostStateEnum.LOCK)
          .setParameter("accessKey", accessKey)
          .setParameter("block", PostStateEnum.BLOCK)
          .setParameter("user", user);
    } else {
      query = entityManager.createQuery(
              """
                  select p from PostEntity p left join fetch p.section ps
                  where p.reviewState = 0
                  and (:show member of p.states
                  or (:hide member of p.states and (:user member of ps.admins or :user member of p.allows))
                  or (:lock member of p.states and (p.accessKey = :accessKey or :user member of p.allows))
                  or (:block member of p.states and not (:user member of p.blocks))
                  or p.user = :user)
                  order by p.sortState desc, p.initialScore desc,
                    (p.pageViews + p.commentsCount + p.repliesCount) *
                    (1 - timestampdiff(second, p.createdOn, now()) / :totalTimeSeconds) desc,
                    p.createdOn desc
                  """,
              PostEntity.class
          )
          .setParameter("show", PostStateEnum.SHOW)
          .setParameter("hide", PostStateEnum.HIDE)
          .setParameter("lock", PostStateEnum.LOCK)
          .setParameter("accessKey", accessKey)
          .setParameter("block", PostStateEnum.BLOCK)
          .setParameter("user", user)
          .setParameter("totalTimeSeconds", totalTimeSeconds);

      totalSizeQuery = entityManager.createQuery(
              """
                  select count(p.id) from PostEntity p left join p.section ps
                  where p.reviewState = 0
                  and (:show member of p.states
                  or (:hide member of p.states and (:user member of ps.admins or :user member of p.allows))
                  or (:lock member of p.states and (p.accessKey = :accessKey or :user member of p.allows))
                  or (:block member of p.states and not (:user member of p.blocks))
                  or p.user = :user)
                  """,
              Long.class
          )
          .setParameter("show", PostStateEnum.SHOW)
          .setParameter("hide", PostStateEnum.HIDE)
          .setParameter("lock", PostStateEnum.LOCK)
          .setParameter("accessKey", accessKey)
          .setParameter("block", PostStateEnum.BLOCK)
          .setParameter("user", user);
    }

    return new TypedQueryPostPage(query, totalSizeQuery);
  }
}
