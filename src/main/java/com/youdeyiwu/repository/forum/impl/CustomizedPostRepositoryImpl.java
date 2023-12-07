package com.youdeyiwu.repository.forum.impl;

import com.youdeyiwu.model.dto.PaginationPositionDto;
import com.youdeyiwu.model.dto.forum.QueryParamsPostDto;
import com.youdeyiwu.model.entity.forum.CommentEntity;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.forum.QuoteReplyEntity;
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
      QueryParamsPostDto dto,
      Boolean isAnonymous,
      Long userId
  ) {
    Long sectionGroupId = dto.sectionGroupId();
    Long sectionId = dto.sectionId();
    Long tagGroupId = dto.tagGroupId();
    Long tagId = dto.tagId();

    TypedQuery<Long> query = entityManager.createQuery(
            getPostIdsQlStringByRelatedId(
                sectionGroupId,
                sectionId,
                tagGroupId,
                tagId
            ),
            Long.class
        )
        .setFirstResult(position.firstResult())
        .setMaxResults(position.maxResults());

    if (Objects.nonNull(sectionGroupId)) {
      query.setParameter("sectionGroupId", sectionGroupId);
    } else if (Objects.nonNull(sectionId)) {
      query.setParameter("sectionId", sectionId);
    } else if (Objects.nonNull(tagGroupId)) {
      query.setParameter("tagGroupId", tagGroupId);
    } else if (Objects.nonNull(tagId)) {
      query.setParameter("tagId", tagId);
    }

    TypedQuery<PostEntity> postQuery = entityManager.createQuery(
        getPostsQlStringByRelatedId(
            sectionGroupId,
            sectionId,
            tagGroupId,
            tagId
        ),
        PostEntity.class
    );

    TypedQuery<Long> totalSizeQuery = entityManager.createQuery(
        getPostsSizeQlStringByRelatedId(
            sectionGroupId,
            sectionId,
            tagGroupId,
            tagId
        ),
        Long.class
    );

    List<Long> ids = query.getResultList();
    if (Objects.nonNull(sectionGroupId)) {
      postQuery.setParameter("ids", ids);
      totalSizeQuery.setParameter("sectionGroupId", sectionGroupId);
    } else if (Objects.nonNull(sectionId)) {
      postQuery.setParameter("ids", ids);
      totalSizeQuery.setParameter("sectionId", sectionId);
    } else if (Objects.nonNull(tagId)) {
      postQuery.setParameter("ids", ids);
      totalSizeQuery.setParameter("tagId", tagId);
    }

    List<PostEntity> entities = postQuery.getResultList();
    Long totalSize = totalSizeQuery.getSingleResult();
    return new PageImpl<>(entities, position.pageable(), totalSize);
  }

  @Override
  public Page<CommentReplyEntityVo> findAllCommentReply(
      PaginationPositionDto position,
      Long postId
  ) {
    List<Tuple> tuples = entityManager.createQuery(
            """
                    select c, r from CommentEntity c left join fetch QuoteReplyEntity r
                    on c.id = r.comment.id
                    where c.post.id = :postId
                    order by c.id, r.id
                """,
            Tuple.class
        )
        .setParameter("postId", postId)
        .setFirstResult(position.firstResult())
        .setMaxResults(position.maxResults())
        .getResultList();

    Tuple result = entityManager.createQuery(
            """
                    select count(c), count(r) from CommentEntity c left join QuoteReplyEntity r
                    on c.id = r.comment.id
                    where c.post.id = :postId
                """,
            Tuple.class
        )
        .setParameter("postId", postId)
        .getSingleResult();
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

  /**
   * getPostIdsQlStringByRelatedId.
   *
   * @param sectionGroupId sectionGroupId
   * @param sectionId      sectionId
   * @param tagGroupId     tagGroupId
   * @param tagId          tagId
   * @return String
   */
  private String getPostIdsQlStringByRelatedId(
      Long sectionGroupId,
      Long sectionId,
      Long tagGroupId,
      Long tagId
  ) {
    String qlString;
    if (Objects.nonNull(sectionGroupId)) {
      qlString = """
          select p.id from PostEntity p
          left join p.section ps
          left join ps.sectionGroups psg
          where psg.id = :sectionGroupId
          order by p.initialScore desc, p.sortState desc, p.id desc
          """;
    } else if (Objects.nonNull(sectionId)) {
      qlString = """
          select p.id from PostEntity p
          left join p.section ps
          where ps.id = :sectionId
          order by p.initialScore desc, p.sortState desc, p.id desc
          """;
    } else if (Objects.nonNull(tagGroupId)) {
      qlString = """
          select p.id from PostEntity p
          left join p.tags pt
          left join pt.tagGroups ptg
          where ptg.id = :tagGroupId
          order by p.initialScore desc, p.sortState desc, p.id desc
          """;
    } else if (Objects.nonNull(tagId)) {
      qlString = """
          select p.id from PostEntity p
          left join p.tags pt
          where pt.id = :tagId
          order by p.initialScore desc, p.sortState desc, p.id desc
          """;
    } else {
      qlString = """
          select p.id from PostEntity p
          order by p.initialScore desc, p.sortState desc, p.id desc
          """;
    }
    return qlString;
  }

  /**
   * getPostsQlStringByRelatedId.
   *
   * @param sectionGroupId sectionGroupId
   * @param sectionId      sectionId
   * @param tagGroupId     tagGroupId
   * @param tagId          tagId
   * @return String
   */
  private String getPostsQlStringByRelatedId(
      Long sectionGroupId,
      Long sectionId,
      Long tagGroupId,
      Long tagId
  ) {
    String qlString;
    if (Objects.nonNull(sectionGroupId)) {
      qlString = """
          select p from PostEntity p
          left join fetch p.section ps
          left join fetch ps.sectionGroups psg
          where p.id in (:ids)
          order by p.initialScore desc, p.sortState desc, p.id desc
          """;
    } else if (Objects.nonNull(sectionId)) {
      qlString = """
          select p from PostEntity p
          left join fetch p.section ps
          where p.id in (:ids)
          order by p.initialScore desc, p.sortState desc, p.id desc
          """;
    } else if (Objects.nonNull(tagGroupId)) {
      qlString = """
          select p from PostEntity p
          left join fetch p.tags pt
          left join fetch pt.tagGroups ptg
          where p.id in (:ids)
          order by p.initialScore desc, p.sortState desc, p.id desc
          """;
    } else if (Objects.nonNull(tagId)) {
      qlString = """
          select p from PostEntity p
          left join fetch p.tags pt
          where p.id in (:ids)
          order by p.initialScore desc, p.sortState desc, p.id desc
          """;
    } else {
      qlString = """
          select p from PostEntity p
          order by p.initialScore desc, p.sortState desc, p.id desc
          """;
    }
    return qlString;
  }

  /**
   * getPostsSizeQlStringByRelatedId.
   *
   * @param sectionGroupId sectionGroupId
   * @param sectionId      sectionId
   * @param tagGroupId     tagGroupId
   * @param tagId          tagId
   * @return String
   */
  private String getPostsSizeQlStringByRelatedId(
      Long sectionGroupId,
      Long sectionId,
      Long tagGroupId,
      Long tagId
  ) {
    String qlString;
    if (Objects.nonNull(sectionGroupId)) {
      qlString = """
          select count(p.id) from PostEntity p
          left join p.section ps
          left join ps.sectionGroups psg
          where psg.id = :sectionGroupId
          """;
    } else if (Objects.nonNull(sectionId)) {
      qlString = """
          select count(p.id) from PostEntity p
          left join p.section ps
          on ps.id = :sectionId
          """;
    } else if (Objects.nonNull(tagGroupId)) {
      qlString = """
          select count(p.id) from PostEntity p
          left join p.tags pt
          left join pt.tagGroups ptg
          on ptg.id = :tagGroupId
          """;
    } else if (Objects.nonNull(tagId)) {
      qlString = """
          select count(p.id) from PostEntity p
          left join p.tags pt
          on pt.id = :tagId
          """;
    } else {
      qlString = """
          select count(p.id) from PostEntity p
          """;
    }
    return qlString;
  }
}
