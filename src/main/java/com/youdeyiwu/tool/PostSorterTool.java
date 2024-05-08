package com.youdeyiwu.tool;

import com.youdeyiwu.model.entity.forum.PostEntity;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.Comparator;
import java.util.List;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

/**
 * post sorter.
 *
 * @author dafengzhen
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class PostSorterTool {

  public static final int RANGE_DAYS = 31;

  public static final int TOTAL_TIME_SECONDS = RANGE_DAYS * 24 * 60 * 60;

  /**
   * sort posts.
   *
   * @param posts posts
   */
  public static void sortPosts(List<PostEntity> posts) {
    sortPosts(posts, OffsetDateTime.now(), RANGE_DAYS);
  }

  /**
   * sort posts.
   *
   * @param posts     posts
   * @param rangeDays rangeDays
   */
  public static void sortPosts(List<PostEntity> posts, int rangeDays) {
    sortPosts(posts, OffsetDateTime.now(), rangeDays);
  }

  /**
   * sort posts.
   *
   * @param posts     posts
   * @param startTime startTime
   * @param rangeDays rangeDays
   */
  public static void sortPosts(List<PostEntity> posts, OffsetDateTime startTime, int rangeDays) {
    // Sort by initialScore in descending order
    posts.sort(Comparator.comparing(PostEntity::getInitialScore).reversed());

    // Sort by (pageViews + commentsCount + repliesCount) * timeWeight in descending order
    final OffsetDateTime endTime = startTime.plusDays(rangeDays);
    final int totalTimeSeconds = rangeDays * 24 * 60 * 60;

    posts.sort((post1, post2) -> {
      long post1Activity = post1.getPageViews() + post1.getCommentsCount() + post1.getRepliesCount();
      long post2Activity = post2.getPageViews() + post2.getCommentsCount() + post2.getRepliesCount();
      long post1TimeWeight = 1 - calculateTimeDifferenceWeight(post1.getCreatedOn(), endTime, totalTimeSeconds);
      long post2TimeWeight = 1 - calculateTimeDifferenceWeight(post2.getCreatedOn(), endTime, totalTimeSeconds);
      long post1Score = post1Activity * post1TimeWeight;
      long post2Score = post2Activity * post2TimeWeight;
      return Long.compare(post2Score, post1Score);
    });
  }

  /**
   * calculate time difference weight.
   *
   * @param startTime        startTime
   * @param endTime          endTime
   * @param totalTimeSeconds totalTimeSeconds
   * @return long
   */
  private static long calculateTimeDifferenceWeight(OffsetDateTime startTime, OffsetDateTime endTime, long totalTimeSeconds) {
    long timeDifferenceSeconds = endTime.toEpochSecond() - startTime.toEpochSecond();
    return timeDifferenceSeconds / totalTimeSeconds;
  }
}
