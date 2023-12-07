package com.youdeyiwu.runner;

import com.youdeyiwu.security.SecurityMetadataSource;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.actuate.endpoint.web.PathMappedEndpoints;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/**
 * metadata.
 *
 * @author dafengzhen
 */
@Order(2)
@Log4j2
@RequiredArgsConstructor
@Component
public class MetaDataApplicationRunner implements ApplicationRunner {

  private final SecurityMetadataSource securityMetadataSource;

  private final PathMappedEndpoints pathMappedEndpoints;

  @Transactional(readOnly = true)
  @Override
  public void run(ApplicationArguments args) throws Exception {
    pathMappedEndpoints.getAllRootPaths()
        .forEach(s -> log.info("=== MetaData === RootPaths === {}", s));
    pathMappedEndpoints.getAllPaths().forEach(s -> log.info("=== MetaData === Paths === {}", s));
    securityMetadataSource.initMetadata();
    log.info("=== MetaData === Initialization metadata completed");
  }
}
