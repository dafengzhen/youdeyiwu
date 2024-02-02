package com.youdeyiwu.event;

import java.util.Objects;
import org.checkerframework.checker.initialization.qual.Initialized;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.UnknownKeyFor;
import org.hibernate.boot.Metadata;
import org.hibernate.boot.spi.BootstrapContext;
import org.hibernate.engine.spi.SessionFactoryImplementor;
import org.hibernate.event.service.spi.EventListenerRegistry;
import org.hibernate.event.spi.EventType;
import org.hibernate.event.spi.PostInsertEvent;
import org.hibernate.event.spi.PostInsertEventListener;
import org.hibernate.integrator.spi.Integrator;
import org.hibernate.persister.entity.EntityPersister;
import org.hibernate.service.spi.SessionFactoryServiceRegistry;

/**
 * notification.
 *
 * @author dafengzhen
 */
public class NotificationEventIntegrator implements Integrator, PostInsertEventListener {

  @Override
  public void integrate(
      @UnknownKeyFor @NonNull @Initialized Metadata metadata,
      @UnknownKeyFor @NonNull @Initialized BootstrapContext bootstrapContext,
      @UnknownKeyFor @NonNull @Initialized SessionFactoryImplementor sessionFactory
  ) {
    SessionFactoryServiceRegistry serviceRegistry =
        (SessionFactoryServiceRegistry) sessionFactory.getServiceRegistry();

    EventListenerRegistry service = Objects.requireNonNull(
        serviceRegistry.getService(EventListenerRegistry.class)
    );

    service.appendListeners(EventType.POST_INSERT, this);
  }

  @Override
  public void disintegrate(
      @UnknownKeyFor @NonNull @Initialized SessionFactoryImplementor sessionFactory,
      @UnknownKeyFor @NonNull @Initialized SessionFactoryServiceRegistry serviceRegistry
  ) {
    // disintegrate
  }

  @Override
  public void onPostInsert(PostInsertEvent event) {
    // onPostInsert
  }

  @Override
  public boolean requiresPostCommitHandling(EntityPersister persister) {
    return false;
  }
}
