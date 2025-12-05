package com.logicaldoc.webservicedoc;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;

public class ContextStartupListener implements ApplicationListener<ContextRefreshedEvent> {

    private static final Logger log = LoggerFactory.getLogger(ContextStartupListener.class);

    @Override
    public void onApplicationEvent(ContextRefreshedEvent event) {
        log.error(">>> Spring context '{}' caricato (XML config)",
                 event.getApplicationContext().getId());
    }
}
