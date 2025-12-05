package com.logicaldoc.webservicedoc;
import jakarta.annotation.PostConstruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class SentinelBean {

    private static final Logger log = LoggerFactory.getLogger(SentinelBean.class);

    @PostConstruct
    public void init() {
        log.error(">>> SentinelBean inizializzato: il context è vivo");
    }
}
