package com.logicaldoc.web.listener;

import jakarta.servlet.ServletContextEvent;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.context.ContextLoaderListener;

import com.logicaldoc.core.SystemInfo;
import com.logicaldoc.core.security.SessionDAO;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.spring.Context;

/**
 * Listener that initializes and destroys the Spring Context
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7
 */
public class ContextListener extends ContextLoaderListener {

	private static final Logger log = LoggerFactory.getLogger(ContextListener.class);

	@Override
	public void contextDestroyed(ServletContextEvent event) {
		cleanupSessions();
		log.info("Application shut down");
		super.contextDestroyed(event);
	}

	@Override
	public void contextInitialized(ServletContextEvent event) {
		super.contextInitialized(event);
		cleanupSessions();
		log.info("Application started and ready");
	}

	private void cleanupSessions() {
		try {
			log.info("Cleanup past sessions of the current node {}", SystemInfo.get().getInstallationId());
			if (Context.get() != null) {
				SessionDAO sessionDAO = Context.get(SessionDAO.class);
				sessionDAO.deleteCurrentNodeSessions();

				ContextProperties config = Context.get().getProperties();
				int sessionTtl = config.getInt("session.ttl", -1);
				sessionDAO.cleanOldSessions(sessionTtl);
			}
		} catch (Exception e) {
			log.warn(e.getMessage(), e);
		}
	}
}