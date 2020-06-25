package com.logicaldoc.web.listener;

import javax.servlet.ServletContextEvent;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.context.ContextLoaderListener;

import com.logicaldoc.core.SystemInfo;
import com.logicaldoc.core.security.dao.SessionDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

/**
 * Listener that initializes and destroys the Spring Context
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7
 */
public class ContextListener extends ContextLoaderListener {

	private static Logger log = LoggerFactory.getLogger(ContextListener.class);

	@Override
	public void contextDestroyed(ServletContextEvent event) {
		cleanupSessions();

		super.contextDestroyed(event);
	}

	@Override
	public void contextInitialized(ServletContextEvent event) {
		super.contextInitialized(event);

		cleanupSessions();
	}

	private void cleanupSessions() {
		try {
			log.info("Cleanup past sessions of the current node {}", SystemInfo.get().getInstallationId());
			SessionDAO sessionDAO = (SessionDAO) Context.get().getBean(SessionDAO.class);
			sessionDAO.deleteCurrentNodeSessions();

			ContextProperties config = (ContextProperties) Context.get().getProperties();
			int sessionTtl = config.getInt("session.ttl", -1);
			sessionDAO.cleanOldSessions(sessionTtl);
		} catch (Throwable e) {
			log.warn(e.getMessage(), e);
		}
	}
}