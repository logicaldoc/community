package com.logicaldoc.web.listener;

import javax.servlet.ServletContextEvent;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.context.ContextLoaderListener;

import com.logicaldoc.core.SystemInfo;
import com.logicaldoc.core.security.dao.SessionDAO;
import com.logicaldoc.util.Context;

/**
 * Listener that initializes and destroys the Spring Context
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7
 *
 */
public class ContextListener extends ContextLoaderListener {

	private static Logger log = LoggerFactory.getLogger(ContextListener.class);

	@Override
	public void contextDestroyed(ServletContextEvent event) {
		try {
			log.info("Cleanup all the sessions of the current node ({})", SystemInfo.get().getInstallationId());
			SessionDAO sessionDAO = (SessionDAO) Context.get().getBean(SessionDAO.class);
			sessionDAO.deleteCurrentNodeSessions();
		} catch (Throwable e) {
			log.warn(e.getMessage(), e);
		}
		
		super.contextDestroyed(event);
	}

	@Override
	public void contextInitialized(ServletContextEvent event) {
		super.contextInitialized(event);
		
		try {
			log.info("Cleanup past sessions of the current node ({})", SystemInfo.get().getInstallationId());
			SessionDAO sessionDAO = (SessionDAO) Context.get().getBean(SessionDAO.class);
			sessionDAO.deleteCurrentNodeSessions();
		} catch (Throwable e) {
			log.warn(e.getMessage(), e);
		}
	}
}