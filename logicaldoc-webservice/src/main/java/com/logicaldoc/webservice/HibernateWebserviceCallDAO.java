package com.logicaldoc.webservice;

import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.history.HibernateHistoryDAO;
import com.logicaldoc.util.config.ContextProperties;

/**
 * Hibernate implementation of {@link WebserviceCallDAO}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class HibernateWebserviceCallDAO extends HibernateHistoryDAO<WebserviceCall> implements WebserviceCallDAO {

	@Autowired
	private HibernateWebserviceCallDAO(ContextProperties config) {
		super(WebserviceCall.class, config);
		super.log = LoggerFactory.getLogger(HibernateWebserviceCallDAO.class);
	}

	@Override
	public void cleanOldCalls(int ttl) throws PersistenceException {
		log.info("cleanOldCalls rows updated: {}", cleanOldRecords(ttl, "ld_webservicecall"));
	}
}