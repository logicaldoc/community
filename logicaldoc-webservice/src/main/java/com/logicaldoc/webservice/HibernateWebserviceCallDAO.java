package com.logicaldoc.webservice;

import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernateHistoryDAO;
import com.logicaldoc.core.PersistenceException;

/**
 * Hibernate implementation of {@link WebserviceCallDAO}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class HibernateWebserviceCallDAO extends HibernateHistoryDAO<WebserviceCall> implements WebserviceCallDAO {

	private HibernateWebserviceCallDAO() {
		super(WebserviceCall.class);
		super.log = LoggerFactory.getLogger(HibernateWebserviceCallDAO.class);
	}

	@Override
	public void cleanOldCalls(int ttl) throws PersistenceException {
		log.info("cleanOldCalls rows updated: {}", cleanOldRecords(ttl, "ld_webservicecall"));
	}
}