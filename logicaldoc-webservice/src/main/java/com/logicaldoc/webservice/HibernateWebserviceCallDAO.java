package com.logicaldoc.webservice;

import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;

/**
 * Hibernate implementation of <code>ChatMessageyDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class HibernateWebserviceCallDAO extends HibernatePersistentObjectDAO<WebserviceCall>
		implements WebserviceCallDAO {

	private HibernateWebserviceCallDAO() {
		super(WebserviceCall.class);
		super.log = LoggerFactory.getLogger(HibernateWebserviceCallDAO.class);
	}

	@Override
	public void cleanOldCalls(int ttl) throws PersistenceException {
		log.info("cleanOldCalls rows updated: {}", cleanOldRecords(ttl, "ld_webservicecall"));
	}
}