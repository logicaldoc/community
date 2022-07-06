package com.logicaldoc.webservice;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;

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
	public void cleanOldCalls(int ttl) {
		if (ttl > 0) {
			Date today = new Date();
			GregorianCalendar cal = new GregorianCalendar();
			cal.add(Calendar.DAY_OF_MONTH, -ttl);
			Date ldDate = cal.getTime();

			try {
				int rowsUpdated = jdbcUpdate("UPDATE ld_webservicecall SET ld_deleted = 1, ld_lastmodified = ?"
						+ " WHERE ld_deleted = 0 AND ld_date < ?", today, ldDate);

				log.info("cleanOldCalls rows updated: " + rowsUpdated);
			} catch (Exception e) {
				if (log.isErrorEnabled())
					log.error(e.getMessage(), e);
			}

		}
	}
}