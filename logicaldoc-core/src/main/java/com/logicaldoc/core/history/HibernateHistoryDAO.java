package com.logicaldoc.core.history;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.annotation.Resource;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.RunLevel;
import com.logicaldoc.core.communication.EventCollector;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

/**
 * Parent of all DAOs that handle histories
 * 
 * @param <T> Class of the implementation of a {@link History} this DAO
 *        handles
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 9.0.1
 */
public abstract class HibernateHistoryDAO<T extends History> extends HibernatePersistentObjectDAO<T>
		implements PersistentObjectDAO<T> {

	@Resource(name = "ContextProperties")
	private ContextProperties config;

	// A cache of tenant names to minimize the DB accesses
	private static final Map<Long, String> tenantNames = new HashMap<>();

	protected HibernateHistoryDAO(Class<T> historyClass) {
		super(historyClass);
		super.log = LoggerFactory.getLogger(HibernateHistoryDAO.class);
	}

	@Override
	public void store(T history) throws PersistenceException {
		// Write only if the history is enabled
		if (!RunLevel.current().aspectEnabled(History.ASPECT))
			return;

		if (history.getDate() == null)
			history.setDate(new Date());
		if (history.getComment() != null) {
			// trim to 4000 chars
			history.setComment(StringUtils.abbreviate(history.getComment(), 4000));

			// remove non printable chars, but maintain the carriage
			// returns and the tabs
			history.setComment(history.getComment().trim().replaceAll("[\\p{Cntrl}&&[^\\n]&&[^\\t]&&[^\\r]]", ""));
		}

		String allowedEvents = config.getString(getTenantName(history) + ".history.events", "all");
		if ("all".equals(allowedEvents) || allowedEvents.contains(history.getEvent()))
			super.store(history);

		EventCollector.get().newEvent(history);
	}

	protected String getTenantName(T history) throws PersistenceException {
		if (HibernateHistoryDAO.tenantNames.containsKey(history.getTenantId()))
			return HibernateHistoryDAO.tenantNames.get(history.getTenantId());

		TenantDAO tenantDao = Context.get().getBean(TenantDAO.class);
		String name = tenantDao.getTenantName(history.getTenantId());
		HibernateHistoryDAO.tenantNames.put(history.getTenantId(), name);
		return name;
	}
}