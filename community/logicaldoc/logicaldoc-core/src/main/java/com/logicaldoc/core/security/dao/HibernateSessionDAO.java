package com.logicaldoc.core.security.dao;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.SystemInfo;
import com.logicaldoc.core.security.Session;

public class HibernateSessionDAO extends HibernatePersistentObjectDAO<Session> implements SessionDAO {

	protected HibernateSessionDAO() {
		super(Session.class);
	}

	@Override
	public void deleteCurrentNodeSessions() {
		try {
			bulkUpdate(" set deleted=1 where node=?1 and deleted=0",
					new Object[] { SystemInfo.get().getInstallationId() });
		} catch (PersistenceException e) {
			log.warn(e.getMessage(), e);
		}

		try {
			bulkUpdate(" set status=" + Session.STATUS_EXPIRED + " where node=?1 and status=?2",
					new Object[] { SystemInfo.get().getInstallationId(), Session.STATUS_OPEN });
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
	}

	@Override
	public int countSessions(Long tenantId, Integer status) {
		StringBuffer query = new StringBuffer(" 1=1 ");
		if (tenantId != null)
			query.append(" and _entity.tenantId = " + tenantId);
		if (status != null)
			query.append(" and _entity.status = " + status);

		try {
			List<Session> sessions = findByWhere(query.toString(), null, null);
			return sessions.size();
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return 0;
		}
	}

	@Override
	public Session findBySid(String sid) {
		try {
			List<Session> sessions = findByWhere("_entity.sid = ?1", new Object[] { sid }, null, null);
			for (Session session : sessions) {
				if (session.getDeleted() == 0)
					return session;
			}
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
		return null;
	}

	@Override
	public void initialize(Session session) {
		refresh(session);
	}

	@Override
	public List<Session> findByNode(String node) {
		try {
			if (StringUtils.isEmpty(node))
				return findByWhere(" 1=1 ", null, "order by _entity.creation desc", null);
			else
				return findByWhere("_entity.node = ?1", new Object[] { node }, "order by _entity.creation desc", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Session>();
		}
	}

	@Override
	public void cleanOldSessions(int ttl) {
		if (ttl > 0) {
			Date today = new Date();
			GregorianCalendar cal = new GregorianCalendar();
			cal.add(Calendar.DAY_OF_MONTH, -ttl);
			Date ldDate = cal.getTime();

			try {
				int rowsUpdated = jdbcUpdate("UPDATE ld_session SET ld_deleted = 1, ld_lastmodified = ?"
						+ " WHERE ld_deleted = 0 AND ld_creation < ?", today, ldDate);

				log.info("cleanOldSessions rows updated: {}", rowsUpdated);
			} catch (Exception e) {
				if (log.isErrorEnabled())
					log.error(e.getMessage(), e);
			}

		}
	}
}