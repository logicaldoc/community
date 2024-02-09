package com.logicaldoc.core.security;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.SystemInfo;

public class HibernateSessionDAO extends HibernatePersistentObjectDAO<Session> implements SessionDAO {

	protected HibernateSessionDAO() {
		super(Session.class);
	}

	@Override
	public void deleteCurrentNodeSessions() {
		try {
			Map<String, Object> params = new HashMap<>();
			params.put("node", SystemInfo.get().getInstallationId());

			bulkUpdate(" set deleted=1 where node = :node and deleted=0", params);
		} catch (PersistenceException e) {
			log.warn(e.getMessage(), e);
		}

		try {
			Map<String, Object> params = new HashMap<>();
			params.put("node", SystemInfo.get().getInstallationId());
			params.put("status", Session.STATUS_OPEN);

			bulkUpdate(" set status=" + Session.STATUS_EXPIRED + " where node = :node and status = :status", params);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
	}

	@Override
	public int countSessions(Long tenantId, Integer status) {
		StringBuilder query = new StringBuilder(" 1=1 ");
		if (tenantId != null)
			query.append(" and " + ENTITY + ".tenantId = " + tenantId);
		if (status != null)
			query.append(" and " + ENTITY + ".status = " + status);

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
			Map<String, Object> params = new HashMap<>();
			params.put("sid", sid);
			List<Session> sessions = findByWhere(ENTITY + ".sid = :sid", params, null, null);
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
				return findByWhere(" 1=1 ", (Map<String, Object>) null, "order by " + ENTITY + ".creation desc",
						null);
			else {
				Map<String, Object> params = new HashMap<>();
				params.put("node", node);
				return findByWhere(ENTITY + ".node = :node", params,
						"order by " + ENTITY + ".creation desc", null);
			}
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	@Override
	public void cleanOldSessions(int ttl) {
		try {
			log.info("cleanOldSessions rows updated: {}", cleanOldRecords(ttl, "ld_session", "ld_creation"));
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
	}
}