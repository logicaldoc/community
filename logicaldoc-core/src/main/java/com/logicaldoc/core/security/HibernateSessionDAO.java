package com.logicaldoc.core.security;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Repository;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.SystemInfo;

import jakarta.transaction.Transactional;

@Repository("sessionDAO")
@Transactional
public class HibernateSessionDAO extends HibernatePersistentObjectDAO<Session> implements SessionDAO {

	private static final String AND = " and ";

	protected HibernateSessionDAO() {
		super(Session.class);
	}

	@Override
	public void deleteCurrentNodeSessions() {
		try {
			jdbcUpdate("update ld_session set ld_deleted=1 where ld_node = :node and ld_deleted=0",
					Map.of("node", SystemInfo.get().getInstallationId()));
		} catch (PersistenceException e) {
			log.warn(e.getMessage(), e);
		}

		try {
			jdbcUpdate("update ld_session set ld_status=" + Session.STATUS_EXPIRED + " where ld_node = :node and ld_status = :status",
					Map.of("node", SystemInfo.get().getInstallationId(), "status", Session.STATUS_OPEN));
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
	}

	@Override
	public int countSessions(Long tenantId, Integer status) {
		StringBuilder query = new StringBuilder(" 1=1 ");
		if (tenantId != null)
			query.append(AND + ENTITY + ".tenantId = " + tenantId);
		if (status != null) {
			query.append(AND + ENTITY + ".status = " + status);
			if (status.intValue() == Session.STATUS_OPEN)
				query.append(AND + ENTITY + ".finished is null ");
		}

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
	public List<Session> findByNode(String node) {
		try {
			if (StringUtils.isEmpty(node))
				return findByWhere(" 1=1 ", (Map<String, Object>) null, ENTITY + ".creation desc", null);
			else {
				Map<String, Object> params = new HashMap<>();
				params.put("node", node);
				return findByWhere(ENTITY + ".node = :node", params, ENTITY + ".creation desc", null);
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