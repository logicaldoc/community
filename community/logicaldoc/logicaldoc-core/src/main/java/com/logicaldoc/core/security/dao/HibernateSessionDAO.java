package com.logicaldoc.core.security.dao;

import java.util.List;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.SystemInfo;
import com.logicaldoc.core.security.Session;

@SuppressWarnings("unchecked")
public class HibernateSessionDAO extends HibernatePersistentObjectDAO<Session> implements SessionDAO {

	@SuppressWarnings("unused")
	private SessionDAO sessionDao;

	protected HibernateSessionDAO() {
		super(Session.class);
	}

	@Override
	public void deleteCurrentNodeSessions() {
		bulkUpdate(" set deleted=1 where node=?1 and deleted=0", new Object[] { SystemInfo.get().getInstallationId() });
		bulkUpdate(" set status="+Session.STATUS_EXPIRED+" where node=?1 and status=?2", new Object[] { SystemInfo.get().getInstallationId() , Session.STATUS_OPEN});
	}

	@Override
	public int countSessions(Long tenantId, Integer status) {
		StringBuffer query = new StringBuffer(" 1=1 ");
		if (tenantId != null)
			query.append(" and _entity.tenantId = " + tenantId);
		if (status != null)
			query.append(" and _entity.status = " + status);

		List<Session> sessions = findByWhere(query.toString(), null, null);
		return sessions.size();
	}

	@Override
	public Session findBySid(String sid) {
		List<Session> sessions = findByWhere("_entity.sid = ?1", new Object[] { sid }, null, null);
		for (Session session : sessions) {
			if (session.getDeleted() == 0)
				return session;
		}
		return null;
	}

	@Override
	public void initialize(Session session) {
		refresh(session);
	}

	@Override
	public List<Session> findByNode(String node) {
		if (StringUtils.isEmpty(node))
			return findByWhere(" 1=1 ", null, "order by _entity.creation desc", null);
		else
			return findByWhere("_entity.node = ?1", new Object[] { node }, "order by _entity.creation desc", null);
	}
}