package com.logicaldoc.core.communication;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.UserEvent;
import com.logicaldoc.core.security.UserHistory;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.security.dao.UserHistoryDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.sql.SqlUtil;

/**
 * Hibernate implementation of <code>SystemMessageDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
@SuppressWarnings("unchecked")
public class HibernateSystemMessageDAO extends HibernatePersistentObjectDAO<SystemMessage> implements SystemMessageDAO {
	public class SystemMessageMapper implements RowMapper<SystemMessage> {

		public SystemMessage mapRow(ResultSet rs, int rowNum) throws SQLException {

			SystemMessage message = new SystemMessage();
			message.setLastModified(rs.getTimestamp(1));
			message.setAuthor(rs.getString(2));
			message.setMessageText(rs.getString(3));
			message.setSubject(rs.getString(4));
			message.setSentDate(rs.getTimestamp(5));
			message.setDateScope(rs.getInt(6));
			message.setPrio(rs.getInt(7));
			message.setConfirmation(rs.getInt(8));
			message.setStatus(rs.getInt(10));
			message.setTrials(rs.getInt(11));
			message.setType(rs.getInt(12));
			message.setId(rs.getLong(13));
			message.setHtml(rs.getInt(14));
			message.setAuthor(rs.getString(15));
			message.setTenantId(rs.getLong(16));

			return message;
		}
	};

	public HibernateSystemMessageDAO() {
		super(SystemMessage.class);
		super.log = LoggerFactory.getLogger(HibernateSystemMessageDAO.class);
	}

	@Override
	public List<SystemMessage> findByRecipient(String recipient, int type, Integer read) {
		String sql = "select ld_lastmodified, ld_author, ld_messagetext, ld_subject, ld_sentdate, ld_datescope, ld_prio, ld_confirmation, ld_lastnotified, ld_status, ld_trials, ld_type, ld_id, ld_html, ld_author, ld_tenantid "
				+ " from ld_systemmessage where ld_deleted = 0 and ld_type = " + type
				+ " and exists (select Q.ld_messageid from ld_recipient Q where Q.ld_name = '"
				+ SqlUtil.doubleQuotes(recipient) + "' and Q.ld_messageid=ld_id)";
		if (read != null)
			sql = sql + " and " + (read == 1 ? "exists" : "not exists")
					+ "  (select R.ld_messageid from ld_recipient R where R.ld_name = '"
					+ SqlUtil.doubleQuotes(recipient) + "' and R.ld_read=1 and R.ld_messageid=ld_id)";
		sql = sql + " order by ld_sentdate desc";

		List<SystemMessage> messages;
		try {
			messages = (List<SystemMessage>) query(sql, null, new SystemMessageMapper(), null);
			return messages;
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<SystemMessage>();
		}
	}

	@Override
	public int getUnreadCount(String recipient, int type) {
		String sql = "select count(distinct(R.ld_messageid)) from ld_recipient R, ld_systemmessage M "
				+ " where R.ld_name = '" + SqlUtil.doubleQuotes(recipient)
				+ "' and R.ld_messageid=M.ld_id and M.ld_deleted=0 and M.ld_type=" + type + " and R.ld_read=0";
		try {
			return queryForInt(sql);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return 0;
		}
	}

	/**
	 * @see com.logicaldoc.core.communication.SystemMessageDAO#deleteExpiredMessages(java.lang.String)
	 */
	public void deleteExpiredMessages(String recipient) {
		collectGarbage(findByRecipient(recipient, Message.TYPE_SYSTEM, null), true);
	}

	/**
	 * Cleans from the passed collection all expired messages
	 * 
	 * @param coll The input messages
	 * @param removeExpired True if expired messages must be deleted
	 * @return The cleaned messages collection
	 */
	protected List<SystemMessage> collectGarbage(Collection<SystemMessage> coll, boolean removeExpired) {
		List<SystemMessage> out = new ArrayList<SystemMessage>();
		try {
			Iterator<SystemMessage> iter = coll.iterator();
			Date date = new Date();
			long time = date.getTime();

			while (iter.hasNext()) {
				SystemMessage sm = (SystemMessage) iter.next();
				long sentdate = new Date().getTime();
				long timespan = sm.getDateScope();
				timespan = timespan * 86400000;
				sentdate += timespan;

				if (time >= sentdate) {
					if (removeExpired)
						delete(sm.getId());
				} else {
					out.add(sm);
				}
			}
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
		}

		return out;
	}

	/**
	 * @see com.logicaldoc.core.communication.SystemMessageDAO#deleteExpiredMessages(int)
	 */
	public void deleteExpiredMessages(int type) {
		collectGarbage(findByType(type), true);
	}

	@Override
	public List<SystemMessage> findByMode(String mode) {
		String sql = "select ld_lastmodified, ld_author, ld_messagetext, ld_subject, ld_sentdate, ld_datescope, ld_prio, ld_confirmation, ld_lastnotified, ld_status, ld_trials, ld_type, ld_id, ld_html, ld_author, ld_tenantid "
				+ " from ld_systemmessage where ld_deleted = 0 and ld_id IN (select ld_messageid from ld_recipient where ld_mode = '"
				+ SqlUtil.doubleQuotes(mode) + "') order by ld_sentdate desc";

		try {
			List<SystemMessage> messages = (List<SystemMessage>) query(sql, null, new SystemMessageMapper(), null);
			return messages;
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<SystemMessage>();
		}
	}

	@Override
	public List<SystemMessage> findByType(int type) {
		String sql = "select ld_lastmodified, ld_author, ld_messagetext, ld_subject, ld_sentdate, ld_datescope, ld_prio, ld_confirmation, ld_lastnotified, ld_status, ld_trials, ld_type, ld_id, ld_html, ld_author, ld_tenantid "
				+ " from ld_systemmessage where ld_deleted = 0 and ld_id IN (select ld_messageid from ld_recipient where ld_type = "
				+ type + ") order by ld_sentdate desc";

		try {
			List<SystemMessage> messages = (List<SystemMessage>) query(sql, null, new SystemMessageMapper(), null);
			return messages;
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<SystemMessage>();
		}
	}

	@Override
	public void initialize(SystemMessage message) {
		refresh(message);

		for (Recipient recipient : message.getRecipients()) {
			recipient.getName();
		}
	}

	@Override
	public List<SystemMessage> findMessagesToBeSent(int type, int maxTrial) {
		String sql = "select ld_lastmodified, ld_author, ld_messagetext, ld_subject, ld_sentdate, ld_datescope, ld_prio, ld_confirmation, ld_lastnotified, ld_status, ld_trials, ld_type, ld_id, ld_html, ld_author, ld_tenantid "
				+ " from ld_systemmessage where ld_deleted = 0 and not ld_status = " + SystemMessage.STATUS_DELIVERED
				+ " and ld_type = " + type;
		if (maxTrial > 0)
			sql = sql + " and ld_trials < " + maxTrial;
		sql = sql + " order by ld_sentdate desc";

		try {
			List<SystemMessage> messages = (List<SystemMessage>) query(sql, null, new SystemMessageMapper(), null);
			return messages;
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<SystemMessage>();
		}
	}

	@Override
	public void store(SystemMessage message) throws PersistenceException {
		long originalId = message.getId();

		// Do standard processing
		super.store(message);

		/**
		 * Store a new event for the recipient, in case this is a new system
		 * message
		 */
		if (originalId == 0L && message.getType() == Message.TYPE_SYSTEM) {
			UserHistoryDAO hDao = (UserHistoryDAO) Context.get().getBean(UserHistoryDAO.class);
			UserDAO uDao = (UserDAO) Context.get().getBean(UserDAO.class);
			for (Recipient rec : message.getRecipients()) {
				if (rec.getType() == Recipient.TYPE_EMAIL)
					continue;

				UserHistory history = new UserHistory();
				history.setTenantId(message.getTenantId());
				history.setComment(message.getMessageText());
				history.setEvent(UserEvent.MESSAGE_RECEIVED.toString());
				history.setAuthor(message.getAuthor());
				history.setNotifyEvent(message.isNotify());

				User recipient = uDao.findByUsername(rec.getName());
				if (recipient != null)
					history.setUser(recipient);

				User author = uDao.findByUsername(message.getAuthor());
				if (author != null)
					history.setAuthor(author.getFullName());

				hDao.store(history);
			}
		}
	}
}