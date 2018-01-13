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
import com.logicaldoc.util.sql.SqlUtil;

/**
 * Hibernate implementation of <code>SystemMessageDAO</code>
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 3.0
 */
@SuppressWarnings("unchecked")
public class HibernateSystemMessageDAO extends HibernatePersistentObjectDAO<SystemMessage> implements SystemMessageDAO {
	public class SystemMessageMapper implements RowMapper<SystemMessage> {

		public SystemMessage mapRow(ResultSet rs, int rowNum) throws SQLException {

			SystemMessage message = new SystemMessage();
			message.setLastModified(rs.getTimestamp(1));
			message.setDeleted(rs.getInt(2));
			message.setAuthor(rs.getString(3));
			message.setMessageText(rs.getString(4));
			message.setSubject(rs.getString(5));
			message.setSentDate(rs.getTimestamp(6));
			message.setDateScope(rs.getInt(7));
			message.setPrio(rs.getInt(8));
			message.setConfirmation(rs.getInt(9));
			message.setLastNotified(rs.getTimestamp(10));
			message.setStatus(rs.getInt(11));
			message.setTrials(rs.getInt(12));
			message.setType(rs.getInt(13));
			message.setId(rs.getLong(14));
			message.setHtml(rs.getInt(15));
			message.setTenantId(rs.getLong(16));

			return message;
		}
	};

	public HibernateSystemMessageDAO() {
		super(SystemMessage.class);
		super.log = LoggerFactory.getLogger(HibernateSystemMessageDAO.class);
	}

	/**
	 * @see com.logicaldoc.core.communication.SystemMessageDAO#findByRecipient(java.lang.String,
	 *      int)
	 */
	public List<SystemMessage> findByRecipient(String recipient, int type, Integer read) {
		String sql = "select ld_lastmodified, ld_deleted, ld_author, ld_messagetext, ld_subject, ld_sentdate, ld_datescope, ld_prio, ld_confirmation, ld_lastnotified, ld_status, ld_trials, ld_type, ld_id, ld_html, ld_tenantid "
				+ " from ld_systemmessage where ld_deleted = 0 and ld_type = "
				+ type
				+ " and ld_id IN (select Q.ld_messageid from ld_recipient Q where Q.ld_name = '"
				+ SqlUtil.doubleQuotes(recipient) + "' )";
		if (read != null)
			sql = sql + " and " + (read == 1 ? "1 <=" : "0 =")
					+ "  (select count(*) from ld_recipient R where R.ld_name = '" + SqlUtil.doubleQuotes(recipient)
					+ "' and R.ld_read=1 and R.ld_messageid=ld_id)";
		sql = sql + " order by ld_sentdate desc";
		
		List<SystemMessage> messages = (List<SystemMessage>) query(sql, null, new SystemMessageMapper(), null);
		return messages;
	}

	/**
	 * @see com.logicaldoc.core.communication.SystemMessageDAO#getCount(java.lang.String,
	 *      int)
	 */
	public int getCount(String recipient, int type, Integer read) {
		return findByRecipient(recipient, type, read).size();
	}

	/**
	 * @see com.logicaldoc.core.communication.SystemMessageDAO#deleteExpiredMessages(java.lang.String)
	 */
	public void deleteExpiredMessages(String recipient) {
		collectGarbage(findByRecipient(recipient, SystemMessage.TYPE_SYSTEM, null), true);
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
		String sql = "select ld_lastmodified, ld_deleted, ld_author, ld_messagetext, ld_subject, ld_sentdate, ld_datescope, ld_prio, ld_confirmation, ld_lastnotified, ld_status, ld_trials, ld_type, ld_id, ld_html, ld_tenantid "
				+ " from ld_systemmessage where ld_deleted = 0 and ld_id IN (select ld_messageid from ld_recipient where ld_mode = '"
				+ SqlUtil.doubleQuotes(mode) + "') order by ld_sentdate desc";

		List<SystemMessage> messages = (List<SystemMessage>) query(sql, null, new SystemMessageMapper(), null);
		return messages;
	}

	@Override
	public List<SystemMessage> findByType(int type) {
		String sql = "select ld_lastmodified, ld_deleted, ld_author, ld_messagetext, ld_subject, ld_sentdate, ld_datescope, ld_prio, ld_confirmation, ld_lastnotified, ld_status, ld_trials, ld_type, ld_id, ld_html, ld_tenantid "
				+ " from ld_systemmessage where ld_deleted = 0 and ld_id IN (select ld_messageid from ld_recipient where ld_type = "
				+ type + ") order by ld_sentdate desc";

		List<SystemMessage> messages = (List<SystemMessage>) query(sql, null, new SystemMessageMapper(), null);
		return messages;
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
		String sql = "select ld_lastmodified, ld_deleted, ld_author, ld_messagetext, ld_subject, ld_sentdate, ld_datescope, ld_prio, ld_confirmation, ld_lastnotified, ld_status, ld_trials, ld_type, ld_id, ld_html, ld_tenantid "
				+ " from ld_systemmessage where ld_deleted = 0 and not ld_status = "
				+ SystemMessage.STATUS_DELIVERED
				+ " and ld_type = " + type;
		if (maxTrial > 0)
			sql = sql + " and ld_trials < " + maxTrial;
		sql = sql + " order by ld_sentdate desc";

		List<SystemMessage> messages = (List<SystemMessage>) query(sql, null, new SystemMessageMapper(), null);
		return messages;
	}
}