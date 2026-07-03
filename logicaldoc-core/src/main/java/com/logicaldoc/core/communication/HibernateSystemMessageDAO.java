package com.logicaldoc.core.communication;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.stereotype.Repository;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.runtime.Aspect;
import com.logicaldoc.core.runtime.RunLevel;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.security.user.UserEvent;
import com.logicaldoc.core.security.user.UserHistory;
import com.logicaldoc.core.security.user.UserHistoryDAO;

import jakarta.transaction.Transactional;

/**
 * Hibernate implementation of <code>SystemMessageDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
@Repository("systemMessageDAO")
@Transactional
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
            message.setType(Message.Type.values()[rs.getInt(12)]);
            message.setId(rs.getLong(13));
            message.setHtml(rs.getInt(14) == 1);
            message.setAuthor(rs.getString(15));
            message.setTenantId(rs.getLong(16));

            return message;
        }
    }

    public HibernateSystemMessageDAO() {
        super(SystemMessage.class);
        super.log = LoggerFactory.getLogger(HibernateSystemMessageDAO.class);
    }

    @Override
    public List<SystemMessage> findByRecipient(String recipient, Message.Type type, Integer read)
            throws PersistenceException {
        StringBuilder sql = new StringBuilder(
                """
                select ld_lastmodified, ld_author, ld_messagetext, ld_subject, ld_sentdate,
                       ld_datescope, ld_prio, ld_confirmation, ld_lastnotified,
                       ld_status, ld_trials, ld_type, ld_id, ld_html, ld_author, ld_tenantid
                  from ld_systemmessage
                  where ld_deleted = 0
                    and ld_type = :type
                    and exists (select Q.ld_messageid
                                  from ld_recipient Q
                                 where Q.ld_name = :recipient
                                   and Q.ld_messageid=ld_id)
                """);

        if (read != null) {
            sql.append(" and %s".formatted(read == 1 ? "exists" : "not exists"));
            sql.append(
                    " (select R.ld_messageid from ld_recipient R where R.ld_name = :recipient and R.ld_read = 1 and R.ld_messageid = ld_id)");
        }

        sql.append(" order by ld_sentdate desc");

        return query(sql.toString(), Map.of("type", type.ordinal(), "recipient", recipient), new SystemMessageMapper(),
                null);
    }

    @Override
    public int getUnreadCount(String recipient, Message.Type type) throws PersistenceException {
        String sql = """
                     select count(distinct(R.ld_messageid))
                       from ld_recipient R, ld_systemmessage M
                      where R.ld_messageid = M.ld_id
                        and R.ld_name = :recipient
                        and M.ld_deleted = 0
                        and M.ld_type = :type and R.ld_read = 0
                     """;
        return queryForInt(sql, Map.of("type", type.ordinal(), "recipient", recipient));
    }

    @Override
    public void deleteExpiredMessages(String recipient) throws PersistenceException {
        collectGarbage(findByRecipient(recipient, Message.Type.SYSTEM, null), true);
    }

    /**
     * Cleans from the passed collection all expired messages
     * 
     * @param coll The input messages
     * @param removeExpired True if expired messages must be deleted
     * @return The cleaned messages collection
     * 
     * @throws PersistenceException Error in the database
     */
    protected List<SystemMessage> collectGarbage(Collection<SystemMessage> coll, boolean removeExpired)
            throws PersistenceException {
        List<SystemMessage> out = new ArrayList<>();

        Iterator<SystemMessage> iter = coll.iterator();
        Date date = new Date();
        long time = date.getTime();

        while (iter.hasNext()) {
            SystemMessage sm = iter.next();
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

        return out;
    }

    @Override
    public void deleteExpiredMessages(Message.Type type) throws PersistenceException {
        collectGarbage(findByType(type), true);
    }

    @Override
    public List<SystemMessage> findByMode(Recipient.Mode mode) throws PersistenceException {
        String sql = """
                       select ld_lastmodified, ld_author, ld_messagetext, ld_subject, ld_sentdate,
                              ld_datescope, ld_prio, ld_confirmation, ld_lastnotified,
                              ld_status, ld_trials, ld_type, ld_id, ld_html, ld_author, ld_tenantid
                         from ld_systemmessage
                        where ld_deleted = 0
                          and ld_id IN (select ld_messageid
                                          from ld_recipient
                                         where ld_mode = :mode)
                     order by ld_sentdate desc
                     """;
        return query(sql, Map.of("mode", mode.name()), new SystemMessageMapper(), null);
    }

    @Override
    public List<SystemMessage> findByType(Message.Type type) throws PersistenceException {
        String sql = """
                       select ld_lastmodified, ld_author, ld_messagetext, ld_subject, ld_sentdate,
                              ld_datescope, ld_prio, ld_confirmation, ld_lastnotified,
                              ld_status, ld_trials, ld_type, ld_id, ld_html, ld_author, ld_tenantid
                         from ld_systemmessage
                        where ld_deleted = 0
                          and ld_type = :type
                     order by ld_sentdate desc
                     """;
        return query(sql, Map.of("type", type.ordinal()), new SystemMessageMapper(), null);
    }

    @Override
    public void initialize(SystemMessage message) {
        refresh(message);

        for (Recipient recipient : message.getRecipients()) {
            recipient.getName();
        }
    }

    @Override
    public List<SystemMessage> findMessagesToBeSent(Message.Type type, int maxTrial) throws PersistenceException {
        StringBuilder sql = new StringBuilder(
                """
                 select ld_lastmodified, ld_author, ld_messagetext, ld_subject, ld_sentdate,
                        ld_datescope, ld_prio, ld_confirmation, ld_lastnotified,
                        ld_status, ld_trials, ld_type, ld_id, ld_html, ld_author, ld_tenantid
                   from ld_systemmessage
                  where ld_deleted = 0
                and not ld_status = :status
                    and ld_type = :type
                """);
        if (maxTrial > 0)
            sql.append(" and ld_trials < %d".formatted(maxTrial));
        sql.append(" order by ld_sentdate desc");

        return query(sql.toString(), Map.of("type", type.ordinal(), "status", SystemMessage.STATUS_DELIVERED),
                new SystemMessageMapper(), null);
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
        if (originalId == 0L && message.getType().equals(Message.Type.SYSTEM)) {
            UserDAO uDao = UserDAO.get();
            for (Recipient rec : message.getRecipients()) {
                if (rec.getType().equals(Recipient.Type.EMAIL))
                    continue;

                UserHistory history = new UserHistory();
                history.setTenantId(message.getTenantId());
                history.setComment(message.getMessageText());
                history.setEvent(UserEvent.MESSAGE_RECEIVED);
                history.setAuthor(message.getAuthor());
                history.setNotifyEvent(message.isNotify() && RunLevel.current().aspectEnabled(Aspect.SENDINGMESSAGES));

                User recipient = uDao.findByUsername(rec.getName());
                if (recipient != null)
                    history.setUser(recipient);

                User author = uDao.findByUsername(message.getAuthor());
                if (author != null)
                    history.setAuthor(author.getFullName());

                UserHistoryDAO.get().store(history);
            }
        }
    }
}