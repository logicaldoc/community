package com.logicaldoc.core.security.user;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.history.HibernateHistoryDAO;
import com.logicaldoc.core.security.Client;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;

import jakarta.transaction.Transactional;

@Repository("userHistoryDAO")
@Transactional
public class HibernateUserHistoryDAO extends HibernateHistoryDAO<UserHistory> implements UserHistoryDAO {

    private HibernateUserHistoryDAO() {
        super(UserHistory.class);
        super.log = LoggerFactory.getLogger(HibernateUserHistoryDAO.class);
    }

    @Override
    public List<UserHistory> findByUserId(long userId) throws PersistenceException {
        return findByUserIdAndEvent(userId, null, null);
    }

    @Override
    public List<UserHistory> findByUserIdAndEvent(long userId, String event, Date oldestDate)
            throws PersistenceException {
        StringBuilder query = new StringBuilder("_entity.userId = :userId");

        Map<String, Object> params = new HashMap<>();
        params.put("userId", userId);

        if (StringUtils.isNotEmpty(event)) {
            params.put("event", event);
            query.append(" and _entity.event = :event ");
        }

        if (oldestDate != null) {
            params.put("oldestDate", oldestDate);
            query.append(" and _entity.date >= :oldestDate");
        }

        return findByWhere(query.toString(), params, "order by _entity.date asc", null);
    }

    @Override
    public UserHistory createUserHistory(
            User user,
            UserEvent eventType,
            String comment,
            String sessionId,
            Client client) throws PersistenceException {
        UserHistory history = new UserHistory();
        history.setComment(comment);
        history.setEvent(eventType);

        Session session = SessionManager.get().get(sessionId);
        if (session != null)
            history.setSession(session);
        else
            history.setSessionId(sessionId);

        if (user != null)
            history.setUser(user);

        if (client != null) {
            history.setIp(client.getAddress());
            if (client.getDevice() != null)
                history.setDevice(client.getDevice().toString());
            if (client.getGeolocation() != null)
                history.setGeolocation(client.getGeolocation().toString());
        }

        store(history);
        return history;
    }

    @Override
    public void cleanOldHistories(int ttl) throws PersistenceException {
        log.info("cleanOldHistories rows updated: {}", cleanOldRecords(ttl, "ld_user_history"));
    }
}