package com.logicaldoc.core.document;

import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.history.HibernateHistoryDAO;
import com.logicaldoc.util.CollectionUtil;
import com.logicaldoc.util.sql.SqlUtil;

import jakarta.transaction.Transactional;

/**
 * Hibernate implementation of <code>HistoryDAO</code>
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 3.0
 */
@Repository("documentHistoryDAO")
@Transactional
public class HibernateDocumentHistoryDAO extends HibernateHistoryDAO<DocumentHistory> implements DocumentHistoryDAO {

    private HibernateDocumentHistoryDAO() {
        super(DocumentHistory.class);
        super.log = LoggerFactory.getLogger(HibernateDocumentHistoryDAO.class);
    }

    @Override
    public List<DocumentHistory> findByDocId(long docId) throws PersistenceException {
        return findByDocIdAndEvent(docId, null);
    }

    @Override
    public List<DocumentHistory> findByDocIdAndEvent(long docId, DocumentEvent event) throws PersistenceException {
        StringBuilder query = new StringBuilder("_entity.docId = %d".formatted(docId));

        if (event != null && StringUtils.isNotEmpty(event.toString()))
            query.append(" and _entity.event = '%s'".formatted(SqlUtil.doubleQuotes(event.toString())));

        return findByWhere(query.toString(), "_entity.date desc", null);
    }

    @Override
    public List<DocumentHistory> findByUserId(long userId) throws PersistenceException {
        return findByUserIdAndEvent(userId, null, null);
    }

    @Override
    public List<DocumentHistory> findByFolderId(long folderId) throws PersistenceException {
        return findByWhere("_entity.folderId = %d".formatted(folderId), "_entity.date asc", null);
    }

    @Override
    public List<DocumentHistory> findNotNotified(Integer max) throws PersistenceException {
        return findByWhere("_entity.notified = false", "_entity.date asc", max);
    }

    @Override
    public void cleanOldHistories(int ttl) throws PersistenceException {
        log.info("cleanOldHistories rows updated: {}", cleanOldRecords(ttl, "ld_history"));
    }

    @Override
    public List<DocumentHistory> findByUserIdAndEvent(long userId, String event, String sessionId)
            throws PersistenceException {
        StringBuilder query = new StringBuilder("_entity.userId = %d".formatted(userId));
        if (StringUtils.isNotEmpty(event))
            query.append(" and _entity.event = '%s'".formatted(SqlUtil.doubleQuotes(event)));
        if (StringUtils.isNotEmpty(sessionId))
            query.append(" and _entity.sessionId = '%s'".formatted(sessionId));

        return findByWhere(query.toString(), "_entity.date asc", null);
    }

    @Override
    public void markHistoriesAsRead(String event, long userId) throws PersistenceException {
        String statement = "update ld_history set ld_new = 0 where ld_new = 1 and ld_userid = %d and ld_event = '%s'"
                .formatted(userId, SqlUtil.doubleQuotes(event));
        jdbcUpdate(statement);
    }

    @Override
    public List<DocumentHistory> findByPath(String pathExpression, Date oldestDate, Collection<String> events,
            Integer max) throws PersistenceException {
        StringBuilder query = new StringBuilder(
                "(_entity.path like :pathExpression or _entity.pathOld like :pathExpression) ");
        Map<String, Object> params = new HashMap<>();
        params.put("pathExpression", pathExpression);

        if (oldestDate != null) {
            query.append(" and _entity.date >= :oldestDate ");
            params.put("oldestDate", oldestDate);
        }

        if (CollectionUtils.isNotEmpty(events))
            query.append(" and _entity.event in ('%s')".formatted(CollectionUtil.join(events, "','")));

        return findByWhere(query.toString(), params, " order by _entity.date asc", max);
    }
}