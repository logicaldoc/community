package com.logicaldoc.core.folder;

import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.history.HibernateHistoryDAO;
import com.logicaldoc.util.sql.SqlUtil;

import jakarta.transaction.Transactional;

/**
 * Hibernate implementation of <code>FolderHistoryDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4
 */
@Repository("folderHistoryDAO")
@Transactional
public class HibernateFolderHistoryDAO extends HibernateHistoryDAO<FolderHistory> implements FolderHistoryDAO {

    private static final String ORDER_BY_ENTITY_DATE_ASC = "order by _entity.date asc";

    private HibernateFolderHistoryDAO() {
        super(FolderHistory.class);
        super.log = LoggerFactory.getLogger(HibernateFolderHistoryDAO.class);
    }

    @Override
    public List<FolderHistory> findByUserId(long userId) throws PersistenceException {
        return findByUserIdAndEvent(userId, null);
    }

    @Override
    public List<FolderHistory> findByFolderId(long folderId) throws PersistenceException {
        return findByWhere("_entity.folderId = %d".formatted(folderId), "_entity.date asc", null);
    }

    @Override
    public List<FolderHistory> findNotNotified(Integer max) throws PersistenceException {
        return findByWhere("_entity.notified = false", "_entity.date asc", max);
    }

    @Override
    public void cleanOldHistories(int ttl) throws PersistenceException {
        log.info("cleanOldHistories rows updated: {}", cleanOldRecords(ttl, "ld_folder_history"));
    }

    @Override
    public List<FolderHistory> findByUserIdAndEvent(long userId, String event) throws PersistenceException {
        StringBuilder query = new StringBuilder("_entity.userId = %d".formatted(userId));
        if (StringUtils.isNotEmpty(event))
            query.append(" and lower(_entity.event) like '%s'".formatted(SqlUtil.doubleQuotes(event.toLowerCase())));

        return findByWhere(query.toString(), ORDER_BY_ENTITY_DATE_ASC, null);
    }

    @Override
    public List<FolderHistory> findByPath(
            String pathExpression,
            Date oldestDate,
            Collection<String> events,
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
            query.append(" and _entity.event in ('%s')".formatted(events.stream().collect(Collectors.joining("','"))));

        return findByWhere(query.toString(), params, ORDER_BY_ENTITY_DATE_ASC, max);
    }

    @Override
    public List<FolderHistory> findByFolderIdAndEvent(long folderId, String event, Date oldestDate)
            throws PersistenceException {
        StringBuilder query = new StringBuilder("_entity.folderId = :folderId and _entity.event = :event ");

        Map<String, Object> params = new HashMap<>();
        params.put("folderId", folderId);
        params.put("event", event);

        if (oldestDate != null) {
            query.append(" and _entity.date >= :oldestDate");
            params.put("oldestDate", oldestDate);
        }

        return findByWhere(query.toString(), params, ORDER_BY_ENTITY_DATE_ASC, null);
    }
}