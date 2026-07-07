package com.logicaldoc.core.document;

import java.util.List;

import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;

import jakarta.transaction.Transactional;

/**
 * Hibernate implementation of <code>BookmarkDAO</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
@Repository("bookmarkDAO")
@Transactional
public class HibernateBookmarkDAO extends HibernatePersistentObjectDAO<Bookmark> implements BookmarkDAO {

    public HibernateBookmarkDAO() {
        super(Bookmark.class);
        super.log = LoggerFactory.getLogger(HibernateBookmarkDAO.class);
    }

    @Override
    public List<Bookmark> findByUserId(long userId) throws PersistenceException {
        return findByWhere("_entity.userId = %d".formatted(userId), "_entity.position asc", null);
    }

    @Override
    public Bookmark findByUserIdAndDocId(long userId, long docId) throws PersistenceException {
        return findByWhere("_entity.userId = %d and _entity.targetId = %d and _entity.type = %d".formatted(userId, docId,
                Bookmark.TYPE_DOCUMENT), null, null).stream().findFirst().orElse(null);
    }

    @Override
    public Bookmark findByUserIdAndFolderId(long userId, long folderId) throws PersistenceException {
        return findByWhere("_entity.userId = %d and _entity.targetId = %d and _entity.type = %d".formatted(userId,
                folderId, Bookmark.TYPE_FOLDER), null, null).stream().findFirst().orElse(null);
    }

    @Override
    public List<Long> findBookmarkedDocs(long userId) throws PersistenceException {
        return queryForList("select ld_docid from ld_bookmark where ld_type = %d and ld_deleted = 0 and ld_userid = %d"
                .formatted(Bookmark.TYPE_DOCUMENT, userId), Long.class);
    }

    @Override
    public boolean isDocBookmarkedByUser(long docId, long userId) throws PersistenceException {
        String sql = "select count(ld_docid) from ld_bookmark where ld_type = %d and ld_deleted = 0 and ld_userid = %d and ld_docid = %d"
                .formatted(Bookmark.TYPE_DOCUMENT, userId, docId);
        try {
            return queryForInt(sql) > 0;
        } catch (Exception t) {
            return queryForLong(sql) > 0;
        }
    }
}