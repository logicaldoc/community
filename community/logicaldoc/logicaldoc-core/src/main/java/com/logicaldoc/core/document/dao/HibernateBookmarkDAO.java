package com.logicaldoc.core.document.dao;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Bookmark;

/**
 * Hibernate implementation of <code>BookmarkDAO</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
@SuppressWarnings("unchecked")
public class HibernateBookmarkDAO extends HibernatePersistentObjectDAO<Bookmark> implements BookmarkDAO {

	public HibernateBookmarkDAO() {
		super(Bookmark.class);
		super.log = LoggerFactory.getLogger(HibernateBookmarkDAO.class);
	}

	@Override
	public List<Bookmark> findByUserId(long userId) {
		try {
			return findByWhere("_entity.userId =" + userId, "order by _entity.position asc", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Bookmark>();
		}
	}

	@Override
	public Bookmark findByUserIdAndDocId(long userId, long docId) {
		List<Bookmark> list = new ArrayList<Bookmark>();

		try {
			list = findByWhere("_entity.userId =" + userId + " and _entity.targetId =" + docId + " and _entity.type="
					+ Bookmark.TYPE_DOCUMENT, null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		if (list.isEmpty())
			return null;
		else
			return list.get(0);
	}

	@Override
	public Bookmark findByUserIdAndFolderId(long userId, long folderId) {
		List<Bookmark> list = new ArrayList<Bookmark>();
		try {
			list = findByWhere("_entity.userId =" + userId + " and _entity.targetId =" + folderId + " and _entity.type="
					+ Bookmark.TYPE_FOLDER, null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		if (list.isEmpty())
			return null;
		else
			return list.get(0);
	}

	@Override
	public List<Long> findBookmarkedDocs(long userId) {
		String sql = "select ld_docid from ld_bookmark where ld_type=" + Bookmark.TYPE_DOCUMENT
				+ " and ld_deleted = 0 and ld_userid = " + userId;
		try {
			return (List<Long>) queryForList(sql, Long.class);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Long>();
		}
	}

	@Override
	public boolean isDocBookmarkedByUser(long docId, long userId) {
		String sql = "select count(ld_docid) from ld_bookmark where ld_type=" + Bookmark.TYPE_DOCUMENT
				+ " and ld_deleted = 0 and ld_userid = " + userId + " and ld_docid = " + docId;
		try {
			return queryForInt(sql) > 0;
		} catch (Throwable t) {
			try {
				return queryForLong(sql) > 0;
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
				return false;
			}
		}
	}
}
