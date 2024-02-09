package com.logicaldoc.core.document;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;

/**
 * Hibernate implementation of <code>BookmarkDAO</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
@SuppressWarnings("unchecked")
public class HibernateBookmarkDAO extends HibernatePersistentObjectDAO<Bookmark> implements BookmarkDAO {

	private static final String AND = " and ";

	private static final String USER_ID = ".userId =";

	public HibernateBookmarkDAO() {
		super(Bookmark.class);
		super.log = LoggerFactory.getLogger(HibernateBookmarkDAO.class);
	}

	@Override
	public List<Bookmark> findByUserId(long userId) {
		try {
			return findByWhere(ENTITY + USER_ID + userId, "order by " + ENTITY + ".position asc", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	@Override
	public Bookmark findByUserIdAndDocId(long userId, long docId) {
		List<Bookmark> list = new ArrayList<>();

		try {
			list = findByWhere(ENTITY + USER_ID + userId + AND + ENTITY + ".targetId =" + docId + AND
					+ ENTITY + ".type=" + Bookmark.TYPE_DOCUMENT, null, null);
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
		List<Bookmark> list = new ArrayList<>();
		try {
			list = findByWhere(ENTITY + USER_ID + userId + AND + ENTITY + ".targetId =" + folderId + AND
					+ ENTITY + ".type=" + Bookmark.TYPE_FOLDER, null, null);
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
			return queryForList(sql, Long.class);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	@Override
	public boolean isDocBookmarkedByUser(long docId, long userId) {
		String sql = "select count(ld_docid) from ld_bookmark where ld_type=" + Bookmark.TYPE_DOCUMENT
				+ " and ld_deleted = 0 and ld_userid = " + userId + " and ld_docid = " + docId;
		try {
			return queryForInt(sql) > 0;
		} catch (Exception t) {
			try {
				return queryForLong(sql) > 0;
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
				return false;
			}
		}
	}
}
