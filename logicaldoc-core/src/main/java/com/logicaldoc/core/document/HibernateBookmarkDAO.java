package com.logicaldoc.core.document;

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
public class HibernateBookmarkDAO extends HibernatePersistentObjectDAO<Bookmark> implements BookmarkDAO {

	private static final String AND = " and ";

	private static final String USER_ID = ".userId =";

	public HibernateBookmarkDAO() {
		super(Bookmark.class);
		super.log = LoggerFactory.getLogger(HibernateBookmarkDAO.class);
	}

	@Override
	public List<Bookmark> findByUserId(long userId) throws PersistenceException {
		return findByWhere(ENTITY + USER_ID + userId, "order by " + ENTITY + ".position asc", null);
	}

	@Override
	public Bookmark findByUserIdAndDocId(long userId, long docId) throws PersistenceException {
		List<Bookmark> list = findByWhere(ENTITY + USER_ID + userId + AND + ENTITY + ".targetId =" + docId + AND
				+ ENTITY + ".type=" + Bookmark.TYPE_DOCUMENT, null, null);

		if (list.isEmpty())
			return null;
		else
			return list.get(0);
	}

	@Override
	public Bookmark findByUserIdAndFolderId(long userId, long folderId) throws PersistenceException {
		List<Bookmark> list = findByWhere(ENTITY + USER_ID + userId + AND + ENTITY + ".targetId =" + folderId + AND
				+ ENTITY + ".type=" + Bookmark.TYPE_FOLDER, null, null);

		if (list.isEmpty())
			return null;
		else
			return list.get(0);
	}

	@Override
	public List<Long> findBookmarkedDocs(long userId) throws PersistenceException {
		String sql = "select ld_docid from ld_bookmark where ld_type=" + Bookmark.TYPE_DOCUMENT
				+ " and ld_deleted = 0 and ld_userid = " + userId;
		return queryForList(sql, Long.class);
	}

	@Override
	public boolean isDocBookmarkedByUser(long docId, long userId) throws PersistenceException {
		String sql = "select count(ld_docid) from ld_bookmark where ld_type=" + Bookmark.TYPE_DOCUMENT
				+ " and ld_deleted = 0 and ld_userid = " + userId + " and ld_docid = " + docId;
		try {
			return queryForInt(sql) > 0;
		} catch (Exception t) {
			return queryForLong(sql) > 0;
		}
	}
}
