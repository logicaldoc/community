package com.logicaldoc.core.document;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.Collection;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Bookmark;
import com.logicaldoc.core.document.BookmarkDAO;

import junit.framework.Assert;

/**
 * Test case for <code>HibernateBookmarkDAOTest</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
public class HibernateBookmarkDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private BookmarkDAO dao;

	@Before
	public void setUp() throws FileNotFoundException, IOException, SQLException {
		super.setUp();
		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateDiscussionDAO
		dao = (BookmarkDAO) context.getBean("BookmarkDAO");
	}

	@Test
	public void testStore() throws PersistenceException {
		Bookmark book1 = dao.findById(1);
		dao.initialize(book1);
		book1.setDescription("pippo");
		dao.store(book1);
		Assert.assertNotNull(book1);

		Bookmark book2 = dao.findById(2);
		dao.initialize(book2);
		book2.setDescription("paperino");
		dao.store(book2);
		Assert.assertNotNull(book2);
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testFindByUserId() {
		Collection bookmarks = dao.findByUserId(1);
		Assert.assertNotNull(bookmarks);
		Assert.assertEquals(2, bookmarks.size());

		bookmarks = dao.findByUserId(2);
		Assert.assertNotNull(bookmarks);
		Assert.assertEquals(1, bookmarks.size());

		// Try with unexisting user
		bookmarks = dao.findByUserId(99);
		Assert.assertNotNull(bookmarks);
		Assert.assertEquals(0, bookmarks.size());
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testFindBookmarkedDocs() {
		Collection bookmarks = dao.findByUserId(1);
		Assert.assertNotNull(bookmarks);
		Assert.assertEquals(2, bookmarks.size());

		List<Long> ids = dao.findBookmarkedDocs(1L);
		Assert.assertEquals(2, ids.size());

		ids = dao.findBookmarkedDocs(55L);
		Assert.assertEquals(0, ids.size());
	}

	@Test
	public void testIsDocBookmarkedByUser() {
		Assert.assertTrue(dao.isDocBookmarkedByUser(1L, 1L));
		Assert.assertFalse(dao.isDocBookmarkedByUser(55L, 1L));
	}

	@Test
	public void testFindByUserIdAndTargetId() throws PersistenceException {
		Bookmark bookmark = dao.findByUserIdAndDocId(1, 1);
		Assert.assertNotNull(bookmark);
		bookmark = dao.findByUserIdAndDocId(1, 2);
		Assert.assertNotNull(bookmark);

		Bookmark book1 = dao.findById(1);
		dao.initialize(book1);
		book1.setTargetId(3);
		dao.store(book1);

		bookmark = dao.findByUserIdAndDocId(1, 1);
		Assert.assertNull(bookmark);
		bookmark = dao.findByUserIdAndDocId(1, 2);
		Assert.assertNotNull(bookmark);
	}
	
	@Test
	public void testDelete() throws PersistenceException {
		
		Bookmark bmark = new Bookmark();
		bmark.setType(Bookmark.TYPE_DOCUMENT);
		bmark.setTitle("Photo-2022-07-13-21-18-28_1495.jpg");
		bmark.setFileType("jpg");
		bmark.setUserId(1);
		bmark.setTargetId(244);
			
		dao.store(bmark);
		long bkmID = bmark.getId();
		
		dao.delete(bkmID);
		bmark = dao.findByUserIdAndDocId(1, bkmID);
		Assert.assertNull(bmark);
	}	
}
