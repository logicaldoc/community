package com.logicaldoc.core.document;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Collection;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.plugin.PluginException;

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
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateDiscussionDAO
		dao = Context.get(BookmarkDAO.class);
	}

	@Test
	public void testStore() throws PersistenceException {
		Bookmark book1 = dao.findById(1);
		dao.initialize(book1);
		book1.setDescription("pippo");
		dao.store(book1);
		assertNotNull(book1);

		Bookmark book2 = dao.findById(2);
		dao.initialize(book2);
		book2.setDescription("paperino");
		dao.store(book2);
		assertNotNull(book2);
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testFindByUserId() throws PersistenceException {
		Collection bookmarks = dao.findByUserId(1);
		assertNotNull(bookmarks);
		assertEquals(2, bookmarks.size());

		bookmarks = dao.findByUserId(2);
		assertNotNull(bookmarks);
		assertEquals(1, bookmarks.size());

		// Try with non-existing user
		bookmarks = dao.findByUserId(99);
		assertNotNull(bookmarks);
		assertEquals(0, bookmarks.size());
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testFindBookmarkedDocs() throws PersistenceException {
		Collection bookmarks = dao.findByUserId(1);
		assertNotNull(bookmarks);
		assertEquals(2, bookmarks.size());

		List<Long> ids = dao.findBookmarkedDocs(1L);
		assertEquals(2, ids.size());

		ids = dao.findBookmarkedDocs(55L);
		assertEquals(0, ids.size());
	}

	@Test
	public void testIsDocBookmarkedByUser() throws PersistenceException {
		assertEquals(true, dao.isDocBookmarkedByUser(1L, 1L));
		assertEquals(false, dao.isDocBookmarkedByUser(55L, 1L));
	}

	@Test
	public void testFindByUserIdAndTargetId() throws PersistenceException {
		Bookmark bookmark = dao.findByUserIdAndDocId(1, 1);
		assertNotNull(bookmark);
		bookmark = dao.findByUserIdAndDocId(1, 2);
		assertNotNull(bookmark);

		Bookmark book1 = dao.findById(1);
		dao.initialize(book1);
		book1.setTargetId(3);
		dao.store(book1);

		bookmark = dao.findByUserIdAndDocId(1, 1);
		assertNull(bookmark);
		bookmark = dao.findByUserIdAndDocId(1, 2);
		assertNotNull(bookmark);
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
		assertNull(bmark);
	}

	@Test
	public void testFindByUserIdAndFolderId() throws PersistenceException {
		FolderDAO folderDao = Context.get(FolderDAO.class);
		Folder folder = folderDao.findById(6);
		assertNotNull(folder);
		
		DocumentDAO docDao = Context.get(DocumentDAO.class);
		Document doc = docDao.findById(1);
		assertNotNull(doc);
		
		Bookmark bookmark = dao.findByUserIdAndDocId(1, 1);
		assertNotNull(bookmark);
		
		bookmark = dao.findByUserIdAndFolderId(1, 6);
		assertNull(bookmark);
	}
}
