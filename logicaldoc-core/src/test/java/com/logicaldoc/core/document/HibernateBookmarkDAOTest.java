package com.logicaldoc.core.document;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
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
 * Test case for {@link HibernateBookmarkDAOTest}
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
public class HibernateBookmarkDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private BookmarkDAO testSubject;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateDiscussionDAO
		testSubject = Context.get(BookmarkDAO.class);
	}

	@Test
	public void testStore() throws PersistenceException {
		Bookmark book1 = testSubject.findById(1);
		testSubject.initialize(book1);
		book1.setDescription("pippo");
		testSubject.store(book1);
		assertNotNull(book1);

		Bookmark book2 = testSubject.findById(2);
		testSubject.initialize(book2);
		book2.setDescription("paperino");
		testSubject.store(book2);
		assertNotNull(book2);
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testFindByUserId() throws PersistenceException {
		Collection bookmarks = testSubject.findByUserId(1);
		assertNotNull(bookmarks);
		assertEquals(2, bookmarks.size());

		bookmarks = testSubject.findByUserId(2);
		assertNotNull(bookmarks);
		assertEquals(1, bookmarks.size());

		// Try with non-existing user
		bookmarks = testSubject.findByUserId(99);
		assertNotNull(bookmarks);
		assertEquals(0, bookmarks.size());

		// Test Bookmark class methods
		Bookmark bookmark1 = testSubject.findByUserIdAndDocId(1, 1);
		assertEquals("blank", bookmark1.getIcon());
		assertNotNull(bookmark1.getPath());

		Bookmark bookmark2 = testSubject.findByUserIdAndDocId(2, 1);
		assertNotSame(bookmark1.hashCode(), bookmark2.hashCode());
		assertEquals(true, bookmark1.equals(bookmark1));
		assertEquals(false, bookmark1.equals(bookmark2));

		Bookmark bookmark3 = new Bookmark();
		bookmark3.setDescription("this is a bookmark");
		testSubject.store(bookmark3);
		
		bookmark3.setId(1);
		assertEquals(false, bookmark1.equals(bookmark3));
		
		bookmark3.setTitle("book1");
		bookmark3.setType(Bookmark.TYPE_FOLDER);
		assertEquals(false, bookmark1.equals(bookmark3));
		
		bookmark3.setType(Bookmark.TYPE_DOCUMENT);
		bookmark3.setUserId(1);
		assertEquals(bookmark1, bookmark3);
		
		bookmark3.setTitle(null);
		assertEquals(null, bookmark3.getTitle());
		
		assertEquals(false, bookmark3.equals(bookmark1));
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testFindBookmarkedDocs() throws PersistenceException {
		Collection bookmarks = testSubject.findByUserId(1);
		assertNotNull(bookmarks);
		assertEquals(2, bookmarks.size());

		List<Long> ids = testSubject.findBookmarkedDocs(1L);
		assertEquals(2, ids.size());

		ids = testSubject.findBookmarkedDocs(55L);
		assertEquals(0, ids.size());
	}

	@Test
	public void testIsDocBookmarkedByUser() throws PersistenceException {
		assertEquals(true, testSubject.isDocBookmarkedByUser(1L, 1L));
		assertEquals(false, testSubject.isDocBookmarkedByUser(55L, 1L));
	}

	@Test
	public void testFindByUserIdAndTargetId() throws PersistenceException {
		Bookmark bookmark = testSubject.findByUserIdAndDocId(1, 1);
		assertNotNull(bookmark);
		bookmark = testSubject.findByUserIdAndDocId(1, 2);
		assertNotNull(bookmark);

		Bookmark book1 = testSubject.findById(1);
		testSubject.initialize(book1);
		book1.setTargetId(3);
		testSubject.store(book1);

		bookmark = testSubject.findByUserIdAndDocId(1, 1);
		assertNull(bookmark);
		bookmark = testSubject.findByUserIdAndDocId(1, 2);
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

		testSubject.store(bmark);
		long bkmID = bmark.getId();

		testSubject.delete(bkmID);
		bmark = testSubject.findByUserIdAndDocId(1, bkmID);
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

		Bookmark bookmark = testSubject.findByUserIdAndDocId(1, 1);
		assertNotNull(bookmark);

		bookmark = testSubject.findByUserIdAndFolderId(1, 6);
		assertNull(bookmark);
	}
}
