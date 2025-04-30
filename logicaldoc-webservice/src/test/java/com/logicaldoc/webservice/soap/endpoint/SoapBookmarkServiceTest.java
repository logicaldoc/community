package com.logicaldoc.webservice.soap.endpoint;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Test;

import com.logicaldoc.core.document.Bookmark;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.webservice.AbstractWebserviceTestCase;
import com.logicaldoc.webservice.model.WSBookmark;

public class SoapBookmarkServiceTest extends AbstractWebserviceTestCase {

	private SoapBookmarkService testSubject;

	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		testSubject = new SoapBookmarkService();
		testSubject.setValidateSession(false);
	}

	@Test
	public void testSaveBookmark() throws Exception {
		assertEquals(0, testSubject.getBookmarks("").size());
		
		WSBookmark bookmark=new WSBookmark();
		bookmark.setTargetId(1L);
		bookmark.setType(Bookmark.TYPE_DOCUMENT);
		bookmark.setFileType("pdf");
		bookmark.setTitle("test.pdf");
		
		testSubject.saveBookmark("", bookmark);
		assertEquals(1, testSubject.getBookmarks("").size());
		
		bookmark=new WSBookmark();
		bookmark.setTargetId(4L);
		bookmark.setType(Bookmark.TYPE_FOLDER);
		bookmark.setFileType("folder");
		bookmark.setTitle("test");
		
		testSubject.saveBookmark("", bookmark);
		assertEquals(2, testSubject.getBookmarks("").size());
		
		testSubject.unbookmarkDocument("", 1L);
		assertEquals(1, testSubject.getBookmarks("").size());
		
		testSubject.unbookmarkFolder("", 4L);
		assertEquals(0, testSubject.getBookmarks("").size());
	}
	
	@Test
	public void testDeleteBookmark() throws Exception {
		assertEquals(0, testSubject.getBookmarks("").size());
		
		WSBookmark bookmark=new WSBookmark();
		bookmark.setTargetId(1L);
		bookmark.setType(Bookmark.TYPE_DOCUMENT);
		bookmark.setFileType("pdf");
		bookmark.setTitle("test.pdf");
		
		bookmark = testSubject.saveBookmark("", bookmark);
		assertEquals(1, testSubject.getBookmarks("").size());
		
		testSubject.deleteBookmark("", bookmark.getId());
		assertEquals(0, testSubject.getBookmarks("").size());
	}
	
	@Test
	public void testBookmarkDocument() throws Exception {
		testSubject.bookmarkDocument("", 1L);
		testSubject.bookmarkDocument("", 2L);

		List<WSBookmark> bookmarks = testSubject.getBookmarks("");
		assertEquals(2, bookmarks.size());

		try {
			testSubject.bookmarkDocument("", 3L);
			fail("the document doesn't exist, why exception was not raised.");
		} catch (Exception e) {
			// Nothing to do
		}
	}

	@Test
	public void testBookmarkFolder() throws Exception {
		testSubject.bookmarkFolder("", 5L);
		testSubject.bookmarkFolder("", 4L);

		List<WSBookmark> bookmarks = testSubject.getBookmarks("");
		assertEquals(2, bookmarks.size());

		try {
			testSubject.bookmarkDocument("", 99L);
			fail("the folder doesn't exist, why exception was not raised.");
		} catch (Exception e) {
			// Nothing to do
		}

	}
}