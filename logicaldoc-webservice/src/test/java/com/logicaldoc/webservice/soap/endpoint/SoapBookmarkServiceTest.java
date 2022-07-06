package com.logicaldoc.webservice.soap.endpoint;

import junit.framework.Assert;

import org.junit.Test;

import com.logicaldoc.webservice.AbstractWebserviceTestCase;
import com.logicaldoc.webservice.model.WSBookmark;

public class SoapBookmarkServiceTest extends AbstractWebserviceTestCase {

	// Instance under test
	private SoapBookmarkService bookmarkService;

	@Override
	public void setUp() throws Exception {
		super.setUp();

		bookmarkService = new SoapBookmarkService();
		bookmarkService.setValidateSession(false);
	}

	@Test
	public void testBookmarkDocument() throws Exception {
		bookmarkService.bookmarkDocument("", 1L);
		bookmarkService.bookmarkDocument("", 2L);

		WSBookmark[] bookmarks = bookmarkService.getBookmarks("");
		Assert.assertEquals(2, bookmarks.length);

		try {
			bookmarkService.bookmarkDocument("", 3L);
			Assert.fail("the document doesn't exist, why exception was not raised.");
		} catch (Throwable t) {
			// We expect to be here
		}
	}

	@Test
	public void testBookmarkFolder() throws Exception {
		bookmarkService.bookmarkFolder("", 5L);
		bookmarkService.bookmarkFolder("", 4L);

		WSBookmark[] bookmarks = bookmarkService.getBookmarks("");
		Assert.assertEquals(2, bookmarks.length);

		try {
			bookmarkService.bookmarkDocument("", 99L);
			Assert.fail("the folder doesn't exist, why exception was not raised.");
		} catch (Throwable t) {
			// We expect to be here
		}
	}
}