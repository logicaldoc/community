package com.logicaldoc.webservice.soap.endpoint;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Test;

import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.webservice.AbstractWebserviceTestCase;
import com.logicaldoc.webservice.model.WSBookmark;

import junit.framework.Assert;

public class SoapBookmarkServiceTest extends AbstractWebserviceTestCase {

	// Instance under test
	private SoapBookmarkService testSubject;

	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		testSubject = new SoapBookmarkService();
		testSubject.setValidateSession(false);
	}

	@Test
	public void testBookmarkDocument() throws Exception {
		testSubject.bookmarkDocument("", 1L);
		testSubject.bookmarkDocument("", 2L);

		List<WSBookmark> bookmarks = testSubject.getBookmarks("");
		Assert.assertEquals(2, bookmarks.size());

		try {
			testSubject.bookmarkDocument("", 3L);
			Assert.fail("the document doesn't exist, why exception was not raised.");
		} catch (Exception e) {
			// Nothing to do
		}
	}

	@Test
	public void testBookmarkFolder() throws Exception {
		testSubject.bookmarkFolder("", 5L);
		testSubject.bookmarkFolder("", 4L);

		List<WSBookmark> bookmarks = testSubject.getBookmarks("");
		Assert.assertEquals(2, bookmarks.size());

		try {
			testSubject.bookmarkDocument("", 99L);
			Assert.fail("the folder doesn't exist, why exception was not raised.");
		} catch (Exception e) {
			// Nothing to do
		}

	}
}