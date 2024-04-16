package com.logicaldoc.webservice.soap.endpoint;

import java.io.FileNotFoundException;
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
	private SoapBookmarkService bookmarkService;

	@Override
	public void setUp() throws FileNotFoundException, IOException, SQLException, PluginException {
		super.setUp();

		bookmarkService = new SoapBookmarkService();
		bookmarkService.setValidateSession(false);
	}

	@Test
	public void testBookmarkDocument() throws Exception {
		bookmarkService.bookmarkDocument("", 1L);
		bookmarkService.bookmarkDocument("", 2L);

		List<WSBookmark> bookmarks = bookmarkService.getBookmarks("");
		Assert.assertEquals(2, bookmarks.size());

		try {
			bookmarkService.bookmarkDocument("", 3L);
			Assert.fail("the document doesn't exist, why exception was not raised.");
		} catch (Exception e) {
			// Nothing to do
		}

	}

	@Test
	public void testBookmarkFolder() throws Exception {
		bookmarkService.bookmarkFolder("", 5L);
		bookmarkService.bookmarkFolder("", 4L);

		List<WSBookmark> bookmarks = bookmarkService.getBookmarks("");
		Assert.assertEquals(2, bookmarks.size());

		try {
			bookmarkService.bookmarkDocument("", 99L);
			Assert.fail("the folder doesn't exist, why exception was not raised.");
		} catch (Exception e) {
			// Nothing to do
		}

	}
}