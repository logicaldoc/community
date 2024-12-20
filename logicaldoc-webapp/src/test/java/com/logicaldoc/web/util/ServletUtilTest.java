package com.logicaldoc.web.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import javax.servlet.ServletException;

import org.junit.Test;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.authentication.InvalidSessionException;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.store.Store;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.servlet.MockServletRequest;
import com.logicaldoc.util.servlet.MockServletResponse;
import com.logicaldoc.web.AbstractFulltextTestCase;

public class ServletUtilTest extends AbstractFulltextTestCase {

	private MockServletRequest mockRequest;

	private final static File RESPONSE_OUTPUT = new File("target/out.txt");

	private final static File PLUGIN_RESOURCE = new File(
			"target/tmp/logicaldoc/plugins/logicaldoc-core/test/resource.txt");

	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		mockRequest = new MockServletRequest(servletSession);
		FileUtil.delete(RESPONSE_OUTPUT);
		FileUtil.delete(PLUGIN_RESOURCE);
		PLUGIN_RESOURCE.getParentFile().mkdirs();
		FileUtil.writeFile("this is a test", PLUGIN_RESOURCE.getAbsolutePath());
		assertEquals("this is a test", FileUtil.readFile(PLUGIN_RESOURCE));
	}

	@Override
	public void tearDown() throws SQLException {
		super.tearDown();
		FileUtil.delete(RESPONSE_OUTPUT);
		FileUtil.delete(PLUGIN_RESOURCE);
	}

	@Override
	protected List<String> getPluginArchives() {
		return List.of("/logicaldoc-core-plugin.jar");
	}

	@Test
	public void testValidateSession() {
		Session session = ServletUtil.validateSession(mockRequest);
		assertNotNull(session);

		ServletUtil.validateSession(session.getSid());

		try {
			ServletUtil.validateSession("unexisting");
			fail("Unexisting session did not generate exception");
		} catch (Exception e) {
			// All ok
		}

		try {
			SessionManager.get().kill(session.getSid());
			ServletUtil.validateSession(session.getSid());
			fail("Closed session did not generate exception");
		} catch (Exception e) {
			// All ok
		}
	}

	@Test
	public void testGetSessionUser() {
		User user = ServletUtil.getSessionUser(mockRequest);
		assertNotNull(user);
		assertEquals("admin", user.getUsername());

		Session session = ServletUtil.validateSession(mockRequest);
		user = ServletUtil.getSessionUser(session.getSid());
		assertNotNull(user);
		assertEquals("admin", user.getUsername());
	}

	@Test
	public void testCheckMenu()
			throws InvalidSessionException, ServletException, PersistenceException, ServerException {
		Session session = ServletUtil.checkMenu(mockRequest, 3L);
		assertNotNull(session);

		prepareSession("boss", "admin");

		try {
			ServletUtil.checkMenu(mockRequest, -999L);
			fail("Unexisting menu did not generate exception");
		} catch (ServletException e) {
			// All ok
		}

		session = ServletUtil.checkEvenOneMenu(mockRequest, 5L, 1500L, -999L);
		assertNotNull(session);

		try {
			ServletUtil.checkEvenOneMenu(mockRequest, -999L, -888L, -777L);
			fail("Unexisting menus did not generate exception");
		} catch (ServletException e) {
			// All ok
		}
	}

	@Test
	public void testDownloadPluginResource() throws InvalidSessionException, IOException {
		Session session = ServletUtil.validateSession(mockRequest);
		MockServletResponse mockResponse = new MockServletResponse(RESPONSE_OUTPUT);
		ServletUtil.downloadPluginResource(mockRequest, mockResponse, session.getSid(), "logicaldoc-core",
				"test/resource.txt", "test.txt");
		assertEquals("this is a test", FileUtil.readFile(RESPONSE_OUTPUT));
	}

	@Test
	public void testDownloadDocument()
			throws InvalidSessionException, IOException, PersistenceException, ServletException {
		Session session = ServletUtil.validateSession(mockRequest);
		MockServletResponse mockResponse = new MockServletResponse(RESPONSE_OUTPUT);
		ServletUtil.downloadDocument(mockRequest, mockResponse, session.getSid(), 1L, "1.0", "test.txt", null,
				ServletUtil.getSessionUser(mockRequest));
		assertEquals(127810L, RESPONSE_OUTPUT.length());

		FileUtil.delete(RESPONSE_OUTPUT);
		mockRequest.setHeader("Range", "bytes=0-24");
		ServletUtil.downloadDocument(mockRequest, mockResponse, session.getSid(), 1L, "1.0", "test.txt", null,
				ServletUtil.getSessionUser(mockRequest));
		assertEquals(25L, RESPONSE_OUTPUT.length());

		FileUtil.delete(RESPONSE_OUTPUT);
		mockRequest.removeHeader("Range");
		ServletUtil.downloadDocument(mockRequest, mockResponse, session.getSid(), 1L, "1.0", "test.txt",
				ServletUtil.getSessionUser(mockRequest));
		assertEquals(127810L, RESPONSE_OUTPUT.length());
	}

	@Test
	public void testGetSelfURL() {
		String selfUrl = ServletUtil.getSelfURL(mockRequest);
		assertEquals("null://null", selfUrl);
	}

	@Test
	public void testDownloadFile() throws InvalidSessionException, IOException {
		MockServletResponse mockResponse = new MockServletResponse(RESPONSE_OUTPUT);
		ServletUtil.downloadFile(mockRequest, mockResponse, PLUGIN_RESOURCE, "test.txt");
		assertEquals("this is a test", FileUtil.readFile(RESPONSE_OUTPUT));
	}

	@Test
	public void testDownloadDocumentText() throws InvalidSessionException, IOException, PersistenceException {
		MockServletResponse mockResponse = new MockServletResponse(RESPONSE_OUTPUT);
		ServletUtil.downloadDocumentText(mockRequest, mockResponse, 1L, ServletUtil.getSessionUser(mockRequest));
		assertTrue(FileUtil.readFile(RESPONSE_OUTPUT).startsWith("Questo e un documento di prova."));
	}

	@Test
	public void testUploadDocumentResource() throws InvalidSessionException, IOException, PersistenceException {

		mockRequest.setContentType(
				"multipart/form-data; boundary=---------------------------110206366124743586943686920340");
		mockRequest.setMethod("POST");

		String body = """
-----------------------------110206366124743586943686920340
Content-Disposition: form-data; name="Filedata"; filename="test.txt"
Content-Type: text/plain\r
\r
test.txt\r
-----------------------------110206366124743586943686920340--
""";

		mockRequest.setBody(body);

		ServletUtil.uploadDocumentResource(mockRequest, 1L, "abc", "1.0", "1.0");
		Store store = Context.get(Store.class);
		assertEquals(8L, store.size(1L, store.getResourceName(1L, "1.0", "abc")));
	}

}