package com.logicaldoc.core.dashlet;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.servlet.MockServletRequest;
import com.logicaldoc.util.servlet.MockServletResponse;

import jakarta.servlet.ServletException;

/**
 * Test case for {@link DashletContent}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.0.1
 */
public class DashletContentTest extends AbstractCoreTestCase {

	// Instance under test
	private DashletContent testSubject = new DashletContent();

	protected final File responseFile = new File("target/documents.xml");

	private DashletDAO dao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		FileUtil.delete(responseFile);
		prepareSession("admin", "admin");

		dao = DashletDAO.get();
	}

	@Override
	public void tearDown() throws IOException {
		super.tearDown();
		FileUtil.delete(responseFile);
	}

	@Test
	public void testService() throws IOException, PersistenceException {
		MockServletRequest mockRequest = new MockServletRequest(servletSession);
		mockRequest.setParameter("locale", "en");
		mockRequest.setParameter("dashletId", "21");

		MockServletResponse response = new MockServletResponse(responseFile);
		testSubject.service(mockRequest, response);
		response.flushBuffer();
		assertTrue(FileUtil.readFile(responseFile).contains("<id>1</id>"));

		FileUtil.delete(responseFile);
		mockRequest.setParameter("dashletId", "22");
		response = new MockServletResponse(responseFile);
		testSubject.service(mockRequest, response);
		response.flushBuffer();
		assertTrue(FileUtil.readFile(responseFile).contains("<id>1</id>"));

		// not null content
		FileUtil.delete(responseFile);
		mockRequest.setParameter("dashletId", "22");
		Dashlet dashlet = dao.findById(22);
		dashlet.setContent("This is some content");
		dao.store(dashlet);
		response = new MockServletResponse(responseFile);
		testSubject.service(mockRequest, response);
		response.flushBuffer();
		assertFalse(FileUtil.readFile(responseFile).contains("<id>1</id>"));

		FileUtil.delete(responseFile);
		mockRequest.setParameter("dashletId", "23");
		response = new MockServletResponse(responseFile);
		testSubject.service(mockRequest, response);
		response.flushBuffer();
		assertEquals("sample content", FileUtil.readFile(responseFile).trim());

		FileUtil.delete(responseFile);
		mockRequest.setParameter("dashletId", "6");
		response = new MockServletResponse(responseFile);
		testSubject.service(mockRequest, response);
		response.flushBuffer();
		assertTrue(FileUtil.readFile(responseFile).contains("<![CDATA[message for note 1]]>"));

		FileUtil.delete(responseFile);
		mockRequest.setParameter("dashletId", "9");
		response = new MockServletResponse(responseFile);
		testSubject.service(mockRequest, response);
		response.flushBuffer();
		assertEquals("", FileUtil.readFile(responseFile));

		FileUtil.delete(responseFile);
		mockRequest.setParameter("dashletId", "9");
		response = new MockServletResponse(responseFile);
		dashlet = dao.findById(9);
		dashlet.setType(Dashlet.TYPE_DOCUMENT);
		dashlet.setContent("This is some content for dashledId 9");
		dao.store(dashlet);
		testSubject.service(mockRequest, response);
		response.flushBuffer();
		assertEquals("This is some content for dashledId 9", FileUtil.readFile(responseFile));

		FileUtil.delete(responseFile);
		mockRequest.setParameter("dashletId", "9");
		response = new MockServletResponse(responseFile);
		dashlet = dao.findById(9);
		dashlet.setType(Dashlet.TYPE_NOTE);
		dashlet.setContent("This is some content for dashledId 9");
		dao.store(dashlet);
		testSubject.service(mockRequest, response);
		response.flushBuffer();
		assertEquals("This is some content for dashledId 9", FileUtil.readFile(responseFile));
	}

	@Test
	public void testValidateSession() throws ServletException {
		session = null;
		MockServletRequest mockRequest = new MockServletRequest(servletSession);
		DashletContent.validateSession(mockRequest);
	}
}