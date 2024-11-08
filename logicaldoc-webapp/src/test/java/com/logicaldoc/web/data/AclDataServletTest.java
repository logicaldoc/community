package com.logicaldoc.web.data;

import static org.junit.Assert.assertTrue;

import java.io.IOException;

import org.junit.Test;

import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.servlet.MockServletRequest;
import com.logicaldoc.util.servlet.MockServletResponse;

public class AclDataServletTest extends AbstractDataServletTestCase {

	// Instance under test
	private AclDataServlet testSubject = new AclDataServlet();

	@Test
	public void testService() throws IOException {
		MockServletRequest mockRequest = new MockServletRequest(servletSession);
		mockRequest.setParameter("id", "1500");
		mockRequest.setParameter("type", "menu");

		MockServletResponse response = new MockServletResponse(responseFile);
		testSubject.service(mockRequest, response);
		response.flushBuffer();
		assertTrue(FileUtil.readFile(responseFile).contains("<entityId>2</entityId>"));

		response.clear();
		mockRequest.setParameter("id", "5");
		mockRequest.setParameter("type", "folder");
		testSubject.service(mockRequest, response);
		assertTrue(FileUtil.readFile(responseFile).contains("<entityId>2</entityId>"));

		response.clear();
		mockRequest.setParameter("id", "-1");
		mockRequest.setParameter("type", "template");
		testSubject.service(mockRequest, response);
		assertTrue(FileUtil.readFile(responseFile).contains("<entityId>2</entityId>"));

		response.clear();
		mockRequest.setParameter("id", "1");
		mockRequest.setParameter("type", "document");
		testSubject.service(mockRequest, response);
		assertTrue(FileUtil.readFile(responseFile).contains("<entityId>2</entityId>"));
	}
}