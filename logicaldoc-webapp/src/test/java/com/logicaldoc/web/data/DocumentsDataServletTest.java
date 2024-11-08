package com.logicaldoc.web.data;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.IOException;

import org.junit.Test;

import com.logicaldoc.util.servlet.MockServletRequest;
import com.logicaldoc.util.servlet.MockServletResponse;

public class DocumentsDataServletTest extends AbstractDataServletTestCase {

	// Instance under test
	private DocumentsDataServlet testSubject = new DocumentsDataServlet();

	@Test
	public void testService() throws IOException {
		MockServletRequest mockRequest = new MockServletRequest(servletSession);
		mockRequest.setParameter("page", "1");
		mockRequest.setParameter("folderId", "5");
		mockRequest.setParameter("extattr", "source,coverage");
		mockRequest.setParameter("sort", "date desc");

		MockServletResponse response = new MockServletResponse(responseFile);
		testSubject.service(mockRequest, response);
		response.flushBuffer();

		assertTrue(responseFile.exists());

		String output = response.getOutputString();
		assertTrue(output.contains("test.zip"));
		assertTrue(output.contains("<id>1</id>"));
		assertTrue(output.contains("<id>4</id>"));

		mockRequest = new MockServletRequest(servletSession);
		mockRequest.setParameter("page", "1");
		mockRequest.setParameter("folderId", "5");
		mockRequest.setParameter("extattr", "source,coverage");
		mockRequest.setParameter("sort", "date desc");
		mockRequest.setParameter("docIds", "1,4");
		mockRequest.setParameter("hiliteDocId", "4m");

		response.clear();
		response = new MockServletResponse(responseFile);
		testSubject.service(mockRequest, response);
		response.flushBuffer();

		assertTrue(responseFile.exists());

		output = response.getOutputString();
		assertTrue(output.contains("<id>1</id>"));
		assertTrue(output.contains("<id>4</id>"));

		mockRequest = new MockServletRequest(servletSession);
		mockRequest.setParameter("page", "1");
		mockRequest.setParameter("folderId", "5");
		mockRequest.setParameter("extattr", "source,coverage");
		mockRequest.setParameter("sort", "date desc");
		mockRequest.setParameter("status", "0");

		response.clear();
		response = new MockServletResponse(responseFile);
		testSubject.service(mockRequest, response);
		response.flushBuffer();

		assertTrue(responseFile.exists());

		output = response.getOutputString();
		assertEquals("<list></list>", output);
	}
}