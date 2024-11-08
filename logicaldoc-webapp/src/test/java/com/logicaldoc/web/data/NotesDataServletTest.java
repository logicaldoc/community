package com.logicaldoc.web.data;

import static org.junit.Assert.assertTrue;

import java.io.IOException;

import org.junit.Test;

import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.servlet.MockServletRequest;
import com.logicaldoc.util.servlet.MockServletResponse;

public class NotesDataServletTest extends AbstractDataServletTestCase {

	// Instance under test
	private NotesDataServlet testSubject = new NotesDataServlet();

	@Test
	public void testService() throws IOException {
		MockServletRequest mockRequest = new MockServletRequest(servletSession);
		mockRequest.setParameter("userId", "1");
		mockRequest.setParameter("docId", "1");
		mockRequest.setParameter("fileVersion", "1.0");

		MockServletResponse response = new MockServletResponse(responseFile);
		testSubject.service(mockRequest, response);
		response.flushBuffer();
		assertTrue(FileUtil.readFile(responseFile).contains("<![CDATA[message for note 1]]>"));
		
		response.clear();
		mockRequest.setParameter("page", "1");
		testSubject.service(mockRequest, response);
		response.flushBuffer();
		assertTrue(FileUtil.readFile(responseFile).contains("<![CDATA[message for note 1]]>"));
	}
}