package com.logicaldoc.web.data;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.IOException;

import org.junit.Test;

import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.servlet.MockServletRequest;
import com.logicaldoc.util.servlet.MockServletResponse;

public class UsersDataServletTest extends AbstractDataServletTestCase {

	private UsersDataServlet testSubject = new UsersDataServlet();

	@Test
	public void testService() throws IOException {
		MockServletRequest mockRequest = new MockServletRequest(servletSession);
		
		MockServletResponse response = new MockServletResponse(responseFile);
		testSubject.service(mockRequest, response);
		response.flushBuffer();
		assertTrue(FileUtil.readFile(responseFile).contains("<username><![CDATA[admin]]></username>"));
		assertTrue(FileUtil.readFile(responseFile).contains("<username><![CDATA[test]]></username>"));
		assertTrue(FileUtil.readFile(responseFile).contains("<username><![CDATA[author]]></username>"));
		assertTrue(FileUtil.readFile(responseFile).contains("<id></id><username></username>"));
		
		mockRequest.setParameter("required", "true");
		response = new MockServletResponse(responseFile);
		testSubject.service(mockRequest, response);
		response.flushBuffer();
		assertTrue(FileUtil.readFile(responseFile).contains("<username><![CDATA[admin]]></username>"));
		assertTrue(FileUtil.readFile(responseFile).contains("<username><![CDATA[test]]></username>"));
		assertTrue(FileUtil.readFile(responseFile).contains("<username><![CDATA[author]]></username>"));
		assertFalse(FileUtil.readFile(responseFile).contains("<id></id><username></username>"));
		
		
		mockRequest.setParameter("skipdisabled", "true");
		response = new MockServletResponse(responseFile);
		testSubject.service(mockRequest, response);
		response.flushBuffer();
		assertTrue(FileUtil.readFile(responseFile).contains("<username><![CDATA[admin]]></username>"));
		assertFalse(FileUtil.readFile(responseFile).contains("<username><![CDATA[test]]></username>"));
		assertTrue(FileUtil.readFile(responseFile).contains("<username><![CDATA[author]]></username>"));
		assertFalse(FileUtil.readFile(responseFile).contains("<id></id><username></username>"));
		
		
		
		mockRequest.setParameter("groupId", "1");
		response = new MockServletResponse(responseFile);
		testSubject.service(mockRequest, response);
		response.flushBuffer();
		assertTrue(FileUtil.readFile(responseFile).contains("<username><![CDATA[admin]]></username>"));
		assertFalse(FileUtil.readFile(responseFile).contains("<username><![CDATA[test]]></username>"));
		assertFalse(FileUtil.readFile(responseFile).contains("<username><![CDATA[author]]></username>"));
		assertFalse(FileUtil.readFile(responseFile).contains("<id></id><username></username>"));
	}
}