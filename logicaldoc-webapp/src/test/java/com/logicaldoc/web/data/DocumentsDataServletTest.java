package com.logicaldoc.web.data;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;

import org.junit.Test;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.util.servlet.MockServletRequest;
import com.logicaldoc.util.servlet.MockServletResponse;
import com.logicaldoc.util.spring.Context;

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

	@Test
	public void testServiceWithLastNote() throws Exception {
		DocumentDAO dao = Context.get(DocumentDAO.class);
		Document doc = dao.findById(1L);
		assertNotNull(doc);

		dao.initialize(doc);

		String original = doc.getLastNote();

		doc.setLastNote("note-one");
		dao.store(doc);

		doc.setLastNote("note-two");
		dao.store(doc);
		
		doc.setLastNote("note-three");
		dao.store(doc);

		MockServletRequest mockRequest = new MockServletRequest(servletSession);
		mockRequest.setParameter("page", "1");
		mockRequest.setParameter("folderId", String.valueOf(doc.getFolder().getId()));
		mockRequest.setParameter("sort", "date desc");

		MockServletResponse response = new MockServletResponse(responseFile);
		testSubject.service(mockRequest, response);
		response.flushBuffer();

		String output = response.getOutputString();

		assertTrue(output.contains("note-three"));

		doc.setLastNote(original);
		dao.store(doc);
	}
}