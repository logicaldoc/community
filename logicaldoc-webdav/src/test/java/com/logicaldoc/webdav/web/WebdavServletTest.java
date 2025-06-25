package com.logicaldoc.webdav.web;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;

import jakarta.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.StringUtils;
import org.apache.jackrabbit.webdav.DavConstants;
import org.apache.jackrabbit.webdav.DavMethods;
import org.junit.Test;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentStatus;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.ResourceUtil;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.servlet.MockServletRequest;
import com.logicaldoc.util.servlet.MockServletResponse;
import com.logicaldoc.webdav.AbstractWebdavTestCase;

public class WebdavServletTest extends AbstractWebdavTestCase {

	private static final String PREFIX = "/store/Default";

	private static final String PROPFIND_SPEC = """
<?xml version='1.0' encoding='UTF-8' ?>
<propfind xmlns="DAV:" xmlns:CAL="urn:ietf:params:xml:ns:caldav" xmlns:CARD="urn:ietf:params:xml:ns:carddav" xmlns:SABRE="http://sabredav.org/ns" xmlns:OC="http://logicaldoc.com/ns">
	<prop>
		<displayname/>
		<getcontenttype/>
		<resourcetype/>
		<getcontentlength/>
		<getlastmodified/>
		<creationdate/>
		<getetag/>
		<OC:permissions/>
		<OC:id/>
		<OC:size/>
		<OC:privatelink/>
		<OC:share-types/>
	</prop>
</propfind>""";

	// Instance under test
	private WebdavServlet testSubject = new WebdavServlet();

	private DocumentDAO docDao;

	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		docDao = (DocumentDAO) context.getBean("documentDAO");
	}

	@Test
	public void testService() throws IOException, PersistenceException {
		/*
		 * We test the methods this way because if we implement them as standard
		 * JUnit test methods we get Hibernate session closed errors
		 */

		testPROPFIND();

		testGET();

		testLOCK();

		testCHECKIN();

		testHEAD();

		testPROPPATCH();

		testPUT();

		testCOPY();

		testMOVE();

		testMKCOL();

		testOPTIONS();

		testREPORT();

		testVERSIONCONTROL();

		testDELETE();

		testUNCHECKOUT();
	}

	public void testVERSIONCONTROL() throws IOException {
		File tempFile = FileUtil.createTempFile("webdav", ".xml");
		try {
			MockServletResponse response = new MockServletResponse(tempFile);
			MockServletRequest request = prepareRequest(DavMethods.METHOD_VERSION_CONTROL, "/five.pdf");
			testSubject.service(request, response);

		} finally {
			FileUtil.delete(tempFile);
		}
	}

	public void testREPORT() throws IOException {
		// This WebDAV mehod is not really implemented
		File tempFile = FileUtil.createTempFile("webdav", ".xml");
		try {
			MockServletResponse response = new MockServletResponse(tempFile);
			MockServletRequest request = prepareRequest(DavMethods.METHOD_REPORT, "/five.pdf");
			request.setBody("""
<?xml version="1.0" encoding="utf-8" ?>
<ld:filter-files xmlns:a="DAV:" xmlns:ld="http://logicaldoc.com/ns" >
        <a:prop>
                <ld:id/>
                <ld:fileid/>
                <ld:permissions/>
                <ld:downloadURL/>
        </a:prop>
</ld:filter-files>
""");
			testSubject.service(request, response);
		} finally {
			FileUtil.delete(tempFile);
		}
	}

	public void testCHECKIN() throws IOException, PersistenceException {
		File tempFile = FileUtil.createTempFile("webdav", ".xml");
		try {
			Document doc = docDao.findById(5L);
			assertEquals(DocumentStatus.UNLOCKED, doc.getStatus());

			MockServletResponse response = new MockServletResponse(tempFile);
			MockServletRequest request = prepareRequest(DavMethods.METHOD_CHECKOUT, "/five.pdf");
			testSubject.service(request, response);

			doc = docDao.findById(5L);
			assertEquals(DocumentStatus.CHECKEDOUT, doc.getStatus());

			request = prepareRequest("CHECKIN", "/five.pdf");
			request.setPayload(ResourceUtil.getInputStream("pdf2.pdf"));

			testSubject.service(request, response);

			doc = docDao.findById(5L);
			assertEquals(DocumentStatus.UNLOCKED, doc.getStatus());
		} finally {
			FileUtil.delete(tempFile);
		}
	}

	public void testLOCK() throws IOException, PersistenceException {
		File tempFile = FileUtil.createTempFile("webdav", ".xml");
		try {
			Document doc = docDao.findById(5L);
			assertEquals(DocumentStatus.UNLOCKED, doc.getStatus());

			MockServletResponse response = new MockServletResponse(tempFile);
			MockServletRequest request = prepareRequest(DavMethods.METHOD_LOCK, "/five.pdf");
			testSubject.service(request, response);

			doc = docDao.findById(5L);
			assertEquals(DocumentStatus.CHECKEDOUT, doc.getStatus());

			request = prepareRequest("UNLOCK", "/five.pdf");
			testSubject.service(request, response);

			doc = docDao.findById(5L);
			assertEquals(DocumentStatus.UNLOCKED, doc.getStatus());
		} finally {
			FileUtil.delete(tempFile);
		}
	}

	public void testOPTIONS() throws IOException {
		File tempFile = FileUtil.createTempFile("webdav", ".xml");
		try {
			MockServletResponse response = new MockServletResponse(tempFile);
			MockServletRequest request = prepareRequest(DavMethods.METHOD_OPTIONS, "/five.pdf");
			testSubject.service(request, response);

			assertTrue(response.getHeader("Allow").contains("POST"));
			assertTrue(response.getHeader("Allow").contains("OPTIONS"));
		} finally {
			FileUtil.delete(tempFile);
		}
	}

	public void testMKCOL() throws IOException {
		File tempFile = FileUtil.createTempFile("webdav", ".xml");
		try {
			assertFalse(callPROPFIND().contains(PREFIX + "/folder6/newfolder"));

			MockServletResponse response = new MockServletResponse(tempFile);
			MockServletRequest request = prepareRequest(DavMethods.METHOD_MKCOL, "/folder6/newfolder");
			testSubject.service(request, response);

			assertTrue(callPROPFIND().contains(PREFIX + "/folder6/newfolder"));
		} finally {
			FileUtil.delete(tempFile);
		}
	}

	public void testMOVE() throws IOException {
		File tempFile = FileUtil.createTempFile("webdav", ".xml");
		try {
			assertTrue(callPROPFIND().contains(PREFIX + "/five.pdf"));
			assertFalse(callPROPFIND().contains(PREFIX + "/folder6/moved.pdf"));

			MockServletResponse response = new MockServletResponse(tempFile);
			MockServletRequest request = prepareRequest(DavMethods.METHOD_MOVE, "/five.pdf");
			request.setHeader(DavConstants.HEADER_DESTINATION, PREFIX + "/folder6/moved.pdf");
			testSubject.service(request, response);

			assertFalse(callPROPFIND().contains(PREFIX + "/five.pdf"));
			assertTrue(callPROPFIND().contains(PREFIX + "/folder6/moved.pdf"));
		} finally {
			FileUtil.delete(tempFile);
		}
	}

	public void testCOPY() throws IOException {
		File tempFile = FileUtil.createTempFile("webdav", ".xml");
		try {
			assertTrue(callPROPFIND().contains(PREFIX + "/one.pdf"));
			assertFalse(callPROPFIND().contains(PREFIX + "/folder6/copied.pdf"));

			MockServletResponse response = new MockServletResponse(tempFile);
			MockServletRequest request = prepareRequest(DavMethods.METHOD_COPY, "/one.pdf");
			request.setHeader(DavConstants.HEADER_DESTINATION, PREFIX + "/folder6/copied.pdf");
			testSubject.service(request, response);

			assertTrue(callPROPFIND().contains(PREFIX + "/one.pdf"));
			assertTrue(callPROPFIND().contains(PREFIX + "/folder6/copied.pdf"));

			// Copy over existing file
			response = new MockServletResponse(tempFile);
			request = prepareRequest(DavMethods.METHOD_COPY, "/one.pdf");
			request.setHeader(DavConstants.HEADER_DESTINATION, PREFIX + "/folder6/copied.pdf");
			testSubject.service(request, response);

			assertTrue(callPROPFIND().contains(PREFIX + "/one.pdf"));
			assertTrue(callPROPFIND().contains(PREFIX + "/folder6/copied.pdf"));
		} finally {
			FileUtil.delete(tempFile);
		}
	}

	public void testDELETE() throws IOException {
		File tempFile = FileUtil.createTempFile("webdav", ".xml");
		try {
			assertTrue(callPROPFIND().contains(PREFIX + "/one.pdf"));

			MockServletResponse response = new MockServletResponse(tempFile);
			MockServletRequest request = prepareRequest(DavMethods.METHOD_DELETE, "/one.pdf");
			testSubject.service(request, response);

			assertFalse(callPROPFIND().contains(PREFIX + "/one.pdf"));
		} finally {
			FileUtil.delete(tempFile);
		}
	}

	private String callPROPFIND() throws IOException {
		File tempFile = FileUtil.createTempFile("webdav", ".xml");
		try {
			MockServletResponse response = new MockServletResponse(tempFile);

			MockServletRequest request = prepareRequest(DavMethods.METHOD_PROPFIND, "");
			request.setHeader("Accept-Encoding", "identity");
			request.setHeader("Content-Type", "application/xml; charset=utf-8");
			request.setBody(PROPFIND_SPEC);

			testSubject.service(request, response);

			return response.getOutputString();
		} finally {
			FileUtil.delete(tempFile);
		}
	}

	public void testPUT() throws IOException {
		File tempFile = FileUtil.createTempFile("webdav", ".xml");
		try {
			assertFalse(callPROPFIND().contains(PREFIX + "/folder6/newfile.pdf"));

			MockServletResponse response = new MockServletResponse(tempFile);
			MockServletRequest request = prepareRequest(DavMethods.METHOD_PUT, "/folder6/newfile1.pdf");
			request.setPayload(ResourceUtil.getInputStream("pdf1.pdf"));
			testSubject.service(request, response);

			response = new MockServletResponse(tempFile);
			request = prepareRequest("POST", "/folder6/newfile2.pdf");
			request.setPayload(ResourceUtil.getInputStream("pdf2.pdf"));
			testSubject.service(request, response);

			String responseBody = callPROPFIND();
			assertTrue(responseBody.contains(PREFIX + "/folder6/newfile1.pdf"));
			assertTrue(responseBody.contains(PREFIX + "/folder6/newfile2.pdf"));
		} finally {
			FileUtil.delete(tempFile);
		}
	}

	public void testPROPFIND() throws IOException {
		File tempFile = FileUtil.createTempFile("webdav", ".xml");
		try {
			String responseBody = callPROPFIND();
			assertFalse(StringUtils.isEmpty(responseBody));
			assertTrue(responseBody.contains(PREFIX + "/one.pdf"));
			assertTrue(responseBody.contains(PREFIX + "/folder6"));
		} finally {
			FileUtil.delete(tempFile);
		}
	}

	public void testGET() throws IOException {
		File tempFile = FileUtil.createTempFile("webdav", ".pdf");
		try {
			MockServletResponse response = new MockServletResponse(tempFile);
			MockServletRequest request = prepareRequest(DavMethods.METHOD_GET, "/one.pdf");
			testSubject.service(request, response);

			assertEquals(127810L, tempFile.length());
		} finally {
			FileUtil.delete(tempFile);
		}
	}

	public void testHEAD() throws IOException {
		File tempFile = FileUtil.createTempFile("webdav", ".xml");
		try {
			MockServletResponse response = new MockServletResponse(tempFile);
			MockServletRequest request = prepareRequest(DavMethods.METHOD_HEAD, "/one.pdf");
			testSubject.service(request, response);

			assertEquals("d-1_1.0", response.getHeaders().get("ETag"));
		} finally {
			FileUtil.delete(tempFile);
		}
	}

	public void testPROPPATCH() throws IOException {
		File tempFile = FileUtil.createTempFile("webdav", ".xml");
		try {
			MockServletResponse response = new MockServletResponse(tempFile);
			MockServletRequest request = prepareRequest(DavMethods.METHOD_PROPPATCH, "/one.pdf");
			testSubject.service(request, response);
		} finally {
			FileUtil.delete(tempFile);
		}
	}

	public void testUNCHECKOUT() throws IOException, PersistenceException {
		File tempFile = FileUtil.createTempFile("webdav", ".xml");
		try {
			Document doc = docDao.findById(7L);
			assertEquals(DocumentStatus.UNLOCKED, doc.getStatus());

			// SET THE STATUS TO CHECKOUT
			MockServletResponse response = new MockServletResponse(tempFile);
			MockServletRequest request = prepareRequest(DavMethods.METHOD_CHECKOUT,
					"/New error indexing documents.eml");
			testSubject.service(request, response);

			doc = docDao.findById(7L);
			assertEquals(DocumentStatus.CHECKEDOUT, doc.getStatus());

			// Perform the uncheckout
			response = new MockServletResponse(tempFile);
			request = prepareRequest(DavMethods.METHOD_UNCHECKOUT, "/New error indexing documents.eml");
			testSubject.service(request, response);

			// Check the response
			assertEquals(HttpServletResponse.SC_OK, response.getStatus());

			// Check the value
			doc = docDao.findById(7L);
			assertEquals(DocumentStatus.UNLOCKED, doc.getStatus());
		} finally {
			FileUtil.delete(tempFile);
		}
	}

	protected MockServletRequest prepareRequest(String method, String path) {
		MockServletRequest request = new MockServletRequest(servletSession);
		request.setMethod(method);
		request.setContextPath("");
		request.setRequestURI(PREFIX + path);
		return request;
	}
}