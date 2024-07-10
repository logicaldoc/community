package com.logicaldoc.webdav.web;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;

import org.apache.jackrabbit.webdav.DavConstants;
import org.junit.Test;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.servlet.MockServletRequest;
import com.logicaldoc.util.servlet.MockServletResponse;
import com.logicaldoc.webdav.AbstractWebdavTestCase;

public class WebdavServletTest extends AbstractWebdavTestCase {

	private static final String PREFIX = "/store/Default";

	private static final String PROPFIND_SPEC = "<?xml version=\"1.0\" encoding=\"utf-8\" ?><propfind xmlns=\"DAV:\"  xmlns:srtns=\"http://www.southrivertech.com/\"><prop><creationdate/><getlastmodified/><getcontentlength/><href/><resourcetype/><locktoken/><lockdiscovery/><collection/><getetag/><activelock/><isreadonly/><ishidden/><Win32FileAttributes/><srtns:srt_modifiedtime/><srtns:srt_creationtime/><srtns:srt_lastaccesstime/><srtns:srt_proptimestamp/><BSI_isreadonly/><SRT_fileattributes/></prop></propfind>";

	// Instance under test
	private WebdavServlet testSubject = new WebdavServlet();

	private DocumentDAO docDao;

	@Override
	public void setUp() throws FileNotFoundException, IOException, SQLException, PluginException {
		super.setUp();

		docDao = (DocumentDAO) context.getBean("DocumentDAO");
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
	}

	public void testVERSIONCONTROL() throws IOException, PersistenceException {
		File tempFile = FileUtil.createTempFile("webdav", ".xml");
		try {
			MockServletResponse response = new MockServletResponse(tempFile);
			MockServletRequest request = prepareRequest("VERSION-CONTROL", "/five.pdf");
			testSubject.service(request, response);

		} finally {
			FileUtil.delete(tempFile);
		}
	}

	public void testREPORT() throws IOException, PersistenceException {
		// This WebDAV mehod is not really implemented
		File tempFile = FileUtil.createTempFile("webdav", ".xml");
		try {
			MockServletResponse response = new MockServletResponse(tempFile);
			MockServletRequest request = prepareRequest("REPORT", "/five.pdf");
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
			assertEquals(AbstractDocument.DOC_UNLOCKED, doc.getStatus());

			MockServletResponse response = new MockServletResponse(tempFile);
			MockServletRequest request = prepareRequest("CHECKOUT", "/five.pdf");
			testSubject.service(request, response);

			doc = docDao.findById(5L);
			assertEquals(AbstractDocument.DOC_CHECKED_OUT, doc.getStatus());

			request = prepareRequest("CHECKIN", "/five.pdf");
			request.setPayload(this.getClass().getResourceAsStream("/pdf2.pdf"));

			testSubject.service(request, response);

			doc = docDao.findById(5L);
			assertEquals(AbstractDocument.DOC_UNLOCKED, doc.getStatus());
		} finally {
			FileUtil.delete(tempFile);
		}
	}

	public void testLOCK() throws IOException, PersistenceException {
		File tempFile = FileUtil.createTempFile("webdav", ".xml");
		try {
			Document doc = docDao.findById(5L);
			assertEquals(AbstractDocument.DOC_UNLOCKED, doc.getStatus());

			MockServletResponse response = new MockServletResponse(tempFile);
			MockServletRequest request = prepareRequest("LOCK", "/five.pdf");
			testSubject.service(request, response);

			doc = docDao.findById(5L);
			assertEquals(AbstractDocument.DOC_CHECKED_OUT, doc.getStatus());

			request = prepareRequest("UNLOCK", "/five.pdf");
			testSubject.service(request, response);

			doc = docDao.findById(5L);
			assertEquals(AbstractDocument.DOC_UNLOCKED, doc.getStatus());
		} finally {
			FileUtil.delete(tempFile);
		}
	}

	public void testOPTIONS() throws IOException, PersistenceException {
		File tempFile = FileUtil.createTempFile("webdav", ".xml");
		try {
			MockServletResponse response = new MockServletResponse(tempFile);
			MockServletRequest request = prepareRequest("OPTIONS", "/five.pdf");
			testSubject.service(request, response);

			assertTrue(response.getHeader("Allow").contains("POST"));
			assertTrue(response.getHeader("Allow").contains("OPTIONS"));
		} finally {
			FileUtil.delete(tempFile);
		}
	}

	public void testMKCOL() throws IOException, PersistenceException {
		File tempFile = FileUtil.createTempFile("webdav", ".xml");
		try {
			assertFalse(callPROPFIND().contains(PREFIX + "/folder6/newfolder"));

			MockServletResponse response = new MockServletResponse(tempFile);
			MockServletRequest request = prepareRequest("MKCOL", "/folder6/newfolder");
			testSubject.service(request, response);

			assertTrue(callPROPFIND().contains(PREFIX + "/folder6/newfolder"));
		} finally {
			FileUtil.delete(tempFile);
		}
	}

	public void testMOVE() throws IOException, PersistenceException {
		File tempFile = FileUtil.createTempFile("webdav", ".xml");
		try {
			assertTrue(callPROPFIND().contains(PREFIX + "/five.pdf"));
			assertFalse(callPROPFIND().contains(PREFIX + "/folder6/moved.pdf"));

			MockServletResponse response = new MockServletResponse(tempFile);
			MockServletRequest request = prepareRequest("MOVE", "/five.pdf");
			request.setHeader(DavConstants.HEADER_DESTINATION, PREFIX + "/folder6/moved.pdf");
			testSubject.service(request, response);

			assertFalse(callPROPFIND().contains(PREFIX + "/five.pdf"));
			assertTrue(callPROPFIND().contains(PREFIX + "/folder6/moved.pdf"));
		} finally {
			FileUtil.delete(tempFile);
		}
	}

	public void testCOPY() throws IOException, PersistenceException {
		File tempFile = FileUtil.createTempFile("webdav", ".xml");
		try {
			assertTrue(callPROPFIND().contains(PREFIX + "/one.pdf"));
			assertFalse(callPROPFIND().contains(PREFIX + "/folder6/copied.pdf"));

			MockServletResponse response = new MockServletResponse(tempFile);
			MockServletRequest request = prepareRequest("COPY", "/one.pdf");
			request.setHeader(DavConstants.HEADER_DESTINATION, PREFIX + "/folder6/copied.pdf");
			testSubject.service(request, response);

			assertTrue(callPROPFIND().contains(PREFIX + "/one.pdf"));
			assertTrue(callPROPFIND().contains(PREFIX + "/folder6/copied.pdf"));

			// Copy over existing file
			response = new MockServletResponse(tempFile);
			request = prepareRequest("COPY", "/one.pdf");
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
			MockServletRequest request = prepareRequest("DELETE", "/one.pdf");
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

			MockServletRequest request = prepareRequest("PROPFIND", "");
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
			MockServletRequest request = prepareRequest("PUT", "/folder6/newfile1.pdf");
			request.setPayload(this.getClass().getResourceAsStream("/pdf1.pdf"));
			testSubject.service(request, response);

			response = new MockServletResponse(tempFile);
			request = prepareRequest("POST", "/folder6/newfile2.pdf");
			request.setPayload(this.getClass().getResourceAsStream("/pdf2.pdf"));
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
			MockServletRequest request = prepareRequest("GET", "/one.pdf");
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
			MockServletRequest request = prepareRequest("HEAD", "/one.pdf");
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
			MockServletRequest request = prepareRequest("PROPPATCH", "/one.pdf");
			testSubject.service(request, response);
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