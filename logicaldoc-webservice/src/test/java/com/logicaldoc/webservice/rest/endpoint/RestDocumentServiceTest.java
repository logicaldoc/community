package com.logicaldoc.webservice.rest.endpoint;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.FileInputStream;
import java.io.IOException;
import java.sql.SQLException;

import javax.activation.DataHandler;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.Response;

import org.apache.cxf.jaxrs.ext.multipart.InputStreamDataSource;
import org.junit.Test;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.webservice.AbstractWebserviceTestCase;
import com.logicaldoc.webservice.WebserviceException;

/**
 * Test case for <code>SoapDocumentService</code>
 * 
 * @author Alessandro Gasparini
 * @since 9.1.1
 */
public class RestDocumentServiceTest extends AbstractWebserviceTestCase {

	// Instance under test
	private RestDocumentService testSubject;

	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		// Make sure that this is a DocumentServiceImpl instance
		testSubject = new RestDocumentService();
		testSubject.setValidateSession(false);
	}

	@Test
	public void testGetThumbnail() throws AuthenticationException, PersistenceException, PermissionException,
			WebserviceException, IOException {

		// First upload the resource
		FileInputStream fis = new FileInputStream("src/test/resources/fortnightemail-(1)-(1).png");
		DataHandler dh = new DataHandler(new InputStreamDataSource(fis, "image/png"));
		testSubject.uploadResource("", 1, "1.0", "mobile.png", dh);

		// Then check that the server return the resource
		Response res = testSubject.getThumbnail("mobile", "menu.adminxxx/text/menu.admin103/pippo", null);
		assertNotNull(res);

		MultivaluedMap<String, Object> sss = res.getHeaders();
		for (String element : sss.keySet()) {
			System.out.println("element: " + element + ", value: " + sss.get(element));
		}

		assertEquals("image/png", res.getHeaderString("Content-Type"));
		assertNotNull(res.getHeaderString("Expires"));
	}

}