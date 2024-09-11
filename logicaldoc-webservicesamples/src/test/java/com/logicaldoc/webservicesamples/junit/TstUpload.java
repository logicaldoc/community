package com.logicaldoc.webservicesamples.junit;

import java.io.File;
import java.io.IOException;

import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.activation.FileDataSource;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Test;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.soap.client.SoapDocumentClient;

public class TstUpload extends BaseTestCase {

	protected static Log log = LogFactory.getLog(TstUpload.class);

	private SoapDocumentClient documentClient;

	@Override
	protected void setUp() throws Exception {
		super.setUp();

		documentClient = new SoapDocumentClient(settings.getProperty("url") + "/services/Document");
	}

	@Test
	public void testCreateDocument() throws AuthenticationException, PermissionException, PersistenceException,
			IOException, WebserviceException {

		File file = new File("C:/tmp/Get-started-EN.pdf");

		DataSource ds = new FileDataSource(file);
		DataHandler content = new DataHandler(ds);

		// This will create a new document children of the folder 5 (Documents)
		// The result is the Id of the new created document, otherwise error...
		WSDocument document = new WSDocument();
		document.setFileName(file.getName());
		document.setFolderId(DEFAULT_WORKSPACE);
		document.setLanguage("en");

		WSDocument docCreated = documentClient.create(sid, document, content);

		System.err.println("result = " + docCreated.getId());
	}
}