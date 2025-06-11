package com.logicaldoc.webservicesamples.junit;

import java.io.File;
import java.io.IOException;

import jakarta.activation.DataHandler;
import jakarta.activation.DataSource;
import jakarta.activation.FileDataSource;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.soap.client.SoapDocumentClient;

public class TstUpload extends BaseTestCase {

	private static final Logger log = LoggerFactory.getLogger(TstUpload.class);

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