package com.logicaldoc.webservicesamples.junit;

import java.io.File;

import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.activation.FileDataSource;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.soap.client.SoapDocumentClient;

public class TstUpload extends BaseUnit {

	protected static Log log = LogFactory.getLog(TstUpload.class);
	
	public TstUpload(String arg0) {
		super(arg0);
	}

	public void testCreateDocument() throws Exception {

		SoapDocumentClient docc = new SoapDocumentClient(DOC_ENDPOINT);

		File file = new File("C:/tmp/Get-started-EN.pdf");

		DataSource ds = new FileDataSource(file);
		DataHandler content = new DataHandler(ds);

		// This will create a new document children of the folder 5 (Documents)
		// The result is the Id of the new created document, otherwise error...
		WSDocument document = new WSDocument();
		document.setFileName(file.getName());
		document.setFolderId(DEFAULT_WORKSPACE);
		document.setLanguage("en");
		
		WSDocument docCreated = docc.create(sid, document, content);

		System.err.println("result = " + docCreated.getId());
	}

}
