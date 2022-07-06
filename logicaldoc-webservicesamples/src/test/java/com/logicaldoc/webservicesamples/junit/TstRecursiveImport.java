package com.logicaldoc.webservicesamples.junit;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.activation.FileDataSource;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.soap.client.SoapDocumentClient;
import com.logicaldoc.webservice.soap.client.SoapFolderClient;

public class TstRecursiveImport extends BaseUnit {

	protected static Log log = LogFactory.getLog(TstRecursiveImport.class);
	
	private static SoapDocumentClient dclient;
	private static SoapFolderClient fclient;
	private static String[] extensionArray = new String[]{"pdf", "txt", "html", "gif", "jpg", "xls", "xlsx"};
	List<String> allowedExtensions = Arrays.asList(extensionArray);

	public TstRecursiveImport(String arg0) {
		super(arg0);
	}

	protected void setUp() throws Exception {
		super.setUp();

		dclient = new SoapDocumentClient(DOC_ENDPOINT);
		fclient = new SoapFolderClient(FOLDER_ENDPOINT);
	}

	public void testRecursiveFolderImport() {
		try {
			String sDir = "C:/Users/alle/Desktop/LinkDetox";
			importTree(sDir, DEFAULT_WORKSPACE);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}


	private void importTree(String sDir, long parentId) throws Exception {

		File[] faFiles = new File(sDir).listFiles();
		for (File file : faFiles) {
			if (file.isDirectory()) {
				// System.out.println("FOLDER: " + file.getName());
				// Creation of the folder tree
				long childFolder = createFolder(file.getName(), parentId);
				importTree(file.getAbsolutePath(), childFolder);
			} else {
				System.out.println(file.getAbsolutePath());
				// System.out.println(file.getName());
				// Import document
				importDocument(file, parentId);
				//Thread.sleep(2 * 1000 * 60);
			}
		}
	}

	private void importDocument(File file, long parentId) throws Exception {

		String fileExt = FilenameUtils.getExtension(file.getName());
		if (!allowedExtensions.contains(fileExt.toLowerCase())) 
			return;
		
		DataSource ds = new FileDataSource(file);
		DataHandler content = new DataHandler(ds);

		WSDocument document = new WSDocument();
		document.setFolderId(parentId);
		document.setFileName(file.getName());

		WSDocument docRes = dclient.create(sid, document, content);

		System.out.println("documentID = " + docRes.getId());
	}

	private long createFolder(String name, long parentId) throws Exception {

		// Check if a subfolder of the parent folder has the same name
		WSFolder[] dsds = fclient.listChildren(sid, parentId);
		if (dsds != null) {
			for (int i = 0; i < dsds.length; i++) {
				if (dsds[i].getName().equals(name)) {
					System.out.println("FOLDER EXIST");
					return dsds[i].getId();
				}
			}
		}

		// Folder Creation
		// result is the string "error" or the newly created folderId
		WSFolder folder = new WSFolder();
		folder.setName(name);
		folder.setParentId(parentId);
		WSFolder fcreated = fclient.create(sid, folder);

		return fcreated.getId();
	}

}
