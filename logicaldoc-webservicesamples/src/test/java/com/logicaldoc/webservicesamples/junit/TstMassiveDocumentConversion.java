package com.logicaldoc.webservicesamples.junit;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import org.apache.commons.io.IOUtils;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.authorization.UnexistingResourceException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.soap.client.SoapDocumentClient;
import com.logicaldoc.webservice.soap.client.SoapFolderClient;

import jakarta.activation.DataHandler;
import jakarta.activation.DataSource;
import jakarta.activation.FileDataSource;

public class TstMassiveDocumentConversion extends BaseTestCase {

	private static final int TIME_ATTENTION_TRESHOLD = 30000;

	private static final int MAX_DOCUMENTS_TOBE_INSERTED = 15000;

	private static final Logger log = LoggerFactory.getLogger(TstMassiveDocumentConversion.class);

	private long startTime;

	public int docsInserted = 0;

	public int foldersCreated = 0;

	public List<Long> documents = new ArrayList<Long>();

	private SoapFolderClient folderClient;

	private SoapDocumentClient documentClient;

	@Override
	protected void setUp() throws Exception {
		super.setUp();

		folderClient = new SoapFolderClient(settings.getProperty("url") + "/services/Folder");
		documentClient = new SoapDocumentClient(settings.getProperty("url") + "/services/Document");
	}

	@Override
	protected void tearDown() throws Exception {
		super.tearDown();

		log.error("Inserted Documents: " + docsInserted);
		log.error("Created Folders: " + foldersCreated);
		long elapsedTime = System.currentTimeMillis() - startTime;
		double seconds = elapsedTime / 1000;
		log.error("Job ended in: " + Math.round(seconds) + " seconds");
		log.error("Job started at: " + new java.util.Date(startTime));
		log.error("Job ended at: " + new java.util.Date());
	}

	@Test
	public void testMassiveImport() {
		docsInserted = 0;
		startTime = System.currentTimeMillis();
		try {
			String sDir = "C:/Users/Alle/Desktop/test-documents";
			// String sDir = "C:/Users/Alle/Desktop/test-documents/Sedacom2";
			// String sDir = "C:/Users/Alle/Desktop/test-documents/Marketing";

			// create an initial folder from which to start the import session
			long branchFolderID = createInitialFolder(DEFAULT_WORKSPACE, "mPDFc");
			importFolder(sDir, branchFolderID);

			String basePath = "C:/tmp";

			// process the documents in folders and request their previews
			// the process is sequential, one request at a time
			// listFoldersRecursive(branchFolderID, basePath);

			downloadDocumentsMultiThread(basePath);

		} catch (Exception e) {
			e.printStackTrace();
			System.err.println("Documents currently Created: " + docsInserted);
		}
	}

	private void downloadDocumentsMultiThread(String basePath) throws IOException {

		/*
		 * ExecutorService exec =
		 * Executors.newFixedThreadPool(NUMBER_OF_THREADS); List<Future<?>>
		 * futures = new ArrayList<Future<?>>(NUMBER_OF_ITEMS); for (Item item :
		 * getItems()) { futures.add(exec.submit(new Processor(item))); }
		 * 
		 * for (Future<?> f : futures) { f.get(); // wait for a processor to
		 * complete } log.info("all items processed");
		 */

		Date date = new Date(); // creates a date based on current date/time

		// provides a formatting string for your eventual output
		SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
		System.out.println(sdf.format(date));

		String datePart = sdf.format(date);

		// Create path for the current folder
		final String parentPath = basePath + "/mPDFc_" + datePart;
		// check if the dir exists on the filesystem

		File file = new File(parentPath);
		if (!file.exists()) {
			file.mkdirs();
		}

		ExecutorService executor = Executors.newFixedThreadPool(10);
		System.out.println("TOTAL DOCUMENTS TO PROCESS: " + documents.size());

		for (final Long documentID : documents) {
			executor.submit(new Runnable() {
				@Override
				public void run() {
					try {
						doProcessing(documentID, parentPath);
					} catch (AuthenticationException | PermissionException | PersistenceException
							| UnexistingResourceException | WebserviceException | IOException e) {
						log.error(e.getMessage(), e);
					}
				}
			});
		}

		// http://stackoverflow.com/questions/1250643/how-to-wait-for-all-threads-to-finish-using-executorservice
		executor.shutdown();
		try {
			executor.awaitTermination(360, TimeUnit.SECONDS);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}

	private void doProcessing(Long docId, String parentPath) throws AuthenticationException, PermissionException,
			PersistenceException, UnexistingResourceException, WebserviceException, IOException {

		// get document data
		WSDocument wsDocument = documentClient.getDocument(sid, docId);
		System.out.println(docId + " : " + wsDocument.getFileName());

		System.out.println("Creating preview docID: " + docId);
		documentClient.createPdf(sid, docId, wsDocument.getFileVersion());

		DataHandler dh = documentClient.getResource(sid, docId, wsDocument.getFileVersion(), "conversion.pdf");
		File outFile = new File(parentPath, wsDocument.getFileName() + ".pdf");
		OutputStream os = new FileOutputStream(outFile);
		IOUtils.copy(dh.getInputStream(), os);
		os.close();
		System.out.println("Operation completed docID: " + docId);
	}

	private long createInitialFolder(long defaultWorkspace, String fname)
			throws AuthenticationException, PermissionException, PersistenceException, WebserviceException {

		WSFolder folder = new WSFolder();
		folder.setName(fname);
		folder.setParentId(defaultWorkspace);

		WSFolder fcreated = folderClient.create(sid, folder);
		System.out.println("Created folderID = " + fcreated.getId());
		return fcreated.getId();
	}

	private long createFolder(String name, long parentId)
			throws AuthenticationException, PermissionException, PersistenceException, WebserviceException {

		// Check if a subfolder of the parent folder has the same name
		List<WSFolder> dsds = folderClient.listChildren(sid, parentId);
		for (WSFolder wsFolder : dsds) {
			if (wsFolder.getName().equals(name)) {
				// System.out.println("FOLDER EXIST");
				return wsFolder.getId();
			}
		}

		// Folder Creation
		// result is the string "error" or the newly created folderId
		WSFolder folder = new WSFolder();
		folder.setName(name);
		folder.setParentId(parentId);

		WSFolder fcreated = folderClient.create(sid, folder);
		foldersCreated++;
		System.out.println("Created folderID = " + fcreated.getId());
		return fcreated.getId();
	}

	private void importFolder(String sDir, long parentId) throws AuthenticationException, PermissionException,
			PersistenceException, WebserviceException, IOException {

		File[] faFiles = new File(sDir).listFiles();

		for (File file : faFiles) {
			if (docsInserted >= MAX_DOCUMENTS_TOBE_INSERTED) {
				System.err.println("Reached limit of " + MAX_DOCUMENTS_TOBE_INSERTED + " document imported");
				return;
			}

			if (file.isDirectory()) {
				// System.out.println("FOLDER: " + file.getName());
				// Creation of the folder tree
				long childFolder = createFolder(file.getName(), parentId);
				importFolder(file.getAbsolutePath(), childFolder);
			} else {
				// if (file.length() < 31615) {
				if (file.length() < 31615000) {
					if (docsInserted % 100 == 0)
						System.err.println("Documents currently Created: " + docsInserted);

					// ****************************************************
					// Add file to document repository
					// ****************************************************
					System.out.println(file.getAbsolutePath());
					// Import document
					createDocument(parentId, file);
					docsInserted++;
				}
			}
		}
	}

	private void createDocument(long targetFolder, File file) throws AuthenticationException, PermissionException,
			PersistenceException, IOException, WebserviceException {

		DataSource ds = new FileDataSource(file);
		DataHandler content = new DataHandler(ds);

		WSDocument document = new WSDocument();
		document.setFileName(file.getName());

		document.setFolderId(targetFolder);
		document.setLanguage("en");

		long startTime = System.currentTimeMillis();
		WSDocument docRes = documentClient.create(sid, document, content);
		System.out.println("Created documentID = " + docRes.getId());
		long timeElapsed = System.currentTimeMillis() - startTime;
		if (timeElapsed > TIME_ATTENTION_TRESHOLD)
			System.err.println("Document created in: " + timeElapsed + " ms");
		documents.add(docRes.getId());
	}

	@Test
	public void testListFoldersRecursive(long parentFolderID, String basePath) throws AuthenticationException,
			PermissionException, PersistenceException, WebserviceException, IOException {
		WSFolder fcreated = folderClient.getFolder(sid, parentFolderID);

		// Create path for the current folder
		String parentPath = basePath + "/" + fcreated.getName();
		// check if the dir exists on the filesystem

		File file = new File(parentPath);
		if (!file.exists()) {
			file.mkdirs();
		}

		// Download documents in the current folder to disk
		downloadDocumentsPreviewInFolder(parentFolderID, parentPath);

		// Get folder children
		List<WSFolder> children = folderClient.listChildren(sid, parentFolderID);

		// Cycle over the results and download all the documents to their
		// folders
		for (WSFolder child : children) {
			System.out.println("folderId: " + child.getId());
			System.out.println("Name: " + child.getName());
			System.out.println("----------------");
			testListFoldersRecursive(child.getId(), parentPath);
		}
	}

	private void downloadDocumentsPreviewInFolder(long parentFolderID, String parentPath)
			throws AuthenticationException, PermissionException, PersistenceException, WebserviceException,
			IOException {

		// Download all the documents in Default Workspace folder (id=4,
		// name='Default')
		long folderId = parentFolderID;

		log.info("Inspecting documents in folder: " + folderId);

		List<WSDocument> docs = documentClient.listDocuments(sid, folderId, null);
		for (WSDocument wsDocument : docs) {
			long docId = wsDocument.getId();
			System.out.println(docId + " : " + wsDocument.getFileName());

			documentClient.createPdf(sid, docId, wsDocument.getFileVersion());
			log.info("Preview correctly created");

			DataHandler dh = documentClient.getResource(sid, docId, wsDocument.getFileVersion(), "conversion.pdf");
			File outFile = new File(parentPath, wsDocument.getFileName() + ".pdf");
			try (OutputStream os = new FileOutputStream(outFile)) {
				IOUtils.copy(dh.getInputStream(), os);
			}
		}
	}
}