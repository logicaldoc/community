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

import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.activation.FileDataSource;

import org.apache.commons.io.IOUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.soap.client.SoapDocumentClient;
import com.logicaldoc.webservice.soap.client.SoapFolderClient;

public class TstMassiveDocumentConversion extends BaseUnit{

	private static final int TIME_ATTENTION_TRESHOLD = 30000;

	private static final int MAX_DOCUMENTS_TOBE_INSERTED = 15000;

	protected static Log log = LogFactory.getLog(TstMassiveDocumentConversion.class);

	private static SoapDocumentClient dclient;
	private static SoapFolderClient fclient;
	
	private long startTime;
	public int docsInserted = 0;
	public int foldersCreated = 0;

	public List<Long> documents = new ArrayList<Long>();
	
	public TstMassiveDocumentConversion(String arg0) {
		super(arg0);
	}

	protected void setUp() throws Exception {
		super.setUp();
		dclient = new SoapDocumentClient(DOC_ENDPOINT);
		fclient = new SoapFolderClient(FOLDER_ENDPOINT);
	}

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

	public void testMassiveImport() {
		docsInserted = 0;
		startTime = System.currentTimeMillis();
		try {
			String sDir = "C:/Users/Alle/Desktop/test-documents";
			//String sDir = "C:/Users/Alle/Desktop/test-documents/Sedacom2";
			//String sDir = "C:/Users/Alle/Desktop/test-documents/Marketing"; 			
			
			// create an initial folder from which to start the import session
			long branchFolderID = createInitialFolder(DEFAULT_WORKSPACE, "mPDFc");
			importFolder(sDir, branchFolderID);			

			String basePath = "C:/tmp";
			
			// process the documents in folders and request their previews
			// the process is sequential, one request at a time
			//listFoldersRecursive(branchFolderID, basePath);
			
			downloadDocumentsMultiThread(basePath);
						
		} catch (Exception e) {
			e.printStackTrace();
			System.err.println("Documents currently Created: " + docsInserted);
		}
	}

	private void downloadDocumentsMultiThread(String basePath) throws IOException {
		
		/*
		ExecutorService exec = Executors.newFixedThreadPool(NUMBER_OF_THREADS);
	    List<Future<?>> futures = new ArrayList<Future<?>>(NUMBER_OF_ITEMS);
	    for (Item item : getItems()) {
	        futures.add(exec.submit(new Processor(item)));
	    }
	    
	    for (Future<?> f : futures) {
	        f.get(); // wait for a processor to complete
	    }
	    log.info("all items processed");	    
	    */
		
		Date date = new Date(); //creates a date based on current date/time

		//provides a formatting string for your eventual output
		SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd"); 		
		System.out.println(sdf.format(date));		
		
		String datePart =  sdf.format(date);
		
		// Create path for the current folder
		final String parentPath = basePath + "/mPDFc_" +datePart;
		//check if the dir exists on the filesystem
				
		File file = new File(parentPath);
		if (!file.exists()) {
			file.mkdirs();
		}				
		
		ExecutorService executor = Executors.newFixedThreadPool(10);
		final SoapDocumentClient docc = new SoapDocumentClient(DOC_ENDPOINT);

		System.out.println("TOTAL DOCUMENTS TO PROCESS: " +documents.size());
		
	    for(final Long documentID : documents) {
	        executor.submit(new Runnable() {
	            @Override
	            public void run() {
					doProcessing(documentID, parentPath, docc);
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

	protected void doProcessing(Long docId, String parentPath, SoapDocumentClient docc) {

		try {
			// get document data
			WSDocument wsDocument = docc.getDocument(sid, docId);
			System.out.println(docId +" : " + wsDocument.getFileName());
			
			System.out.println("Creating preview docID: " + docId);
			docc.createPdf(sid, docId, wsDocument.getFileVersion());

			DataHandler dh = docc.getResource(sid, docId, wsDocument.getFileVersion(), "conversion.pdf");
			File outFile = new File(parentPath, wsDocument.getFileName() +".pdf");
			OutputStream os = new FileOutputStream(outFile);
			IOUtils.copy(dh.getInputStream(), os);
			os.close();
			System.out.println("Operation completed docID: " + docId);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (Throwable tw) {
			// TODO Auto-generated catch block
			tw.printStackTrace();			
		} 				
	}

	private long createInitialFolder(long defaultWorkspace, String fname) throws Exception {

		WSFolder folder = new WSFolder();
		folder.setName(fname);
		folder.setParentId(defaultWorkspace);
		
		WSFolder fcreated = fclient.create(sid, folder);
		System.out.println("Created folderID = " + fcreated.getId());
		return fcreated.getId();
	}

	private long createFolder(String name, long parentId) throws Exception {

		// Check if a subfolder of the parent folder has the same name
		WSFolder[] dsds = fclient.listChildren(sid, parentId);
		if (dsds != null) {
			for (int i = 0; i < dsds.length; i++) {
				if (dsds[i].getName().equals(name)) {
					// System.out.println("FOLDER EXIST");
					return dsds[i].getId();
				}
			}
		}

		// Folder Creation
		// result is the string "error" or the newly created folderId
		WSFolder folder = new WSFolder();
		folder.setName(name);
		folder.setParentId(parentId);

		long startTime = System.currentTimeMillis();
		try {
			WSFolder fcreated = fclient.create(sid, folder);
			foldersCreated++;
			System.out.println("Created folderID = " + fcreated.getId());
			return fcreated.getId();
		} catch (Exception e) {
			long timeElapsed = System.currentTimeMillis() - startTime;
			System.err.println("TimeOut after: " + timeElapsed + " ms");
			throw e;
		}
	}

	private void importFolder(String sDir, long parentId) throws Exception {

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
				//if (file.length() < 31615) {
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

	private void createDocument(long targetFolder, File file)
			throws Exception {

		DataSource ds = new FileDataSource(file);
		DataHandler content = new DataHandler(ds);

		WSDocument document = new WSDocument();
		document.setFileName(file.getName());

		document.setFolderId(targetFolder);
		document.setLanguage("en");

		long startTime = System.currentTimeMillis();
		try {
			WSDocument docRes = dclient.create(sid, document, content);
			System.out.println("Created documentID = " + docRes.getId());
			long timeElapsed = System.currentTimeMillis() - startTime;
			if (timeElapsed > TIME_ATTENTION_TRESHOLD)
				System.err.println("Document created in: " + timeElapsed + " ms");
			documents.add(docRes.getId());
		} catch (Exception e) {
			long timeElapsed = System.currentTimeMillis() - startTime;
			System.err.println("Error after: " + timeElapsed + " ms");
			throw e;
		}
	}
	
	
	public void listFoldersRecursive(long parentFolderID, String basePath) throws Exception {
		
		SoapFolderClient folderc = new SoapFolderClient(FOLDER_ENDPOINT);
		
		WSFolder fcreated = folderc.getFolder(sid, parentFolderID);
		
		// Create path for the current folder
		String parentPath = basePath + "/" + fcreated.getName();
		//check if the dir exists on the filesystem
		
		File file = new File(parentPath);
		if (!file.exists()) {
			file.mkdirs();
		}
		
		// Download documents in the current folder to disk	
		downloadDocumentsPreviewInFolder(parentFolderID, parentPath);
		
		// Get folder children 
		WSFolder[] fchild = folderc.listChildren(sid, parentFolderID);
		
		// Cycle over the results and download all the documents to their folders
		if (fchild != null && fchild.length > 0) {
			for (int i = 0; i < fchild.length; i++) {
				System.out.println("folderId: " + fchild[i].getId());
				System.out.println("Name: " + fchild[i].getName());
				System.out.println("----------------");
				listFoldersRecursive (fchild[i].getId(), parentPath);
			}
		}		
	}
	
	
	private void downloadDocumentsPreviewInFolder(long parentFolderID, String parentPath) throws Exception {
		
		SoapDocumentClient docc = new SoapDocumentClient(DOC_ENDPOINT);				

		// Download all the documents in Default Workspace folder (id=4, name='Default')
		long folderId = parentFolderID;
		
		log.info("Inspecting documents in folder: " +folderId);
		
		WSDocument[] docs = docc.listDocuments(sid, folderId, null);
		if (docs != null) {
			log.error("docs.length: " + docs.length);
			for (WSDocument wsDocument : docs) {
				long docId = wsDocument.getId();
				System.out.println(docId +" : " + wsDocument.getFileName());
				
				docc.createPdf(sid, docId, wsDocument.getFileVersion());
				log.info("Preview correctly created");
				
				DataHandler dh = docc.getResource(sid, docId, wsDocument.getFileVersion(), "conversion.pdf");
				File outFile = new File(parentPath, wsDocument.getFileName() +".pdf");
				OutputStream os = new FileOutputStream(outFile);
				IOUtils.copy(dh.getInputStream(), os);
				os.close();
			}
		} else {
			log.info("No documents found in folder: " +folderId);
		}
		
	}	

}
