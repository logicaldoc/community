package com.logicaldoc.webservice;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.IOUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.logicaldoc.webservice.model.WSBookmark;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSNote;
import com.logicaldoc.webservice.model.WSRating;
import com.logicaldoc.webservice.model.WSSearchOptions;
import com.logicaldoc.webservice.model.WSSearchResult;
import com.logicaldoc.webservice.rest.client.RestAuthClient;
import com.logicaldoc.webservice.rest.client.RestBookmarkClient;
import com.logicaldoc.webservice.rest.client.RestDocumentClient;
import com.logicaldoc.webservice.rest.client.RestDocumentMetadataClient;
import com.logicaldoc.webservice.rest.client.RestFolderClient;
import com.logicaldoc.webservice.rest.client.RestSearchClient;
import com.logicaldoc.webservice.rest.client.RestTagClient;

import jakarta.activation.DataHandler;
import jakarta.activation.DataSource;
import jakarta.activation.FileDataSource;

public class RestWorkbench {

	private static RestAuthClient authClient = null;

	private static RestDocumentClient docClient = null;

	private static RestDocumentMetadataClient docMetadataClient = null;

	private static RestFolderClient fldClient = null;

	private static RestSearchClient searchClient = null;

	private static RestTagClient tagClient = null;

	private static RestBookmarkClient bookmarkClient = null;

	private static String BASE_PATH = "http://localhost:9080";

	public static void main(String[] args) throws Exception {
		// String test1="<?xml version=\"1.0\"
		// encoding=\"utf-8\"?><soap:Envelope
		// xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\"
		// xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
		// xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"><soap:Body><checkin
		// xmlns=\"http://ws.logicaldoc.com\"><sid
		// xmlns=\"\">XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX2a84c8</sid><docId
		// xmlns=\"\">723651853</docId><filename xmlns=\"\">17.01.11 -
		// F33OC2Z.doc</filename><release xmlns=\"\">false</release><content
		// xmlns=\"\">e1xydGYxXGFkZWZsYW5nMTAyNVx</content>";

		// System.out.println(test1.replaceAll("<content[^>]*>.*</content>",
		// "<content>...</content>"));

		Properties develProps = new Properties();
		develProps.load(new FileInputStream(new File(System.getProperty("user.home")+"/logicaldoc-dev.properties")));
		String apiKey = develProps.getProperty("apikey.Development");

		authClient = new RestAuthClient(BASE_PATH + "/services/rest/auth");
		docClient = new RestDocumentClient(BASE_PATH + "/services/rest/document", apiKey);
		fldClient = new RestFolderClient(BASE_PATH + "/services/rest/folder", apiKey);
		searchClient = new RestSearchClient(BASE_PATH + "/services/rest/search", apiKey);
		tagClient = new RestTagClient(BASE_PATH + "/services/rest/tag", apiKey);
		bookmarkClient = new RestBookmarkClient(BASE_PATH + "/services/rest/bookmark", apiKey);
		docMetadataClient = new RestDocumentMetadataClient(BASE_PATH + "/services/rest/documentMetadata", apiKey);

		String sid = authClient.loginApiKey(apiKey);
		System.err.println(sid);
//		
//		docMetadataClient.setAttributeOptions(188055552L, "test", new String[] {"value1", "value2", "value3"});
//		
//		authClient.logout(sid);
//		System.err.println("op finished");

		// createDocument();

		// tagStuff();

		// bookmarkStuff();

		// noteStuff();

		// ratingStuff();

//		folderStuff();

		// Note: 04L is the id of the default workspace

		// listDocuments(04L, null);
		// listDocuments(04L, "InvoiceProcessing01-workflow*.png"); // 4
		// documents
		// listDocuments(04L, "InvoiceProcessing01-workflow.png"); // 1 document
		// listDocuments(04L, "InvoiceProcessing01-workflow(3).png"); // 1
		// document
		// listFolderChildren(3342342L);
		// listFolderChildren(4L);

		// createPath(04L, "/sgsgsgs/Barisoni/rurururu");
		// createPath(04L, "/La zanzara/Cruciani/coloqui via Sky");

		// getFolder(04L);

		// checkout(735L);
		// checkin(735L);
		// getVersionContent(735L, "1.2");

		/*
		 * WSDocument myDoc = getDocument(3407874L);
		 * 
		 * Calendar cal = Calendar.getInstance(); SimpleDateFormat sdf = new
		 * SimpleDateFormat("HH:mm"); System.out.println(
		 * sdf.format(cal.getTime()) );
		 * 
		 * myDoc.setTitle("document test(" +sdf.format(cal.getTime()) +")");
		 * 
		 * updateDocument(myDoc);
		 */

		// WSFolder cfld = createFolder();
		// updateFolder(cfld, "Folder updated by REST ws");

		// createDocument02();
		// createFolder(04L, "DJ KATCH");

		// deleteDocument(3375105L);
		// deleteFolder(4128768L);
		// updateDocument(164986881L);

		// listDocuments(04L);
		// listDocuments(04L, "InvoiceProcessing01-workflow*.png"); // 3
		// documents
		// listDocuments(04L, "InvoiceProcessing01-workflow.png"); // 0 document
		// listDocuments(04L, "InvoiceProcessing01-workflow(3).png"); // 1
		// document
		// listFolderChildren(3342386L);
		// listFolderChildren(4L);

		// createPath(04L, "/sgsgsgs/Barisoni/rurururu");
		// createPath(04L, "/La zanzara/Cruciani/coloqui via Sky");

		// getFolder(04L);

		// WSDocument myDoc = getDocument(3375109L);
		//
		// Calendar cal = Calendar.getInstance();
		// SimpleDateFormat sdf = new SimpleDateFormat("HH:mm");
		// System.out.println( sdf.format(cal.getTime()) );
		//
		// myDoc.setTitle("document test(" +sdf.format(cal.getTime()) +")");
		//
		// updateDocument(myDoc);

		// createDocument02();
		// createFolder(04L, "DJ KATCH");

		// deleteDocument(4325376L);
		// deleteFolder(4128768L);
		// updateDocument(164986881L);

		// long start_time = System.nanoTime();

		WSSearchOptions options = new WSSearchOptions();
		options.setLanguage("en");
		options.setExpression("release");
		options.setExpressionLanguage("en");
		options.setFields(List.of("fileName", "content"));
		options.setFolderId(4L);
		options.setSearchInSubPath(1);
		options.setMaxHits(50);
		
		find(options);

		/*
		 * WSSearchOptions wsso = buildSearchOptions("en",
		 * "document management system"); find(sid, wsso);
		 * 
		 * wsso = buildSearchOptions("en", "document management"); find(sid,
		 * wsso);
		 * 
		 * wsso = buildSearchOptions("en", "Document Management"); find(sid,
		 * wsso);
		 * 
		 * wsso = buildSearchOptions("en", "Document Management system");
		 * find(sid, wsso);
		 * 
		 * wsso = buildSearchOptions("en", "Management system"); find(sid,
		 * wsso);
		 * 
		 * wsso = buildSearchOptions("en", "document system"); find(sid, wsso);
		 * 
		 * wsso = buildSearchOptions("en", "documental system"); find(sid,
		 * wsso);
		 * 
		 * wsso = buildSearchOptions("en", "documental system"); find(sid,
		 * wsso);
		 * 
		 * wsso = buildSearchOptions("en", "electronic document system");
		 * find(sid, wsso);
		 * 
		 * wsso = buildSearchOptions("en",
		 * "electronic document management system"); find(sid, wsso);
		 * 
		 * wsso = buildSearchOptions("en",
		 * "electronic system for document management"); find(sid, wsso);
		 */

		// logoutDelete(sid);

		// long end_time = System.nanoTime();
		// double difference = (end_time - start_time) / 1e6;
		// System.out.println("Total Exec. time (ms): " +difference);

		// HttpRestWb - (Apache HttpClient 4.5.2)
		// Total Exec. time (ms): 988.231266
		// Total Exec. time (ms): 681.909198
		// Total Exec. time (ms): 737.044408
		// Total Exec. time (ms): 705.149953
		// Total Exec. time (ms): 739.934107
		// Total Exec. time (ms): 767.015352

		// HttpRestWb - With deserialization in Java object and serialization to
		// JSON (to write string on system out)
		// Total Exec. time (ms): 1114.652089
		// Total Exec. time (ms): 951.179299
		// Total Exec. time (ms): 1080.464628
		// Total Exec. time (ms): 973.297579
		// Total Exec. time (ms): 1062.980412
		// Total Exec. time (ms): 1064.021243
		// Total Exec. time (ms): 1024.741121
		// Total Exec. time (ms): 1047.152382

		// HttpRestWb - With optimized object creation/reuse
		// Total Exec. time (ms): 994.939062
		// Total Exec. time (ms): 913.421004
		// Total Exec. time (ms): 1189.928415

		// XtestRestClients - (CXF JAXRSClientFactory with transform
		// JSON-2-Java)
		// Total Exec. time (ms): 1284.876061
		// Total Exec. time (ms): 1471.814473
		// Total Exec. time (ms): 1302.452651
		// Total Exec. time (ms): 1112.79949
		// Total Exec. time (ms): 1202.391816
		// Total Exec. time (ms): 1134.588864
		// Total Exec. time (ms): 968.517025
		// Total Exec. time (ms): 1268.148093
		// Total Exec. time (ms): 1279.542668
		// Total Exec. time (ms): 1160.630676
		// Total Exec. time (ms): 932.782546
		// Total Exec. time (ms): 990.117454
		// Total Exec. time (ms): 1023.894365
		// Total Exec. time (ms): 931.256054
		// Total Exec. time (ms): 1292.162823
		// Total Exec. time (ms): 870.193555
		// Total Exec. time (ms): 889.628808
		// Total Exec. time (ms): 918.054599
	}

	private static void ratingStuff() throws Exception {
		WSDocument doc = null;
		for (long id = 700; id < 10000; id++) {
			doc = docClient.getDocument(id);
			if (doc != null) {
				System.out.println("Found document " + id);
				break;
			}
		}

		List<WSRating> ratings = docClient.getRatings(doc.getId());
		if (ratings != null)
			System.out.println("Found " + ratings.size() + " ratings");

		WSRating rating = docClient.rateDocument(doc.getId(), 3);
		System.out.println("Created rating: " + rating.getUsername() + " - " + rating.getVote());

		ratings = docClient.getRatings(doc.getId());
		if (ratings != null)
			System.out.println("Found " + ratings.size() + " ratings");
	}

	private static void noteStuff() throws Exception {
		WSDocument doc = null;
		for (long id = 700; id < 10000; id++) {
			doc = docClient.getDocument(id);
			if (doc != null) {
				System.out.println("Found document " + id);
				break;
			}
		}

		WSNote note = docClient.addNote(doc.getId(), "Test note 1");
		System.out.println("Created note: " + note.getId() + " - " + note.getUsername() + " - " + note.getMessage());
		note = docClient.addNote(doc.getId(), "Test note 2");
		System.out.println("Created note: " + note.getId() + " - " + note.getUsername() + " - " + note.getMessage());
		List<WSNote> notes = docClient.getNotes(doc.getId());
		System.out.println("Found " + notes.size() + " notes");
		docClient.deleteNote(note.getId());
		System.out.println("Deleted note " + note.getId());
		notes = docClient.getNotes(doc.getId());
		System.out.println("Found " + notes.size() + " notes");
	}

	private static void bookmarkStuff() throws Exception {
		WSDocument doc = null;
		for (long id = 700; id < 10000; id++) {
			doc = docClient.getDocument(id);
			if (doc != null) {
				System.out.println("Found document " + id);
				break;
			}
		}

		List<WSBookmark> bookmarks = bookmarkClient.getBookmarks();
		if (bookmarks != null)
			System.out.println("Found " + bookmarks.size() + " bookmarks");

		WSBookmark bookmark = bookmarkClient.bookmarkDocument(doc.getId());
		System.out.println("Created bookmark: " + bookmark.getTitle() + " - " + bookmark.getFileType());

		bookmarks = bookmarkClient.getBookmarks();
		if (bookmarks != null)
			System.out.println("Found " + bookmarks.size() + " bookmarks");
	}

	private static void tagStuff() throws Exception {
		List<String> tags = tagClient.getTags();
		System.out.println("Found tags " + tags);

		for (String tag : tags) {
			List<WSDocument> docs = tagClient.findDocumentsByTag(tag);
			if (docs != null && docs.size() > 0) {
				System.out.println("Found " + docs.size() + " documents tagged with '" + tag + "'");
				for (WSDocument doc : docs)
					tagClient.addDocumentTags(doc.getId(), Arrays.asList("xyz"));
				break;
			}
		}

		for (String tag : tags) {
			List<WSFolder> folders = tagClient.findFoldersByTag(tag);
			if (folders != null && folders.size() > 0) {
				System.out.println("Found " + folders.size() + " folders tagged with '" + tag + "'");
				for (WSFolder folder : folders)
					tagClient.addFolderTags(folder.getId(), Arrays.asList("xyz"));
				break;
			}
		}
	}

	private static void folderStuff() throws Exception {
		WSFolder folder = fldClient.getRootFolder();
		System.out.println("Root: " + folder.getName());

		folder = fldClient.findByPath("/Default/Alle");
		System.out.println("Folder: " + folder.getName());
	}

	private static void checkin(long docId) throws Exception {

		try {
			docClient.checkout(docId);
		} catch (Exception e) {
			System.err.println("Captured exception: " + e.getMessage());
			// e.printStackTrace();
		}

		// verify document status
		WSDocument xxx = docClient.getDocument(docId);
		System.out.println("Doc status: " + xxx.getStatus());

		File packageFile = new File("C:/tmp/simply.pdf");
		docClient.checkin(docId, "comment", false, packageFile);
		System.out.println("Checkin completed");
	}

	private static void checkout(long docId) throws Exception {

		try {
			docClient.checkout(docId);
		} catch (Exception e) {
			// Captured exception
			System.err.println("Captured exception: " + e.getMessage());
			// e.printStackTrace();
		}

		// verify document status
		WSDocument xxx = docClient.getDocument(docId);
		System.out.println("Doc status: " + xxx.getStatus());
	}

	private static void getVersionContent(long docId, String version) throws Exception {

		DataHandler handler = docClient.getVersionContent(docId, version);

		InputStream is = handler.getInputStream();
		OutputStream os = new FileOutputStream(new File("C:/tmp/AD6uk07-" + version + ".pdf"));
		IOUtils.copy(is, os);
		IOUtils.closeQuietly(os);
		IOUtils.closeQuietly(is);
	}

	private static WSFolder createFolder() throws Exception {

		WSFolder fld = new WSFolder();
		fld.setParentId(04L);
		fld.setName("REST created fld");
		fld.setDescription("folder created by Java REST client (JSON post)");
		WSFolder cfld = fldClient.create(fld);
		System.out.println("Folder created with ID: " + cfld.getId());
		return cfld;
	}

	private static void updateFolder(WSFolder cfld, String newFolderName) throws Exception {
		cfld.setName(newFolderName);
		fldClient.update(cfld);
		System.out.println("Folder updated");
	}

	private static void updateDocument(long docId) throws Exception {

		System.out.println("docId: " + docId);

		DataHandler handler = docClient.getContent(docId);

		InputStream is = handler.getInputStream();
		OutputStream os = new FileOutputStream(new File("C:/tmp/myFile.raw"));
		IOUtils.copy(is, os);
		IOUtils.closeQuietly(os);
		IOUtils.closeQuietly(is);
	}

	private static void deleteFolder(long folderId) {
		try {
			fldClient.delete(folderId);
			System.out.println("Successfully deleted folder");
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private static void deleteDocument(long docId) {
		try {
			docClient.delete(docId);
			System.out.println("Successfully deleted document");
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private static void createFolder(long parentId, String fname) throws Exception {
		long fldId = fldClient.createFolder(parentId, fname);
		System.out.println("fldId: " + fldId);
	}

	private static void createDocument() throws Exception {
		File xxxx = new File("C:\\tmp\\test-split.pdf");
		WSDocument document = new WSDocument();
		document.setFolderId(04L);
		document.setFileName(xxxx.getName());
		docClient.create(document, xxxx);
	}

	private static void createDocument02() throws Exception {

		File xxxx = new File("C:\\tmp\\InvoiceProcessing02-dashboard.png");

		WSDocument document = new WSDocument();
		document.setFolderId(04L);
		document.setFileName(xxxx.getName());

		DataSource fds = new FileDataSource(xxxx);
		DataHandler handler = new DataHandler(fds);

		WSDocument created = docClient.create(document, handler);
		System.out.println(created.getId());
	}

	private static void updateDocument(WSDocument document) throws Exception {

		docClient.update(document);
	}

	private static WSDocument getDocument(long docId) throws Exception {

		WSDocument myDoc = docClient.getDocument(docId);

		// Object to JSON in String
		ObjectMapper mapper = new ObjectMapper();
		String jsonInString = mapper.writeValueAsString(myDoc);
		System.out.println(jsonInString);

		return myDoc;
	}

	private static void getFolder(long fldId) throws Exception {

		WSFolder sss = fldClient.getFolder(fldId);

		// Object to JSON in String
		ObjectMapper mapper = new ObjectMapper();
		String jsonInString = mapper.writeValueAsString(sss);
		System.out.println(jsonInString);
	}

	private static void createPath(long rootFolder, String path) throws Exception {

		WSFolder sss = fldClient.createPath(rootFolder, path);

		// Object to JSON in String
		ObjectMapper mapper = new ObjectMapper();
		String jsonInString = mapper.writeValueAsString(sss);
		System.out.println("sss: " + jsonInString);
	}

	private static void find(WSSearchOptions options) throws Exception {

		WSSearchResult res = searchClient.find(options);

		// ObjectWriter ow = new
		// ObjectMapper().writer().withDefaultPrettyPrinter();
		ObjectWriter ow = new ObjectMapper().writer();
		String jsonStr = ow.writeValueAsString(res);
		System.out.println(jsonStr);
	}
	

	private static void listPaginated(long folderId, String fileName, String sort, Integer page, Integer max)
			throws Exception {
		List<WSDocument> docs = docClient.listPaginated(folderId, fileName, sort, page, max);
		System.out.println("docs: " + docs);
		System.out.println("docs.length: " + docs.size());

		// Object to JSON in String
		ObjectMapper mapper = new ObjectMapper();
		if (docs.size() > 0) {
			String jsonInString = mapper.writeValueAsString(docs.get(0));
			System.out.println("doc[1]: " + jsonInString);
		}
	}

	private static void listDocuments(long folderId, String fileName) throws Exception {
		List<WSDocument> docs = docClient.listDocuments(folderId, fileName);
		System.out.println("docs: " + docs);

		if (docs.size() > 0) {
			// Object to JSON in String
			ObjectMapper mapper = new ObjectMapper();
			String jsonInString = mapper.writeValueAsString(docs.get(0));
			System.out.println("doc[1]: " + jsonInString);
		}
	}

	private static void listFolderChildren(long folderId) throws Exception {
		List<WSFolder> dirs = fldClient.listChildren(folderId);
		System.out.println("docs: " + dirs);
		System.out.println("docs.length: " + dirs.size());

		// Object to JSON in String
		if (CollectionUtils.isNotEmpty(dirs)) {
			ObjectMapper mapper = new ObjectMapper();
			String jsonInString = mapper.writeValueAsString(dirs.get(0));
			System.out.println("dirs[0]: " + jsonInString);
		}
	}

	private static String loginGet() throws Exception {
		String sid = authClient.login("admin", "admin");
		System.out.println("sid: " + sid);
		return sid;
	}

}