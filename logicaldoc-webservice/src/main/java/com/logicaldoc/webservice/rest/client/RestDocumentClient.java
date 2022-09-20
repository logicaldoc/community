package com.logicaldoc.webservice.rest.client;

import java.io.File;
import java.io.FileInputStream;
import java.util.Arrays;

import javax.activation.DataHandler;
import javax.ws.rs.core.MediaType;

import org.apache.cxf.jaxrs.client.JAXRSClientFactory;
import org.apache.cxf.jaxrs.client.WebClient;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.AttachmentBuilder;
import org.apache.cxf.jaxrs.ext.multipart.ContentDisposition;
import org.apache.cxf.transport.http.HTTPConduit;
import org.apache.cxf.transports.http.configuration.HTTPClientPolicy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.jaxrs.json.JacksonJsonProvider;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSNote;
import com.logicaldoc.webservice.model.WSRating;
import com.logicaldoc.webservice.rest.DocumentService;

public class RestDocumentClient extends AbstractRestClient {

	protected static Logger log = LoggerFactory.getLogger(RestDocumentClient.class);

	private DocumentService proxy = null;

	public RestDocumentClient(String endpoint, String username, String password) {
		this(endpoint, username, password, -1);
	}

	public RestDocumentClient(String endpoint, String username, String password, int timeout) {
		super(endpoint, username, password, timeout);

		JacksonJsonProvider provider = new JacksonJsonProvider();

		if ((username == null) || (password == null)) {
			proxy = JAXRSClientFactory.create(endpoint, DocumentService.class, Arrays.asList(provider));
		} else {
			proxy = JAXRSClientFactory.create(endpoint, DocumentService.class, Arrays.asList(provider), username,
					password, null);
		}

		if (timeout > 0) {
			HTTPConduit conduit = WebClient.getConfig(proxy).getHttpConduit();
			HTTPClientPolicy policy = new HTTPClientPolicy();
			policy.setReceiveTimeout(timeout);
			conduit.setClient(policy);
		}
	}

	public WSDocument create(WSDocument document, File packageFile) throws Exception {

		WebClient.client(proxy).type(MediaType.MULTIPART_FORM_DATA);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		
		Attachment fileAttachment = new Attachment("content", new FileInputStream(packageFile), new ContentDisposition("form-data; name=\"content\"; filename=\"" + packageFile.getName() + "\""));		
		
		return proxy.create(document, fileAttachment);
	}

	public WSDocument create(WSDocument document, DataHandler dataHandler) throws Exception {

		WebClient.client(proxy).type(MediaType.MULTIPART_FORM_DATA);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		
		Attachment fileAttachment = new AttachmentBuilder().id("content").dataHandler(dataHandler)
				.mediaType("application/octet-stream")
				.contentDisposition(new ContentDisposition("form-data; name=\"content\"")).build();
		
		return proxy.create(document, fileAttachment);		
	}

	public WSDocument[] list(long folderId) throws Exception {

		WebClient.client(proxy).type("*/*");

		return proxy.list(folderId);
	}

	public WSDocument[] listDocuments(long folderId, String fileName) throws Exception {
		WebClient.client(proxy).type("*/*");
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);

		return proxy.listDocuments(folderId, fileName);
	}

	public WSDocument getDocument(long docId) throws Exception {
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);

		return proxy.getDocument(docId);
	}

	public void delete(long docId) throws Exception {

		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);

		proxy.delete(docId);
	}

	public DataHandler getContent(long docId) throws Exception {
		WebClient.client(proxy).accept(MediaType.APPLICATION_OCTET_STREAM);
		return proxy.getContent(docId);
	}

	public DataHandler getVersionContent(long docId, String version) throws Exception {
		WebClient.client(proxy).accept(MediaType.APPLICATION_OCTET_STREAM);
		return proxy.getVersionContent(docId, version);
	}

	public void checkout(long docId) throws Exception {
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.checkout(docId);
	}

	public void update(WSDocument document) throws Exception {

		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);

		proxy.update(document);
	}

	public void checkin(long docId, String comment, Boolean release, File packageFile) throws Exception {
		
		Attachment fileAttachment = new Attachment("filedata", new FileInputStream(packageFile),
		new ContentDisposition("form-data; name=\"filedata\"; filename=\"" + packageFile.getName() + "\""));
		
		WebClient.client(proxy).type(MediaType.MULTIPART_FORM_DATA);
		WebClient.client(proxy).accept(MediaType.TEXT_PLAIN);
		
		proxy.checkin(Long.toString(docId), comment, release.toString(), packageFile.getName(), fileAttachment);
	}

	public WSNote addNote(long docId, String note) throws Exception {
		WebClient.client(proxy).type(MediaType.APPLICATION_FORM_URLENCODED);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.addNote(docId, note);
	}

	/**
	 * Adds a new note for the given document
	 * 
	 * @param noteId identifier of the note
	 * 
	 * @throws Exception error in the server application
	 */
	public void deleteNote(long noteId) throws Exception {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.deleteNote(noteId);
	}

	/**
	 * Adds a new note for the given document
	 * 
	 * @param docId identifier of the document
	 * @param version document's version
	 * 
	 * @throws Exception error in the server application
	 */
	public void deleteVersion(long docId, String version) throws Exception {
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.deleteVersion(docId, version);
	}

	/**
	 * Gets the notes for the given document
	 * 
	 * @param docId identifier of the document
	 * 
	 * @return array of ratings
	 * 
	 * @throws Exception error in the server application
	 */
	public WSNote[] getNotes(long docId) throws Exception {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.getNotes(docId);
	}

	/**
	 * Puts a new rating on the given document
	 * 
	 * @param docId identifier of the document
	 * @param vote the vote
	 * 
	 * @return the rating
	 * 
	 * @throws Exception error in the server application
	 */
	public WSRating rateDocument(long docId, int vote) throws Exception {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.rateDocument(docId, vote);
	}

	/**
	 * Gets all the ratings of the given document
	 * 
	 * @param docId identifier of the document
	 *
	 * @return array of ratings
	 * 
	 * @throws Exception error in the server application
	 */
	public WSRating[] getRatings(long docId) throws Exception {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.getRatings(docId);
	}

	public void createPdf(long docId, String fileVersion) throws Exception {
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.createPdf(docId, fileVersion);
	}

	public void createThumbnail(long docId, String fileVersion, String type) throws Exception {
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.createThumbnail(docId, fileVersion, type);
	}
}