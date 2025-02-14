package com.logicaldoc.webservice.rest.client;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;

import javax.activation.DataHandler;
import javax.ws.rs.core.MediaType;

import org.apache.cxf.jaxrs.client.WebClient;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.AttachmentBuilder;
import org.apache.cxf.jaxrs.ext.multipart.ContentDisposition;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.authorization.UnexistingResourceException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSNote;
import com.logicaldoc.webservice.model.WSRating;
import com.logicaldoc.webservice.rest.DocumentService;

public class RestDocumentClient extends AbstractRestClient<DocumentService> {

	private static final Logger log = LoggerFactory.getLogger(RestDocumentClient.class);

	public RestDocumentClient(String endpoint, String apiKey) {
		this(endpoint, apiKey, -1);
	}

	public RestDocumentClient(String endpoint, String apiKey, int timeout) {
		super(DocumentService.class, endpoint, apiKey, timeout);
	}

	public WSDocument create(WSDocument document, File packageFile) throws FileNotFoundException {

		WebClient.client(proxy).type(MediaType.MULTIPART_FORM_DATA);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);

		Attachment fileAttachment = new Attachment("content", new FileInputStream(packageFile),
				new ContentDisposition("form-data; name=\"content\"; filename=\"" + packageFile.getName() + "\""));

		return proxy.create(document, fileAttachment);
	}

	public WSDocument create(WSDocument document, DataHandler dataHandler) {
		WebClient.client(proxy).type(MediaType.MULTIPART_FORM_DATA);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);

		Attachment fileAttachment = new AttachmentBuilder().id("content").dataHandler(dataHandler)
				.mediaType("application/octet-stream")
				.contentDisposition(new ContentDisposition("form-data; name=\"content\"")).build();

		return proxy.create(document, fileAttachment);
	}

	public List<WSDocument> list(long folderId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).type("*/*");
		return proxy.list(folderId);
	}

	public List<WSDocument> listPaginated(long folderId, String fileName, String sort, Integer page, Integer max)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).type("*/*");
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);

		return proxy.listPaginated(folderId, fileName, sort, page, max);
	}

	public List<WSDocument> listDocuments(long folderId, String fileName)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).type("*/*");
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);

		return proxy.listDocuments(folderId, fileName);
	}

	public WSDocument getDocument(long docId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.getDocument(docId);
	}

	public void delete(long docId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.delete(docId);
	}

	public DataHandler getContent(long docId) throws AuthenticationException, PermissionException, WebserviceException,
			PersistenceException, IOException {
		WebClient.client(proxy).accept(MediaType.APPLICATION_OCTET_STREAM);
		return proxy.getContent(docId);
	}

	public DataHandler getVersionContent(long docId, String version) throws AuthenticationException,
			PermissionException, WebserviceException, PersistenceException, IOException {
		WebClient.client(proxy).accept(MediaType.APPLICATION_OCTET_STREAM);
		return proxy.getVersionContent(docId, version);
	}

	public void checkout(long docId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.checkout(docId);
	}

	public void update(WSDocument document) throws AuthenticationException, PermissionException, WebserviceException,
			PersistenceException, UnexistingResourceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.update(document);
	}

	public void checkin(long docId, String comment, Boolean release, File packageFile) throws FileNotFoundException {
		Attachment fileAttachment = new Attachment("filedata", new FileInputStream(packageFile),
				new ContentDisposition("form-data; name=\"filedata\"; filename=\"" + packageFile.getName() + "\""));

		WebClient.client(proxy).type(MediaType.MULTIPART_FORM_DATA);
		WebClient.client(proxy).accept(MediaType.TEXT_PLAIN);

		proxy.checkin(Long.toString(docId), comment, release.toString(), packageFile.getName(), fileAttachment);
	}

	public WSNote addNote(long docId, String note) throws AuthenticationException, PermissionException,
			WebserviceException, PersistenceException, UnexistingResourceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_FORM_URLENCODED);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.addNote(docId, note);
	}

	/**
	 * Adds a new note for the given document
	 * 
	 * @param noteId identifier of the note
	 * 
	 * @throws PersistenceException Error in the data layer
	 * @throws WebserviceException Error in the Webservice layer
	 * @throws AuthenticationException Authentication issue
	 */
	public void deleteNote(long noteId) throws AuthenticationException, WebserviceException, PersistenceException {
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
	 * @throws PersistenceException Error in the data layer
	 * @throws WebserviceException Error in the Webservice layer
	 * @throws AuthenticationException Authentication issue
	 */
	public void deleteVersion(long docId, String version)
			throws AuthenticationException, WebserviceException, PersistenceException {
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.deleteVersion(docId, version);
	}

	/**
	 * Gets the notes for the given document
	 * 
	 * @param docId identifier of the document
	 * 
	 * @return list of ratings
	 * 
	 * @throws PersistenceException Error in the data layer
	 * @throws WebserviceException Error in the Webservice layer
	 * @throws AuthenticationException Authentication issue
	 * @throws PermissionException Not enough permissions
	 * @throws UnexistingResourceException The specified document does not exist
	 */
	public List<WSNote> getNotes(long docId) throws AuthenticationException, PermissionException, WebserviceException,
			PersistenceException, UnexistingResourceException {
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
	 * @throws PersistenceException Error in the data layer
	 * @throws WebserviceException Error in the Webservice layer
	 * @throws AuthenticationException Authentication issue
	 * @throws PermissionException Not enough permissions
	 * @throws UnexistingResourceException The specified document does not exist
	 */
	public WSRating rateDocument(long docId, int vote) throws AuthenticationException, PermissionException,
			WebserviceException, PersistenceException, UnexistingResourceException {
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
	 * @throws PersistenceException Error in the data layer
	 * @throws WebserviceException Error in the Webservice layer
	 * @throws AuthenticationException Authentication issue
	 * @throws PermissionException Not enough permissions
	 * @throws UnexistingResourceException The specified document does not exist
	 */
	public List<WSRating> getRatings(long docId) throws AuthenticationException, PermissionException,
			WebserviceException, PersistenceException, UnexistingResourceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.getRatings(docId);
	}

	public void createPdf(long docId, String fileVersion) throws AuthenticationException, PermissionException,
			WebserviceException, PersistenceException, IOException {
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.createPdf(docId, fileVersion);
	}

	public void createThumbnail(long docId, String fileVersion, String type)
			throws AuthenticationException, WebserviceException, PersistenceException, IOException {
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.createThumbnail(docId, fileVersion, type);
	}
}