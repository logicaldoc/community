package com.logicaldoc.webservice.rest.client;

import java.io.File;
import java.io.FileInputStream;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import javax.activation.DataHandler;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.apache.commons.lang3.StringUtils;
import org.apache.cxf.jaxrs.client.JAXRSClientFactory;
import org.apache.cxf.jaxrs.client.WebClient;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.AttachmentBuilder;
import org.apache.cxf.jaxrs.ext.multipart.ContentDisposition;
import org.apache.cxf.transport.http.HTTPConduit;
import org.apache.cxf.transports.http.configuration.HTTPClientPolicy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
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

		ObjectWriter ow = new ObjectMapper().writer();
		String jsonStr = ow.writeValueAsString(document);

		Attachment docAttachment = new AttachmentBuilder().id("document").object(jsonStr).mediaType("application/json")
				.contentDisposition(new ContentDisposition("form-data; name=\"document\"")).build();
		Attachment fileAttachment = new Attachment("content", new FileInputStream(packageFile), new ContentDisposition(
				"form-data; name=\"content\"; filename=\"" + packageFile.getName() + "\""));

		List<Attachment> atts = new LinkedList<Attachment>();
		atts.add(docAttachment);
		atts.add(fileAttachment);

		Response res = proxy.create(atts);
		WSDocument cdoc = res.readEntity(WSDocument.class);

		return cdoc;
	}

	public WSDocument create(WSDocument document, DataHandler dataHandler) throws Exception {

		WebClient.client(proxy).type(MediaType.MULTIPART_FORM_DATA);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);

		ObjectWriter ow = new ObjectMapper().writer();
		String jsonStr = ow.writeValueAsString(document);

		Attachment docAttachment = new AttachmentBuilder().id("document").object(jsonStr).mediaType("application/json")
				.contentDisposition(new ContentDisposition("form-data; name=\"document\"")).build();
		Attachment fileAttachment = new AttachmentBuilder().id("content").dataHandler(dataHandler)
				.mediaType("application/octet-stream")
				.contentDisposition(new ContentDisposition("form-data; name=\"content\"")).build();

		List<Attachment> atts = new LinkedList<Attachment>();
		atts.add(docAttachment);
		atts.add(fileAttachment);

		Response res = proxy.create(atts);
		WSDocument cdoc = res.readEntity(WSDocument.class);

		return cdoc;
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

	public String checkin(long docId, String comment, Boolean release, File packageFile) throws Exception {

		/*
		 * if ("docId".equals(params.get("name"))) { docId =
		 * Long.parseLong(att.getObject(String.class)); } else if
		 * ("comment".equals(params.get("name"))) { comment =
		 * att.getObject(String.class); } else if
		 * ("release".equals(params.get("name"))) { release =
		 * Boolean.parseBoolean(att.getObject(String.class)); } else if
		 * ("filename".equals(params.get("name"))) { filename =
		 * att.getObject(String.class); } else if
		 * ("filedata".equals(params.get("name"))) { datah =
		 * att.getDataHandler(); }
		 */

		WebClient.client(proxy).type(MediaType.MULTIPART_FORM_DATA);
		WebClient.client(proxy).accept(MediaType.TEXT_PLAIN);

		Attachment docAttachment = new AttachmentBuilder().id("docId").object(docId).mediaType("text/plain")
				.contentDisposition(new ContentDisposition("form-data; name=\"docId\"")).build();

		Attachment commentAttachment = new AttachmentBuilder().id("comment").object(comment).mediaType("text/plain")
				.contentDisposition(new ContentDisposition("form-data; name=\"comment\"")).build();

		Attachment releaseAttachment = new AttachmentBuilder().id("release").object(release.toString())
				.mediaType("text/plain").contentDisposition(new ContentDisposition("form-data; name=\"release\""))
				.build();

		Attachment filenameAttachment = new AttachmentBuilder().id("filename").object(packageFile.getName())
				.mediaType("text/plain").contentDisposition(new ContentDisposition("form-data; name=\"filename\""))
				.build();

		Attachment fileAttachment = new Attachment("filedata", new FileInputStream(packageFile),
				new ContentDisposition("form-data; name=\"filedata\"; filename=\"" + packageFile.getName() + "\""));

		List<Attachment> atts = new LinkedList<Attachment>();
		atts.add(docAttachment);

		if (StringUtils.isNotEmpty(comment)) {
			atts.add(commentAttachment);
		}
		if (release != null) {
			atts.add(releaseAttachment);
		}
		if (packageFile != null) {
			atts.add(filenameAttachment);
		}
		atts.add(fileAttachment);

		Response res = proxy.checkin(atts);
		return res.readEntity(String.class);
	}

	public WSNote addNote(long docId, String note) throws Exception {
		WebClient.client(proxy).type(MediaType.APPLICATION_FORM_URLENCODED);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.addNote(docId, note);
	}

	/**
	 * Adds a new note for the given document
	 */
	public void deleteNote(long noteId) throws Exception {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.deleteNote(noteId);
	}
	
	/**
	 * Adds a new note for the given document
	 */
	public void deleteVersion(long docId, String version) throws Exception {
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.deleteVersion(docId, version);
	}	

	/**
	 * Gets the notes for the given document
	 */
	public WSNote[] getNotes(long docId) throws Exception {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.getNotes(docId);
	}

	/**
	 * Puts a new rating on the given document
	 */
	public WSRating rateDocument(long docId, int vote) throws Exception {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.rateDocument(docId, vote);
	}

	/**
	 * Gets all the ratings of the given document
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
	
	public void createThumbnail(long docId, String fileVersion) throws Exception {
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.createThumbnail(docId, fileVersion);
	}
}