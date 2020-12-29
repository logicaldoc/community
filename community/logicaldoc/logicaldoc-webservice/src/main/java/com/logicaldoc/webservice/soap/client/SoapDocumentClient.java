package com.logicaldoc.webservice.soap.client;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import javax.activation.DataHandler;
import javax.activation.FileDataSource;
import javax.jws.WebService;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSLink;
import com.logicaldoc.webservice.model.WSNote;
import com.logicaldoc.webservice.model.WSRating;
import com.logicaldoc.webservice.soap.DocumentService;

/**
 * Document Web Service client.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
@WebService(name = "Document", serviceName = "Document")
public class SoapDocumentClient extends SoapClient<DocumentService> implements DocumentService {

	public SoapDocumentClient(String endpoint, int gzipThreshold, boolean log, int timeout) throws IOException {
		super(endpoint, DocumentService.class, gzipThreshold, log, timeout);
	}

	public SoapDocumentClient(String endpoint) throws IOException {
		super(endpoint, DocumentService.class, -1, true, -1);
	}

	@Override
	public WSDocument create(String sid, WSDocument document, DataHandler content) throws Exception {
		return client.create(sid, document, content);
	}

	public WSDocument create(String sid, WSDocument document, File content) throws Exception {
		if (StringUtils.isEmpty(document.getFileName()))
			document.setFileName(content.getName());
		return create(sid, document, new DataHandler(new FileDataSource(content)));
	}

	@Override
	public void checkout(String sid, long docId) throws Exception {
		client.checkout(sid, docId);
	}

	@Override
	public void delete(String sid, long docId) throws Exception {
		client.delete(sid, docId);
	}

	@Override
	public WSDocument getDocument(String sid, long docId) throws Exception {
		return client.getDocument(sid, docId);
	}

	@Override
	public WSDocument getDocumentByCustomId(String sid, String customId) throws Exception {
		return client.getDocumentByCustomId(sid, customId);
	}

	@Override
	public boolean isReadable(String sid, long docId) throws Exception {
		return client.isReadable(sid, docId);
	}

	@Override
	public void lock(String sid, long docId) throws Exception {
		client.lock(sid, docId);
	}

	@Override
	public void move(String sid, long docId, long folderId) throws Exception {
		client.move(sid, docId, folderId);
	}

	@Override
	public void unlock(String sid, long docId) throws Exception {
		client.unlock(sid, docId);
	}

	@Override
	public void update(String sid, WSDocument document) throws Exception {
		client.update(sid, document);
	}

	@Override
	public void checkin(String sid, long docId, String comment, String filename, boolean release, DataHandler content)
			throws Exception {
		client.checkin(sid, docId, comment, filename, release, content);
	}

	public void checkin(String sid, long docId, String comment, String filename, boolean release, File content)
			throws Exception {
		this.checkin(sid, docId, comment, filename, release, new DataHandler(new FileDataSource(content)));
	}

	@Override
	public void checkinDocument(String sid, long docId, String comment, String filename, boolean release,
			WSDocument docVO, DataHandler content) throws Exception {
		client.checkinDocument(sid, docId, comment, filename, release, docVO, content);
	}

	public void checkinDocument(String sid, long docId, String comment, String filename, boolean release,
			WSDocument docVO, File content) throws Exception {
		client.checkinDocument(sid, docId, comment, filename, release, docVO,
				new DataHandler(new FileDataSource(content)));
	}

	@Override
	public DataHandler getContent(String sid, long docId) throws Exception {
		return client.getContent(sid, docId);
	}

	@Override
	public DataHandler getVersionContent(String sid, long docId, String version) throws Exception {
		return client.getVersionContent(sid, docId, version);
	}

	@Override
	public DataHandler getResource(String sid, long docId, String fileVersion, String suffix) throws Exception {
		return client.getResource(sid, docId, fileVersion, suffix);
	}
	
	public void downloadContent(String sid, long docId, File out) throws Exception {
		DataHandler data = client.getContent(sid, docId);
		data.writeTo(new FileOutputStream(out));
	}
	
	public void downloadVersionContent(String sid, long docId, String version, File out) throws Exception {
		DataHandler data = client.getVersionContent(sid, docId, version);
		data.writeTo(new FileOutputStream(out));
	}

	public void downloadResourceContent(String sid, long docId, String fileVersion, String suffix, File out)
			throws Exception {
		DataHandler data = client.getResource(sid, docId, fileVersion, suffix);
		data.writeTo(new FileOutputStream(out));
	}

	@Override
	public WSDocument[] getVersions(String sid, long docId) throws Exception {
		return client.getVersions(sid, docId);
	}

	@Override
	public void restore(String sid, long docId, long folderId) throws Exception {
		client.restore(sid, docId, folderId);
	}

	@Override
	public void rename(String sid, long docId, String name) throws Exception {
		client.rename(sid, docId, name);
	}

	@Override
	public WSDocument[] getDocuments(String sid, Long[] docIds) throws Exception {
		return client.getDocuments(sid, docIds);
	}

	@Override
	public WSDocument[] getRecentDocuments(String sid, Integer max) throws Exception {
		return client.getRecentDocuments(sid, max);
	}

	@Override
	public void sendEmail(String sid, Long[] docIds, String recipients, String subject, String message)
			throws Exception {
		client.sendEmail(sid, docIds, recipients, subject, message);
	}

	@Override
	public WSDocument createAlias(String sid, long docId, long folderId, String type) throws Exception {
		return client.createAlias(sid, docId, folderId, type);
	}

	@Override
	public void reindex(String sid, long docId, String content) throws Exception {
		client.reindex(sid, docId, content);
	}

	@Override
	public WSDocument[] listDocuments(String sid, long folderId, String fileName) throws Exception {
		return client.listDocuments(sid, folderId, fileName);
	}

	@Override
	public WSDocument[] getAliases(String sid, long docId) throws Exception {
		return client.getAliases(sid, docId);
	}

	@Override
	public WSLink link(String sid, long doc1, long doc2, String type) throws Exception {
		return client.link(sid, doc1, doc2, type);
	}

	@Override
	public WSLink[] getLinks(String sid, long docId) throws Exception {
		return client.getLinks(sid, docId);
	}

	@Override
	public void deleteLink(String sid, long id) throws Exception {
		client.deleteLink(sid, id);
	}

	@Override
	public void createPdf(String sid, long docId, String fileVersion) throws Exception {
		client.createPdf(sid, docId, fileVersion);
	}

	@Override
	public void createThumbnail(String sid, long docId, String fileVersion, String type) throws Exception {
		client.createThumbnail(sid, docId, fileVersion, type);
	}

	@Override
	public void uploadResource(String sid, long docId, String fileVersion, String suffix, DataHandler content)
			throws Exception {
		client.uploadResource(sid, docId, fileVersion, suffix, content);
	}

	public void uploadResource(String sid, long docId, String fileVersion, String suffix, File content)
			throws Exception {
		uploadResource(sid, docId, fileVersion, suffix, new DataHandler(new FileDataSource(content)));
	}

	@Override
	public String getExtractedText(String sid, long docId) throws Exception {
		return client.getExtractedText(sid, docId);
	}

	@Override
	public String createDownloadTicket(String sid, long docId, String suffix, Integer expireHours, String expireDate,
			Integer maxDownloads) throws Exception {
		return client.createDownloadTicket(sid, docId, suffix, expireHours, expireDate, maxDownloads);
	}

	@Override
	public void setPassword(String sid, long docId, String password) throws Exception {
		client.setPassword(sid, docId, password);
	}

	@Override
	public void unsetPassword(String sid, long docId, String currentPassword) throws Exception {
		client.unsetPassword(sid, docId, currentPassword);
	}

	@Override
	public boolean unprotect(String sid, long docId, String password) throws Exception {
		return client.unprotect(sid, docId, password);
	}

	@Override
	public WSNote addNote(String sid, long docId, String note) throws Exception {
		return client.addNote(sid, docId, note);
	}

	@Override
	public void deleteNote(String sid, long noteId) throws Exception {
		client.deleteNote(sid, noteId);
	}

	@Override
	public WSNote[] getNotes(String sid, long docId) throws Exception {
		return client.getNotes(sid, docId);
	}

	@Override
	public WSRating rateDocument(String sid, long docId, int vote) throws Exception {
		return client.rateDocument(sid, docId, vote);
	}

	@Override
	public WSRating[] getRatings(String sid, long docId) throws Exception {
		return client.getRatings(sid, docId);
	}

	@Override
	public String deleteVersion(String sid, long docId, String version) throws Exception {
		return client.deleteVersion(sid, docId, version);
	}

	@Override
	public void replaceFile(String sid, long docId, String fileVersion, String comment, DataHandler content)
			throws Exception {
		client.replaceFile(sid, docId, fileVersion, comment, content);
	}

	@Override
	public void promoteVersion(String sid, long docId, String version) throws Exception {
		client.promoteVersion(sid, docId, version);
	}

	@Override
	public WSNote saveNote(String sid, long docId, WSNote note) throws Exception {
		return client.saveNote(sid, docId, note);
	}

	@Override
	public long upload(String sid, Long docId, Long folderId, boolean release, String filename, String language,
			DataHandler content) throws Exception {
		return client.upload(sid, docId, folderId, release, filename, language, content);
	}
}