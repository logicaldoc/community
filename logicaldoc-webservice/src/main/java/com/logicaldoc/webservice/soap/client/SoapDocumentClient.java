package com.logicaldoc.webservice.soap.client;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import javax.activation.DataHandler;
import javax.activation.FileDataSource;
import javax.jws.WebService;
import javax.mail.MessagingException;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.parser.ParseException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.authorization.UnexistingResourceException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSAccessControlEntry;
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

	public SoapDocumentClient(String endpoint, int gzipThreshold, boolean log, int timeout) {
		super(endpoint, DocumentService.class, gzipThreshold, log, timeout);
	}

	public SoapDocumentClient(String endpoint) {
		super(endpoint, DocumentService.class, -1, true, -1);
	}

	@Override
	public WSDocument create(String sid, WSDocument document, DataHandler content) throws AuthenticationException,
			PermissionException, IOException, WebserviceException, PersistenceException {
		return client.create(sid, document, content);
	}

	public WSDocument create(String sid, WSDocument document, File content) throws AuthenticationException,
			PermissionException, IOException, WebserviceException, PersistenceException {
		if (StringUtils.isEmpty(document.getFileName()))
			document.setFileName(content.getName());
		return create(sid, document, new DataHandler(new FileDataSource(content)));
	}

	@Override
	public void checkout(String sid, long docId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		client.checkout(sid, docId);
	}

	@Override
	public void delete(String sid, long docId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		client.delete(sid, docId);
	}

	@Override
	public WSDocument getDocument(String sid, long docId) throws AuthenticationException, PermissionException,
			WebserviceException, PersistenceException, UnexistingResourceException {
		return client.getDocument(sid, docId);
	}

	@Override
	public WSDocument getDocumentByCustomId(String sid, String customId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		return client.getDocumentByCustomId(sid, customId);
	}

	@Override
	public void lock(String sid, long docId) throws AuthenticationException, PermissionException, WebserviceException,
			PersistenceException, UnexistingResourceException {
		client.lock(sid, docId);
	}

	@Override
	public void move(String sid, long docId, long folderId) throws AuthenticationException, PermissionException,
			WebserviceException, PersistenceException, UnexistingResourceException {
		client.move(sid, docId, folderId);
	}

	@Override
	public WSDocument copy(String sid, long docId, long folderId, boolean links, boolean notes, boolean security)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			IOException {
		return client.copy(sid, docId, folderId, links, notes, security);
	}

	@Override
	public void unlock(String sid, long docId) throws AuthenticationException, PermissionException, WebserviceException,
			PersistenceException, UnexistingResourceException {
		client.unlock(sid, docId);
	}

	@Override
	public void update(String sid, WSDocument document) throws AuthenticationException, PermissionException,
			WebserviceException, PersistenceException, UnexistingResourceException {
		client.update(sid, document);
	}

	@Override
	public void checkin(String sid, long docId, String comment, String filename, boolean release, DataHandler content)
			throws AuthenticationException, PermissionException, IOException, WebserviceException,
			PersistenceException {
		client.checkin(sid, docId, comment, filename, release, content);
	}

	public void checkin(String sid, long docId, String comment, String filename, boolean release, File content)
			throws AuthenticationException, PermissionException, IOException, WebserviceException,
			PersistenceException {
		this.checkin(sid, docId, comment, filename, release, new DataHandler(new FileDataSource(content)));
	}

	@Override
	public void checkinDocument(String sid, long docId, String comment, String filename, boolean release,
			WSDocument docVO, DataHandler content) throws AuthenticationException, PermissionException, IOException,
			WebserviceException, PersistenceException {
		client.checkinDocument(sid, docId, comment, filename, release, docVO, content);
	}

	public void checkinDocument(String sid, long docId, String comment, String filename, boolean release,
			WSDocument docVO, File content) throws AuthenticationException, PermissionException, IOException,
			WebserviceException, PersistenceException {
		client.checkinDocument(sid, docId, comment, filename, release, docVO,
				new DataHandler(new FileDataSource(content)));
	}

	@Override
	public DataHandler getContent(String sid, long docId) throws AuthenticationException, PermissionException,
			WebserviceException, PersistenceException, IOException {
		return client.getContent(sid, docId);
	}

	@Override
	public DataHandler getVersionContent(String sid, long docId, String version) throws AuthenticationException,
			PermissionException, WebserviceException, PersistenceException, IOException {
		return client.getVersionContent(sid, docId, version);
	}

	@Override
	public DataHandler getResource(String sid, long docId, String fileVersion, String suffix)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			IOException {
		return client.getResource(sid, docId, fileVersion, suffix);
	}

	public void downloadContent(String sid, long docId, File out) throws AuthenticationException, PermissionException,
			WebserviceException, PersistenceException, IOException {
		DataHandler data = client.getContent(sid, docId);
		data.writeTo(new FileOutputStream(out));
	}

	public void downloadVersionContent(String sid, long docId, String version, File out) throws AuthenticationException,
			PermissionException, WebserviceException, PersistenceException, IOException {
		DataHandler data = client.getVersionContent(sid, docId, version);
		data.writeTo(new FileOutputStream(out));
	}

	public void downloadResourceContent(String sid, long docId, String fileVersion, String suffix, File out)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			IOException {
		DataHandler data = client.getResource(sid, docId, fileVersion, suffix);
		data.writeTo(new FileOutputStream(out));
	}

	@Override
	public void restore(String sid, long docId, long folderId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		client.restore(sid, docId, folderId);
	}

	@Override
	public void rename(String sid, long docId, String name) throws AuthenticationException, PermissionException,
			WebserviceException, PersistenceException, UnexistingResourceException {
		client.rename(sid, docId, name);
	}

	@Override
	public WSDocument[] getDocuments(String sid, Long[] docIds)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.getDocuments(sid, docIds);
	}

	@Override
	public WSDocument[] getRecentDocuments(String sid, Integer max)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.getRecentDocuments(sid, max);
	}

	@Override
	public void sendEmail(String sid, Long[] docIds, String recipients, String subject, String message)
			throws AuthenticationException, WebserviceException, PersistenceException, IOException, MessagingException {
		client.sendEmail(sid, docIds, recipients, subject, message);
	}

	@Override
	public WSDocument createAlias(String sid, long docId, long folderId, String type)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		return client.createAlias(sid, docId, folderId, type);
	}

	@Override
	public void reindex(String sid, long docId, String content)
			throws AuthenticationException, WebserviceException, PersistenceException, ParseException {
		client.reindex(sid, docId, content);
	}

	@Override
	public WSDocument[] listDocuments(String sid, long folderId, String fileName)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		return client.listDocuments(sid, folderId, fileName);
	}

	@Override
	public WSDocument[] getAliases(String sid, long docId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.getAliases(sid, docId);
	}

	@Override
	public WSLink link(String sid, long doc1, long doc2, String type) throws AuthenticationException,
			PermissionException, WebserviceException, PersistenceException, UnexistingResourceException {
		return client.link(sid, doc1, doc2, type);
	}

	@Override
	public WSLink[] getLinks(String sid, long docId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		return client.getLinks(sid, docId);
	}

	@Override
	public void deleteLink(String sid, long id)
			throws AuthenticationException, WebserviceException, PersistenceException {
		client.deleteLink(sid, id);
	}

	@Override
	public void createPdf(String sid, long docId, String fileVersion) throws AuthenticationException,
			PermissionException, WebserviceException, PersistenceException, IOException {
		client.createPdf(sid, docId, fileVersion);
	}

	@Override
	public void createThumbnail(String sid, long docId, String fileVersion, String type)
			throws AuthenticationException, WebserviceException, PersistenceException, IOException {
		client.createThumbnail(sid, docId, fileVersion, type);
	}

	@Override
	public void uploadResource(String sid, long docId, String fileVersion, String suffix, DataHandler content)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			IOException {
		client.uploadResource(sid, docId, fileVersion, suffix, content);
	}

	public void uploadResource(String sid, long docId, String fileVersion, String suffix, File content)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			IOException {
		uploadResource(sid, docId, fileVersion, suffix, new DataHandler(new FileDataSource(content)));
	}

	@Override
	public String getExtractedText(String sid, long docId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.getExtractedText(sid, docId);
	}

	@Override
	public String createDownloadTicket(String sid, long docId, String suffix, Integer expireHours, String expireDate,
			Integer maxDownloads)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		return client.createDownloadTicket(sid, docId, suffix, expireHours, expireDate, maxDownloads);
	}

	@Override
	public String createViewTicket(String sid, long docId, String suffix, Integer expireHours, String expireDate,
			Integer maxDownloads, Integer maxViews)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		return client.createViewTicket(sid, docId, suffix, expireHours, expireDate, maxDownloads, maxViews);
	}

	@Override
	public void setPassword(String sid, long docId, String password) throws AuthenticationException,
			PermissionException, WebserviceException, PersistenceException, UnexistingResourceException {
		client.setPassword(sid, docId, password);
	}

	@Override
	public void unsetPassword(String sid, long docId, String currentPassword) throws AuthenticationException,
			PermissionException, WebserviceException, PersistenceException, UnexistingResourceException {
		client.unsetPassword(sid, docId, currentPassword);
	}

	@Override
	public boolean unprotect(String sid, long docId, String password)
			throws PersistenceException, AuthenticationException, WebserviceException {
		return client.unprotect(sid, docId, password);
	}

	@Override
	public WSNote addNote(String sid, long docId, String note) throws AuthenticationException, PermissionException,
			WebserviceException, PersistenceException, UnexistingResourceException {
		return client.addNote(sid, docId, note);
	}

	@Override
	public void deleteNote(String sid, long noteId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		client.deleteNote(sid, noteId);
	}

	@Override
	public WSNote[] getNotes(String sid, long docId) throws AuthenticationException, PermissionException,
			WebserviceException, PersistenceException, UnexistingResourceException {
		return client.getNotes(sid, docId);
	}

	@Override
	public WSRating rateDocument(String sid, long docId, int vote) throws AuthenticationException, PermissionException,
			WebserviceException, PersistenceException, UnexistingResourceException {
		return client.rateDocument(sid, docId, vote);
	}

	@Override
	public WSRating[] getRatings(String sid, long docId) throws AuthenticationException, PermissionException,
			WebserviceException, PersistenceException, UnexistingResourceException {
		return client.getRatings(sid, docId);
	}

	@Override
	public String deleteVersion(String sid, long docId, String version)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.deleteVersion(sid, docId, version);
	}

	@Override
	public void replaceFile(String sid, long docId, String fileVersion, String comment, DataHandler content)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException, IOException,
			UnexistingResourceException {
		client.replaceFile(sid, docId, fileVersion, comment, content);
	}

	@Override
	public void promoteVersion(String sid, long docId, String version) throws AuthenticationException,
			PermissionException, WebserviceException, PersistenceException, IOException, UnexistingResourceException {
		client.promoteVersion(sid, docId, version);
	}

	@Override
	public WSNote saveNote(String sid, long docId, WSNote note) throws AuthenticationException, PermissionException,
			WebserviceException, PersistenceException, UnexistingResourceException {
		return client.saveNote(sid, docId, note);
	}

	@Override
	public long upload(String sid, Long docId, Long folderId, boolean release, String filename, String language,
			DataHandler content) throws AuthenticationException, PermissionException, WebserviceException,
			PersistenceException, IOException {
		return client.upload(sid, docId, folderId, release, filename, language, content);
	}

	@Override
	public WSDocument[] getVersions(String sid, long docId) throws AuthenticationException, PermissionException,
			WebserviceException, PersistenceException, UnexistingResourceException {
		return client.getVersions(sid, docId);
	}

	@Override
	public WSDocument getVersion(String sid, long docId, String version) throws AuthenticationException,
			PermissionException, WebserviceException, PersistenceException, UnexistingResourceException {
		return client.getVersion(sid, docId, version);
	}

	@Override
	public boolean isRead(String sid, long docId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.isRead(sid, docId);
	}

	@Override
	public boolean isWrite(String sid, long docId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.isWrite(sid, docId);
	}

	@Override
	public boolean isDownload(String sid, long docId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.isDownload(sid, docId);
	}

	@Override
	public boolean isGranted(String sid, long docId, String permission)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.isGranted(sid, docId, permission);
	}

	@Override
	public WSAccessControlEntry[] getAccessControlList(String sid, long docId)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		return client.getAccessControlList(sid, docId);
	}

	@Override
	public void setAccessControlList(String sid, long docId, WSAccessControlEntry[] acl)
			throws PersistenceException, PermissionException, AuthenticationException, WebserviceException {
		client.setAccessControlList(sid, docId, acl);
	}
}