package com.logicaldoc.core.transfer;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.zip.ZipEntry;

import org.apache.commons.compress.archivers.zip.ZipArchiveEntry;
import org.apache.commons.compress.archivers.zip.ZipArchiveOutputStream;
import org.apache.commons.compress.archivers.zip.ZipArchiveOutputStream.UnicodeExtraFieldPolicy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.conversion.FormatConversionManager;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.folder.FolderEvent;
import com.logicaldoc.core.folder.FolderHistory;
import com.logicaldoc.core.store.Store;
import com.logicaldoc.core.store.StoreResource;
import com.logicaldoc.util.io.FileUtil;

/**
 * Exports a folder hierarchy and all documents in it as a zip file. Can also be
 * used to export a selection of documents
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @author Matteo Caruso - LogicalDOC
 */
public class ZipExport {

	private static final Logger log = LoggerFactory.getLogger(ZipExport.class);

	private ZipArchiveOutputStream zos;

	private long userId;

	private boolean allLevel;

	private long startFolderId;

	public ZipExport() {
		zos = null;
		userId = -1;
		allLevel = false;
		startFolderId = Folder.DEFAULTWORKSPACEID;
	}

	/**
	 * Exports the specified folder content
	 * 
	 * @param transaction Transaction with all informations about the export
	 * @param pdfConversion True if the pdf conversion has to be used instead of
	 *        the original files
	 * 
	 * @return The Stream of the zip archive
	 * @throws PersistenceException error at database level
	 */
	public ByteArrayOutputStream process(FolderHistory transaction, boolean pdfConversion) throws PersistenceException {
		FolderDAO folderDao = FolderDAO.get();
		Folder folder = folderDao.findFolder(transaction.getFolderId());
		this.userId = transaction.getUserId();
		this.startFolderId = folder.getId();
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		zos = new ZipArchiveOutputStream(bos);
		zos.setMethod(ZipEntry.DEFLATED);
		zos.setEncoding("UTF-8");
		zos.setCreateUnicodeExtraFields(UnicodeExtraFieldPolicy.ALWAYS);
		zos.setUseLanguageEncodingFlag(true);

		try {
			appendChildren(folder, 0, pdfConversion, transaction.getSessionId());
		} finally {
			try {
				zos.flush();
				zos.close();
			} catch (Exception e) {
				// Nothing to do
			}
		}

		/*
		 * Record the export event
		 */
		transaction.setEvent(FolderEvent.EXPORTED);
		folderDao.saveFolderHistory(folder, transaction);

		return bos;
	}

	/**
	 * Exports a selection of documents
	 * 
	 * @param docIds Identifiers of the documents
	 * @param pdfConversion True if the pdf conversion has to be used instead of
	 *        the original files
	 * @param transaction session informations
	 * 
	 * @return The Stream of the zip archive
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public ByteArrayOutputStream process(Long[] docIds, boolean pdfConversion, DocumentHistory transaction)
			throws PersistenceException {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		process(docIds, bos, pdfConversion, transaction);
		return bos;
	}

	/**
	 * Exports a selection of documents
	 * 
	 * @param docIds Identifiers of the documents
	 * @param out The stream that will receive the zip
	 * @param pdfConversion True if the pdf conversion has to be used instead of
	 *        the original files
	 * @param transaction session informations
	 * @throws PersistenceException error at database level
	 */
	public void process(Long[] docIds, OutputStream out, boolean pdfConversion, DocumentHistory transaction)
			throws PersistenceException {
		DocumentDAO ddao = DocumentDAO.get();
		FolderDAO fdao = FolderDAO.get();

		zos = new ZipArchiveOutputStream(out);
		zos.setEncoding("UTF-8");
		zos.setMethod(ZipEntry.DEFLATED);
		zos.setCreateUnicodeExtraFields(UnicodeExtraFieldPolicy.ALWAYS);
		zos.setUseLanguageEncodingFlag(true);

		try {
			for (long id : docIds) {
				Document doc = ddao.findById(id);

				// Check if the current user has the download permission in the
				// document's folder
				if (transaction != null && transaction.getUserId() != 0L
						&& !fdao.isDownloadllowed(doc.getFolder().getId(), transaction.getUserId()))
					continue;

				boolean convertToPdf = pdfConversion;
				if (doc.getDocRef() != null) {
					// This is an alias, retrieve the real document
					doc = ddao.findById(doc.getDocRef());
					if ("pdf".equals(doc.getDocRefType()))
						convertToPdf = true;
				}
				addDocument("", doc, convertToPdf);

				try {
					zos.flush();
				} catch (Exception e) {
					log.warn(e.getMessage());
				}
				saveHistory(transaction, doc);
			}
		} finally {
			try {
				zos.flush();
				zos.close();
			} catch (Exception e) {
				log.warn(e.getMessage());
			}
		}
	}

	private void saveHistory(DocumentHistory transaction, Document doc) {
		DocumentDAO ddao = DocumentDAO.get();
		if (transaction != null) {
			DocumentHistory t = new DocumentHistory(transaction);
			transaction.setEvent(DocumentEvent.DOWNLOADED);
			try {
				ddao.saveDocumentHistory(doc, t);
			} catch (PersistenceException e) {
				log.warn(e.getMessage(), e);
			}
		}
	}

	/**
	 * If allLevel set true all children of a specified folder will be export.
	 * Otherwise only the first level will be export.
	 * 
	 * @param level the compression level
	 */
	public void setAllLevel(boolean level) {
		allLevel = level;
	}

	/**
	 * Adds all children of the specified folder up to the given level
	 * 
	 * @param folder the folder to elaborate
	 * @param depth the depth
	 * @param pdfConversion if the PDF conversions have to be used instead
	 * @param sid identifier of the session
	 * 
	 * @throws PersistenceException error at data layer
	 */
	protected void appendChildren(Folder folder, int depth, boolean pdfConversion, String sid)
			throws PersistenceException {
		if (allLevel || depth < 1) {
			addFolderDocuments(folder, pdfConversion, sid);
			FolderDAO folderDao = FolderDAO.get();
			Collection<Folder> children = folderDao.findByUserId(userId, folder.getId());
			Iterator<Folder> iter = children.iterator();

			while (iter.hasNext())
				appendChildren(iter.next(), depth + 1, pdfConversion, sid);
		}
	}

	/**
	 * Adds all folder's documents
	 * 
	 * @param folder the folder to elaborate
	 * @param pdfConversion if the PDF conversions have to be used instead
	 * @param sid identifier of the session
	 * 
	 * @throws PersistenceException error in the databaes
	 */
	protected void addFolderDocuments(Folder folder, boolean pdfConversion, String sid) throws PersistenceException {
		DocumentDAO ddao = DocumentDAO.get();
		Collection<Document> docs = ddao.findByFolder(folder.getId(), null);

		for (Document document : docs) {
			Document doc = document;
			boolean convertToPdf = pdfConversion;
			if (doc.getDocRef() != null) {
				// This is an alias, retrieve the real document
				doc = ddao.findById(doc.getDocRef());
				if ("pdf".equals(doc.getDocRefType()))
					convertToPdf = true;
			}

			addDocument(getZipEntryPath(folder), doc, convertToPdf);
		}
	}

	/**
	 * To maximize the compatibility with Windows we have to remove special
	 * chars and the total filename size must be <=250
	 * 
	 * @param src the source filename
	 * @return the compatible name
	 */
	private String adjustFileNameForWindows(String src) {
		String newName = src.replace("/", "").replace("\\", "");
		if (newName.length() > 250) {
			String ext = FileUtil.getExtension(newName);
			int maxSize = 250 - ext.length() - 1;
			String name = FileUtil.getBaseName(newName).substring(0, maxSize);
			newName = name + "." + ext;
		}
		return newName;
	}

	/**
	 * Adds a single document into the archive in the specified path.
	 * 
	 * @param path path to store the document in
	 * @param document the document
	 * @param pdfConversion if the PDF conversion has to be used instead
	 * @throws PersistenceException Error in the data layer
	 */
	private void addDocument(String path, Document document, boolean pdfConversion) throws PersistenceException {
		StoreResource resource = new StoreResource.Builder().document(document).build();

		if (pdfConversion && !"pdf".equals(FileUtil.getExtension(document.getFileName().toLowerCase()))) {
			try {
				FormatConversionManager.get().convertToPdf(document, null);
			} catch (IOException | PersistenceException e) {
				log.warn(e.getMessage(), e);
				return;
			}

			resource = new StoreResource.Builder().document(document)
					.suffix(FormatConversionManager.PDF_CONVERSION_SUFFIX).build();
		}

		try (BufferedInputStream bis = new BufferedInputStream(Store.get().getStream(resource))) {
			String fileName = document.getFileName();
			if (pdfConversion)
				fileName = FileUtil.getBaseName(fileName) + ".pdf";

			ZipEntry entry = new ZipEntry(path + adjustFileNameForWindows(fileName));
			entry.setMethod(ZipEntry.DEFLATED);
			zos.putArchiveEntry(new ZipArchiveEntry(entry));

			// Transfer bytes from the file to the ZIP file
			int len;
			while ((len = bis.read()) != -1) {
				zos.write(len);
			}
		} catch (IOException e) {
			log.error(e.getMessage());
		} finally {
			try {
				zos.closeArchiveEntry();
			} catch (IOException e) {
				log.warn(e.getMessage());
			}
		}
	}

	/**
	 * Computes the correct entry path inside the zip archive
	 * 
	 * @param folder The folder of the document to be inserted
	 * @return The full path
	 * 
	 * @throws PersistenceException Error in the database
	 */
	private String getZipEntryPath(Folder folder) throws PersistenceException {
		FolderDAO folderDao = FolderDAO.get();

		long rootId = folderDao.findRoot(folder.getTenantId()).getId();
		if (folder.getId() == rootId)
			return "";

		List<Folder> folders = folderDao.findParents(folder.getId());
		folders.add(folder);
		Collections.reverse(folders);

		List<String> folderNames = new ArrayList<>();
		for (int i = 0; i < folders.size(); i++) {
			Folder f = folders.get(i);
			if (f.getId() == startFolderId)
				break;
			folderNames.add(f.getName());
		}
		Collections.reverse(folderNames);

		StringBuilder path = new StringBuilder("");
		for (String name : folderNames) {
			path.append(name);
			path.append("/");
		}

		return path.toString();
	}
}