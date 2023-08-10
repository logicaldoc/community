package com.logicaldoc.core.store;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.StringUtil;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;

/**
 * Common methods for all the Storer implementations.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public abstract class AbstractStorer implements Storer {
	protected static final int DEFAULT_BUFFER_SIZE = 1024;

	protected static Logger log = LoggerFactory.getLogger(AbstractStorer.class);

	protected ContextProperties config;

	protected StorerManager manager;

	protected int id = 1;

	protected Map<String, String> parameters = new HashMap<>();

	protected AbstractStorer() {
	}

	public ContextProperties getConfig() {
		if (config == null)
			config = Context.get().getProperties();
		return config;
	}

	public void setManager(StorerManager manager) {
		this.manager = manager;
	}

	public void setConfig(ContextProperties config) {
		this.config = config;
	}

	public int getId() {
		return id;
	}

	public void setId(int id) {
		this.id = id;
	}

	@Override
	public int compareTo(Storer o) {
		return Integer.valueOf(id).compareTo(o.getId());
	}

	@Override
	public int hashCode() {
		return Integer.valueOf(id).hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == null)
			return false;

		if (this.getClass() != obj.getClass())
			return false;

		return id == ((Storer) obj).getId();
	}

	@Override
	public void store(File file, long docId, String resource) throws IOException {
		if (!isEnabled()) {
			log.warn("Storer not enabled");
			throw new IOException("Storer not enabled");
		}

		InputStream is = new BufferedInputStream(new FileInputStream(file), DEFAULT_BUFFER_SIZE);

		store(is, docId, resource);
	}

	/**
	 * Computes the relative path of a document's folder inside the storage
	 * root. The document's id is tokenized by three chars tokens, than the doc/
	 * dir is appended, so if the docId=12345, the document's path will be:
	 * 123/45/doc.
	 */
	protected String computeRelativePath(long docId) {
		return StringUtil.split(Long.toString(docId), '/', 3) + "/doc";
	}

	/**
	 * Computes the relative path of a document's resource inside the storage
	 * root. The document's id is tokenized by three chars tokens, than the doc/
	 * dir is appended, so if the docId=12345, the document's path will be:
	 * 123/45/doc/1.0.
	 */
	protected String computeRelativePath(long docId, String resource) {
		StringBuilder tmp = new StringBuilder(computeRelativePath(docId));
		if (StringUtils.isNotEmpty(resource)) {
			if (!tmp.toString().endsWith("/"))
				tmp.append("/");
			tmp.append(resource.startsWith("/") ? resource.substring(1) : resource);
		} else if (!tmp.toString().endsWith("/"))
			tmp.append("/");

		return tmp.toString();
	}

	@Override
	public byte[] getBytes(long docId, String resource) throws IOException {
		try (InputStream is = getStream(docId, resource);) {
			return IOUtils.toByteArray(is);
		}
	}

	@Override
	public void writeToStream(long docId, String resource, OutputStream output, long start, long length)
			throws IOException {
		try {
			IOUtils.copyLarge(getStream(docId, resource), output, start, length);
		} catch (IOException ioe) {
			throw ioe;
		}
	}

	@Override
	public void writeToStream(long docId, String resource, OutputStream output) throws IOException {
		try {
			IOUtils.copyLarge(getStream(docId, resource), output);
		} catch (IOException ioe) {
			throw ioe;
		}
	}

	@Override
	public void writeToFile(long docId, String resource, File out) throws IOException {

		try (OutputStream os = new BufferedOutputStream(new FileOutputStream(out, false), DEFAULT_BUFFER_SIZE);
				InputStream is = getStream(docId, resource);) {
			FileUtil.writeFile(is, out.getPath());
		} catch (IOException ioe) {
			throw ioe;
		} catch (Exception e) {
			log.error("Error writing document {} into {}", docId, out.getPath());
			log.error(e.getMessage(), e);
		}
	}

	@Override
	public String getString(long docId, String resource) {
		StringWriter writer = new StringWriter();
		try {
			IOUtils.copy(getStream(docId, resource), writer, "UTF-8");
			return writer.toString();
		} catch (Exception e) {
			log.error(e.getMessage());
			return null;
		}
	}

	/**
	 * Retrieves the setting "store." + id + ".dir"
	 */
	protected String getDir() {
		return getConfig().getProperty("store." + id + ".dir");
	}

	@Override
	public String getResourceName(Document doc, String fileVersion, String suffix) {
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document document = doc;

		/*
		 * All versions of a document are stored in the same directory as the
		 * current version, but the filename is the version number without
		 * extension, e.g. "docId/2.1"
		 */
		String filename;
		if (doc.getDocRef() != null) {
			// The shortcut document doesn't have the 'fileversion' and the
			// 'version'
			try {
				document = docDao.findById(doc.getDocRef());
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
		}

		if (StringUtils.isEmpty(fileVersion))
			filename = document.getFileVersion();
		else
			filename = fileVersion;
		if (StringUtils.isEmpty(filename))
			filename = document.getVersion();

		/*
		 * Document's related resources are stored with a suffix, e.g.
		 * "doc/2.1-thumb.png"
		 */
		if (StringUtils.isNotEmpty(suffix))
			filename += "-" + suffix;

		return filename;
	}

	@Override
	public String getResourceName(long docId, String fileVersion, String suffix) {
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		try {
			Document doc = docDao.findById(docId);
			return getResourceName(doc, fileVersion, suffix);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		}
	}

	@Override
	public Map<String, String> getParameters() {
		return parameters;
	}

	@Override
	public boolean test() {
		String resource = "test";
		File tmpFile = null;
		try {
			tmpFile = FileUtil.createTempFile("st-test", ".txt");
			FileUtil.writeFile("test", tmpFile.getAbsolutePath());
			store(tmpFile, 0L, resource);
			return exists(0L, resource);
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			return false;
		} finally {
			FileUtil.strongDelete(tmpFile);
			try {
				if (exists(0L, resource))
					delete(0L, resource);
			} catch (Exception t) {
				// Noting to do
			}
		}
	}

	@Override
	public boolean isEnabled() {
		return true;
	}

	@Override
	public String toString() {
		return this.getClass().getSimpleName() + " #" + id;
	}

	@Override
	public void init() {
		// Noting to do
	}

	@Override
	public void destroy() {
		// Noting to do
	}
}