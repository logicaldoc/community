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
import java.lang.reflect.InvocationTargetException;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import javax.annotation.Resource;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.java.plugin.registry.Extension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.support.rowset.SqlRowSet;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.RunLevel;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.document.dao.DocumentHistoryDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.StringUtil;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.plugin.PluginRegistry;

/**
 * Common methods for all the Storer implementations.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public abstract class AbstractStorer implements Storer {

	private static final String STORE = "store.";

	protected static final int DEFAULT_BUFFER_SIZE = 1024;

	protected static Logger log = LoggerFactory.getLogger(AbstractStorer.class);

	protected static Logger deletionsLog = LoggerFactory.getLogger("STORAGE_DELETIONS");

	@Resource(name = "ContextProperties")
	protected ContextProperties config;

	protected int id = 1;

	protected Map<String, String> parameters = new HashMap<>();

	// Key is the type, value is the associated storer
	protected Map<String, Storer> storerDefinitions = new HashMap<>();

	protected AbstractStorer() {
	}

	public ContextProperties getConfig() {
		if (config == null)
			config = Context.get().getProperties();
		return config;
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
		return Integer.compare(id, o.getId());
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
		checkEnabled();

		try (InputStream is = new BufferedInputStream(new FileInputStream(file), DEFAULT_BUFFER_SIZE)) {
			store(is, docId, resource);
		}

		checkWriteAfterStore(docId, resource, docId);
	}

	/**
	 * Checks if the given file is empty
	 * 
	 * @param file The file to check
	 * @throws IOException raised only if the file is 0 byte
	 */
	protected void checkNotEmpty(File file) throws IOException {
		if (file==null || file.length() == 0L)
			throw new IOException("Do not store 0 byte file");
	}

	/**
	 * Checks if the stored resource matches the expected size
	 * 
	 * @param docId Identifier of the document
	 * @param resource Name of the resource
	 * @param expectedSize The expected size in bytes
	 * 
	 * @throws IOException raised just in case the size of the resource does not
	 *         match the expected one+
	 * 
	 */
	protected void checkWriteAfterStore(long docId, String resource, long expectedSize) throws IOException {
		if (RunLevel.current().aspectEnabled("writeCheck")) {
			long storedSize = size(docId, resource);
			if (storedSize != expectedSize)
				throw new IOException(String.format(
						"Wrong file size, the original file was %d bytes while the stored one is %d bytes (docId: %d,  resource: %s",
						expectedSize, storedSize, docId, resource));
		}
	}

	/**
	 * Checks if the current store is enabled
	 * 
	 * @throws IOException raised just in case of disabled storer
	 */
	protected void checkEnabled() throws IOException {
		if (!isEnabled())
			throw new IOException("Storer not enabled");
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
		IOUtils.copyLarge(getStream(docId, resource), output, start, length);
	}

	@Override
	public void writeToStream(long docId, String resource, OutputStream output) throws IOException {
		IOUtils.copyLarge(getStream(docId, resource), output);
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
			IOUtils.copy(getStream(docId, resource), writer, StandardCharsets.UTF_8);
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
		return getConfig().getProperty(STORE + id + ".dir");
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
		if (!storerDefinitions.isEmpty())
			return;

		// Acquire the 'Storer' extensions
		PluginRegistry registry = PluginRegistry.getInstance();
		Collection<Extension> exts = registry.getExtensions("logicaldoc-core", "Storer");

		for (Extension ext : exts) {
			String type = ext.getParameter("type").valueAsString();
			String className = ext.getParameter("class").valueAsString();

			try {
				@SuppressWarnings("rawtypes")
				Class clazz = Class.forName(className);
				// Try to instantiate the builder
				@SuppressWarnings("unchecked")
				Object storer = clazz.getDeclaredConstructor().newInstance();
				if (!(storer instanceof Storer))
					throw new ClassNotFoundException(
							String.format("The specified storer %s doesn't implement the Storer interface", className));
				storerDefinitions.put(type, (Storer) storer);

				Storer st = (Storer) storer;
				for (String name : st.getParameterNames())
					st.getParameters().put(name, null);

				log.info("Added new storer {} for type {}", clazz.getSimpleName(), type);
			} catch (ClassNotFoundException | InstantiationException | IllegalAccessException | IllegalArgumentException
					| InvocationTargetException | NoSuchMethodException | SecurityException e) {
				log.error(e.getMessage(), e);
			}
		}
	}

	@Override
	public void destroy() {
		// Noting to do
	}

	protected void logDeletion(long docId, String path) {
		if (deletionsLog.isInfoEnabled())
			deletionsLog.info("str: {}, doc: {}, res: {}\n{}", getId(), docId, path,
					Arrays.toString(Thread.currentThread().getStackTrace()).replace(',', '\n'));

		DocumentHistoryDAO documentHistoryDAO = (DocumentHistoryDAO) Context.get().getBean(DocumentHistoryDAO.class);
		DocumentHistory history = new DocumentHistory();
		history.setEvent(DocumentEvent.RESOURCE_DELETED.toString());
		history.setDocId(docId);
		history.setComment(path);
		history.setFilename(StringUtils.right(path, 255));
		history.setReason("deleted from store " + getId());
		try {
			SqlRowSet rows = documentHistoryDAO.queryForRowSet(
					"select ld_tenantid, ld_filename, ld_version, ld_fileversion, ld_color, ld_folderid from ld_document where ld_id="
							+ docId,
					null);
			if (rows.next()) {
				history.setTenantId(rows.getLong(1));
				history.setFilename(rows.getString(2));
				history.setVersion(rows.getString(3));
				history.setFileVersion(rows.getString(4));
				history.setColor(rows.getString(5));
				history.setFolderId(rows.getLong(6));
				documentHistoryDAO.store(history);
			}
		} catch (PersistenceException e) {
			log.warn("Cannot record in the database the deleteion of resource {} for document {}", path, docId, e);
		}
	}

	/**
	 * Instantiate a new storer and fully configures it.
	 * 
	 * @param id identifier of the storer to create
	 * 
	 * @return the created instance
	 */
	public Storer newStorer(int id) {
		String type = config.getProperty(STORE + id + ".type", "fs");
		Storer definition = storerDefinitions.get(type);
		if (definition == null) {
			log.error("Unexisting definition for {}", type);
			return null;
		}

		Storer storer = null;
		try {
			storer = definition.getClass().getDeclaredConstructor().newInstance();
		} catch (Exception e) {
			log.error("Unable to instanciate class {} / {}", definition.getClass(), e.getMessage(), e);
			return null;
		}
		storer.setId(id);

		Set<String> params = definition.getParameters().keySet();
		for (String param : params)
			storer.getParameters().put(param, config.getProperty(STORE + id + "." + param, ""));

		return storer;
	}

	@Override
	public Map<String, Storer> getStorerDefinitions() {
		return storerDefinitions;
	}
}