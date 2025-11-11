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

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.java.plugin.registry.Extension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.RunLevel;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentHistoryDAO;
import com.logicaldoc.util.StringUtil;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.plugin.PluginRegistry;
import com.logicaldoc.util.spring.Context;

import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import jakarta.annotation.Resource;

/**
 * Common methods for all the Store implementations.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public abstract class AbstractStore implements Store {

	private static final String STORE = "store.";

	protected static final int DEFAULT_BUFFER_SIZE = 1024;

	private static final Logger log = LoggerFactory.getLogger(AbstractStore.class);

	protected static Logger deletionsLog = LoggerFactory.getLogger("STORE_DELETIONS");

	@Resource(name = "ContextProperties")
	protected ContextProperties config;

	protected int id = 1;

	protected Map<String, String> parameters = new HashMap<>();

	// Key is the type, value is the associated store
	protected Map<String, Store> storeDefinitions = new HashMap<>();

	protected AbstractStore() {
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
	public int compareTo(Store o) {
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

		return id == ((Store) obj).getId();
	}

	@Override
	public void store(File file, StoreResource resource) throws IOException {
		checkEnabled();

		try (InputStream is = new BufferedInputStream(new FileInputStream(file), DEFAULT_BUFFER_SIZE)) {
			store(is, resource);
		}

		checkWriteAfterStore(resource, file.length());
	}

	/**
	 * Checks if the given file is empty
	 * 
	 * @param file The file to check
	 * @throws IOException raised only if the file is 0 byte
	 */
	protected void checkNotEmpty(File file) throws IOException {
		if (file == null)
			throw new IOException("Do not store null file");
		if (file.length() == 0L)
			throw new IOException("Do not store 0 byte file");
	}

	/**
	 * Checks if the stored resource matches the expected size
	 * 
	 * @param docId Identifier of the document
	 * @param resource The resource, make sure to specify the document's ID
	 * @param expectedSize The expected size in bytes
	 * 
	 * @throws IOException raised just in case the size of the resource does not
	 *         match the expected one+
	 * 
	 */
	protected void checkWriteAfterStore(StoreResource resource, long expectedSize) throws IOException {
		if (RunLevel.current().aspectEnabled("writeCheck") && resource.getDocId() != 0L) {
			long storedSize = size(resource);
			if (storedSize != expectedSize)
				throw new IOException(String.format(
						"Wrong file size, the original file was %d bytes while the stored one is %d bytes (docId: %d,  resource: %s",
						expectedSize, storedSize, resource.getDocId(), resource));
		}
	}

	/**
	 * Checks if the current store is enabled
	 * 
	 * @throws IOException raised just in case of disabled store
	 */
	protected void checkEnabled() throws IOException {
		if (!isEnabled())
			throw new IOException("Store not enabled");
	}

	/**
	 * Computes the relative path of a document's folder inside the store root.
	 * The document's id is tokenized by three chars tokens, than the doc/ dir
	 * is appended, so if the docId=12345, the document's path will be:
	 * 123/45/doc.
	 */
	protected String computeRelativePath(long docId) {
		return StringUtil.split(Long.toString(docId), '/', 3) + "/doc";
	}

	/**
	 * Computes the relative path of a document's resource inside the store
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
		try (InputStream input = getStream(docId, resource)) {
			IOUtils.copyLarge(input, output, start, length);
		}
	}

	@Override
	public void writeToStream(long docId, String resource, OutputStream output) throws IOException {
		try (InputStream input = getStream(docId, resource)) {
			IOUtils.copyLarge(input, output);
		}
	}

	@Override
	public void writeToFile(StoreResource resource, File out) throws IOException {
		try (OutputStream os = new BufferedOutputStream(new FileOutputStream(out, false), DEFAULT_BUFFER_SIZE);
				InputStream is = getStream(resource.getDocId(), resource.name());) {
			FileUtil.writeFile(is, out.getPath());
		} catch (IOException ioe) {
			throw ioe;
		} catch (Exception e) {
			log.error("Error writing document {} into {}", resource.getDocId(), out.getPath());
			log.error(e.getMessage(), e);
		}
	}

	@Override
	public String getString(long docId, String resource) {
		StringWriter writer = new StringWriter();
		try (InputStream input = getStream(docId, resource)) {
			IOUtils.copy(input, writer, StandardCharsets.UTF_8);
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
		DocumentDAO docDao = DocumentDAO.get();
		Document document = doc;

		/*
		 * All versions of a document are stored in the same directory as the
		 * current version, but the filename is the version number without
		 * extension, e.g. "doc/2.1"
		 */
		String resourceName;
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
			resourceName = document.getFileVersion();
		else
			resourceName = fileVersion;
		if (StringUtils.isEmpty(resourceName))
			resourceName = document.getVersion();

		/*
		 * Document's related resources are stored with a suffix, e.g.
		 * "doc/2.1-thumb.png"
		 */
		if (StringUtils.isNotEmpty(suffix))
			resourceName += "-" + suffix;

		return sanitizeResourceName(resourceName);
	}

	@Override
	public String getResourceName(long docId, String fileVersion, String suffix) {
		DocumentDAO docDao = DocumentDAO.get();
		try {
			Document doc = docDao.findById(docId);
			return getResourceName(doc, fileVersion, suffix);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		}
	}

	@Deprecated
	protected String sanitizeResourceName(String resourceName) {
		return resourceName.replace("..", "").replaceAll("[^a-zA-Z0-9\\-\\\\.]", "");
	}

	@Override
	public Map<String, String> getParameters() {
		return parameters;
	}

	@Override
	public boolean test() {
		File tmpFile = null;
		StoreResource resource = new StoreResource.Builder().docId(0L).fileVersion("test").build();
		try {
			tmpFile = FileUtil.createTempFile("st-test", ".txt");
			FileUtil.writeFile("test", tmpFile.getAbsolutePath());
			store(tmpFile, resource);
			return exists(resource);
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			return false;
		} finally {
			FileUtil.delete(tmpFile);
			try {
				if (exists(resource))
					delete(resource);
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
	@PostConstruct
	public void init() {
		if (!storeDefinitions.isEmpty())
			return;

		// Acquire the 'Store' extensions
		PluginRegistry registry = PluginRegistry.getInstance();
		Collection<Extension> exts = registry.getExtensions("logicaldoc-core", "Store");

		for (Extension ext : exts) {
			String type = ext.getParameter("type").valueAsString();
			String className = ext.getParameter("class").valueAsString();

			try {
				@SuppressWarnings("rawtypes")
				Class clazz = Class.forName(className);
				// Try to instantiate the builder
				@SuppressWarnings("unchecked")
				Object store = clazz.getDeclaredConstructor().newInstance();
				if (!(store instanceof Store))
					throw new ClassNotFoundException(
							String.format("The specified store %s doesn't implement the Store interface", className));
				storeDefinitions.put(type, (Store) store);

				Store st = (Store) store;
				for (String name : st.getParameterNames())
					st.getParameters().put(name, null);

				log.info("Added new store {} for type {}", clazz.getSimpleName(), type);
			} catch (ClassNotFoundException | InstantiationException | IllegalAccessException | IllegalArgumentException
					| InvocationTargetException | NoSuchMethodException e) {
				log.error(e.getMessage(), e);
			}
		}
	}

	@Override
	@PreDestroy
	public void destroy() {
		// Noting to do
	}

	protected void logDeletion(long docId, String path) {
		if (deletionsLog.isInfoEnabled())
			deletionsLog.info("str: {}, doc: {}, res: {}\n{}", getId(), docId, path,
					Arrays.toString(Thread.currentThread().getStackTrace()).replace(',', '\n'));

		DocumentHistoryDAO documentHistoryDAO = DocumentHistoryDAO.get();
		DocumentHistory history = new DocumentHistory();
		history.setEvent(DocumentEvent.RESOURCE_DELETED);
		history.setDocId(docId);
		history.setComment(path);
		history.setFilename(StringUtils.right(path, 255));
		history.setReason("deleted from store " + getId());
		try {
			documentHistoryDAO.queryForResultSet(
					"select ld_tenantid, ld_filename, ld_version, ld_fileversion, ld_color, ld_folderid from ld_document where ld_id="
							+ docId,
					null, null, rows -> {
						if (rows.next()) {
							history.setTenantId(rows.getLong(1));
							history.setFilename(rows.getString(2));
							history.setVersion(rows.getString(3));
							history.setFileVersion(rows.getString(4));
							history.setColor(rows.getString(5));
							history.setFolderId(rows.getLong(6));
						}
					});

			documentHistoryDAO.store(history);
		} catch (PersistenceException e) {
			log.warn("Cannot record in the database the deleteion of resource {} for document {}", path, docId, e);
		}
	}

	/**
	 * Instantiate a new store and fully configures it.+
	 * 
	 * 
	 * @param id identifier of the store to create
	 * 
	 * @return the created instance
	 */
	public Store newStore(int id) {
		String type = config.getProperty(STORE + id + ".type", "fs");
		Store definition = storeDefinitions.get(type);
		if (definition == null) {
			log.error("Unexisting definition for {}", type);
			return null;
		}

		Store store = null;
		try {
			store = definition.getClass().getDeclaredConstructor().newInstance();
		} catch (Exception e) {
			log.error("Unable to instanciate class {} / {}", definition.getClass(), e.getMessage());
			log.error(e.getMessage(), e);
			return null;
		}
		store.setId(id);

		Set<String> params = definition.getParameters().keySet();
		for (String param : params)
			store.getParameters().put(param, config.getProperty(STORE + id + "." + param, ""));

		return store;
	}

	@Override
	public Map<String, Store> getStoreDefinitions() {
		return storeDefinitions;
	}
}