package com.logicaldoc.core.document.thumbnail;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.io.FilenameUtils;
import org.java.plugin.registry.Extension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.dao.TenantDAO;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.core.util.DocUtil;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.MimeType;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.plugin.PluginRegistry;

/**
 * Manager class used to handle document thumbnails
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class ThumbnailManager {
	public static final String SUFFIX_PREVIEW = "conversion.pdf";

	public static final String SUFFIX_TILE = "tile.jpg";

	public static final String SUFFIX_THUMB = "thumb.jpg";

	protected static Logger log = LoggerFactory.getLogger(ThumbnailManager.class);

	private Storer storer;

	// Key is the extension, value is the associated builder
	private Map<String, ThumbnailBuilder> builders = new HashMap<String, ThumbnailBuilder>();

	/**
	 * Creates the thumbnail for the specified document and file version. The
	 * thumbnail is an image rendering of the first page only.
	 * 
	 * @param document The document to be treated
	 * @param fileVersion The file version(optional)
	 * @param sid The session identifier(optional)
	 * 
	 * @throws IOException in case an error happens during image creation
	 */
	public void createTumbnail(Document document, String fileVersion, String sid) throws IOException {
		createImage(document, fileVersion, "thumbnail", SUFFIX_THUMB, sid);
	}

	/**
	 * Creates the tile for the specified document and file version. The
	 * thumbnail is an image rendering of the first page only.
	 * 
	 * @param document The document to be treated
	 * @param fileVersion The file version(optional)
	 * @param sid The session identifier(optional)
	 * 
	 * @throws IOException in case an error happens during image creation
	 */
	public void createTile(Document document, String fileVersion, String sid) throws IOException {
		createImage(document, fileVersion, "tile", SUFFIX_TILE, sid);
	}

	protected void createImage(Document document, String fileVersion, String type, String suffix, String sid)
			throws IOException {
		TenantDAO tDao = (TenantDAO) Context.get().getBean(TenantDAO.class);
		Tenant tenant = tDao.findById(document.getTenantId());
		long maxFileSize = Context.get().getProperties().getLong(tenant.getName() + ".gui.preview.maxfilesize", 0)
				* 1024 * 1024;

		if (maxFileSize > 0 && maxFileSize < document.getFileSize()) {
			log.warn("Document {} is too big for the thumbnail", document.getId());
			return;
		}

		ThumbnailBuilder builder = getBuilder(document);
		if (builder == null) {
			log.warn("No builder found for document {}", document.getId());
			return;
		}

		String tenantName = DocUtil.getTenantName(document);

		int size = 150;
		try {
			ContextProperties conf = Context.get().getProperties();
			size = Integer.parseInt(conf.getProperty(tenantName + ".gui." + type + ".size"));
		} catch (Throwable t) {
			log.error(t.getMessage());
		}

		int quality = 100;
		try {
			ContextProperties conf = Context.get().getProperties();
			int buf = Integer.parseInt(conf.getProperty(tenantName + ".gui." + type + ".quality"));
			if (buf < 1)
				buf = 1;
			if (buf > 100)
				buf = 100;
			quality = buf;
		} catch (Throwable t) {
			log.error(t.getMessage());
		}

		// Prepare I/O files
		File src = null;
		File dest = File.createTempFile("dest", suffix);

		try {
			src = writeToTempFile(document, fileVersion);

			builder.buildThumbnail(sid, document, fileVersion, src, dest, size, quality);

			// Put the resource
			String resource = storer.getResourceName(document, getSuitableFileVersion(document, fileVersion), suffix);
			storer.store(dest, document.getId(), resource);
		} catch (Throwable e) {
			log.warn("Error rendering image for document: {} - {}", document.getId(), document.getFileName(), e);
		} finally {
			// Delete temporary resources
			FileUtil.strongDelete(src);
			FileUtil.strongDelete(dest);
		}
	}

	/**
	 * Creates the thumbnail for the specified document
	 * 
	 * @param document The document to be treated
	 * @param sid The session identifier (optional)
	 * 
	 * @throws IOException raised in case the thumbnail file cannot be created
	 */
	public void createTumbnail(Document document, String sid) throws IOException {
		createTumbnail(document, null, sid);
	}

	/**
	 * Loads the proper builder for the passed document
	 * 
	 * @param document the document
	 * 
	 * @return the right thumbnail builder for the given document
	 */
	private ThumbnailBuilder getBuilder(Document document) {
		ThumbnailBuilder builder = getBuilders().get(document.getFileExtension().toLowerCase());

		if (builder == null) {
			log.warn("No registered thumbnail builder for extension {}", document.getFileExtension().toLowerCase());
			try {
				String mime = MimeType.getByFilename(document.getFileName());
				if ("text/plain".equals(mime)) {
					log.warn("Try to convert as plain text");
					builder = getBuilders().get("txt");
				} else
					builder = getBuilders().get("*");
			} catch (Throwable e) {
				log.error(e.getMessage());
			}
		}

		return builder;
	}

	/**
	 * Write a document into a temporary file.
	 * 
	 * @param document the document
	 * @param fileVersion version of the file
	 * 
	 * @return the temporary file
	 * 
	 * @throws IOException raised if the temporary file cannot be written
	 */
	private File writeToTempFile(Document document, String fileVersion) throws IOException {
		File target = File.createTempFile("scr",
				"." + FilenameUtils.getExtension(DocUtil.getFileName(document, fileVersion)));
		String fver = getSuitableFileVersion(document, fileVersion);
		String resource = storer.getResourceName(document, fver, null);
		storer.writeToFile(document.getId(), resource, target);
		return target;
	}

	/**
	 * Returns the fileVersion in case this is not null or
	 * document.getFileVersion() otherwise
	 * 
	 * @param document the document
	 * @param fileVersion version of the file
	 * 
	 * @return the file version name
	 */
	private String getSuitableFileVersion(Document document, String fileVersion) {
		String fver = fileVersion;
		if (fver == null)
			fver = document.getFileVersion();
		return fver;
	}

	/**
	 * Initializes the builders map
	 */
	private void initBuilders() {
		builders.clear();
		// Acquire the 'ThumbnailBuilder' extensions of the core plugin
		PluginRegistry registry = PluginRegistry.getInstance();
		Collection<Extension> exts = registry.getExtensions("logicaldoc-core", "ThumbnailBuilder");

		for (Extension ext : exts) {
			String className = ext.getParameter("class").valueAsString();
			String extension = ext.getParameter("extension").valueAsString().toLowerCase();
			try {
				@SuppressWarnings("rawtypes")
				Class clazz = Class.forName(className);
				// Try to instantiate the builder
				Object builder = clazz.newInstance();
				if (!(builder instanceof ThumbnailBuilder))
					throw new Exception(
							"The specified builder " + className + " doesn't implement ThumbnailBuilder interface");
				builders.put(extension, (ThumbnailBuilder) builder);
				log.info("Added new thumbnail builder {} for extension {}", className, extension);
			} catch (Throwable e) {
				log.error(e.getMessage(), e);
			}
		}
	}

	public Map<String, ThumbnailBuilder> getBuilders() {
		if (builders.isEmpty())
			initBuilders();
		return builders;
	}

	public void setStorer(Storer storer) {
		this.storer = storer;
	}
}