package com.logicaldoc.core.document.thumbnail;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.java.plugin.registry.Extension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.core.store.Store;
import com.logicaldoc.core.util.DocUtil;
import com.logicaldoc.util.MimeType;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.plugin.PluginRegistry;
import com.logicaldoc.util.spring.Context;

import jakarta.annotation.PostConstruct;
import jakarta.annotation.Resource;

/**
 * Manager class used to handle document thumbnails
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
@Component("thumbnailManager")
public class ThumbnailManager {

	public static final String SUFFIX_PREVIEW = "conversion.pdf";

	public static final String SUFFIX_TILE = "tile.png";

	public static final String THUMB = "thumb";

	public static final String SUFFIX_THUMB = THUMB + ".png";

	public static final String SUFFIX_MOBILE = "mobile.png";

	private static final Logger log = LoggerFactory.getLogger(ThumbnailManager.class);

	@Resource(name = "store")
	private Store store;

	// Key is the extension, value is the associated builder
	private Map<String, ThumbnailBuilder> builders = new HashMap<>();

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
	 * Creates the thumbnail for the specified document and file version using
	 * given size and quality. The thumbnail is an image rendering of the first
	 * page only.
	 * 
	 * @param document The document to be treated
	 * @param fileVersion The file version(optional)
	 * @param size The thumbnail size
	 * @param quality Compression quality(0..100, 100 is maximum quality). If
	 *        not specified the standard thumbnail quality will be used.
	 * @param sid The session identifier(optional)
	 * 
	 * @throws IOException in case an error happens during image creation
	 */
	public void createTumbnail(Document document, String fileVersion, int size, Integer quality, String sid)
			throws IOException {
		createImage(document, fileVersion, size, quality, SUFFIX_THUMB, sid);
	}

	/**
	 * Creates the tile for the specified document and file version. The tile is
	 * an image rendering of the first page only.
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
	
	/**
	 * Creates the tile for the specified document
	 * 
	 * @param document The document to be treated
	 * @param sid The session identifier (optional)
	 * 
	 * @throws IOException raised in case the tile file cannot be created
	 */
	public void createTile(Document document, String sid) throws IOException {
		createTile(document, null, sid);
	}

	/**
	 * Creates the mobile image for the specified document and file version. The
	 * mobile is an image rendering of the first page only.
	 * 
	 * @param document The document to be treated
	 * @param fileVersion The file version(optional)
	 * @param sid The session identifier(optional)
	 * 
	 * @throws IOException in case an error happens during image creation
	 */
	public void createMobile(Document document, String fileVersion, String sid) throws IOException {
		createImage(document, fileVersion, "mobile", SUFFIX_MOBILE, sid);
	}
	
	/**
	 * Creates the mobile image for the specified document. The
	 * mobile is an image rendering of the first page only.
	 * 
	 * @param document The document to be treated
	 * @param sid The session identifier(optional)
	 * 
	 * @throws IOException in case an error happens during image creation
	 */
	public void createMobile(Document document, String sid) throws IOException {
		createImage(document, null, "mobile", SUFFIX_MOBILE, sid);
	}


	protected void createImage(Document document, String fileVersion, int size, Integer quality, String suffix,
			String sid) throws IOException {

		ThumbnailBuilder builder = getBuilder(document);
		if (builder == null) {
			log.warn("No builder found for document {}", document.getId());
			return;
		}

		// Prepare I/O files
		File src = null;
		File dest = FileUtil.createTempFile("dest", suffix);

		try {
			src = writeToTempFile(document, fileVersion);

			builder.buildThumbnail(sid, document, fileVersion, src, dest, size,
					quality != null ? quality
							: Context.get().getProperties()
									.getInt(TenantDAO.get().getTenantName(document.getTenantId()) + ".gui.thumbnail.quality", 93));

			// Put the resource
			String resource = store.getResourceName(document, getSuitableFileVersion(document, fileVersion), suffix);
			store.store(dest, document.getId(), resource);
		} catch (Exception e) {
			log.warn("Error rendering image for document: {} - {}", document.getId(), document.getFileName(), e);
		} finally {
			// Delete temporary resources
			FileUtil.delete(src);
			FileUtil.delete(dest);
		}
	}

	protected void createImage(Document document, String fileVersion, String type, String suffix, String sid)
			throws IOException {
		String tenantName = DocUtil.getTenantName(document);
		createImage(document, fileVersion,
				Context.get().getProperties().getInt(tenantName + ".gui." + type + ".size", 200),
				Context.get().getProperties().getInt(tenantName + ".gui." + type + ".quality", 93), suffix, sid);
	}

	/**
	 * Loads the proper builder for the passed file name
	 * 
	 * @param filename the filename to be rendered
	 * 
	 * @return the right thumbnail builder for the given file name
	 */
	public ThumbnailBuilder getBuilder(String filename) {
		String ext = filename.contains(".") ? FileUtil.getExtension(filename.toLowerCase()) : filename.toLowerCase();
		ThumbnailBuilder builder = getBuilders().get(ext);

		if (builder == null) {
			log.warn("No registered thumbnail builder for extension {}", ext);
			try {
				String mime = MimeType.getByFilename(filename);
				if ("text/plain".equals(mime)) {
					log.warn("Try to use a txt builder");
					builder = getBuilders().get("txt");
				} else
					builder = getBuilders().get("*");
			} catch (Exception e) {
				log.error(e.getMessage());
			}
		}

		return builder;
	}

	/**
	 * Loads the proper builder for the passed document
	 * 
	 * @param document the document
	 * 
	 * @return the right thumbnail builder for the given document
	 */
	private ThumbnailBuilder getBuilder(Document document) {
		return getBuilder(document.getFileExtension().toLowerCase());
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
		File target = FileUtil.createTempFile("scr",
				"." + FileUtil.getExtension(DocUtil.getFileName(document, fileVersion)));
		String fver = getSuitableFileVersion(document, fileVersion);
		String resource = store.getResourceName(document, fver, null);
		store.writeToFile(document.getId(), resource, target);
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
	@PostConstruct
	public synchronized void init() {
		if (!builders.isEmpty())
			return;

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
				@SuppressWarnings("unchecked")
				Object builder = clazz.getDeclaredConstructor().newInstance();
				if (!(builder instanceof ThumbnailBuilder))
					throw new ClassNotFoundException(
							"The specified builder " + className + " doesn't implement ThumbnailBuilder interface");
				builders.put(extension, (ThumbnailBuilder) builder);
				log.info("Added new thumbnail builder {} for extension {}", className, extension);
			} catch (Exception e) {
				log.error(e.getMessage(), e);
			}
		}
	}

	public Map<String, ThumbnailBuilder> getBuilders() {
		if (builders.isEmpty())
			init();
		return builders;
	}

	public void setStore(Store store) {
		this.store = store;
	}
}