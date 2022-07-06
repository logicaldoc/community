package com.logicaldoc.core.document.thumbnail;

import java.io.File;
import java.io.IOException;

import com.logicaldoc.core.document.Document;

/**
 * Implementations of this interface are specialized classes that produce
 * thumbnails for a specific type of document.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public interface ThumbnailBuilder {

	/**
	 * Produce the thumbnail
	 * 
	 * @param sid The session identifier (optional)
	 * @param document The document
	 * @param fileVersion Optional file version spec
	 * @param src The source file
	 * @param size The thumbnail size
	 * @param dest The destination thumbnail file
	 * @param quality Compression quality(0..100, 100 is maximum quality)

	 * @throws IOException raised when the image cannot be written
	 */
	public void buildThumbnail(String sid, Document document, String fileVersion, File src,	File dest, int size, int quality) throws IOException;
}