package com.logicaldoc.util;

import java.io.IOException;
import java.util.Properties;

import org.apache.commons.io.FilenameUtils;

/**
 * Utility class for MIME Type detection using the classpath
 * /mimetypes.properties RESOURCE
 * 
 * @author Marco Meschieri
 * 
 * @since 5.2
 */
public class MimeType {
	private static Properties mimeTypes = null;

	private MimeType() {

	}

	public static String get(String extension) {
		if (mimeTypes == null) {
			mimeTypes = new Properties();
			try {
				mimeTypes.load(MimeType.class.getResourceAsStream("/mimetypes.properties"));
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		if (extension.startsWith("."))
			extension = extension.substring(1);
		
		String type = mimeTypes.getProperty(extension, "application/octet-stream");
		if (type == null)
			type = "application/octet-stream";
		return type;
	}

	public static String getByFilename(String filename) {
		return get(FilenameUtils.getExtension(filename));
	}
}
