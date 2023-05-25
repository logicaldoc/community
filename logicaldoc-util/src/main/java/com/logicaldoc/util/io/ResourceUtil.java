package com.logicaldoc.util.io;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;

/**
 * Utiliry class for classpath resources IO issues
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 4.5
 */
public class ResourceUtil {

	public static String readAsString(String resourceName) throws IOException {
		StringBuilder resourceData = new StringBuilder(1000);
		try (BufferedReader reader = new BufferedReader(
				new InputStreamReader(ResourceUtil.class.getResourceAsStream(resourceName)))) {
			char[] buf = new char[1024];
			int numRead = 0;
			while ((numRead = reader.read(buf)) != -1) {
				resourceData.append(buf, 0, numRead);
			}
		}
		return resourceData.toString();
	}

	/**
	 * Copy a resource from the classpath into a file
	 * 
	 * @param classpath The classpath specification
	 * @param out The target file
	 * 
	 * @throws IOException raised in case the resource does not exist or the
	 *         output file cannot be written
	 */
	public static void copyResource(String classpath, File out) throws IOException {
		try (InputStream is = new BufferedInputStream(ResourceUtil.class.getResource(classpath).openStream());
				OutputStream os = new BufferedOutputStream(new FileOutputStream(out));) {
			for (;;) {
				int b = is.read();
				if (b == -1)
					break;
				os.write(b);
			}
		}
	}

	public static boolean existsResource(String resourceName) {
		InputStream is = null;
		try {
			try {
				is = new BufferedInputStream(FileUtil.class.getResource(resourceName).openStream());
			} catch (Exception e) {
				is = new BufferedInputStream(
						Thread.currentThread().getContextClassLoader().getResource(resourceName).openStream());
			}
			return is != null;
		} catch (Exception e) {
			return false;
		} finally {
			if (is != null)
				try {
					is.close();
				} catch (IOException e) {
					// Nothing to do
				}
		}
	}
}
