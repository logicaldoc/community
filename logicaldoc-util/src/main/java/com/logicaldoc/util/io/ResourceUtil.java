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

	private ResourceUtil() {
		throw new IllegalStateException("Utility class");
	}

	public static byte[] readAsBytes(String resourceName) throws IOException {
		return getInputStream(resourceName).readAllBytes();
	}

	public static String readAsString(String resourceName) throws IOException {
		StringBuilder resourceData = new StringBuilder(1000);
		try (BufferedReader reader = new BufferedReader(new InputStreamReader(getInputStream(resourceName)))) {
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
	 * @param resourceName The resource fully qualified name
	 * @param out The target file
	 * 
	 * @throws IOException raised in case the resource does not exist or the
	 *         output file cannot be written
	 */
	public static void copyResource(String resourceName, File out) throws IOException {
		try (InputStream is = getInputStream(resourceName);
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
		try (InputStream is = getInputStream(resourceName)) {
			return is.available() > 0L;
		} catch (Exception e) {
			return false;
		}
	}

	public static InputStream getInputStream(String resourceName) {
		if (resourceName.startsWith("/"))
			resourceName = resourceName.substring(1);

		InputStream is;
		try {
			is = new BufferedInputStream(
					Thread.currentThread().getContextClassLoader().getResourceAsStream(resourceName));
		} catch (Exception e) {
			is = new BufferedInputStream(FileUtil.class.getResourceAsStream(resourceName));
		}
		return is;
	}
}
