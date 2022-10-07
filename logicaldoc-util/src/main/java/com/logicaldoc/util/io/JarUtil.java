package com.logicaldoc.util.io;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.Context;

/**
 * This class is for handling with jar-files.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 4.0
 */
public class JarUtil {

	protected static Logger log = LoggerFactory.getLogger(JarUtil.class);

	/**
	 * Maximum number of entries in the compressed archive, config parameter
	 * zip.maxentires
	 */
	private int maxEntries = 100000;

	/**
	 * Maximum size of the uncompressed contents of the compressed archive,
	 * config parameter zip.maxsize
	 */
	private int maxSize = 1024 * 1024 * 1024; // 1 GB

	/**
	 * Maximum compression ratio, config parameter zip.maxratio
	 */
	private double maxCompressionRatio = 30D;

	public JarUtil() {
		try {
			maxEntries = Context.get().getProperties().getInt("zip.maxentries", 100000);
			maxSize = Context.get().getProperties().getInt("zip.maxsize", 1024) * 1024 * 1024;
			maxCompressionRatio = Context.get().getProperties().getDouble("zip.maxratio", 30D);
		} catch (Throwable t) {

		}
	}

	/**
	 * This method extracts all entries of a jar-file.
	 * 
	 * @param jarsource Path of the jar-file.
	 * @param target Path of the extracted files.
	 * @return True if successfully extracted.
	 */
	public boolean unjar(String jarsource, String target) {
		boolean result = true;

		int totalSizeArchive = 0;
		int totalEntryArchive = 0;

		try {
			File targetDir = new File(target);

			if (targetDir.exists() && targetDir.isFile())
				FileUtil.strongDelete(targetDir);

			if (!targetDir.exists())
				FileUtils.forceMkdir(targetDir);

			try (JarFile jar = new JarFile(jarsource)) {
				Enumeration<? extends JarEntry> entries = jar.entries();
				while (entries.hasMoreElements()) {
					JarEntry je = entries.nextElement();
					totalSizeArchive += unzipEntry(jar, je, target);
					totalEntryArchive++;

					if (totalSizeArchive > maxSize)
						throw new IOException(String.format(
								"Jar file %s looks like a Zip Bomb Attack: the uncompressed data size is over the maximum allowed of %s",
								jarsource, FileUtil.getDisplaySize(maxSize, "en")));

					if (totalEntryArchive > maxEntries)
						throw new IOException(String.format(
								"Jar file %s looks like a Zip Bomb Attack: can lead to inodes exhaustion of the system and is over the maximum allowed of %d",
								jarsource, maxEntries));
				}
			}
		} catch (Exception e) {
			result = false;
			log.error(e.getMessage(), e);
		}

		return result;
	}

	/**
	 * This method extracts one entry of a jar-file.
	 * 
	 * @param jarsource Path of the jar-file.
	 * @param target Path of the extracted files.
	 * @param entry Name of the entry to be extracted.
	 * @return True if successfully extracted.
	 */
	public boolean unjar(String jarsource, String entry, String target) {
		boolean result = true;

		try {
			File targetDir = new File(target);
			if (targetDir.exists() && targetDir.isFile())
				FileUtil.strongDelete(targetDir);

			if (!targetDir.exists())
				FileUtils.forceMkdir(targetDir);

			try (JarFile jar = new JarFile(jarsource)) {
				JarEntry jare = new JarEntry(entry);
				unzipEntry(jar, jare, target);
			}
		} catch (Exception e) {
			result = false;
			log.error(e.getMessage(), e);
		}

		return result;
	}

	private long unzipEntry(JarFile jar, JarEntry je, String target) throws IOException {
		File file = new File(target, je.getName());

		if (je.isDirectory()) {
			file.mkdirs();
			return 0L;
		} else {
			try (InputStream is = jar.getInputStream(je);
					BufferedInputStream bis = new BufferedInputStream(is);
					FileOutputStream fos = new FileOutputStream(file);
					BufferedOutputStream bos = new BufferedOutputStream(fos);) {
				File dir = new File(file.getParent());
				dir.mkdirs();

				int nBytes = -1;
				byte[] buffer = new byte[4096];
				int totalSizeEntry = 0;

				while ((nBytes = is.read(buffer)) > 0) { // Compliant
					bos.write(buffer, 0, nBytes);
					totalSizeEntry += nBytes;

					double compressionRatio = totalSizeEntry / (double)je.getCompressedSize();
					if (compressionRatio > maxCompressionRatio)
						throw new IOException(String.format(
								"Jar file looks like a Zip Bomb Attack: ratio between compressed and uncompressed data %f is highly suspicious and is over the maximum allowed of %f",
								compressionRatio, maxCompressionRatio));
					if (totalSizeEntry > maxSize)
						throw new IOException(String.format(
								"Jar file looks like a Zip Bomb Attack: the uncompressed data size is over the maximum allowed of %s",
								FileUtil.getDisplaySize(maxSize, "en")));
				}

				return totalSizeEntry;
			}
		}
	}
}