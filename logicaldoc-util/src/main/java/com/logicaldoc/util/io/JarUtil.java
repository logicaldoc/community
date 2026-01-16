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

import com.logicaldoc.util.spring.Context;

/**
 * This class is for handling with jar-files.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 4.0
 */
public class JarUtil {

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
			maxEntries = Context.get().getConfig().getInt("zip.maxentries", 100000);
			maxSize = Context.get().getConfig().getInt("zip.maxsize", 1024) * 1024 * 1024;
			maxCompressionRatio = Context.get().getConfig().getDouble("zip.maxratio", 30D);
		} catch (Exception t) {
			// Nothing to do
		}
	}

	/**
	 * This method extracts all entries of a jar-file.
	 * 
	 * @param jarsource Path of the jar-file.
	 * @param target Path of the extracted files.
	 * 
	 * @throws IOException I/O Error
	 */
	public void unjar(String jarsource, String target) throws IOException {
		int totalSizeArchive = 0;
		int totalEntryArchive = 0;
		File targetDir = new File(target);

		if (targetDir.exists() && targetDir.isFile())
			FileUtil.delete(targetDir);

		if (!targetDir.exists())
			FileUtils.forceMkdir(targetDir);

		try (JarFile jar = new JarFile(jarsource)) {
			Enumeration<? extends JarEntry> entries = jar.entries();
			while (entries.hasMoreElements()) {
				JarEntry je = entries.nextElement();
				File file = new File(targetDir.getPath() + File.separatorChar + je.getName());
				if (je.isDirectory()) {
					file.mkdirs();
				} else {
					file.getParentFile().mkdirs();
					totalSizeArchive += extractEntry(jar, je, file.getPath());
				}
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
	}

	/**
	 * This method extracts one entry of a jar-file.
	 * 
	 * @param jarsource Path of the jar-file.
	 * @param target Path of the extracted file.
	 * @param entry Name of the entry to be extracted.
	 * 
	 * @throws IOException I/O Error
	 */
	public void unjar(String jarsource, String entry, String target) throws IOException {
		try (JarFile jar = new JarFile(jarsource)) {
			JarEntry jare = new JarEntry(entry);
			extractEntry(jar, jare, target);
		}
	}

	private long extractEntry(JarFile jar, JarEntry je, String target) throws IOException {
		File file = new File(target);
		file.getParentFile().mkdirs();

		try (InputStream is = jar.getInputStream(je);
				BufferedInputStream bis = new BufferedInputStream(is);
				FileOutputStream fos = new FileOutputStream(file);
				BufferedOutputStream bos = new BufferedOutputStream(fos);) {

			int nBytes = -1;
			byte[] buffer = new byte[4096];
			int totalSizeEntry = 0;

			while ((nBytes = is.read(buffer)) > 0) { // Compliant
				bos.write(buffer, 0, nBytes);
				totalSizeEntry += nBytes;

				double compressionRatio = totalSizeEntry / (double) je.getCompressedSize();
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