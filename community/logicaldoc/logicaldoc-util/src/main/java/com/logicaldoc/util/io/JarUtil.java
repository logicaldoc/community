package com.logicaldoc.util.io;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.util.Enumeration;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class is for handling with jar-files.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 4.0
 */
public class JarUtil {

	protected static Logger log = LoggerFactory.getLogger(JarUtil.class);

	/**
	 * This method extracts all entries of a jar-file.
	 * 
	 * @param jarsource Path of the jar-file.
	 * @param target Path of the extracted files.
	 * @return True if successfully extracted.
	 */
	public static boolean unjar(String jarsource, String target) {
		boolean result = true;

		try {
			File targetDir = new File(target);
			if (targetDir.exists() && targetDir.isFile()) {
				targetDir.delete();
			}
			if (!targetDir.exists()) {
				FileUtils.forceMkdir(targetDir);
			}

			JarFile jar = new JarFile(jarsource);
			JarEntry jare;

			for (Enumeration enum1 = jar.entries(); enum1.hasMoreElements(); saveEntry(jar, jare, target)) {
				jare = (JarEntry) enum1.nextElement();
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
	public static boolean unjar(String jarsource, String target, String entry) {

		boolean result = true;

		try {
			File targetDir = new File(target);
			if (targetDir.exists() && targetDir.isFile()) {
				targetDir.delete();
			}
			if (!targetDir.exists()) {
				FileUtils.forceMkdir(targetDir);
			}

			JarFile jar = new JarFile(jarsource);
			JarEntry jare = new JarEntry(entry);
			saveEntry(jar, jare, target);
		} catch (Exception e) {
			result = false;
			log.error(e.getMessage(), e);
		}

		return result;
	}

	private static void saveEntry(JarFile jar, JarEntry jare, String target) throws Exception {
		File file = new File(target, jare.getName());

		if (jare.isDirectory()) {
			file.mkdirs();
		} else {
			java.io.InputStream is = null;
			BufferedInputStream bis = null;
			BufferedOutputStream bos = null;
			FileOutputStream fos = null;
			try {
				is = jar.getInputStream(jare);
				bis = new BufferedInputStream(is);
				File dir = new File(file.getParent());
				dir.mkdirs();

				fos = new FileOutputStream(file);
				bos = new BufferedOutputStream(fos);

				for (int letter = 0; (letter = bis.read()) != -1;) {
					bos.write((byte) letter);
				}
			} finally {
				if (bos != null)
					bos.close();
				if (fos != null)
					fos.close();
				if (bis != null)
					bis.close();
			}
		}
	}
}