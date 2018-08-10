package com.logicaldoc.util.io;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import net.lingala.zip4j.core.ZipFile;
import net.lingala.zip4j.exception.ZipException;
import net.lingala.zip4j.model.FileHeader;

import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class is for handling with zip-files.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 4.0
 */
public class ZipUtil {

	private String fileNameCharset = "UTF-8";

	public ZipUtil() {
	}

	public ZipUtil(String charset) {
		this.fileNameCharset = charset;
	}

	public List<String> listEntries(File zipsource) {
		List<String> files = new ArrayList<String>();
		try {
			ZipFile zipFile = new ZipFile(zipsource);
			setCharset(zipFile);
			@SuppressWarnings("unchecked")
			List<FileHeader> fileHeaders = zipFile.getFileHeaders();
			for (FileHeader fileHeader : fileHeaders)
				files.add(fileHeader.getFileName());
		} catch (Throwable e) {
			logError(e.getMessage());
		}
		return files;
	}

	/**
	 * This method extracts all entries of a zip-file.
	 * 
	 * @param zipsource Path of the zip-file.
	 * @param target Path of the extracted files.
	 * @return True if successfully extracted.
	 */
	public boolean unzip(String zipsource, String target) {
		boolean result = true;
		try {
			ZipFile zipFile = new ZipFile(zipsource);
			setCharset(zipFile);
			zipFile.extractAll(target);
		} catch (Throwable e) {
			result = false;
			logError(e.getMessage());
		}
		return result;
	}

	/**
	 * Read the entry inside the file zip resource.
	 * 
	 * @param zipFile File to read inside it
	 * @param entry The entry to be read
	 * @return The bytes of the entry
	 */
	public byte[] getEntryBytes(File zipsource, String entry) {
		if (entry.startsWith("/"))
			entry = entry.substring(1);

		InputStream entryStream = null;
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		try {
			ZipFile zipFile = new ZipFile(zipsource);
			setCharset(zipFile);
			FileHeader header = zipFile.getFileHeader(entry);

			entryStream = zipFile.getInputStream(header);
			IOUtils.copy(entryStream, baos);
			baos.flush();
		} catch (Throwable e) {
			logError(e.getMessage());
		} finally {
			try {
				baos.close();
				entryStream.close();
			} catch (Throwable e) {
			}
		}
		return baos.toByteArray();
	}

	private void setCharset(ZipFile zipFile) throws ZipException {
		if (fileNameCharset != null && !"auto".equals(fileNameCharset))
			zipFile.setFileNameCharset(fileNameCharset);
	}

	/**
	 * Read the entry inside the file zip resource.
	 * 
	 * @param zipFile File to read inside it
	 * @param entry The entry to be read
	 * @return The stream of the entry
	 */
	public InputStream getEntryStream(File zip, String entry) {
		if (entry.startsWith("/"))
			entry = entry.substring(1);

		try {
			ZipFile zipFile = new ZipFile(zip);
			setCharset(zipFile);
			FileHeader header = zipFile.getFileHeader(entry);
			return zipFile.getInputStream(header);
		} catch (Throwable e) {
			logError(e.getMessage());
			return null;
		}
	}

	public String getEntryContent(File zip, String entry) throws FileNotFoundException, IOException {
		return IOUtil.getStringFromInputStream(getEntryStream(zip, entry));
	}

	private static void logError(String message) {
		Logger logger = LoggerFactory.getLogger(ZipUtil.class);
		logger.error(message);
	}

	private void zipDir(File zipDir, ZipOutputStream zos, File startZipDir) {
		try {
			// get a listing of the directory content
			File[] dirList = zipDir.listFiles();
			byte[] readBuffer = new byte[2156];
			int bytesIn = 0;
			// loop through dirList, and zip the files
			for (int i = 0; i < dirList.length; i++) {
				File f = dirList[i];
				if (f.isDirectory()) {
					// if the File object is a directory, call this
					// function again to add its content recursively
					zipDir(f, zos, startZipDir);
					// loop again
					continue;
				}
				// if we reached here, the File object f was not
				// a directory
				// create a FileInputStream on top of f
				FileInputStream fis = new FileInputStream(f);
				// create a new zip entry
				String path = f.getPath();
				if (!path.equals(startZipDir.getPath()))
					path = path.substring(startZipDir.getPath().length());
				if (path.startsWith(File.separator))
					path = path.substring(1);
				ZipEntry anEntry = new ZipEntry(path);
				// place the zip entry in the ZipOutputStream object
				zos.putNextEntry(anEntry);
				// now write the content of the file to the ZipOutputStream
				while ((bytesIn = fis.read(readBuffer)) != -1) {
					zos.write(readBuffer, 0, bytesIn);
				}
				// close the Stream
				fis.close();
			}
		} catch (Exception e) {
			logError(e.getMessage());
		}
	}

	/**
	 * Compress a single file
	 * 
	 * @param src The source file
	 * @param dest The destination archive file
	 */
	public void zipFile(File src, File dest) {
		try {
			// create a ZipOutputStream to zip the data to
			ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(dest));

			FileInputStream fis = new FileInputStream(src);
			// create a new zip entry
			ZipEntry anEntry = new ZipEntry(src.getName());
			// place the zip entry in the ZipOutputStream object
			zos.putNextEntry(anEntry);

			byte[] readBuffer = new byte[2156];
			int bytesIn = 0;

			// now write the content of the file to the ZipOutputStream
			while ((bytesIn = fis.read(readBuffer)) != -1) {
				zos.write(readBuffer, 0, bytesIn);
			}
			// close the Stream
			fis.close();
			// close the stream
			zos.flush();
			zos.close();
		} catch (Exception e) {
			e.printStackTrace();
			logError(e.getMessage());
		}
	}

	/**
	 * Zips a folder into a .zip archive
	 */
	public void zipFolder(File inFolder, File outFile) {
		try {
			// create a ZipOutputStream to zip the data to
			ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(outFile));
			// assuming that there is a directory named inFolder (If there
			// isn't create one) in the same directory as the one the code
			// runs from,
			// call the zipDir method
			zipDir(inFolder, zos, inFolder);
			// close the stream
			zos.flush();
			zos.close();
		} catch (Exception e) {
			e.printStackTrace();
			logError(e.getMessage());
		}
	}

	public String getFileNameCharset() {
		return fileNameCharset;
	}

	public void setFileNameCharset(String fileNameCharset) {
		this.fileNameCharset = fileNameCharset;
	}
}